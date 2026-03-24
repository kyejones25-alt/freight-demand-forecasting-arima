# ============================================================
# Cass Freight Index: Time-Series Analysis & Forecasting
# Author: Kye Jones
# Course: Econ 325 - Forecasting & Predictive Analytics
# Central Washington University, 2023
#
# Tools: R, fable, tsibble, ggplot2, ARIMA, ETS, STL Decomposition
# Data: Cass Freight Index (Jan 1990 - Apr 2023)
# ============================================================

library(readxl)
library(dplyr)
library(tidyr)
library(tsibble)
library(fable)
library(feasts)
library(ggplot2)

# --- Load Data ---
CassFreight <- read_xlsx("data/Cass_Freight_Data.xlsx")
Cass <- CassFreight %>% 
  mutate(Month = yearmonth(Month)) %>%
  as_tsibble(index = Month)

# --- Exploratory Visualization ---
Cass_plotting <- Cass %>% gather(key = "variable", value = "value", -c(Month, TLLH))

Cass_plotting %>% 
  ggplot(aes(x = Month, y = value)) + 
  geom_line(aes(color = variable, linetype = variable)) + 
  scale_color_manual(values = c("darkred","steelblue","darkgreen")) +
  labs(title = "Freight Data Over Time", x = "Month", y = "Value")

Cass %>% 
  drop_na() %>% 
  autoplot(.vars = TLLH) +
  labs(title = "Truckload Linehaul Index Over Time", x = "Month", y = "TLLH")

# --- Seasonality Analysis ---
Cass %>% gg_subseries(Rates) + labs(title = "Rates")
Cass %>% gg_subseries(Shipments) + labs(title = "Shipments")
Cass %>% gg_season(Rates) + labs(title = "Rates")
Cass %>% gg_season(Shipments) + labs(title = "Shipments")

# --- STL Decomposition ---
Cass %>%
  model(stl = STL(Rates ~ season(window = "periodic") + trend(window = 13))) %>% 
  components() %>% autoplot()

Cass %>%
  model(stl = STL(Shipments ~ season(window = "periodic") + trend(window = 13))) %>% 
  components() %>% autoplot()

# --- Box-Cox Transformation ---
lambda_Rates <- Cass %>% features(Rates, features = guerrero) %>% pull()
lambda_Shipments <- Cass %>% features(Shipments, features = guerrero) %>% pull()

# --- Train/Test Split ---
Cass_test <- Cass %>% filter_index("2022 May" ~ "2023 Apr")
Cass_training <- anti_join(Cass, Cass_test, by = "Month")

# --- Simple Models: SNAIVE & RW with Drift ---
simple_models_Rates <- Cass_training %>% model(
  snaive_drift_Rates = SNAIVE(Rates ~ drift(), lambda = lambda_Rates),
  rw_drift_Rates = RW(Rates ~ drift(), lambda = lambda_Rates)
)
simple_models_Rates %>% forecast(h = 12) %>% autoplot(Cass_training) + labs(title = "RW and SNAIVE Rates")

simple_models_Shipments <- Cass_training %>% model(
  snaive_drift_Shipments = SNAIVE(Shipments ~ drift(), lambda = lambda_Shipments),
  rw_drift_Shipments = RW(Shipments ~ drift(), lambda = lambda_Shipments)
)
simple_models_Shipments %>% forecast(h = 12) %>% autoplot(Cass_training) + labs(title = "RW and SNAIVE Shipments")

# --- TSLM & Fourier Models ---
tslm_model_Rates <- Cass_training %>% model(
  tslm_Rates = TSLM(Rates ~ trend() + season()),
  fourier_Rates = TSLM(Rates ~ trend() + fourier(K = 2))
) 
tslm_model_Rates %>% forecast(h = "1 year") %>% autoplot(Cass_training, level = NULL) + labs(title = "TSLM and Fourier Rates")

Cass_training_shipments <- Cass_training %>%
  mutate(Shipments_transform = box_cox(Shipments, lambda_Shipments)) 

tslm_models_Shipments <- Cass_training_shipments %>% model(
  tslm_Shipments = TSLM(Shipments_transform ~ trend() + season()),
  fourier_Shipments = TSLM(Shipments_transform ~ trend() + fourier(K = 2))
)
tslm_models_Shipments %>% forecast(h = "1 year") %>% autoplot(Cass_training_shipments, level = NULL) + labs(title = "TSLM and Fourier Shipments")

# --- ETS / Holt-Winters Models ---
ets_models_Rates <- Cass_training %>% model(
  hw_Rates = ETS(Rates ~ error("M") + trend("A") + season("M")),
  hw_damped_Rates = ETS(Rates ~ error("M") + trend("Ad") + season("M"))
)
ets_models_Rates %>% forecast(h = "1 year") %>% autoplot(Cass_training, level = NULL) + labs(title = "ETS Rates: HW and HW Damped")

ets_models_Shipments <- Cass_training_shipments %>% model(
  hw_shipments = ETS(Shipments_transform ~ error("A") + trend("A") + season("A")),
  hw_damped_shipments = ETS(Shipments_transform ~ error("A") + trend("Ad") + season("A"))
)
ets_models_Shipments %>% forecast(h = "1 year") %>% autoplot(Cass_training_shipments, level = NULL) + labs(title = "ETS Shipments: HW and HW Damped")

# --- STL of ETS Models ---
stl_ets_Rates <- decomposition_model(
  STL(box_cox(Rates, lambda = lambda_Rates)),
  ETS(season_adjust ~ season("N"))
)
stl_ets_model_Rates <- Cass_training %>% model(stl_ets = stl_ets_Rates)
stl_ets_model_Rates %>% forecast(h = "1 year") %>% autoplot(Cass_training) + labs(title = "STL-ETS Rates")

stl_ets_shipments <- decomposition_model(
  STL(box_cox(Shipments, lambda = lambda_Shipments)),
  ETS(season_adjust ~ season("N"))
)
stl_ets_model_Shipments <- Cass_training %>% model(stl_ets = stl_ets_shipments)
stl_ets_model_Shipments %>% forecast(h = "1 year") %>% autoplot(Cass_training) + labs(title = "STL-ETS Shipments")

# --- ARIMA Models ---
arima_model_Rates <- Cass_training %>% model(
  auto_arima = ARIMA(box_cox(Rates, lambda = lambda_Rates), stepwise = FALSE, approximation = FALSE)
)
arima_model_Rates %>% report()
arima_model_Rates %>% forecast(h = "1 year", bootstrap = TRUE) %>% autoplot(Cass_training) + labs(title = "ARIMA Rates")

arima_model_Shipments <- Cass_training %>% model(
  auto_arima = ARIMA(box_cox(Shipments, lambda = lambda_Shipments), stepwise = FALSE, approximation = FALSE)
)
arima_model_Shipments %>% report()
arima_model_Shipments %>% forecast(h = "1 year", bootstrap = TRUE) %>% autoplot(Cass_training) + labs(title = "ARIMA Shipments")

# --- Model Selection: Residual Diagnostics ---
# Rates candidates: ARIMA (MASE=0.325), RW Drift (MASE=0.346)
gg_tsresiduals(arima_model_Rates) + labs(title = "Residuals: ARIMA Rates")
accuracy(arima_model_Rates)

simple_models_Rates %>% select(rw_drift_Rates) %>% gg_tsresiduals() + labs(title = "Residuals: RW Drift Rates")
simple_models_Rates %>% select(rw_drift_Rates) %>% accuracy()

# Shipments candidates: ARIMA (MASE=0.428), STL-ETS (MASE=0.416)
gg_tsresiduals(arima_model_Shipments) + labs(title = "Residuals: ARIMA Shipments")
accuracy(arima_model_Shipments)

gg_tsresiduals(stl_ets_model_Shipments) + labs(title = "Residuals: STL-ETS Shipments")
accuracy(stl_ets_model_Shipments)

# --- Final 3-Year Forecasts ---
# Best models: ARIMA for Rates, ARIMA + STL-ETS for Shipments
arima_model_Rates %>% forecast(h = "3 years", bootstrap = FALSE) %>% autoplot(Cass_training) + labs(title = "Final Forecast: ARIMA Rates")
arima_model_Shipments %>% forecast(h = "3 years", bootstrap = TRUE) %>% autoplot(Cass_training) + labs(title = "Final Forecast: ARIMA Shipments")
stl_ets_model_Shipments %>% forecast(h = "3 years", bootstrap = TRUE) %>% autoplot(Cass_training) + labs(title = "Final Forecast: STL-ETS Shipments")
