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
library(here)      # install.packages("here") if needed

set.seed(42)       # ensures bootstrap forecasts are reproducible

# ==============================================================
#  DATA LOADING & PREPARATION
# ==============================================================

CassFreight <- read_xlsx(here("data", "Cass_Freight_Data.xlsx"))

Cass <- CassFreight %>%
  mutate(
    Month = yearmonth(Month),
    # COVID-19 structural break dummies
    #   covid_shock:      sharp demand collapse during initial lockdowns (Mar–Jun 2020)
    #   covid_disruption: prolonged supply-chain disruption period   (Mar 2020–Dec 2021)
    covid_shock      = as.integer(
      Month >= yearmonth("2020 Mar") & Month <= yearmonth("2020 Jun")
    ),
    covid_disruption = as.integer(
      Month >= yearmonth("2020 Mar") & Month <= yearmonth("2021 Dec")
    )
  ) %>%
  as_tsibble(index = Month)

# ==============================================================
#  HELPER FUNCTIONS
# ==============================================================

# --- Forecast plot (models with no external regressors) ---
plot_fc <- function(model, data, title, h = "1 year",
                    bootstrap = FALSE, level = c(80, 95)) {
  model %>%
    forecast(h = h, bootstrap = bootstrap) %>%
    autoplot(data, level = level) +
    labs(title = title)
}

# --- Forecast plot supplying future regressor values explicitly ---
#     Required for TSLM and ARIMA models that include COVID dummies.
plot_fc_newdata <- function(model, train_data, new_data, title,
                            bootstrap = FALSE, level = c(80, 95)) {
  model %>%
    forecast(new_data = new_data, bootstrap = bootstrap) %>%
    autoplot(train_data, level = level) +
    labs(title = title)
}

# --- Out-of-sample accuracy against the held-out test set ---
accuracy_oos <- function(model, test_data) {
  model %>%
    forecast(new_data = test_data) %>%
    accuracy(test_data)
}

# --------------------------------------------------------------
#  Model-fitting helpers
#  Box-Cox is applied uniformly inside every model formula via
#  box_cox(var, lambda).  Lambda is computed once (Guerrero method)
#  and passed in — no model ever sees the raw un-transformed series.
# --------------------------------------------------------------

fit_simple <- function(data, var, lambda) {
  v <- sym(var)
  data %>% model(
    snaive   = SNAIVE(box_cox(!!v, lambda) ~ drift()),
    rw_drift = RW(box_cox(!!v, lambda) ~ drift())
  )
}

# TSLM and Fourier models include the two COVID dummies as regressors
# so the structural break is explicitly captured in the trend equation.
fit_tslm <- function(data, var, lambda) {
  v <- sym(var)
  data %>% model(
    tslm    = TSLM(box_cox(!!v, lambda) ~ trend() + season()
                   + covid_shock + covid_disruption),
    fourier = TSLM(box_cox(!!v, lambda) ~ trend() + fourier(K = 2)
                   + covid_shock + covid_disruption)
  )
}

# ETS / Holt-Winters — error and season type differ by variable
# (multiplicative for Rates which has proportional variance;
#  additive for Shipments which has more stable variance)
fit_ets <- function(data, var, lambda, error_type, season_type) {
  v <- sym(var)
  data %>% model(
    hw        = ETS(box_cox(!!v, lambda) ~ error(error_type) + trend("A")
                    + season(season_type)),
    hw_damped = ETS(box_cox(!!v, lambda) ~ error(error_type) + trend("Ad")
                    + season(season_type))
  )
}

# STL decomposition + ETS on the seasonally adjusted component
fit_stl_ets <- function(data, var, lambda) {
  v <- sym(var)
  data %>% model(
    stl_ets = decomposition_model(
      STL(box_cox(!!v, lambda)),
      ETS(season_adjust ~ season("N"))
    )
  )
}

# ARIMA with COVID dummies as external regressors.
# stepwise = FALSE and approximation = FALSE give the exhaustive
# search for the best ARIMA(p,d,q)(P,D,Q) order.
fit_arima <- function(data, var, lambda) {
  v <- sym(var)
  data %>% model(
    arima = ARIMA(
      box_cox(!!v, lambda) ~ covid_shock + covid_disruption,
      stepwise = FALSE, approximation = FALSE
    )
  )
}

# ==============================================================
#  EXPLORATORY ANALYSIS
# ==============================================================

# Multi-variable time series plot (updated from deprecated gather())
Cass %>%
  pivot_longer(cols = c(Rates, Shipments),
               names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = Month, y = value, color = variable, linetype = variable)) +
  geom_line() +
  scale_color_manual(values = c("darkred", "steelblue")) +
  labs(title = "Freight Data Over Time", x = "Month", y = "Value")

Cass %>%
  drop_na() %>%
  autoplot(.vars = TLLH) +
  labs(title = "Truckload Linehaul Index Over Time", x = "Month", y = "TLLH")

# Seasonality analysis
Cass %>% gg_subseries(Rates)     + labs(title = "Subseries: Rates")
Cass %>% gg_subseries(Shipments) + labs(title = "Subseries: Shipments")
Cass %>% gg_season(Rates)        + labs(title = "Seasonal: Rates")
Cass %>% gg_season(Shipments)    + labs(title = "Seasonal: Shipments")

# STL decomposition
Cass %>%
  model(stl = STL(Rates ~ season(window = "periodic") + trend(window = 13))) %>%
  components() %>% autoplot() + labs(title = "STL Decomposition: Rates")

Cass %>%
  model(stl = STL(Shipments ~ season(window = "periodic") + trend(window = 13))) %>%
  components() %>% autoplot() + labs(title = "STL Decomposition: Shipments")

# ==============================================================
#  BOX-COX LAMBDAS  (computed once; passed into every model)
# ==============================================================

lambda_Rates     <- Cass %>%
  features(Rates,     features = guerrero) %>%
  pull(lambda_guerrero)

lambda_Shipments <- Cass %>%
  features(Shipments, features = guerrero) %>%
  pull(lambda_guerrero)

message(sprintf(
  "Box-Cox lambda — Rates: %.4f | Shipments: %.4f",
  lambda_Rates, lambda_Shipments
))

# ==============================================================
#  TRAIN / TEST SPLIT  (12-month holdout)
# ==============================================================

Cass_test     <- Cass %>% filter_index("2022 May" ~ "2023 Apr")
Cass_training <- Cass %>% filter_index(~ "2022 Apr")

# ==============================================================
#  MODEL FITTING
# ==============================================================

simple_Rates     <- fit_simple(Cass_training, "Rates",     lambda_Rates)
simple_Shipments <- fit_simple(Cass_training, "Shipments", lambda_Shipments)

tslm_Rates     <- fit_tslm(Cass_training, "Rates",     lambda_Rates)
tslm_Shipments <- fit_tslm(Cass_training, "Shipments", lambda_Shipments)

ets_Rates     <- fit_ets(Cass_training, "Rates",     lambda_Rates,
                         error_type = "M", season_type = "M")
ets_Shipments <- fit_ets(Cass_training, "Shipments", lambda_Shipments,
                         error_type = "A", season_type = "A")

stl_ets_Rates     <- fit_stl_ets(Cass_training, "Rates",     lambda_Rates)
stl_ets_Shipments <- fit_stl_ets(Cass_training, "Shipments", lambda_Shipments)

arima_Rates     <- fit_arima(Cass_training, "Rates",     lambda_Rates)
arima_Shipments <- fit_arima(Cass_training, "Shipments", lambda_Shipments)

# Print the selected ARIMA(p,d,q)(P,D,Q) orders for transparency
message("=== ARIMA model reports ===")
arima_Rates     %>% report()
arima_Shipments %>% report()

# ==============================================================
#  IN-SAMPLE FORECAST PLOTS  (1-year horizon against training data)
# ==============================================================

plot_fc(simple_Rates,      Cass_training, "RW and SNAIVE: Rates",        h = 12)
plot_fc(simple_Shipments,  Cass_training, "RW and SNAIVE: Shipments",    h = 12)

# TSLM needs future COVID dummy values → pass test set as new_data
plot_fc_newdata(tslm_Rates,     Cass_training, Cass_test,
                "TSLM and Fourier: Rates",     level = NULL)
plot_fc_newdata(tslm_Shipments, Cass_training, Cass_test,
                "TSLM and Fourier: Shipments", level = NULL)

plot_fc(ets_Rates,         Cass_training, "ETS: Rates",                  level = NULL)
plot_fc(ets_Shipments,     Cass_training, "ETS: Shipments",              level = NULL)
plot_fc(stl_ets_Rates,     Cass_training, "STL-ETS: Rates")
plot_fc(stl_ets_Shipments, Cass_training, "STL-ETS: Shipments")

# ARIMA also needs future COVID dummy values
plot_fc_newdata(arima_Rates,     Cass_training, Cass_test,
                "ARIMA: Rates",     bootstrap = TRUE)
plot_fc_newdata(arima_Shipments, Cass_training, Cass_test,
                "ARIMA: Shipments", bootstrap = TRUE)

# ==============================================================
#  MODEL COMPARISON: Out-of-sample accuracy vs test set
#  All models evaluated on the same 12-month holdout.
# ==============================================================

all_accuracy <- bind_rows(
  accuracy_oos(simple_Rates,      Cass_test) %>% mutate(variable = "Rates"),
  accuracy_oos(simple_Shipments,  Cass_test) %>% mutate(variable = "Shipments"),
  accuracy_oos(tslm_Rates,        Cass_test) %>% mutate(variable = "Rates"),
  accuracy_oos(tslm_Shipments,    Cass_test) %>% mutate(variable = "Shipments"),
  accuracy_oos(ets_Rates,         Cass_test) %>% mutate(variable = "Rates"),
  accuracy_oos(ets_Shipments,     Cass_test) %>% mutate(variable = "Shipments"),
  accuracy_oos(stl_ets_Rates,     Cass_test) %>% mutate(variable = "Rates"),
  accuracy_oos(stl_ets_Shipments, Cass_test) %>% mutate(variable = "Shipments"),
  accuracy_oos(arima_Rates,       Cass_test) %>% mutate(variable = "Rates"),
  accuracy_oos(arima_Shipments,   Cass_test) %>% mutate(variable = "Shipments")
)

message("=== Out-of-sample accuracy (ranked by MASE) ===")
all_accuracy %>%
  select(variable, .model, RMSE, MAE, MASE, RMSSE) %>%
  arrange(variable, MASE) %>%
  print(n = Inf)

# ==============================================================
#  RESIDUAL DIAGNOSTICS  (best candidates from accuracy table)
# ==============================================================

gg_tsresiduals(arima_Rates)       + labs(title = "Residuals: ARIMA Rates")
gg_tsresiduals(arima_Shipments)   + labs(title = "Residuals: ARIMA Shipments")
gg_tsresiduals(stl_ets_Shipments) + labs(title = "Residuals: STL-ETS Shipments")

# Ljung-Box test — p > 0.05 indicates no remaining autocorrelation in residuals.
# dof = number of estimated ARIMA parameters (adjust if order changes).
message("=== Ljung-Box tests ===")
arima_Rates     %>% augment() %>% features(.innov, ljung_box, lag = 24, dof = 3)
arima_Shipments %>% augment() %>% features(.innov, ljung_box, lag = 24, dof = 3)
stl_ets_Shipments %>% augment() %>% features(.innov, ljung_box, lag = 24, dof = 0)

# ==============================================================
#  FINAL 3-YEAR FORECASTS  (best models refit on full dataset)
# ==============================================================

arima_Rates_full      <- fit_arima(Cass, "Rates",     lambda_Rates)
arima_Shipments_full  <- fit_arima(Cass, "Shipments", lambda_Shipments)
stl_ets_Ship_full     <- fit_stl_ets(Cass, "Shipments", lambda_Shipments)

# Future periods post-2023: pandemic disruption has ended → both dummies = 0
future_3yr <- new_data(Cass, n = 36) %>%
  mutate(covid_shock = 0L, covid_disruption = 0L)

plot_fc_newdata(arima_Rates_full,    Cass, future_3yr,
                "Final Forecast: ARIMA Rates (2023–2026)")
plot_fc_newdata(arima_Shipments_full, Cass, future_3yr,
                "Final Forecast: ARIMA Shipments (2023–2026)", bootstrap = TRUE)
plot_fc_newdata(stl_ets_Ship_full,   Cass, future_3yr,
                "Final Forecast: STL-ETS Shipments (2023–2026)", bootstrap = TRUE)

# ==============================================================
#  REPRODUCIBILITY
# ==============================================================

sessionInfo()
