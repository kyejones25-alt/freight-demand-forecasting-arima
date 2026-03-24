# Cass Freight Index — Time-Series Forecasting

## Overview
Time-series analysis and forecasting on **33 years of North American freight data** (1990–2023) using the Cass Freight Index. Applied multiple forecasting models to identify seasonal patterns and generate forward-looking predictions for freight rates and shipment volumes.

## Tools & Libraries
- **R** (primary language)
- forecast, tseries, ggplot2, dplyr

## Methods
- **ARIMA** — Auto-regressive Integrated Moving Average
- **ETS** — Exponential Smoothing State Space
- **STL Decomposition** — Seasonal and Trend decomposition using Loess
- **Holt-Winters** — Triple exponential smoothing

## Key Findings
- Identified strong seasonal freight patterns with consistent Q4 peaks
- ARIMA and STL-ETS models produced the most accurate forward-looking forecasts
- Freight volumes showed long-term growth trend with cyclical sensitivity to economic downturns

## Files
- `Cass_Freight_Forecasting.Rmd` — Full R Markdown analysis
- `Cass_Freight_Forecasting.html` — Rendered report
- `Executive_Summary.pdf` — One-page summary of findings
- `data/` — Source data files

## Author
**Kye Jones** — MS in IT Management (Data Analytics), Central Washington University
Completed as part of IT 686: Approaches to Data Analytics for IT Managers
