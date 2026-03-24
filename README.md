# Cass Freight Index — Time-Series Forecasting

## Overview
Time-series analysis and forecasting on **33 years of North American freight data** (1990–2023) using the Cass Freight Index. Compared 7 forecasting models across Rates and Shipments variables, selecting ARIMA as the best performer based on residual diagnostics and accuracy metrics.

## Tools & Libraries
- **R** (primary language)
- fable, tsibble, feasts, ggplot2, dplyr, tidyr, readxl

## Methods Applied
- **ARIMA** — Auto-regressive Integrated Moving Average (best performer: MASE = 0.325 for Rates)
- **ETS / Holt-Winters** — Exponential Smoothing with damped trend
- **STL Decomposition** — Seasonal and Trend decomposition using Loess
- **SNAIVE & RW with Drift** — Baseline benchmark models
- **TSLM & Fourier** — Time Series Linear Models
- **Box-Cox Transformation** — Variance stabilization (Guerrero method)

## Key Findings
- **Cyclical pattern confirmed** across all 4 variables with a consistent positive long-run trend
- **COVID-19 structural break** in 2020 required special handling to avoid forecast distortion
- **Strong seasonality in Shipments** — freight volumes peak in Q3/Q4, consistent with retail inventory cycles
- **ARIMA outperformed ETS** on both Rates (MASE: 0.325 vs 0.346) and Shipments (MASE: 0.428 vs 0.416)
- **Rates & TLLH divergence** post-2020 suggests fuel surcharge volatility as a new structural driver

## Files
| File | Description |
|------|-------------|
| `Cass_Freight_Analysis.R` | Complete R script with all models and diagnostics |
| `Cass_Freight_Full_Report.docx` | Full 28-page analysis report with visualizations |
| `Executive_Summary.pdf` | 2-page executive summary of findings and recommendations |
| `data/Cass_Freight_Data.xlsx` | Source dataset from Cass Information Systems |

## Author
**Kye Jones** — BS Economics (Business Forecasting), Central Washington University
Completed as part of ECON 325: Introduction to Forecasting (2023)
