# Comparison of SARIMA and LSTM Models for Hourly Ethereum Price Forecasting

## Objective
Build and compare two forecasting models — **SARIMA** (classical time-series) and **LSTM** (deep learning) — to predict hourly Ethereum prices and evaluate which method better captures trend, seasonality, volatility, and nonlinear structure.

## Data Description
- Hourly Ethereum (ETH) Perpetual Futures data  
- Source: FTX via TradingView (Kaggle)  
- Period: 1 Jan 2020 – 10 Sep 2022  
- Total points: 23,615 (23,596 after removing missing Volume MA rows)

**Variables**
- `time` – Unix timestamp (UTC)  
- `open` – Opening price  
- `high` – Highest price in the hour  
- `low` – Lowest price in the hour  
- `close` – Closing price  
- `volume` – Contracts traded  
- `volume_MA` – 20-hour moving average of volume  

Unix timestamps allow consistent ordering of time events across locations and systems.

## Methods
### SARIMA (Statistical Model)
- Checked stationarity (ADF)  
- Differencing to remove trend/seasonality  
- ACF/PACF inspection for model selection  
- Automated SARIMA search  
- Residual diagnostics (Ljung-Box, ACF residuals)  
- Forecast generation and accuracy computation

### LSTM (Deep Learning Model)
- Used multiple features including OHLC, volume, and lagged values  
- Scaled data for network stability  
- Defined sequences of past timesteps as input  
- LSTM network with ReLU activation + Adam optimizer  
- Epoch training with validation to prevent overfitting  
- Forecast generation on test set

## Visual Outputs
- `sarima_forecast.png` – SARIMA forecast vs actual  
- `lstm_forecast.png` – LSTM forecast vs actual  
- `residuals_compare.png` – Model residual comparison  
- `error_metrics_table.png` – RMSE, MAE, MAPE comparison  

## Tools
- **R:** forecast, tseries, ggplot2  
- **Python (or R Keras):** TensorFlow/Keras, NumPy, pandas, matplotlib  

## Key Findings (Summary)
- SARIMA performs well on **linear** seasonal structure  
- LSTM captures **nonlinear** patterns and volatile regimes  
- Accuracy differences depend on:
  - Regime shifts  
  - Trend strength  
  - Nonlinear component magnitude  

## Files in This Folder
- `sarima_model.R` – SARIMA modelling script  
- `lstm_model.py` or `lstm_model.R` – LSTM modelling script  
- `plots/` – Forecasts, residuals, error metrics  
- `README.md` – This document

## Outcome
- Demonstrated the strengths and limitations of classical vs deep learning models  
- Showed practical skills in time-series modelling, feature engineering, neural networks, and evaluation  
- Highlighted the importance of model selection based on data characteristics (linear vs nonlinear)

