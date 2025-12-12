# 📊 Moses Maluleke — Data Scientist | Quant Analyst | Risk Modelling  

Hi, I'm **Moses Maluleke**, a Data Scientist and Quantitative Analyst with strong foundations in  
**statistical modelling, machine learning, risk analytics, time series forecasting, and actuarial methods.**

I hold a **BSc in Mathematical Statistics & Econometrics** and have completed my **BSc Honours in Risk Analysis**, with additional expertise in:  
- Volatility forecasting  
- Machine learning for financial time series  
- Statistical distribution fitting for risk analysis  
- Spatial analysis  
- GLMs, Bayesian regression, and predictive modelling  
- R, Python, SQL, VBA and Excel  

This portfolio showcases my strongest data, ML, quantitative and risk projects — structured the same way top firms like **McKinsey, QuantumBlack, Nedbank, FNB, Absa, and hedge funds** prefer.

---

# 📁 **Portfolio Projects**

## 1️⃣ Cryptocurrency Risk Modelling — *22 Statistical Distributions*
**Tools:** R, fitdistrplus, actuar, ggplot2  
**Skills:** Risk modelling, extreme value analysis, distribution fitting  

This project investigates the **loss and gain distributions** of the **top 3 cryptocurrencies** by fitting  
**22 statistical distributions** and comparing them using:  
- AIC/BIC  
- Goodness-of-fit tests  
- Tail-risk behaviour  
- Log-likelihood analysis  
- Out-of-sample validation  

👉 *Folder:* `crypto-risk-modelling`  
👉 Includes:  
- Full analysis  
- R scripts  
- Distribution comparison tables  
- Plots  
- Interpretation of tail risk  

---

# 2 Comparison of SARIMA and LSTM Models for Hourly Ethereum Price Forecasting

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


## 3️⃣ J540 Health Time Series Forecasting  
**Tools:** R, forecast, tsibble  
**Skills:** Time series decomposition, anomaly detection, ARIMA  

Forecasting monthly health data (J540 dataset) using:  
- STL decomposition  
- ARIMA  
- Exponential smoothing  
- Trend assessment  
- Seasonality identification  

👉 *Folder:* `j540-health-forecasting`

---

## 4️⃣ Spatial Analysis — Hotspot Analysis, Moran’s I, Getis-Ord Gi*  
**Tools:** ArcGIS Pro, QGIS, R  
**Skills:** Spatial statistics, clustering analysis, hotspot detection  

Spatial modelling of school distributions, evaluating:  
- Moran’s I Global Autocorrelation  
- Local Gi* Hotspot Analysis  
- Ripley’s K Function  
- Kernel Density Estimation  

👉 *Folder:* `spatial-analysis`

---

## 5️⃣ Quadratic Discriminant Analysis (QDA) Classification

**Objective:**  
Classify observations using Quadratic Discriminant Analysis and evaluate predictive performance.

**Techniques & Analysis:**  
- Performed QDA on dataset to classify target variable  
- Assessed model performance using:  
  - Confusion Matrix  
  - Accuracy, Precision, Recall  
  - ROC curve (optional)  
- Compared QDA performance with baseline models (if applicable)

**Tools:**  
R, MASS, caret, ggplot2

**Contents:**  
- `qda_analysis.R` → R code  
- Plots:  
  - `qda_confusion.png` → confusion matrix  
  - `qda_roc.png` → ROC curve (optional)  
- README.md → project summary

**Outcome:**  
- Built a predictive classification model using QDA  
- Evaluated model performance with clear metrics and visualizations  
- Demonstrated ability to implement statistical classification methods in R

👉 *Folder:* `QDA`

---

# 🧠 **Research Summaries**

### 📌 **Honours Research (Main Study)**
**Title:** *Quantifying the Risk of the Top 3 Cryptocurrency Assets Using Statistical Distribution Fitting*  
- Fitted 22 statistical distributions to crypto losses/gains  
- Assessed risk using tail-heavy models (e.g., Pareto, Cauchy)  
- Evaluated fit using AIC/BIC & KS/AD tests  
- Insights: crypto returns show strong heavy-tailed behaviour  

---

### 📌 **Machine Learning Project (Financial Time Series Module)**
**Title:** *Volatility Forecasting & Price Prediction Using ML and Econometric Models*  
- Compared GARCH, LSTM and tree-based models  
- Used feature engineering (lags, rolling vol, market indicators)  
- Combined statistical and deep learning forecasting approaches  

---

# 🛠 **Technical Skills**

| Category | Skills |
|--------|--------|
| **Programming** | R, Python, SQL, VBA |
| **Modeling** | Time Series, ML, GLMs, Bayesian Regression, GARCH, Panel Data |
| **Risk Analytics** | Distribution Fitting, Tail Risk, VaR Concepts |
| **Tools** | Excel, ArcGIS, GitHub, Git, RStudio, Jupyter |
| **ML Methods** | LSTM, RF, GBM, XGBoost |
| **Statistical Methods** | ANOVA, Kruskal-Wallis, Imputation, Regression |

---


# 📬 **Contact**
📧 Email: **muxemaluleke5@gmail.com**  
📱 Phone: **061 656 8460**  
🌍 Location: Johannesburg south africa  

---

# ⭐ If you find this portfolio useful, feel free to star the repository!
