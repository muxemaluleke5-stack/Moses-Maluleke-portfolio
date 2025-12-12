# Spatial Analysis — OLS vs GWR

**Objective:**  
Compare a global OLS model with a local Geographically Weighted Regression (GWR) to assess spatial non-stationarity in the relationship between [dependent variable] and predictors.

**Data:**  
- Spatial dataset with geometry (points/polygons) and variables: [dependent], [predictor1], [predictor2], ...  
- Source: [describe]

**Methods:**  
- Fit global OLS and inspect diagnostics (residuals, multicollinearity, global R²).  
- Test spatial autocorrelation (Moran's I) of residuals.  
- Fit GWR to allow coefficients to vary across space.  
- Compare models using AIC/AICc, adjusted R², and residual Moran's I.  
- Visualise: residuals map (OLS), local coefficients map (GWR), local R² map, and model comparison table.

**Key files:**  
- `spatial_analysis.R` — full code (data cleaning → OLS → GWR → plots)  
- `plots/` — maps and diagnostics  
- `tables/` — model comparison outputs

**Outcome:**  
- Demonstrates whether relationships are spatially varying and which variables show non-stationarity.  
- Shows improved fit (if any) from GWR and provides interpretable local coefficient maps.
