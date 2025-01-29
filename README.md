# 5-Spatial-Analysis

---

## Folder Structure

### 1. `SpatialRegression`
- **Title**: Modeling Unemployment in West Java Using Spatial Regression
- **Description**:
  - Explores unemployment rates across West Java using Spatial Lag Model (SLM) and Spatial Durbin Model (SDM) to account for spatial dependencies between regions.
  - Key variables include GDP, labor force participation, sanitation access, and poverty rates.
- **Key Insights**:
  - SDM shows GDP positively impacts neighboring regions' unemployment rates, while higher labor force participation and sanitation access reduce unemployment locally.
- **Data Source**: [BPS Jawa Barat (2021)](https://jabar.bps.go.id)

### 2. `GWR`
- **Title**: Localized Population Density Analysis in West Java Using GWR
- **Description**:
  - Uses Geographically Weighted Regression (GWR) to model the effects of variables such as school participation rates, family planning, and migration on population density.
  - Compares Gaussian, Bisquare, and Tricube kernels to determine the best fit for localized analysis.
- **Key Insights**:
  - Family planning participation and education level significantly influence population density locally, with the Gaussian kernel yielding the best results.
- **Data Source**: [BPS Jawa Barat (2022)](https://jabar.bps.go.id)

### 3. `HotspotDetection`
- **Title**: Identifying Tuberculosis Hotspots in South Kalimantan
- **Description**:
  - Combines spatial and space-time analysis using Poisson-based models to identify clusters of tuberculosis (TBC) cases in South Kalimantan from 2020 to 2022.
  - Tools such as SaTScan and RStudio are utilized for hotspot detection.
- **Key Insights**:
  - Persistent hotspots identified in Banjarmasin, Barito Kuala, and Hulu Sungai Tengah highlight the importance of targeted public health strategies.
- **Data Source**: [South Kalimantan Open Data](https://data.kalselprov.go.id/home)

### 4. `QuadratMethod`
- **Title**: Spatial Point Pattern Analysis of Traffic Light Distribution in DKI Jakarta
- **Description**:
  - Quadrat Method evaluates the distribution of traffic lights to determine whether it follows Complete Spatial Randomness (CSR) or exhibits clustering patterns.
  - Chi-square tests confirm clustering tendencies in traffic light locations.
- **Key Insights**:
  - Traffic lights are significantly clustered, particularly in Central Jakarta, suggesting opportunities for optimized traffic management strategies.
- **Data Source**: [DKI Jakarta Open Data](https://katalog.satudata.go.id)

## Tools:
- **R**
- **SaTScan**
