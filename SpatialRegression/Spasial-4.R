# Load libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(tmap)
library(sf)
library(lmtest)
library(spdep)
library(spatialreg)
library(geosphere)
library(car)

# Read data
df <- read_excel("/Users/Golda/College/sem 5/spasial/Tugas 4/data regresi spasial.xlsx")
head(df)

# Read shapefile
shapefile <- st_read("/Users/Golda/College/sem 5/spasial/Tugas 4/shapefile jabar/Jawa_Barat_ADMIN_BPS.shp")
shapefile <- shapefile[shapefile$Kabupaten != "Waduk Cirata", ]
colnames(df)[colnames(df) == "Kabupaten/Kota"] <- "Kabupaten"
shapefile <- shapefile %>% left_join(df, by = "Kabupaten")

# Descriptive statistics
summary(df)

# Data normalization to avoid scale differences
df <- df %>%
  mutate(across(c(PDRB, APS, TPAK, KP, ASL, TK, JAK), ~ scale(.)))

# Data visualization
variables <- c("TPT", "PDRB", "APS", "TPAK", "KP", "ASL", "TK", "JAK")

# Iterate for each variable
for (var in variables) {
  # Map
  p <- ggplot(data = shapefile) +
    geom_sf(aes_string(fill = var), color = "black", size = 0.2) +
    scale_fill_gradient(low = "yellow", high = "dark green", name = var) +
    labs(title = paste("Map of", var, "Values by Region")) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", vjust = 2, size = 20), 
      plot.margin = margin(t = 20, r = 10, b = 10, l = 10) 
    )
  print(p) 
  
  # Boxplot
  boxplot(df[[var]],
          main = paste("Boxplot of", var),
          ylab = var,
          col = "lightblue")
}

# OLS model for linear regression
ols_model <- lm(TPT ~ PDRB + APS + TPAK + KP + ASL + TK + JAK, data = df)
summary(ols_model)

# OLS model assumption tests
  # 1. Residual normality
  qqnorm(residuals(ols_model))
  qqline(residuals(ols_model), col = "red")
  shapiro_test <- shapiro.test(residuals(ols_model))
  cat("Shapiro-Wilk Test:", shapiro_test$p.value, "\n")
  
  # 2. Heteroscedasticity
  bptest_ols <- bptest(ols_model)
  cat("Breusch-Pagan Test:", bptest_ols$p.value, "\n")
  
  # 3. Residual autocorrelation
  dw_test <- dwtest(ols_model)
  cat("Durbin-Watson Test:", dw_test$p.value, "\n")
  
  # 4. Multicollinearity
  vif_values <- vif(ols_model)
  print(vif_values)

  # 5. OLS residual plot
  residuals_best <- residuals(ols_model) 
  shapefile$Residuals <- residuals_best
  tm_shape(shapefile) +
    tm_polygons("Residuals", palette = "RdYlBu", midpoint = 0, title = paste("Residuals:", ols_model)) +
    tm_layout(
      outer.margins = c(0.1, 0, 0, 0), 
      main.title = paste("Residuals Map for OLS"), 
      main.title.size = 1.5, 
      main.title.fontface = "bold", 
      main.title.position = "center" 
    )  

# Create coordinate matrix
coords <- cbind(df$Longitude, df$Latitude)

# Calculate distances between coordinates using geosphere
distances <- distm(coords)

# Calculate distance threshold for spatial relationship
threshold <- quantile(distances[distances > 0], 0.2)  # Increase if too sparse

# Create spatial weights matrix based on threshold
weights_matrix <- distances < threshold
diag(weights_matrix) <- 0  # No self-relationship
listw <- mat2listw(weights_matrix, style = "W")

# Moran's I Test
  # 1. For dependent variable
  moran_test_dep <- moran.test(df$TPT, listw)
  print(moran_test_dep)
  
  # 2. For independent variables
  indep_vars <- c("PDRB", "APS", "TPAK", "KP", "ASL", "TK", "JAK")
  
  # Iterate for each independent variable
  for (var in indep_vars) {
    cat("\nMoran's I Test for", var, ":\n")
    print(moran.test(df[[var]], listw))
  }
  
  # 3. For OLS residuals
  moran_test_resid <- moran.test(residuals(ols_model), listw)
  print(moran_test_resid)

# Spatial Regression Models
models <- list(
  SLX = lmSLX(TPT ~ PDRB + APS + TPAK + KP + ASL + TK + JAK, data = df, listw = listw),
  SLM = lagsarlm(TPT ~ PDRB + APS + TPAK + KP + ASL + TK + JAK, data = df, listw = listw, zero.policy = TRUE),
  SEM = errorsarlm(TPT ~ PDRB + APS + TPAK + KP + ASL + TK + JAK, data = df, listw = listw, zero.policy = TRUE),
  GSM = sacsarlm(TPT ~ PDRB + APS + TPAK + KP + ASL + TK + JAK, data = df, listw = listw, zero.policy = TRUE),
  SDM = lagsarlm(TPT ~ PDRB + APS + TPAK + KP + ASL + TK + JAK, data = df, listw = listw, type = "mixed", zero.policy = TRUE),
  SDEM = errorsarlm(TPT ~ PDRB + APS + TPAK + KP + ASL + TK + JAK, data = df, listw = listw, etype = "emixed", zero.policy = TRUE),
  GNSM = sacsarlm(TPT ~ PDRB + APS + TPAK + KP + ASL + TK + JAK, data = df, listw = listw, type = "general", zero.policy = TRUE)
)

# Iterate for each model
for (model_name in names(models)) {
  model <- models[[model_name]]
  
  # Display model summary
  cat("\n=== Model:", model_name, "===\n")
  print(summary(model))
  
  # Model residuals
  residuals_model <- residuals(model)
  
  # KS-test
  ks_test <- ks.test(residuals_model, "pnorm", mean(residuals_model), sd(residuals_model))
  cat("\nKS-test results for", model_name, ":\n")
  print(ks_test)
  
  # BP-test
  bp_test <- bptest(residuals_model ~ PDRB + APS + TPAK + KP + ASL + TK + JAK, data = df)
  cat("\nBP-test results for", model_name, ":\n")
  print(bp_test)
  
  # Moran's I
  moran_test <- moran.test(residuals_model, listw)
  cat("\nMoran's I results for", model_name, ":\n")
  print(moran_test)
  
  # Add residuals to shapefile
  shapefile$Residuals <- residuals_model
  
  # Plot spatial residuals on the map
  cat("\nVisualizing spatial residuals for", model_name, ":\n")
  print(
    tm_shape(shapefile) +
      tm_polygons("Residuals", palette = "RdYlBu", midpoint = 0, title = paste("Residuals:", model_name)) +
      tm_layout(
        outer.margins = c(0.1, 0, 0, 0), 
        main.title = paste("Residuals Map for", model_name), 
        main.title.size = 1.5, 
        main.title.fontface = "bold", 
        main.title.position = "center"
      )
  )
}

# Filter only SLM and SDM models from the list of models
filtered_models <- models[c("SLM", "SDM")]

# (Based on Moran's I test: spatial autocorrelation is present in dependent and independent variables, so SLM and SDM models are used)
# Compare AIC values for SLM and SDM
aic_values <- sapply(filtered_models, AIC)
cat("AIC values for SLM and SDM models:\n")
print(aic_values)

# Determine the best model based on AIC
best_model <- names(aic_values)[which.min(aic_values)]
cat("\nThe best model based on AIC is:", best_model, "\n")

# In the SDM model, some variables (APS and JAK) are not significant. APS will be excluded
sdm2_model <- lagsarlm(TPT ~ PDRB + TPAK + KP + ASL + TK + JAK, data = df, listw = listw, type = "mixed", zero.policy = TRUE) 
summary(sdm2_model)

  # KS-test
  ks_test <- ks.test(residuals(sdm2_model), "pnorm", mean(residuals(sdm2_model)), sd(residuals(sdm2_model)))
  cat("KS-test results:\n")
  print(ks_test)
  
  # BP-test 
  bp_test <- bptest(residuals(sdm2_model) ~ PDRB + APS + TPAK + KP + ASL + TK + JAK, data = df)
  cat("\nBP-test results:\n")
  print(bp_test)
  
  # Moran's I
  moran_test <- moran.test(residuals(sdm2_model), listw)
  cat("\nMoran's I results:\n")
  print(moran_test)

# Plot spatial residuals for the best model 
residuals_best <- residuals(sdm2_model) 

# Add residuals to shapefile 
shapefile$Residuals <- residuals_best
tm_shape(shapefile) +
  tm_polygons("Residuals", palette = "RdYlBu", midpoint = 0, title = paste("Residuals:", sdm2_model)) +
  tm_layout(
    outer.margins = c(0.1, 0, 0, 0), 
    main.title = paste("Residuals Map for SDM"), 
    main.title.size = 1.5, 
    main.title.fontface = "bold", 
    main.title.position = "center" 
  )

# Calculate direct, indirect, and total effects for SDM
impacts_sdm <- impacts(sdm2_model, listw = listw, R = 1000)  # Bootstrap for confidence intervals
summary(impacts_sdm)

# Extract data from impacts_sdm$res
effects_direct <- impacts_sdm$res$direct
effects_indirect <- impacts_sdm$res$indirect
effects_total <- impacts_sdm$res$total

# Combine effects into a matrix
effects_matrix <- cbind(Direct = effects_direct, Indirect = effects_indirect, Total = effects_total)

# Ensure the matrix has row names (independent variables)
rownames(effects_matrix) <- attr(impacts_sdm, "bnames")  # Independent variable names

# Visualize direct, indirect, and total effects
barplot(
  t(effects_matrix),
  beside = TRUE,
  legend = rownames(t(effects_matrix)),
  col = c("blue", "green", "red"),
  names.arg = rownames(effects_matrix),
  main = "Direct, Indirect, and Total Effects",
  xlab = "Independent Variables",
  ylab = "Effect Magnitude"
)
