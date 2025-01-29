# Library
library(readxl)
library(corrplot)
library(dplyr)
library(ggcorrplot)
library(GGally)
library(car)
library(lmtest)
library(sf)
library(GWmodel)
library(RColorBrewer)
library(writexl)
library(sp)
library(ggplot2)

# Membaca file Excel
data <- read_excel("/Users/Golda/College/sem 5/spasial/Tugas 3/Data Densitas.xlsx", sheet = "Sheet1")

# Membaca shapefile
shapefile <- st_read("/Users/Golda/College/sem 5/spasial/Tugas 3/shapefile jabar/Jawa_Barat_ADMIN_BPS.shp")
shapefile <- shapefile[shapefile$Kabupaten != "Waduk Cirata", ]
shapefile <- shapefile %>% left_join(data, by = "Kabupaten")
shapefile <- shapefile %>% mutate(centroid = st_centroid(geometry))
shapefile <- shapefile %>% mutate(x = st_coordinates(centroid)[, 1], y = st_coordinates(centroid)[, 2])

# Menampilkan peta Jawa Barat
plot(shapefile["geometry"], col = "grey", axes = TRUE, cex.axis = 0.75)
title(xlab = "Longitude", ylab = "Latitude", cex.lab = 0.75, line = 2.25)

# Menambahkan titik dan label untuk data lokasi
points(x = data$longitude, y = data$latitude)
text(x = data$longitude, y = data$latitude, labels = data$Kabupaten, pos = 4, col = "blue")

# Analisis Korelasi
# Memilih data numerik
data_numeric <- dplyr::select(data, -Kabupaten)

# Membuat matriks korelasi
cor_matrix <- cor(data_numeric)
print(cor_matrix)

# Visualisasi korelasi
corrplot(cor_matrix, method = "color", type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

corrplot(cor_matrix, method = "circle", type = "lower", order = "hclust", 
         tl.col = "red", tl.srt = 45, addCoef.col = "black", 
         col = colorRampPalette(c("red", "white", "blue"))(200))

# Pairplot untuk data numerik
ggpairs(data_numeric, 
        title = "Pairplot dari Data Numeric", 
        lower = list(continuous = "smooth"), 
        diag = list(continuous = "densityDiag"), 
        upper = list(continuous = "cor"))

# Membuat model regresi
# Model awal
model <- lm(DP ~ LS + KB + MK + MM + UHH + LP + JBL, data = data)
summary(model)

# Backward elimination dengan BIC
n <- nrow(data)  # Jumlah observasi
model_BIC <- step(model, direction = "backward", k = log(n))
summary(model_BIC)

# Model terbaik tanpa variabel LP
model_terbaik <- lm(DP ~ LS + KB + MK + MM, data = data)
summary(model_terbaik)

# Menghitung VIF
vif_values <- vif(model_BIC)
print(vif_values)

# Uji asumsi model
# Uji homoskedastisitas
bptest(model_BIC)

# Mengambil residual dari model terbaik
residuals_model <- residuals(model_terbaik)

# Uji Shapiro-Wilk
shapiro_test <- shapiro.test(residuals_model)
print(shapiro_test)

# Q-Q plot
qqnorm(residuals_model, main = "Q-Q Plot of Model Residuals")
qqline(residuals_model, col = "blue")

# Uji independensi
dwtest(model_BIC)

# Menambahkan kolom centroid
shapefile <- shapefile %>% 
  mutate(centroid = st_centroid(geometry),
         x = st_coordinates(centroid)[, 1], 
         y = st_coordinates(centroid)[, 2])

# Visualisasi data
variables <- list(
  list(name = "DP", title = "Densitas Penduduk", ylab = "Nilai DP"),
  list(name = "LP", title = "Laju Pertumbuhan", ylab = "Nilai LP"),
  list(name = "MM", title = "Migrasi Masuk", ylab = "Nilai MM"),
  list(name = "MK", title = "Migrasi Keluar", ylab = "Nilai MK"),
  list(name = "KB", title = "Keluarga Berencana", ylab = "Nilai KB"),
  list(name = "UHH", title = "Umur Harapan Hidup", ylab = "Nilai UHH"),
  list(name = "LS", title = "Lama Sekolah", ylab = "Nilai LS"),
  list(name = "JBL", title = "Jumlah Bayi Lahir", ylab = "Nilai JBL")
)

for (var in variables) {
  ggplot(data = shapefile) + 
    geom_sf(aes_string(fill = var$name), color = "black", size = 0.2) + 
    scale_fill_gradient(low = "yellow", high = "dark green", name = "Gradient") + 
    labs(title = paste("Peta Gradien Nilai", var$title, "Per Wilayah")) + 
    theme_minimal()
  
  boxplot(
    data[[var$name]],
    main = paste("Boxplot", var$title),
    ylab = var$ylab,
    col = "lightblue"
  )
}

# Estimasi Model Global (Metode OLS)
ols <- lm(DP ~ LS + KB + MK + MM + LP, data)
summary(ols)
AIC(ols)

# Uji Asumsi
# 1. Asumsi Normalitas
shapiro.test(residuals(ols))

# 2. Asumsi Multikolinearitas (VIF > 10, maka multikolinearitas)
vif(ols)

# 3. Asumsi Autokorelasi (Gagal menolak H0: tidak terjadi autokorelasi)
dwtest(ols)

# 4. Asumsi Heteroskedastisitas (Menolak H0: terjadi heteroskedastisitas)
bptest(ols)

# GWR dengan 5 Variabel
# Membuat SpatialPointsDataFrame untuk data
Data.spdf <- SpatialPointsDataFrame(data[, 2:3], data)
Data.spdf
head(Data.spdf)

# Matriks jarak antar lokasi observasi
DM <- gw.dist(dp.locat = coordinates(Data.spdf))
DM1 <- as.data.frame(DM)
DM1

# Membuat Data Subset
data_subset <- data[, c(1:8, 10)]
Data.spdf <- SpatialPointsDataFrame(coords = data_subset[, 2:3], data = data_subset)
Data.spdf

# Model GWR dengan Kernel Gaussian
DeVar <- "DP"
InDeVars <- c("LS", "KB", "MK", "MM", "LP")

model.sel <- model.selection.gwr(DeVar, InDeVars, data = Data.spdf, 
                                 kernel = "gaussian", adaptive = FALSE, bw = 1, approach = "CV", dMat = DM)
model.sel

sorted.models <- model.sort.gwr(model.sel, numVars = length(InDeVars), 
                                ruler.vector = model.sel[[2]][, 2])
sorted.models

model.list <- sorted.models[[1]]
model.list

model.view.gwr(DeVar, InDeVars, model.list = model.list)
plot(sorted.models[[2]][, 2], col = "black", pch = 20, lty = 5, 
     main = "Alternative view of GWR model selection procedure", 
     ylab = "CV", xlab = "Model number", type = "b")

# Model GWR dengan Kernel Bisquare
model.sel <- model.selection.gwr(DeVar, InDeVars, data = Data.spdf, 
                                 kernel = "bisquare", adaptive = FALSE, bw = 1, approach = "CV", dMat = DM)
model.sel

sorted.models <- model.sort.gwr(model.sel, numVars = length(InDeVars), 
                                ruler.vector = model.sel[[2]][, 2])
sorted.models

model.list <- sorted.models[[1]]
model.list

model.view.gwr(DeVar, InDeVars, model.list = model.list)
plot(sorted.models[[2]][, 2], col = "black", pch = 20, lty = 5, 
     main = "Alternative view of GWR model selection procedure", 
     ylab = "CV", xlab = "Model number", type = "b")

# Model GWR dengan Kernel Tricube
model.sel <- model.selection.gwr(DeVar, InDeVars, data = Data.spdf, 
                                 kernel = "tricube", adaptive = FALSE, bw = 1, approach = "CV", dMat = DM)
model.sel

sorted.models <- model.sort.gwr(model.sel, numVars = length(InDeVars), 
                                ruler.vector = model.sel[[2]][, 2])
sorted.models

model.list <- sorted.models[[1]]
model.list

model.view.gwr(DeVar, InDeVars, model.list = model.list)
plot(sorted.models[[2]][, 2], col = "black", pch = 20, lty = 5, 
     main = "Alternative view of GWR model selection procedure", 
     ylab = "CV", xlab = "Model number", type = "b")

# Matriks jarak antar lokasi observasi
Data.spdf <- SpatialPointsDataFrame(data[, 2:3], data)
Data.spdf
head(Data.spdf)

DM <- gw.dist(dp.locat = coordinates(Data.spdf))
DM1 <- as.data.frame(DM)
DM1

# Menentukan bandwidth optimum
bw.gauss <- bw.gwr(DP ~ LS + KB + MK + MM + LP, data = Data.spdf, approach = "CV", kernel = "gaussian", adaptive = F)
bw.gauss

bw.bisq <- bw.gwr(DP ~ LS + KB + MK + MM + LP, data = Data.spdf, approach = "CV", kernel = "bisquare", adaptive = F)
bw.bisq

bw.tric <- bw.gwr(DP ~ LS + KB + MK + MM + LP, data = Data.spdf, approach = "CV", kernel = "tricube", adaptive = F)
bw.tric

# Estimasi parameter model GWR
gwr.gaus <- gwr.basic(DP ~ LS + KB + MK + MM + LP, data = Data.spdf, bw = bw.gauss, kernel = "gaussian", adaptive = F)
print(gwr.gaus)

gwr.bisq <- gwr.basic(DP ~ LS + KB + MK + MM + LP, data = Data.spdf, bw = bw.bisq, kernel = "bisquare", adaptive = F)
print(gwr.bisq)

gwr.tric <- gwr.basic(DP ~ LS + KB + MK + MM + LP, data = Data.spdf, bw = bw.tric, kernel = "tricube", adaptive = F)
print(gwr.tric)

# Buat tabel untuk nilai R-squared dan AIC
result_table <- data.frame(
  Model = c("OLS", "GWR Gaussian", "GWR Bisquare", "GWR Tricube"),
  R_Squared = c(summary(ols)$r.squared,
                gwr.gaus$GW.diagnostic$gw.R2,
                gwr.bisq$GW.diagnostic$gw.R2,
                gwr.tric$GW.diagnostic$gw.R2),
  AIC = c(AIC(ols),
          gwr.gaus$GW.diagnostic$AIC,
          gwr.bisq$GW.diagnostic$AIC,
          gwr.tric$GW.diagnostic$AIC)
)
print(result_table)

# Membentuk kolom residual OLS
residuals_ols <- residuals(ols)
resid_data <- data.frame(Kabupaten = data$Kabupaten, res_ols = residuals_ols)

# Membentuk kolom p-value GWR Gaussian
pvalue_gauss <- data.frame(
  Intercept_p = 2 * (1 - pnorm(abs(gwr.gaus$SDF$Intercept / gwr.gaus$SDF$Intercept_SE))),
  LS_p = 2 * (1 - pnorm(abs(gwr.gaus$SDF$LS / gwr.gaus$SDF$LS_SE))),
  KB_p = 2 * (1 - pnorm(abs(gwr.gaus$SDF$KB / gwr.gaus$SDF$KB_SE))),
  MK_p = 2 * (1 - pnorm(abs(gwr.gaus$SDF$MK / gwr.gaus$SDF$MK_SE))),
  MM_p = 2 * (1 - pnorm(abs(gwr.gaus$SDF$MM / gwr.gaus$SDF$MM_SE))),
  LP_p = 2 * (1 - pnorm(abs(gwr.gaus$SDF$LP / gwr.gaus$SDF$LP_SE)))
)

# Menggabungkan data koefisien GWR, residual, dan p-value
gausscap <- data.frame(
  LS_coef = gwr.gaus$SDF$LS,
  KB_coef = gwr.gaus$SDF$KB,
  MK_coef = gwr.gaus$SDF$MK,
  MM_coef = gwr.gaus$SDF$MM,
  LP_coef = gwr.gaus$SDF$LP,
  residual_gauss = gwr.gaus$SDF$residual
)

# Gabungkan dengan data kabupaten, residual OLS, dan p-value
gausstab <- cbind(
  data[, c("Kabupaten", "DP")],
  gausscap,
  resid_data,
  pvalue_gauss
)

# Pastikan kolom ID untuk penggabungan
gausstab$ID <- 1:nrow(gausstab)
shapefile$ID <- 1:nrow(shapefile)

# Hapus kolom `Kabupaten` yang redundan
gausstab <- gausstab %>% dplyr::select(-Kabupaten)

# Gabungkan data dengan pengaturan nama kolom duplikat
merged_data_gauss <- merge(shapefile, gausstab, by = "ID", all.x = TRUE, suffixes = c("_shapefile", "_gausstab"))

# Hapus kolom ID dari hasil penggabungan jika tidak diperlukan
merged_data_gauss <- merged_data_gauss %>% dplyr::select(-ID)

# Menambahkan Kolom 'Kelompok' berdasarkan p-value
merged_data_gauss$Kelompok <- ifelse(merged_data_gauss$LS_p < 0.05 & merged_data_gauss$MK_p < 0.05 & merged_data_gauss$MM_p < 0.05  & merged_data_gauss$LP_p < 0.05, 5, 
                                     ifelse(merged_data_gauss$LS_p < 0.05 & merged_data_gauss$MK_p < 0.05 & merged_data_gauss$MM_p < 0.05  & merged_data_gauss$KB_p < 0.05, 4, 
                                            ifelse(merged_data_gauss$LS_p < 0.05 & merged_data_gauss$MM_p < 0.05 & merged_data_gauss$LP_p < 0.05, 3, 
                                                   ifelse(merged_data_gauss$LS_p < 0.05 & merged_data_gauss$KB_p < 0.05 & merged_data_gauss$MK_p < 0.05, 2, 
                                                          ifelse(merged_data_gauss$LS_p < 0.05 & merged_data_gauss$KB_p < 0.05, 1,0))))) 

# Peta Residual untuk Regresi OLS
ggplot() + 
  geom_sf(data = merged_data_gauss, aes(fill = res_ols)) + 
  scale_fill_gradient2(low = "red", high = "royalblue", midpoint = 0, name = "Gradient") +
  theme_minimal() + 
  ggtitle("Global Regression Residual Map (OLS)")

# Nilai minimum dari kolom 
min_value <- min(merged_data_gauss$res_ols, na.rm = TRUE) 
print(min_value) 

# Nilai maksimum dari kolom 
max_value <- max(merged_data_gauss$res_ols, na.rm = TRUE) 
print(max_value) 

# Peta Residual untuk GWR Gaussian
ggplot() + 
  geom_sf(data = merged_data_gauss, aes(fill = residual_gauss)) + 
  scale_fill_gradient2(low = "red", high = "royalblue", midpoint = 0, name = "Gradient") +
  theme_minimal() + 
  ggtitle("GWR Gaussian Residual Map")

# Nilai minimum dari kolom 
min_value <- min(merged_data_gauss$residual_gauss, na.rm = TRUE) 
print(min_value) 

# Nilai maksimum dari kolom 
max_value <- max(merged_data_gauss$residual_gauss, na.rm = TRUE) 
print(max_value) 

# Peta Berdasarkan Variabel Signifikan
ggplot() + 
  geom_sf(data = merged_data_gauss, aes(fill = factor(Kelompok))) + 
  scale_fill_manual(values = c("white", "lightblue", "lightgreen", "yellow", "pink"), 
                    breaks = c(1, 2, 3, 4, 5), 
                    labels = c("Group 1: No Variables", "Group 2: LS", "Group 3: LS, KB", 
                               "Group 4: LS, KB, MK", "Group 5: LS, KB, MK, MM, LP")) + 
  labs(fill = "Kelompok") + 
  ggtitle("Peta Berdasarkan Variabel yang Signifikan")

# Peta Persentase Densitas Penduduk (Variabel Y)
ggplot() + 
  geom_sf(data = merged_data_gauss, aes(fill = DP_gausstab)) + 
  scale_fill_gradient(low = "yellow", high = "darkred") + 
  labs(fill = "Persentase Densitas Penduduk") +
  ggtitle("Peta Persentase Densitas Penduduk") + 
  theme_minimal()

# Peta Keragaman Spasial
coef_variables <- list(
  list(name = "LS_coef", title = "Keragaman Spasial Beta (LS)"),
  list(name = "KB_coef", title = "Keragaman Spasial Beta (KB)"),
  list(name = "MK_coef", title = "Keragaman Spasial Beta (MK)"),
  list(name = "MM_coef", title = "Keragaman Spasial Beta (MM)"),
  list(name = "LP_coef", title = "Keragaman Spasial Beta (LP)")
)

for (coef in coef_variables) {
  print(
    ggplot() +  
      geom_sf(data = merged_data_gauss, aes_string(fill = coef$name)) +  
      scale_fill_gradient(low = "white", high = "purple", name = "Gradient") +  
      ggtitle(coef$title) + 
      theme_minimal()
  )
}

# Peta p-value
pvalue_variables <- list(
  list(name = "LS_p", title = "Peta p-value Beta (LS)"),
  list(name = "KB_p", title = "Peta p-value Beta (KB)"),
  list(name = "MK_p", title = "Peta p-value Beta (MK)"),
  list(name = "MM_p", title = "Peta p-value Beta (MM)"),
  list(name = "LP_p", title = "Peta p-value Beta (LP)")
)

for (pval in pvalue_variables) {
  print(
    ggplot() +  
      geom_sf(data = merged_data_gauss, aes_string(fill = pval$name)) +  
      scale_fill_gradient2(low = "white", high = "orange", midpoint = 0.05, name = "P-Value") +  
      ggtitle(pval$title) + 
      theme_minimal()
  )
}
