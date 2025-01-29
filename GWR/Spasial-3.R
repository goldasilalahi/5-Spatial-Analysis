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
library(maptools)
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
data_numeric <- data %>% select(-Kabupaten)

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
# Peta densitas penduduk
ggplot(data = shapefile) + 
  geom_sf(aes(fill = DP), color = "black", size = 0.2) + 
  scale_fill_gradient(low = "yellow", high = "dark green", name = "Gradient") + 
  labs(title = "Peta Gradien Nilai Densitas Penduduk Per Wilayah") + 
  theme_minimal()

# Boxplot densitas penduduk
boxplot(data$DP, 
        main = "Boxplot Densitas Penduduk", 
        ylab = "Nilai DP", 
        col = "lightblue")

# Peta laju pertumbuhan
ggplot(data = shapefile) + 
  geom_sf(aes(fill = LP), color = "black", size = 0.2) + 
  scale_fill_gradient(low = "yellow", high = "dark green", name = "Gradient") + 
  labs(title = "Peta Gradien Nilai Laju Pertumbuhan Per Wilayah") + 
  theme_minimal()

# Boxplot laju pertumbuhan
boxplot(data$LP, 
        main = "Boxplot Laju Pertumbuhan", 
        ylab = "Nilai LP", 
        col = "lightblue")

# Peta migrasi masuk
ggplot(data = shapefile) + 
  geom_sf(aes(fill = MM), color = "black", size = 0.2) + 
  scale_fill_gradient(low = "yellow", high = "dark green", name = "Gradient") + 
  labs(title = "Peta Gradien Nilai Migrasi Masuk Per Wilayah") + 
  theme_minimal()

# Boxplot migrasi masuk
boxplot(data$MM, 
        main = "Boxplot Migrasi Masuk", 
        ylab = "Nilai MM", 
        col = "lightblue")

# Peta migrasi keluar
ggplot(data = shapefile) + 
  geom_sf(aes(fill = MK), color = "black", size = 0.2) + 
  scale_fill_gradient(low = "yellow", high = "dark green", name = "Gradient") + 
  labs(title = "Peta Gradien Nilai Migrasi Keluar Per Wilayah") + 
  theme_minimal()

# Boxplot migrasi keluar
boxplot(data$MK, 
        main = "Boxplot Migrasi Keluar", 
        ylab = "Nilai MK", 
        col = "lightblue")

# Peta keluarga berencana
ggplot(data = shapefile) + 
  geom_sf(aes(fill = KB), color = "black", size = 0.2) + 
  scale_fill_gradient(low = "yellow", high = "dark green", name = "Gradient") + 
  labs(title = "Peta Gradien Nilai Persentase Keluarga Berencana Per Wilayah") + 
  theme_minimal()

# Boxplot keluarga berencana
boxplot(data$KB, 
        main = "Boxplot Persentase Keluarga Berencana", 
        ylab = "Nilai KB", 
        col = "lightblue")

# Peta umur harapan hidup
ggplot(data = shapefile) + 
  geom_sf(aes(fill = UHH), color = "black", size = 0.2) + 
  scale_fill_gradient(low = "yellow", high = "dark green", name = "Gradient") + 
  labs(title = "Peta Gradien Nilai Umur Harapan Hidup Per Wilayah") + 
  theme_minimal()

# Boxplot umur harapan hidup
boxplot(data$UHH, 
        main = "Boxplot Umur Harapan Hidup", 
        ylab = "Nilai UHH", 
        col = "lightblue")

# Peta lama sekolah
ggplot(data = shapefile) + 
  geom_sf(aes(fill = LS), color = "black", size = 0.2) + 
  scale_fill_gradient(low = "yellow", high = "dark green", name = "Gradient") + 
  labs(title = "Peta Gradien Nilai Rata-Rata Lama Sekolah Per Wilayah") + 
  theme_minimal()

# Boxplot lama sekolah
boxplot(data$LS, 
        main = "Boxplot Lama Sekolah", 
        ylab = "Nilai LP", 
        col = "lightblue")

# Peta jumlah bayi lahir
ggplot(data = shapefile) + 
  geom_sf(aes(fill = JBL), color = "black", size = 0.2) + 
  scale_fill_gradient(low = "yellow", high = "dark green", name = "Gradient") + 
  geom_text(aes(x = x, y = y, label = Kabupaten), size = 2.5, color = "black") + 
  labs(title = "Peta Gradien Nilai Jumlah Bayi Lahir Per Wilayah") + 
  theme_minimal()

# Boxplot jumlah bayi lahir
boxplot(data$JBL, 
        main = "Boxplot Jumlah Bayi Lahir", 
        ylab = "Nilai LP", 
        col = "lightblue")

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

# Membuat kolom ID untuk menggabungkan data shapefile
gausstab$ID <- 1:nrow(gausstab)
shapefile$ID <- 1:nrow(shapefile)

# Menggabungkan data shapefile dengan data hasil GWR Gaussian
merged_data_gauss <- merge(shapefile, gausstab, by = "ID", all.x = TRUE)
merged_data_gauss <- merged_data_gauss %>% select(-ID)

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
  geom_sf(data = merged_data_gauss, aes(fill = DP.x)) + 
  scale_fill_gradient(low = "yellow", high = "darkred") + 
  labs(fill = "Persentase Densitas Penduduk") +
  ggtitle("Peta Persentase Densitas Penduduk") + 
  theme_minimal()

# Peta Keragaman Spasial Beta 1 (LS_coef)
ggplot() +  
  geom_sf(data = merged_data_gauss, aes(fill = LS_coef)) +  
  scale_fill_gradient(low = "white", high = "purple", name = "Gradient") +  
  ggtitle("Keragaman Spasial Beta (LS)") + 
  theme_minimal()

# Peta Keragaman Spasial Beta 2 (KB_coef)
ggplot() +  
  geom_sf(data = merged_data_gauss, aes(fill = KB_coef)) +  
  scale_fill_gradient(low = "white", high = "purple", name = "Gradient") +  
  ggtitle("Keragaman Spasial Beta (KB)") + 
  theme_minimal()

# Peta Keragaman Spasial Beta 3 (MK_coef)
ggplot() +  
  geom_sf(data = merged_data_gauss, aes(fill = MK_coef)) +  
  scale_fill_gradient(low = "white", high = "purple", name = "Gradient") +  
  ggtitle("Keragaman Spasial Beta (MK)") + 
  theme_minimal()

# Peta Keragaman Spasial Beta 4 (MM_coef)
ggplot() +  
  geom_sf(data = merged_data_gauss, aes(fill = MM_coef)) +  
  scale_fill_gradient(low = "white", high = "purple", name = "Gradient") +  
  ggtitle("Keragaman Spasial Beta (MM)") + 
  theme_minimal()

# Peta Keragaman Spasial Beta 5 (LP_coef)
ggplot() +  
  geom_sf(data = merged_data_gauss, aes(fill = LP_coef)) +  
  scale_fill_gradient(low = "white", high = "purple", name = "Gradient") +  
  ggtitle("Keragaman Spasial Beta (LP)") + 
  theme_minimal()

# Peta P-Value untuk Beta 1 (LS_p)
ggplot() +  
  geom_sf(data = merged_data_gauss, aes(fill = LS_p)) +  
  scale_fill_gradient2(low = "white", high = "orange", midpoint = 0.05, name = "P-Value") +  
  ggtitle("Peta P-value Beta (LS)") + 
  theme_minimal()

# Peta P-Value untuk Beta 2 (KB_p)
ggplot() +  
  geom_sf(data = merged_data_gauss, aes(fill = KB_p)) +  
  scale_fill_gradient2(low = "white", high = "orange", midpoint = 0.05, name = "P-Value") +  
  ggtitle("Peta P-value Beta (KB)") + 
  theme_minimal()

# Peta P-Value untuk Beta 3 (MK_p)
ggplot() +  
  geom_sf(data = merged_data_gauss, aes(fill = MK_p)) +  
  scale_fill_gradient2(low = "white", high = "orange", midpoint = 0.05, name = "P-Value") +  
  ggtitle("Peta P-value Beta (MK)") + 
  theme_minimal()

# Peta P-Value untuk Beta 4 (MM_p)
ggplot() +  
  geom_sf(data = merged_data_gauss, aes(fill = MM_p)) +  
  scale_fill_gradient2(low = "white", high = "orange", midpoint = 0.05, name = "P-Value") +  
  ggtitle("Peta P-value Beta (MM)") + 
  theme_minimal()

# Peta P-Value untuk Beta 5 (LP_p)
ggplot() +  
  geom_sf(data = merged_data_gauss, aes(fill = LP_p)) +  
  scale_fill_gradient2(low = "white", high = "orange", midpoint = 0.05, name = "P-Value") +  
  ggtitle("Peta P-value Beta (LP)") + 
  theme_minimal()