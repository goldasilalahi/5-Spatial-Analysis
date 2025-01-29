# Memuat library yang diperlukan
library(readxl)
library(dplyr)
library(leaflet)
library(spatstat)
library(sf)
library(ggplot2)

# Membaca dataset dari file Excel
dataset <- read_excel("/Users/Golda/College/sem 5/spasial/submit/Data Sebaran Lampu Lalu Lintas.xlsx")

# Menampilkan data awal
head(dataset)

# Membersihkan data: Menghapus data dengan latitude atau longitude bernilai 0
dataset <- dataset[!(dataset$latitude == 0 | dataset$longitude == 0), ]

# Konversi kolom longitude dan latitude menjadi numerik
dataset$longitude <- as.numeric(dataset$longitude)
dataset$latitude <- as.numeric(dataset$latitude)

# Mengubah latitude dan longitude dari satuan ratusan ribu menjadi derajat
dataset$longitude <- dataset$longitude / 1000000
dataset$latitude <- dataset$latitude / 1000000

# Memfilter data sesuai dengan rentang wilayah yang valid (DKI Jakarta)
dataset_clean <- dataset %>% filter(
  longitude >= 106 & longitude <= 107,
  latitude >= -7 & latitude <= -6
)

# Membuat peta interaktif dengan leaflet
leaflet() %>%
  addTiles() %>% # Menambahkan layer peta dasar (OpenStreetMap)
  addCircleMarkers(
    data = dataset_clean,
    ~longitude, ~latitude,
    color = "red",
    radius = 3, # Ukuran marker
    fillOpacity = 0.8,
    popup = ~paste("Lampu:", seq_along(longitude))
  ) %>%
  setView(lng = 106.845, lat = -6.214, zoom = 12) %>%
  addControl(
    html = "<strong>Sebaran Lampu Lalu Lintas di Provinsi DKI Jakarta</strong>",
    position = "topleft"
  )

# Data scaling untuk menentukan window spasial
x_max <- max(dataset_clean$longitude)
x_min <- min(dataset_clean$longitude)
y_max <- max(dataset_clean$latitude)
y_min <- min(dataset_clean$latitude)

# Membuat objek PPP (point pattern process)
lampumerah <- ppp(
  x = dataset_clean$longitude,
  y = dataset_clean$latitude,
  window = owin(c(x_min, x_max), c(y_min, y_max))
)

# Quadrat count
Q <- quadratcount(lampumerah, nx = 5, ny = 5)
View(Q)

# Plot PPP dan Quadrat Count
plot(lampumerah, axes = TRUE, main = "Persebaran Lampu Lalu Lintas Provinsi DKI Jakarta")
plot(Q, add = TRUE, col = "green")

# Uji Quadrat Test
qt <- quadrat.test(lampumerah, nx = 5, ny = 5)
qt

# Plot hasil Quadrat Test
plot(lampumerah, pch = 20, cols = "red", main = "Lampu Lalu Lintas - Quadrat Test")
plot(qt, add = TRUE, cex = 1)

# Membaca shapefile untuk peta administrasi Jakarta
jakbar <- st_read("/Users/Golda/College/sem 5/spasial/shp file/KOTA_JAKARTA_BARAT/ADMINISTRASI_LN_25K.shp")
jakpus <- st_read("/Users/Golda/College/sem 5/spasial/shp file/KOTA_JAKARTA_PUSAT/ADMINISTRASI_LN_25K.shp")
jaksel <- st_read("/Users/Golda/College/sem 5/spasial/shp file/KOTA_JAKARTA_SELATAN/ADMINISTRASI_LN_25K.shp")
jaktim <- st_read("/Users/Golda/College/sem 5/spasial/shp file/KOTA_JAKARTA_TIMUR/ADMINISTRASI_LN_25K.shp")
jakut <- st_read("/Users/Golda/College/sem 5/spasial/shp file/KOTA_JAKARTA_UTARA/ADMINISTRASI_LN_25K.shp")

# Menggabungkan semua shapefile menjadi satu objek
jktprovince <- rbind(jakbar, jakpus, jaksel, jaktim, jakut)

# Memperbaiki geometri shapefile jika diperlukan
jktprovince <- st_make_valid(jktprovince)

# Mengubah data titik menjadi objek sf (spatial features)
dataset_clean_sf <- st_as_sf(dataset_clean, coords = c("longitude", "latitude"), crs = 4326)

# Plot data menggunakan ggplot2
ggplot() +
  geom_sf(data = jktprovince, fill = "lightgrey", color = "black") +
  geom_sf(data = dataset_clean_sf, color = "red", size = 2, alpha = 0.7) +
  labs(
    title = "Sebaran Lampu Lalu Lintas di Provinsi DKI Jakarta",
    subtitle = "Data Titik di atas Peta Administrasi Jakarta"
  ) +
  theme_minimal()

# Hipotesis dan Aturan Penolakan
# H0: Distribusi lampu lalu lintas adalah acak (CSR).
# H1: Distribusi lampu lalu lintas tidak acak, dan menunjukkan pola clustered atau uniform.

# Aturan penolakan:
# Jika RV > 1, maka tolak H0 (pola clustered).
# Jika RV < 1, maka tolak H0 (pola uniform).
# Jika RV ≈ 1, gagal menolak H0 (pola CSR/acak).

# Konversi data ke objek ppp dengan proyeksi UTM
sf_data_utm <- st_transform(dataset_clean_sf, crs = 32648)
utm_coords <- st_coordinates(sf_data_utm)
x <- utm_coords[, 1]
y <- utm_coords[, 2]

# Membuat jendela spasial berdasarkan rentang koordinat
window <- owin(xrange = range(x), yrange = range(y))
data_ppp <- ppp(x = x, y = y, window = window)

# Quadrat Count dan Relative Variance
quadrat_counts <- quadratcount(data_ppp, nx = 5, ny = 5)
plot(quadrat_counts)

# Menghitung varians, rata-rata, dan Relative Variance (RV)
quadrat_values <- as.vector(quadrat_counts)
var_quadrat <- var(quadrat_values)
mean_quadrat <- mean(quadrat_values)
RV <- var_quadrat / mean_quadrat

# Kesimpulan berdasarkan nilai RV
if (RV > 1) {
  cat("Kesimpulan: Pola distribusi lampu lalu lintas adalah clustered.\n")
} else if (RV < 1) {
  cat("Kesimpulan: Pola distribusi lampu lalu lintas adalah uniform.\n")
} else {
  cat("Kesimpulan: Pola distribusi lampu lalu lintas adalah acak (CSR).\n")
}
