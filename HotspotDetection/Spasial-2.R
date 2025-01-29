# Load necessary libraries
library(sf)
library(ggplot2)

# Purely Spatial Dataframes
hotspot_data <- list(
  "2020" = data.frame(Lokasi = c("Kota Banjarmasin")),
  "2021" = data.frame(Lokasi = c("Barito Kuala", "Kota Banjarmasin")),
  "2022" = data.frame(Lokasi = c("Kota Banjarmasin"))
)

# Space-Time Dataframes
sthotspot_data <- list(
  "2020" = data.frame(Lokasi = c("Kota Banjarmasin")),
  "2021" = data.frame(Lokasi = c("Tabalong", "Balangan", "Hulu Sungai Utara", "Hulu Sungai Tengah", "Hulu Sungai Selatan", "Tapin", "Kotabaru", "Barito Kuala")),
  "2022" = data.frame(Lokasi = c("Tanah Laut", "Kota Banjarbaru", "Banjar"))
)

# Path to shapefile
shapefile_path <- "/Users/Golda/College/sem 5/spasial/Tugas 2/Shapefile Kalsel/Kalimantan_Selatan_ADMIN_BPS.shp"

# Function to plot hotspot maps
plot_hotspot <- function(year, data, title_prefix, cluster_colors) {
  shapefile <- st_read(shapefile_path)
  shapefile$cluster <- ifelse(shapefile$Kabupaten %in% data[[year]]$Lokasi, "Cluster", "Non-Cluster")
  
  ggplot() +
    geom_sf(data = shapefile, aes(fill = cluster), color = "black") +
    geom_sf_text(data = shapefile, aes(label = Kabupaten), size = 3, color = "black", check_overlap = TRUE) +
    scale_fill_manual(values = cluster_colors) +
    theme_minimal() +
    labs(
      title = paste0(title_prefix, " ", year),
      fill = "Category"
    )
}

# Plot purely spatial hotspots
for (year in names(hotspot_data)) {
  print(plot_hotspot(
    year,
    hotspot_data,
    "Hotspot Map of TBC in South Kalimantan",
    c("Cluster" = "red", "Non-Cluster" = "yellow")
  ))
}

# Plot space-time hotspots
for (year in names(sthotspot_data)) {
  print(plot_hotspot(
    year,
    sthotspot_data,
    "ST: Significant Cluster Map in",
    c("Cluster" = "blue", "Non-Cluster" = "yellow")
  ))
}
