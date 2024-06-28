# Calculate area (! number of DEM pixels !) and count stations in 500 m steps for every GMBA_V2_ID 

library(raster)
library(terra)
library(sf)
library(ggplot2)
library(dplyr)
library(sp)


# Preparation ####

# Read GMBA shp-File
GMBA <- read_sf("data-raw/GMBA/GMBA_Inventory_v2.0_standard_300.shx")
# Clean geometry
GMBA_clean <- st_make_valid(GMBA)

# Read in tif as a raster 
DEM <- raster("data-raw/ETOPO_2022_v1_60s_N90W180_bed.tif")

# Get the CRS of the raster
raster_crs <- crs(DEM)
# Transform the CRS of the datasets to match the raster's CRS
GMBA <- st_transform(GMBA, raster_crs)
GMBA_clean <- st_transform(GMBA_clean, raster_crs)
rm(raster_crs)

# Read in the dataset
`inventory-01-read` <- readRDS("data/inventory-01-read.rds")
# Create points with coordinates for inventory-01-read
inventory <- st_as_sf(`inventory-01-read`, coords = c("Longitude", "Latitude"), crs = 4326)
rm(`inventory-01-read`)

# Read GMBA shp-File
GMBA <- read_sf("data-raw/GMBA/GMBA_Inventory_v2.0_standard_300.shx")
# Clean geometry
GMBA_clean <- st_make_valid(GMBA)

# Cut points
inventory_GMBA <- st_join(inventory, GMBA_clean)



# Calculate the area ####

# Define the breaks and labels for altitude bins
breaks <- seq(-500, 9000, by = 500)
labels <- paste0("(", breaks[-length(breaks)], "-", breaks[-1], "] m")

# Create an empty list to store the areas
areas <- list()

# Hier wird als Name in der Liste das GMBA_V2_ID angegeben und kein df in der liste.

# Loop over each row in GMBA_1
# for(i in 1:10) { # test a few rows
for(i in 1:nrow(GMBA)) {
  
  # Get the current GMBA_V2_ID
  current_GMBA_V2_ID <- GMBA$GMBA_V2_ID[i]
  
  # Extract the polygon for the current row
  polygon <- st_geometry(GMBA[i, ])
  
  # Convert the sf object into a Spatial object
  polygon_sp <- as(polygon, "Spatial")
  
  # Crop the raster to the polygon
  cropped_raster <- crop(DEM, polygon_sp)
  cropped_raster_masked <- mask(cropped_raster, polygon_sp)
  
  # Extract the altitude values from the cropped raster
  altitudes <- values(cropped_raster_masked)
  
  # Divide the altitude values into the defined bins
  altitude_bins <- cut(altitudes, breaks = breaks, labels = labels, include.lowest = TRUE)
  
  # Calculate the area for each altitude interval
  npixel_per_bin <- tapply(altitudes, altitude_bins, function(x) sum(!is.na(x)))
  
  # Store the result in the list
  areas[[as.character(current_GMBA_V2_ID)]] <- npixel_per_bin
  
}


# Count the Stations ####

# Cut the Altitude into bins
inventory_GMBA$Altitude_bin <- cut(inventory_GMBA$`Altitude (m)`, breaks = breaks, labels = labels)

# Count the number of stations in each altitude bin for each GMBA_V2_ID polygon
station_count <- inventory_GMBA %>%
  group_by(GMBA_V2_ID, Altitude_bin) %>%
  summarise(n = n(), .groups = 'drop')



# Initialize an empty list to store the results
station_counts <- list()

# Loop over each row in the GMBA DataFrame
for(i in 1:nrow(GMBA)) {
  # Get the current GMBA_V2_ID
  current_GMBA_V2_ID <- GMBA$GMBA_V2_ID[i]
  
  # Subset the inventory_GMBA DataFrame for the current GMBA_V2_ID
  inventory_subset <- inventory_GMBA[inventory_GMBA$GMBA_V2_ID == current_GMBA_V2_ID, ]
  
  # Cut the Altitude into bins
  inventory_subset$Altitude_bin <- cut(inventory_subset$`Altitude (m)`, breaks = breaks, labels = labels)
  
  # Count the number of stations in each altitude bin
  station_count <- table(inventory_subset$Altitude_bin)
  
  # Store the result in the list
  station_counts[[as.character(current_GMBA_V2_ID)]] <- station_count
}

# Alle Werte mit 0 auf NA setzen
station_counts <- lapply(station_counts, function(x) {
  # Ersetzen Sie alle Werte, die 0 sind, durch NA
  x[x == 0] <- NA
  # Geben Sie die modifizierte Liste zurück
  return(x)
})


# Join area und station count ####

narea <- list()

# Durchlaufen Sie jede Zeile in GMBA
for(i in 1:nrow(GMBA)) {
  
  # Holen Sie sich die aktuelle GMBA_V2_ID
  current_GMBA_V2_ID <- GMBA$GMBA_V2_ID[i]
  
  # Erstellen Sie ein neues DataFrame und speichern Sie es in der Liste
  narea[[as.character(current_GMBA_V2_ID)]] <- tibble(
    altitude_bin = names(areas[[as.character(current_GMBA_V2_ID)]]),
    ares = areas[[as.character(current_GMBA_V2_ID)]],
    station_counts = station_counts[[as.character(current_GMBA_V2_ID)]]
  )
}

# Change header names
narea <- lapply(narea, function(df) {
  rename(df, a_bin = altitude_bin, area = ares, n = station_counts)
})

# Save data
saveRDS(narea, file = "data/GMBA-inv_area_counts.rds")

