library(raster)
library(stars)
library(terra)
library(sf)
library(ggplot2)
library(dplyr)
library(sp)


# Prep ####

# Save location
save_l <- "Overview_Finished/"

# Read in the dataset
`inventory-01-read` <- readRDS("data/inventory-01-read.rds")
# Read GMBA shp-File
GMBA <- read_sf("data-raw/GMBA/GMBA_Inventory_v2.0_standard_300.shx")
# Clean geometry
GMBA_clean <- st_make_valid(GMBA)
# Create points with coordinates for inventory-01-read
inventory <- st_as_sf(`inventory-01-read`, coords = c("Longitude", "Latitude"), crs = 4326)
# Cut points
inventory_GMBA <- st_join(inventory, GMBA_clean)
rm(`inventory-01-read`)

# Path to tif File
DEM_path <- "data-raw/ETOPO_2022_v1_60s_N90W180_bed.tif"
# Read in tif as a raster 
DEM <- raster(DEM_path)
rm(DEM_path)

# Get the CRS of the raster
raster_crs <- crs(DEM)

# Transform the CRS of the datasets to match the raster's CRS
GMBA <- st_transform(GMBA, raster_crs)
GMBA_clean <- st_transform(GMBA_clean, raster_crs)
inventory <- st_transform(inventory, raster_crs)
inventory_GMBA <- st_transform(inventory_GMBA, raster_crs)
rm(raster_crs)


# Calculation ####

# Define the breaks and labels for altitude bins
breaks <- seq(-500, 9000, by = 500)
labels <- paste0("(", breaks[-length(breaks)], "-", breaks[-1], "] m")


# 1. Categorize each station into altitude bins
inventory_GMBA$altitude_bin <- cut(inventory_GMBA$`Altitude (m)`, breaks = breaks, labels = labels, include.lowest = TRUE)

# 2. Calculate the area of each altitude bin in each region of "Level_01" to "Level_04" using the raster
# Extract the altitude information from the raster
altitude <- extract(DEM, inventory_GMBA)

# Add the altitude information to the inventory_GMBA DataFrame
inventory_GMBA$altitude <- altitude

# check differences
# plot(inventory_GMBA$`Altitude (m)`, inventory_GMBA$altitude)
# plot(inventory_GMBA$`Altitude (m)` - inventory_GMBA$altitude)
# hist(inventory_GMBA$`Altitude (m)` - inventory_GMBA$altitude, 50)

# Categorize the altitude information into altitude bins
# inventory_GMBA$altitude_bin <- cut(inventory_GMBA$altitude, breaks = breaks, labels = labels, include.lowest = TRUE)

# Calculate the area of each altitude bin in each region of "Level_01" to "Level_04"
# area_by_altitude_and_region <- inventory_GMBA %>%
#   group_by(Level_01, Level_02, Level_03, Level_04, altitude_bin) %>%
#   summarise(area = sum(!is.na(altitude)) * (1.85)^2)  # Use 1.85 km as the resolution

# 3. Count the number of stations in each altitude bin and region
stations_by_altitude_and_region <- inventory_GMBA %>%
  group_by(Level_01, Level_02, Level_03, Level_04, altitude_bin) %>%
  summarise(n_stations = n())

# Remove the geometries from the DataFrames
stations_by_altitude_and_region_nogeom <- st_set_geometry(stations_by_altitude_and_region, NULL)
# area_by_altitude_and_region_nogeom <- st_set_geometry(area_by_altitude_and_region, NULL)

# Perform the join and calculate the number of stations per km²
# stations_by_altitude_and_region <- stations_by_altitude_and_region_nogeom %>%
#   left_join(area_by_altitude_and_region_nogeom, by = c("Level_01", "Level_02", "Level_03", "Level_04", "altitude_bin")) %>%
#   mutate(stations_per_km2 = n_stations / area)



# # For Level_01
# stations_by_altitude_and_region_Level_01 <- inventory_GMBA %>%
#   group_by(Level_01, altitude_bin) %>%
#   summarise(n_stations = n(), area = sum(!is.na(altitude)) * (1.85)^2) %>%
#   mutate(stations_per_km2 = n_stations / area)
# 
# # For Level_02
# stations_by_altitude_and_region_Level_02 <- inventory_GMBA %>%
#   group_by(Level_02, altitude_bin) %>%
#   summarise(n_stations = n(), area = sum(!is.na(altitude)) * (1.85)^2) %>%
#   mutate(stations_per_km2 = n_stations / area)
# 
# # For Level_03
# stations_by_altitude_and_region_Level_03 <- inventory_GMBA %>%
#   group_by(Level_03, altitude_bin) %>%
#   summarise(n_stations = n(), area = sum(!is.na(altitude)) * (1.85)^2) %>%
#   mutate(stations_per_km2 = n_stations / area)
# 
# # For Level_04
# stations_by_altitude_and_region_Level_04 <- inventory_GMBA %>%
#   group_by(Level_04, altitude_bin) %>%
#   summarise(n_stations = n(), area = sum(!is.na(altitude)) * (1.85)^2) %>%
#   mutate(stations_per_km2 = n_stations / area)















# test ####
# Erstellen Sie eine leere Liste, um die zugeschnittenen Raster zu speichern
cropped_rasters <- list()

# Schleife über jede Zeile in GMBA_1
for(i in 1:nrow(GMBA_1)) {
  # Extrahieren Sie das Polygon für die aktuelle Zeile
  polygon <- st_geometry(GMBA_1[i, ])
  
  # Konvertieren Sie das sf-Objekt in ein Spatial-Objekt
  polygon_sp <- as(polygon, "Spatial")
  
  # Zuschneiden des Rasters auf das Polygon
  cropped_raster <- crop(DEM, polygon_sp)
  
  # Speichern Sie das zugeschnittene Raster in der Liste
  cropped_rasters[[i]] <- cropped_raster
}






# Erstellen Sie eine leere Liste, um die Flächen zu speichern
areas <- list()

# Schleife über jede Zeile in GMBA_1
# for(i in 1:10) { # test a few rows
for(i in 1:nrow(GMBA)) {
  # Extrahieren Sie das Polygon für die aktuelle Zeile
  polygon <- st_geometry(GMBA[i, ])
  
  # Konvertieren Sie das sf-Objekt in ein Spatial-Objekt
  polygon_sp <- as(polygon, "Spatial")
  
  # Zuschneiden des Rasters auf das Polygon
  cropped_raster <- crop(DEM, polygon_sp)
  cropped_raster_masked <- mask(cropped_raster, polygon_sp)
  
  # Extrahieren Sie die Höhenwerte aus dem zugeschnittenen Raster
  altitudes <- values(cropped_raster_masked)
  
  # Teilen Sie die Höhenwerte in die definierten Bereiche ein
  altitude_bins <- cut(altitudes, breaks = breaks, labels = labels, include.lowest = TRUE)
  
  # Berechnen Sie die Fläche für jedes Höhenintervall
  # area_per_bin <- tapply(altitudes, altitude_bins, function(x) sum(!is.na(x)) * res(cropped_raster)[1] * res(cropped_raster)[2])
  npixel_per_bin <- tapply(altitudes, altitude_bins, function(x) sum(!is.na(x)))
  
  # Speichern Sie die Flächen in der Liste
  # areas[[i]] <- npixel_per_bin
  
  # better: save as tbl
  tbl_out <- as_tibble(npixel_per_bin, rownames = "altitude_bin") %>% 
    cbind(GMBA_V2_ID = GMBA$GMBA_V2_ID[i])
  
  areas[[i]] <- tbl_out
}

# Jetzt enthält 'areas' die Flächen der Polygone in 'GMBA_1' für jedes Höhenintervall

df_areas <- bind_rows(areas)


# Erstellen Sie eine leere Liste, um die Anzahl der Stationen pro Höhenstufe zu speichern
stations_per_bin <- list()

# Schleife über jede Zeile in inventory
for(i in 1:nrow(inventory)) {
  # Extrahieren Sie die Position der Station für die aktuelle Zeile
  station <- st_geometry(inventory[i, ])
  
  # Konvertieren Sie das sf-Objekt in ein Spatial-Objekt
  station_sp <- as(station, "Spatial")
  
  # Extrahieren Sie die Höhenwerte aus dem Raster an der Position der Station
  altitude <- extract(DEM, station_sp)
  
  # Teilen Sie die Höhenwerte in die definierten Bereiche ein
  altitude_bin <- cut(altitude, breaks = breaks, labels = labels, include.lowest = TRUE)
  
  # Fügen Sie das Höhenintervall zur Liste hinzu
  stations_per_bin[[i]] <- altitude_bin
}

# Jetzt enthält 'stations_per_bin' das Höhenintervall für jede Station in 'inventory'
# Zählen Sie die Anzahl der Stationen pro Höhenstufe
station_counts <- table(unlist(stations_per_bin))


# Konvertieren Sie die Listen 'areas' und 'stations_per_bin' in Dataframes
df_areas <- do.call(rbind, lapply(areas, function(x) data.frame(Height = names(x), Area = x)))
df_stations <- do.call(rbind, lapply(stations_per_bin, function(x) data.frame(Height = names(x), Stations = length(x))))

# Führen Sie die beiden Dataframes zusammen
df <- merge(df_areas, df_stations, by = "Height")

# Konvertieren Sie die Fläche in km² und berechnen Sie die Anzahl der Stationen pro km²
df$Area <- df$Area / 1e6
df$Stations_per_km2 <- df$Stations / df$Area

# Jetzt enthält 'df$Stations_per_km2' die Anzahl der Stationen pro km² für jede Höhenstufe
