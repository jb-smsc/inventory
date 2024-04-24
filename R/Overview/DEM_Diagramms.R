# Load Packages
library(sf)
library(ggplot2)
library(dplyr)
library(raster)
library(sp)

# Save location
save_l <- "Overview_Finished/"

#Preparation####
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

# Path to tif File
DEM_path <- "data/ETOPO_2022_v1_60s_N90W180_bed.tif"
# Read in tif as a raster 
DEM <- raster(DEM_path)






#####
# Convert multipoints to polygons
GMBA_clean_P <- st_cast(GMBA_clean, "POLYGON")

# Initialize an empty vector to store average heights
average_heights <- c()

# Loop through each polygon in GMBA_clean
for(i in 1:nrow(GMBA_clean)){
  
  # Extract the polygon
  polygon <- GMBA_clean[i, ]
  
  # Crop the DEM to the extent of the polygon
  DEM_cropped <- crop(DEM, extent(polygon))
  
  # Mask the DEM to the polygon
  DEM_masked <- mask(DEM_cropped, polygon)
  
  # Calculate the average height
  avg_height <- cellStats(DEM_masked, stat = 'mean', na.rm = TRUE)
  
  # Check if avg_height is NA, if so assign a default value or handle appropriately
  if(is.na(avg_height)){
    avg_height <- 0  # or any other value that makes sense in your context
  }
  
  # Append the average height to the vector
  average_heights <- c(average_heights, avg_height)
}

# Add the average heights to the GMBA_clean dataframe
GMBA_clean$average_height <- average_heights



#AVG_height for Polygons####
# Initialize an empty vector to store average heights
average_heights <- c()
# Loop through each multipolygon in GMBA_clean_P
for(i in 1:nrow(GMBA_clean_P)){
  # Extract the multipolygon
  multipolygon <- GMBA_clean_P[i, ]
  # Crop the DEM to the extent of the multipolygon
  DEM_cropped <- crop(DEM, extent(multipolygon))
  # Mask the DEM to the multipolygon
  DEM_masked <- mask(DEM_cropped, multipolygon)
  # Calculate the average height
  avg_height <- cellStats(DEM_masked, stat = 'mean', na.rm = TRUE)
  # Check if avg_height is NA, if so assign a default value or handle appropriately
  if(is.na(avg_height)){
    avg_height <- 0  # or any other value that makes sense in your context
  }
  # Append the average height to the vector
  average_heights <- c(average_heights, avg_height)
}
# Add the average heights to the GMBA_clean_P dataframe
GMBA_clean_P$average_height <- average_heights





#####
# Only Stations which are in GMBA
inventory_GMBA_01 <- inventory_GMBA[!is.na(inventory_GMBA$Level_01), ]

# Find which polygon each station is in
inventory_GMBA_01$Polygon_ID <- st_within(inventory_GMBA_01, GMBA_clean_P)

# Verbinde die Daten basierend auf der räumlichen Beziehung
joined_data <- st_join(inventory_GMBA_01, GMBA_clean_P)

# Berechne die Höhendifferenz
joined_data$Height_Difference <- joined_data$`Altitude (m)` - joined_data$average_height.x

# Gruppiere nach Level_01 und berechne die durchschnittliche Höhendifferenz
average_height_difference <- joined_data %>%
  group_by(Level_01.x) %>%
  summarise(Average_Height_Difference = mean(Height_Difference, na.rm = TRUE))


# Füge die durchschnittliche Höhe der Polygone hinzu
average_heights <- st_join(average_station_height, average_height_difference, by = "Level_01.x")






## Konvertiere das sf Objekt in ein normales DataFrame
joined_data_df <- st_set_geometry(joined_data, NULL)

# Erstelle das neue DataFrame
clean_heights <- dplyr::select(joined_data_df, average_height.y, Height_Difference, `Altitude (m)`, Level_01.x, Level_02.x, Level_03.x, Level_04.x)

# Berechne die durchschnittliche Höhe der Stationen und der Regionen für jede Level_01.x Region
average_heights <- clean_heights %>%
  group_by(Level_01.x) %>%
  summarise(Average_Station_Height = mean(`Altitude (m)`, na.rm = TRUE),
            Average_Region_Height = mean(unique(average_height.y), na.rm = TRUE))

# Erstelle das Diagramm mit einer Legende
p <- ggplot(average_heights, aes(x = Level_01.x)) +
       geom_bar(aes(y = Average_Station_Height, fill = "Station"), stat = "identity", alpha = 0.5, position = "dodge") +
       geom_bar(aes(y = Average_Region_Height, fill = "Region"), stat = "identity", alpha = 0.5, position = "dodge") +
       theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
       labs(x = "", y = "Average Height (m)", title = "Average Heights by Level 01") +
        scale_fill_manual("", values = c("Station" = "blue", "Region" = "red"))


ggsave(paste0(save_l, "Average_Heights.png"),
       plot = p, width = 20, height = 10)
