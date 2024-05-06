library(sf)
library(ggplot2)
library(dplyr)
library(tidyverse)

# Preparation####

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
# Read in the GMBA_avgheight    All Polygons form GMBA Dataset with average height of them calculated via worldwide DEM
GMBA_avgheight <- readRDS("data/GMBA_avgheight.rds")
# Only Stations which are in GMBA
inventory_GMBA_01 <- inventory_GMBA[!is.na(inventory_GMBA$Level_01), ]
# Find which polygon each station is in
inventory_GMBA_01$Polygon_ID <- st_within(inventory_GMBA_01, GMBA_avgheight)
# Join data based in location
joined_data <- st_join(inventory_GMBA_01, GMBA_avgheight)
# Clean Enviroment
rm(GMBA, "inventory-01-read", inventory)


# Calculate Area and Create Diagrams ####

# Define the breaks and labels for altitude bins
breaks <- seq(0, max(joined_data$`Altitude (m)`), by = 500)
labels <- paste0("(", breaks[-length(breaks)], "-", breaks[-1], "] m")

# Create a new column "Altitude_bin" that represents the altitude bins
joined_data$Altitude_bin <- cut(joined_data$`Altitude (m)`, breaks = breaks, labels = labels)

# Define the levels
levels <- c("Level_01", "Level_02", "Level_03", "Level_04")

# Loop over the levels
for(level in levels) {
  
  # Group the data by the current level and Altitude_bin
  grouped_data <- joined_data %>% 
    group_by_at(vars(level, "Altitude_bin")) %>% 
    summarise(Total_Area = sum(Area, na.rm = TRUE))
  
  # Count the number of stations for each altitude bin
  station_count <- joined_data %>%
    group_by_at(vars(level, "Altitude_bin")) %>%
    summarise(Num_Stations = n(), .groups = "drop")
  
  # Remove 'geometry'-Collum
  grouped_data$geometry <- NULL
  
  # Merge the station_count and grouped_data dataframes
  grouped_data <- left_join(grouped_data, station_count, by = c("Altitude_bin", level))
  
  # Create a new column "sqkm_per_Station" that represents the square kilometers per station
  grouped_data$sqkm_per_Station <- grouped_data$Total_Area / grouped_data$Num_Stations
  
  # Create the plot
  p <- ggplot(grouped_data, aes_string(x = level, y = "sqkm_per_Station", fill = "Altitude_bin")) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_y_continuous(labels = scales::comma) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(x = "Region", y = "km² per Station", fill = "Altitude bin") +
    ggtitle(paste("Square kilometers per station in 500 m", level))
  
  # Save the plot
  ggsave(paste0(save_l, "/Height_Area_", level, "_plot.png"), plot = p,
         width = 10, height = 5)
  
}
