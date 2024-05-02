library(sf)
library(ggplot2)
library(dplyr)
library(raster)
library(sp)
library(tidyverse)

#Preparation####

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

# Preparation 02####

# Only Stations which are in GMBA
inventory_GMBA_01 <- inventory_GMBA[!is.na(inventory_GMBA$Level_01), ]

# Find which polygon each station is in
inventory_GMBA_01$Polygon_ID <- st_within(inventory_GMBA_01, GMBA_avgheight)

# Join data baed in location
joined_data <- st_join(inventory_GMBA_01, GMBA_avgheight)

# calculate hight Difference
joined_data$Height_Difference <- joined_data$`Altitude (m)` - joined_data$average_height

# Select only the desired columns for better workability and overview
joined_data <- joined_data[, c("ID", "Altitude (m)", "geometry", "GMBA_V2_ID.x", "Level_01", "Level_02", "Level_03", "Level_04", "average_height", "Height_Difference")]
#rename colame
colnames(joined_data)[colnames(joined_data) == "GMBA_V2_ID.x"] <- "GMBA_V2_ID"



# Start Analyzing####

# List of levels
levels <- c("Level_01", "Level_02", "Level_03", "Level_04")

# Loop through all levels
for(level in levels){
  
  # Group by Level and calculate the average height difference
  average_height_difference <- joined_data %>%
    group_by(!!sym(level)) %>%
    summarise(Average_Height_Difference = mean(Height_Difference, na.rm = TRUE))
  
  # Calculate the average height of the stations and the regions for each Level region
  average_heights <- joined_data %>%
    group_by(!!sym(level)) %>%
    summarise(Average_Station_Height = mean(`Altitude (m)`, na.rm = TRUE),
              Average_Region_Height = mean(unique(average_height), na.rm = TRUE))
  
  # Add the column "Average_Height_Difference" to "average_heights"
  average_heights$Average_Height_Difference <- average_height_difference$Average_Height_Difference
  # Delete the DataFrame "average_height_difference"
  rm(average_height_difference)
  
  # Create the plot with a legend
  p <- ggplot(average_heights, aes(x = !!sym(level))) +
    geom_bar(aes(y = Average_Station_Height, fill = "Station"), stat = "identity", alpha = 0.5, position = "dodge") +
    geom_bar(aes(y = Average_Region_Height, fill = "Region"), stat = "identity", alpha = 0.5, position = "dodge") +
    theme(axis.text = element_text(angle = 90, hjust = 1)) +
    labs(x = "", y = "Average Height (m)", title = paste("Average Heights by", level)) +
    scale_fill_manual("", values = c("Station" = "blue", "Region" = "red"))
  
  # Save Plot
  ggsave(paste0(save_l, paste("Average_Heights_", level, ".png", sep = "")),
         plot = p, width = 20, height = 10)
}
