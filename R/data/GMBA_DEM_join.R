# Load Packages
library(sf)
library(ggplot2)
library(dplyr)
library(raster)
library(sp)

# Save location
save_l <- "Overview_Finished/"

#Preparation
# Read in the dataset
`inventory-01-read` <- readRDS("data-raw/inventory-01-read.rds")
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


#AVG_height for Polygons
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


# Save File
saveRDS(GMBA_clean_P, file = "/data/GMBA_clean_P.rds")
