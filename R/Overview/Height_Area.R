library(sf)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(raster)

# Preparation ####

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
# Remove GMBA_V2_ID.x from joined_data
joined_data$GMBA_V2_ID.x <- NULL
# Rename GMBA_V2_ID.y to GMBA_V2_ID
names(joined_data)[names(joined_data) == "GMBA_V2_ID.y"] <- "GMBA_V2_ID"
# calculate area for every polygon
GMBA_avgheight$Area <- st_area(GMBA_avgheight)
# Replace all Values smaller 1 with 1
GMBA_avgheight$average_height <- ifelse(GMBA_avgheight$average_height < 1, 1, GMBA_avgheight$average_height)


# Calculate Area and No. of Stations ####

# Define the breaks and labels for altitude bins
breaks <- seq(0, 6000, by = 500)
labels <- paste0("(", breaks[-length(breaks)], "-", breaks[-1], "] m")

# Create a new column "Altitude_bin" that represents the altitude bins
#joined_data$Altitude_bin <- cut(joined_data$`Altitude (m)`, breaks = breaks, labels = labels)
GMBA_avgheight$Altitude_bin <- cut(GMBA_avgheight$average_height, breaks = breaks, labels = labels)




























# Bestimmen Sie die Anzahl der Zeilen, die Sie behalten möchten
num_rows <- round(nrow(GMBA_avgheight) * 0.05)

# Erstellen Sie einen zufälligen Index
set.seed(161) # Für die Reproduzierbarkeit
index <- sample(1:nrow(GMBA_avgheight), num_rows)

# Reduzieren Sie den Datensatz
GMBA_avgheight_reduced <- GMBA_avgheight[index, ]

# Führen Sie nun Ihren Code mit dem reduzierten Datensatz aus

# Erstellen Sie eine Kopie von GMBA_avgheight für die getrimmten Daten
GMBA_avgheight_trimmed <- GMBA_avgheight

# Durchlaufen Sie jedes Polygon im reduzierten DataFrame
for (i in 1:nrow(GMBA_avgheight)) {
  for (j in 1:nrow(GMBA_avgheight)) {
    # Überspringen Sie den Fall, dass sich ein Polygon selbst überschneidet
    if (i != j) {
      # Überprüfen Sie, ob sich die Polygone überschneiden
      if (any(lengths(st_intersects(GMBA_avgheight[i, ], GMBA_avgheight[j, ])) > 0)) {
        # Überprüfen Sie, welches Polygon größer ist
        if (st_area(GMBA_avgheight[i, ]) > st_area(GMBA_avgheight[j, ])) {
          # Schneiden Sie das kleinere Polygon vom größeren ab
          difference <- st_difference(GMBA_avgheight[i, ], GMBA_avgheight[j, ])
          # Stellen Sie sicher, dass das zurückgegebene Objekt die gleiche Struktur hat
          difference <- difference[ , names(GMBA_avgheight)]
          GMBA_avgheight_trimmed[i, ] <- difference
        }
      }
    }
  }
}












# Erstellen Sie eine Kopie des DataFrames
GMBA_avgheight_trimmed <- GMBA_avgheight

# Durchlaufen Sie jedes Polygon im DataFrame
for (i in 1:nrow(GMBA_avgheight)) {
  for (j in 1:nrow(GMBA_avgheight)) {
    # Überspringen Sie den Fall, dass sich ein Polygon selbst überschneidet
    if (i != j) {
      # Überprüfen Sie, ob sich die Polygone überschneiden
      if (any(lengths(st_intersects(GMBA_avgheight[i, ], GMBA_avgheight[j, ])) > 0)) {
        # Überprüfen Sie, welches Polygon größer ist
        if (st_area(GMBA_avgheight[i, ]) > st_area(GMBA_avgheight[j, ])) {
          # Schneiden Sie das kleinere Polygon vom größeren ab
          GMBA_avgheight_trimmed[i, ] <- st_difference(GMBA_avgheight[i, ], GMBA_avgheight[j, ])
        }
      }
    }
  }
}





GMBA_avgheight <- st_set_crs(GMBA_avgheight, 4326)
GMBA_avgheight <- st_transform(GMBA_avgheight, 3395)



# Bereinigen Sie die Geometrien
GMBA_avgheight <- st_make_valid(GMBA_avgheight_trimmed)



# Transform to a planar CRS (e.g., World Mercator)
GMBA_avgheight <- st_transform(GMBA_avgheight_trimmed, crs = "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")



# Berechnen Sie die Überschneidungen
overlaps <- st_overlaps(GMBA_avgheight_trimmed)

# Initialisieren Sie einen Zähler für die Anzahl der Überschneidungen
num_overlaps <- 0

# Durchlaufen Sie jeden Vektor in der Liste
for (overlap in overlaps) {
  # Wenn der Vektor mehr als ein Element enthält, gibt es eine Überschneidung
  if (length(overlap) > 1) {
    num_overlaps <- num_overlaps + 1
  }
}

# Drucken Sie die Anzahl der Überschneidungen
print(paste("Anzahl der sich überschneidenden Polygone: ", num_overlaps))






# Erstellen Sie eine Matrix, die angibt, welche Polygone sich überschneiden
overlaps <- st_overlaps(GMBA_avgheight, sparse=FALSE)

# Zählen Sie die Anzahl der Überschneidungen
num_overlaps <- sum(overlaps) - length(GMBA_avgheight)  # Subtrahieren Sie die Diagonalelemente

print(paste("Anzahl der sich überschneidenden Polygone: ", num_overlaps))








# Group New
GMBA_avgheight <- GMBA_avgheight %>% 
  group_by(GMBA_V2_ID, Altitude_bin)

# Calculate Total Area for Altitude bins for each GMBA_V2_ID
area_summary <- GMBA_avgheight %>% 
  summarise(Total_Area = sum(Area, na.rm = TRUE))

# Führen Sie eine räumliche Verknüpfung durch, um die Anzahl der Stationen zu area_summary hinzuzufügen
area_stations <- st_join(area_summary, joined_data)

# Remove GMBA_V2_ID.y 
area_stations$GMBA_V2_ID.y <- NULL
# Rename GMBA_V2_ID.y to GMBA_V2_ID
names(area_stations)[names(area_stations) == "GMBA_V2_ID.x"] <- "GMBA_V2_ID"


# Group the area_stations dataframe by GMBA_V2_ID and Altitude_bin
grouped_data <- area_stations %>% group_by(GMBA_V2_ID, Altitude_bin)

# Count the number of unique stations in each group
station_counts <- grouped_data %>% summarise(Station_Count = n_distinct(ID))






















# Group the area_stations dataframe by GMBA_V2_ID and Altitude_bin
grouped_data <- area_stations %>% group_by(GMBA_V2_ID, Altitude_bin)

# Count the number of stations in each group
station_counts <- grouped_data %>% summarise(Station_Count = sum(Station_Count, na.rm = TRUE))









# Gruppieren Sie die Daten nach Altitude_bin und zählen Sie die Anzahl der Stationen
station_count_per_bin <- area_stations %>%
  group_by(Altitude_bin) %>%
  summarise(Num_Stations = n(), .groups = "drop")







# Gruppieren Sie die Daten nach GMBA_V2_ID und Altitude_bin und zählen Sie die Anzahl der Stationen
station_count <- joined_data %>%
  group_by(GMBA_V2_ID) %>%
  summarise(Num_Stations = n(), .groups = "drop")

# Fügen Sie die Anzahl der Stationen zu area_summary hinzu
area_summary_with_stations <- left_join(area_summary, station_count, by = c("GMBA_V2_ID", "Altitude_bin"))








# Group the data by GMBA_V2_ID and Altitude_bin and count the number of stations
station_count <- joined_data %>%
  group_by(GMBA_V2_ID) %>%
  summarise(Num_Stations = n(), .groups = "drop")

# Convert station_count to an sf object
station_count <- st_as_sf(station_count)

# Add the number of stations to area_summary
area_stations <- st_join(area_summary, station_count)

# Remove GMBA_V2_ID.y 
area_stations$GMBA_V2_ID.y <- NULL
# Rename GMBA_V2_ID.y to GMBA_V2_ID
names(area_stations)[names(area_stations) == "GMBA_V2_ID.x"] <- "GMBA_V2_ID"





















# Define the levels
levels <- c("Level_01")

# Loop over the levels
#for(level in levels) {
  
  # Group the data by the current level and Altitude_bin
  grouped_data <- GMBA_clean %>% 
    group_by_at(vars(levels,  "Altitude_bin")) %>% 
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
