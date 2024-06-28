# Creating plots with number of stations and area for given altitude bins

library(sf)
library(ggplot2)
library(dplyr)

# Prep #####

# Read Area and Staion count data
narea <- readRDS("data/GMBA-inv_area_counts.rds")

# Read GMBA shp-File
GMBA <- read_sf("data-raw/GMBA/GMBA_Inventory_v2.0_standard_300.shx")

# Definieren Sie die Grenzen und Bezeichnungen für Höhenintervalle
# Define the breaks and labels for altitude bins
breaks <- seq(-500, 9000, by = 500)
labels <- paste0("(", breaks[-length(breaks)], "-", breaks[-1], "] m")



# nach Level_01 zusammenführen
 

# Erstellen Sie eine leere Liste, um die DataFrames zu speichern
Level_01 <- list()

# Durchlaufen Sie jede einzigartige "Level_01" Kategorie
for(level in unique(GMBA$Level_01)) {
  
  # Filtern Sie die GMBA Daten für die aktuelle "Level_01" Kategorie
  filtered_gmba <- GMBA[GMBA$Level_01 == level,]
  
  # Finden Sie die entsprechenden DataFrames in Ihrer Liste
  matching_dfs <- narea[names(narea) %in% filtered_gmba$GMBA_V2_ID]
  
  # Fügen Sie den DataFrame zur Liste hinzu
  Level_01[[level]] <- matching_dfs
}


















# Old ##############################

# Preparation ####

# Save location
save_l <- "Overview_Finished/"
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

# read in GMBA Area
GMBA_area_bins <- readRDS("data/GMBA_area_bins.rds")



# Count Staions ####

# Define the breaks and labels for altitude bins
breaks <- seq(-500, 9000, by = 500)
labels <- paste0("(", breaks[-length(breaks)], "-", breaks[-1], "] m")

  
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


# Beide listen areas und station_counts zusammenführen
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








##########################################




  # Erstellen Sie eine leere Liste, um die DataFrames zu speichern
  df_list <- list()
  
  # Durchlaufen Sie jede GMBA_V2_ID
  for(i in 1:length(areas)) {
    
    # Erstellen Sie ein DataFrame für die aktuelle GMBA_V2_ID
    df <- data.frame(
      altitude_bin = names(areas[[i]]),
      ares = areas[[i]],
      station_counts = station_counts[[i]]
    )
    
    # Fügen Sie das DataFrame zur Liste hinzu
    df_list[[names(areas)[i]]] <- df
  }
  






# Definieren Sie eine Funktion, die tapply auf eine Liste anwendet
apply_tapply <- function(x) {
  tapply(x, INDEX = names(x), FUN = sum)
}

# Verwenden Sie lapply(), um die Funktion auf Ihre Liste anzuwenden
result_list <- lapply(areas, apply_tapply)















# Definieren Sie eine Funktion, die ein DataFrame erstellt
create_dataframe <- function(x) {
  # Erstellen Sie ein DataFrame mit den gewünschten Spalten
  df <- data.frame(
    altitude_bin = names(x),
    ares = unlist(x),
    station_counts = unlist(station_counts[names(x)])
  )
  
  # Geben Sie das DataFrame zurück
  return(df)
}

# Verwenden Sie lapply(), um die Funktion auf Ihre Liste anzuwenden
list_of_dataframes <- lapply(areas, create_dataframe)




# 1. Categorize each station into altitude bins
inventory_GMBA$altitude_bin <- cut(inventory_GMBA$`Altitude (m)`, breaks = breaks, labels = labels, include.lowest = TRUE)

# 2. Count the number of stations in each altitude bin and region
stations_by_altitude_and_region <- inventory_GMBA %>%
  group_by(Level_01, Level_02, Level_03, Level_04, altitude_bin) %>%
  summarise(n_stations = n())
# Filter the data to remove NA rows
stations_by_altitude_and_region <- stations_by_altitude_and_region[!is.na(stations_by_altitude_and_region$Level_01),]

# Create Seperate dataframes
for(i in 1:4) {
  level_name <- paste0("Level_0", i)
  
  # Zählen Sie die Anzahl der Stationen in jeder Höhenstufe und Region
  df_temp <- inventory_GMBA %>%
    group_by_at(c(level_name, "altitude_bin")) %>%
    summarise(n_stations = n())
  
  # Filtern Sie die Daten, um NA-Zeilen zu entfernen
  df_temp <- df_temp[!is.na(df_temp[[level_name]]),]
  
  # Erstellen Sie den Dataframe-Namen
  df_name <- paste0("L", i, "N")
  
  # Weisen Sie das Dataframe einem neuen Objekt mit dem erstellten Namen zu
  assign(df_name, df_temp, envir = .GlobalEnv)
  
  rm(df_temp)
}

