# Load Packages
library("sf")
library("writexl")
library("openxlsx")
library("ggplot2")
library("dplyr")
library("countrycode")
library("rnaturalearth")

# Save location
save_l <- "Overview_Finished/"

# Preparation
# Read in the dataset
`inventory-01-read` <- readRDS("data/inventory-01-read.rds")
# Read GMBA shp-File
GMBA <- sf::read_sf("data-raw/GMBA/GMBA_Inventory_v2.0_standard_300.shx")
# Clean geometry
GMBA_clean <- sf::st_make_valid(GMBA)
# Create points with coordinates for inventory-01-read
inventory <- sf::st_as_sf(`inventory-01-read`, coords = c("Longitude", "Latitude"), crs = 4326)
# Cut points
inventory_GMBA <- sf::st_join(inventory, GMBA_clean)



# Function to calculate the data
calculate_data <- function(level) {
  # Get unique regions
  unique_regions <- unique(GMBA_clean[[level]])
  # Initialize an empty dataframe
  Area <- data.frame()
  # Iterate through each region
  for (region in unique_regions) {
    # Calculate Avg. Altitiude of Stations in slected Regions: Select rows where 'Level' is equal to the current region 
    selected_region_AvgAlt <- subset(inventory_GMBA, inventory_GMBA[[level]] == region)
    # Calculate the average altitude for the current region
    avg_altitude <- mean(selected_region_AvgAlt$`Altitude (m)`, na.rm = TRUE)
    # Calculate Area of the Region: Select rows where 'Level' is equal to the current region
    selected_region_Area <- subset(GMBA_clean, GMBA_clean[[level]] == region)
    # Add up the areas of these rows
    total_area_km2 <- sum(selected_region_Area$Area, na.rm = TRUE)
    # Add Area and average altitude to the dataframe
    Area <- rbind(Area, data.frame(Region = region, total_area_km2 = total_area_km2, AvgAltitude = avg_altitude))
    }
  # Take unique data
  mount_L <- table(inventory_GMBA[[level]])
  # Create the dataframe
  df_m <- data.frame(Region = names(mount_L), Counts = as.integer(mount_L))
  # Calculate the number of NA values
  na_count <- sum(is.na(inventory_GMBA[[level]]))
  # Insert the area information into df_m
  df_m <- merge(df_m, Area, by = "Region", all.x = TRUE)
  # Create a new column 'km^2/station' that divides the area by the number of points
  df_m$`km^2/station` <- df_m$total_area_km2 / df_m$Counts
  
  
  
  # Add the NA count as an additional row in df_m
  na_df <- data.frame(Region = "NA", Counts = na_count, stringsAsFactors = FALSE)
  na_df[setdiff(names(df_m), names(na_df))] <- NA
  df_m <- rbind(na_df, df_m)
  return(df_m)
}

# Level01
df_m01 <- calculate_data("Level_01")
# Level02
df_m02 <- calculate_data("Level_02")
# Level03
df_m03 <- calculate_data("Level_03")
# Level04
df_m04 <- calculate_data("Level_04")
# Level05
df_m05 <- calculate_data("Level_05")



# List of properties
properties <- c("Snow depth", "Depth of snowfall", "SWE", "Bulk snow density", "Snow albedo")

# Function to calculate the number of measurements for each property in each region
calculate_measurements <- function(level) {
  # Get unique regions
  unique_regions <- unique(GMBA_clean[[level]])
  # Initialize an empty dataframe
  Measurements <- data.frame()
  # Iterate through each region
  for (region in unique_regions) {
    # Select rows where 'Level' is equal to the current region
    selected_region <- subset(inventory_GMBA, inventory_GMBA[[level]] == region)
    # Initialize a list to store the counts for each property
    property_counts <- list()
    # Iterate through each property
    for (property in properties) {
      # Count the number of "YES" entries for the current property
      property_count <- sum(selected_region[[property]] == "YES", na.rm = TRUE)
      # Add the count to the list
      property_counts[[property]] <- property_count
    }
    # Add the counts for the current region to the dataframe
    Measurements <- rbind(Measurements, c(region, unlist(property_counts)))
  }
  # Set the column names of the dataframe
  colnames(Measurements) <- c("Region", properties)
  return(Measurements)
}

# Calculate the measurements for each level
measurements_L01 <- calculate_measurements("Level_01")
measurements_L02 <- calculate_measurements("Level_02")
measurements_L03 <- calculate_measurements("Level_03")
measurements_L04 <- calculate_measurements("Level_04")
measurements_L05 <- calculate_measurements("Level_05")


# Function to convert all character columns in a dataframe to numeric
convert_to_numeric <- function(df) {
  # Iterate through each column in the dataframe
  for (col_name in names(df)) {
    # Check if the column is character and not "Region"
    if (is.character(df[[col_name]]) && col_name != "Region") {
      # Convert the column to numeric
      df[[col_name]] <- as.numeric(df[[col_name]])
    }
  }
  return(df)
}


# Convert the character columns in measurements_L01 to numeric
measurements_L01 <- convert_to_numeric(measurements_L01)
measurements_L02 <- convert_to_numeric(measurements_L02)
measurements_L03 <- convert_to_numeric(measurements_L03)
measurements_L04 <- convert_to_numeric(measurements_L04)
measurements_L05 <- convert_to_numeric(measurements_L05)



# Merge the measurements with the existing dataframes
df_m01 <- merge(df_m01, measurements_L01, by = "Region", all.x = TRUE)
df_m02 <- merge(df_m02, measurements_L02, by = "Region", all.x = TRUE)
df_m03 <- merge(df_m03, measurements_L03, by = "Region", all.x = TRUE)
df_m04 <- merge(df_m04, measurements_L04, by = "Region", all.x = TRUE)
df_m05 <- merge(df_m05, measurements_L05, by = "Region", all.x = TRUE)


# Function to sort the dataframe and keep NA at the top
sort_dataframe <- function(df) {
  # Remove the NA row from the dataframe
  df_no_na <- df[df$Region != "NA", ]
  # Sort the dataframe by Counts in descending order
  df_no_na <- df_no_na[order(-df_no_na$Counts), ]
  # Get the NA row
  na_row <- df[df$Region == "NA", ]
  # Add the NA row at the top
  df_sorted <- rbind(na_row, df_no_na)
  return(df_sorted)
}

# Sort the dataframes
df_m01 <- sort_dataframe(df_m01)
df_m02 <- sort_dataframe(df_m02)
df_m03 <- sort_dataframe(df_m03)
df_m04 <- sort_dataframe(df_m04)
df_m05 <- sort_dataframe(df_m05)




#Theme function for Diagrams
my_theme <- function() {
  theme_linedraw() +
    theme(legend.text = element_text(size = 15),
          axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size = 15),
          plot.title = element_text(size = 20),
          axis.line.x = element_line(size = 1),
          axis.line.y = element_line(size = 1),
          axis.text = element_text(size = 15))
    }


###Diagramm With all NA in GMBA_V2_ID
# Sort Data where GMAB_V2_ID is NA 
df_filtered <- inventory_GMBA[is.na(inventory_GMBA$GMBA_V2_ID),]
# Create diagram
height_plot <- ggplot(df_filtered, aes(x=`Altitude (m)`))+
  geom_freqpoly(size = 1 )+
  scale_x_continuous(breaks = seq(0,5000, by = 500))+
  ylab("Counts")+ xlab("Height in m")+ 
  ggtitle("Height of NA stations in Level_01")+
  my_theme()
# Automatically save diagram
ggsave(paste0(save_l, "height_of_NA_stations_Level01.png"), plot = height_plot, width = 8, height = 6)



### Countries where stations are located. Also stations that are not intersected with GMBA
library("rnaturalearth")
# Load the country borders
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
# Perform a spatial join to determine the country of each station
inventory_country <- st_join(inventory, world)
# Count the number of stations per country
library("rnaturalearth")
stations_per_country <- inventory_country %>%
  group_by(name) %>%
  summarise(n = n(), .groups = 'drop')
# Sort the data in descending order
stations_per_country <- stations_per_country %>%
  arrange(desc(n))
# NA Staions at end of df
stations_per_country <- stations_per_country %>%
  arrange(is.na(name), desc(n))


### Continents where stations are located. Also stations that are not intersected with GMBA
# Load the country borders
world$continent <- countrycode(world$iso_a3, "iso3c", "continent")
# Perform a spatial join to determine the continent of each station
inventory_continent <- st_join(inventory, world)
# Count the number of stations per continent
stations_per_continent <- inventory_continent %>%
  group_by(continent) %>%
  summarise(n = n(), .groups = 'drop')
# Sort the data in descending order
stations_per_continent <- stations_per_continent %>%
  arrange(desc(n))
# NA Staions at end of df
stations_per_continent <- stations_per_continent %>%
  arrange(is.na(continent), desc(n))


# Create list
all_dfs <- list("Level01" = df_m01, "Level02" = df_m02, "Level03" = df_m03, "Level04" = df_m04, "Level05" = df_m05, "StationsPerCountry" = stations_per_country, "StationsPerContinent" = stations_per_continent)

# Round each value in the dataframes to one decimal place
all_dfs <- lapply(all_dfs, function(df) {
  df[] <- lapply(df, function(x) {
    if(is.numeric(x)) round(x, 1) else x
  })
  return(df)
})


# Write the data to an Excel file
write_xlsx(all_dfs, path = paste0(save_l, "jb-smsc_analyzed.xlsx"))