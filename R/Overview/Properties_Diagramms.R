# Load Packages
library("dplyr")
library("ggplot2")
library("forcats")


# Save location
save_l <- "Overview_Finished/"

# Preparation
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



#Theme function for Diagrams
my_theme <- function() {
  theme_bw() +
    theme(legend.text = element_text(size = 15),
          axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size = 15),
          plot.title = element_text(size = 20),
          axis.line.x = element_line(size = 1),
          axis.line.y = element_line(size = 1),
          axis.text = element_text(size = 15))
}


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


### Diagramms of Proberties for each region 
# List of properties
properties <- c("Snow depth", "Depth of snowfall", "SWE", "Bulk snow density", "Snow albedo")

# Create a function to create the plot for a given level and property
create_plot <- function(level, df, property) {
  # Get unique regions
  unique_regions <- unique(df$Region)
  # Iterate through each region
  for (region in unique_regions) {
    # Select rows where 'Level' is equal to the current region and the property is 'YES'
    selected_region <- subset(inventory_GMBA, inventory_GMBA[[level]] == region & inventory_GMBA[[property]] == "YES")
    # Convert 'Begin' to numeric and handle NA values
    selected_region$Begin <- as.numeric(selected_region$Begin)
    selected_region <- selected_region[!is.na(selected_region$Begin), ]
    # Replace NA values in 'End' with 2024
    selected_region$End[is.na(selected_region$End)] <- 2024
    # Create a plot with 'Begin' and 'End' on the x-axis and the stations on the y-axis
    plot <- selected_region %>%
      mutate(yy_name_sorted = fct_reorder(rownames(selected_region), Begin)) %>%
      ggplot() +
      geom_segment(aes(x = Begin, xend = End, y = yy_name_sorted, yend = yy_name_sorted)) +
      theme_bw() +
      theme(panel.grid = element_blank())+
      xlab(NULL) +
      ylab("Stations") +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
      labs(title = paste("Region:", region, "| Level:", level, "| Property:", property))  # Add a title to the plot
    # Save the plot in the directory 'save_l' with specified width and height
    ggsave(filename = paste0(save_l, "/", level, "/", property, "/", region, ".png"), plot = plot, width = 8, height = 6)
  }
}


# List of dataframes
dataframes <- list(df_m01, df_m02, df_m03)
# List of levels
levels <- c("Level_01", "Level_02", "Level_03")

# Create the plots for each property in each dataframe
for (i in 1:length(dataframes)) {
  for (property in properties) {
    create_plot(levels[i], dataframes[[i]], property)
  }
}
