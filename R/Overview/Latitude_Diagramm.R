# Load Packages
library(sf)
library(ggplot2)
library(dplyr)

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


# Extract latitudes
inventory_GMBA$latitude <- sf::st_coordinates(inventory_GMBA$geometry)[,2]

# Create latitude groups starting from -50 (50 degrees South)
inventory_GMBA$latitude_group <- cut(inventory_GMBA$latitude, breaks = seq(-50, max(inventory_GMBA$latitude), by = 5))

# Add N or S to the latitude groups
inventory_GMBA$latitude_group <- paste0(inventory_GMBA$latitude_group, ifelse(inventory_GMBA$latitude >= 0, "N", "S"))

# Calculate the average altitude and count for each latitude group
summary_df <- inventory_GMBA %>%
  group_by(latitude_group) %>%
  summarise(`Number of Stations` = n(),
            `Average Altitude (m)` = mean(`Altitude (m)`))

# Replace 'NAN' and 'NAS' with 'NA_count'
summary_df$latitude_group[summary_df$latitude_group %in% c('NAN', 'NAS')] <- 'NA_count'

# Group the data by 'latitude_group'
grouped_df <- table(summary_df$latitude_group)

# Plot
p <- ggplot(summary_df, aes(x = latitude_group)) +
        geom_bar(aes(y = `Number of Stations`), stat = "identity", fill = "steelblue") +
        geom_text(aes(label=`Number of Stations`, y=`Number of Stations`), vjust=-0.5) +
        geom_line(aes(y = `Average Altitude (m)`), group = 1, colour = "red", size = 1) +
        scale_y_continuous(sec.axis = sec_axis(~./max(summary_df$`Number of Stations`)*max(summary_df$`Average Altitude (m)`), name = "Average Altitude (m)")) +
        theme_bw() +
        labs(title = "Number of Stations and Average Altitude per 5 degrees Latitude", x = "Latitude Group", y = "Number of Stations")

# Automatically save diagram
ggsave(paste0(save_l, "Latitude.png"),
       plot = p, width = 20, height = 10)
