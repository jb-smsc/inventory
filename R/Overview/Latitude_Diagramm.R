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
GMBA <- read_sf("data-raw/GMBA/GMBA_Inventory_v2.0_standard_300.shx")
# Clean geometry
GMBA_clean <- st_make_valid(GMBA)
# Create points with coordinates for inventory-01-read
inventory <- st_as_sf(`inventory-01-read`, coords = c("Longitude", "Latitude"), crs = 4326)
# Cut points
inventory_GMBA <- st_join(inventory, GMBA_clean)

# Extract latitudes
inventory_GMBA$latitude <- st_coordinates(inventory_GMBA$geometry)[,2]



### Latitiude Diagramm

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
#summary_df$latitude_group[summary_df$latitude_group %in% c('NAN', 'NAS')] <- 'NA_count'

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



### Level_01 Latitiude Diagramm:

# Filter the data where 'Level_01' is not 'NA'
inventory_GMBA_filtered <- inventory_GMBA %>% filter(!is.na(Level_01))

# Extract latitudes
inventory_GMBA_filtered$latitude <- sf::st_coordinates(inventory_GMBA_filtered$geometry)[,2]

# Create latitude groups starting from -50 (50 degrees South)
inventory_GMBA_filtered$latitude_group <- cut(inventory_GMBA_filtered$latitude, breaks = seq(-50, max(inventory_GMBA_filtered$latitude), by = 5))

# Add N or S to the latitude groups
inventory_GMBA_filtered$latitude_group <- paste0(inventory_GMBA_filtered$latitude_group, ifelse(inventory_GMBA_filtered$latitude >= 0, "N", "S"))

# Calculate the average altitude and count for each latitude group
summary_df_filtered <- inventory_GMBA_filtered %>%
  group_by(latitude_group) %>%
  summarise(`Number of Stations` = n(),
            `Average Altitude (m)` = mean(`Altitude (m)`))

# Replace 'NAN' and 'NAS' with 'NA_count'
#summary_df_filtered$latitude_group[summary_df_filtered$latitude_group %in% c('NAN', 'NAS')] <- 'NA_count'

# Group the data by 'latitude_group'
grouped_df_filtered <- table(summary_df_filtered$latitude_group)

# Plot
p_filtered <- ggplot(summary_df_filtered, aes(x = latitude_group)) +
  geom_bar(aes(y = `Number of Stations`), stat = "identity", fill = "steelblue") +
  geom_text(aes(label=`Number of Stations`, y=`Number of Stations`), vjust=-0.5) +
  geom_line(aes(y = `Average Altitude (m)`), group = 1, colour = "red", size = 1) +
  scale_y_continuous(sec.axis = sec_axis(~./max(summary_df_filtered$`Number of Stations`)*max(summary_df_filtered$`Average Altitude (m)`), name = "Average Altitude (m)")) +
  theme_bw() +
  labs(title = "Number of Stations and Average Altitude per 5 degrees Latitude (GMBA Level_01)", x = "Latitude Group", y = "Number of Stations")

# Automatically save diagram
ggsave(paste0(save_l, "Latitude_GMBA.png"),
       plot = p_filtered, width = 20, height = 10)

