# plot inventory

library(dplyr)
library(sf)
library(ggplot2)

tbl_inv <- readRDS("data/inventory-01-read.rds")
sf_inv <- st_as_sf(tbl_inv,
                   crs = 4326,
                   coords = c("Longitude", "Latitude"))




# plot --------------------------------------------------------------------

ggplot()+
  borders()+
  geom_sf(data = sf_inv)


