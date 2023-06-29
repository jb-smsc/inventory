# plot inventory

library(dplyr)
library(sf)
library(tmap)
library(ggplot2)

tbl_inv <- readRDS("data/inventory-01-read.rds")
sf_inv <- st_as_sf(tbl_inv,
                   crs = 4326,
                   coords = c("Longitude", "Latitude"))



# check inventory plots --------------------------------------------------------------------

mapview::mapview(sf_inv)

tmap_mode("view")

tm_shape(sf_inv)+
  tm_dots()+
  tm_basemap()+
  tm_facets(by = "sheet_name")

sf_inv$sheet_name %>% table
sf_inv %>% filter(sheet_name == "Snow information_HKH") %>% 
  mapview::mapview()







