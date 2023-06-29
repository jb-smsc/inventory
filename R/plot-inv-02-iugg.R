# plot inventory

library(dplyr)
library(forcats)
library(sf)
# library(tmap)
library(ggplot2)

tbl_inv <- readRDS("data/inventory-01-read.rds")
sf_inv <- st_as_sf(tbl_inv,
                   crs = 4326,
                   coords = c("Longitude", "Latitude"))



# clean data --------------------------------------------------------------

sf_inv$`Snow depth` %>% table
sf_inv$`Depth of snowfall` %>% table
sf_inv$SWE %>% table
sf_inv$`Bulk snow density` %>% table
sf_inv$`Snow albedo` %>% table

tbl_inv %>% with(range(End-Begin, na.rm = T))

sf_inv_long <- sf_inv %>% 
  select(sheet_name:`Snow albedo`) %>% 
  mutate(Begin = if_else(is.na(Begin), 2023, Begin),
         End = if_else(is.na(End), 2023, End)) %>% 
  mutate(obs_period = cut(End - Begin, breaks = c(0, 10, 30, 60, 240), include.lowest = T)) %>% 
  mutate(obs_period_fct = fct_recode(obs_period,  "60+" = "(60,240]")) %>% 
  # rename(Station_name = "Station name", Altitude = "Altitude (m)") %>% 
  pivot_longer(`Snow depth`:`Snow albedo`, names_to = "snow_variable") %>% 
  filter(value != "NO")

# sf_inv_long %>% filter(is.na(obs_period))
  
# overview plots ----------------------------------------------------------

sf_inv_robin <- st_transform(sf_inv, "+proj=robin")
sf_inv_long_robin <- st_transform(sf_inv_long, "+proj=robin")
world_robin <- st_transform(World, "+proj=robin")



## any obs -----------------------------------------------------------------

ggplot()+
  geom_sf(data = world_robin, fill = "grey95")+
  geom_sf(data = sf_inv_robin, pch = 20)+
  coord_sf(xlim = st_bbox(sf_inv_robin)[c(1,3)],
           ylim = st_bbox(sf_inv_robin)[c(2,4)])+
  theme_minimal()

ggsave("fig/inv-iugg-01-all.png",
       width = 8, height = 4)



# facet by variable -------------------------------------------------------


ggplot()+
  geom_sf(data = world_robin, fill = "grey95")+
  geom_sf(data = sf_inv_long_robin, pch = 20)+
  coord_sf(xlim = st_bbox(sf_inv_long_robin)[c(1,3)],
           ylim = st_bbox(sf_inv_long_robin)[c(2,4)])+
  facet_wrap(~ snow_variable)+
  theme_minimal()

ggsave("fig/inv-iugg-02-by-var.png",
       width = 16, height = 6)

# facet by length ---------------------------------------------------------

ggplot()+
  geom_sf(data = world_robin, fill = "grey95")+
  geom_sf(data = sf_inv_long_robin, pch = 20)+
  coord_sf(xlim = st_bbox(sf_inv_long_robin)[c(1,3)],
           ylim = st_bbox(sf_inv_long_robin)[c(2,4)])+
  facet_wrap(~ obs_period_fct)+
  theme_minimal()+
  ggtitle("Years of observations (theoretical, including gaps)")

ggsave("fig/inv-iugg-03-by-length.png",
       width = 16, height = 8)

# facet by var and length -------------------------------------------------
# 
# ggplot()+
#   geom_sf(data = world_robin, fill = "grey95")+
#   geom_sf(data = sf_inv_long_robin, pch = 20)+
#   coord_sf(xlim = st_bbox(sf_inv_long_robin)[c(1,3)],
#            ylim = st_bbox(sf_inv_long_robin)[c(2,4)])+
#   facet_grid(obs_period_fct ~ snow_variable)+
#   theme_minimal()+
#   ggtitle("Snow variable and years of observations (theoretical, including gaps)")
# 
# ggsave("fig/inv-iugg-04-by-var-and-length.png",
#        width = 16, height = 8)


