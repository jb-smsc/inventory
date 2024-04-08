library(ggplot2)
library(dplyr)
library(forcats)

dat_inv <- readRDS("data/inventory-01-read.rds")

dat1 <- dat_inv %>% filter(sheet_name == "Canada" & SWE == "YES")

# needs a unique ID/name per row/station
# in this case station name is not unique


dat1 %>% 
  mutate(yy_name_sorted = fct_reorder(ID, Begin)) %>% # sort the y axis-variable by Begin column
  ggplot()+
  geom_segment(aes(x = Begin, xend = End, y = yy_name_sorted, yend = yy_name_sorted))+
  theme_bw()+
  xlab(NULL)+
  ylab("Stations")+
  theme(axis.text.y = element_blank(), # remove y-axis overplotting
        axis.ticks.y = element_blank())

