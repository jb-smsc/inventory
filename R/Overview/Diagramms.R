# Load packages
library(writexl)
library(ggplot2)

# Save location
save_l <- "Overview_Finished/"


# Read in the dataset
`inventory-01-read` <- readRDS("data/inventory-01-read.rds")

# If NA in End, set 2024 as value
`inventory-01-read`$End[is.na(`inventory-01-read`$End)] <- 2024

### Height of the stations
# Create diagram
height_plot <- ggplot(`inventory-01-read`, aes(x=`Altitude (m)`))+
  geom_freqpoly(size = 1 )+
  scale_x_continuous(breaks = seq(0,5000, by = 500))+
  ylab("Counts")+ xlab("Height in m")+ 
  ggtitle("Height of the stations")+
  theme_linedraw()+
  theme(legend.text = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        plot.title = element_text(size = 20),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1),
        axis.text = element_text(size = 15))
# Automatically save diagram
ggsave(paste0(save_l, "height_plot.png"), plot = height_plot, width = 8, height = 6)



### Start of recording
# Create diagram
begin_plot <- ggplot(`inventory-01-read`, aes(x=`Begin`))+
  geom_freqpoly(size = 1 )+
  ylab("Counts")+ xlab("Year")+ 
  ggtitle("Start of recording")+
  theme_linedraw()+
  theme(legend.text = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        plot.title = element_text(size = 20),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1),
        axis.text = element_text(size = 15))
# Automatically save diagram
ggsave(paste0(save_l, "begin_plot.png"),
       plot = begin_plot, width = 8, height = 6)



### Duration of recording
duration_plot <- ggplot(`inventory-01-read`, aes(x=End - Begin))+
  geom_freqpoly(size = 1 )+
  ylab("Counts")+ xlab("Years")+ 
  ggtitle("Duration of recording")+
  theme_linedraw()+
  theme(legend.text = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        plot.title = element_text(size = 20),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1),
        axis.text = element_text(size = 15))
# Automatically save diagram
ggsave(paste0(save_l, "duration_plot.png"), plot = duration_plot, width = 8, height = 6)



### Frequency table
# Convert to string and then lowercase so everything is sorted the same. 
`inventory-01-read`$Frequency <-  as.character(`inventory-01-read`$Frequency)
`inventory-01-read`$Frequency <-  tolower(`inventory-01-read`$Frequency)
# How often each frequency occurs
freq <- table(`inventory-01-read`$Frequency)
# Sort descending
freq <- sort(freq, decreasing  = TRUE)
# Create the dataframe
df <- data.frame(Frequency = names(freq), Counts = as.integer(freq))
# Print the dataframe as an Excel file
write_xlsx(df, paste0(save_l, "Frequency.xlsx"))