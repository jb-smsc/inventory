# read inventory

library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(stringr)
# library(purrr)

fn_in <- "data-raw/copy_2023-06-27_2023-05-15_JB-SMSC-Spreadsheet-In-situ-meta-information2.xlsx"

all_sheets <- excel_sheets(fn_in)
all_sheets <- all_sheets[all_sheets != "Snow information_template"] # template
all_sheets <- all_sheets[all_sheets != "Snow information Alps"] # first empty sheet
all_sheets <- all_sheets[all_sheets != "Tabelle1"] # some non-existing sheet

read_xlsx(fn_in, all_sheets[1]) %>% 
  as.matrix %>% 
  unname() -> mat_names
names_cols <- c(mat_names[2, 1:9], mat_names[3, 10:15], mat_names[2, 16])


# manual read in ----------------------------------------------------------


# 1 hkh -------------------------------------------------------------------

tbl_in <- read_xlsx(fn_in, all_sheets[1], skip = 4, col_names = names_cols)
tbl_in %>% 
  mutate(Longitude = str_replace(Longitude, ",", "."),
         Latitude = str_replace(Latitude, ",", ".")) %>% 
  mutate(Longitude = parse_number(Longitude),
         Latitude = parse_number(Latitude)) -> tbl1_hkh


# 2 rofental -------------------------------------------------------------------

tbl_in <- read_xlsx(fn_in, all_sheets[2], skip = 4, col_names = names_cols)
tbl_in[1:3, ]  %>% 
  mutate(Longitude = parse_number(Longitude),
         Latitude = parse_number(Latitude),
         Begin = year(Begin),
         End = NA) -> tbl2_rofental


# 3 Geosphere Austria -------------------------------------------------------------

# needs updated coords! or epsg info

tbl_in <- read_xlsx(fn_in, all_sheets[3], skip = 4, col_names = names_cols)

tbl3_geosphere

# 4 swiss alps ------------------------------------------------------------

tbl_in <- read_xlsx(fn_in, all_sheets[4], skip = 4, col_names = names_cols[1:14])
tbl_in %>% 
  mutate(Latitude = str_c(str_sub(Latitude, 1, 2), ".", str_sub(Latitude, 3))) %>% 
  mutate(Latitude = parse_number(Latitude)) %>% 
  mutate(Longitude = if_else(str_sub(Longitude, 1, 1) == "1",
                             str_c(str_sub(Longitude, 1, 2), ".", str_sub(Longitude, 3)),
                             str_c(str_sub(Longitude, 1, 1), ".", str_sub(Longitude, 2)))) %>% 
  mutate(Longitude = parse_number(Longitude)) -> tbl4_swiss
  

# 5 european alps tc2021 --------------------------------------------------

tbl_in <- read_xlsx(fn_in, all_sheets[5], skip = 4, col_names = names_cols[1:11])
tbl5_alps_tc2021 <- tbl_in




# 6 french alps -----------------------------------------------------------

tbl_in <- read_xlsx(fn_in, all_sheets[6], skip = 4, col_names = names_cols)
tbl_in %>% 
  mutate(Longitude = parse_number(Longitude, locale = locale(decimal_mark = ",")),
         Latitude = parse_number(Latitude),
         `Altitude (m)` = parse_number(`Altitude (m)`)) %>% 
  mutate(Latitude = if_else(Latitude > 90,
                            parse_number(str_c(str_sub(Latitude, 1, 2), ".", str_sub(Latitude, 3))),
                            Latitude)) -> tbl6_french_alps










# remove duplicates? ------------------------------------------------------

# remove slf from tc2021





  


