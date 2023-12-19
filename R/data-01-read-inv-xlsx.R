# read inventory

library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(stringr)
# library(purrr)

fn_in <- "data-raw/copy_2023-12-18_2023-05-15_JB-SMSC-Spreadsheet-In-situ-meta-information2.xlsx"

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
         Latitude = parse_number(Latitude)) %>% 
  mutate(long_old = Longitude, lat_old = Latitude) %>% 
  mutate(Longitude = if_else(str_starts(ID, "NP_") & !is.na(ID),
                             long_old,
                             lat_old),
         Latitude = if_else(str_starts(ID, "NP_") & !is.na(ID),
                            lat_old,
                            long_old)) %>% 
  select(-long_old, -lat_old) %>% 
  mutate(SWE = if_else(str_starts(ID, "SSS") & !is.na(ID), "YES", SWE),
         `Depth of snowfall` = if_else(str_starts(ID, "SSS") & !is.na(ID), "YES", `Depth of snowfall`)) -> tbl_01_hkh


# 2 rofental -------------------------------------------------------------------

tbl_in <- read_xlsx(fn_in, all_sheets[2], skip = 4, col_names = names_cols)
tbl_in[1:3, ]  %>% 
  mutate(Longitude = parse_number(Longitude),
         Latitude = parse_number(Latitude),
         Begin = year(Begin),
         End = NA) -> tbl_02_rofental


# 3 Geosphere Austria -------------------------------------------------------------

tbl_in <- read_xlsx(fn_in, all_sheets[3], skip = 4, 
                    col_names = c(names_cols, str_c("V", 1:3)))

dms2dec_austria <- function(x){
  # sign(x)
  x_abs <- abs(x)
  s <- str_sub(x_abs, start = - 2) %>% as.numeric
  m <- str_sub(x_abs, start = - 4, end = -3) %>% as.numeric
  d <- str_sub(x_abs, end = -5) %>% as.numeric
  
  dec <- sign(x) * (d + m/60 + s/3600)
  dec
}

tbl_in %>% 
  mutate(Latitude = dms2dec_austria(Latitude)) %>% 
  mutate(Longitude = dms2dec_austria(Longitude)) %>% 
  mutate(Begin = year(ymd(Begin)),
         End = year(ymd(End))) -> tbl_03_geosphere

# 4 swiss alps ------------------------------------------------------------

tbl_in <- read_xlsx(fn_in, all_sheets[4], skip = 4, col_names = names_cols[1:14])
tbl_in %>% 
  mutate(Latitude = str_c(str_sub(Latitude, 1, 2), ".", str_sub(Latitude, 3))) %>% 
  mutate(Latitude = parse_number(Latitude)) %>% 
  mutate(Longitude = if_else(str_sub(Longitude, 1, 1) == "1",
                             str_c(str_sub(Longitude, 1, 2), ".", str_sub(Longitude, 3)),
                             str_c(str_sub(Longitude, 1, 1), ".", str_sub(Longitude, 2)))) %>% 
  mutate(Longitude = parse_number(Longitude)) -> tbl_04_swiss
  

# 5 european alps tc2021 --------------------------------------------------

tbl_in <- read_xlsx(fn_in, all_sheets[5], skip = 4, col_names = names_cols[1:11])
tbl_05_alps_tc2021 <- tbl_in




# 6 french alps -----------------------------------------------------------

tbl_in <- read_xlsx(fn_in, all_sheets[6], skip = 4, col_names = names_cols, 
                    na = c("", "now"))
tbl_in %>% 
  mutate(Longitude = parse_number(Longitude, locale = locale(decimal_mark = ",")),
         Latitude = parse_number(Latitude),
         `Altitude (m)` = parse_number(`Altitude (m)`)) %>% 
  mutate(Latitude = if_else(Latitude > 90,
                            parse_number(str_c(str_sub(Latitude, 1, 2), ".", str_sub(Latitude, 3))),
                            Latitude)) -> tbl_06_french_alps



# 7 Canada ----------------------------------------------------------------

tbl_in <- read_xlsx(fn_in, all_sheets[7], skip = 4, col_names = names_cols)
tbl_07_canada <- tbl_in



# 8 chilean andes ---------------------------------------------------------

tbl_in <- read_xlsx(fn_in, all_sheets[8], skip = 4, col_names = names_cols)
tbl_08_chilean_andes <- tbl_in

# 9 australian alps -------------------------------------------------------

tbl_in <- read_xlsx(fn_in, all_sheets[9], skip = 4, col_names = names_cols[2:16], 
                    na = c("", "Ongoing"))
tbl_in %>% 
  mutate(Longitude = parse_number(Longitude),
         Latitude = - parse_number(Latitude)) -> tbl_09_australian_alps


# 10 pyrenees -------------------------------------------------------------


tbl_in <- read_xlsx(fn_in, all_sheets[10], skip = 4, col_names = names_cols[2:16],
                    na = c("", "now"))
tbl_in %>% 
  mutate(Longitude = Longitude %>% 
           str_replace(",", ".") %>% 
           str_replace("−", "-") %>% 
           parse_number,
         Begin = year(Begin),
         End = year(End)) -> tbl_10_pyrenees





# 11 corsica --------------------------------------------------------------

col_types <- rep("guess", 15)
col_types[names_cols[2:16] == "End"] <- "date"
tbl_in <- read_xlsx(fn_in, all_sheets[11], skip = 4, col_names = names_cols[2:16], 
                    na = c("", "now"), col_types = col_types)
tbl_in %>% 
  mutate(Begin = year(Begin),
         End = year(End)) -> tbl_11_corsica


# 12 Dinaric Alps ---------------------------------------------------------

tbl_in <- read_xlsx(fn_in, all_sheets[12], skip = 4, col_names = names_cols[1:15])
tbl_in %>% 
  mutate(Longitude = parse_number(Longitude),
         Latitude = parse_number(Latitude),
         Begin = year(ymd(Begin))) -> tbl_12_dinaric_alps


# 13 southern andes argentina ---------------------------------------------

tbl_in <- read_xlsx(fn_in, all_sheets[13], skip = 4, col_names = names_cols)
tbl_in %>% 
  mutate(Latitude = str_c(str_sub(Latitude, 1, 3), ".", str_sub(Latitude, 4))) %>% 
  mutate(Latitude = parse_number(Latitude)) %>% 
  mutate(Longitude = str_c(str_sub(Longitude, 1, 3), ".", str_sub(Longitude, 4))) %>% 
  mutate(Longitude = parse_number(Longitude)) %>% 
  mutate(Begin = year(ymd(Begin, truncated = 2)),
         End = year(ymd(End, truncated = 2))) -> tbl_13_southern_andes_argentina
  



# 14 western carpathians (slovakia) ---------------------------------------

tbl_in <- read_xlsx(fn_in, all_sheets[14], skip = 4, col_names = names_cols)
tbl_14_slovakia <- tbl_in



# 15 atlas ----------------------------------------------------------------

tbl_in <- read_xlsx(fn_in, all_sheets[15], skip = 4, col_names = names_cols[2:16])
tbl_15_atlas <- tbl_in




# 16 Mt Lebanon -----------------------------------------------------------

tbl_in <- read_xlsx(fn_in, all_sheets[16], skip = 4, col_names = names_cols)
tbl_in %>% 
  mutate(Latitude = parse_number(Latitude)) %>% 
  mutate(Longitude = parse_number(Longitude))  -> tbl_16_mt_lebanon


# 17 spain ----------------------------------------------------------------

dms2dec_spain <- function(x){
  s <- str_sub(x, start = - 2) %>% as.numeric
  m <- str_sub(x, start = - 4, end = -3) %>% as.numeric
  d <- str_sub(x, end = -5) %>% as.numeric
  d + m/60 + s/3600
}

dms_lon_spain <- function(x){
  # "Si, grados, minutos y segundos y en el caso de la longitud se añade al final 
  # un 1 para orientación E o 2 para W."
  EW <- str_sub(x, start = -1)
  if_else(EW == "1", 1, -1)
  s <- str_sub(x, start = -3, end = -2) %>% as.numeric
  m <- str_sub(x, start = -5, end = -4) %>% as.numeric
  m[is.na(m)] <- 0
  d <- str_sub(x, end = -6) %>% as.numeric
  d[is.na(d)] <- 0
  
  if_else(EW == "1", 1, -1) * (d + m/60 + s/3600)
}

tbl_in <- read_xlsx(fn_in, all_sheets[17], skip = 4, col_names = names_cols)
tbl_in %>% 
  mutate(Begin = year(Begin),
         Latitude = dms2dec_spain(Latitude),
         Longitude = dms_lon_spain(Longitude)) -> tbl_17_spain
  


# 18 Greenland -----------------------------------------------------------

tbl_in <- read_xlsx(fn_in, all_sheets[18], skip = 4, col_names = names_cols)
tbl_in %>% 
  mutate(Begin = year(Begin), 
         End = year(End),
         Longitude = as.numeric(Longitude), 
         Latitude = as.numeric(Latitude)) -> tbl_18_greenland



# 19 Japanese mountains -----------------------------------------------------------

tbl_in <- read_xlsx(fn_in, all_sheets[19], skip = 4, col_names = names_cols)
# tbl_in %>% 
#   mutate(Begin = year(Begin), End = year(End)) -> tbl_19_japanese_mountains
# 
tbl_19_japanese_mountains <- tibble()

# combine -----------------------------------------------------------------

l_all <- list(
  tbl_01_hkh,
  tbl_02_rofental,
  tbl_03_geosphere,
  tbl_04_swiss,
  tbl_05_alps_tc2021,
  tbl_06_french_alps,
  tbl_07_canada,
  tbl_08_chilean_andes,
  tbl_09_australian_alps,
  tbl_10_pyrenees,
  tbl_11_corsica,
  tbl_12_dinaric_alps,
  tbl_13_southern_andes_argentina,
  tbl_14_slovakia,
  tbl_15_atlas,
  tbl_16_mt_lebanon,
  tbl_17_spain,
  tbl_18_greenland,
  tbl_19_japanese_mountains
)

names(l_all) <- all_sheets

tbl_all <- bind_rows(l_all, .id = "sheet_name")

# remove NA rows
tbl_all <- tbl_all %>% filter(!is.na(Longitude))


# remove duplicates? ------------------------------------------------------

# remove slf from tc2021
tbl_all <- tbl_all %>% 
  filter(!str_starts(ID, "CH_SLF_") | is.na(ID))

saveRDS(tbl_all, "data/inventory-01-read.rds")




