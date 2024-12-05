# read inventory

library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(stringr)
# library(purrr)

fn_in <- "data-raw/copy_2024-12-04_2023-05-15_JB-SMSC-Spreadsheet-In-situ-meta-information2.xlsx"

all_sheets <- excel_sheets(fn_in)
all_sheets <- all_sheets[all_sheets != "Snow information_template"] # template
all_sheets <- all_sheets[all_sheets != "Snow information Alps"] # first empty sheet
all_sheets <- all_sheets[all_sheets != "Tabelle1"] # some non-existing sheet

read_xlsx(fn_in, all_sheets[1], col_names = str_c("V", 1:25)) %>% 
  as.matrix %>% 
  unname() -> mat_names
names_cols <- c(mat_names[3, 1:9], mat_names[4, 10:15], mat_names[3, 16])


all_sheets_manual <- character(length(all_sheets))

# manual read in ----------------------------------------------------------


# 1 hkh -------------------------------------------------------------------

tbl_in <- read_xlsx(fn_in, "Snow information_HKH", skip = 4, col_names = names_cols)
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

all_sheets_manual[1] <- "Snow information_HKH"

# 2 rofental -------------------------------------------------------------------

tbl_in <- read_xlsx(fn_in, "Snow information Rofental", skip = 4, col_names = names_cols)
tbl_in[1:3, ]  %>% 
  mutate(Longitude = parse_number(Longitude),
         Latitude = parse_number(Latitude),
         Begin = year(Begin),
         End = NA) -> tbl_02_rofental

all_sheets_manual[2] <- "Snow information Rofental"

# 3 Geosphere Austria -------------------------------------------------------------

tbl_in <- read_xlsx(fn_in, "GeoSphere Austria", skip = 4, 
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

all_sheets_manual[3] <- "GeoSphere Austria"

# 4 swiss alps ------------------------------------------------------------

tbl_in <- read_xlsx(fn_in, "Swiss Alps", skip = 4, col_names = names_cols[1:14])
tbl_in %>% 
  mutate(Latitude = str_c(str_sub(Latitude, 1, 2), ".", str_sub(Latitude, 3))) %>% 
  mutate(Latitude = parse_number(Latitude)) %>% 
  mutate(Longitude = if_else(str_sub(Longitude, 1, 1) == "1",
                             str_c(str_sub(Longitude, 1, 2), ".", str_sub(Longitude, 3)),
                             str_c(str_sub(Longitude, 1, 1), ".", str_sub(Longitude, 2)))) %>% 
  mutate(Longitude = parse_number(Longitude)) -> tbl_04_swiss
  
all_sheets_manual[4] <- "Swiss Alps"


# 5 european alps tc2021 --------------------------------------------------

tbl_in <- read_xlsx(fn_in, "European Alps (TC2021)", skip = 4, col_names = names_cols[1:11])
tbl_05_alps_tc2021 <- tbl_in

all_sheets_manual[5] <- "European Alps (TC2021)"


# 6 french alps -----------------------------------------------------------

tbl_in <- read_xlsx(fn_in, "French Alps (in addition to TC)", skip = 4, col_names = names_cols, 
                    na = c("", "now"))
tbl_in %>% 
  mutate(Longitude = parse_number(Longitude, locale = locale(decimal_mark = ",")),
         Latitude = parse_number(Latitude),
         `Altitude (m)` = parse_number(`Altitude (m)`),
         ID = as.character(ID)) %>% 
  mutate(Latitude = if_else(Latitude > 90,
                            parse_number(str_c(str_sub(Latitude, 1, 2), ".", str_sub(Latitude, 3))),
                            Latitude)) -> tbl_06_french_alps

all_sheets_manual[6] <- "French Alps (in addition to TC)"

# 7 Canada ----------------------------------------------------------------

tbl_in <- read_xlsx(fn_in, "Canada", skip = 4, col_names = names_cols)
tbl_07_canada <- tbl_in

all_sheets_manual[7] <- "Canada"

# 8 chilean andes ---------------------------------------------------------

tbl_in <- read_xlsx(fn_in, "Chilean Andes", skip = 4, col_names = names_cols)
tbl_08_chilean_andes <- tbl_in

all_sheets_manual[8] <- "Chilean Andes"

# 9 australian alps -------------------------------------------------------

tbl_in <- read_xlsx(fn_in, "Australian Alps (NSW)", skip = 4, col_names = names_cols[2:16], 
                    na = c("", "Ongoing"))
tbl_in %>% 
  mutate(Longitude = parse_number(Longitude),
         Latitude = - parse_number(Latitude)) -> tbl_09_australian_alps

all_sheets_manual[9] <- "Australian Alps (NSW)"

# 10 pyrenees -------------------------------------------------------------


tbl_in <- read_xlsx(fn_in, "Pyrenees", skip = 4, col_names = names_cols[2:16],
                    na = c("", "now"))
tbl_in %>% 
  mutate(Longitude = Longitude %>% 
           str_replace(",", ".") %>% 
           str_replace("−", "-") %>% 
           parse_number,
         Begin = year(Begin),
         End = year(End)) -> tbl_10_pyrenees


all_sheets_manual[10] <- "Pyrenees"


# 11 corsica --------------------------------------------------------------

col_types <- rep("guess", 15)
col_types[names_cols[2:16] == "End"] <- "date"
tbl_in <- read_xlsx(fn_in, "Corsica", skip = 4, col_names = names_cols[2:16], 
                    na = c("", "now"), col_types = col_types)
tbl_in %>% 
  mutate(Begin = year(Begin),
         End = year(End)) -> tbl_11_corsica

all_sheets_manual[11] <- "Corsica"

# 12 Dinaric Alps ---------------------------------------------------------

tbl_in <- read_xlsx(fn_in, "Dinaric Alps" , skip = 4, col_names = names_cols[1:15])
tbl_in %>% 
  mutate(Longitude = parse_number(Longitude),
         Latitude = parse_number(Latitude),
         Begin = year(ymd(Begin))) -> tbl_12_dinaric_alps

all_sheets_manual[12] <- "Dinaric Alps"

# 13 southern andes argentina ---------------------------------------------

tbl_in <- read_xlsx(fn_in, "Southern Andes Argentina", skip = 4, col_names = names_cols)
tbl_in %>% 
  mutate(Latitude = str_c(str_sub(Latitude, 1, 3), ".", str_sub(Latitude, 4))) %>% 
  mutate(Latitude = parse_number(Latitude)) %>% 
  mutate(Longitude = str_c(str_sub(Longitude, 1, 3), ".", str_sub(Longitude, 4))) %>% 
  mutate(Longitude = parse_number(Longitude)) %>% 
  mutate(Begin = year(ymd(Begin, truncated = 2)),
         End = year(ymd(End, truncated = 2))) -> tbl_13_southern_andes_argentina
  
all_sheets_manual[13] <- "Southern Andes Argentina"


# 14 western carpathians (slovakia) ---------------------------------------

tbl_in <- read_xlsx(fn_in, "Western Carpathians (Slovakia)", skip = 4, col_names = names_cols)
tbl_14_slovakia <- tbl_in

all_sheets_manual[14] <- "Western Carpathians (Slovakia)"

# 15 atlas ----------------------------------------------------------------

tbl_in <- read_xlsx(fn_in, "Atlas", skip = 4, col_names = names_cols[2:16])
tbl_15_atlas <- tbl_in

all_sheets_manual[15] <- "Atlas"


# 16 Mt Lebanon -----------------------------------------------------------

tbl_in <- read_xlsx(fn_in, "Mt Lebanon", skip = 4, col_names = names_cols)
tbl_in %>% 
  mutate(Latitude = parse_number(Latitude)) %>% 
  mutate(Longitude = parse_number(Longitude))  -> tbl_16_mt_lebanon


all_sheets_manual[16] <- "Mt Lebanon"

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

tbl_in <- read_xlsx(fn_in, "Spain", skip = 4, col_names = names_cols)
tbl_in %>% 
  mutate(Begin = year(Begin),
         Latitude = dms2dec_spain(Latitude),
         Longitude = dms_lon_spain(Longitude)) -> tbl_17_spain
  
all_sheets_manual[17] <- "Spain"


# 18 Greenland -----------------------------------------------------------

tbl_in <- read_xlsx(fn_in, "Greenland", skip = 4, col_names = names_cols)
tbl_in %>% 
  mutate(Begin = year(Begin), 
         End = year(End),
         Longitude = as.numeric(Longitude), 
         Latitude = as.numeric(Latitude)) -> tbl_18_greenland

all_sheets_manual[18] <- "Greenland"

# 19 Japanese mountains -----------------------------------------------------------

tbl_in <- read_xlsx(fn_in, "Japanese mountains", skip = 4, col_names = names_cols)
tbl_in %>%
  mutate(Begin = year(my(Begin)),
         Longitude = as.numeric(Longitude), 
         Latitude = as.numeric(Latitude)) -> tbl_19_japanese_mountains

all_sheets_manual[19] <- "Japanese mountains"

# 20 Russia ---------------------------------------------------------------

tbl_in <- read_xlsx(fn_in, "Russia", skip = 4, col_names = names_cols[1:13])
tbl_in %>% 
  mutate(ID = as.character(ID))-> tbl_20_russia

all_sheets_manual[20] <- "Russia"


# 21 US_Northeast ---------------------------------------------------------

tbl_in <- read_xlsx(fn_in, "US_Northeast", skip = 4, col_names = names_cols)
tbl_in -> tbl_21_us_ne

all_sheets_manual[21] <- "US_Northeast"

# 22 US_NRCS --------------------------------------------------------------

tbl_in <- read_xlsx(fn_in, "US_NRCS", skip = 4, col_names = names_cols)
tbl_in -> tbl_22_us_nrcs

all_sheets_manual[22] <- "US_NRCS"

# 23 US_SNOTEL --------------------------------------------------------------

tbl_in <- read_xlsx(fn_in, "US_SNOTEL", skip = 4, col_names = names_cols)
tbl_in -> tbl_23_us_snotel

all_sheets_manual[23] <- "US_SNOTEL"


# 24 China --------------------------------------------------------------

tbl_in <- read_xlsx(fn_in, "China", skip = 4, col_names = names_cols[1:10])
tbl_in %>%
  mutate(Begin = year(ymd(Begin)),
         Longitude = as.numeric(Longitude), 
         Latitude = as.numeric(Latitude),
         `Altitude (m)` = as.numeric(`Altitude (m)`)) -> tbl_24_china

all_sheets_manual[24] <- "China"


# 25 Romania --------------------------------------------------------------

tbl_in <- read_xlsx(fn_in, "Romania", skip = 4, col_names = names_cols,
                    na = c("", "present"))
tbl_in %>%
  mutate(Begin = year(dmy(Begin)),
         Longitude = as.numeric(Longitude), 
         Latitude = as.numeric(Latitude),
         `Altitude (m)` = as.numeric(`Altitude (m)`)) -> tbl_25_romania

all_sheets_manual[25] <- "Romania"

# 26 Sierra Nevada (US) --------------------------------------------------------------
f_date_sierra <- function(x){
  out <- numeric(length(x))
  out[!is.na(x) & str_detect(x, fixed("/"))] <- year(mdy(x[!is.na(x) & str_detect(x, fixed("/"))]))
  out[!is.na(x) & !str_detect(x, fixed("/"))] <- as.numeric(x[!is.na(x) & !str_detect(x, fixed("/"))])
  out
}

tbl_in <- read_xlsx(fn_in, "Sierra Nevada (US)", skip = 4, col_names = names_cols, 
                    na = c("", "Present", "--", "2230-2700"))
tbl_in %>%
  mutate(Begin = f_date_sierra(Begin),
         End = f_date_sierra(End),
         Longitude = as.numeric(Longitude), 
         Latitude = as.numeric(Latitude),
         `Altitude (m)` = as.numeric(`Altitude (m)`)) -> tbl_26_sierra_nevada

all_sheets_manual[26] <- "Sierra Nevada (US)"


# 27 Central Asia (Uzbekistan) --------------------------------------------------------------

tbl_in <- read_xlsx(fn_in, "Central Asia (Uzbekistan)", skip = 4, col_names = names_cols[2:10])
tbl_in %>%
  mutate(Begin = year(dmy(Begin)),
         Longitude = as.numeric(str_replace(Longitude, ",", ".")), 
         Latitude = as.numeric(str_replace(Latitude, ",", ".")),
         `Altitude (m)` = as.numeric(`Altitude (m)`)) -> tbl_27_casia_uzbekistan

all_sheets_manual[27] <- "Central Asia (Uzbekistan)"


# 28 Central Asia (Kazakhstan) --------------------------------------------------------------

dms2dec_kazakh <- function(x){
  x2 <- str_split_fixed(x, "[⁰|'| ]", 3)
  s <- x2[, 3] %>% str_replace(",", ".") %>% parse_number()
  m <- x2[, 2] %>% str_trim() %>% as.numeric
  d <- x2[, 1] %>% str_trim() %>% as.numeric
  d + m/60 + s/3600
}

tbl_in <- read_xlsx(fn_in, "Central Asia (Kazakhstan)", skip = 4, col_names = names_cols[1:10])
tbl_in %>% 
  mutate(Begin = as.numeric(Begin),
         End = as.numeric(End),
         Longitude = dms2dec_kazakh(Longitude),
         Latitude = dms2dec_kazakh(Latitude),
         `Altitude (m)` = as.numeric(`Altitude (m)`)
         ) -> tbl_28_casia_kazakhstan

all_sheets_manual[28] <- "Central Asia (Kazakhstan)"

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
  tbl_19_japanese_mountains,
  tbl_20_russia,
  tbl_21_us_ne,
  tbl_22_us_nrcs,
  tbl_23_us_snotel,
  tbl_24_china,
  tbl_25_romania,
  tbl_26_sierra_nevada,
  tbl_27_casia_uzbekistan,
  tbl_28_casia_kazakhstan
)

names(l_all) <- all_sheets_manual

tbl_all <- bind_rows(l_all, .id = "sheet_name")

# remove NA rows
tbl_all <- tbl_all %>% filter(!is.na(Longitude))


# remove duplicates? ------------------------------------------------------

# remove slf from tc2021
tbl_all <- tbl_all %>% 
  filter(!str_starts(ID, "CH_SLF_") | is.na(ID))


# clean columns: freq to albedo  -----------------------------------------------------------

# manual testing
# tbl_all$Frequency %>% str_to_lower() %>% table(useNA = "a")
# tbl_all$Type %>% str_to_lower() %>%
#   str_replace("^a$", "automatic") %>% 
#   str_replace("^auto$", "automatic") %>% 
#   str_replace("^m$", "manual") %>% 
#   str_replace(fixed("automatic/manual"), "manual/automatic") %>% 
#   str_replace(fixed("manual to automatic"), "manual/automatic") %>% 
#   str_replace(fixed("manual snow course"), "manual") %>% 
#   str_replace(fixed("undefined & ruler"), "manual") %>% 
#   str_replace(fixed("undefined & sr50"), "automatic") %>% 
#   str_replace(fixed("undefined, ruler & sr50"), "manual/automatic") %>% 
#   str_replace(fixed("undefined"), NA) %>% 
  # table(useNA = "a")
# tbl_all %>% filter(is.na(Type))
# tbl_all %>% filter(Type == "automatic/manual")
# tbl_all %>% filter(Type == "manual to automatic")
# tbl_all %>% filter(Type == "undefined")
# tbl_all %>% filter(Type == "undefined & ruler")
# 
# tbl_all$`Snow depth` %>% 
#   str_replace("^x|X$", "YES") %>% 
#   str_replace(fixed("Campbell SR50"), "YES") %>% 
#   str_replace(fixed("Jenoptik/Lufft SHM30"), "YES") %>% 
#   str_replace(fixed("Lufft SHM31"), "YES") %>% 
#   table(useNA = "a")
# tbl_all %>% filter(is.na(`Snow depth`))
# 
# tbl_all$`Depth of snowfall` %>% 
#   str_replace("^x|X$", "YES") %>% 
#   table(useNA = "a")
# 
# tbl_all$SWE %>% 
#   str_replace("^x|X$", "YES") %>% 
#   table(useNA = "a")
# 
# tbl_all$`Bulk snow density` %>%
#   str_replace("^x|X$", "YES") %>%
#   str_replace(fixed("NO*"), "YES") %>%
#   str_replace(fixed("Derive from HS and SWE"), "YES") %>%
#   str_replace(fixed("YES (derived from HS and SWE)"), "YES") %>%
#   str_replace(fixed("manually measured in snow pit once a year around May, 1st"), "YES") %>%
#   table(useNA = "a")
# tbl_all %>% filter(`Bulk snow density` == "NO*")
# 
# tbl_all$`Snow albedo` %>%
#   str_replace("^x|X$", "YES") %>%
#   str_replace(fixed("Kipp & Zonen Net Radiometer CNR 4"), "YES") %>%
#   table(useNA = "a")


# all-in-one
tbl_all2 <- tbl_all %>% mutate(
  Frequency = str_to_lower(Frequency),
  Type = Type %>% 
    str_to_lower() %>%
    str_replace("^a$", "automatic") %>% 
    str_replace("^auto$", "automatic") %>% 
    str_replace("^m$", "manual") %>% 
    str_replace(fixed("automatic/manual"), "manual/automatic") %>% 
    str_replace(fixed("manual to automatic"), "manual/automatic") %>% 
    str_replace(fixed("manual snow course"), "manual") %>% 
    str_replace(fixed("undefined & ruler"), "manual") %>% 
    str_replace(fixed("undefined & sr50"), "automatic") %>% 
    str_replace(fixed("undefined, ruler & sr50"), "manual/automatic") %>% 
    str_replace(fixed("undefined"), NA),
  `Snow depth` = `Snow depth` %>% 
    str_replace("^x|X$", "YES") %>% 
    str_replace(fixed("Campbell SR50"), "YES") %>% 
    str_replace(fixed("Jenoptik/Lufft SHM30"), "YES") %>% 
    str_replace(fixed("Lufft SHM31"), "YES"),
  `Depth of snowfall` = `Depth of snowfall` %>% 
    str_replace("^x|X$", "YES"),
  SWE = SWE %>% 
    str_replace("^x|X$", "YES"),
  `Bulk snow density` = `Bulk snow density` %>% 
    str_replace("^x|X$", "YES") %>% 
    str_replace(fixed("NO*"), "YES") %>% 
    str_replace(fixed("Derive from HS and SWE"), "YES") %>% 
    str_replace(fixed("YES (derived from HS and SWE)"), "YES") %>%
    str_replace(fixed("manually measured in snow pit once a year around May, 1st"), "YES"),
  `Snow albedo` = `Snow albedo` %>% 
    str_replace("^x|X$", "YES") %>% 
    str_replace(fixed("Kipp & Zonen Net Radiometer CNR 4"), "YES")
) %>% 
  mutate(across(`Snow depth`:`Snow albedo`, \(x) if_else(is.na(x), "NO", x))) # replace NA with no

tbl_all2 %>% 
  summarise(across(`Snow depth`:`Snow albedo`, \(x) sum(is.na(x)) == 0)) %>% 
  all %>% 
  stopifnot()

tbl_all2 %>% 
  select(`Snow depth`:`Snow albedo`) %>% 
  summarise(across(everything(), \(x) sum(!x %in% c("YES", "NO")) == 0)) %>% 
  all %>% 
  stopifnot()


# get missing elevation from public DEM -----------------------------------

# prefill from previous version
tbl_old <- readRDS("data/inventory-01-read.rds")
fs::file_copy("data/inventory-01-read.rds", "data/inventory-01-read_backup.rds", overwrite = T)

tbl_all3 <- tbl_all2 %>% 
  rows_patch(
    tbl_old %>% 
      select(sheet_name, ID, `Station name`, `Altitude (m)`, 
             Begin, End, `Snow depth`, `Depth of snowfall`),
    by = c("sheet_name", "ID", "Station name", 
           "Begin", "End", "Snow depth", "Depth of snowfall"),
    unmatched = "ignore"
  )

if(any(is.na(tbl_all3$`Altitude (m)`))){
  sf_elev_missing <- tbl_all3 %>% 
    filter(is.na(`Altitude (m)`)) %>% 
    select(sheet_name:Latitude) %>% 
    sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
  
  # might take very long (depending on internet connection!), decrease z (zoom level)
  # z=5 ~ 3.5 km resolution at lat=45
  # z=6 ~ 1.7 km resolution at lat=45
  # z=7 ~ 800m resolution at lat=45
  tbl_elev <- elevatr::get_elev_point(sf_elev_missing, src = "aws", z = 6)
  
  tbl_out <- tbl_all3 %>% 
    rows_patch(
      tbl_elev %>% 
        sf::st_drop_geometry() %>% 
        select(sheet_name, ID, `Station name`, `Altitude (m)` = elevation),
      by = c("sheet_name", "ID", "Station name")
    )
} else {
  tbl_out <- tbl_all3
}


# save --------------------------------------------------------------------


saveRDS(tbl_out, "data/inventory-01-read.rds")
