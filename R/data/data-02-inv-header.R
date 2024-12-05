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

all_sheets_manual <- character(length(all_sheets))

# manual read in ----------------------------------------------------------


# 1 hkh -------------------------------------------------------------------


tbl_in <- read_xlsx(fn_in, "Snow information_HKH", 
                    range = "F1:P2", col_names = str_c("col", 1:11), col_types = "text")
tbl_01_hkh <- tibble(
  region = tbl_in[1,] %>% pull(col1),
  creator_expert = tbl_in[2,] %>% 
    mutate(ff = str_c(col2, col3, col4, sep = ", ")) %>% 
    pull(ff)
)

all_sheets_manual[1] <- "Snow information_HKH"

# 2 rofental -------------------------------------------------------------------


tbl_in <- read_xlsx(fn_in, "Snow information Rofental", 
                    range = "F1:P2", col_names = str_c("col", 1:11), col_types = "text")
tbl_02_rofental <- tibble(
  region = tbl_in[1,] %>% pull(col1),
  creator_expert = tbl_in[2,] %>% pull(col2),
)

all_sheets_manual[2] <- "Snow information Rofental"

# 3 Geosphere Austria -------------------------------------------------------------

tbl_in <- read_xlsx(fn_in, "GeoSphere Austria", 
                    range = "F1:P2", col_names = str_c("col", 1:11), col_types = "text")
tbl_03_geosphere <- tibble(
  region = "Austria",
  creator_expert = "Bernhard Hynek (Geosphere)",
)

all_sheets_manual[3] <- "GeoSphere Austria"


# 4 swiss alps ------------------------------------------------------------

tbl_in <- read_xlsx(fn_in, "Swiss Alps", 
                    range = "F1:P2", col_names = str_c("col", 1:11), col_types = "text")
tbl_04_swiss <- tibble(
  region = tbl_in[1,] %>% pull(col1),
  creator_expert = tbl_in[2,] %>% pull(col1),
)

all_sheets_manual[4] <- "Swiss Alps"

# 5 european alps tc2021 --------------------------------------------------


tbl_in <- read_xlsx(fn_in, "European Alps (TC2021)", 
                    range = "F1:P2", col_names = str_c("col", 1:11), col_types = "text")
tbl_05_alps_tc2021 <- tibble(
  region = tbl_in[1,] %>% pull(col1),
  creator_expert = tbl_in[2,] %>% pull(col2),
)

all_sheets_manual[5] <- "European Alps (TC2021)"

# 6 french alps -----------------------------------------------------------

tbl_in <- read_xlsx(fn_in, "French Alps (in addition to TC)", 
                    range = "F1:P2", col_names = str_c("col", 1:11), col_types = "text")
tbl_06_french_alps <- tibble(
  region = "French Alps (in addition to TC)",
  creator_expert = tbl_in[2,] %>% pull(col3),
)

all_sheets_manual[6] <- "French Alps (in addition to TC)"


# 7 Canada ----------------------------------------------------------------

tbl_in <- read_xlsx(fn_in, "Canada", 
                    range = "F1:P2", col_names = str_c("col", 1:11), col_types = "text")
tbl_07_canada <- tibble(
  region = tbl_in[1,] %>% pull(col1),
  creator_expert = tbl_in[2,] %>% pull(col2),
)

all_sheets_manual[7] <- "Canada"

# 8 chilean andes ---------------------------------------------------------

tbl_in <- read_xlsx(fn_in, "Chilean Andes", 
                    range = "F1:P2", col_names = str_c("col", 1:11), col_types = "text")
tbl_08_chilean_andes <- tibble(
  region = tbl_in[1,] %>% pull(col1),
  creator_expert = "Simone Schauwecker",
)

all_sheets_manual[8] <- "Chilean Andes"

# 9 australian alps -------------------------------------------------------

tbl_in <- read_xlsx(fn_in, "Australian Alps (NSW)", 
                    range = "F1:P2", col_names = str_c("col", 1:11), col_types = "text")
tbl_09_australian_alps <- tibble(
  region = tbl_in[1,] %>% pull(col1),
  creator_expert = tbl_in[2,] %>% pull(col2),
)

all_sheets_manual[9] <- "Australian Alps (NSW)"

# 10 pyrenees -------------------------------------------------------------

tbl_in <- read_xlsx(fn_in, "Pyrenees", 
                    range = "F1:P2", col_names = str_c("col", 1:11), col_types = "text")
tbl_10_pyrenees <- tibble(
  region = "Pyrenees",
  creator_expert = tbl_in[2,] %>% pull(col3),
)

all_sheets_manual[10] <- "Pyrenees"

# 11 corsica --------------------------------------------------------------


tbl_in <- read_xlsx(fn_in, "Corsica", 
                    range = "F1:P2", col_names = str_c("col", 1:11), col_types = "text")
tbl_11_corsica <- tibble(
  region = "Corsica",
  creator_expert = tbl_in[2,] %>% pull(col3),
)

all_sheets_manual[11] <- "Corsica"

# 12 Dinaric Alps ---------------------------------------------------------

tbl_in <- read_xlsx(fn_in, "Dinaric Alps", 
                    range = "F1:P2", col_names = str_c("col", 1:11), col_types = "text")
tbl_12_dinaric_alps <- tibble(
  region = tbl_in[1,] %>% pull(col1),
  creator_expert = tbl_in[2,] %>% pull(col2),
)

all_sheets_manual[12] <- "Dinaric Alps"


# 13 southern andes argentina ---------------------------------------------

tbl_in <- read_xlsx(fn_in, "Southern Andes Argentina", 
                    range = "F1:P2", col_names = str_c("col", 1:11), col_types = "text")
tbl_13_southern_andes_argentina <- tibble(
  region = tbl_in[1,] %>% pull(col1) %>% str_to_title(),
  creator_expert = tbl_in[2,] %>% pull(col2) %>% str_to_title(),
)

all_sheets_manual[13] <- "Southern Andes Argentina"


# 14 western carpathians (slovakia) ---------------------------------------

tbl_in <- read_xlsx(fn_in, "Western Carpathians (Slovakia)", 
                    range = "F1:P2", col_names = str_c("col", 1:11), col_types = "text")
tbl_14_slovakia <- tibble(
  region = tbl_in[1,] %>% pull(col1),
  creator_expert = tbl_in[2,] %>% pull(col2),
)

all_sheets_manual[14] <- "Western Carpathians (Slovakia)"


# 15 atlas ----------------------------------------------------------------

tbl_in <- read_xlsx(fn_in, "Atlas", 
                    range = "F1:P2", col_names = str_c("col", 1:11), col_types = "text")
tbl_15_atlas <- tibble(
  region = tbl_in[1,] %>% pull(col1),
  creator_expert = "Lahoucine Hancih",
)

all_sheets_manual[15] <- "Atlas"

# 16 Mt Lebanon -----------------------------------------------------------

tbl_in <- read_xlsx(fn_in, "Mt Lebanon", 
                    range = "F1:P2", col_names = str_c("col", 1:11), col_types = "text")
tbl_16_mt_lebanon <- tibble(
  region = tbl_in[1,] %>% pull(col1),
  creator_expert = tbl_in[2,] %>% pull(col1),
)

all_sheets_manual[16] <- "Mt Lebanon"


# 17 spain ----------------------------------------------------------------

tbl_in <- read_xlsx(fn_in, "Spain", 
                    range = "F1:P2", col_names = str_c("col", 1:11), col_types = "text")
tbl_17_spain <- tibble(
  region = tbl_in[1,] %>% pull(col1),
  creator_expert = tbl_in[2,] %>% pull(col2),
)

all_sheets_manual[17] <- "Spain"

# 18 Greenland -----------------------------------------------------------

tbl_in <- read_xlsx(fn_in, "Greenland", 
                    range = "F1:P2", col_names = str_c("col", 1:11), col_types = "text")
tbl_18_greenland <- tibble(
  region = "Greenland",
  creator_expert = "Jorrit van der Schot",
)

all_sheets_manual[18] <- "Greenland"

# 19 Japanese mountains -----------------------------------------------------------

tbl_in <- read_xlsx(fn_in, "Japanese mountains", 
                    range = "F1:P2", col_names = str_c("col", 1:11), col_types = "text")
tbl_19_japanese_mountains <- tibble(
  region = "Japan",
  creator_expert = tbl_in[2,] %>% pull(col3),
)

all_sheets_manual[19] <- "Japanese mountains"

# 20 Russia ---------------------------------------------------------------

tbl_in <- read_xlsx(fn_in, "Russia", 
                    range = "F1:P2", col_names = str_c("col", 1:11), col_types = "text")
tbl_20_russia <- tibble(
  region = "Russia",
  creator_expert = "Colleen Mortimer",
)

all_sheets_manual[20] <- "Russia"

# 21 US_Northeast ---------------------------------------------------------

tbl_in <- read_xlsx(fn_in, "US_Northeast", 
                    range = "F1:P2", col_names = str_c("col", 1:11), col_types = "text")
tbl_21_us_ne <- tibble(
  region = "Northeast United States",
  creator_expert = "Colleen Mortimer",
)

all_sheets_manual[21] <- "US_Northeast"

# 22 US_NRCS --------------------------------------------------------------


tbl_in <- read_xlsx(fn_in, "US_NRCS", 
                    range = "F1:P2", col_names = str_c("col", 1:11), col_types = "text")
tbl_22_us_nrcs <- tibble(
  region = "Western US and Alaska",
  creator_expert = "Colleen Mortimer",
)

all_sheets_manual[22] <- "US_NRCS"


# 23 US_SNOTEL --------------------------------------------------------------


tbl_in <- read_xlsx(fn_in, "US_SNOTEL", 
                    range = "F1:P2", col_names = str_c("col", 1:11), col_types = "text")
tbl_23_us_snotel <- tibble(
  region = "Western US and Alaska",
  creator_expert = "Colleen Mortimer",
)

all_sheets_manual[23] <- "US_SNOTEL"


# 24 China --------------------------------------------------------------

tbl_in <- read_xlsx(fn_in, "China", 
                    range = "F1:P2", col_names = str_c("col", 1:11), col_types = "text")
tbl_24_china <- tibble(
  region = tbl_in[1,] %>% pull(col1),
  creator_expert = tbl_in[2,] %>% pull(col2),
)


all_sheets_manual[24] <- "China"


# 25 Romania --------------------------------------------------------------

tbl_in <- read_xlsx(fn_in, "Romania", 
                    range = "F1:P2", col_names = str_c("col", 1:11), col_types = "text")
tbl_25_romania <- tibble(
  region = tbl_in[1,] %>% pull(col1),
  creator_expert = tbl_in[2,] %>% pull(col2),
)


all_sheets_manual[25] <- "Romania"

# 26 Sierra Nevada (US) --------------------------------------------------------------


tbl_in <- read_xlsx(fn_in, "Sierra Nevada (US)", 
                    range = "F1:P2", col_names = str_c("col", 1:11), col_types = "text")
tbl_26_sierra_nevada <- tibble(
  region = tbl_in[1,] %>% pull(col1),
  creator_expert = tbl_in[2,] %>% pull(col2),
)


all_sheets_manual[26] <- "Sierra Nevada (US)"


# 27 Central Asia (Uzbekistan) --------------------------------------------------------------

tbl_in <- read_xlsx(fn_in, "Central Asia (Uzbekistan)", 
                    range = "F1:P2", col_names = str_c("col", 1:11), col_types = "text")
tbl_27_casia_uzbekistan <- tibble(
  region = tbl_in[1,] %>% pull(col1),
  creator_expert = tbl_in[2,] %>% pull(col2),
)


all_sheets_manual[27] <- "Central Asia (Uzbekistan)"


# 28 Central Asia (Kazakhstan) --------------------------------------------------------------


tbl_in <- read_xlsx(fn_in, "Central Asia (Kazakhstan)", 
                    range = "F1:P2", col_names = str_c("col", 1:11), col_types = "text")
tbl_28_casia_kazakhstan <- tibble(
  region = tbl_in[1,] %>% pull(col1),
  creator_expert = tbl_in[2,] %>% pull(col2),
)

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
stopifnot(any(!is.na(tbl_all)))


saveRDS(tbl_all, "data/inventory-02-header.rds")




