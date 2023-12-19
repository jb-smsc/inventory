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

# manual read in ----------------------------------------------------------


# 1 hkh -------------------------------------------------------------------


tbl_in <- read_xlsx(fn_in, all_sheets[1], 
                    range = "F1:P2", col_names = str_c("col", 1:11), col_types = "text")
tbl_01_hkh <- tibble(
  region = tbl_in[1,] %>% pull(col1),
  creator_expert = tbl_in[2,] %>% 
    mutate(ff = str_c(col2, col3, col4, sep = ", ")) %>% 
    pull(ff)
)

# 2 rofental -------------------------------------------------------------------


tbl_in <- read_xlsx(fn_in, all_sheets[2], 
                    range = "F1:P2", col_names = str_c("col", 1:11), col_types = "text")
tbl_02_rofental <- tibble(
  region = tbl_in[1,] %>% pull(col1),
  creator_expert = tbl_in[2,] %>% pull(col2),
)

# 3 Geosphere Austria -------------------------------------------------------------

tbl_in <- read_xlsx(fn_in, all_sheets[3], 
                    range = "F1:P2", col_names = str_c("col", 1:11), col_types = "text")
tbl_03_geosphere <- tibble(
  region = "Austria",
  creator_expert = "Bernhard Hynek (Geosphere)",
)




# 4 swiss alps ------------------------------------------------------------

tbl_in <- read_xlsx(fn_in, all_sheets[4], 
                    range = "F1:P2", col_names = str_c("col", 1:11), col_types = "text")
tbl_04_swiss <- tibble(
  region = tbl_in[1,] %>% pull(col1),
  creator_expert = tbl_in[2,] %>% pull(col1),
)

# 5 european alps tc2021 --------------------------------------------------


tbl_in <- read_xlsx(fn_in, all_sheets[5], 
                    range = "F1:P2", col_names = str_c("col", 1:11), col_types = "text")
tbl_05_alps_tc2021 <- tibble(
  region = tbl_in[1,] %>% pull(col1),
  creator_expert = tbl_in[2,] %>% pull(col2),
)


# 6 french alps -----------------------------------------------------------

tbl_in <- read_xlsx(fn_in, all_sheets[6], 
                    range = "F1:P2", col_names = str_c("col", 1:11), col_types = "text")
tbl_06_french_alps <- tibble(
  region = "French Alps (in addition to TC)",
  creator_expert = tbl_in[2,] %>% pull(col3),
)


# 7 Canada ----------------------------------------------------------------

tbl_in <- read_xlsx(fn_in, all_sheets[7], 
                    range = "F1:P2", col_names = str_c("col", 1:11), col_types = "text")
tbl_07_canada <- tibble(
  region = tbl_in[1,] %>% pull(col1),
  creator_expert = tbl_in[2,] %>% pull(col2),
)


# 8 chilean andes ---------------------------------------------------------

tbl_in <- read_xlsx(fn_in, all_sheets[8], 
                    range = "F1:P2", col_names = str_c("col", 1:11), col_types = "text")
tbl_08_chilean_andes <- tibble(
  region = tbl_in[1,] %>% pull(col1),
  creator_expert = "Simone Schauwecker",
)


# 9 australian alps -------------------------------------------------------

tbl_in <- read_xlsx(fn_in, all_sheets[9], 
                    range = "F1:P2", col_names = str_c("col", 1:11), col_types = "text")
tbl_09_australian_alps <- tibble(
  region = tbl_in[1,] %>% pull(col1),
  creator_expert = tbl_in[2,] %>% pull(col2),
)

# 10 pyrenees -------------------------------------------------------------

tbl_in <- read_xlsx(fn_in, all_sheets[10], 
                    range = "F1:P2", col_names = str_c("col", 1:11), col_types = "text")
tbl_10_pyrenees <- tibble(
  region = "Pyrenees",
  creator_expert = tbl_in[2,] %>% pull(col3),
)

# 11 corsica --------------------------------------------------------------


tbl_in <- read_xlsx(fn_in, all_sheets[11], 
                    range = "F1:P2", col_names = str_c("col", 1:11), col_types = "text")
tbl_11_corsica <- tibble(
  region = "Corsica",
  creator_expert = tbl_in[2,] %>% pull(col3),
)


# 12 Dinaric Alps ---------------------------------------------------------

tbl_in <- read_xlsx(fn_in, all_sheets[12], 
                    range = "F1:P2", col_names = str_c("col", 1:11), col_types = "text")
tbl_12_dinaric_alps <- tibble(
  region = tbl_in[1,] %>% pull(col1),
  creator_expert = tbl_in[2,] %>% pull(col2),
)


# 13 southern andes argentina ---------------------------------------------

tbl_in <- read_xlsx(fn_in, all_sheets[13], 
                    range = "F1:P2", col_names = str_c("col", 1:11), col_types = "text")
tbl_13_southern_andes_argentina <- tibble(
  region = tbl_in[1,] %>% pull(col1) %>% str_to_title(),
  creator_expert = tbl_in[2,] %>% pull(col2) %>% str_to_title(),
)




# 14 western carpathians (slovakia) ---------------------------------------

tbl_in <- read_xlsx(fn_in, all_sheets[14], 
                    range = "F1:P2", col_names = str_c("col", 1:11), col_types = "text")
tbl_14_slovakia <- tibble(
  region = tbl_in[1,] %>% pull(col1),
  creator_expert = tbl_in[2,] %>% pull(col2),
)


# 15 atlas ----------------------------------------------------------------

tbl_in <- read_xlsx(fn_in, all_sheets[15], 
                    range = "F1:P2", col_names = str_c("col", 1:11), col_types = "text")
tbl_15_atlas <- tibble(
  region = tbl_in[1,] %>% pull(col1),
  creator_expert = "Lahoucine Hancih",
)


# 16 Mt Lebanon -----------------------------------------------------------

tbl_in <- read_xlsx(fn_in, all_sheets[16], 
                    range = "F1:P2", col_names = str_c("col", 1:11), col_types = "text")
tbl_16_mt_lebanon <- tibble(
  region = tbl_in[1,] %>% pull(col1),
  creator_expert = tbl_in[2,] %>% pull(col1),
)


# 17 spain ----------------------------------------------------------------

tbl_in <- read_xlsx(fn_in, all_sheets[17], 
                    range = "F1:P2", col_names = str_c("col", 1:11), col_types = "text")
tbl_17_spain <- tibble(
  region = tbl_in[1,] %>% pull(col1),
  creator_expert = tbl_in[2,] %>% pull(col2),
)


# 18 Greenland -----------------------------------------------------------

tbl_in <- read_xlsx(fn_in, all_sheets[18], 
                    range = "F1:P2", col_names = str_c("col", 1:11), col_types = "text")
tbl_18_greenland <- tibble(
  region = "Greenland",
  creator_expert = "Jorrit van der Schot",
)

# 19 Japanese mountains -----------------------------------------------------------

tbl_in <- read_xlsx(fn_in, all_sheets[19], 
                    range = "F1:P2", col_names = str_c("col", 1:11), col_types = "text")
tbl_19_japanese_mountains <- tibble(
  region = "Japan",
  creator_expert = tbl_in[2,] %>% pull(col3),
)


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
stopifnot(any(!is.na(tbl_all)))


saveRDS(tbl_all, "data/inventory-02-header.rds")




