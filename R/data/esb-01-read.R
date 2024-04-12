# read European Snow booklet metadata

library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(stringr)


fn_in <- "data-raw/ESB_metadata.xlsx"

all_sheets <- excel_sheets(fn_in)


tbl_hs <- read_xlsx(fn_in, all_sheets[1], range = cell_cols("C:K"), na = c("", "NAN"))
tbl_hn <- read_xlsx(fn_in, all_sheets[2], range = cell_cols("C:K"), na = c("", "NAN"))
tbl_swe <- read_xlsx(fn_in, all_sheets[3], range = cell_cols("C:K"), na = c("", "NAN"))

l_all <- list(tbl_hs, tbl_hn, tbl_swe)
names(l_all) <- all_sheets[1:3]

tbl_all <- bind_rows(l_all, .id = "variable")




# clean -------------------------------------------------------------------


tbl_all <- tbl_all %>% filter(!is.na(AUTOMATIC))

tbl_all %>% filter(is.na(`ELEVATION A.S.L. (m)`))
tbl_all %>% filter(`ELEVATION A.S.L. (m)` < 0) -> zz
tbl_all$`ELEVATION A.S.L. (m)` %>% hist
tbl_all$`ELEVATION A.S.L. (m)`[tbl_all$`ELEVATION A.S.L. (m)` < -100] <- NA

tbl_all %>% filter(is.na(`LATITUDE (WGS 1984)`))
tbl_all %>% filter(is.na(`LONGITUDE (WGS 1984)`))

tbl_all$AUTOMATIC %>% table
tbl_all$MANUAL %>% table
with(tbl_all, table(AUTOMATIC, MANUAL, useNA = "a"))

tbl_all$`INTERVAL (h)` %>% table

saveRDS(tbl_all, file = "data/esb-01-read.rds")

