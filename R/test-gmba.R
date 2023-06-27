# test gmbaR

library(magrittr)
library(gmbaR)
gmba_read()

gmba_ipbes_maj_str %>% str
gmba_ipcc_ar6_maj_str %>% str

sf_inv <- gmba_inv()
sf_inv

