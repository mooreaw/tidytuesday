library(tidyverse)
library(scico)

tt <- tidytuesdayR::tt_load('2020-08-25')

chopped <- pull(tt$chopped, 1) %>%
  str_c(collapse = "\n") %>%
  read_tsv(col_names = names(tt$chopped) %>% str_split("\t") %>% unlist())
