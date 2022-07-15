

library(tidyverse)
library(here)
library(reshape)


lpi <- read.csv(here("data/CGRC_LPI_20220714.csv")) %>% 
  add.pointyear()

transect = distinct(lpi, Transect.Name)
