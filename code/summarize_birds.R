


library(tidyverse)
library(lubridate)
library(here)
library(birdnames)


cgrc_birds <- read.csv(here("data/CGRC_AS_20230807.csv")) %>% 
  filter(Date != "") %>% 
  mutate(Date = as.Date(Date))


