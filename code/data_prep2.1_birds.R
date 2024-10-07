



library(tidyverse)
library(lubridate)
library(here)
library(birdnames)

source(here("code/bird_functions.R"))

points_years_treatments <- readRDS(here("data/derived/points_years_treatments")) %>% 
  rename(year = treatment.effect.year)


cgrc_birds <- read.csv(here("data/CGRC_AS.csv")) %>% 
  clean_area_search() %>% 
  bird_taxa_filter(keep_taxa = c("Passeriformes", "Apodiformes", "Cathartiformes", "Galliformes", "Piciformes")) %>% 
  select(point, visit, date, start.time, end.time, count, alpha.code, breeding.evidence) %>% 
  mutate(year = year(date)) 

saveRDS(cgrc_birds, here("data/derived/cgrc_birds"))

birds_area_year <- cgrc_birds %>% 
  birds_per_area_year() %>% 
  group_by(point, year) %>% 
  summarise(total.birds = sum(mean.birds)) %>% 
  ungroup() %>% 
  full_join(points_years_treatments)

saveRDS(birds_area_year, here("data/derived/birds_area_year"))


species_area_year <- cgrc_birds %>% 
  species_per_area_year() %>% 
  ungroup() %>% 
  full_join(points_years_treatments) 

saveRDS(species_area_year, here("data/derived/species_area_year"))
