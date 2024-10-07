



library(tidyverse)
library(stringi)
library(here)


source(here("code/general_functions.R"))
source(here("code/veg_functions.R"))
options(scipen = 999)



# read and clean veg data ----
lpi <- read.csv(here("data/CGRC_LPI.csv")) %>% 
  clean_date_year() %>% # in general_functions
  filter(!is.na(Direction)) %>% 
  split_point_id() # in general_functions

points_years_treatments <- readRDS(here("data/derived/points_years_treatments")) %>% 
  rename(year = treatment.effect.year) %>% 
  select(point, year, treatment) %>% 
  right_join(lpi %>% distinct(point, year))

# make long format
long_lpi <- lengthen_lpi(lpi) # in veg_functions

# assign functional group, filter out non plants, cleaning
long_lpi_assigned <- assign_functional_group_status(long_lpi) # in veg_functions



saveRDS(long_lpi_assigned, here("data/derived/long_lpi_assigned"))

# which points have multiple years of plant data
multi_year_points <- long_lpi_assigned %>% 
  distinct(year, point) %>% 
  count(point) %>% 
  filter(n > 1) %>% 
  select(point)

fun_groups = long_lpi_assigned %>% 
  distinct(FunGrp) %>% 
  select(FunGrp) %>% 
  filter(!str_detect(FunGrp, "\\*"))


# need this to expand the df out and fill 0s
points_years_groups <- points_years_treatments %>% 
  merge(fun_groups) %>% 
  merge(data.frame(nat.nnat.inv = c("Non-native", "Invasive", "Native")))


# calculate percent cover ----
percent_cover <- long_lpi_assigned %>% 
  filter(!is.na(FunGrp), !is.na(nat.nnat.inv)) %>% 
  get_fungrp_native_cover() %>% # in veg_functions
  full_join(points_years_groups) %>% # join with this to fill 0s 
  mutate(abs.cover = ifelse(is.na(abs.cover), 0, abs.cover),
         rel.cover = ifelse(is.na(rel.cover), 0, rel.cover)) %>% 
  pivot_longer(cols = contains("cover"), names_to = "cover.type", values_to = "cover") %>% # this pivot should double the # of rows
  group_by(year, FunGrp, nat.nnat.inv, cover.type, treatment) %>% 
  mutate(mean.all.points.annual.cov = mean(cover),
         sd.all.points.annual.cov = sd(cover)) %>% 
  ungroup()  

saveRDS(percent_cover, here("data/derived/percent_cover"))

# calculate species richness ----
richness <- long_lpi_assigned %>% 
  filter(!is.na(FunGrp), !is.na(nat.nnat.inv)) %>% 
  drop_non_plants() %>%  # in veg_functions. want to remove things not ID to species for calculating species richness
  get_fungrp_native_richness() %>%
  full_join(points_years_groups) %>% 
  mutate(richness = replace_na(richness, 0)) %>% 
  left_join(points_years_treatments %>% select(year, point, treatment)) %>% 
  right_join(multi_year_points) %>% 
  group_by(year, FunGrp, nat.nnat.inv, treatment) %>% 
  mutate(mean.richness = mean(richness),
         sd.richness = sd(richness)) %>% 
  ungroup()


saveRDS(richness, here("data/derived/richness"))
