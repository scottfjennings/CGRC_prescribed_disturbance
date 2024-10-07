
library(tidyverse)
library(stringi)
library(here)


source(here("code/general_functions.R"))
source(here("code/veg_functions.R"))
options(scipen = 999)


# 1 set up df with plots and treatments across years ----
treatments <- read.csv(here("data/treatment_history.csv"))  %>% 
  rename(point = plot) %>% 
  mutate(treatment.effect.year = ifelse(before.after.monitoring == "before", treatment.year, treatment.year + 1))

first_year_tx_response <- treatments %>%
  select(point, treatment.year, before.after.monitoring) %>% 
  group_by(point) %>% 
  filter(treatment.year == min(treatment.year)) %>% 
  mutate(first.year.tx.response = ifelse(before.after.monitoring == "before", treatment.year, treatment.year + 1)) %>% 
  select(point, first.year.tx.response)

# make expanded df with all FunGrp and nat.nnat.inv for each point and year that we have plant data for, including treatment history 
points_years <- expand.grid(point = paste("CGRC-", str_pad(seq(1, 10), 2, pad = "0"), sep = ""),
                            treatment.effect.year = seq(2022, year(Sys.time())))


points_years_treatments <- treatments %>% 
  #full_join(first_year_tx_response) %>% 
  full_join(points_years) %>% 
  mutate(treatment = case_when(is.na(treatment) & point %in% treatments$point ~ "before treatment",
                               is.na(treatment) & !point %in% treatments$point ~ "none",
                               TRUE ~ treatment)
         #treatment = replace_na(treatment, "none"),
         #before.after.treatment = case_when(treatment != "none" & year < first.year.tx.response ~ "before treatment",
          #                                  treatment != "none" & year >= first.year.tx.response ~ treatment,
          #                                  treatment == "none" ~ "no treatment")
         ) %>% 
  arrange(point, treatment.effect.year)

saveRDS(points_years_treatments, here("data/derived/points_years_treatments"))
