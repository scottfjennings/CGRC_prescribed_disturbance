


library(tidyverse)
library(lubridate)
library(ggrepel)
library(here)
library(birdnames)

source(here("code/bird_functions.R"))


cgrc_birds <- read.csv(here("data/CGRC_AS.csv")) %>% 
  filter(Date != "") %>% 
  mutate(Date = as.Date(Date))


birds_area_date <- cgrc_birds %>% 
  birds_per_area_date()


birds_area_year <- cgrc_birds %>% 
  birds_per_area_year() %>% 
  group_by(Sampling.Unit.Name, year) %>% 
  summarise(total.birds = sum(mean.birds)) %>% 
  ungroup() %>% 
  mutate(treatment = ifelse(Sampling.Unit.Name == "XCGRC-02", "Grazed", "No treatment"))


species_area_date <- cgrc_birds %>% 
  species_per_area_date()


species_area_year <- cgrc_birds %>% 
  species_per_area_year()



birds_area_year %>% 
  ggplot() +
  #  geom_boxplot(aes(x = factor(year), y = mean.species, color = treatment)) +
  geom_line(stat="smooth", aes(x = year, y = total.birds, group = Sampling.Unit.Name, color = treatment), alpha = 0.5, method = "lm") +
  geom_point(aes(x = year, y = total.birds, color = treatment), size = 2) +
  stat_smooth(aes(x = year, y = total.birds, color = treatment), size = 2, method = "lm", se = FALSE) +
  expand_limits(y = 0) +
  geom_text_repel(data = filter(birds_area_year, 
                                year == 2022), aes(label = Sampling.Unit.Name, x = 2022, y = total.birds), nudge_x = -.2, size = 2.5) +
  scale_x_continuous(breaks = seq(min(birds_area_year$year), max(birds_area_year$year))) +
  theme_bw() +
  labs(color = NULL,
       x = "",
       y = "Mean # birds detected per survey",
       title = "Number of individual birds detected at each plot") +
  theme(legend.position=c(.5,.25))




species_area_year %>% 
  mutate(treatment = ifelse(Sampling.Unit.Name == "XCGRC-02", "Grazed", "Mean of no treatment")) %>% 
  ggplot() +
#  geom_boxplot(aes(x = factor(year), y = mean.species, color = treatment)) +
  stat_smooth(aes(x = year, y = mean.species, group = Sampling.Unit.Name), color = "gray75", method = "lm") +
  geom_point(aes(x = year, y = mean.species, color = treatment), size = 2) +
  stat_smooth(aes(x = year, y = mean.species, color = treatment), size = 2, method = "lm", se = FALSE) +
  expand_limits(y = 0) +
  geom_text_repel(data = filter(species_area_year, 
                                year == 2022), aes(label = Sampling.Unit.Name, x = 2022, y = mean.species), nudge_x = -.2, size = 2.5) +
  scale_x_continuous(breaks = seq(min(species_area_year$year), max(species_area_year$year))) +
  theme_bw() +
  labs(color = NULL,
       x = "",
       y = "Mean # species detected per survey",
       title = "Number of bird species detected at each plot") +
  theme(legend.position=c(.5,.25))

