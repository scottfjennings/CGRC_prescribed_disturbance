

library(tidyverse)
library(here)
library(cowplot)
library(reshape)

source(here("code/general_functions.R"))
source(here("code/veg_functions.R"))
options(scipen = 999)
surveyyear = 2023

lpi <- read.csv(here("data/CGRC_LPI.csv")) %>% 
  add.pointyear()


# percent cover ----
veg_cover <- lpi %>% 
  get_fungrp_native_cover() %>% 
  split_point_id()

# separate on Point.Id from first answer here: https://stackoverflow.com/questions/9756360/split-character-data-into-numbers-and-letters
# esp this comment: "?<= is "look behind" : here it basically matches any uppercase or lowercase letter ([A-Za-z]) which is "before the cursor". And ?= is "look ahead" : it matches any number ([0-9]) "after the cursor". None of these two "moves the cursor" so put together they match the "in between" the letter and numbers, ie where we want to split."


veg_cover %>% 
  group_by(Point.Id, year) %>% 
  summarise(ztest = sum(rel.cover)) %>% 
  view()





plot_veg_mean_cov <- veg_cover %>% 
  full_join(expand.grid(year = distinct(veg_cover, year)$year,
                        point = distinct(veg_cover, point)$point,
                        line = distinct(veg_cover, line)$line,
                        FunGrp = distinct(veg_cover, FunGrp)$FunGrp,
                        nat.nnat.inv = distinct(veg_cover, nat.nnat.inv)$nat.nnat.inv)) %>% 
  mutate(abs.cover = ifelse(is.na(abs.cover), 0, abs.cover),
         rel.cover = ifelse(is.na(rel.cover), 0, rel.cover)) %>% 
  pivot_longer(cols = contains("cover"), names_to = "cover.type", values_to = "cover") %>% 
  group_by(year, point, FunGrp, nat.nnat.inv, cover.type) %>% 
  summarise(n.line = n(),
            mean.cov = mean(cover),
            sd.cover = sd(cover)) %>% 
  ungroup() %>% 
  mutate(se.cover = sd.cover/sqrt(n.line),
         lwr.se = mean.cov - se.cover,
         upr.se = mean.cov + se.cover)

# plotting % cover ----
# plot woody plant cover

max.y.wood = plot_veg_mean_cov %>% 
  filter(cover.type == "abs.cover", FunGrp == "Shrub", nat.nnat.inv == "Native") %>% 
  filter(upr.se == max(upr.se)) %>% 
  select(upr.se)

plot_veg_mean_cov %>% 
  filter(cover.type == "abs.cover", FunGrp == "Shrub", nat.nnat.inv == "Native") %>% 
  ggplot() +
  geom_point(aes(x = point, y = mean.cov)) +
  geom_errorbar(aes(x = point, ymin = lwr.se, ymax = upr.se)) +
  geom_hline(yintercept = 15, color = "red") +
  scale_y_continuous(breaks = seq(0, max.y.wood$upr.se, by = 20), labels = seq(0, max.y.wood$upr.se, by = 20)) +
  coord_flip() +
  labs(x = "Management unit",
       y = "Mean woody plant % cover") +
  theme_bw()

ggsave(here("figures/woody_cover.png"), width = 8, height = 6)

# plot grass and forb cover

max.y.grassforb = plot_veg_mean_cov %>% 
  filter(cover.type == "abs.cover", grepl("Forb", FunGrp) | grepl("Grass", FunGrp)) %>% 
  filter(upr.se == max(upr.se)) %>% 
  select(upr.se)

plot_veg_mean_cov %>% 
  filter(cover.type == "abs.cover", grepl("Forb", FunGrp) | grepl("Grass", FunGrp)) %>% 
  mutate(FunGrp = gsub(" ", "\n", FunGrp),
         #manag.obj = ifelse(nat.nnat.inv == "Native", (mean.cov + 1) * 1.1, NA)
         manag.obj = ifelse(nat.nnat.inv == "Native", ifelse(point %in% c("CGRC-07", "CGRC-08"), 50, 10), NA) 
         ) %>% 
  ggplot() +
  geom_point(aes(x = FunGrp, y = mean.cov, color = nat.nnat.inv), position=position_dodge(width=0.5)) +
  #geom_point(aes(x = FunGrp, y = manag.obj, color = nat.nnat.inv), position=position_dodge(width=0.5), shape = 8, show.legend=FALSE) +
  geom_errorbar(aes(x = FunGrp, ymin = lwr.se, ymax = upr.se, color = nat.nnat.inv), position=position_dodge(width=0.5)) +
  labs(x = "Vegetation type",
       y = "Mean grass and forb % cover",
       shape = "",
       color = "") +
  theme_bw() +
  facet_wrap(~point, scales = "free_y")

ggsave(here("figures/grass_forb_cover.png"), width = 8.5, height = 6)


# plant species richness ----

richness <- get_fungrp_native_richness(lpi)

# plotting richness ----
# forbs and grasses

richness %>% 
  filter(grepl("Forb", FunGrp) | grepl("Grass", FunGrp)) %>% 
  mutate(FunGrp = gsub(" ", "\n", FunGrp),
         manag.obj = ifelse(nat.nnat.inv == "Native", richness + 2, NA)) %>%
  ggplot() +
  geom_point(aes(x = FunGrp, y = richness, color = nat.nnat.inv), position=position_dodge(width=0.5)) +
  #geom_point(aes(x = FunGrp, y = manag.obj, color = nat.nnat.inv), position=position_dodge(width=0.5), shape = 8, show.legend=FALSE) +
  ylim(0, 8) +
  labs(x = "Vegetation type",
       y = "Mean grass and forb species richness",
       shape = "",
       color = "") +
  theme_bw() +
  facet_wrap(~point)

ggsave(here("figures/grass_forb_richness.png"), width = 8.5, height = 6)


# dominant species at each point


spp_cover <- lpi %>% 
  get_species_cover() %>% 
  split_point_id()


plot_fungrp_spec_cover <- function(zfungrp, zpoint = NA) {

  if(!is.na(zpoint)) {
spp_cover %>% 
  filter(FunGrp == zfungrp, point == zpoint) %>% 
  group_by(year, point, USDA.code) %>% 
  summarise(mean.cov = mean(abs.cover)) %>%
  left_join(distinct(spp_cover, USDA.code, Common.Name, Scientific.Name, FunGrp, nat.nnat.inv)) %>% 
  ungroup() %>% 
  ggplot(aes(y = mean.cov, x = nat.nnat.inv, fill = Common.Name))  + 
    geom_bar(position="stack", stat="identity") +
  labs(x = "",
       y = "% cover",
       fill = "Common name",
       title = zfungrp)+
    theme_bw()
  } else {
spp_cover %>% 
  filter(FunGrp == zfungrp) %>% 
  group_by(year, point, USDA.code) %>% 
  summarise(mean.cov = mean(abs.cover)) %>%
  left_join(distinct(spp_cover, USDA.code, Common.Name, Scientific.Name, FunGrp, nat.nnat.inv)) %>% 
  ungroup() %>% 
  ggplot(aes(y = mean.cov, x = nat.nnat.inv, fill = Common.Name))  + 
    geom_bar(position="stack", stat="identity") +
  labs(x = "",
       y = "% cover",
       fill = "Common name",
       title = zfungrp)+
    theme_bw() +
      facet_wrap(~point)
  }
  
}

plot_fungrp_spec_cover("Annual Grass")
ggsave(here("figures/annual_grass_spp_cov.png"), width = 8.5, height = 6)

plot_fungrp_spec_cover("Annual Forb")
ggsave(here("figures/annual_forb_spp_cov.png"), width = 8.5, height = 6)


plot_fungrp_spec_cover("Perennial Grass")
ggsave(here("figures/perennial_grass_spp_cov.png"), width = 8.5, height = 6)

plot_fungrp_spec_cover("Perennial Forb")
ggsave(here("figures/perennial_forb_spp_cov.png"), width = 8.5, height = 6)

plot_fungrp_spec_cover("Shrub")
ggsave(here("figures/annual_forb_spp_cov.png"), width = 8.5, height = 6)


  


by_point_herb_cover <- function(zpoint) {
  
  title <- ggdraw() + 
  draw_label(
    zpoint,
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )
out_plot <- plot_grid(title,
plot_grid(plot_fungrp_spec_cover("Annual Grass", zpoint),
          plot_fungrp_spec_cover("Annual Forb", zpoint),
          plot_fungrp_spec_cover("Perennial Grass", zpoint),
          plot_fungrp_spec_cover("Perennial Forb", zpoint)), 
ncol = 1, rel_heights = c(0.1, 0.9)
)
ggsave(here(paste("figures/", zpoint, "_herb_cov.png", sep = "")), width = 8.5, height = 6)

}
by_point_herb_cover("CGRC-02")


map(distinct(spp_cover, point)$point, by_point_herb_cover)

