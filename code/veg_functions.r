
# the code here is adapted for ACR use from the Point Blue Wrangler program
# wrangler code accessed July 2022

#veg code

get_fungrp_native_cover = function(lpi){

  CAPlants = read.csv(here("data/helper_files/CAPlantsv2.csv"))
  # Invazivesv1.csv is all plants in CAPlantsv2 with Cal-IPC in the Invasive field
  #Invasives = read.csv(here("data/helper_files/Invasivesv1.csv"))
  
   non_spp = c("", "2FA", "2FORB", "2FP", "2GA", "2GP", "2LICHN",
              "2LTR", "2LTRWS",  "2PLANT",  "2W", "NOPLANT", "L", "WL")

  # need number of hits per line to calculate relative cover
  a <- lpi %>% 
    group_by(Point.Id, year) %>% 
    summarise(NumIndices = n())
  
  #lpi.trim = lpi %>% 
   # select(Transect.Name, pointyear, year, Point.Id, Canopy1,
    #       Canopy2, Canopy3, Top.Layer, Lower1, Lower2, Lower3,
     #      Lower4, Lower5, Lower6, Lower7, Lower8, Lower9, Lower10, Soil.Surface)
  
  longlpi = lpi %>% 
    pivot_longer(cols = c(Canopy1,
           Canopy2, Canopy3, Top.Layer, Lower1, Lower2, Lower3,
           Lower4, Lower5, Lower6, Lower7, Lower8, Lower9, Lower10, Soil.Surface),
           names_to = "layer",
           values_to = "USDA.code") %>% 
    filter(!is.na(USDA.code), USDA.code != "", !USDA.code %in% non_spp) %>% 
    left_join(CAPlants %>% dplyr::select('USDA.code' = Symbol, Scientific.Name, Common.Name, Native.Status, Invasive, FunGrp1, FunGrp2, FunGrp, Growth.Habit)) %>% 
    mutate(nat.nnat.inv = case_when(grepl("L48 \\(N\\)", Native.Status) ~ "Native",
                                    grepl("L48 \\(I\\)", Native.Status) & !grepl("Cal-IPC", Invasive)~ "Non-native",
                                    grepl("L48 \\(I\\)", Native.Status) & grepl("Cal-IPC", Invasive) ~ "Invasive"),
           FunGrp = case_when(FunGrp == "ShrubsTrees" & grepl("hrub", Growth.Habit) ~ "Shrub",
                              FunGrp == "ShrubsTrees" & !grepl("hrub", Growth.Habit) ~ "Tree",
                              TRUE ~ as.character(FunGrp)))
  
  
  Fun.Sum <- longlpi %>% 
    filter(!is.na(nat.nnat.inv)) %>% 
    group_by(year, Point.Id, FunGrp, nat.nnat.inv) %>% 
    summarise(Count = n()) %>%
    ungroup() %>% 
    group_by(Point.Id, year) %>% 
    mutate(group.hits = sum(Count)) %>% 
    ungroup() %>% 
    full_join(a) %>% 
    mutate(abs.cover = (Count/NumIndices) * 100,
           rel.cover = (Count/group.hits) * 100)
  
  
}




get_species_cover = function(lpi){

  CAPlants = read.csv(here("data/helper_files/CAPlantsv2.csv"))
  # Invazivesv1.csv is all plants in CAPlantsv2 with Cal-IPC in the Invasive field
  #Invasives = read.csv(here("data/helper_files/Invasivesv1.csv"))
  
   non_spp = c("", "2FA", "2FORB", "2FP", "2GA", "2GP", "2LICHN",
              "2LTR", "2LTRWS",  "2PLANT",  "2W", "NOPLANT", "L", "WL")
   

  # need number of hits per line to calculate relative cover
  a <- lpi %>% 
    group_by(Point.Id, year) %>% 
    summarise(NumIndices = n())
  
  #lpi.trim = lpi %>% 
   # select(Transect.Name, pointyear, year, Point.Id, Canopy1,
    #       Canopy2, Canopy3, Top.Layer, Lower1, Lower2, Lower3,
     #      Lower4, Lower5, Lower6, Lower7, Lower8, Lower9, Lower10, Soil.Surface)
  
  longlpi = lpi %>% 
    pivot_longer(cols = c(Canopy1, Canopy2, Canopy3, Top.Layer, Lower1, Lower2, Lower3,
           Lower4, Lower5, Lower6, Lower7, Lower8, Lower9, Lower10, Soil.Surface),
           names_to = "layer",
           values_to = "USDA.code") %>% 
    filter(!is.na(USDA.code), USDA.code != "", !USDA.code %in% non_spp) %>% 
    left_join(CAPlants %>% dplyr::select('USDA.code' = Symbol, Scientific.Name, Common.Name, Native.Status, Invasive, FunGrp1, FunGrp2, FunGrp, Growth.Habit)) %>% 
    mutate(nat.nnat.inv = case_when(grepl("L48 \\(N\\)", Native.Status) ~ "Native",
                                    grepl("L48 \\(I\\)", Native.Status) & !grepl("Cal-IPC", Invasive)~ "Non-native",
                                    grepl("L48 \\(I\\)", Native.Status) & grepl("Cal-IPC", Invasive) ~ "Invasive"),
           FunGrp = case_when(FunGrp == "ShrubsTrees" & grepl("hrub", Growth.Habit) ~ "Shrub",
                              FunGrp == "ShrubsTrees" & !grepl("hrub", Growth.Habit) ~ "Tree",
                              TRUE ~ as.character(FunGrp)))
  
  
  Fun.Sum <- longlpi %>% 
    filter(!is.na(nat.nnat.inv)) %>% 
    group_by(Transect.Name, Point.Id, year, USDA.code) %>% 
    summarise(Count = n()) %>%
    ungroup() %>% 
    group_by(Transect.Name, Point.Id, year) %>% 
    mutate(group.hits = sum(Count)) %>% 
    ungroup() %>% 
    full_join(a) %>% 
    mutate(abs.cover = (Count/NumIndices) * 100,
           rel.cover = (Count/group.hits) * 100) %>% 
    left_join(CAPlants %>% select("USDA.code" = Symbol, Common.Name, Scientific.Name)) %>% 
    left_join(longlpi %>% distinct(FunGrp, nat.nnat.inv, USDA.code))
}


get_fungrp_native_richness = function(lpi){

  CAPlants = read.csv(here("data/helper_files/CAPlantsv2.csv"))
  
   non_spp = c("", "2FA", "2FORB", "2FP", "2GA", "2GP", "2LICHN",
              "2LTR", "2LTRWS",  "2PLANT",  "2W", "NOPLANT", "L", "WL")
   

 
  obs_species = lpi %>% 
    pivot_longer(cols = c(Canopy1,
           Canopy2, Canopy3, Top.Layer, Lower1, Lower2, Lower3,
           Lower4, Lower5, Lower6, Lower7, Lower8, Lower9, Lower10, Soil.Surface),
           names_to = "layer",
           values_to = "USDA.code") %>% 
    filter(!is.na(USDA.code), USDA.code != "", !USDA.code %in% non_spp) %>% 
  mutate(Point.Id = gsub("X", "", Point.Id)) %>% 
   separate(Point.Id, 
           into = c("point", "line"), 
           sep = "(?<=[0-9])(?=[A-Za-z])", remove = FALSE) %>% 
    distinct(point, year, USDA.code) %>% 
    left_join(CAPlants %>% dplyr::select('USDA.code' = Symbol, Scientific.Name, Common.Name, Native.Status, Invasive, FunGrp1, FunGrp2, FunGrp)) %>% 
    mutate(nat.nnat.inv = case_when(grepl("L48 \\(N\\)", Native.Status) ~ "Native",
                                    grepl("L48 \\(I\\)", Native.Status) & !grepl("Cal-IPC", Invasive)~ "Non-native",
                                    grepl("L48 \\(I\\)", Native.Status) & grepl("Cal-IPC", Invasive) ~ "Invasive"))

  richness <- obs_species  %>% 
    filter(!is.na(nat.nnat.inv)) %>% 
    group_by(point, year, FunGrp, nat.nnat.inv) %>% 
    summarize(richness = n())
  
  }

