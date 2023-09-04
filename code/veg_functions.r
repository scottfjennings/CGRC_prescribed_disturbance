
# the code here is adapted for ACR use from the Point Blue Wrangler program
# wrangler code accessed July 2022


#' lengthen_lpi
#' 
#' convert lpi data from wide to long format
#'
#' @param lpi data frame with wide format lpi data. this will generally be the df downloaded directly from CADC, Must have all columns: Canopy1, Canopy2, Canopy3, Top.Layer, Lower1, Lower2, Lower3, Lower4, Lower5, Lower6, Lower7, Lower8, Lower9, Lower10, Soil.Surface
#'
#' @return long format data frame with all columns listed above pivoted to a single column. ID
#' @export
#'
#' @examples long_lpi <- lengthen_lpi(lpi)
lengthen_lpi <- function(lpi) {
  
long_lpi = lpi %>% 
  pivot_longer(cols = c(Canopy1, Canopy2, Canopy3, Top.Layer, Lower1, Lower2, Lower3, Lower4, Lower5, Lower6, Lower7, Lower8, Lower9, Lower10, Soil.Surface),
               names_to = "layer",
               values_to = "USDA.code") %>% 
  filter(!is.na(USDA.code), USDA.code != "")
}


#' @title drop_non_plants
#' 
#' @description
#' remove non plant records
#' 
#'
#' @param df 
#'
#' @return
#' @export
#' 
#' @details
#' currently removes USDA.code == c(2FA", "2FORB", "2FP", "2GA", "2GP", "2LICHN", "2LTR", "2LTRWS",  "2PLANT",  "2W", "NOPLANT", "L", "WL", "2GRAM", "S")
#' 
#'
#' @examples
drop_non_plants <- function(df) {
  
  non_spp = c("2FA", "2FORB", "2FP", "2GA", "2GP", "2LICHN",
              "2LTR", "2LTRWS",  "2PLANT",  "2W", "NOPLANT", "L", "WL", "2GRAM", "S")
  
  df <- df %>% 
    filter(!USDA.code %in% non_spp)
  
}

#' assign_functional_group
#' 
#' join plant data frame with the CA Plants list and create Functional Group and Status fields
#'
#' @param df data frame with at least the field USDA.Code
#' @param CAPlants_obj the CA Plants list, default is to read it in: read.csv(here("data/helper_files/CAPlantsv2.csv")). Could also specify a different location or an object already in the environment
#'
#' @return
#' @export
#' 
#' @details
#' may also use list of invasive species only:
#' Invazivesv1.csv is all plants in CAPlantsv2 with Cal-IPC in the Invasive field
#' read.csv(here("data/helper_files/Invasivesv1.csv"))
#' 
#'
#' @examples long_lpi_assigned <- assign_functional_group_status(long_lpi)
assign_functional_group_status <- function(df, CAPlants = read.csv(here("data/helper_files/CAPlantsv2.csv"))) {
  df <- df %>% 
  left_join(CAPlants %>% dplyr::select('USDA.code' = Symbol, Scientific.Name, Common.Name, Native.Status, Invasive, FunGrp1, FunGrp2, FunGrp, Growth.Habit)) %>% 
    mutate(nat.nnat.inv = case_when(grepl("L48 \\(N\\)", Native.Status) ~ "Native",
                                    grepl("L48 \\(I\\)", Native.Status) & !grepl("Cal-IPC", Invasive)~ "Non-native",
                                    grepl("L48 \\(I\\)", Native.Status) & grepl("Cal-IPC", Invasive) ~ "Invasive"),
           FunGrp = case_when(FunGrp == "ShrubsTrees" & grepl("hrub", Growth.Habit) ~ "Shrub",
                              FunGrp == "ShrubsTrees" & !grepl("hrub", Growth.Habit) ~ "Tree",
                              TRUE ~ as.character(FunGrp)))
}




#' @title get_fungrp_native_cover
#' 
#' @description
#'  calculate percent cover by functional group and native/non-native status
#'
#' @param df data frame with species assigned to functional group ("FunGrp") and native/non-native status ("nat.nnat.inv"). Must also have a year column (added with clean_date_year()) and a point column (added with split_point_id())
#'
#' @return
#' @export
#' 
#' @details
#' calculates absolute and relative percent cover of each functional group at each point. Calculates relative cover using the total number of hits per point as the denominator; thus this denominator may be many more than the number of pins.
#' 
#' 
#'
#' @examples
get_fungrp_native_cover = function(df){


  a <- df %>% 
    distinct(point, Point.Id, year, Point.Index) %>% 
    group_by(point, year) %>% 
    summarise(NumIndices = n())
  
  Fun.Sum <- df %>% 
    filter(!is.na(nat.nnat.inv)) %>% 
    group_by(year, point, FunGrp, nat.nnat.inv) %>% 
    summarise(Count = n()) %>%
    ungroup() %>% 
    group_by(point, year) %>% 
    mutate(group.hits = sum(Count)) %>% 
    ungroup() %>% 
    full_join(a) %>% 
    mutate(abs.cover = (Count/NumIndices) * 100,
           rel.cover = (Count/group.hits) * 100)
}




#' get_species_cover
#' 
#' calculate percent cover for each species
#'
#' @param lpi 
#'
#' @return
#' @export
#'
#' @details
#' calculates percent cover for each transect at each point
#' 
#' @examples
get_species_cover = function(df){

  
  a <- df %>% 
    distinct(point, Point.Id, year, Point.Index) %>% 
    group_by(point, year) %>% 
    summarise(NumIndices = n())
  
  Fun.Sum <- df %>% 
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


#' get_fungrp_native_richness
#' 
#' calculate the by point species richness
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @details
#' calculates richness for each point, not for each transect in each point
#' 
#'
#' @examples
get_fungrp_native_richness = function(df){

  
df <- df %>%   
  filter(!is.na(nat.nnat.inv)) %>%
    distinct(point, year, USDA.code, FunGrp, nat.nnat.inv) %>%  
    group_by(point, year, FunGrp, nat.nnat.inv) %>% 
    summarise(richness = n(),
              species = paste(USDA.code, collapse = ", "))
  
  }

