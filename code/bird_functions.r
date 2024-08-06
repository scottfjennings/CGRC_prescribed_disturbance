


#' clean_area_search
#' 
#' basic data cleaning tasks
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
clean_area_search <- function(df) {
df_out <- df  %>% 
    filter(Date != "") %>% 
    mutate(Date = as.Date(Date),
           Sampling.Unit.Name = gsub("X", "", Sampling.Unit.Name)) %>% 
    rename("point" = Sampling.Unit.Name,
           "alpha.code" = Spp)
names(df_out) <- tolower(names(df_out))
return(df_out)
}



#' birds_per_area
#' 
#' Calculates the total number of individuals of each species detected in each count area on each day. Collapses individuals detected by different cues.
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
birds_per_area_date <- function(df) {
  df_out <- df %>%
    group_by(point, date, alpha.code, common.name) %>% 
    summarise(tot.Count = sum(count)) %>% 
    ungroup()
}



#' birds_per_area_year
#'
#' calculate the mean and standard deviation number of individuals of each species detected at each plot each year, and number of dates within each year that each species was detected at each plot
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
birds_per_area_year <- function(df) {
  df_out <- df %>%
    group_by(point, date, alpha.code, common.name) %>% 
    summarise(tot.count = sum(count)) %>% 
    ungroup() %>% 
    mutate(year = lubridate::year(date)) %>% 
    group_by(point, year, alpha.code, common.name) %>% 
    summarise(mean.birds = mean(tot.count),
              sd.birds = sd(tot.count),
              num.dates = n())
  
}



#' species_per_area_date
#'
#' calculate the  number of species detected at each plot each date
#' 
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
species_per_area_date <- function(df) {
  df_out <- df %>%
    distinct(point, date, alpha.code) %>% 
    group_by(point, date) %>% 
    summarise(spp.per.area = n()) %>% 
    ungroup()
}



#' species_per_area_year
#' 
#' calculate the mean and standard deviation number of species detected at each plot each year
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
species_per_area_year <- function(df) {
  df_out <- df %>%
    distinct(point, date, alpha.code) %>% 
    group_by(point, date) %>% 
    summarise(spp.per.area = n()) %>% 
    ungroup()%>% 
    mutate(year = lubridate::year(date)) %>% 
    group_by(point, year) %>% 
    summarise(mean.species = mean(spp.per.area),
              sd.species = sd(spp.per.area))
}
