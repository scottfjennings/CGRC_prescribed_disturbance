

read.date = function(date_string){
  library(lubridate)
  
  if(grepl("/", date_string)){
    splitter = "/"
  } else if(grepl( "-", date_string)){
    splitter = "-"
  } else {stop("Date format not recognized")}
  
  
  
  date = as.character(date_string)
  x = strsplit(date, splitter)
  x = unlist(x)
  
  
  if(nchar(x[1]) <= 2){
    date_format = paste("%m", "%d", "%Y", sep = splitter)
  } else if(nchar(x[1]) == 4){
    date_format = paste("%Y", "%m", "%d", sep = splitter)
  } else {stop("Date format not recognized")}
  
  the_date = as.Date(date_string, format = date_format)
  
  return(the_date)
  
}



add.pointyear = function(data){
  
  if("Soil.Surface" %in% colnames(data)){
    data = subset(data, subset = data$Soil.Surface != "")
  }
  
  data <- data %>% 
    mutate(across(contains("Date"), ~as.Date(.)),
           year = year(Date))
  
  
  if("PointId" %in% colnames(data)){
    data$pointyear = paste(data$PointId, "-", data$year, sep = "")
  } else if("Point.Id" %in% colnames(data)){
    data$pointyear = paste(data$Point.Id, "-", data$year, sep = "")
  } else {stop("No Point.Id column identified")}
  
  return(data)
}

split_point_id <- function(df) {
df <- df %>% 
  mutate(Point.Id = gsub("X", "", Point.Id)) %>% 
   separate(Point.Id, 
           into = c("point", "line"), 
           sep = "(?<=[0-9])(?=[A-Za-z])", remove = FALSE)
}
