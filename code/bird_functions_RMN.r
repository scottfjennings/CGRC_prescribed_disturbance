#bird code


#' oakFocal
#'
#' @param focal 
#' @param commonName 
#'
#' @return
#' @export
#'
#' @examples
oakFocal=function(focal, commonName){
  oak<-focal %>% 
    filter(Focal_Group1=="Oak Woodland"|Focal_Group2=="Oak Woodland")
  if(commonName==TRUE){
    oakCommonNames<-as.character(oak$Focal.Species)
    return(oakCommonNames)
  }else{
    oakSppCodes<-as.character(oak$Code)
    return(oakSppCodes)
  }
  
}

#' riparianFocal
#'
#' @param focal 
#' @param commonName 
#'
#' @return
#' @export
#'
#' @examples
riparianFocal=function(focal, commonName){
  ripar<-focal %>% 
    filter(Focal_Group1=="Riparian"|Focal_Group2=="Riparian"|Focal_Group3=="Riparian")
  if(commonName==TRUE){
    ripCommonNames<-as.character(ripar$Focal.Species)
    return(ripCommonNames)
  }else{
    ripSppCodes<-as.character(ripar$Code)
    return(ripSppCodes)
  }
  
}


#' grassFocal
#'
#' @param focal 
#' @param commonName 
#'
#' @return
#' @export
#'
#' @examples
grassFocal=function(focal,commonName){
  Grass<-focal %>% 
    filter(Focal_Group1=="Grassland"|Focal_Group2=="Grassland"|Focal_Group3=="Grassland")
  if(commonName==TRUE){
    grassCommonNames<-as.character(Grass$Focal.Species)    
    return(grassCommonNames)
  }else{
    grassSppCode<-as.character(Grass$Code)
    return(grassSppCode)
  }
  
}

#' mountainFocal
#'
#' @param focal 
#' @param commonName 
#'
#' @return
#' @export
#'
#' @examples
mountainFocal=function(focal,commonName){
  mountian<-focal %>% 
    filter(Focal_Group1=="Mountain Meadow 3"| Focal_Group2=="Mountain Meadow 3"| Focal_Group3=="Mountain Meadow 3")
  if(commonName==TRUE){
    mountCommonNames<-as.character(mountian$Focal.Species)
    return(mountCommonNames)
  }else{
    mountSppCode<-as.character(mountian$Code)
    return(mountSppCode)
  }
  
}



#' all_remove.first
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
all_remove.first<-function(df){
  if(df[1,1]==""){
    df<-df[-1,]
    rownames(df)<-1:nrow(df)
    return(df)
  } 
  else { 
    df<-df
  }
}


#' all_Tidynames
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
all_Tidynames<-function(df){
  tidy.name.vector <- make.names(names(df), unique=TRUE)
  names(df)<-tidy.name.vector
  return(df)
}

#' bird_MAPdistance.bin
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
bird_MAPdistance.bin<-function (df){
  if(is.factor(df$Distance.Bin)){
    df$Distance.Bin <- mapvalues(df$Distance.Bin, c("<0","75 to 100", "50 to 75", ">75", ">50","<50" ,"", "<10", "10 to 20", "20 to 30", "30 to 50", "30 to 40" ,"40 to 50", "50 to 100", ">100" ,"FlyOver", ">300"), c(0,85, 65, 300, 65, 25, 300,5, 15, 25, 35 ,40,45 ,  75, 300, 300,300))
    df$Distance.Bin<-as.numeric(as.character(df$Distance.Bin))
    return(df)
  } else if(is.numeric(df$Distance.Bin) ){ return(df) 
  }else {
    df$Distance.Bin=as.factor(df$Distance.Bin)
    df$Distance.Bin <- mapvalues(df$Distance.Bin, c("<0","75 to 100", "50 to 75", ">75", ">50","<50" ,"", "<10", "10 to 20", "20 to 30", "30 to 50", "30 to 40" ,"40 to 50", "50 to 100", ">100" ,"FlyOver", ">300"), c(0,85, 65, 300, 65, 25, 300,5, 15, 25, 35 ,40,45 ,  75, 300, 300,300))
    df$Distance.Bin<-as.numeric(as.character(df$Distance.Bin))
    return(df)
  }
}

#' bird_prep.columns
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
bird_prep.columns<-function(df){
  df$Distance.Bin<-as.numeric(df$Distance.Bin)
  if(is.Date(df$Date)){
    print('already a date!')
  }else{
    df$Date<-as_date(df$Date, format="%m/%d/%Y")
  }
  
  #df$Date<-as_date(df$Date, format="%m/%d/%Y")
  df$YEAR<-year(df$Date)
  df$SURVEY<-as.factor(paste(df$Point,df$YEAR,"V",df$Visit,sep=""))
  df$PointYear<-as.factor(paste(df$Point,df$YEAR, sep=""))
  df$SAMPLE<-as.factor(paste(df$Point,df$YEAR, sep=""))
  df$Tally<-1
  
  return(df)
}

#' bird_subset.cue
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
bird_subset.cue<-function(df){
  testing<-df %>%
    filter( Distance.Bin.ID != "B30") %>%
    filter(Distance.Bin.ID !="FLO") %>%
    filter(Distance.Bin.ID != "B00") %>%
    filter(Detection.Cue !="J")
  df<-testing
  return(df)
}


#' bird_updateSpeciesNames
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
bird_updateSpeciesNames<-function(df){
  
  df$Spp[df$YEAR < 2016 & df$Spp=="WESJ"]<-"CASJ"
  df$Common.Name[df$YEAR < 2016 & df$Common.Name=="Western Scrub-Jay"]<-"California Scrub-Jay"
  return(df)
}

#' bird_prepare
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
bird_prepare=function(df){
  bird<-all_remove.first(df)
  bird<-all_Tidynames(bird)
  bird<-bird_MAPdistance.bin(bird)
  
  bird<-bird_prep.columns(bird)
  bird<-bird_subset.cue(bird)
  bird<-bird_updateSpeciesNames(bird)
  return(bird)
}

#' bird_visits
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
bird_visits<-function(df){
  
  df2<-df
  a<-subset(df2, duplicated(df2$SURVEY)==FALSE)
  visits<-aggregate(a$Tally, list(a$PointYear), sum)
  names(visits)<-c("PointYear", "Visits")
  return(visits)
}


#' bird_birdcount.year
#'
#' @param df 
#' @param distance 
#' @param transect 
#' @param surveyyear 
#'
#' @return
#' @export
#'
#' @examples
bird_birdcount.year<-function(df, distance =300, transect=c(levels(as.factor(df$Transect))), surveyyear=c(levels(as.factor(df$YEAR)))){
  
  
  visits<-bird_visits(df)
  
  data<-subset(df, subset = df$Distance.Bin < distance)
  data$Distance.Bin.ID<-as.factor(data$Distance.Bin.ID)
  data = subset(data, Transect %in% transect)
  data = subset(data, YEAR %in% surveyyear)
  df<-data
  
  
  species2<-aggregate(df$Count,list(df$Spp,df$Common.Name ,df$Scientific.Name  , df$Transect, df$YEAR),sum)
  names(species2)<-c("Spp","Common.Name", "Scientific.Name", "Transect", "YEAR","COUNT")
  
  
  df4<- species2%>%
    dplyr::group_by(Spp, Transect, YEAR ) %>%
    dplyr::arrange(desc(COUNT))
  
  #  species<-left_join(df2, df3, by="Spp")
  
  return(df4)
}


#' add.zeros.commonName
#'
#' @param pc2 
#'
#' @return
#' @export
#'
#' @examples
add.zeros.commonName<-function(pc2){
  
  columnsRemoved<-names(pc2) %in% c("Spp","Scientific.Name")
  pc2_reduced<-pc2[!columnsRemoved]
  wide <- reshape(pc2_reduced, v.names="COUNT", idvar="PointYear",timevar="Common.Name", direction="wide")
  first<-wide[,1:5]
  second<-wide[,6:length(wide[1,])]
  second0 <- second
  second[] <- lapply(second,function(x) replace(x, is.na(x), 0))
  final<-as.data.frame(cbind(first,second))
  narrow<-reshape(final,idvar="PointYear",varying=list(names(final)[6:length(final[1,])]),direction="long",times=names(final)[6:length(final[1,])],v.names="COUNT",timevar="Spp")
  narrow2<-separate(data = narrow, col = Spp, into = c("Count", "Spp"), sep = "\\.")
  countRemoved<-names(narrow2)%in%c("Count")
  narrow2<-narrow2[!countRemoved]
  row.names(narrow2)<-NULL
  narrow2$ABUNDANCE<-narrow2$COUNT/narrow2$Visits
  narrow2<-subset(narrow2, select=c("PointYear","Transect", "YEAR", "POINT", "Visits", "Spp", "ABUNDANCE"))
  return(narrow2)
}

#' add.zeros.noCount
#'
#' @param pc 
#'
#' @return
#' @export
#'
#' @examples
add.zeros.noCount<-function(pc){
  #takes in df that has been through bird_species.common
  wide <- reshape(pc, v.names="COUNT", idvar="PointYear",timevar="Spp", direction="wide")
  first<-wide[,1:5]
  second<-wide[,6:length(wide[1,])]
  second0 <- second
  second[] <- lapply(second,function(x) replace(x, is.na(x), 0))
  final<-as.data.frame(cbind(first,second))
  narrow<-reshape(final,idvar="PointYear",varying=list(names(final)[6:length(final[1,])]),direction="long",times=names(final)[6:length(final[1,])],v.names="COUNT",timevar="Spp")
  narrow$Spp<-as.factor(substr(narrow$Spp,7,10))
  row.names(narrow)<-NULL
  narrow$ABUNDANCE<-narrow$COUNT/narrow$Visits
  narrow2<-subset(narrow, select=c("PointYear","Transect", "YEAR", "POINT", "Visits", "Spp", "ABUNDANCE"))
  return(narrow2)
}


#' bird_species.narrow
#'
#' @param df 
#' @param distance 
#' @param transect 
#' @param surveyyear 
#'
#' @return
#' @export
#'
#' @examples
bird_species.narrow<-function(df,distance=300,transect=c(levels(as.factor(df$Transect))), surveyyear=c(levels(as.factor(df$YEAR)))){
  
  visits<-bird_visits(df)
  
  data<-subset(df, subset = df$Distance.Bin < distance)
  data$Distance.Bin.ID<-as.factor(data$Distance.Bin.ID)
  data = subset(data, Transect %in% transect)
  data = subset(data, YEAR %in% surveyyear)
  df<-data
  
  species<-aggregate(df$Count,list(df$Spp, df$Transect, df$YEAR, df$Point),sum)
  names(species)<-c("Spp","Transect", "YEAR", "POINT","COUNT")
  species$PointYear<-as.factor(paste(species$POINT,species$YEAR, sep=""))
  
  species<-left_join(species, visits, by="PointYear")
  
  
  return(species)
  
}


#' bird_focal.graph_RelativeAbundance
#'
#' @param df 
#' @param focal 
#' @param distance 
#' @param transect 
#' @param surveyyear 
#' @param choose_focal_group 
#'
#' @return
#' @export
#'
#' @examples
bird_focal.graph_RelativeAbundance=function(df, focal, distance=300,transect=c(levels(as.factor(df$Transect))), surveyyear=c(levels(as.factor(df$YEAR))), choose_focal_group= c("Grassland", "Oak.Woodland", "Riparian", "Mountian.Meadow")){
  species<-bird_species.narrow(df, 300)
  print(species)
  pc<-droplevels(species)
  newpc2<-add.zeros.noCount(pc)
  print(newpc2)
  
  
  points.variable =newpc2 %>% 
    dplyr::group_by(Transect, YEAR) %>% 
    dplyr::distinct(POINT) %>% 
    mutate(pointyear=paste(POINT,YEAR, sep = "_"))
  
  ##Display this above graph.
  NumberOfPoints<-points.variable %>% 
    dplyr::group_by(Transect,YEAR) %>% 
    dplyr::summarize(NumberOfPointCountLocations=length(pointyear)) %>% 
    dplyr::mutate(tranYear=paste(Transect,YEAR,sep="_")) %>% 
    select("tranYear", "NumberOfPointCountLocations") 
  # NumberOfPoints<-NumberOfPoints[,-1]
  
  narrow<-bird_species.narrow(df, distance = distance)
  pc2<-droplevels(narrow)
  newpc2<-add.zeros.noCount(pc2)
  
  df2<- newpc2 %>% 
    dplyr::mutate(tranYear=paste(Transect,YEAR,sep = "_")) %>% 
    dplyr::group_by(Spp,tranYear,YEAR) %>% 
    dplyr::summarise(Abundance=sum(ABUNDANCE)) %>% 
    merge(NumberOfPoints,by="tranYear") %>% 
    dplyr::mutate(Relative.Abundance=round(Abundance/NumberOfPointCountLocations, digits=2))
  
  
  
  df2 = subset(df2, Transect %in% transect)
  df2 = subset(df2, YEAR %in% surveyyear)
  # df<-subset(df, subset = df$Distance.Bin <= distance)
  
  if(choose_focal_group=="Mountian.Meadow"){
    #get focal data
    mountSppCode=mountainFocal(focal,commonName = FALSE)
    df2 = subset(df2, Spp %in% mountSppCode)
  }
  if(choose_focal_group=="Grassland"){
    grassSppCode=grassFocal(focal,commonName = FALSE)
    df2 = subset(df2, Spp %in% grassSppCode)
  }
  if(choose_focal_group=="Oak.Woodland"){
    oakSppCodes=oakFocal(focal,commonName = FALSE)
    df2 = subset(df2, Spp %in% oakSppCodes)
  }
  if(choose_focal_group=="Riparian"){
    ripSppCodes=riparianFocal(focal, commonName = FALSE)
    df2 = subset(df2, Spp %in% ripSppCodes)
  }
  df2$YEAR<-as.factor(df2$YEAR)
  
  titleCustom = paste(choose_focal_group ,": Bird Species Relative Abundance: Radius of " , distance, "meters")
  df2 = df2 %>% 
    filter(Abundance != 0.0)
  
  ggplot(df2,aes(x=Spp, y=Relative.Abundance, group=YEAR, fill = YEAR))+
    geom_col( position = "dodge")+
    ylab("Focal Species Abundance")+
    xlab("Species Code")+
    theme(axis.text.x = element_text(angle = 40, hjust = 1))+
    geom_text(aes(label=as.character(Relative.Abundance)), position=position_dodge(width=0.9), vjust=-0.25)+
    #geom_text(aes(label=as.character(df3$Abundance)),vjust=-.7 )+
    ggtitle(titleCustom)+
    scale_fill_manual(values = c("olivedrab", "olivedrab3","gray50", "gray70", "gray38", "gray12", "gray05"))
  
  
}


#' bird_focal.graph_TotalAbundance
#'
#' @param df 
#' @param focal 
#' @param distance 
#' @param transect 
#' @param surveyyear 
#' @param choose_focal_group 
#'
#' @return
#' @export
#'
#' @examples
bird_focal.graph_TotalAbundance<-function (df,focal, distance,transect, surveyyear, choose_focal_group= c("Grassland", "Oak.Woodland", "Riparian", "Mountian.Meadow")) {
  
  
  species<-bird_species.narrow(df, 300)
  
  pc<-droplevels(species)
  newpc2<-add.zeros.noCount(pc)
  
  points.variable =newpc2 %>% 
    dplyr::group_by(Transect, YEAR) %>% 
    dplyr::distinct(POINT) %>% 
    mutate(pointyear=paste(POINT,YEAR, sep = "_"))
  
  ##Display this above graph.
  NumberOfPoints<-points.variable %>% 
    dplyr::group_by(Transect,YEAR) %>% 
    dplyr::summarize(NumberOfPointCountLocations=length(pointyear)) %>% 
    dplyr::mutate(tranYear=paste(Transect,YEAR,sep="_")) %>% 
    select("tranYear", "NumberOfPointCountLocations") 
  # NumberOfPoints<-NumberOfPoints[,-1]
  
  narrow<-bird_species.narrow(df, distance = distance)
  pc2<-droplevels(narrow)
  newpc2<-add.zeros.noCount(pc2)
  df2<- newpc2 %>% 
    dplyr::mutate(tranYear=paste(Transect,YEAR,sep = "_")) %>% 
    dplyr::group_by(Spp,tranYear,YEAR) %>% 
    dplyr::summarise(Abundance=sum(ABUNDANCE)) %>% 
    merge(NumberOfPoints,by="tranYear") %>% 
    dplyr::mutate(Relative.Abundance=Abundance/NumberOfPointCountLocations)
  
  
  df2 = subset(df2, Transect %in% transect)
  df2 = subset(df2, YEAR %in% surveyyear)
  # df<-subset(df, subset = df$Distance.Bin <= distance)
  
  if(choose_focal_group=="Mountian.Meadow"){
    #get focal data
    mountSppCode=mountainFocal(focal, commonName = FALSE)
    df2 = subset(df2, Spp %in% mountSppCode)
  }
  if(choose_focal_group=="Grassland"){
    grassSppCode=grassFocal(focal, commonName = FALSE)
    df2 = subset(df2, Spp %in% grassSppCode)
  }
  if(choose_focal_group=="Oak.Woodland"){
    oakSppCodes=oakFocal(focal, commonName = FALSE)
    df2 = subset(df2, Spp %in% oakSppCodes)
  }
  if(choose_focal_group=="Riparian"){
    ripSppCodes=riparianFocal(focal,commonName = FALSE)
    df2 = subset(df2, Spp %in% ripSppCodes)
  }
  df2$YEAR<-as.factor(df2$YEAR)
  
  titleCustom = paste(choose_focal_group ,": Bird Species Total Abundance: Radius of " , distance, "meters")
  df2 = df2 %>% 
    filter(Abundance != 0.0)
  plt= ggplot(df2,aes(x=Spp, y=Abundance, group=YEAR, fill=YEAR))+
    geom_col( stat="identity", position = "dodge")+
    ylab("Focal Species Abundance")+
    xlab("Species Code")+
    theme(axis.text.x = element_text(angle = 40, hjust = 1))+
    geom_text(aes(label=as.character(Abundance)), position=position_dodge(width=0.9), vjust=-0.25)+
    ggtitle(titleCustom)+
    scale_fill_manual(values = c("olivedrab", "olivedrab3","gray50", "gray70", "gray38", "gray12", "gray05"))
  return(plt)
}


#' bird_focal.RelativeAbundance.Facet
#'
#' @param df 
#' @param focal 
#' @param distance 
#' @param transect 
#' @param surveyyear 
#' @param choose_focal_group 
#'
#' @return
#' @export
#'
#' @examples
bird_focal.RelativeAbundance.Facet<-function (df,focal, distance,transect, surveyyear, choose_focal_group= c("Grassland", "Oak.Woodland", "Riparian", "Mountian.Meadow")) {
  
  
  species2<- bird_species.pointYear(df, 300)
  pc2<-droplevels(species2)
  pcCommonName<-add.zeros.commonName(pc2)
  
  
  points.variable =pcCommonName %>% 
    dplyr::group_by(Transect, YEAR) %>% 
    dplyr::distinct(POINT) %>% 
    mutate(pointyear=paste(POINT,YEAR, sep = "_"))
  
  ##Display this above graph.
  NumberOfPoints<-points.variable %>% 
    dplyr::group_by(Transect,YEAR) %>% 
    dplyr::summarize(NumberOfPointCountLocations=length(pointyear)) %>% 
    dplyr::mutate(tranYear=paste(Transect,YEAR,sep="_")) %>% 
    select("tranYear", "NumberOfPointCountLocations") 
  # NumberOfPoints<-NumberOfPoints[,-1]
  
  narrow<-bird_species.pointYear(df, distance = distance)
  pcnarrow<-droplevels(narrow)
  newpc2<-add.zeros.commonName(pcnarrow)
  
  df2<- newpc2 %>% 
    dplyr::mutate(tranYear=paste(Transect,YEAR,sep = "_")) %>% 
    dplyr::group_by(Spp,tranYear,YEAR) %>% 
    dplyr::summarise(Abundance=sum(ABUNDANCE)) %>% 
    merge(NumberOfPoints,by="tranYear") %>% 
    dplyr::mutate(Relative.Abundance=round(Abundance/NumberOfPointCountLocations, digits = 2))
  
  
  
  df2 = subset(df2, Transect %in% transect)
  df2 = subset(df2, YEAR %in% surveyyear)
  # df<-subset(df, subset = df$Distance.Bin <= distance)
  
  if(choose_focal_group=="Mountian.Meadow"){
    #get focal data
    mountSppCode=mountainFocal(focal, commonName = TRUE)
    df2 = subset(df2, Spp %in% mountSppCode)
  }
  if(choose_focal_group=="Grassland"){
    grassSppCode=grassFocal(focal, commonName = TRUE)
    df2 = subset(df2, Spp %in% grassSppCode)
  }
  if(choose_focal_group=="Oak.Woodland"){
    oakSppCodes=oakFocal(focal, commonName = TRUE)
    df2 = subset(df2, Spp %in% oakSppCodes)
  }
  if(choose_focal_group=="Riparian"){
    ripSppCodes=riparianFocal(focal, commonName = TRUE)
    df2 = subset(df2, Spp %in% ripSppCodes)
  }
  df2$YEAR<-as.factor(df2$YEAR)
  
  titleCustom = paste(choose_focal_group ,": Bird Species Relative Abundance: Radius of " , distance, "meters")
  
  df2 = df2 %>% 
    filter(Abundance != 0.0)
  print(head(df2))
  ggplot(df2,aes(x=df2$YEAR, y=df2$Relative.Abundance, group=df2$YEAR,fill = df2$YEAR))+
    geom_col( position = "dodge")+
    labs(fill="Year")+
    facet_wrap( ~ df2$Spp, scales = "free")+
    ylim(0,max(df2$Relative.Abundance)+(.25*max(df2$Relative.Abundance)))+
    ylab("Focal Species Abundance")+
    xlab("Year")+
    theme(axis.text.x = element_text(angle = 40, hjust = 1))+
    geom_text(aes(label=as.character(df2$Relative.Abundance)), position=position_dodge(width=0.9), vjust=-0.25)+
    #geom_text(aes(label=as.character(df3$Abundance)),vjust=-.7 )+
    ggtitle(titleCustom)+
    scale_fill_manual(values = c("olivedrab", "olivedrab3","gray50", "gray70", "gray38", "gray12", "gray05"))
  
  
}


#' bird_focal.TotalAbundance.Facet
#'
#' @param df 
#' @param focal 
#' @param distance 
#' @param transect 
#' @param surveyyear 
#' @param choose_focal_group 
#'
#' @return
#' @export
#'
#' @examples
bird_focal.TotalAbundance.Facet<-function (df,focal, distance,transect=c(levels(as.factor(df$Transect))), surveyyear=c(levels(as.factor(df$YEAR))), choose_focal_group= c("Grassland", "Oak.Woodland", "Riparian", "Mountian.Meadow")) {
  
  species2<- bird_species.pointYear(df, 300)
  pc2<-droplevels(species2)
  pcCommonName<-add.zeros.commonName(pc2)
  points.variable =pcCommonName %>% 
    dplyr::group_by(Transect, YEAR) %>% 
    dplyr::distinct(POINT) %>% 
    mutate(pointyear=paste(POINT,YEAR, sep = "_"))
  ##Display this above graph.
  NumberOfPoints<-points.variable %>% 
    dplyr::group_by(Transect,YEAR) %>% 
    dplyr::summarize(NumberOfPointCountLocations=length(pointyear)) %>% 
    dplyr::mutate(tranYear=paste(Transect,YEAR,sep="_")) %>% 
    select("tranYear", "NumberOfPointCountLocations") 
  # NumberOfPoints<-NumberOfPoints[,-1]
  narrow<-bird_species.pointYear(df, distance = distance)
  pcnarrow<-droplevels(narrow)
  newpc2<-add.zeros.commonName(pcnarrow)
  df2<- newpc2 %>% 
    dplyr::mutate(tranYear=paste(Transect,YEAR,sep = "_")) %>% 
    dplyr::group_by(Spp,tranYear,YEAR) %>% 
    dplyr::summarise(Abundance=sum(ABUNDANCE)) %>% 
    merge(NumberOfPoints,by="tranYear") %>% 
    dplyr::mutate(Relative.Abundance=round(Abundance/NumberOfPointCountLocations, digits = 2))
  
  
  df2 = subset(df2, Transect %in% transect)
  
  df2 = subset(df2, YEAR %in% surveyyear)
  
  # df<-subset(df, subset = df$Distance.Bin <= distance)
  
  if(choose_focal_group=="Mountian.Meadow"){
    #get focal data
    mountSppCode=mountainFocal(focal, commonName = TRUE)
    df2 = subset(df2, Spp %in% mountSppCode)
  }
  if(choose_focal_group=="Grassland"){
    
    grassSppCode<-grassFocal(focal, commonName = TRUE)
    
    df2 = subset(df2, Spp %in% grassSppCode)
    print(head(df2))
  }
  if(choose_focal_group=="Oak.Woodland"){
    #oakSppCodes=oakFocal(focal)
    
    oakSppCodes<-oakFocal(focal, commonName = TRUE)
    
    df2 = subset(df2, Spp %in% oakCommonNames)
  }
  if(choose_focal_group=="Riparian"){
    ripSppCodes=riparianFocal(focal, commonName = TRUE)
    df2 = subset(df2, Spp %in% ripSppCodes)
  }
  df2$YEAR<-as.factor(df2$YEAR)
  
  titleCustom = paste(choose_focal_group, ": Bird Species Total Abundance: Radius of " , distance, "meters")
  df2 = df2 %>% 
    filter(Abundance != 0.0)
  
  print(str(df2$Abundance))
  
  ggplot(df2,aes(x=YEAR, y=Abundance, fill=YEAR ))+
    geom_col( position = "dodge")+
    labs(fill='Year')+
    facet_wrap( ~ df2$Spp, scales = "free")+
    ylim(0,max(df2$Abundance)+(.25*max(df2$Abundance)))+
    ylab("Focal Species Abundance")+
    xlab("Year")+
    theme(axis.text.x = element_text(angle = 40, hjust = 1))+
    geom_text(aes(label=as.character(Abundance)), position=position_dodge(width=0.9), vjust=-0.25)+
    #geom_text(aes(label=as.character(df3$Abundance)),vjust=-.7 )+
    ggtitle(titleCustom)+
    scale_fill_manual(values = c("olivedrab", "olivedrab3","gray50", "gray70", "gray38", "gray12", "gray05"))
  
}



#' bird_species.relative.richness
#'
#' @param df 
#' @param distance 
#' @param transect 
#' @param surveyyear 
#'
#' @return
#' @export
#'
#' @examples
bird_species.relative.richness<-function(df,distance = 300, transect=c(levels(as.factor(df$Transect))), surveyyear=c(levels(as.factor(df$YEAR)))){
  
  
  a<-subset(df,duplicated(df$SURVEY)==FALSE)
  visits<-aggregate(a$Tally, list(a$PointYear), sum)
  names(visits)<-c("PointYear", "Visits")
  
  
  data2<-subset(df, subset = df$Distance.Bin <= distance)
  data2$Distance.Bin.ID<-as.factor(data2$Distance.Bin.ID)
  data2 = subset(data2, Transect %in% transect)
  data2 = subset(data2, YEAR %in% surveyyear)
  df2<-data2
  
  species2<-aggregate(df2$Count,list(df2$Spp , df2$Transect, df2$YEAR, df2$Point),sum)
  names(species2)<-c("Spp", "Transect", "YEAR", "POINT","COUNT")
  species2$PointYear<-as.factor(paste(species2$POINT,species2$YEAR, sep=""))
  
  species<-left_join(species2, visits, by="PointYear")
  df3<-add.zeros.noCount(species)
  
  df4<-reshape(df3, v.names="ABUNDANCE", idvar="PointYear",timevar="Spp", direction="wide")
  
  JustSpp<-substr(names(df4[,6:ncol(df4)]),11,14)
  colnames(df4)[6:ncol(df4)] <- JustSpp
  
  first<-df4[,1:5]
  second<-df4[,6:length(df4[1,])]
  second<-second[,order(colnames(second))]
  df5<-as.data.frame(cbind(first,second))
  
  
  df5$Richness<-rowSums(df5[,5:ncol(df5)] != 0)
  
  
  
  
  df5<-df5[,c("YEAR", "POINT","Visits", "Richness")]
  df5<-df5 %>% 
    mutate(RelativeAbundance= Richness/Visits)
  df5[,2]<-as.factor(df5[,2])
  
  titleCustom = paste("Bird Species Relative Richness per Point per Visit: Radius of " , distance, "meters")
  
  ggplot(df5,aes(x=df5$POINT, y=as.factor(df5$RelativeAbundance), fill=as.factor(df5$YEAR), group=YEAR))+
    geom_col( position = "dodge")+
    ylab("Bird Species Relative Richness")+
    xlab("Point Id")+
    theme(axis.text.x = element_text(angle = 40, hjust = 1))+
    geom_text(aes(label=as.character(df5$RelativeAbundance)), position=position_dodge(width=0.9), vjust=-0.25)+
    ggtitle(titleCustom)+
    scale_fill_manual(name ="Year",values = c("gray28","dodgerblue4","deepskyblue3","lightblue3", "lightblue4", "lightblue3","lightblue1"))
  
  
  
}



#' bird_species.total.richness
#'
#' @param df 
#' @param distance 
#' @param transect 
#' @param surveyyear 
#'
#' @return
#' @export
#'
#' @examples
bird_species.total.richness<-function(df,distance = 300, transect=c(levels(as.factor(df$Transect))), surveyyear=c(levels(as.factor(df$YEAR)))){
  a<-subset(df,duplicated(df$SURVEY)==FALSE)
  visits<-aggregate(a$Tally, list(a$PointYear), sum)
  names(visits)<-c("PointYear", "Visits")
  
  
  data2<-subset(df, subset = df$Distance.Bin <= distance)
  data2$Distance.Bin.ID<-as.factor(data2$Distance.Bin.ID)
  data2 = subset(data2, Transect %in% transect)
  data2 = subset(data2, YEAR %in% surveyyear)
  df2<-data2
  
  species2<-aggregate(df2$Count,list(df2$Spp , df2$Transect, df2$YEAR, df2$Point),sum)
  names(species2)<-c("Spp", "Transect", "YEAR", "POINT","COUNT")
  species2$PointYear<-as.factor(paste(species2$POINT,species2$YEAR, sep=""))
  
  species<-left_join(species2, visits, by="PointYear")
  df3<-add.zeros.noCount(species)
  
  df4<-reshape(df3, v.names="ABUNDANCE", idvar="PointYear",timevar="Spp", direction="wide")
  
  JustSpp<-substr(names(df4[,6:ncol(df4)]),11,14)
  colnames(df4)[6:ncol(df4)] <- JustSpp
  
  first<-df4[,1:5]
  second<-df4[,6:length(df4[1,])]
  second<-second[,order(colnames(second))]
  df5<-as.data.frame(cbind(first,second))
  
  
  df5$Richness<-rowSums(df5[,5:ncol(df5)] != 0)
  
  
  
  
  df5<-df5[,c("YEAR", "POINT", "Richness")]
  
  df5[,2]<-as.factor(df5[,2])
  
  titleCustom = paste("Bird Species Total Richness per Point: Radius of " , distance, "meters")
  
  ggplot(df5,aes(x=df5$POINT, y=as.factor(df5$Richness), fill=as.factor(df5$YEAR), group=YEAR))+
    geom_col( position = "dodge")+
    ylab("Bird Species Total Richness")+
    xlab("Point Id")+
    theme(axis.text.x = element_text(angle = 40, hjust = 1))+
    geom_text(aes(label=as.character(df5$Richness)), position=position_dodge(width=0.9), vjust=-0.25)+
    ggtitle(titleCustom)+
    scale_fill_manual(name ="Year",values = c("gray28","dodgerblue4","deepskyblue3","lightblue3", "lightblue4", "lightblue3","lightblue1"))
  
  
  
}





#' bird_species.pointYear
#'
#' @param df 
#' @param distance 
#' @param transect 
#' @param surveyyear 
#'
#' @return
#' @export
#'
#' @examples
bird_species.pointYear<-function(df, distance=300,transect=c(levels(as.factor(df$Transect))), surveyyear=c(levels(as.factor(df$YEAR)))){
  visits<-bird_visits(df)
  
  data<-subset(df, subset = df$Distance.Bin < distance)
  data$Distance.Bin.ID<-as.factor(data$Distance.Bin.ID)
  data = subset(data, Transect %in% transect)
  data = subset(data, YEAR %in% surveyyear)
  df<-data
  
  
  species2<-aggregate(df$Count,list(df$Spp,df$Common.Name ,df$Scientific.Name  , df$Transect, df$YEAR, df$Point),sum)
  names(species2)<-c("Spp","Common.Name", "Scientific.Name", "Transect", "YEAR", "POINT","COUNT")
  species2$PointYear<-as.factor(paste(species2$POINT,species2$YEAR, sep=""))
  
  species<-left_join(species2, visits, by="PointYear")
  return(species)
  
}


#' bird_wide.focal
#'
#' @param df 
#' @param distance 
#' @param transect 
#' @param surveyyear 
#' @param choose_focal_group 
#'
#' @return
#' @export
#'
#' @examples
bird_wide.focal<-function(df, distance, transect=c(levels(as.factor(df$Transect))), surveyyear=c(levels(as.factor(df$YEAR))), choose_focal_group= c("Grassland", "Oak Woodland", "Riparian")){
  
  visits<-bird_visits(df)
  
  data<-subset(df, subset = df$Distance.Bin <= distance)
  data$Distance.Bin.ID<-as.factor(data$Distance.Bin.ID)
  data = subset(data, Transect %in% transect)
  data = subset(data, YEAR %in% surveyyear)
  df2<-data
  
  
  species2<-aggregate(df2$Count,list(df2$Spp , df2$Transect, df2$YEAR, df2$Point),sum)
  names(species2)<-c("Spp", "Transect", "YEAR", "POINT","COUNT")
  species2$PointYear<-as.factor(paste(species2$POINT,species2$YEAR, sep=""))
  
  species<-left_join(species2, visits, by="PointYear")
  
  df3<-add.zeros.noCount(species)
  
  
  df4<-reshape(df3, v.names="ABUNDANCE", idvar="PointYear",timevar="Spp", direction="wide")
  
  JustSpp<-substr(names(df4[,6:ncol(df4)]),11,14)
  colnames(df4)[6:ncol(df4)] <- JustSpp
  
  first<-df4[,1:5]
  second<-df4[,6:length(df4[1,])]
  second<-second[,order(colnames(second))]
  df5<-as.data.frame(cbind(first,second))
  
  data = subset(data, Transect %in% transect)
  data = subset(data, YEAR %in% surveyyear)
  df5 = subset(df5, df5$YEAR %in% surveyyear)
  df<-df5
  
  #subset grassland birds, Ferruginous Hawk, Grasshopper Sparrow , Mountain Plover, Northern Harrier,White-tailed Kite, Western Meadowlark
  
  grassland<-df[,c(1:5)]
  if("MOPL" %in% colnames(df)){grassland6<-select(df, MOPL)
  grassland<-bind_cols(grassland, grassland6)}else{df<-df}
  if("GRSP" %in% colnames(df)){grassland2<-select(df, GRSP)
  grassland<-bind_cols(grassland, grassland2)}else{df<-df}
  if("SAVS" %in% colnames(df)){grassland3<-select(df, SAVS)
  grassland<-bind_cols(grassland, grassland3)}else{df<-df}
  if("WEME" %in% colnames(df)){grassland8<-select(df, WEME)
  grassland<-bind_cols(grassland, grassland8)}else{df<-df}
  if("LOSH" %in% colnames(df)){grassland4<-select(df, LOSH)
  grassland<-bind_cols(grassland, grassland4)}else{df<-df}
  if("FEHA" %in% colnames(df)){grassland5<-select(df, FEHA)
  grassland<-bind_cols(grassland, grassland5)}else{df<-df}
  if("WTKI" %in% colnames(df)){grassland7<-select(df, WTKI)
  grassland<-bind_cols(grassland, grassland7)}else{df<-df}
  if("NOHA" %in% colnames(df)){grassland9<-select(df, NOHA)
  grassland<-bind_cols(grassland, grassland9)}else{df<-df}
  if("BUOW" %in% colnames(df)){grassland10<-select(df, BUOW)
  grassland<-bind_cols(grassland, grassland10)}else{df<-df}
  if("AMKE" %in% colnames(df)){grassland11<-select(df, AMKE)
  grassland<-bind_cols(grassland, grassland11)}else{df<-df}
  
  
  oak<-df[,c(1:5)]
  if("ACWO" %in% colnames(df)){oak1<-select(df, ACWO)
  oak<-bind_cols(oak, oak1)}else{df<-df}
  if("NUWO" %in% colnames(df)){oak2<-select(df, NUWO)
  oak<-bind_cols(oak, oak2)}else{df<-df}
  if("ATFL" %in% colnames(df)){oak3<-select(df, ATFL)
  oak<-bind_cols(oak, oak3)}else{df<-df}
  if("WBNU" %in% colnames(df)){oak4<-select(df, WBNU)
  oak<-bind_cols(oak, oak4)}else{df<-df}
  if("WEBL" %in% colnames(df)){oak5<-select(df, WEBL)
  oak<-bind_cols(oak, oak5)}else{df<-df}
  if("OATI" %in% colnames(df)){oak6<-select(df, OATI)
  oak<-bind_cols(oak, oak6)}else{df<-df}
  if("EUST" %in% colnames(df)){oak7<-select(df, EUST)
  oak<-bind_cols(oak, oak7)}else{df<-df}
  if("YBMA" %in% colnames(df)){oak8<-select(df, YBMA)
  oak<-bind_cols(oak, oak8)}else{df<-df}
  if("HUVI" %in% colnames(df)){oak9<-select(df, HUVI)
  oak<-bind_cols(oak, oak9)}else{df<-df}
  if("BEWR" %in% colnames(df)){oak10<-select(df, BEWR)
  oak<-bind_cols(oak, oak10)}else{df<-df}
  if("BLGR" %in% colnames(df)){oak11<-select(df, BLGR)
  oak<-bind_cols(oak, oak10)}else{df<-df}
  if("CALT" %in% colnames(df)){oak12<-select(df, CALT)
  oak<-bind_cols(oak, oak12)}else{df<-df}
  if("CASJ" %in% colnames(df)){oak13<-select(df, CASJ)
  oak<-bind_cols(oak, oak13)}else{df<-df}
  if("LASP" %in% colnames(df)){oak14<-select(df, LASP)
  oak<-bind_cols(oak, oak14)}else{df<-df}
  if("CAQU" %in% colnames(df)){oak15<-select(df, CAQU)
  oak<-bind_cols(oak, oak15)}else{df<-df}
  
  riparian<-df[,c(1:5)]
  if("ATFL" %in% colnames(df)){riparian1<-select(df, ATFL)
  riparian<-bind_cols(riparian, riparian1)}else{df<-df}
  if("NOFL" %in% colnames(df)){riparian2<-select(df, NOFL)
  riparian<-bind_cols(riparian, riparian2)}else{df<-df}
  if("NUWO" %in% colnames(df)){riparian3<-select(df, NUWO)
  riparian<-bind_cols(riparian, riparian3)}else{df<-df}
  if("LAZB" %in% colnames(df)){riparian4<-select(df, LAZB)
  riparian<-bind_cols(riparian, riparian4)}else{df<-df}
  if("BEWR" %in% colnames(df)){riparian5<-select(df, BEWR)
  riparian<-bind_cols(riparian, riparian5)}else{df<-df}
  if("SPTO" %in% colnames(df)){riparian6<-select(df, SPTO)
  riparian<-bind_cols(riparian, riparian6)}else{df<-df}
  if("YEWA" %in% colnames(df)){riparian7<-select(df, YEWA)
  riparian<-bind_cols(riparian, riparian7)}else{df<-df}
  if("COYE" %in% colnames(df)){riparian8<-select(df, COYE)
  riparian<-bind_cols(riparian, riparian8)}else{df<-df}
  if("YBCH" %in% colnames(df)){riparian9<-select(df, YBCH)
  riparian<-bind_cols(riparian, riparian9)}else{df<-df}
  if("SOSP" %in% colnames(df)){riparian10<-select(df, SOSP)
  riparian<-bind_cols(riparian, riparian10)}else{df<-df}
  if("BHGR" %in% colnames(df)){riparian11<-select(df, BHGR)
  riparian<-bind_cols(riparian, riparian11)}else{df<-df}
  if("BLGR" %in% colnames(df)){riparian12<-select(df, BLGR)
  riparian<-bind_cols(riparian, riparian12)}else{df<-df}
  if("BUOR" %in% colnames(df)){riparian13<-select(df, BUOR)
  riparian<-bind_cols(riparian, riparian13)}else{df<-df}
  if("WAVI" %in% colnames(df)){riparian14<-select(df, WAVI)
  riparian<-bind_cols(riparian, riparian14)}else{df<-df}
  
  if(("Grassland" %in% choose_focal_group)){df<-grassland}
  if(("Oak Woodland" %in% choose_focal_group)){df<-oak}
  if(("Riparian" %in% choose_focal_group)){df<-riparian }
  
  df$Richness<-rowSums(df[,6:ncol(df)] != 0)
  
  ###########################NEED TO FIX THIS FUNCTION. Currently not filtering properly.
  return(df)
}

bird_function_library = c(bird_focal.graph_TotalAbundance, bird_focal.graph_RelativeAbundance, bird_species.total.richness,bird_species.relative.richness, bird_focal.TotalAbundance.Facet, bird_focal.RelativeAbundance.Facet)

names(bird_function_library) = c("Bird.Focal.Total.Abundance","Bird.Focal.Relative.Abundance", "Total.Species.Richness","Relative.Species.Richness.per.Visit", "Facet.Bird.Focal.TotalAbundance","Facet.Bird.Focal.RelativeAbundance" )

bird_map_variables = c("Richness", "Visits")
names(bird_map_variables) = c("Richness", "visits")

bird_list_functions = c(bird_birdcount.year, bird_wide.focal, bird_species.pointYear )

names(bird_list_functions)=c("Bird.count.by.year","Focal.Birds", "Bird.Species.List_pointYear")

listNames=names(bird_list_functions)
functionNames=names(bird_function_library)
functionNames2=append(functionNames,listNames)
functionNames2=append(functionNames2,"bird_map")

bird_list_help = c( "help_text/bird/bird_focal.graph_Abundance.txt", "help_text/bird/bird_focal.graph_RAbundance.txt",
                    "help_text/bird/bird_species.richness.txt","help_text/bird/bird_species.richnessR.txt",
                    "help_text/bird/bird_focal.Abundance.Facet.txt",
                    "help_text/bird/bird_focal.RelativeAbundance.Facet.txt",
                    "help_text/bird/bird_birdcount.year.txt","help_text/bird/bird_wide.focal.txt",
                    "help_text/bird/bird_species.pointYear.txt", 
                    "help_text/bird/bird.map.txt")
names(bird_list_help) = c(functionNames2)

