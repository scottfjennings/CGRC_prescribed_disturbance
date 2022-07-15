
# the code here is adapted for ACR use from the Point Blue Wrangler program
# wrangler code accessed July 2022

#veg code


functional.cover.table = function(main, lpi,
                                  transect,
                                  type = "absolute",
                                  invasives = FALSE,
                                  surveyyear = max(levels(as.factor(lpi$year))),
                                  includemeta = FALSE){
  
  library(reshape2)
  #CAPlants = RMN.functions:::CAPlantsv2
  #Invasives = RMN.functions:::Invasivesv1
  
  CAPlants = read.csv(here("data/helper_files/CAPlantsv2.csv"))
  Invasives = read.csv(here("data/helper_files/Invasivesv1.csv"))
  
  CAPlantsI<-CAPlants
  levels(CAPlantsI$FunGrp) <- c(levels(CAPlantsI$FunGrp), "Invasives")
  CAPlantsI$FunGrp[CAPlantsI$Accepted.Symbol %in% Invasives$USDA.code]<-"Invasives"
  lpi$year = as.factor(lpi$year)
  lpi = subset(lpi, subset = lpi$year %in% surveyyear)
  lpi = subset(lpi, subset = lpi$Transect.Name %in% transect)
  lpi$Tally = 1
  
  a = aggregate(lpi$Tally, list(lpi$pointyear), sum)
  names(a) = c("pointyear", "NumIndices")
  
  lpi.trim = subset(lpi, select=c("Transect.Name", "pointyear", "year", "Point.Id", "Canopy1",
                                  "Canopy2", "Canopy3","Top.Layer","Lower1","Lower2", "Lower3",
                                  "Lower4","Lower5","Lower6","Lower7",
                                  "Lower8","Lower9","Lower10","Soil.Surface"))
  
  longlpi = melt(lpi.trim, id=c("Transect.Name", "pointyear", "Point.Id", "year"))
  # Note, this step may give a warning, but it's
  names(longlpi) = c("Transect.Name", "pointyear", "Point.Id", "year", "Layer", "Spp")
  
  longlpi = subset(longlpi, select=c("Transect.Name", "pointyear", "Point.Id", "year", "Spp"))
  longlpi$Tally = 1
  
  
  
  longlpi$FunGrp = NULL
  if(isTRUE(invasives)){
    longlpi$FunGrp<-CAPlantsI$FunGrp[match(longlpi$Spp,CAPlantsI$Accepted.Symbol)]
  } else if(isFALSE(invasives)){
    longlpi$FunGrp = CAPlants$FunGrp[match(longlpi$Spp,CAPlants$Accepted.Symbol)]
  } else {stop("Argument for invasives not recognized")}
  
  
  Fun.Sum = aggregate(longlpi$Tally, by = list(longlpi$Transect.Name, longlpi$pointyear, longlpi$Point.Id, longlpi$year, longlpi$FunGrp), sum)
  names(Fun.Sum) = c("Transect" ,"pointyear", "Point.Id", "year", "FunGrp", "Count")
  
  
  Fun.Sum2 = reshape2::dcast(Fun.Sum, pointyear ~ FunGrp, value.var = c("Count"), sum)
  Fun.Sum2 = merge(Fun.Sum2, a, by = "pointyear")
  pointyears = as.data.frame(Fun.Sum[,1:4])
  pointyears = unique(pointyears)
  
  
  
  
  
  Fun.Sum3 = Fun.Sum2
  if(type == "absolute"){
    Fun.Sum3[,2:(ncol(Fun.Sum3) - 1)] = Fun.Sum3[,2:(ncol(Fun.Sum3) - 1)]/Fun.Sum3[,ncol(Fun.Sum3)] * 100
  } else if(type == "relative"){
    Fun.Sum3[,2:(ncol(Fun.Sum3) - 1)] = Fun.Sum3[,2:(ncol(Fun.Sum3) - 1)]/rowSums(Fun.Sum3[,2:(ncol(Fun.Sum3) - 1)], na.rm = TRUE) * 100
  } else {stop("Argument for type not recognized")}
  
  Fun.Sum3 = merge(pointyears, Fun.Sum3, by = "pointyear", all = FALSE)
  
  if(includemeta){
    return(Fun.Sum3)
  }else{
    Fun.Sum3$pointyear = NULL
    Fun.Sum3$Transect = NULL
    Fun.Sum3[,3:ncol(Fun.Sum3)] = round(Fun.Sum3[,3:ncol(Fun.Sum3)], 2)
    return(Fun.Sum3)
  }
  
  
}



cover.sum.plot = function(main, lpi, releve,
                          transect,
                          surveyyear = c(levels(as.factor(lpi$year)), levels(as.factor(checklist$year))),
                          choose.variable = c("SpeciesRichness", "Litter", "Thatch", "BareGround",
                                              "Trees", "Shrubs"),
                          plot.names = c("Species Richness", "Litter", "Thatch", "Bare Ground",
                                         "Trees", "Shrubs"),
                          multiple_years = TRUE,
                          barcolors = c("olivedrab", "steelblue3", "mediumpurple4", "orange3", "tan2"),
                          legend = TRUE,
                          legendname = "Year",
                          xlab = "Point ID",
                          ylab = "Value",
                          xangle = 45
)
{
  main2=main
  datasum = cover.summary(main = main2, lpi = lpi, releve = releve, transect = transect,
                          choose.variable = choose.variable, surveyyear = surveyyear, rounded = FALSE)
  datasum$NumIndices = NULL
  
  if(isFALSE(multiple_years)){datasum = subset(datasum, subset = datasum$year == max(datasum$year))}
  
  datasum_melt = melt(datasum, id = c("pointyear", "PointId", "year"))
  levels(datasum_melt$variable) = plot.names
  
  p = ggplot(datasum_melt, aes(x = PointId, y = value, fill = year)) +
    geom_bar(stat = "identity",position = position_dodge(), color = "black") +
    scale_fill_manual(values= barcolors, name = legendname) +
    facet_wrap(~variable) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = xangle, hjust = 1), legend.position = ifelse(legend, "right", "none")) +
    xlab(xlab) +
    ylab(ylab)
  
  
  return(p)
  
}

species.cover.tables = function(main, lpi,
                                transect,
                                type = "absolute",
                                surveyyear = max(levels(as.factor(lpi$year))))
{
  
  lpi$Tally = 1
  lpi$year = as.factor(lpi$year)
  lpi = subset(lpi, subset = lpi$year %in% surveyyear)
  lpi = subset(lpi, subset = lpi$Transect.Name %in% transect)
  
  
  a = aggregate(lpi$Tally, list(lpi$pointyear), sum)
  names(a) = c("pointyear", "NumIndices")
  
  lpi.trim = subset(lpi, select=c("pointyear", "year", "Point.Id", "Canopy1", "Canopy2", "Canopy3",
                                  "Top.Layer","Lower1","Lower2", "Lower3", "Lower4","Lower5","Lower6","Lower7",
                                  "Lower8","Lower9","Lower10","Soil.Surface"))
  
  longlpi = melt(lpi.trim, id=c("pointyear", "Point.Id", "year"))
  # Note, this step may give a warning, but it's
  names(longlpi) = c("pointyear", "Point.Id", "year", "Layer", "Spp")
  
  longlpi = subset(longlpi, select=c("pointyear", "Point.Id", "year", "Spp"))
  longlpi$Tally = 1
  
  
  newlpi = dcast(longlpi, pointyear~Spp,value.var=c("Tally"), sum)
  newlpi = merge(newlpi, a, by="pointyear")
  pointyear = newlpi$pointyear
  firstdrops = c("Var.2", "NA", "NOPLANT", "M", "L", "EM", "AM", "R", "WL", "S")
  newlpi = newlpi[,!(names(newlpi) %in% firstdrops)]
  
  if(type == "absolute"){
    newlpi.relative = (newlpi[,3:ncol(newlpi)-1]/newlpi$NumIndices) *100
  } else if(type == "relative"){
    newlpi.relative = newlpi[,3:ncol(newlpi)-1]/rowSums(newlpi[,3:ncol(newlpi)-1], na.rm=TRUE) * 100
  } else{ stop("Argument for type not recognized") }
  
  newlpi.relative = cbind(pointyear, newlpi.relative)
  
  ## Almost there, just some tidying
  
  drops = c("Var.2", "NA", "NumIndices", "NOPLANT", "UNKNWN", "M", "L", "EM", "AM", "R", "WL", "S")
  newlpi.relative = newlpi.relative[,!(names(newlpi.relative) %in% drops)]
  newlpi = newlpi[,!(names(newlpi) %in% drops)]
  
  t.lpirelative = as.data.frame(t(newlpi.relative[,-1]))
  colnames(t.lpirelative) = newlpi.relative$pointyear
  
  t.lpirelative$mean_cover = rowMeans(t.lpirelative, na.rm = TRUE)
  
  rownames(t.lpirelative) = colnames(newlpi.relative)[-1]
  
  t.lpirelative$species = rownames(t.lpirelative)
  t.lpirelative = arrange(t.lpirelative, desc(mean_cover))
  
  
  t.lpirelative2 = t.lpirelative[,c(ncol(t.lpirelative), 1:(ncol(t.lpirelative) - 1))]
  
  t.lpirelative2[,2:ncol(t.lpirelative2)] = round(t.lpirelative2[,2:ncol(t.lpirelative2)], 2)
  
  return(t.lpirelative2)
  
}

species.list = function(main, lpi, releve,
                        transect,
                        surveyyear = levels(as.factor(lpi$year))){
  #CAPlants = RMN.functions:::CAPlantsv2
  #Invasives = RMN.functions:::Invasivesv1
  CAPlants = read.csv("data_required/CAPlantsv2.csv")
  Invasives = read.csv("data_required/Invasivesv1.csv")
  
  lpi$Point.Dir = paste(lpi$pointyear, lpi$Direction, sep = "-")
  lpi$year = as.factor(lpi$year)
  releve$year = as.factor(releve$year)
  
  lpi = subset(lpi, subset = lpi$year %in% surveyyear)
  releve = subset(releve, subset = releve$year %in% surveyyear)
  
  lpi = subset(lpi, subset = Transect.Name %in% transect)
  releve = subset(releve, subset = Transect.Name %in% transect)
  
  layers = subset(lpi, select=c("pointyear",  "Top.Layer", "Lower1", "Lower2",
                                "Lower3", "Lower4", "Lower5", "Lower6", "Lower7",
                                "Lower8", "Lower9", "Soil.Surface"))
  
  longlpi = melt(layers, id="pointyear")
  names(longlpi) = c("pointyear", "Layer", "Spp")
  releve$Layer = "extras"
  extras = subset(releve, select=c("pointyear", "Layer", "USDA.Code"))
  names(extras) = c("pointyear", "Layer", "Spp")
  
  both = rbind(longlpi, extras)
  both$Spp = as.factor(both$Spp)
  
  #Remove the non-spp
  Exclude = c("", "2FA", "2FORB", "2FP", "2GA", "2GP", "2LICHN",
              "2LTR", "2LTRWS",  "2PLANT",  "2W", "NOPLANT", "L", "WL")
  both = both[!(both$Spp %in% Exclude),]
  both = droplevels(both)
  
  
  ##add plants that are in the soil surface hit to this that##
  
  richness = NULL
  both1 = both
  both1$Layer = NULL
  both1 = both1[!duplicated(both1),]
  richness = both1
  richness = merge(richness, CAPlants, by.x = "Spp", by.y = "Accepted.Symbol", all.x = TRUE, all.y = FALSE)
  richness2 = aggregate(richness$Spp, list(richness$pointyear), length)
  names(richness2) = c("pointyear", "NumSpp")
  
  SppList = subset(richness, select=c("Spp", "Symbol", "Scientific.Name", "Common.Name","Genus","Family","Family.Common.Name",
                                      "Duration","Growth.Habit", "Native.Status" ,"Invasive", "FunGrp"))
  
  SppList = subset(SppList, !duplicated(SppList$Spp))
  
  levels(CAPlants$FunGrp) = c(levels(CAPlants$FunGrp), "Invasives")
  
  CAPlants$FunGrp[CAPlants$Accepted.Symbol %in% Invasives$USDA.code] = "Invasives"
  
  #########################################################
  ######
  
  SppList$Native.Status = substr(SppList$Native.Status, 1, 7)
  
  ##### Make a Native/Invasive category
  SppList$Provenance = NULL
  SppList$Provenance = replace(SppList$Provenance, SppList$Native.Status == "L48 (N)", "Native")
  SppList$Provenance = replace(SppList$Provenance, SppList$Native.Status == "L48 (I)", "Non-native")
  
  SppList2 = subset(SppList, select=c("Symbol", "Scientific.Name", "Common.Name", "Family", "Provenance", "FunGrp"))
  
  
  return(SppList2)
  
}


species.cover.table = function(main, lpi,
                               transect,
                               type = "absolute",
                               surveyyear = max(levels(as.factor(lpi$year))))
{
  library(reshape2)
  library(plyr)
  lpi$Tally = 1
  lpi$year = as.factor(lpi$year)
  lpi = subset(lpi, subset = lpi$year %in% surveyyear)
  lpi = subset(lpi, subset = lpi$Transect.Name %in% transect)
  
  a = stats::aggregate(lpi$Tally, list(lpi$pointyear), sum)
  names(a) = c("pointyear", "NumIndices")
  
  lpi.trim = subset(lpi, select=c("pointyear", "year", "Point.Id", "Canopy1", "Canopy2", "Canopy3",
                                  "Top.Layer","Lower1","Lower2", "Lower3", "Lower4","Lower5","Lower6","Lower7",
                                  "Lower8","Lower9","Lower10","Soil.Surface"))
  
  longlpi = reshape2::melt(lpi.trim, id=c("pointyear", "Point.Id", "year"))
  # Note, this step may give a warning, but it's
  names(longlpi) = c("pointyear", "Point.Id", "year", "Layer", "Spp")
  
  longlpi = subset(longlpi, select=c("pointyear", "Point.Id", "year", "Spp"))
  longlpi$Tally = 1
  
  
  newlpi = reshape2::dcast(longlpi, pointyear~Spp,value.var=c("Tally"), sum)
  newlpi = merge(newlpi, a, by="pointyear")
  pointyear = newlpi$pointyear
  firstdrops = c("Var.2", "NA", "NOPLANT", "M", "L", "EM", "AM", "R", "WL", "S")
  newlpi = newlpi[,!(names(newlpi) %in% firstdrops)]
  
  if(type == "absolute"){
    newlpi.relative = (newlpi[,3:ncol(newlpi)-1]/newlpi$NumIndices) *100
  } else if(type == "relative"){
    newlpi.relative = newlpi[,3:ncol(newlpi)-1]/rowSums(newlpi[,3:ncol(newlpi)-1], na.rm=TRUE) * 100
  } else{ stop("Argument for type not recognized") }
  
  newlpi.relative = cbind(pointyear, newlpi.relative)
  
  ## Almost there, just some tidying
  
  drops = c("Var.2", "NA", "NumIndices", "NOPLANT", "UNKNWN", "M", "L", "EM", "AM", "R", "WL", "S")
  newlpi.relative = newlpi.relative[,!(names(newlpi.relative) %in% drops)]
  newlpi = newlpi[,!(names(newlpi) %in% drops)]
  
  t.lpirelative = as.data.frame(t(newlpi.relative[,-1]))
  colnames(t.lpirelative) = newlpi.relative$pointyear
  
  
  t.lpirelative$mean_cover = rowMeans(t.lpirelative, na.rm = TRUE)
  rownames(t.lpirelative) = colnames(newlpi.relative)[-1]
  t.lpirelative$species = rownames(t.lpirelative)
  t.lpirelative = arrange(t.lpirelative, dplyr::desc(mean_cover))
  
  t.lpirelative2 = t.lpirelative[,c(ncol(t.lpirelative), 1:(ncol(t.lpirelative) - 1))]
  t.lpirelative2[,2:ncol(t.lpirelative2)] = round(t.lpirelative2[,5:ncol(t.lpirelative2)], 2)
  return(t.lpirelative2)
  
  
  
}



points.cover.plot = function(main, lpi, transect,
                             type = "absolute",
                             surveyyear = levels(as.factor(lpi$year)),
                             invasives = FALSE,
                             legend.position = "top",
                             pallete = "YlGnBu",
                             x.angle = 45){
  
  fct = functional.cover.table(main=main, lpi = lpi, type = type, transect = transect, invasives = invasives, surveyyear = surveyyear, includemeta = TRUE)
  
  
  abs<- fct[,colnames(fct) != "NumIndices"]
  abs<-melt(abs, id= c("pointyear", "Transect", "Point.Id", "year"))
  # Note, this step may give a warning, but it's okay
  names(abs)<-c("pointyear", "Transect", "Point.Id", "year", "Type", "Cover")
  
  absolute = ggplot(abs, aes(x = year, y = Cover))
  
  p = absolute + geom_col(aes(fill = Type), position = position_stack(reverse = TRUE)) +
    # coord_flip() +
    theme(legend.position = legend.position) +
    theme(legend.text = element_text(size=8)) +
    theme(legend.title = element_blank()) +
    scale_fill_manual(values = c("olivedrab", "steelblue3", "orange3",
                                 "mediumpurple4", "firebrick4", "tan", "mistyrose", "yellowgreen")) +
    theme(axis.text.x = element_text(angle = x.angle, hjust = 1)) +
    facet_wrap(~Point.Id) +
    theme_bw()
  # coord_flip()
  return(p)
}

functional.cover.change.plot = function(main, lpi,
                                        transect,
                                        invasives = FALSE,
                                        surveyyear = levels(as.factor(lpi$year)),
                                        background = TRUE,
                                        type = "absolute",
                                        xlab = "Functional Group",
                                        ylab = paste("Cover Change",
                                                     " ",
                                                     min(levels(as.factor(surveyyear))),
                                                     "-",
                                                     max(levels(as.factor(surveyyear))),
                                                     sep = ""),
                                        legendtitle = "Ranch",
                                        legendnames = c(paste(transect, collapse = " "), "Others"),
                                        boxcolors = c("black","gray")
){
  
  ranchlpi = subset(lpi, subset = lpi$Transect.Name %in% transect)
  ranchlpi$year = droplevels(as.factor(ranchlpi$year))
  surveyyear = surveyyear[surveyyear %in% ranchlpi$year]
  
  main2=main
  
  coveryear = functional.cover.change.table(main=main2,lpi = lpi,
                                            transect = transect,
                                            type = type,
                                            invasives = invasives,
                                            surveyyear = surveyyear,
                                            casted = FALSE
  )
  masked = coveryear
  if(!background){masked = subset(masked, subset = Transect.x %in% transect)}
  
  masked$Transect.x = as.character(replace(as.character(masked$Transect.x),
                                           masked$Transect.x != transect, values = "zzzz"))
  masked$Transect.x = as.character(replace(as.character(masked$Transect.x),
                                           masked$Transect.x == transect, values = "aaaa"))
  if(!("aaaa" %in% masked$Transect.x)){stop("This ranch does not contain multiple surveys in the years selected")}
  
  
  
  c = ggplot(masked, aes(x = Type, y = change, color = Transect.x)) +
    geom_boxplot() +
    scale_color_manual(name = legendtitle, values = boxcolors, labels = legendnames) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    xlab(label = xlab) +
    ylab(label = ylab) +
    geom_hline(yintercept = 0, linetype = 'dotted')
  
  return(c)
  
  
}


functional.cover.change.table = function(main, lpi,
                                         transect,
                                         invasives = FALSE,
                                         surveyyear = levels(as.factor(lpi$year)),
                                         type = "absolute",
                                         casted = TRUE)
  
{
  
  main2=main
  cov = functional.cover.table(main2, lpi=lpi, type = type, transect = transect,
                               surveyyear = surveyyear, invasives = invasives, includemeta = TRUE)
  cov$NumIndices = NULL
  cov = melt(cov, id = c("Transect", "pointyear", "Point.Id", "year"))
  names(cov)<-c("Transect", "pointyear", "Point.Id", "year", "Type", "Cover")
  
  cov$year = droplevels(as.factor(cov$year))
  cov$Type = as.factor(cov$Type)
  
  
  d2 = subset(cov, subset = year == max(levels(year)))
  d1 = subset(cov, subset = year == min(levels(year)))
  
  coveryear = merge(d1, d2, by = c("Point.Id", "Type"))
  coveryear$change = coveryear$Cover.y - coveryear$Cover.x
  
  if(casted){coveryear = dcast(coveryear, Point.Id ~ Type, value.var = "change")}
  
  return(coveryear)
  
}


functional.cover.plot = function(main, lpi,
                                 type = "absolute",
                                 transect,
                                 background = TRUE,
                                 invasives = FALSE,
                                 surveyyear = max(levels(as.factor(lpi$year))),
                                 xlab = "Functional Group",
                                 ylab = "Percent Cover",
                                 legendtitle = "Ranch",
                                 legendnames = c(paste(transect, collapse = " "), "Others"),
                                 boxcolors = c("black","gray")){
  main2=main
  
  cov = functional.cover.table(main=main2, lpi=lpi, type = type, transect = transect,
                               surveyyear = surveyyear, invasives = invasives,
                               includemeta = TRUE)
  if(!background){cov = subset(cov, subset = Transect %in% transect)}
  cov$NumIndices = NULL
  
  
  cov = melt(cov, id = c("Transect" ,"pointyear", "Point.Id", "year"))
  
  names(cov)<-c("Transect", "pointyear", "Point.Id", "year", "Type", "Cover")
  masked = cov
  masked$Transect = as.character(replace(as.character(masked$Transect),
                                         masked$Transect != transect, values = "zzzz"))
  masked$Transect = as.character(replace(as.character(masked$Transect),
                                         masked$Transect == transect, values = "aaaa"))
  
  masked$year = as.factor(masked$year)
  masked$Type = as.factor(masked$Type)
  
  
  cover_plot = ggplot(masked, aes(x = Type, y = Cover, color = Transect)) +
    geom_boxplot() +
    scale_color_manual(name = legendtitle, values = boxcolors, labels = legendnames) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    xlab(xlab) +
    ylab(ylab)
  
  return(cover_plot)
  
}



cover.summary = function(main, lpi, releve,
                         transect,
                         surveyyear = c(levels(as.factor(lpi$year)), levels(as.factor(releve$year))),
                         choose.variable = c("SpeciesRichness", "Litter", "Thatch",
                                             "BareGround", "Trees", "Shrubs"),
                         rounded = TRUE){
  
  
  
  
  lpi$year = as.factor(lpi$year)
  
  releve$year = as.factor(releve$year)
  
  lpi = subset(lpi, Transect.Name %in% transect)
  
  releve = subset(releve, Transect.Name %in% transect)
  releve = subset(releve, year %in% surveyyear)
  lpi = subset(lpi, year %in% surveyyear)
  
  
  covsum<- plyr::ddply(releve, .(Vegetation.Type, pointyear), summarise, Percent.Cover=sum(Percent.Cover), .drop=F)
  
  shrubs<-subset(covsum, subset=covsum$Vegetation.Type == "shrubs")
  colnames(shrubs) = c("covertype", "pointyear", "Shrubcover")
  shrubs = subset(shrubs, select = c("pointyear", "Shrubcover"))
  trees<-subset(covsum, subset=covsum$Vegetation.Type == "trees")
  colnames(trees) = c("covertype", "pointyear", "Treecover")
  trees = subset(trees, select = c("pointyear", "Treecover"))
  
  lpi$BG<-0
  lpi$BG <- replace(lpi$BG,
                    lpi$Top.Layer == "NOPLANT" &
                      lpi$Lower1 == "" & lpi$Lower2 == "" & lpi$Lower3 == "" & lpi$Soil.Surface == "S", 1)
  
  #Aggregate Bare Grounds By Point
  BareGround<-aggregate(lpi$BG,list(lpi$pointyear),sum)
  names(BareGround)<-c("pointyear", "BareGround")
  
  
  #Calculate Litter
  #First, rename all of the things that could be litter into just one term
  lpi$Litter<-0
  lpi$Litter <- replace(lpi$Litter, lpi$Lower1 == "L", 1)
  lpi$Litter <- replace(lpi$Litter, lpi$Lower2 == "L", 1)
  lpi$Litter <- replace(lpi$Litter, lpi$Lower3 == "L", 1)
  lpi$Litter <- replace(lpi$Litter, lpi$Lower4 == "L", 1)
  lpi$Litter <- replace(lpi$Litter, lpi$Lower5 == "L", 1)
  lpi$Litter <- replace(lpi$Litter, lpi$Lower6 == "L", 1)
  lpi$Litter <- replace(lpi$Litter, lpi$Lower7 == "L", 1)
  lpi$Litter <- replace(lpi$Litter, lpi$Lower8 == "L", 1)
  lpi$Litter <- replace(lpi$Litter, lpi$Lower9 == "L", 1)
  lpi$Litter <- replace(lpi$Litter, lpi$Lower10 == "L", 1)
  lpi$Litter <- replace(lpi$Litter, lpi$Soil.Surface == "L", 1)
  lpi$Litter <- replace(lpi$Litter, lpi$Soil.Surface == "EM", 1)
  
  Litter<-aggregate(lpi$Litter,list(lpi$pointyear),sum)
  names(Litter)<-c("pointyear", "Litter")
  
  lpi$Thatch<-1
  lpi$Thatch<-replace(lpi$Thatch, is.na(lpi$Thatch.Indices.Lower), 0)
  lpi$Thatch<-replace(lpi$Thatch, lpi$Thatch.Indices.Lower == "", 0)
  lpi$Thatch<-replace(lpi$Thatch, is.na(lpi$Thatch.Top.Layer), 0)
  lpi$Thatch<-replace(lpi$Thatch, lpi$Thatch.Top.Layer == "", 0)
  
  Thatch<-aggregate(lpi$Thatch,list(lpi$pointyear),sum, na.rm = TRUE)
  names(Thatch)<-c("pointyear", "Thatch")
  
  
  lpi$Point.Dir<-paste(lpi$pointyear, lpi$Direction, sep="-")
  
  layers<-subset(lpi, select=c("pointyear",  "Top.Layer", "Lower1", "Lower2",
                               "Lower3", "Lower4", "Lower5", "Lower6", "Lower7"))
  
  longlpi<-melt(layers, id="pointyear")
  names(longlpi)<-c("pointyear", "Layer", "Spp")
  releve$Layer<-"extras"
  extras<-subset(releve, select=c("pointyear", "Layer", "USDA.Code"))
  names(extras)<-c("pointyear", "Layer", "Spp")
  
  both<-rbind(longlpi, extras)
  both$Spp<-as.factor(both$Spp)
  
  #Remove the non-spp
  Exclude<-c("", "2FA", "2FORB", "2FP", "2GA", "2GP", "2LICHN",
             "2LTR", "2LTRWS",  "2PLANT",  "2W", "NOPLANT", "L", "WL")
  both <- both[!(both$Spp %in% Exclude),]
  both<-droplevels(both)
  
  
  ##add plants that are in the soil surface hit to this that##
  #CAPlants = RMN.functions:::CAPlantsv2
  CAPlants = read.csv("data_required/CAPlantsv2.csv")
  richness<-NULL
  both1 = both
  both1$Layer = NULL
  both1 = both1[!duplicated(both1),]
  richness = both1
  richness<-merge(richness, CAPlants, by.x = "Spp", by.y = "Accepted.Symbol", all.x = TRUE, all.y = FALSE)
  richness2<-aggregate(richness$Spp, list(richness$pointyear), length)
  names(richness2)<-c("pointyear", "NumSpp")
  
  
  #### Now bring everything together
  main = read.csv("data/Visit.csv") %>% add.pointyear()
  
  Pointyears = subset(main, select = c(pointyear, PointId, year))
  
  data.summary = Pointyears
  data.summary = merge(data.summary, richness2, by = "pointyear", all.x=TRUE)
  data.summary<-merge(data.summary, Litter, by="pointyear", all.x=TRUE)
  data.summary<-merge(data.summary, Thatch,by="pointyear", all.x=TRUE)
  data.summary<-merge(data.summary, BareGround,by="pointyear", all.x=TRUE)
  data.summary<-merge(data.summary, trees,by="pointyear", all.x=TRUE)
  data.summary<-merge(data.summary, shrubs,by="pointyear", all.x=TRUE)
  
  
  # IF you have transect where Not every point has all 100 subsamples,
  # Then run this to correct for reduced effort
  
  lpi$Tally<-1
  indices<-aggregate(lpi$Tally, list(lpi$pointyear), sum)
  names(indices)<-c("pointyear", "NumIndices")
  
  data.summary<-merge(data.summary, indices, by="pointyear")
  data.summary$Litter<-(data.summary$Litter/data.summary$NumIndices)*100
  data.summary$BareGround<-(data.summary$BareGround/data.summary$NumIndices)*100
  data.summary$Thatch = (data.summary$Thatch/data.summary$NumIndices)*100
  
  #select variables you want to
  if(!("SpeciesRichness" %in% choose.variable)){data.summary$NumSpp = NULL}
  if(!("Litter" %in% choose.variable)){data.summary$Litter = NULL}
  if(!("Thatch" %in% choose.variable)){data.summary$Thatch = NULL}
  if(!("BareGround" %in% choose.variable)){data.summary$BareGround = NULL}
  if(!("Trees" %in% choose.variable)){data.summary$Treecover = NULL}
  if(!("Shrubs" %in% choose.variable)){data.summary$Shrubcover = NULL}
  
  if(rounded){data.summary[,5:ncol(data.summary)] = round(data.summary[,5:ncol(data.summary)], 2)}
  
  return(data.summary)
  
}

functional.cover.plot.year<-function(main, lpi, type = "absolute", transect,
                                     invasives = FALSE, 
                                     surveyyear = max(levels(as.factor(lpi$year)))){
  lpi = subset(lpi, Transect.Name %in% transect)
  
  lpi = subset(lpi, year %in% surveyyear)  
  CAPlants = read.csv("data_required/CAPlantsv2.csv")
  print(levels(as.factor(CAPlants$FunGrp)))
  Invasives = read.csv("data_required/Invasivesv1.csv")
  print(str(Invasives))
  CAPlantsI = CAPlants
  #add Invasives as a level
  levels(CAPlantsI$FunGrp)
  levels(CAPlantsI$FunGrp) <- c(levels(as.factor(CAPlantsI$FunGrp)), "Invasives")
  print(levels(CAPlantsI$FunGrp))
  #add Invasives as functional group by its code
  
  CAPlantsI$FunGrp[CAPlantsI$Accepted.Symbol %in% Invasives$USDA.code]<-"Invasives"
  print(table(CAPlantsI$FunGrp))
  lpi$year = as.factor(lpi$year)
  
  lpi$Tally = 1
  
  
  a = aggregate(lpi$Tally, list(lpi$pointyear), sum)
  names(a) = c("pointyear", "NumIndices")
  
  lpi.trim = subset(lpi, select=c("Transect.Name", "pointyear", "year", "Point.Id", "Canopy1",
                                  "Canopy2", "Canopy3","Top.Layer","Lower1","Lower2", "Lower3",
                                  "Lower4","Lower5","Lower6","Lower7",
                                  "Lower8","Lower9","Lower10","Soil.Surface"))
  
  longlpi = melt(lpi.trim, id=c("Transect.Name", "pointyear", "Point.Id", "year"))
  
  names(longlpi) = c("Transect.Name", "pointyear", "Point.Id", "year", "Layer", "Spp")
  
  longlpi$Tally = 1
  
  longlpi$FunGrp = NULL
  #if true for invasives
  #needs a new code for invasives
  longlpi$FunGrp<-CAPlantsI$FunGrp[match(longlpi$Spp,CAPlantsI$Accepted.Symbol)]
  #if false
  #other r script
  print(table(longlpi$FunGrp))
  
  Fun.Sum = aggregate(longlpi$Tally, by = list(longlpi$Transect.Name, longlpi$pointyear, longlpi$Point.Id, longlpi$year, longlpi$FunGrp), sum)
  names(Fun.Sum) = c("Transect" ,"pointyear", "Point.Id", "year", "FunGrp", "Count")
  
  print(head(Fun.Sum))
  print('-1')
  Fun.Sum2 = dcast(Fun.Sum, pointyear ~ FunGrp, value.var = c("Count"), sum)
  print('-2')
  print(head(Fun.Sum2))
  print(str(Fun.Sum2$pointyear))
  
  Fun.Sum2$year = strsplit(Fun.Sum2$pointyear, " - ")[1]
  print('wow')
  print(head(Fun.Sum2))
  print(str(Fun.Sum2$pointyear))
  getyear=function(x){
    strsplit(x,"[ - ]")[[1]][3]
  }
  getpoint=function(x){
    strsplit(x,"[ - ]")[[1]][1]
  }
  gettransect=function(x){
    strsplit(x,"[-]")[[1]][1]
  }
  #Fun.Sum$year = result_2 <- strsplit(v,"[ - ]")[[1]][3]
  Fun.Sum2$year = as.numeric(sapply(Fun.Sum2$pointyear,getyear))
  print('maybe')
  print(head(Fun.Sum2))
  Fun.Sum2$point = as.character(sapply(Fun.Sum2$pointyear,getpoint))
  Fun.Sum2$transect = as.character(sapply(Fun.Sum2$point,gettransect))
  print('hehe')
  print(Fun.Sum2)
  PointCountTable= as.data.frame(table(Fun.Sum2$point))
  #Visited more than 1
  
  pointsOverOneSample = filter(PointCountTable, Freq > 1)
  names(pointsOverOneSample)<-c('point', "freq")
  
  ResampledPoints = inner_join(pointsOverOneSample, Fun.Sum2, by ="point")
  subResampledPoints =select(ResampledPoints, -"freq")
  subResampledPoints = merge(subResampledPoints, a, by = "pointyear")
  
  #worried about weighting values wrong. 
  #different than no invasives.
  print(head(subResampledPoints))
  print('check')
  if (invasives == FALSE){
    col_order <- c("transect", "NumIndices", "point","year", "pointyear", "Annual Forb","Annual Grass" ,"Legumes", "Perennial Forb", "Perennial Grass", "SedgesRushes", "ShrubsTrees"  )
    #my_data2 <- my_data[, col_order]
    #my_data2
    subResPoints_reordered = subResampledPoints[,col_order]
  }else{
    if("Invasives" %in% colnames(subResampledPoints))
    {
      col_order <- c("transect", "NumIndices", "point","year", "pointyear", "Annual Forb","Annual Grass" ,"Legumes", "Perennial Forb", "Perennial Grass", "SedgesRushes", "ShrubsTrees" , "Invasives" )
    }else{
      col_order <- c("transect", "NumIndices", "point","year", "pointyear", "Annual Forb","Annual Grass" ,"Legumes", "Perennial Forb", "Perennial Grass", "SedgesRushes", "ShrubsTrees"  )
      
    }
    
    subResPoints_reordered = subResampledPoints[,col_order]
  }
  print("yieie")
  print(head(subResPoints_reordered))
  #subResPoints_reordered[,5:(ncol(subResPoints_reordered) - 1)] = subResPoints_reordered[,5:(ncol(subResPoints_reordered) - 1)]/subResPoints_reordered[ncol(subResPoints_reordered)] * 100
  #Fun.Sum3_test$year = as.factor(Fun.Sum3_test$year)
  print('1')
  print(subResPoints_reordered)
  subPoints_clean = all_Tidynames(subResPoints_reordered)
  print("2")
  print(subPoints_clean)
  subPoints_clean$year = as.factor(subPoints_clean$year)
  print('3')
  print(subPoints_clean)
  if(invasives == FALSE){
    summarizedTable_sd = subPoints_clean %>% 
      dplyr::group_by(year,transect) %>% 
      dplyr::summarise(Annual.Forb_sd = sd(Annual.Forb),Annual.Grass_sd = sd(Annual.Grass) , Legumes_sd=sd(Legumes), Perennial.Forb_sd=sd(Perennial.Forb), Perennial.Grass_sd=sd(Perennial.Grass), SedgesRushes_sd=sd(SedgesRushes),  ShrubsTrees_sd=sd(ShrubsTrees))
    print('4')
    
    summarizedTable = subPoints_clean %>% 
      dplyr::group_by(year,transect) %>% 
      dplyr::summarise(Annual.Forb = mean(Annual.Forb),Annual.Grass = mean(Annual.Grass) , Legumes=mean(Legumes), Perennial.Forb=mean(Perennial.Forb), Perennial.Grass=mean(Perennial.Grass), SedgesRushes=mean(SedgesRushes),  ShrubsTrees=mean(ShrubsTrees))
    
  }else{
    if("Invasives" %in% colnames(subPoints_clean)){
      
      summarizedTable_sd = subPoints_clean %>% 
        dplyr::group_by(year,transect) %>% 
        dplyr::summarise(Annual.Forb_sd = sd(Annual.Forb),Annual.Grass_sd = sd(Annual.Grass) , Legumes_sd=sd(Legumes), Perennial.Forb_sd=sd(Perennial.Forb), Perennial.Grass_sd=sd(Perennial.Grass), SedgesRushes_sd=sd(SedgesRushes), ShrubsTrees_sd=sd(ShrubsTrees), Invasives_sd=sd(Invasives))
      
      summarizedTable = subPoints_clean %>% 
        dplyr::group_by(year,transect) %>% 
        dplyr::summarise(Annual.Forb = mean(Annual.Forb),Annual.Grass = mean(Annual.Grass) , Legumes=mean(Legumes), Perennial.Forb=mean(Perennial.Forb), Perennial.Grass=mean(Perennial.Grass), SedgesRushes=mean(SedgesRushes), ShrubsTrees=mean(ShrubsTrees), Invasives=mean(Invasives))
    }else{
      summarizedTable_sd = subPoints_clean %>% 
        dplyr::group_by(year,transect) %>% 
        dplyr::summarise(Annual.Forb_sd = sd(Annual.Forb),Annual.Grass_sd = sd(Annual.Grass) , Legumes_sd=sd(Legumes), Perennial.Forb_sd=sd(Perennial.Forb), Perennial.Grass_sd=sd(Perennial.Grass), SedgesRushes_sd=sd(SedgesRushes),  ShrubsTrees_sd=sd(ShrubsTrees))
      
      summarizedTable = subPoints_clean %>% 
        dplyr::group_by(year,transect) %>% 
        dplyr::summarise(Annual.Forb = mean(Annual.Forb),Annual.Grass = mean(Annual.Grass) , Legumes=mean(Legumes), Perennial.Forb=mean(Perennial.Forb), Perennial.Grass=mean(Perennial.Grass), SedgesRushes=mean(SedgesRushes),  ShrubsTrees=mean(ShrubsTrees))
    } 
    
    
  } 
  
  
  #add in se
  #merge or join values together then use ggplot error bar to add values to graph
  
  tableReshaped_sd<-melt(summarizedTable_sd, id= c("year", "transect"))
  names(tableReshaped_sd)<-c("Year", "Transect", "Type", "sd_value")
  
  str(tableReshaped_sd$sd_value)
  tableReshaped<-melt(summarizedTable, id= c("year", "transect"))
  names(tableReshaped)<-c("Year", "Transect", "Type", "Cover")
  
  
  tableReshaped_bind = cbind(tableReshaped, tableReshaped_sd)
  tableReshared_test = tableReshaped_bind[,c(1,2,3,4, 8)]
  
  
  
  ggplot(tableReshared_test, aes(x = Year, y = Cover))+ 
    geom_col(aes(fill = Type), position = position_stack(reverse = TRUE)) +
    coord_flip() +
    theme(legend.position = "top") +
    theme(legend.text = element_text(size=8)) +
    theme(legend.title = element_blank()) +
    scale_fill_manual(values = c("olivedrab", "steelblue3", "orange3",
                                 "mediumpurple4", "firebrick4", "tan", "mistyrose", "yellowgreen")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    facet_wrap(~Transect) +
    theme_bw()
  
  
}

#functional.cover.plot.year(main = main, lpi = lpi, transect = c("BRRA"),surveyyear = c(2016,2017), invasives = TRUE)


