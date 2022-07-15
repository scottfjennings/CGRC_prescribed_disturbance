#soil code

add.soilcolumns = function(data, inf_diameter = 15.2, bd_diameter = 5.2, volume = 450, bd_height = 7.5){
  
  
  
  #interpret date, isolate year as column
  data$DATE = read.date(data$Date)
  data$YEAR = year(data$DATE)
  data$SURVEY = as.factor(paste(data$Point.Name,"-",data$YEAR,sep=""))
  
  #
  ## add some simple data about our measurements
  data$Ring.Infiltrometer.Diameter[data$Ring.Infiltrometer.Diameter == 0.0]<-inf_diameter
  data$Bulk.Density.Diameter[data$Bulk.Density.Diameter == 0.0]<-bd_diameter
  data$Water.Volume[data$Water.Volume ==0.0]<-volume
  data$Bulk.Density.Height[data$Bulk.Density.Height ==0.0]<-bd_height
  data$Total.Volume<-pi*((data$Bulk.Density.Diameter/2)^2)*data$Bulk.Density.Height
  data$Bulk.Density.Rock.Vol[is.na(data$Bulk.Density.Rock.Vol)] = 0
  data$Bulk.Density<-(data$Bulk.Density.Dry.Wt/(data$Total.Volume-data$Bulk.Density.Rock.Vol))
  data[,"Bulk.Density"][data[,"Bulk.Density"] <= 0] <- NA
  
  #convert water infiltration time to minutes in base-ten decimal
  data[,"Water.Infiltration.Time.1"][data[,as.character("Water.Infiltration.Time.1")] == ""] <- "00:00:00"
  data$ISec1<-NULL
  data$ISec1<-toSeconds(as.character(data$Water.Infiltration.Time.1))
  data$ISec1<-data$ISec1/60
  
  return(data)
  
  
}



survey.sums = function(data){
  
  data1 = data
  
  data1[data1 == 0]<- NA
  filtmean<-aggregate(data1$ISec1, by=list(data1$SURVEY), mean, na.rm=TRUE)
  bulkmean<-aggregate(data1$Bulk.Density, by=list(data1$SURVEY), mean, na.rm=TRUE)
  names(bulkmean)<-c("SURVEY", "Bulk.Density")
  names(filtmean)<-c("SURVEY","Infilt1")
  
  carbon<-subset(data1, select=c("Transect.Name","Point.Name","SURVEY","YEAR","Carbon.0.10.cm", "Carbon.10.40.cm","Clay.10.40.cm","Sand.10.40.cm","Silt.10.40.cm"))
  names(carbon)<-c("Transect","Point","SURVEY","YEAR","Carbon.0.10.cm", "Carbon.10.40.cm","Clay.10.40.cm","Sand.10.40.cm","Silt.10.40.cm")
  carbon<-unique(carbon)
  
  ##Merge water infiltration, bulkdensity, and carbon together##
  newdata<-merge(filtmean, bulkmean, by="SURVEY")
  newdata<-merge(newdata, carbon, by="SURVEY")
  
  
  return(newdata)
}

soil.final.cleanup = function(survey_sums){
  
  soil = survey_sums
  
  soil$CLAY<-soil$Clay.10.40.cm
  soil$SILT<-soil$Silt.10.40.cm
  soil$SAND<-soil$Sand.10.40.cm
  
  return(soil)
  
}

toSeconds <- function(x){
  if (!is.character(x)) stop("x must be a character string of the form H:M:S")
  if (length(x)<=0)return(x)
  
  unlist(
    lapply(x,
           function(i){
             i <- as.numeric(strsplit(i,':',fixed=TRUE)[[1]])
             if (length(i) == 3) 
               i[1]*3600 + i[2]*60 + i[3]
             else if (length(i) == 2) 
               i[1]*60 + i[2]
             else if (length(i) == 1) 
               i[1]
           }  
    )  
  )  
}



data.sum = function(data, transect,
                    surveyyear = levels(as.factor(data$YEAR)),
                    return_df = FALSE){
  
  library(gridExtra)
  library(stats)
  library(dplyr)
  
  data1 = subset(data, data$Transect %in% transect)
  data1 = subset(data1, data1$YEAR %in% surveyyear)
  data1$YEAR = as.factor(data1$YEAR)
  if(nrow(data1) == 0){
    warning("NULL object returned - no rows to aggregate")
    return(NULL)
  } else {
    
    pc = function(x){(diff(x)/x[1]) * 100}
    
    if(nlevels(as.factor(surveyyear)) == 1){
      
      data2 = subset(data1,select=c("Point","Carbon.0.10.cm", "Carbon.10.40.cm",
                                    "Bulk.Density", "Infilt1"))
      
      
      
      data2[,2:5] = round(data2[,2:5], 2)
      
      
    } else if(nlevels(as.factor(surveyyear > 1))){
      data2 = subset(data1,select=c("Point", "YEAR", "Carbon.0.10.cm", "Carbon.10.40.cm",
                                    "Bulk.Density", "Infilt1"))
      
      if(nrow(data2) == 0){return(NULL)}
      
      aggs = aggregate(data2[3:5], by = list(data2$Point), pc)
      
      for(i in 2:4){
        
        aggs[,i] = as.numeric(aggs[,i])
        
      }
      
      colnames(aggs) = c("Point", "Change.Carbon0-10", "Change.Carbon10-40", "BD")
      data2 = subset(data2, YEAR == max(levels(data2$YEAR)))
      data2 = merge(data2, aggs, by= "Point")
      data2$YEAR = NULL
      
      data2[,2:8] = round(data2[,2:8], 2)
    }
    
    
    if(ncol(data2) == 8 & !return_df){
      colnames(data2) = c("Point","Carbon\n0-10cm", "Carbon\n10-40cm",
                          "Bulk\nDensity", "Average\nInfiltration\n(min)",
                          "Carbon\n% Change\n0-10cm","Carbon\n% Change\n10-40cm",
                          "Bulk\nDensity\n% change")
    } else if (ncol(data2) == 5 & !return_df){
      colnames(data2) = c("Point","Carbon\n0-10cm", "Carbon\n10-40cm", "Bulk\nDensity", "Average\nInfiltration\n(min)")
    }
    
    
    if(!return_df){
      grid.table(data2,theme= ttheme_default(base_size=10), rows = NULL)
    } else {return(data2)}
    
    
  }
  
}


texture.triangle.plot = function(data, transect, year,
                                 labels = TRUE,
                                 background = TRUE,
                                 pch = 16,
                                 main = paste("Soil Texture - ",transect),
                                 cex.axis = 0.5,
                                 cex.lab = 0.75,
                                 cex.main = 0.75,
                                 text.tol = 0.2,
                                 tri.sum.tst = FALSE,
                                 cex = 0.75,
                                 frame.bg.col = "white",
                                 grid.show = FALSE,
                                 legend = TRUE,
                                 colors = c("black", "gray"),
                                 legendtitle = "Ranch"){
  
  library(soiltexture)
  data = subset(data, YEAR %in% year)
  if(!background){data = subset(data, subset = Transect %in% transect)}
  data = prepare.soil.triangle(data)
  data$color = NULL
  data$color[data$Transect %in% transect] = colors[1]
  data$color[!(data$Transect %in% transect)] = colors[2]
  data = arrange(data, desc(color))
  
  
  data1 = subset(data, data$Transect %in% transect)
  data2 = subset(data, !(data$Transect %in% transect))
  
  if(isTRUE(labels)){
    l = data1$Point
  } else{l = NA}
  
  
  ##texture plot##
  title_ranches=paste(transect, collapse = ', ')
  print(str(title_ranches))
  
  
  my_title <- paste("Soil texture: ", title_ranches)
  
  test = TT.plot(
    class.sys = "USDA.TT",    ## with the UDSA texture classes
    tri.data = data,
    pch = 16,
    main = my_title,
    cex.axis = cex.axis,
    cex.lab = cex.axis,
    cex.main = cex.main,
    text.tol = text.tol,
    tri.sum.tst = tri.sum.tst,
    col = data$color,
    cex = cex,
    frame.bg.col = frame.bg.col,
    grid.show = grid.show
  )
  
  
  TT.text(
    tri.data = data1,
    geo = test,
    tri.sum.tst=tri.sum.tst,
    labels = l,
    pos=c(1,3,2),
    offset=0.2,
    font = 0.1,
    cex = cex,
    col= "black")
  
  
  
  
  
  if(legend){legend(x = 90, y = 90,
                    title = "Transects",
                    legend = c(transect, if(background){"Others"}),
                    col = c(rep(colors[1], times = length(transect)), if(background){colors[2]}),
                    pch = pch)}
}

carbon.plot_updated<-function(data, transect, year,
                              labels = TRUE,
                              background = TRUE,
                              pointcolors = c(rep("black", length(transect)),"gray"),
                              legend = TRUE,
                              legendnames = c(transect, "Others"),
                              legendtitle = "Ranch",
                              box.padding = 0.5,
                              xlab = "% Carbon 10-40 cm",
                              ylab = "% Carbon 0-10 cm",
                              pointsize =4){
  
  title_ranches=paste(transect, collapse = ', ')
  print(str(title_ranches))
  
  
  my_title <- expression(paste("Soil Carbon Plot"),title_ranches)
  
  
  data = subset(data, data$YEAR %in% year)
  transect = transect[transect %in% data$Transect]
  if(!background){data = subset(data, subset = Transect %in% transect)}
  masked = data
  masked$Transect = as.character(masked$Transect)
  masked$Transect[!(masked$Transect %in% transect)] = "Background_points"
  
  
  masked2 = masked[!is.na(masked$Carbon.0.10.cm) & !is.na(masked$Carbon.10.40.cm),]
  
  
  masked_soil = arrange(masked2, desc(Transect))
  
  
  if(all(masked_soil$Transect == "Background_points")){
    pointcolors = pointcolors[length(pointcolors)]
    legendnames = legendnames[length(legendnames)]
  }
  
  if(isTRUE(labels)){
    labs = as.character(masked_soil$Point[masked_soil$Transect %in% transect])
  } else {
    labs = NA
  }
  
  
  p = ggplot(masked_soil, aes(x = Carbon.10.40.cm, y = Carbon.0.10.cm, color = factor(Transect),  shape=factor(YEAR))) +
    geom_point(size = pointsize) +
    labs(colour="Transect", shape="Year", size="Pointsize")+
    #scale_color_manual(values = pointcolors, labels = legendnames) +
    theme_bw() +
    xlab(xlab) +
    ylab(ylab) +
    guides(color = ifelse(legend, guide_legend(title = legendtitle), FALSE), label = FALSE) +
    geom_label_repel(data = masked_soil[masked_soil$Transect %in% transect,],aes(label = labs),color='black', segment.color = 'grey50',  box.padding = box.padding, show.legend = FALSE) +
    ggtitle(my_title)
  #ggtitle(paste("Soil Carbon Plot: ",title_ranches))
  
  return(p)
  
}



percent.change.plot = function(data, transect,
                               surveyyear = levels(as.factor(data$YEAR)),
                               background = TRUE,
                               legendnames = c(paste(transect, collapse = " "), "Others"),
                               legendtitle = "Ranch",
                               legend = TRUE,
                               boxcolors = c("black", "gray"),
                               xlabels = c("Carbon 0-10 cm", "Carbon 10-40 cm", "Bulk Density"),
                               ylab = "% Change",
                               choosevariables = c("Carbon010change", "Carbon1040change","Bulk.density.change")){
  
  title_ranches=paste(transect, collapse = ', ')
  
  
  pc = function(x){(diff(x)/x[1]) * 100}
  
  validate(
    if(length(transect)>1){
      "Select only one ranch."
    }else{
      NULL
    }
  )
  
  data$YEAR = as.factor(data$YEAR)
  data = subset(data, YEAR %in% surveyyear)
  if(!background){data = subset(data, subset = Transect %in% transect)}
  duped = data$Point[duplicated(data$Point)]
  data = data[data$Point %in% duped,]
  if(!(transect %in% data$Transect)){stop("Transect does not contain multiple surveys")}
  data$YEAR = as.factor(data$YEAR)
  
  
  data2 = subset(data,select=c("Point", "Transect", "YEAR", "Carbon.0.10.cm", "Carbon.10.40.cm",
                               "Bulk.Density"))
  
  
  aggs = aggregate(data2[4:6], by = list(data2$Point), pc)
  colnames(aggs) = c("Point", "Change.Carbon0-10", "Change.Carbon10-40", "BD")
  data2 = subset(data2, data2$YEAR == max(levels(droplevels(data2$YEAR))))
  data2 = merge(data2, aggs, by= "Point", all.x = T)
  
  
  masked = data2
  masked$Transect = as.character(replace(as.character(masked$Transect),
                                         masked$Transect != transect, values = "zzzz"))
  masked$Transect = as.character(replace(as.character(masked$Transect),
                                         masked$Transect == transect, values = "aaaa"))
  
  masked = masked %>% arrange(desc(Transect))
  
  colnames(masked)[7:9] = c("Carbon010change", "Carbon1040change","Bulk.density.change")
  
  masked = subset(masked, select = c("Point", "Transect" , choosevariables))
  
  masked = melt(masked, id.vars = c("Point", "Transect"))
  masked = masked[!is.na(masked$variable),]
  masked = masked[!is.nan(masked$variable),]
  
  
  p = ggplot(masked, aes(x = variable, y = value, color = Transect)) +
    geom_boxplot() +
    scale_color_manual(values = boxcolors, labels = legendnames) +
    guides(color = ifelse(legend, guide_legend(title = legendtitle), FALSE), label = FALSE) +
    theme_bw() +
    scale_x_discrete(labels = xlabels) +
    geom_hline(yintercept = 0, linetype = 'dotted') +
    ylab(ylab) +
    xlab(NULL)+
    ggtitle(paste("Carbon Percent Change: ",title_ranches))
  
  
  return(p)
  
  
}




add.soilcolumns_list=function(data, transect, year){
  data2=data
  final_data= subset(data2,Transect %in% transect)
  final_data=subset(final_data,YEAR %in% year)
  return(final_data)
}

prepare.soil.triangle = function(data, Inftarget = 3.81){
  if(any(is.na(data$CLAY))){
    removed = nrow(data[is.na(data$CLAY),])
    warning(paste(removed, "Observations without soil texture data have been removed"))
    data1 = data[complete.cases(data$CLAY),]
  } else {
    print("All observations used")
    data1 = data
  }
  
  data1$texture = TT.points.in.classes(
    tri.data    = data1,
    class.sys   = "USDA.TT",
    PiC.type    = "t", text.tol=1)
  
  data1$TextCategory = ifelse(data1$CLAY > 25, "Fine", ifelse(data1$CLAY < 15 & data1$SAND > 80, "Coarse","Coarse"))
  
  data1$BD_target = replace(data1$BD_target, data1$TextCategory == "Fine", 1.1)
  data1$BD_target = replace(data1$BD_target, data1$TextCategory == "Coarse", 1.4)
  
  data1$Infilt_target = Inftarget
  
  
  
  #data1$Infilt_dist = data1$Infilt_target - data1$Infilt1
  data1$Infilt_dist = data1$Infilt1
  #data1$BD_dist = data1$BD_target - data1$Bulk.Density
  data1$BD_dist =  data1$Bulk.Density
  
  data1$Location<-str_sub(data1$Point, -2)
  
  return(data1)
  
}




compaction.plot.exactValues<-function(data,
                                      transect,
                                      year,
                                      background = TRUE,
                                      labels = TRUE,
                                      pointcolors = c(rep("black", length(transect)),"gray"),
                                      legend = TRUE,
                                      legendnames = c(paste(c(transect)), "Others"),
                                      legendtitle = "Ranch",
                                      xlab = "Bulk density (g/cm3)",
                                      ylab = "Water Infiltration Rate (minutes)",
                                      box.padding = 0.5,
                                      pointsize = 4,
                                      linetype = "dashed",
                                      xlims = c(NA, NA),
                                      ylims = c(NA, NA)){
  
  
  title_years=paste(year, collapse = '_')
  title_ranches=paste(transect, collapse = ', ')
  
  
  year2<- year
  data = subset(data, YEAR %in% year)
  if(any(is.na(data$BD_dist))){
    removed = nrow(data[is.na(data$BD_dist) | is.na(data$Infilt_dist),])
    warning(paste(removed, "Observations missing bulk density data have been removed"))
    data1 = data[!is.na(data$BD_dist),]
    data1 = data[!is.na(data$Infilt_dist),]
  } else {
    data1 = data
  }
  transect = transect[transect %in% data$Transect]
  if(!background){data = subset(data, subset = Transect %in% transect)}
  
  if(isTRUE(labels)){
    labs = as.character(data$Point[data$Transect %in% transect])
  } else {
    labs = NA
  }
  
  p = ggplot(data, aes(x = Bulk.Density, y = Infilt1, color = factor(Transect),  shape=factor(YEAR))) +
    geom_point(size=pointsize) +
    #scale_fill_manual(name ="Transect",values = c("gray28","dodgerblue4","deepskyblue3","lightblue3", "lightblue4", "lightblue1"))+
    labs(colour="Transect", shape="Year", size="Pointsize")+
    geom_hline(yintercept = 10, linetype = linetype) +
    #scale_color_manual(values = pointcolors, labels = legendnames) +
    guides(color = ifelse(legend, guide_legend(title = legendtitle), FALSE), label = FALSE) +
    ylab("Water Infiltration (minutes)")+
    xlab("Bulk Density (g/cm3)")+
    theme_bw() +
    geom_label_repel(data = data[data$Transect %in% transect,],aes(label = labs),color='black', segment.color = 'grey50',  box.padding = box.padding, show.legend = FALSE) +
    ylim(0,max(data$Infilt1)+5)+
    xlim(min(data$Bulk.Density)-.25, max(data$Bulk.Density)+.25)+
    ggtitle(paste("Soil Compaction Plot: ",title_ranches))
  
  
  q = ggplot(data, aes(x=Bulk.Density, y=Infilt1))+
    geom_point()+
    ylab("Water Infiltration (minutes)")+
    xlab("Bulk Density (g/cm3)")+
    geom_hline(yintercept = 15)+
    geom_vline(xintercept = 1.4)+
    ylim(0,max(data$Infilt1)+5)+
    xlim(min(data$Bulk.Density)-.25, max(data$Bulk.Density)+.25)+
    geom_text(aes(label=as.character(data$Point)),position=position_jitter(height = 1.5))+
    ggtitle(paste("Soil Compaction Plot", as.character(year)))
  return(p)
}


compaction.plot<-function(data,
                          transect,
                          year,
                          background = TRUE,
                          labels = TRUE,
                          pointcolors = c(rep("black", length(transect)),"gray"),
                          legend = TRUE,
                          legendnames = c(paste(c(transect)), "Others"),
                          legendtitle = "Ranch",
                          xlab = "Bulk density (g/cm3)",
                          ylab = "Water Infiltration Rate (minutes)",
                          box.padding = 0.5,
                          pointsize = 4,
                          linetype = "dashed",
                          Inftarget = 3.81,
                          xlims = c(NA, NA),
                          ylims = c(NA, NA)){
  
  
  title_ranches=paste(transect, collapse = ', ')
  
  data = subset(data, YEAR %in% year)
  
  if(any(is.na(data$BD_dist))){
    removed = nrow(data[is.na(data$BD_dist) | is.na(data$Infilt_dist),])
    warning(paste(removed, "Observations missing bulk density data have been removed"))
    data1 = data[!is.na(data$BD_dist),]
    data1 = data[!is.na(data$Infilt_dist),]
  } else {
    data1 = data
  }
  
  transect = transect[transect %in% data$Transect]
  if(!background){data = subset(data, subset = Transect %in% transect)}
  masked = data %>% prepare.soil.triangle(Inftarget = Inftarget)
  masked$Transect = as.character(replace(as.character(masked$Transect), !(masked$Transect %in% transect), values = "Background_points"))
  
  
  masked_soil = arrange(masked, desc(Transect))
  
  if(all(masked_soil$Transect == "Background_points")){
    pointcolors = pointcolors[length(pointcolors)]
    legendnames = legendnames[length(legendnames)]
  }
  
  
  if(isTRUE(labels)){
    labs = as.character(masked_soil$Point[masked_soil$Transect %in% transect])
  } else {
    labs = NA
  }
  
  p = ggplot(masked_soil, aes(x = BD_dist, y = Infilt_dist, color = factor(Transect),  shape=factor(YEAR))) +
    geom_point(size=pointsize) +
    #scale_fill_manual(name ="Transect",values = c("gray28","dodgerblue4","deepskyblue3","lightblue3", "lightblue4", "lightblue1"))+
    labs(colour="Transect", shape="Year", size="Pointsize")+
    geom_hline(yintercept = 10, linetype = linetype) +
    #scale_color_manual(values = pointcolors, labels = legendnames) +
    guides(color = ifelse(legend, guide_legend(title = legendtitle), FALSE), label = FALSE) +
    xlab(xlab) +
    ylab(ylab) +
    theme_bw() +
    geom_label_repel(data = masked_soil[masked_soil$Transect %in% transect,],aes(label = labs),color='black', segment.color = 'grey50',  box.padding = box.padding, show.legend = FALSE) +
    xlim(as.numeric(xlims[1]), as.numeric(xlims[2])) +
    ylim(as.numeric(ylims[1]), as.numeric(ylims[2]))+
    ggtitle(paste("Soil Compaction Plot: ",title_ranches))
  
  return(p)
}


soil_function_library = c( texture.triangle.plot, compaction.plot, compaction.plot.exactValues, carbon.plot_updated, percent.change.plot)

names(soil_function_library) = c( "Texture Triangle", "Compaction Plot", "Compaction Plot Exact Values", "Carbon Plot", "Carbon Percent Change")

soil_lists_function_library=c(add.soilcolumns_list)
names(soil_lists_function_library)=c("Soil Data")

soil_plot_help = c("help_text/soil/soil.data.sum.txt", "help_text/soil/texture.triangle.plot.txt", "help_text/soil/compaction.plot.txt","help_text/soil/compaction.plot.exactValue.txt", "help_text/soil/carbon.plot.txt", "help_text/soil/percent.change.plot.txt")
names(soil_plot_help) = names(soil_function_library)


soil_map_variables = c("Carbon.0.10.cm", "Carbon.10.40.cm", "Bulk.Density", "Infilt1", "Change.Carbon0-10", "Change.Carbon10-40", "BD")
names(soil_map_variables) = c("Carbon 0-10 cm", "Carbon 10-40 cm", "Bulk Density", "Infiltration Time", "Percent Change Carbon 0-10 cm", "Percent Change Carbon 10-40 cm", "Percent Change Bulk Density")


