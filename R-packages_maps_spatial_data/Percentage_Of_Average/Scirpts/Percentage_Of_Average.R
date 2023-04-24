################################################################################
# Script for the State of Climate Technical note: ACMAD
#
# Input Data: Monthly Data: Monthly Cumulative, Africa Shape file
#
# Product Output: Percentage Map
# 
# Data Source: CAMS-OPI
# 
# 
# CopYearight: ACMAD/Niamey-Niger
# 
# Realized at DCE
# 
# For any assistance please contact: ibrahim.d.dije@aims-senegal.org
#                                  : sosnku2002@yahoo.com
#
# Data source link:https://iridl.ldeo.columbia.edu/expert/expert/SOURCES/.NOAA/.NCEP/.CPC/.CAMS_OPI/.v0208/.mean/.prcp/T/
################################################################################
#Used Packages
library(rio)
library(dplYear)
library(ncdf4)
library(RColorBrewer)
library(metR)
library(ggplot2)
library(rgdal)
library(ggplot2)
library(tidyverse)
library(raster)
library(sp)
library(ggdark)
library(showtext)
library(ggrepel)
library(Hmisc)
options(download.file.extra = '--no-check-certificate')
options(timeout=600)
options(warn=-1)
#Remove all the data that are contained in the enironment

################################################################################
# Set the Working Directory
# Make Sure that you have in your working directory the folders that contain:

# Logos/Acmad_logo_1.png

# SHP_AFRIQUE/Afrique_frontier_news.shp

setwd("D:/LRF/Percentage_Of_Average")

Africa<-readOGR(paste("SHP_AFRIQUE/Afrique_frontier_news.shp",sep=""))

################################################################################

Data_Source="CAMS-OPI"

#Give the main variables


Start<-"Nov"


End<-"Jan"


Season<-"NDJ"


Year=2021


Period<-paste(Start,"_",End,sep="")


####Number of Days in the Last Month: Dec in this case

  if(Season %in% c("NDJ","DJF","ONDJ","NDJF","DJFM")){
    Year=c(Year,Year+1)
    if(format(Sys.Date(),"%Y")==Year){
      Year=c(Year-1,Year)
      
    }else{
      Year=c(Year,Year+1)
    }
    Clim_End<-2011
    
  }else{
    Year=c(Year,Year)
    
    Clim_End<-2010
  }
  
  
  Nbday_End<-Hmisc::monthDays(as.Date(paste(Year[2],"-",End,"-01",sep=""),"%Y-%b-%d"))
  
  Number_of_Days<-length(seq(as.Date(paste(Year[1],"-",Start,"-","01",sep=""),format = "%Y-%b-%d"),as.Date(paste(Year[2],"-",End,"-",Nbday_End,sep=""),format = "%Y-%b-%d"),by="day"))
  
  Number_of_Months<-length(seq(as.Date(paste(Year[1],"-",Start,"-","01",sep=""),format = "%Y-%b-%d"),as.Date(paste(Year[2],"-",End,"-",Nbday_End,sep=""),format = "%Y-%b-%d"),by="month"))
  
  
################################################################################


################################################################################
#Monthly Cumulative

dir.create("Data",recursive = T,showWarnings = F)


download.file(paste("https://iridl.ldeo.columbia.edu/expert/expert/SOURCES/.NOAA/.NCEP/.CPC/.CAMS_OPI/.v0208/.mean/.prcp/T/(",Start,"%20",Year[1],")/(",End,"%20",Year[2],")/RANGEEDGES/Y/-40/0.5/40/GRID/X/55/0.5/-25/GRID/%5BT%5D/average/",Number_of_Days,"/mul/data.nc",sep=""),mode="wb",paste("Data/Cumulative_",Period,".nc",sep=""))

Data<-nc_open(filename = paste("Data/Cumulative_",Period,".nc",sep=""))
Lon<-ncvar_get(Data,"X")
Lat<-ncvar_get(Data,"Y")
Val<-ncvar_get(Data,"prcp")
nc_close(Data)
i=1
for(i in 1:length(Lat)){
  if(i==1){
    Cum<-data.frame(Lon=Lon,Lat=Lat[i],Cum=Val[,i])
  }
  else{
    Cum1<-data.frame(Lon=Lon,Lat=Lat[i],Cum=Val[,i])
    Cum<-rbind(Cum,Cum1)
  }
}
#Climatology
if(Season %in% c("NDJ","DJF","ONDJ","NDJF","DJFM")){
  Year=c(Year,Year+1)
   Clim_End<-2011
}else{
  
  Year=c(Year,Year)
  
  Clim_End<-2010
}


download.file(paste("https://iridl.ldeo.columbia.edu/expert/expert/SOURCES/.NOAA/.NCEP/.CPC/.CAMS_OPI/.v0208/.mean/.prcp/T/(",Start,"%201981)/(",End,"%20",Clim_End,")/RANGEEDGES/Y/-40/0.5/40/GRID/X/55/0.5/-25/GRID/T/",Number_of_Months,"/runningAverage/T/12/STEP/%5BT%5Daverage/",Number_of_Days,"/mul/data.nc",sep=""),mode="wb",paste("Data/Climatology_",Period,".nc",sep=""))

Data<-nc_open(filename =paste("Data/Climatology_",Period,".nc",sep=""))
Lon<-ncvar_get(Data,"X")
Lat<-ncvar_get(Data,"Y")
Val<-ncvar_get(Data,"prcp")
nc_close(Data)

for(i in 1:length(Lat)){
  if(i==1){
    Climatology<-data.frame(Lon=Lon,Lat=Lat[i],Climatology=Val[,i])
  }
  else{
    Climatology1<-data.frame(Lon=Lon,Lat=Lat[i],Climatology=Val[,i])
    Climatology<-rbind(Climatology,Climatology1)
  }
}

####################################Percentage##################################

Year<-Year[1]

dir.create("Products/Data",recursive = T,showWarnings = F)

Percentage<-merge(Cum,Climatology,by=c("Lon","Lat"))

Percentage$RR<-ifelse(Percentage$Climatology<100,NA,(Percentage$Cum/Percentage$Climatology)*100)

dir.create(paste("Products/Data/",Year,"/",Season,sep=""),recursive = T,showWarnings = F)

rio::export(Percentage,paste("Products/Data/",Year,"/",Season,"/Percentage_",Period,"_",Year,".csv",sep=""))



################################################################################


Raster_Perc<-raster::rasterFromXYZ(Percentage[,c("Lon","Lat","RR")])

Raster_Perc_Interpolation<-raster::disaggregate(Raster_Perc,6,method="bilinear")

Raster_Perc_Masked<-raster::mask(Raster_Perc_Interpolation,Africa)

RR_Perc_df<-as.data.frame(rasterToPoints(Raster_Perc_Masked))




mybreaks <- c(0,50,75,125,200,Inf)

#Function to return the dersired number of colors
mycolors<- function(x) {
  colors<-colorRampPalette(c("red","orange","darkgray","#69d205","darkgreen"))(5)
  colors[1:x]
}

#Function to create labels for legend
breaklabel <- function(x){
  labels<- as.character(c("Well below average","Below average","Near average","Above average","Well above average"))
  labels[1:x]
}



Title = toupper(paste("PRECIPITATION IN PERCENT OF AVERAGE FROM ",Start," TO ",End," ", Year,"\n OVER AFRICA",sep=""))

Im<-grid::rasterGrob(png::readPNG(paste("Logos/Acmad_logo_1.png",sep="")), interpolate = TRUE)

l<-ggplot()+geom_contour_filled(data=RR_Perc_df, aes(x,y,z = RR),breaks= mybreaks, show.legend = TRUE) +
  scale_fill_manual(palette=mycolors, values=breaklabel(5), name="", drop=FALSE, guide = guide_legend(reverse = TRUE))+theme_bw()

last<-l+geom_polygon(data = Africa, aes(x = long,y = lat, group = group), fill = NA,color = "black",size = 1.1)+ theme(legend.position = c(.1, .1),legend.justification = c("left", "bottom"),legend.box.just = "right",legend.margin = margin(6, 6, 6, 6),legend.text = element_text(size=20,face = "bold"),plot.title = element_text(hjust = 0.5,size=16,face = "bold"),axis.text.x = element_text(size=15,face = "bold"),axis.text.y = element_text(size=15,face = "bold"))
last<-last+  annotation_custom(Im, xmin = 50, xmax = 60, ymin =30, ymax = 40) +coord_cartesian(clip = "off")
last<-last+ metR::scale_x_longitude(limits = c(-25, 60),breaks = seq(-25, 60,10)) + metR:: scale_y_latitude(limits = c(-40, 40),breaks = seq(-40, 40,10))
last<-last+labs(title = Title,x="",y="")


dir.create(paste("Products/Maps/",Year,sep=""),recursive = T,showWarnings = F)

jpeg(filename = paste("Products/Maps/",Year,"/Percentage_",Season,"_CAMS_OPI.jpeg",sep=""),
     width = 11,
     height = 12,
     units = "in",
     res=300)
print(last)
dev.off()

