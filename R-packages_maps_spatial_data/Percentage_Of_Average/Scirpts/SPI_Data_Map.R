library(rio)
library(dplyr)
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

setwd("/Climate-Agriculture-1/Drought_Monitoring_Package/")

options(download.file.extra = '--no-check-certificate')
rm(list=ls())
#dir.create("Results",recursive = T,showWarnings = F)
#Monthly data

Africa<-readOGR("/ACMAD_Git/SHP_AFRIQUE/Afrique_frontier_news.shp") 

#Parameters<-import("Parameter/Parameters.csv")

Month="Jan"
Month_name="January"
Year=2022

Checking<-as.numeric(format(as.Date(paste(Year,"-",Month,"-",01,sep=""), tryFormats = c("%Y-%b-%d")),"%m"))


if(Checking==1){
  
  First_Month<-format(as.Date(paste(Year,"-",11,"-",01,sep=""), tryFormats = c("%Y-%m-%d")),"%b")
  
  First_Month_Name<-format(as.Date(paste(Year,"-",11,"-",01,sep=""), tryFormats = c("%Y-%m-%d")),"%B")
  
  download.file(paste("http://iridl.ldeo.columbia.edu/SOURCES/.IRI/.Analyses/.SPI/.SPI-CAMSOPI_3-Month/X/-25/0.5/55/GRID/Y/-40/0.5/40/GRID/T/%28",First_Month,"-",Month,"%20",Year-1,"%29%28",First_Month,"-",Month,"%20",Year,"%29RANGEEDGES/data.nc",sep=""),mode="wb",paste("/ACMAD_Git/Data/SPI_",First_Month,"_",Month,".nc",sep=""))
  
}
if(Checking==2){
  
  First_Month<-format(as.Date(paste(Year,"-",12,"-",01,sep=""), tryFormats = c("%Y-%m-%d")),"%b")
  
  First_Month_Name<-format(as.Date(paste(Year,"-",12,"-",01,sep=""), tryFormats = c("%Y-%m-%d")),"%B")
  
  download.file(paste("http://iridl.ldeo.columbia.edu/SOURCES/.IRI/.Analyses/.SPI/.SPI-CAMSOPI_3-Month/X/-25/0.5/55/GRID/Y/-40/0.5/40/GRID/T/%28",First_Month,"-",Month,"%20",Year-1,"%29%28",First_Month,"-",Month,"%20",Year,"%29RANGEEDGES/data.nc",sep=""),mode="wb",paste("/ACMAD_Git/Data/SPI_",First_Month,"_",Month,".nc",sep=""))
  
}
if(Checking>=3){
  
  First_Month<-format(as.Date(paste(Year,"-",Checking-2,"-",01,sep=""), tryFormats = c("%Y-%m-%d")),"%b")
  
  First_Month_Name<-format(as.Date(paste(Year,"-",Checking-2,"-",01,sep=""), tryFormats = c("%Y-%m-%d")),"%B")
  
  download.file(paste("http://iridl.ldeo.columbia.edu/SOURCES/.IRI/.Analyses/.SPI/.SPI-CAMSOPI_3-Month/X/-25/0.5/55/GRID/Y/-40/0.5/40/GRID/T/%28",First_Month,"-",Month,"%20",Year,"%29%28",First_Month,"-",Month,"%20",Year,"%29RANGEEDGES/data.nc",sep=""),mode="wb",paste("/ACMAD_Git/Data/SPI_",First_Month,"_",Month,".nc",sep=""))
  
}
#SPI Index
#http://iridl.ldeo.columbia.edu/SOURCES/.IRI/.Analyses/.SPI/.SPI-CAMSOPI_3-Month/X/-25/2.5/55/GRID/Y/-40/2.5/40/GRID/T/(Feb-Apr%202022)/(Feb-Apr%202022)/RANGEEDGES/data.nc
#http://iridl.ldeo.columbia.edu/expert/expert/SOURCES/a%3A/.IRI/.Analyses/.SPI/.SPI-CAMSOPI_1-Month/T/(Jun%202020)/VALUES/X/-20/0.5/55/GRID/Y/-40/0.5/40/GRID/data.nc
#download.file(paste("http://iridl.ldeo.columbia.edu/SOURCES/.IRI/.Analyses/.SPI/.SPI-CAMSOPI_3-Month/X/-25/0.5/55/GRID/Y/-40/0.5/40/GRID/T/%28",First_Month,"-",Month,"%20",Year,"%29%28",First_Month,"-",Month,"%20",Year,"%29RANGEEDGES/data.nc",sep=""),mode="wb",paste("Data/SPI_",First_Month,"_",Month,".nc",sep=""))

Data<-nc_open(filename = paste("/ACMAD_Git/Data/SPI_",First_Month,"_",Month,".nc",sep=""))
Lon<-ncvar_get(Data,"X")
Lat<-ncvar_get(Data,"Y")
Val<-ncvar_get(Data,"SPI-CAMSOPI_3-Month")
nc_close(Data)
for(i in 1:length(Lat)){
  if(i==1){
    SPI<-data.frame(Lon=Lon,Lat=Lat[i],SPI=Val[,i])
  }
  else{
    SPI1<-data.frame(Lon=Lon,Lat=Lat[i],SPI=Val[,i])
    SPI<-rbind(SPI,SPI1)
  }
}

SPI_Raster_format<-rasterFromXYZ(SPI)

Data_interpolted_SPI<-raster::disaggregate(SPI_Raster_format,8,method="bilinear")

Masked_SPI<-raster::mask(Data_interpolted_SPI,Africa)

Data_df_SPI<- as.data.frame(rasterToPoints(Masked_SPI))

names(Data_df_SPI)[3]="SPI"
rio::export(Data_df_SPI,paste("/ACMAD_Git/Products/Drought_Monitoring/Data/SPI_",First_Month,"-",Month,".csv",sep=""))

####################Climatology of the season###################################


download.file(paste("http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.CPC/.CAMS_OPI/.v0208/.mean/.prcp/T/(",First_Month,"%201981)/(",Month,"%202010)/RANGEEDGES/Y/-40/0.5/40/GRID/X/-25/0.5/55/GRID/T/",3,"/runningAverage/T/12/STEP/%5BT%5Daverage/",89,"/mul/data.nc",sep=""),mode="wb",paste("/ACMAD_Git/Data/Climatology_",First_Month,"_",Month,".nc",sep=""))

Climatology_Raster_format<-raster::raster(x =paste("/ACMAD_Git/Data/Climatology_",First_Month,"_",Month,".nc",sep=""))

Data_interpolted_clim<-raster::disaggregate(Climatology_Raster_format,8,method="bilinear")

Data_Masked_Clim<-raster::mask(Data_interpolted_clim,Africa)

Data_df_Clim<- as.data.frame(rasterToPoints(Data_Masked_Clim))

names(Data_df_Clim)[3]="Climatology"



Data_df_SPI<-merge(Data_df_SPI,Data_df_Clim,by=c("x","y"))


Data_df_SPI$SPI<-ifelse(Data_df_SPI$Climatology<100,0,Data_df_SPI$SPI)


rio::export(Data_df_SPI[,c("x","y","SPI","Climatology")],paste("/ACMAD_Git/Products/Drought_Monitoring/Data/SPI_",First_Month,"-",Month,".csv",sep=""))

######Mapping

mybreaks <- c(-Inf,-1.5,-0.5,0,0.5,1,2,Inf)

#Function to return the dersired number of colors
mycolors<- function(x) {
  colors<-colorRampPalette(c("darkred","darkorange","orange","gray","#AAFF00","#50C878","#008000"))(7)
  colors[1:x]
}

#Function to create labels for legend
breaklabel <- function(x){
  labels<- c("Extremely Dry","Very Dry","Moderately Dry","Near Normal","Moderately Wet","Very Wet","Extremely Wet")
  labels[1:x]
}

Title = paste("STANDARDIZED PRECIPITATION INDEX FROM ",toupper(First_Month_Name)," TO ",toupper(Month_name)," ",Year,"\nData Source:CAMSOPI",sep="")

Im<-grid::rasterGrob(png::readPNG("/ACMAD_Git/Logos/Acmad_logo_1.png"), interpolate = TRUE)

l<-ggplot()+geom_contour_filled(data=Data_df_SPI, aes(x,y,z = SPI),breaks= mybreaks, show.legend = TRUE) +
  scale_fill_manual(palette=mycolors, values=breaklabel(7), name="", drop=FALSE, guide = guide_legend(reverse = TRUE))+theme_bw()

last<-l+geom_polygon(data = Africa, aes(x = long,y = lat, group = group), fill = NA,color = "black",size = 1.1)+ theme(legend.position = c(.1, .1),legend.justification = c("left", "bottom"),legend.box.just = "right",legend.margin = margin(6, 6, 6, 6),legend.text = element_text(size=20,face = "bold"),plot.title = element_text(hjust = 0.5,size=20,face = "bold"),axis.text.x = element_text(size=15,face = "bold"),axis.text.y = element_text(size=15,face = "bold"))
last<-last+  annotation_custom(Im, xmin = 50, xmax = 60, ymin =30, ymax = 40) +coord_cartesian(clip = "off")
last<-last+ metR::scale_x_longitude(limits = c(-25, 60),breaks = seq(-25, 60,10)) + metR:: scale_y_latitude(limits = c(-40, 40),breaks = seq(-40, 40,10))
last<-last+labs(title = Title,x="",y="")



dir.create(paste("Products/Drought_Monitoring/Maps/",Month_name,sep=""),recursive = T,showWarnings = F)

jpeg(filename = paste("Products/Drought_Monitoring/Maps/",Month_name,"/SPI_",First_Month,"_",Month,"_",Year,".jpeg",sep=""),
     width = 11,
     height = 12,
     units = "in",
     res=300)
print(last)
dev.off()
