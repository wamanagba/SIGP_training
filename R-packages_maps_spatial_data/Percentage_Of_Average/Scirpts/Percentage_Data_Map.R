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
#library(showtext)
library(ggrepel)
library(Hmisc)

options(download.file.extra = '--no-check-certificate')
# rm(list=ls())

setwd("~/Desktop/SIGP_training/R-packages_maps_spatial_data/Percentage_Of_Average/")


Africa<-readOGR("SHP_AFRIQUE/Afrique_frontier_news.shp") 



Month="Mar"
Month_name="March"
Year=2023

#Monthly Cumulative

download.file(paste("https://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.CPC/.CAMS_OPI/.v0208/.mean/.prcp/T/(",Month,"%20",Year,")/(",Month,"%20",Year,")/RANGEEDGES/Y/-40/0.5/40/GRID/X/-25/0.5/55/GRID/%5BT%5D/average/31/mul/data.nc",sep=""),mode="wb",paste("Data/Cumulative_",Month_name,"_",Year,".nc",sep=""))

Data_Cum<-raster::raster(x = paste("Data/Cumulative_",Month_name,"_",Year,".nc",sep=""))


Data_interpolted_Cum<-raster::disaggregate(Data_Cum,5,method="bilinear")

Data_Masked_Cum<-raster::mask(Data_interpolted_Cum,Africa)

Data_df_Cum<- as.data.frame(rasterToPoints(Data_Masked_Cum))

names(Data_df_Cum)[3]="Precipitation"

#Monthly Climatology

download.file(paste("https://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.CPC/.CAMS_OPI/.v0208/.mean/.prcp/T/(",Month,"%201981)/(",Month,"%202010)/RANGEEDGES/Y/-40/0.5/40/GRID/X/-25/0.5/55/GRID/T/12/STEP/%5BT%5Daverage/31/mul/data.nc",sep=""),mode="wb", paste("Data/Climatology_",Month_name,"_",Year,".nc",sep=""))


Data_Clim<-raster::raster(x=paste("Data/Climatology_",Month_name,"_",Year,".nc",sep=""))



Data_interpolted_Clim<-raster::disaggregate(Data_Clim,5,method="bilinear")

Data_Masked_Clim<-raster::mask(Data_interpolted_Clim,Africa)

Data_df_Clim<- as.data.frame(rasterToPoints(Data_Masked_Clim))

names(Data_df_Clim)[3]="Climatology"

Percentage<-merge(Data_df_Cum,Data_df_Clim,by=c("x","y"))

Percentage$RR<-ifelse(Percentage$Climatology<=31,100,(Percentage$Precipitation/Percentage$Climatology)*100)

rio::export(Percentage[,c("x","y","RR")],paste("Data/Percentage_",Month,".csv",sep=""))



###############################Mapping##########################################


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

Title = paste("MONTHLY PRECIPITATION IN PERCENT OF AVERAGE FOR ",toupper(Month_name)," ", Year,"\n Data source: CAMS-OPI",sep="")


l<-ggplot()+geom_contour_filled(data=Percentage, aes(x,y,z = RR),breaks= mybreaks, show.legend = TRUE) +
  scale_fill_manual(palette=mycolors, values=breaklabel(5), name="", drop=FALSE, guide = guide_legend(reverse = TRUE))+theme_bw()

last<-l+geom_polygon(data = Africa, aes(x = long,y = lat, group = group), fill = NA,color = "black",size = 1.1)+ theme(legend.position = c(.01, .05),legend.justification = c("left", "bottom"),legend.box.just = "right",legend.margin = margin(6, 6, 6, 6),legend.text = element_text(size=15,face = "bold"),plot.title = element_text(hjust = 0.5,size=13,face = "bold"),axis.text.x = element_text(size=15,face = "bold"),axis.text.y = element_text(size=15,face = "bold"))
last<-last+ metR::scale_x_longitude(limits = c(-25, 60),breaks = seq(-25, 60,10)) + metR:: scale_y_latitude(limits = c(-40, 40),breaks = seq(-40, 40,10))
last<-last+labs(title = Title,x="",y="")


dir.create(paste("Products/Maps/",Year,"/",Month_name,sep=""),recursive = T,showWarnings = F)

jpeg(filename = paste("Products/Maps/",Year,"/",Month_name,"/Percentage_",Month,"_",Year,".jpeg",sep=""),
     width = 8,
     height = 10,
     units = "in",
     res=50)
print(last)
dev.off()

