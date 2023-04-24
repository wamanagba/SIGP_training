
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
rm(list=ls())

setwd("~/Desktop/SIGP_training/R-packages_maps_spatial_data/Total_Rainfall/")

dir.create("Products",recursive = T,showWarnings = F)
dir.create("Data",recursive = T,showWarnings = F)

Africa<-readOGR("SHP_AFRIQUE/Afrique_frontier_news.shp") 

#Give the month abbr and name
# Parameters<-import("Parameter/Parameters.csv")

Month="Mar"
Month_name="March"
Year=2023

#Monthly Cumulative
download.file(paste("https://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.CPC/.CAMS_OPI/.v0208/.mean/.prcp/T/(",Month,"%20",Year,")/(",Month,"%20",Year,")/RANGEEDGES/Y/-40/0.5/40/GRID/X/-25/0.5/55/GRID/%5BT%5D/average/31/mul/data.nc",sep=""),mode="wb",paste("Data/Cumulative_",Month_name,"_",Year,".nc",sep=""))

Data_Cum<-raster::raster(x = paste("Data/Cumulative_",Month_name,"_",Year,".nc",sep=""))


Data_interpolted_Cum<-raster::disaggregate(Data_Cum,5,method="bilinear")

Data_Masked_Cum<-raster::mask(Data_interpolted_Cum,Africa)

Data_df_Cum<- as.data.frame(rasterToPoints(Data_Masked_Cum))

names(Data_df_Cum)[3]="Prcp"



data <- Data_df_Cum
mybreaks <- c(-Inf,100,400,600,1200,1500,Inf)

#Function to return the desired number of colors

mycolors<- function(x) {
  colors<-colorRampPalette(c("#89522a","#8cb02c","darkviolet","#37fdf8","#2ccac6","blue"))(6)
  colors[1:x]
}

#Function to create labels for legend

breaklabel <- function(x){
  labels<- as.character(c("< 100 mm: Off Season Area","100-400 mm: Arid Zone","400-600 mm: Semi-Arid Zone","600-1200 mm: Sub-Humid Zone","1200-1500 mm: Moist Sub-Humid Zone",">1500 mm:  Humid Zone"))
  labels[1:x]
}
################################################################################


Title = paste("MONTHLY TOTAL PRECIPITATION FOR ",toupper(Month_name)," ", Year,"\n Data source: CAMS-OPI",sep="")

#Im<-grid::rasterGrob(png::readPNG("Logos/Acmad_logo_1.png"), interpolate = TRUE)

l<-ggplot()+geom_contour_filled(data=data, aes(x,y,z = Prcp),breaks= mybreaks, show.legend = TRUE) +
  scale_fill_manual(palette=mycolors, values=breaklabel(6), name="", drop=FALSE, guide = guide_legend(reverse = TRUE))+theme_bw()

last<-l+geom_polygon(data = Africa, aes(x = long,y = lat, group = group), fill = NA,color = "black",size = 1.1)+ theme(legend.position = c(.01, .05),legend.justification = c("left", "bottom"),legend.box.just = "right",legend.margin = margin(6, 6, 6, 6),legend.text = element_text(size=10,face = "bold"),plot.title = element_text(hjust = 0.5,size=13,face = "bold"),axis.text.x = element_text(size=15,face = "bold"),axis.text.y = element_text(size=15,face = "bold"))
#last<-last+  annotation_custom(Im, xmin = 50, xmax = 60, ymin =30, ymax = 40) +coord_cartesian(clip = "off")
last<-last+ metR::scale_x_longitude(limits = c(-25, 60),breaks = seq(-25, 60,10)) + metR:: scale_y_latitude(limits = c(-40, 40),breaks = seq(-40, 40,10))
last<-last+labs(title = Title,x="",y="")


dir.create(paste("Products/Maps/",Year,"/",Month_name,sep=""),recursive = T,showWarnings = F)

jpeg(filename = paste("Products/Maps/",Year,"/",Month_name,"/Total_Precip_",Month,"_",Year,".jpeg",sep=""),
     width = 8,
     height = 10,
     units = "in",
     res=50)
print(last)
dev.off()

