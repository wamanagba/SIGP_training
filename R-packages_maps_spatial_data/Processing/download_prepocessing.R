library(dplyr)
library(tidync)
library(ncdf4)
library(rio)
library(rgdal)
library(ggplot2)
library(metR)
library(raster)
setwd("~/Desktop/SIGP_training/R-packages_maps_spatial_data/Processing")

dir.create("Products",recursive = T,showWarnings = F)
dir.create("Data",recursive = T,showWarnings = F)

for (k in 2020:2022) {
  download.file(paste("https://iridl.ldeo.columbia.edu/SOURCES/.UCSB/.CHIRPS/.v2p0/.daily-improved/.global/.0p05/.prcp/Y/-40/0.5/40/GRID/X/-25/0.5/55/GRID/T/(1%20Jun%20",k,")/(31%20Octc%20",k,")/RANGEEDGES/data.nc",sep=""),destfile =paste("Data/",k,".nc",sep = "" ))
  
}



rm(list = ls())

k=1981

for (k in 2020:2022) {
  Data_NC<-nc_open(paste("Data/",k,".nc",sep=""))
  Data<-tidync(paste("Data/",k,".nc",sep=""))%>%hyper_tibble(na.rm = F)
  Date=seq(as.Date(paste(k,"-06-01",sep="")),as.Date(paste(k,"-10-31",sep="")),by="days")
  X<-length(ncvar_get(Data_NC,"X"))
  Y<-length(ncvar_get(Data_NC,"Y"))
  Date_All=sort(rep(Date,X*Y),decreasing = F)
  Data$T<-Date_All
  
  dir.create("Products/CHIRPS/",recursive = T,showWarnings = F)
  rio::export(Data,paste("Products/CHIRPS/",k,".csv",sep=""))  
}
