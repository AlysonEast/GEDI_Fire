# install.packages("rGEDI")
# install.packages("rjson")
# install.packages("geojsonR")
# install.packages("jsonlite")
# install.packages("MESS")
library(geojsonR)
library(sp)
library(jsonlite)
library(tidyverse)
library(MESS)
library(reshape)
library(sp)
library(raster)
library(rgdal)

#Reading in data####
#set the working directory from which the files will be read from
setwd("G:/Thesis/Data/L2A_process")

#create a list of the files from your target directory
file_list <- list.files(path="G:/Thesis/Data/L2A_process")

#initiate a blank data frame, each iteration of the loop will append the data from the given file to this variable
gedilevel2a <- data.frame()

Sys.time()
#had to specify columns to get rid of the total column
for (i in 1:length(file_list)){
  temp_data <- fromJSON(txt= file_list[i],simplifyDataFrame = TRUE, flatten=TRUE) %>% as.data.frame #each file will be read in from the working directory
  temp_data$year <- substr(file_list[i], 10,13)
  temp_data$day <- substr(file_list[i], 14,16)
  temp_data$hour <- substr(file_list[i], 17,18)
  temp_data$min <- substr(file_list[i], 19,20)
  temp_data$min <- substr(file_list[i], 21,22)
  temp_data$orbit <- substr(file_list[i], 24,29)
  temp_data$track <- substr(file_list[i], 31,36)
  temp_data$PPDS <- substr(file_list[i], 38,39)
  temp_data$GOC_SDS <- substr(file_list[i], 41,43)
  temp_data$Version <- substr(file_list[i], 45,46)
  gedilevel2a <- rbind(gedilevel2a, temp_data) #for each iteration, bind the new data to the building dataset
  file_list[i]
}

#Filtering out data with quality_flag=0
gedilevel2a<-subset(gedilevel2a, features.properties.quality_flag==1)
Sys.time()
print("removed quality flags")
#Convert Shot number from numeric to character
gedilevel2a$features.properties.shot_number<-as.character(gedilevel2a$features.properties.shot_number)

#Removing Unnessisary Columns
Sys.time()
print("start editing Colnames")
gedilevel2a_spdf<-gedilevel2a[,c(-2,-3,-4,-5,-123,-124)]
for ( col in 1:118){
  colnames(gedilevel2a_spdf)[col] <- substr(colnames(gedilevel2a_spdf[col]),21,nchar(colnames(gedilevel2a_spdf[col])))
}
Sys.time()
print("Colnames Edited")

#Make all RH less than 0 = 0
gedilevel2a_spdf[,c(13:113)][gedilevel2a_spdf[,c(13:113)] < 0] <-0
#Take RH sum
gedilevel2a_spdf$rh_sum<-rowSums(gedilevel2a_spdf[,c(13:113)])
  
#Converting to spatial DF and writing out
Sys.time()
print("create spatial file")
for_proj<-readOGR(dsn = "/mnt/lustrefs/scratch/v38p156/", layer = "GEDI_tiny")
coordinates(gedilevel2a_spdf) <- ~Longitude + Latitude
proj4string(gedilevel2a_spdf)<-proj4string(for_proj)
writeOGR(gedilevel2a_spdf, dsn = "/mnt/lustrefs/scratch/v38p156/", layer = "gediL2A", driver = "ESRI Shapefile")
Sys.time()

#   OLD ####
#Restructure data for AUC calculation
rh_sub<-gedilevel2a[c(6,17:117)]
rh_melt<-melt(rh_sub, id="features.properties.shot_number")
rh_cast<-cast(rh_melt, variable~features.properties.shot_number)

#List of shot numbers for for loop
shot_list<-gedilevel2a$features.properties.shot_number
#empty dataframe to store for loop output
auc_list<-data.frame(features.properties.shot_number = character(),
                     rh_auc = numeric())

#Calculate AUC for each shot and store it in a dataframe
Sys.time()
for(i in shot_list){
y<-rh_cast[,i]
x<-0:100

val<-auc(x,y)
df<-data.frame(i, val)
auc_list<-rbind(auc_list, df)
i
Sys.time()
}
Sys.time()
colnames(auc_list)<-c("features.properties.shot_number", "rh_auc")

#Merge AUC values with L2A dataframe
gedilevel2a_auc<-merge(gedilevel2a, auc_list, by="features.properties.shot_number")

#Removing Unnessisary Columns
gedilevel2a_spdf<-gedilevel2a_auc[,c(-2,-3,-4,-5,-123,-124)]
for ( col in 1:118){
  colnames(gedilevel2a_spdf)[col] <- substr(colnames(gedilevel2a_spdf[col]),21,nchar(colnames(gedilevel2a_spdf[col])))
}

#Converting to spatial DF and writing out
Sys.time()
for_proj<-readOGR(dsn = "G:/Thesis/Spatial", layer = "GEDI_tiny")
coordinates(gedilevel2a_spdf) <- ~Longitude + Latitude
proj4string(gedilevel2a_spdf)<-proj4string(for_proj)
writeOGR(gedilevel2a_spdf, dsn = "G:/Thesis/Spatial", layer = "gediL2A", driver = "ESRI Shapefile")
Sys.time()
