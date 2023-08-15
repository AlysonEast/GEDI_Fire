library(tidyverse)
library(geojsonR)
library(jsonlite)
library(reshape)
library(sp)
library(rgdal)
library(raster)

#Reading in data####
#set the working directory from which the files will be read from
setwd("/mnt/lustrefs/scratch/v38p156/L2A1/output") #CHANGE HERE

#create a list of the files from your target directory
file_list <- list.files(path="/mnt/lustrefs/scratch/v38p156/L2A1/output") #CHANGE HERE

#initiate a blank data frame, each iteration of the loop will append the data from the given file to this variable
gedilevel2a <- data.frame()

#had to specify columns to get rid of the total column
Sys.time()
print("start file read in")
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
  print(file_list[i])
}
Sys.time()
print("finished reading files")

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

#Converting to spatial DF to extract
Sys.time()
print("create spatial file")
for_proj<-readOGR(dsn = "/mnt/lustrefs/scratch/v38p156/", layer = "GEDI_tiny")
coordinates(gedilevel2a_spdf) <- ~Longitude + Latitude
proj4string(gedilevel2a_spdf)<-proj4string(for_proj)

#Extract region to points
region<-raster("/mnt/lustrefs/scratch/v38p156/Rasters/Region_100kmi.tif")

Sys.time()
print("start extraction")
beginCluster(n=4) #Assign N 
gedilevel2a_spdf<-extract(region, gedilevel2a_spdf, sp=TRUE)
endCluster()
Sys.time()
print("extraction done")

#Subset by region
Region_list<-c(1:63)

print("Start Subset")
for (i in 1:length(Region_list)){
  temp_data <- subset(gedilevel2a_spdf, Region_100kmi==i)
  writeOGR(temp_data, dsn = paste0("/mnt/lustrefs/scratch/v38p156/Regions/", i), layer = paste0("L2A_",Region_list[i],"_34"), driver = "ESRI Shapefile") #CHANGE _##
}
Sys.time()