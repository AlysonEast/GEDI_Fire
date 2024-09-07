library(tidyverse)
library(geojsonR)
library(jsonlite)
library(reshape)
library(sp)
library(rgdal) 
library(tidyr)
 
#Reading in data####
#set the working directory from which the files will be read from
setwd("/media/aly/Bridger/Thesis/gediSimulator/GEDI_Data/Zone19/L2A/output")

#create a list of the files from your target directory
file_list <- list.files(path="/media/aly/Bridger/Thesis/gediSimulator/GEDI_Data/Zone19/L2A/output")

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
  temp_data$track <- substr(file_list[i], 34,39)
  temp_data$PPDS <- substr(file_list[i], 41,42)
  temp_data$GOC_SDS <- substr(file_list[i], 44,46)
  temp_data$Version <- substr(file_list[i], 51,54)
  temp_data$file <- file_list[i]
  gedilevel2a <- rbind(gedilevel2a, temp_data) #for each iteration, bind the new data to the building dataset
  print(file_list[i])
}
Sys.time()
print("finished reading files")

#set the working directory from which the files will be read from
setwd("/media/aly/Bridger/Thesis/gediSimulator/GEDI_Data/Zone21/L2A/output")

#create a list of the files from your target directory
file_list <- list.files(path="//media/aly/Bridger/Thesis/gediSimulator/GEDI_Data/Zone21/L2A/output")

#add zone 21
for (i in 1:length(file_list)){
  temp_data <- fromJSON(txt= file_list[i],simplifyDataFrame = TRUE, flatten=TRUE) %>% as.data.frame #each file will be read in from the working directory
  temp_data$year <- substr(file_list[i], 10,13)
  temp_data$day <- substr(file_list[i], 14,16)
  temp_data$hour <- substr(file_list[i], 17,18)
  temp_data$min <- substr(file_list[i], 19,20)
  temp_data$min <- substr(file_list[i], 21,22)
  temp_data$orbit <- substr(file_list[i], 24,29)
  temp_data$track <- substr(file_list[i], 34,39)
  temp_data$PPDS <- substr(file_list[i], 41,42)
  temp_data$GOC_SDS <- substr(file_list[i], 44,46)
  temp_data$Version <- substr(file_list[i], 51,54)
  temp_data$file <- file_list[i]
  gedilevel2a <- rbind(gedilevel2a, temp_data) #for each iteration, bind the new data to the building dataset
  print(file_list[i])
}
Sys.time()

#Convert Shot number from numeric to character
gedilevel2a$features.properties.shot_number<-as.character(gedilevel2a$features.properties.shot_number)

#Removing Unnessisary Columns
Sys.time()
print("start editing Colnames")
gedilevel2a_spdf<-gedilevel2a[,c(-2,-3,-4,-123)]
for ( col in 2:120){
  colnames(gedilevel2a_spdf)[col] <- substr(colnames(gedilevel2a_spdf[col]),21,nchar(colnames(gedilevel2a_spdf[col])))
}
Sys.time()
print("Colnames Edited")

gedilevel2a_spdf$ordinates<-as.character(gedilevel2a_spdf$ordinates)

#Converting to spatial DF and writing out
Sys.time()
print("create spatial file")
for_proj<-readOGR(dsn = "/media/aly/Bridger/Thesis/Spatial/", layer = "GEDI_tiny")
coordinates(gedilevel2a_spdf) <- ~Longitude + Latitude
proj4string(gedilevel2a_spdf)<-proj4string(for_proj)

Collo19<-readOGR(dsn="/media/aly/Bridger/Thesis/Spatial/LiDAR", layer = "Collocate_19_v4_w_L2A_join")
Collo21<-readOGR(dsn="/media/aly/Bridger/Thesis/Spatial/LiDAR", layer = "Collocate_21_v4_w_L2A_join")

zone21<-gedilevel2a_spdf[gedilevel2a_spdf$file %in% file_list, ]
`%notin%` <- Negate(`%in%`)
zone19<-gedilevel2a_spdf[gedilevel2a_spdf$file %notin% file_list, ]

zone21<-spTransform(zone21, crs(Collo21))
zone19<-spTransform(zone19, crs(Collo19))

write.csv(zone19@coords, "/media/aly/Bridger/Thesis/gediSimulator/GEDI_coords_zone19.csv")
write.csv(zone21@coords, "/media/aly/Bridger/Thesis/gediSimulator/GEDI_coords_zone21.csv")

gedilevel2a_spdf@data$utm_Long<-0
gedilevel2a_spdf@data$utm_Lat<-0

gedilevel2a_spdf@data[gedilevel2a_spdf@data$file %in% file_list, ]$utm_Long<-zone21@coords[,1]
gedilevel2a_spdf@data[gedilevel2a_spdf@data$file %in% file_list, ]$utm_Lat<-zone21@coords[,2]
gedilevel2a_spdf@data[gedilevel2a_spdf@data$file %notin% file_list, ]$utm_Long<-zone19@coords[,1]
gedilevel2a_spdf@data[gedilevel2a_spdf@data$file %notin% file_list, ]$utm_Lat<-zone19@coords[,2]

writeOGR(gedilevel2a_spdf, dsn = "/media/aly/Bridger/Thesis/Spatial/LiDAR/", layer = "L2A_redo", driver = "ESRI Shapefile", overwrite_layer = TRUE)
write.csv(gedilevel2a_spdf@data, "/media/aly/Bridger/Thesis/Spatial/LiDAR/L2A_redo_data.csv")

Sys.time()