library(sp)
library(rgdal)
library(raster)
library(reshape)
library(snow)

path<-paste0("/mnt/lustrefs/scratch/v38p156/Regions/",n) # CHANGE HERE
#create a list of the files from your target directory
file_list <- list.files(path=path, pattern="\\.shp$", full.names=FALSE)
file_list2<-c()
for (i in 1:length(file_list)){
  tempdata<-substr(file_list[i],1,nchar(file_list[i])-4)
  file_list2<-c(file_list2, tempdata)}

#Raster for smaller regions
region<-raster("/mnt/lustrefs/scratch/v38p156/Rasters/Region_50kkm.tif")

#Read in Data
Sys.time()
print("start file read in")
beginCluster(n=8)
for (i in 1:length(file_list2)){
    temp_data <- readOGR(dsn = path, layer = file_list2[i]) #Read in 1 shapefile at a time
    print(file_list2[i])
    temp_data<-extract(region, temp_data, sp=TRUE) #Extract the regions to it
    regions_list<-unique(extract$Region_50kkm) #Create a list of all of the regions in that file
    for (i in 1:length(Region_list)){
      temp_data_region <- subset(temp_data, Region_50kkm==i)
      writeOGR(temp_data, dsn = paste0("/mnt/lustrefs/scratch/v38p156/Small_Regions/", i), layer = paste0("L2A_",Region_list[i],"_",n), driver = "ESRI Shapefile") #CHANGE HERE_##
      print("Regions Done")
    }
}
endCluster()

Sys.time()