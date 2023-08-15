library(reshape)
library(sp)
library(rgdal)
library(raster)
library(reshape)
#Reading in data####
#set the working directory from which the files will be read from
setwd("/mnt/lustrefs/scratch/v38p156")

file_list <- c(L2Adata_1, L2Adata_3, L2Adata_4, L2Adata_5, L2Adata_6, L2Adata_7, L2Adata_8)

L2A <- data.frame()

Sys.time()
print("start file read in")
for (i in 1:length(file_list)){
  temp_data <- readOGR(dsn = "/mnt/lustrefs/scratch/v38p156/Chunks", layer = file_list[i]) #each file will be read in from the working directory
  L2A <- rbind(L2A, temp_data) #for each iteration, bind the new data to the building dataset
  print(file_list[i])
}
Sys.time()
print("finished reading files")

L2A$Day_code<-paste0(L2A$year, L2A$day)

Sys.time()
print("Writing out L2A full dataset")
writeOGR(L2A, dsn = "/mnt/lustrefs/scratch/v38p156/L2A", layer = L2A, driver = "ESRI Shapefile")
Sys.time()
print("L2A full dataset out")

Fire_FID<-raster("/mnt/lustrefs/scratch/v38p156/Rasters/Fire_atlas_FID.tif")
start_DOY<-raster("/mnt/lustrefs/scratch/v38p156/Rasters/Fire_atlas_start_DOY.tif")
end_DOY<-raster("/mnt/lustrefs/scratch/v38p156/Rasters/Fire_atlas_end_DOY.tif")

Atlas_Stack <- stack(start_DOY, end_DOY, Fire_FID)

Sys.time()
print("start extraction")
L2A<-extract(Atlas_Stack, L2A)
Sys.time()
print("extraction done")

Sys.time()
print("Creating burn status code")
L2A$Burn_stat<-if(L2A$Day_code < L2A$start_DOY) {
  print("Pre-Burn")
} else if (L2A$Day_code > L2A$end_DOY) {
  print("Post-Burn")
} else {
  print("Inter")
}
Sys.time()
print("burn status code done")

L2A$num<-1
Sys.time()
print("Write out L2A_Atlas.shp")
writeOGR(L2A, dsn = "/mnt/lustrefs/scratch/v38p156/", layer = "L2A_Atlas", driver = "ESRI Shapefile")
Sys.time()
print("Write out L2A_Atlas.shp done")

Sys.time()
print("cast for fire Breakdown")
Fire_Breakdown<-cast(L2A, Fire_FID~Burn_stat, value = "num", sum)
write.csv(Fire_Breakdown, file = "/mnt/lustrefs/scratch/v38p156/Fire_Atlas_Breakdown.csv")


