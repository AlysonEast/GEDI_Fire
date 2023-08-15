library(reshape)
library(sp)
library(rgdal)
library(raster)

#Reading in data####
Sys.time()
print("start file read in")
gedilevel2a_spdf <- readOGR(dsn = "/mnt/lustrefs/scratch/v38p156/Chunks", layer = L2Adata_5) #each file will be read in from the working directory
Sys.time()
print("finished reading files")

#Extract region to points
region<-raster("/mnt/lustrefs/scratch/v38p156/Rasters/region.tif")

Sys.time()
print("start extraction")
beginCluster(n=4) #Assign N 
gedilevel2a_spdf<-extract(region, gedilevel2a_spdf)
endCluster()
Sys.time()
print("extraction done")

#Subset by region
Region_list<-c(1:63)

print("Start Subset")
for (i in 1:length(Region_list)){
  temp_data <- subset(gedilevel2a_spdf, Region_100kmi==i)
  writeOGR(temp_data, dsn = paste0("/mnt/lustrefs/scratch/v38p156/Regions/", Region_list[i]), layer = paste0("L2A_",i,"_5"), driver = "ESRI Shapefile")
}
Sys.time()