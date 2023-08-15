library(sp)
library(rgdal)
library(raster)
library(reshape)
library(snow)

command_args <- commandArgs(trailingOnly = TRUE)
# Define command line arguments as a variable
n <- command_args[1]

path<-paste0("/mnt/lustrefs/scratch/v38p156/Small_Regions_v002/",n)
#create a list of the files from your target directory
file_list <- list.files(path=path, pattern="\\.shp$", full.names=FALSE)
file_list2<-c()
for (i in 1:length(file_list)){
  tempdata<-substr(file_list[i],1,nchar(file_list[i])-4)
  file_list2<-c(file_list2, tempdata)}


#Read in Data
Sys.time()
print("start file read in")
for (i in 1:1){
  L2A_all <- readOGR(dsn = path, layer = file_list2[i])
  print(file_list2[i])
}
for (i in 2:length(file_list2)){
  temp_data <- readOGR(dsn = path, layer = file_list2[i])
  L2A_all <- rbind(L2A_all, temp_data)
  print(file_list2[i])
}
Sys.time()
print("finished reading files")

#Creating code to match rasters
L2A_all$Day_code<-paste0(L2A_all$year, L2A_all$day)
L2A_all$Day_code<-as.numeric(L2A_all$Day_code)

table(L2A_all$Day_code)

#reading in the Rasters
Fire_FID<-raster("/mnt/lustrefs/scratch/v38p156/Rasters/Fire_atlas_FID.tif")
start_DOY<-raster("/mnt/lustrefs/scratch/v38p156/Rasters/Fire_atlas_start_DOY.tif")
end_DOY<-raster("/mnt/lustrefs/scratch/v38p156/Rasters/Fire_atlas_end_DOY.tif")
Fire19_FID<-raster("/mnt/lustrefs/scratch/v38p156/Rasters/M19_FID.tif")
M19_DOY<-raster("/mnt/lustrefs/scratch/v38p156/Rasters/M19_DOY.tif")
Fire20_FID<-raster("/mnt/lustrefs/scratch/v38p156/Rasters/M20_FID.tif")
M20_DOY<-raster("/mnt/lustrefs/scratch/v38p156/Rasters/M20_DOY.tif")
ROI<-raster("/mnt/lustrefs/scratch/v38p156/Rasters/ROI_30.tif")

Stack1 <- stack(Fire_FID, start_DOY, end_DOY)
Stack2 <- stack(Fire20_FID, M20_DOY)
Stack3 <- stack(Fire19_FID, M19_DOY)
#Extract Raster data to dataset
Sys.time()
print("start extraction")
beginCluster(n=8) #Assign N 
L2A_all <- spTransform(L2A_all, CRS=proj4string(Fire_FID))
L2A_all<-extract(Stack1, L2A_all, sp=TRUE)
L2A_all <- spTransform(L2A_all, CRS=proj4string(Fire20_FID))
L2A_all<-extract(Stack2, L2A_all, sp=TRUE)
L2A_all <- spTransform(L2A_all, CRS=proj4string(Fire19_FID))
L2A_all<-extract(Stack3, L2A_all, sp=TRUE)
L2A_all <- spTransform(L2A_all, CRS=proj4string(ROI))
L2A_all<-extract(ROI, L2A_all, sp=TRUE)
endCluster()
Sys.time()
print("extraction done")

dim(L2A_all)
L2A<-subset(L2A_all, ROI_30==1)
dim(L2A)
table(L2A$ROI_30)

L2A$atlas_start_code<-paste0(2020,L2A$Fire_atlas_start_DOY)
L2A$atlas_end_code<-paste0(2020,L2A$Fire_atlas_end_DOY)
L2A$M19_Code<-paste0(2019,L2A$M19_DOY)
L2A$M20_Code<-paste0(2020,L2A$M20_DOY)

#Creaing codes for M19 M20 and Atlas

Sys.time()
print("Creating burn status code")
L2A$Atlas_Burn_stat<-ifelse(L2A$Day_code < L2A$atlas_start_code, print("Pre-Burn"),
                            ifelse(L2A$Day_code > L2A$atlas_end_code, print("Post-Burn"), 
                                   print("Inter")))

Sys.time()
print("M19 Creating burn status code")
L2A$M19_Burn_stat<-ifelse(L2A$Day_code < L2A$M19_Code, print("Pre-Burn"),
                          ifelse(L2A$Day_code == L2A$M19_Code, print("Inter"), 
                                 print("Post-Burn")))

print("Creating burn status code")
L2A$M20_Burn_stat<-ifelse(L2A$Day_code < L2A$M20_Code, print("Pre-Burn"),
                          ifelse(L2A$Day_code == L2A$M20_Code, print("Inter"), 
                                 print("Post-Burn")))

#############################

#Write out shapfiles
Sys.time()
#print("Write out L2A_Fire.json")
#writeOGR(L2A, dsn=paste0("/mnt/lustrefs/scratch/v38p156/Fire_Summary_v002/Fire_Regions/L2A_Fire_",n,".geojson"), layer=paste0("L2A_Fire_",n), driver="GeoJSON") 
# writeOGR(L2A, dsn = "/mnt/lustrefs/scratch/v38p156/Fire_Summary_v002/Fire_regions", layer = paste0("L2A_Fire_",n), driver = "ESRI Shapefile")
writeOGR(subset(L2A, M20_FID>0 | M19_FID>0 | Fire_atlas_FID>0), dsn = "/mnt/lustrefs/scratch/v38p156/Fire_Summary_v002/Fire_Regions/Burn_Pts", layer = paste0("L2A_InFire_",n), driver = "ESRI Shapefile")
data_out<-subset(L2A, M20_FID>0 | M19_FID>0 | Fire_atlas_FID>0)
data_out_df<-data_out@data
write.csv(data_out_df, paste0("/mnt/lustrefs/scratch/v38p156/Fire_Summary_v002/Fire_Regions/Burn_Pts/L2A_InFire_",n,".csv"))
Sys.time()
#print("Write out L2A.shp done")

# #Start rasterizing
# #Pre
# L2A_pre<-subset(L2A, M19_DOY<1 | M19_Burn_stat=="Pre-Burn")
# print("Read in Raster and sync")
# 
# blank<-raster("/mnt/lustrefs/scratch/v38p156/Rasters/Blank_19.tif")
# L2A_pre <- spTransform(L2A_pre, CRS=proj4string(blank))
# 
# print("Rasterize pre 500")
# Sys.time()
# grid_pre_mean<-rasterize(L2A_pre, blank, "rh_sum", fun=mean)
# grid_pre_count<-rasterize(L2A_pre, blank, "rh_sum", fun="count")
# grid_pre_sd<-rasterize(L2A_pre, blank, "rh_sum", fun=sd)
# Sys.time()
# print("Write Rasters")
# writeRaster(grid_pre_mean, paste0("/mnt/lustrefs/scratch/v38p156/Fire_Summary_v002/M19_Rasters/500m/Pre/Mean/pre_mean",n,".tif"))
# writeRaster(grid_pre_count, paste0("/mnt/lustrefs/scratch/v38p156/Fire_Summary_v002/M19_Rasters/500m/Pre/Count/pre_count",n,".tif"))
# writeRaster(grid_pre_sd, paste0("/mnt/lustrefs/scratch/v38p156/Fire_Summary_v002/M19_Rasters/500m/Pre/sd/pre_sd",n,".tif"))
# 
# #Post
# L2A_post<-subset(L2A, M19_DOY>0 & M19_Burn_stat=="Post-Burn")
# 
# L2A_post <- spTransform(L2A_post, CRS=proj4string(blank))
# 
# print("Rasterize post 500")
# Sys.time()
# grid_post_mean<-rasterize(L2A_post, blank, "rh_sum", fun=mean)
# grid_post_count<-rasterize(L2A_post, blank, "rh_sum", fun="count")
# grid_post_sd<-rasterize(L2A_post, blank, "rh_sum", fun=sd)
# Sys.time()
# 
# print("Write Rasters")
# writeRaster(grid_post_mean, paste0("/mnt/lustrefs/scratch/v38p156/Fire_Summary_v002/M19_Rasters/500m/Post/Mean/post_mean",n,".tif"))
# writeRaster(grid_post_count, paste0("/mnt/lustrefs/scratch/v38p156/Fire_Summary_v002/M19_Rasters/500m/Post/Count/post_count",n,".tif"))
# writeRaster(grid_post_sd, paste0("/mnt/lustrefs/scratch/v38p156/Fire_Summary_v002/M19_Rasters/500m/Post/sd/post_sd",n,".tif"))
# 
# #Raterize 30m
# #Start rasterizing
# #Pre
# print("Read in Raster and sync")
# blank<-raster("/mnt/lustrefs/scratch/v38p156/Rasters/Blank_30m.tif")
# L2A_pre <- spTransform(L2A_pre, CRS=proj4string(blank))
# 
# print("Rasterize pre 30")
# Sys.time()
# grid_pre_mean<-rasterize(L2A_pre, blank, "rh_sum", fun=mean)
# Sys.time()
# print("Write Rasters")
# writeRaster(grid_pre_mean, paste0("/mnt/lustrefs/scratch/v38p156/Fire_Summary_v002/M19_Rasters/30m/Pre/Mean/pre_mean",n,".tif"))
# print("DONE!")
# 
# #Post
# L2A_post<-subset(L2A, M19_DOY>0 & M19_Burn_stat=="Post-Burn")
# 
# L2A_post <- spTransform(L2A_post, CRS=proj4string(blank))
# 
# print("Rasterize post 30")
# Sys.time()
# grid_post_mean<-rasterize(L2A_post, blank, "rh_sum", fun=mean)
# Sys.time()
# 
# print("Write Rasters")
# writeRaster(grid_post_mean, paste0("/mnt/lustrefs/scratch/v38p156/Fire_Summary_v002/M19_Rasters/30m/Post/Mean/post_mean",n,".tif"))


print("I'm DONE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")