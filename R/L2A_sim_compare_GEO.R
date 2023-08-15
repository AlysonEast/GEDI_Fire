library(sp)
library(raster)
library(rgdal)
library(lidR)
library(ggplot2)
library(hdf5r)
library(devtools)
#library(rGEDI)
library(ggpubr)
library(viridis)
library(effects)
library(matrixStats)
library(rgeos)
.rs.unloadPackage("tidyr")

#### Reading in the  data ####
#Read in non Geolocation corrected GEDI sim data from file directory
file_list <- list.files(path="G:/Thesis/Spatial/LiDAR/SIM_geo/Version_3/", full.names=FALSE)
#file_list<- file_list[3:46]
file_list
path<-"G:/Thesis/Spatial/LiDAR/SIM_geo/Version_3/"

RH_sim <- read.delim(file=paste0(path,file_list[1]), sep = ",", header = TRUE)
RH_sim_colnames<-colnames(RH_sim)

for (i in 1:1){
  RH_sim <- read.delim(file=paste0(path,file_list[i]), sep = " ")
  RH_sim<-RH_sim[,c(1:469)]
  colnames(RH_sim)<-RH_sim_colnames
  print(file_list[i])
}
for (i in 2:length(file_list)){
  temp_data <- read.delim(file=paste0(path,file_list[i]), sep = " ")
  temp_data<-temp_data[,c(1:469)]
  colnames(temp_data)<-RH_sim_colnames
  RH_sim <- rbind(RH_sim, temp_data)
  print(file_list[i])
}

#Writing out data to read into ArcGIS for visualization and SPDF creation, and run a NEAR table
#write.csv(RH_sim, "G:/Thesis/Spatial/Scratch/Collocated_v3.csv") #This is edited

# Add on #took all this out.. but might need still####
# file_list <- list.files(path="G:/Thesis/Spatial/LiDAR/SIM_geo/AddOn/", full.names=FALSE)
# path<-"G:/Thesis/Spatial/LiDAR/SIM_geo/AddOn/"
# 
# RH_simAdd <- read.delim(file=paste0(path,file_list[1]), sep = ",", header = TRUE)
# RH_sim_colnames<-colnames(RH_simAdd)
# 
# for (i in 1:1){
#   RH_simAdd <- read.delim(file=paste0(path,file_list[i]), sep = " ")
#   RH_simAdd<-RH_simAdd[,c(1:469)]
#   colnames(RH_simAdd)<-RH_sim_colnames
#   print(file_list[i])
# }
# for (i in 2:length(file_list)){
#   temp_data <- read.delim(file=paste0(path,file_list[i]), sep = " ")
#   temp_data<-temp_data[,c(1:469)]
#   colnames(temp_data)<-RH_sim_colnames
#   RH_simAdd <- rbind(RH_simAdd, temp_data)
#   print(file_list[i])
# }
# #write.csv(RH_simAdd, "G:/Thesis/Spatial/Scratch/Collocated_addon.csv")
# 
# RH_sim<-rbind(RH_sim, RH_simAdd)

#Read in L2A data from shapfile
L2A<-readOGR(dsn="G:/Thesis/Spatial/LiDAR", layer = "L2A_ALS_overlap_All_wFID")
#View(L2A@data)
#L2A_df<-read.csv("G:/Thesis/Spatial/LiDAR/L2A_ALS_overlap_All_wFID_Z21.csv")

# Reading in the Geolocation corrected data
Collo19<-readOGR(dsn="G:/Thesis/Spatial/LiDAR/Shps", layer = "Collocated_Zone19_v4")
Collo21<-readOGR(dsn="G:/Thesis/Spatial/LiDAR/Shps", layer = "Collocated_Zone21_v4")
#ColloAdd<-readOGR(dsn="G:/Thesis/Spatial/LiDAR/Shps", layer = "Collocated_addon")

Collo19<-spTransform(Collo19, crs(L2A))
Collo21<-spTransform(Collo21, crs(L2A))
#ColloAdd<-spTransform(ColloAdd, crs(L2A))
crs(Collo19)
crs(Collo21)
crs(L2A)
plot(L2A, pch=1)
plot(Collo19, pch=2, col="red", add=TRUE)
plot(Collo21, pch=2, col="red", add=TRUE)

Collo<-rbind(Collo19, Collo21)#, ColloAdd)


#bring in Land cover rasters
LC18_1<-raster("G:/Thesis/Spatial/Rasters/MAPBIOMAS/LandCover/2018/mapbiomas-amazon-collection-30-matogrossobrasil-2018.tif")
LC18_2<-raster("G:/Thesis/Spatial/Rasters/MAPBIOMAS/LandCover/2018/mapbiomas-amazon-collection-30-para-2018.tif")
LC18_3<-raster("G:/Thesis/Spatial/Rasters/MAPBIOMAS/LandCover/2018/mapbiomas-amazon-collection-30-acrebrasil-2018.tif")

LC19<-raster("G:/Thesis/Spatial/Rasters/MAPBIOMAS/LandCover/2019/LC19.tif")

LC20_1<-raster("G:/Thesis/Spatial/Rasters/MAPBIOMAS/LandCover/2020/mapbiomas-amazon-collection-30-matogrossobrasil-2020.tif")
LC20_2<-raster("G:/Thesis/Spatial/Rasters/MAPBIOMAS/LandCover/2020/mapbiomas-amazon-collection-30-para2020.tif")
LC20_3<-raster("G:/Thesis/Spatial/Rasters/MAPBIOMAS/LandCover/2020/mapbiomas-amazon-collection-30-acrebrasil-2020.tif")

Slope_1<-raster("G:/Thesis/Spatial/Rasters/Terrain/slope-0000000000-0000032768-002.tif")
Slope_2<-raster("G:/Thesis/Spatial/Rasters/Terrain/slope-0000000000-0000000000-003.tif")
Slope_3<-raster("G:/Thesis/Spatial/Rasters/Terrain/slope-0000032768-0000032768.tif")
landforms<-raster("G:/Thesis/Spatial/Rasters/Terrain/landforms.tif")


c(colnames(Collo@data),"LC18_1")
Collo_add<-extract(LC18_1, Collo, sp=TRUE)
Collo_add<-extract(LC18_2, Collo_add, sp=TRUE)
Collo_add<-extract(LC18_3, Collo_add, sp=TRUE)
Collo_add<-extract(LC19, Collo_add, sp=TRUE)
Collo_add<-extract(LC20_1, Collo_add, sp=TRUE)
Collo_add<-extract(LC20_2, Collo_add, sp=TRUE)
Collo_add<-extract(LC20_3, Collo_add, sp=TRUE)
Collo_add<-extract(Slope_1, Collo_add, sp=TRUE)
Collo_add<-extract(Slope_2, Collo_add, sp=TRUE)
Collo_add<-extract(Slope_3, Collo_add, sp=TRUE)
Collo_add<-extract(landforms, Collo_add, sp=TRUE)
colnames(Collo_add@data)<-c(colnames(Collo@data),"LC18_1","LC18_2","LC18_3","LC19","LC20_1","LC20_2","LC20_3",
                          "Slope_1","Slope_2","Slope_3","landforms")

Collo_add@data[,c("LC18_1","LC18_2","LC18_3")][is.na(Collo_add@data[,c("LC18_1","LC18_2","LC18_3")])]<-0
Collo@data$LC18<-rowSums((Collo_add@data[,c("LC18_1","LC18_2","LC18_3")]))
table(Collo@data$LC18)
Collo@data$LC19<-Collo_add@data$LC19
Collo_add@data[,c("LC20_1","LC20_2","LC20_3")][is.na(Collo_add@data[,c("LC20_1","LC20_2","LC20_3")])]<-0
Collo@data$LC20<-rowSums((Collo_add@data[,c("LC20_1","LC20_2","LC20_3")]))
table(Collo@data$LC20)
Collo_add@data[,c("Slope_1","Slope_2","Slope_3")][is.na(Collo_add@data[,c("Slope_1","Slope_2","Slope_3")])]<-0
Collo@data$slope<-rowSums((Collo_add@data[,c("Slope_1","Slope_2","Slope_3")]))
hist(Collo@data$slope)

M_CCI_18<-raster("G:/Thesis/Spatial/Rasters/CCI/BurnCat_18_MODIS_CCI.tif")
MB_18<-raster("G:/Thesis/Spatial/Rasters/MAPBIOMAS/Fire/2018/Mapbio18-0000065536-0000065536.tif")

Modis19<-raster("G:/Thesis/Spatial/M19_DOY.tif")
CCI19<-raster("G:/Thesis/Spatial/Rasters/CCI/CCI19_DOY.tif")
MB_19<-raster("G:/Thesis/Spatial/MAPBIOMAS-EXPORT/MAPBIOMAS_Fire.tif")

Collo_add<-extract(M_CCI_18, Collo_add, sp=TRUE)
Collo_add<-extract(MB_18, Collo_add, sp=TRUE)
Collo_add<-extract(Modis19, Collo_add, sp=TRUE)
Collo_add<-extract(CCI19, Collo_add, sp=TRUE)
Collo_add<-extract(MB_19, Collo_add, sp=TRUE)

#Collo@data$Burn_18<-rowMaxs(as.matrix(Collo_add@data[,c("BurnCat_18_MODIS_CCI","Mapbio18.0000065536.0000065536")]))
Collo@data$Burn_18<-rowMaxs(as.matrix(Collo_add@data[,c("constant","burned_coverage_2018")]))
table(Collo@data$Burn_18)
Collo@data[,c("Burn_18")][is.na(Collo@data[,c("Burn_18")])]<-0
Collo@data$landforms<-Collo_add@data$landforms
Collo@data$Burn_19<-rowMaxs(as.matrix(Collo_add@data[,c("MAPBIOMAS_Fire","CCI19_DOY","M19_DOY")]))
table(Collo@data$Burn_19)
Collo@data[,c("Burn_19")][Collo@data[,c("Burn_19")] > 0] <-1


L2A_spdf<-Collo@data
L2A_spdf$LC_d18_19<-L2A_spdf$LC19-L2A_spdf$LC18
L2A_spdf$LC_d18_20<-L2A_spdf$LC20-L2A_spdf$LC18
L2A_spdf<-L2A_spdf[,c(2,256:ncol(L2A_spdf))]
RH_sim<-merge(RH_sim, L2A_spdf, by.x="X..1.wave.ID",by.y="X__1_wave_")

# #Making L2A data comparable to collocated data
L2A@data$ID<-c(1:nrow(L2A@data))
L2A@data$ID<-L2A@data$ID-1
L2A_df<-L2A@data
#L2A_df$FileID<-paste0(L2A_df$year, L2A_df$day, L2A_df$hour, L2A_df$min, L2A_df$mi)

#Finding Distances between Points
# library(geosphere)
# d <- distm(Collo,L2A)
# RH_sim$min_dist<-rowMins(d)
# RH_sim$min_id<-as.matrix(apply(d,1,which.min))

table(RH_sim$NEAR_FID)
str(RH_sim$NEAR_FID)
RH_sim<-subset(RH_sim, NEAR_FID!="-1")
#Creating unique Pairs
library(tidyverse)
library(dplyr)
RH_sim_unique<-RH_sim %>%
  group_by(NEAR_FID) %>%
  arrange(NEAR_DIST) %>%
  slice(1)

#Merge L2a and GEDI sim data
L2A_compare<-merge(L2A_df, RH_sim_unique, by.x="ID", by.y="NEAR_FID")
hist(L2A_compare$NEAR_DIST)

# L2A_compare<-subset(L2A_compare, NEAR_DIST<34)
# hist(L2A_compare$NEAR_DIST)

#write a shapefile with just paired points
L2A_spdf<-merge(L2A, RH_sim_unique, by.x="FID", by.y="wave.ID", all.x=FALSE)
#L2A_spdf <- L2A[L2A$FID %in% L2A_compare$FID, ]
#rgdal::writeOGR(L2A_spdf, dsn="G:/Thesis/Spatial/LiDAR", layer = "Collocate_Compare_v4", driver = "ESRI Shapefile")
#L2A_spdf_far <- L2A[L2A$FID %in% subset(L2A_compare, NEAR_DIST>34)$FID, ]
#rgdal::writeOGR(L2A_spdf_far, dsn="G:/Thesis/Spatial/LiDAR", layer = "Collocate_Compare_v3_far", driver = "ESRI Shapefile")

#Summarize land use and changed in paired data####
#L2A_compare$day<-paste0(substr(L2A_compare$X418.filename,43,45))
#L2A_compare$year<-paste0(substr(L2A_compare$X418.filename,39,42))
#hist(as.numeric(L2A_compare$Dy_cd))

#L2A_compare<-subset(L2A_compare, FID!="3367" & FID!="3668" & FID!="3669" & FID!="3815" & FID!="3153" & FID!="3781")

table(L2A_compare$Yr_Ls)
hist(L2A_compare$Yr_Ls)

table(L2A_compare$Burn_18)

table(L2A_compare$LC19)
table(L2A_compare$LC18)
L2A_compare$LC_d18_19<-L2A_compare$LC19.y-L2A_compare$LC18

table(L2A_compare$LC_d18_19)
L2A_compare[,c("LC_d18_19")][L2A_compare[,c("LC_d18_19")] > 0] <-1
L2A_compare[,c("LC_d18_19")][L2A_compare[,c("LC_d18_19")] < 0] <-1
table(L2A_compare$LC_d18_20)
L2A_compare[,c("LC_d18_20")][L2A_compare[,c("LC_d18_20")] > 0] <-1
L2A_compare[,c("LC_d18_20")][L2A_compare[,c("LC_d18_20")] < 0] <-1

#### Omitting data based on temporal change####
#### START HERE LOOK FOR NAS CASUINGG BAD DATA OMISSION####
dim(L2A_compare)
table(L2A_compare$Burn_18)
table(L2A_compare$Burn_19)
table(L2A_compare$LC_d18_19)
L2A_compare1<-subset(L2A_compare, Burn_18!=1 & Burn_19<1 & LC_d18_19==0)
dim(L2A_compare1)
table(L2A_compare1$LC19.y)
table(L2A_compare1$LC_d18_20)
table(subset(L2A_compare1, LC_d18_20!=0)$year)
dim(L2A_compare1)
L2A_compare1<-subset(L2A_compare1, year==2019 | year==2020 & LC_d18_20==0)
dim(L2A_compare1)
table(L2A_compare1$Burn_18)
L2A_compare1<-subset(L2A_compare1, LC18==3 | LC18==14 | LC18==12)

lost<-length(L2A_compare$FID)-length(L2A_compare1$FID) 
length(L2A_compare$FID)
lost/length(L2A_compare$FID)*100

#### Merging the non-geolocation corrected dataset with the rh.GAUSS values from the geolocation error corrected dataset####


#### Adding in catigorical and derrived metrics ####

#Make a temp dataset and set all negitive values of rh to 0
# L2A_compare2<-L2A_compare1
# L2A_compare2[,c(173:273)][L2A_compare2[,c(173:273)] < 0] <-0
# L2A_compare2[,c(280:380)][L2A_compare2[,c(280:380)] < 0] <-0
# L2A_compare2[,c(381:481)][L2A_compare2[,c(381:481)] < 0] <-0
#Take RH sum
# L2A_compare1$rhGauss_sum<-rowSums((L2A_compare2[,c(173:273)]))
# L2A_compare1$rhMax_sum<-rowSums((L2A_compare2[,c(280:380)]))
# L2A_compare1$rhinfl_sum<-rowSums((L2A_compare2[,c(381:481)]))

# plot(L2A_compare1$rh_sm~L2A_compare1$rhGauss_sum)
# plot(L2A_compare1$rh_sm~L2A_compare1$rhMax_sum)
# plot(L2A_compare1$rh_sm~L2A_compare1$rhinfl_sum)

# Create a beam strenght classifcation
L2A_compare1$BeamType<- ifelse(L2A_compare1$BEAM=="BEAM0101"|
                                 L2A_compare1$BEAM=="BEAM0110"|
                                 L2A_compare1$BEAM=="BEAM1000"|
                                 L2A_compare1$BEAM=="BEAM1011", 
                               print("Full"), print("Coverage")) 
table(L2A_compare1$BeamType)

table(L2A_compare1$X418.filename)
## /home/v38p156/Sim/Version_3/Zone21/addon_GEDI01_B_2019266175745_O04423_01_T01604_02_005_01_V002.h5
## /home/v38p156/Sim/Version_3/Zone21/addon_GEDI01_B_2020011222012_O06132_01_T04297_02_005_01_V002.h5
##/home/v38p156/Sim/Version_3/Zone21/addon_GEDI01_B_2021165210133_O14192_04_T07564_02_005_02_V002.h5 
##/home/v38p156/Sim/Version_3/Zone21/addon_GEDI01_B_2021169192836_O14253_04_T00296_02_005_02_V002.h5 
##/home/v38p156/Sim/Version_3/Zone21/addon_GEDI01_B_2021249115827_O15488_04_T08834_02_005_02_V002.h5
## /home/v38p156/Sim/Version_3/Zone21/SIM_GEDI01_B_2019179162610_O03072_04_T01122_02_005_01_V002.h5 
##  /home/v38p156/Sim/Version_3/Zone21/SIM_GEDI01_B_2019220124906_O03706_01_T03670_02_005_01_V002.h5
## /home/v38p156/Sim/Version_3/Zone21/SIM_GEDI01_B_2020179044014_O08725_01_T01436_02_005_01_V002.h5 
## /home/v38p156/Sim/Version_3/Zone21/SIM_GEDI01_B_2020190235733_O08908_01_T00013_02_005_01_V002.h5
## /home/v38p156/Sim/Version_3/Zone21/SIM_GEDI01_B_2021109070431_O13315_01_T11259_02_005_02_V002.h5 
## /home/v38p156/Sim/Version_3/Zone21/SIM_GEDI01_B_2021177162315_O14375_04_T05835_02_005_02_V002.h5 
# /home/v38p156/Sim/Version_3/Zone21/SIM_GEDI01_B_2021253102652_O15549_04_T10104_02_005_02_V002.h5 
## /home/v38p156/Sim/Version_3/Zone21/SIM_GEDI01_B_2021284094850_O16029_01_T08413_02_005_02_V002.h5 
L2A_compare1<-subset(L2A_compare1,
                     X418.filename!="/home/v38p156/Sim/Version_3/Zone21/addon_GEDI01_B_2019266175745_O04423_01_T01604_02_005_01_V002.h5" &
                     X418.filename!="/home/v38p156/Sim/Version_3/Zone21/addon_GEDI01_B_2020011222012_O06132_01_T04297_02_005_01_V002.h5" &
                     X418.filename!="/home/v38p156/Sim/Version_3/Zone21/addon_GEDI01_B_2021165210133_O14192_04_T07564_02_005_02_V002.h5" &
                     X418.filename!="/home/v38p156/Sim/Version_3/Zone21/addon_GEDI01_B_2021169192836_O14253_04_T00296_02_005_02_V002.h5" &
                     X418.filename!="/home/v38p156/Sim/Version_3/Zone21/addon_GEDI01_B_2021249115827_O15488_04_T08834_02_005_02_V002.h5" &
                     X418.filename!="/home/v38p156/Sim/Version_3/Zone21/SIM_GEDI01_B_2019179162610_O03072_04_T01122_02_005_01_V002.h5" &
                     X418.filename!="/home/v38p156/Sim/Version_3/Zone21/SIM_GEDI01_B_2019220124906_O03706_01_T03670_02_005_01_V002.h5" &
                     X418.filename!="/home/v38p156/Sim/Version_3/Zone21/SIM_GEDI01_B_2020179044014_O08725_01_T01436_02_005_01_V002.h5" &
                     X418.filename!="/home/v38p156/Sim/Version_3/Zone21/SIM_GEDI01_B_2020190235733_O08908_01_T00013_02_005_01_V002.h5" &
                     X418.filename!="/home/v38p156/Sim/Version_3/Zone21/SIM_GEDI01_B_2021109070431_O13315_01_T11259_02_005_02_V002.h5" &
                     X418.filename!="/home/v38p156/Sim/Version_3/Zone21/SIM_GEDI01_B_2021177162315_O14375_04_T05835_02_005_02_V002.h5" &
                     X418.filename!="/home/v38p156/Sim/Version_3/Zone21/SIM_GEDI01_B_2021253102652_O15549_04_T10104_02_005_02_V002.h5" &
                     X418.filename!="/home/v38p156/Sim/Version_3/Zone21/SIM_GEDI01_B_2021284094850_O16029_01_T08413_02_005_02_V002.h5")
table(L2A_compare1$X418.filename)

#remove low beam density and low point density footprint. 
L2A_compare1<-subset(L2A_compare1, 
                     X434.pointDense>=5 &
                     X435.beamDense>=5)


L2A_compare1<-subset(L2A_compare1, X415.rhReal.98<100)
dim(L2A_compare1)
#Create rh_sum metrics that goe from 0 to 98 instead of 0 to 100
# L2A_compare2[,c(16:114)][L2A_compare2[,c(16:114)] < 0] <-0


#Take out beams with known issuse
#L2A_compare1<-subset(L2A_compare1, BEAM!="BEAM0000" &BEAM!="BEAM0001")

#make hour a numeric value
L2A_compare1$CC_snstv_diff<-L2A_compare1$snstv-L2A_compare1$X11.cover
L2A_compare1$day<-as.numeric(L2A_compare1$day)
L2A_compare1$date_index<-ifelse(L2A_compare1$year==2019, print(L2A_compare1$day), 
                                print(L2A_compare1$day+365))
L2A_compare1$LC18<-as.factor(L2A_compare1$LC18)

#plotting metric aggrement with density as the color
get_density <- function(x, y, ...) {
  dens <- MASS::kde2d(x, y, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}

# Calculate error ####
library(Metrics)
L2A_error<-(L2A_compare1[,c(16:116)] - L2A_compare1[,c(173:273)])

RMSE<-sqrt(mean((L2A_compare1[,c(179)] - L2A_compare1[,c(16)])^2))
rss <- sum((L2A_compare1[,c((279))]- L2A_compare1[,c((116))])^ 2)  ## residual sum of squares
tss <- sum((L2A_compare1[,c((116))] - mean(L2A_compare1[,c((116))])) ^ 2)  ## total sum of squares
1 - rss/tss

error_summary<-as.data.frame(c(1:98))
colnames(error_summary)<-c("rh")
error_summary$RMSE<-0
error_summary$rsq<-0
for (i in 1:98) {
  error_summary[i,2]<-sqrt(mean((L2A_compare1[,c((173+i))] - L2A_compare1[,c((15+i))])^2))
  rss <- sum((L2A_compare1[,c((173+i))]- L2A_compare1[,c((15+i))])^ 2)  ## residual sum of squares
  tss <- sum((L2A_compare1[,c((15+i))] - mean(L2A_compare1[,c((15+i))])) ^ 2)  ## total sum of squares
  error_summary[i,3] <- 1 - rss/tss
}
par(mfrow=c(2,1))
plot(rh~RMSE, data=error_summary)
plot(rh~rsq, data=error_summary)


L2A_compare1$rh_50error<-(L2A_compare1$rh_50 - L2A_compare1$X64.rhGauss.50)
L2A_compare1$rh_75error<-(L2A_compare1$rh_75 - L2A_compare1$X89.rhGauss.75)
L2A_compare1$rh_98error<-(L2A_compare1$rh_98 - L2A_compare1$X112.rhGauss.98)
#L2A_compare1$rh_sumerror<-(L2A_compare1$rh_sum98 - L2A_compare1$rhGauss_sum98)

par(mfrow=c(2,2))
hist(L2A_compare1$rh_50error)
hist(L2A_compare1$rh_75error)
hist(L2A_compare1$rh_98error)
#hist(L2A_compare1$rh_sumerror)

#Creating binaries for error
df50<-data.frame("rh50"=L2A_compare1$rh_50error[order(L2A_compare1$rh_50error)],"index"=c(1:length(L2A_compare1$rh_50error)))
df75<-data.frame("rh75"=L2A_compare1$rh_75error[order(L2A_compare1$rh_75error)],"index"=c(1:length(L2A_compare1$rh_75error)))
df98<-data.frame("rh98"=L2A_compare1$rh_98error[order(L2A_compare1$rh_98error)],"index"=c(1:length(L2A_compare1$rh_98error)))
L2A_compare1$high_error<-ifelse(L2A_compare1$rh_50error<=(mean(df50$rh50)-(2*sd(df50$rh50))), print(1), 
                                ifelse(L2A_compare1$rh_50error>=(mean(df50$rh50)+(2*sd(df50$rh50))), print(1),
                                       ifelse(L2A_compare1$rh_75error<=(mean(df75$rh75)-(2*sd(df75$rh75))), print(1),
                                              ifelse(L2A_compare1$rh_75error>=(mean(df75$rh75)+(2*sd(df75$rh75))), print(1),
                                                     ifelse(L2A_compare1$rh_98error<=(mean(df98$rh98)-(2*sd(df98$rh98))), print(1),
                                                            ifelse(L2A_compare1$rh_98error>=(mean(df98$rh98)+(2*sd(df98$rh98))), print(1),
                                                                   print(0)))))))

L2A_compare1$errorGT10<-ifelse(L2A_compare1$rh_50error<=-10, print(1), 
                               ifelse(L2A_compare1$rh_50error>=10, print(1),
                                      ifelse(L2A_compare1$rh_75error<=-10, print(1),
                                             ifelse(L2A_compare1$rh_75error>=10, print(1),
                                                    ifelse(L2A_compare1$rh_98error<=-10, print(1),
                                                           ifelse(L2A_compare1$rh_98error>=10, print(1),
                                                                  print(0)))))))
L2A_compare1$rh_50error10<-ifelse(L2A_compare1$rh_50error<=-10, print(1), 
                                  ifelse(L2A_compare1$rh_50error>=10, print(1), print(0)))
L2A_compare1$rh_98error10<-ifelse(L2A_compare1$rh_98error<=-10, print(1), 
                                  ifelse(L2A_compare1$rh_98error>=10, print(1), print(0)))
L2A_compare1$rh_75error10<-ifelse(L2A_compare1$rh_75error<=-10, print(1), 
                                  ifelse(L2A_compare1$rh_75error>=10, print(1), print(0)))


table(L2A_compare1$high_error)

L2A_compare1$Forest<-ifelse(L2A_compare1$LC18=="3", print(1),print(0))
L2A_compare1$Forest<-as.factor(L2A_compare1$Forest)
L2A_compare1$slct_<-as.factor(L2A_compare1$slct_)

#Removing by Quality flag ####
L2A_compareWflags<-L2A_compare1
dim(L2A_compare1)
L2A_compare1<-subset(L2A_compare1, dgrd_==0)
dim(L2A_compare1)
L2A_compare1<-subset(L2A_compare1, elv__==0)
dim(L2A_compare1)

error_summary2<-as.data.frame(c(1:98))
colnames(error_summary2)<-c("rh")
error_summary2$RMSE<-0
error_summary2$rsq<-0
for (i in 1:98) {
  error_summary2[i,2]<-sqrt(mean((L2A_compare1[,c((173+i))] - L2A_compare1[,c((15+i))])^2))
  rss <- sum((L2A_compare1[,c((173+i))]- L2A_compare1[,c((15+i))])^ 2)  ## residual sum of squares
  tss <- sum((L2A_compare1[,c((15+i))] - mean(L2A_compare1[,c((15+i))])) ^ 2)  ## total sum of squares
  error_summary2[i,3] <- 1 - rss/tss
}

#plotting the change in error after quality filtering
par(mfrow=c(2,2))
plot(rh~RMSE, data=error_summary)
plot(rh~rsq, data=error_summary)
plot(rh~RMSE, data=error_summary2)
plot(rh~rsq, data=error_summary2)

par(mfrow=c(1,2))
plot(rh~RMSE, data=error_summary, xlim = c(3,6.5))
points(rh~RMSE, data=error_summary2, col="red", xlim = c(3,6.5))
plot(rh~rsq, data=error_summary, xlim = c(0,1))
points(rh~rsq, data=error_summary2, xlim = c(0,1), col="red")




#Spliting out a training and testing dataset####
set.seed(123)
dt <- sort(sample(nrow(L2A_compare1), nrow(L2A_compare1)*.5))
train<-L2A_compare1[dt,]
test<-L2A_compare1[-dt,]

par(mfrow=c(1,1))
#### Linear Models ####
par(mfrow=c(1,1))
hist(train$rh_50error)
hist(train$rh_75error)
hist(train$rh_98error)
plot(rh_98error~snstv, data=train)
plot(rh_98error~time, data=train)
plot(rh_98error~CC_snstv_diff, data=train)
hist(train$CC_snstv_diff)
boxplot(rh_98error~BeamType, data=train)

library(psych)
pairs.panels(train[,c("rh_50error","rh_75error","rh_98error")], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)
pairs.panels(train[,c("rh_98error","snstv","X11.cover","CC_snstv_diff","BeamType","date_index","LC18","slct_","slr_l","night")], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)
train$landforms<-as.factor(train$landforms)
train$night<-ifelse(train$slr_l<0, paste0(1), paste(0))
train$night<-as.factor(train$night)
#full models with different interactions
m1<-glm(rh_98error~snstv*X11.cover+I(X11.cover^2)+BeamType+date_index+Forest+slct_+night+slope+landforms, data = train)
summary(m1)
m2<-glm(rh_98error~snstv*CC_snstv_diff+I(CC_snstv_diff^2)+BeamType+date_index+Forest+slct_+night+slope+landforms, data = train)
summary(m2)
m3<-glm(rh_98error~snstv*BeamType+CC_snstv_diff+I(CC_snstv_diff^2)+date_index+Forest+slct_+night+slope+landforms, data = train)
summary(m3)
m4<-glm(rh_98error~snstv*night+CC_snstv_diff+I(CC_snstv_diff^2)+BeamType+date_index+Forest+slct_+slope+landforms, data = train)
summary(m4)
m5<-glm(rh_98error~snstv*BeamType*night+CC_snstv_diff+I(CC_snstv_diff^2)+date_index+Forest+slct_+slope+landforms, data = train)
summary(m5)
m6<-glm(rh_98error~snstv*BeamType*CC_snstv_diff+I(CC_snstv_diff^2)+date_index+Forest+slct_+night+slope+landforms, data = train)
summary(m6)
AIC(m1, m2, m3, m4, m5, m6)

summary(m3)

#model refinement
m7<-glm(rh_98error~snstv*BeamType+CC_snstv_diff+I(CC_snstv_diff^2)+date_index+slct_+night+slope+landforms, data = train)
summary(m7)
m8<-glm(rh_98error~snstv*BeamType+CC_snstv_diff+I(CC_snstv_diff^2)+date_index+night+slope, data = train)
summary(m8)
m9<-glm(rh_98error~snstv*BeamType+CC_snstv_diff+I(CC_snstv_diff^2)+date_index+slope, data = train)
summary(m9)
m10<-glm(rh_98error~snstv*BeamType+CC_snstv_diff+I(CC_snstv_diff^2)+slope, data = train)
summary(m10)
m11<-glm(rh_98error~snstv+BeamType+CC_snstv_diff+I(CC_snstv_diff^2)+slope, data = train)
summary(m11)

AIC(m3, m7, m8, m9, m10, m11)

library(stargazer)
stargazer(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11,
          type="html",
          out="G:/Thesis/Figures/rh98_linear.doc",
          title = "Generalized Linear Models",
          intercept.bottom = F,
          intercept.top = T,
          ci = FALSE, digits=1,
          notes = "This is a caption.",
          model.names = T,
          single.row = F,
          covariate.labels = xtable::sanitize(c("Constant",
                               "Sensitivity",
                               "Canopy Cover", "Canopy Cover sq",
                               "Canopy Sensitivity Diff","Canopy Sensitivity Diff sq",
                               "Beam Type",
                               "Date",
                               "Forest",
                               "Algorithm Number",
                               "Night",
                               "Slope",
                               "Landform=Upper Slope (Flat)","Landform=Lower Slope (Warm)","Landform=Lower Slope","Landform=Valley","Landform=Narrow Valley",
                               "Sensitivity : CanopyCover",
                               "Sensitivity : Canopy Sensitivity Diff",
                               "BeamType : Canopy Sensitivity Diff",
                               "Sensitivity : Beam Type : Canopy Sensitivity Diff", 
                               "Sensitivity : BeamType", 
                               "Sensitivity : Night",
                               "BeamType : Night",
                               "Sensitivity : BeamType : Night"),type="latex"),
          dep.var.labels = c("Error in rh98"))


rh98_mod<-m9
par(mfrow=c(2,2))
plot(rh98_mod)
plot(allEffects(rh98_mod))


pairs.panels(train[,c("rh_50error","snstv","X11.cover","CC_snstv_diff","time","BeamType","date_index","LC18","slct_","night","zenith")], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)
m1<-glm(rh_50error~snstv*X11.cover+I(X11.cover^2)+BeamType+date_index+Forest+slct_+night+slope+landforms, data = train)
summary(m1)
m2<-glm(rh_50error~snstv*CC_snstv_diff+I(CC_snstv_diff^2)+BeamType+date_index+Forest+slct_+night+slope+landforms, data = train)
summary(m2)
m3<-glm(rh_50error~snstv*BeamType+CC_snstv_diff+I(CC_snstv_diff^2)+date_index+Forest+slct_+night+slope+landforms, data = train)
summary(m3)
m4<-glm(rh_50error~snstv*night+CC_snstv_diff+I(CC_snstv_diff^2)+BeamType+date_index+Forest+slct_+night+slope+landforms, data = train)
summary(m4)
m5<-glm(rh_50error~snstv*BeamType*night+CC_snstv_diff+I(CC_snstv_diff^2)+date_index+Forest+slct_+night+slope+landforms, data = train)
summary(m5)
m6<-glm(rh_50error~snstv*BeamType*CC_snstv_diff+I(CC_snstv_diff^2)+date_index+Forest+slct_+night+slope+landforms, data = train)
summary(m6)
m7<-glm(rh_50error~snstv+BeamType+CC_snstv_diff+I(CC_snstv_diff^2)+date_index+Forest+slct_+night+slope+landforms, data = train)
summary(m7)
AIC(m1, m2, m3, m4, m5, m6, m7)

summary(m3)
summary(m4) #M3 and M4 are functionally the same.. model selection with both.. 

m7<-glm(rh_50error~snstv+BeamType+CC_snstv_diff+I(CC_snstv_diff^2)+date_index+Forest+slct_+night+slope+landforms, data = train)
summary(m7)
m8<-glm(rh_50error~snstv+BeamType+CC_snstv_diff+I(CC_snstv_diff^2)+date_index+Forest+slct_+night+slope, data = train)
summary(m8)
m9<-glm(rh_50error~snstv+BeamType+CC_snstv_diff+I(CC_snstv_diff^2)+Forest+slct_+night+slope, data = train)
summary(m9)
m10<-glm(rh_50error~snstv+BeamType+CC_snstv_diff+I(CC_snstv_diff^2)+slct_+night+slope, data = train)
summary(m10)
m11<-glm(rh_50error~snstv+CC_snstv_diff+I(CC_snstv_diff^2)+slct_+night+slope, data = train)
summary(m11)
m12<-glm(rh_50error~snstv+CC_snstv_diff+I(CC_snstv_diff^2)+slct_+slope, data = train)
summary(m12)
m13<-glm(rh_50error~snstv+CC_snstv_diff+I(CC_snstv_diff^2)+slope, data = train)
summary(m13)
m14<-glm(rh_50error~snstv+CC_snstv_diff+I(CC_snstv_diff^2)+slope+night, data = train)
summary(m14)

AIC(m3, m7, m8, m9, m10, m11, m12, m13, m14)

stargazer(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14,
          type="html",
          out="G:/Thesis/Figures/rh50_linear.doc",
          intercept.bottom = F,
          intercept.top = T,
          ci = FALSE, digits=1,
          notes = "This is a caption.",
          model.names = T,
          single.row = F,
          covariate.labels = xtable::sanitize(c("Constant",
                                                "Sensitivity",
                                                "Canopy Cover", "Canopy Cover sq",
                                                "Canopy Sensitivity Diff","Canopy Sensitivity Diff sq",
                                                "Beam Type",
                                                "Date",
                                                "Forest",
                                                "Algorithm Number",
                                                "Night",
                                                "Slope",
                                                "Landform=Upper Slope (Flat)","Landform=Lower Slope (Warm)","Landform=Lower Slope","Landform=Valley","Landform=Narrow Valley",
                                                "Sensitivity : CanopyCover",
                                                "Sensitivity : Canopy Sensitivity Diff",
                                                "BeamType : Canopy Sensitivity Diff",
                                                "Sensitivity : Beam Type : Canopy Sensitivity Diff",
                                                "Sensitivity : BeamType",
                                                "Sensitivity : Night",
                                                "BeamType : Night",
                                                "Sensitivity : BeamType : Night"),type="latex"),
          dep.var.labels = c("Error in rh50"))


rh50_mod<-m13
par(mfrow=c(2,2))
plot(rh50_mod)
plot(allEffects(rh50_mod))

pairs.panels(train[,c("rh_98error10","snstv","X11.cover","CC_snstv_diff","time","BeamType","date_index","LC18","slct_","night")], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

m1<-glm(rh_98error10~snstv*X11.cover+I(X11.cover^2)+BeamType+date_index+Forest+slct_+night+slope+landforms, data = train, family="binomial")
summary(m1)
m2<-glm(rh_98error10~snstv*CC_snstv_diff+I(CC_snstv_diff^2)+BeamType+date_index+Forest+slct_+night+slope+landforms, data = train, family="binomial")
summary(m2)
m3<-glm(rh_98error10~snstv*BeamType+CC_snstv_diff+I(CC_snstv_diff^2)+date_index+Forest+slct_+night+slope+landforms, data = train, family="binomial")
summary(m3)
m4<-glm(rh_98error10~snstv*night+CC_snstv_diff+I(CC_snstv_diff^2)+BeamType+date_index+Forest+slct_+night+slope+landforms, data = train, family="binomial")
summary(m4)
m5<-glm(rh_98error10~snstv*BeamType*night+CC_snstv_diff+I(CC_snstv_diff^2)+date_index+Forest+slct_+night+slope+landforms, data = train, family="binomial")
summary(m5)
m6<-glm(rh_98error10~snstv*BeamType*CC_snstv_diff+I(CC_snstv_diff^2)+date_index+Forest+slct_+night+slope+landforms, data = train, family="binomial")
summary(m6)
AIC(m1, m2, m3, m4, m5, m6)

summary(m2)

m7<-glm(rh_98error10~snstv*CC_snstv_diff+I(CC_snstv_diff^2)+BeamType+date_index+Forest+slct_+night+slope, data = train, family="binomial")
summary(m7)
m8<-glm(rh_98error10~snstv*CC_snstv_diff+I(CC_snstv_diff^2)+BeamType+date_index+Forest+night+slope, data = train, family="binomial")
summary(m8)
m9<-glm(rh_98error10~snstv*CC_snstv_diff+BeamType+date_index+Forest+night+slope, data = train, family="binomial")
summary(m9)
AIC(m8, m9)
m10<-glm(rh_98error10~snstv*CC_snstv_diff+BeamType+date_index+night+slope, data = train, family="binomial")
summary(m10)
m11<-glm(rh_98error10~snstv*CC_snstv_diff+BeamType+date_index+slope, data = train, family="binomial")
summary(m11)
m12<-glm(rh_98error10~snstv*CC_snstv_diff+date_index+night+slope, data = train, family="binomial")
summary(m12)
m13<-glm(rh_98error10~snstv*CC_snstv_diff+date_index+slope, data = train, family="binomial")
summary(m13)
AIC(m7, m8, m9, m10, m11, m12, m13)

stargazer(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11,
          type="html",
          out="G:/Thesis/Figures/rh98_linear_binomial.doc",
          title = "Generlized Linear Model (Binomial)",
          intercept.bottom = F,
          intercept.top = T,
          ci = FALSE, digits=1,
          notes = "This is a caption.",
          model.names = T,
          single.row = F,
          covariate.labels = xtable::sanitize(c("Constant",
                                                "Sensitivity",
                                                "Canopy Cover", "Canopy Cover sq",
                                                "Canopy Sensitivity Diff","Canopy Sensitivity Diff sq",
                                                "Beam Type",
                                                "Date",
                                                "Forest",
                                                "Algorithm Number",
                                                "Night",
                                                "Slope",
                                                "Landform=Upper Slope (Flat)","Landform=Lower Slope (Warm)","Landform=Lower Slope","Landform=Valley","Landform=Narrow Valley",
                                                "Sensitivity : CanopyCover",
                                                "Sensitivity : Canopy Sensitivity Diff",
                                                "BeamType : Canopy Sensitivity Diff",
                                                "Sensitivity : Beam Type : Canopy Sensitivity Diff", 
                                                "Sensitivity : BeamType", 
                                                "Sensitivity : Night",
                                                "BeamType : Night",
                                                "Sensitivity : BeamType : Night"),type="latex"),
          dep.var.labels = c("rh98 error > 10m"))


rh98GT10_mod<-m13
par(mfrow=c(2,2))
plot(rh98GT10_mod)
plot(allEffects(rh98GT10_mod))

pairs.panels(train[,c("rh_50error10","snstv","X11.cover","CC_snstv_diff","time","BeamType","date_index","LC18","slct_","night","zenith")], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

m1<-glm(rh_50error10~snstv*X11.cover+I(X11.cover^2)+BeamType+date_index+Forest+slct_+night+slope+landforms, data = train, family="binomial")
summary(m1)
m2<-glm(rh_50error10~snstv*CC_snstv_diff+I(CC_snstv_diff^2)+BeamType+date_index+Forest+slct_+night+slope+landforms, data = train, family="binomial")
summary(m2)
m3<-glm(rh_50error10~snstv*BeamType+CC_snstv_diff+I(CC_snstv_diff^2)+date_index+Forest+slct_+night+slope+landforms, data = train, family="binomial")
summary(m3)
m4<-glm(rh_50error10~snstv*night+CC_snstv_diff+I(CC_snstv_diff^2)+BeamType+date_index+Forest+slct_+night+slope+landforms, data = train, family="binomial")
summary(m4)
m5<-glm(rh_50error10~snstv*BeamType*night+CC_snstv_diff+I(CC_snstv_diff^2)+date_index+Forest+slct_+night+slope+landforms, data = train, family="binomial")
summary(m5)
m6<-glm(rh_50error10~snstv*BeamType*CC_snstv_diff+I(CC_snstv_diff^2)+date_index+Forest+slct_+night+slope+landforms, data = train, family="binomial")
summary(m6)
m7<-glm(rh_50error10~snstv+BeamType+CC_snstv_diff+I(CC_snstv_diff^2)+date_index+Forest+slct_+night+slope+landforms, data = train, family="binomial")
summary(m7)
AIC(m1, m2, m3, m4, m5, m6, m7)

summary(m7)

m7<-glm(rh_50error10~snstv+BeamType+CC_snstv_diff+I(CC_snstv_diff^2)+date_index+Forest+slct_+night+slope, data = train, family="binomial")
summary(m7)
m8<-glm(rh_50error10~snstv+BeamType+CC_snstv_diff+I(CC_snstv_diff^2)+date_index+Forest+night+slope, data = train, family="binomial")
summary(m8)
m9<-glm(rh_50error10~snstv+BeamType+CC_snstv_diff+I(CC_snstv_diff^2)+date_index+night+slope, data = train, family="binomial")
summary(m9)
m10<-glm(rh_50error10~snstv+CC_snstv_diff+I(CC_snstv_diff^2)+date_index+night+slope, data = train, family="binomial")
summary(m10)
m11<-glm(rh_50error10~snstv+CC_snstv_diff+date_index+night+slope, data = train, family="binomial")
summary(m11)
m12<-glm(rh_50error10~CC_snstv_diff+I(CC_snstv_diff^2)+date_index+night+slope, data = train, family="binomial")
summary(m12)
m13<-glm(rh_50error10~CC_snstv_diff+date_index+night+slope, data = train, family="binomial")
summary(m13)
AIC(m7, m8, m9, m10, m11, m12, m13)


stargazer(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11,
          type="html",
          out="G:/Thesis/Figures/rh50_linear_binomial.doc",
          intercept.bottom = F,
          intercept.top = T,
          ci = FALSE, digits=1,
          notes = "This is a caption.",
          model.names = T,
          single.row = F,
          covariate.labels = xtable::sanitize(c("Constant",
                                                "Sensitivity",
                                                "Canopy Cover", "Canopy Cover sq",
                                                "Canopy Sensitivity Diff","Canopy Sensitivity Diff sq",
                                                "Beam Type",
                                                "Date",
                                                "Forest",
                                                "Algorithm Number",
                                                "Night",
                                                "Slope",
                                                "Landform=Upper Slope (Flat)","Landform=Lower Slope (Warm)","Landform=Lower Slope","Landform=Valley","Landform=Narrow Valley",
                                                "Sensitivity : CanopyCover",
                                                "Sensitivity : Canopy Sensitivity Diff",
                                                "BeamType : Canopy Sensitivity Diff",
                                                "Sensitivity : Beam Type : Canopy Sensitivity Diff", 
                                                "Sensitivity : BeamType", 
                                                "Sensitivity : Night",
                                                "BeamType : Night",
                                                "Sensitivity : BeamType : Night"),type="latex"),
          dep.var.labels = c("rh50 Error > 10m"))


rh50GT10_mod<-m13
par(mfrow=c(2,2))
plot(rh50GT10_mod)
plot(allEffects(rh50GT10_mod))

#### Classification Trees ####

#rh 98
par(mfrow=c(1,1))
library(tree)
set.seed(123)
tree_high<-tree(factor(rh_98error10)~snstv+CC_snstv_diff+BeamType+date_index+Forest+slct_+night+slope+landforms, data = train)
summary(tree_high)
tree_high
plot(tree_high)
text(tree_high)

t_high<-cv.tree(tree_high)
plot(t_high)
Tree_rh98_10<-prune.tree(tree_high, best = 7)
#png(file="G:/Thesis/Figures/Final/Tree_rh98_binary.png", width=6, height=6, units = "in", res=120) #res in ppi 300dpi = 118 ppi
plot(Tree_rh98_10)
text(Tree_rh98_10)
dev.off()

#rh 50
par(mfrow=c(1,1))
set.seed(123)
tree_high<-tree(factor(rh_50error10)~snstv+CC_snstv_diff+BeamType+date_index+Forest+slct_+night+slope+landforms, data = train)
summary(tree_high)
tree_high
plot(tree_high)
text(tree_high)

t_high<-cv.tree(tree_high)
plot(t_high)
Tree_rh50_10<-prune.tree(tree_high, best = 5)
#png(file="G:/Thesis/Figures/Final/Tree_rh50_binary.png", width=6, height=6, units = "in", res=120) #res in ppi 300dpi = 118 ppi
plot(Tree_rh50_10)
text(Tree_rh50_10)
dev.off()

#### Regression Trees ####
#RH 98 #
par(mfrow=c(1,1))
set.seed(123)
tree_regress98<-tree(rh_98error~snstv+CC_snstv_diff+BeamType+date_index+Forest+slct_+night+slope+landforms, data = train)
summary(tree_regress98)
tree_regress98
plot(tree_regress98)
text(tree_regress98)

t_high<-cv.tree(tree_regress98)
plot(t_high)
Best_98<-prune.tree(tree_regress98, best = 5)
plot(Best_98)
text(Best_98)

#RH 50 #
par(mfrow=c(1,1))
set.seed(123)
tree_regress50<-tree(rh_50error~snstv+CC_snstv_diff+BeamType+date_index+Forest+slct_+night+slope+landforms, data = subset(train, rh_50>0))
summary(tree_regress50)
tree_regress50
plot(tree_regress50)
text(tree_regress50)

t_high<-cv.tree(tree_regress50)
plot(t_high)
Best_50<-prune.tree(tree_regress50, best = 6)
plot(Best_50)
text(Best_50)


# Random Forest classification####
library(caret)
fitControl <- trainControl(method="repeatedcv", number=10, repeats=10, 
                           classProbs=TRUE, summaryFunction=twoClassSummary, savePredictions="all")
mtry <- sqrt(ncol(train))
tunegrid <- expand.grid(.mtry=mtry)

set.seed(5678)
train$rh_98error10 <- factor(train$rh_98error10, levels=c(0,1), 
                             labels=c("good", "high"))
model_weights <- ifelse(train$rh_98error10 == "high",
                        (1/table(train$rh_98error10)[2]) * 0.5,
                        (1/table(train$rh_98error10)[1]) * 0.5)
error98GT10_fit_grid<- train(rh_98error10 ~ snstv+BeamType+X11.cover+CC_snstv_diff+date_index+Forest+slct_+night+slope, data = train, method = "rf", trControl = fitControl,
                             verbose=T, tuneGrid=tunegrid, metric="ROC", weights = model_weights)

error98GT10_fit_grid$results
max(error98GT10_fit_grid$results$ROC) #.6989

threshold_stats <- thresholder(error98GT10_fit_grid, threshold=seq(0.1,1,by=0.001),final=TRUE)
tss98 <- threshold_stats[which.max(threshold_stats$Specificity + 
                                     threshold_stats$Sensitivity - 1),]$prob_threshold  # 0.351

set.seed(5678)
train$rh_50error10 <- factor(train$rh_50error10, levels=c(0,1), 
                             labels=c("good", "high"))
model_weights <- ifelse(train$rh_50error10 == "high",
                        (1/table(train$rh_98error10)[2]) * 0.5,
                        (1/table(train$rh_98error10)[1]) * 0.5)
error50GT10_fit_grid<- train(rh_50error10 ~ snstv+BeamType+X11.cover+CC_snstv_diff+date_index+Forest+slct_+night+slope, data = train, method = "rf", trControl = fitControl,
                             verbose=T, tuneGrid=tunegrid, metric="ROC")

error50GT10_fit_grid$results
max(error50GT10_fit_grid$results$ROC) #.5789

threshold_stats <- thresholder(error50GT10_fit_grid, threshold=seq(0.1,1,by=0.001),final=TRUE)
tss50 <- threshold_stats[which.max(threshold_stats$Specificity + 
                                     threshold_stats$Sensitivity - 1),]$prob_threshold  # 0.351

# Random Forest regression####
# library(caret)
# fitControl <- trainControl(method="repeatedcv", number=10, repeats=10, savePredictions="all")
# mtry <- sqrt(ncol(train))
# tunegrid <- expand.grid(.mtry=mtry)
# 
# set.seed(5678)
# error98_fitgrid<- train(rh_98error ~ snstv+BeamType+X11.cover+CC_snstv_diff+date_index+Forest+slct_+night+slope, data = train, method = "rf", trControl = fitControl,
#                         verbose=T, tuneGrid=tunegrid, metric="Rsquared")
# 
# error98_fitgrid$results
# max(error98_fitgrid$results$Rsquared) #0.18
# 
# error50_fitgrid<-train(rh_50error ~ snstv+BeamType+X11.cover+CC_snstv_diff+date_index+Forest+slct_+night+slope, data = train, method = "rf", trControl = fitControl,
#                        verbose=T, tuneGrid=tunegrid, metric="Rsquared")
# 
# error50_fitgrid$results
# max(error50_fitgrid$results$Rsquared) #0.13
# 
#Variable loadings of Simpler random forest####
#Classification

Error_importance98 <- varImp(error98GT10_fit_grid)
loadings98<-ggplot(Error_importance98) + ggtitle("Variable Loadings error in rh98")  +
  theme(text = element_text(size=15))

Error_importance50 <- varImp(error50GT10_fit_grid)
loadings50<-ggplot(Error_importance50) + ggtitle("Variable Loadings error in rh50")  +
  theme(text = element_text(size=15))

#png(file="G:/Thesis/Figures/Final/RF_classification_loadings.png", width=12, height=6, units = "in", res=120) #res in ppi 300dpi = 118 ppi
ggpubr::ggarrange(loadings98, loadings50,
          ncol = 2,
          nrow = 1, 
          common.legend = FALSE)
dev.off()

#REgression
Error_importance982 <- varImp(error98_fitgrid)
ggplot(Error_importance982) + ggtitle("Variable Loadings error in rh98")  +
  theme(text = element_text(size=15))

Error_importance502 <- varImp(error50_fitgrid)
ggplot(Error_importance502) + ggtitle("Variable Loadings error in rh50")  +
  theme(text = element_text(size=15))

#Partial Dependancy plots
library(pdp)


#classification 98 rf
Sensitivity <- partial(error98GT10_fit_grid, pred.var="snstv", plot=F, smooth=T, prob=T)
Difference <- partial(error98GT10_fit_grid, pred.var="CC_snstv_diff", plot=F, smooth=T, prob=T)
Slope <- partial(error98GT10_fit_grid, pred.var="slope", plot=F, smooth=T, prob=T)

png(file="G:/Thesis/Figures/Final/Partial_dependance_RF_rh98GT10.png", width=9, height=4, units = "in", res=120) #res in ppi 300dpi = 118 ppi
par(mfrow=c(1,3), mar=c(4,4,1.5,1), oma=c(1,1,1,1), xpd=F)
scatter.smooth(Difference$CC_snstv_diff, Difference$yhat, pch=NA,
               xlab="Sensitivity Difference", ylab="Probability",
               ylim = c(0.85, 0.92),
               lpars=list(lwd=4)); grid(lwd=2)
scatter.smooth(Slope$slope, Slope$yhat, pch=NA,
               xlab="Slope", ylab="",
               lpars=list(lwd=4)); grid(lwd=2)
scatter.smooth(Sensitivity$snstv, Sensitivity$yhat, pch=NA,
               xlab=expression("Sensitivity"),ylab="",
               lpars=list(lwd=4)); grid(lwd=2)

dev.off()

#classification 50 rf
Sensitivity <- partial(error50GT10_fit_grid, pred.var="snstv", plot=F, smooth=T, prob=T)
Cover <- partial(error50GT10_fit_grid, pred.var="X11.cover", plot=F, smooth=T, prob=T)
Slope <- partial(error50GT10_fit_grid, pred.var="slope", plot=F, smooth=T, prob=T)
Difference <- partial(error50GT10_fit_grid, pred.var="CC_snstv_diff", plot=F, smooth=T, prob=T)

png(file="G:/Thesis/Figures/Final/Partial_dependance_RF_rh50GT10.png", width=9, height=4, units = "in", res=120) #res in ppi 300dpi = 118 ppi
par(mfrow=c(1,4), mar=c(4,4,1.5,1), oma=c(1,1,1,1), xpd=F)
scatter.smooth(Slope$slope, Slope$yhat, pch=NA,
               xlab="Slope", ylab="Probability",
               ylim = c(0.8, 0.93),
               lpars=list(lwd=4)); grid(lwd=2)
scatter.smooth(Cover$X11.cover, Cover$yhat, pch=NA,
               xlab="Canopy Cover", ylab="",
               ylim = c(0.9, 0.93),
               lpars=list(lwd=4)); grid(lwd=2)
scatter.smooth(Sensitivity$snstv, Sensitivity$yhat, pch=NA,
               xlab=expression("Sensitivity"),ylab="",
               ylim = c(0.89, 0.93),
               lpars=list(lwd=4)); grid(lwd=2)
scatter.smooth(Difference$CC_snstv_diff, Difference$yhat, pch=NA,
               xlab=expression("Sensitivity Difference"),ylab="",
               ylim = c(0.85, 0.91),
               lpars=list(lwd=4)); grid(lwd=2)
dev.off()

#Regression 98 rf
Sensitivity <- partial(error98_fitgrid, pred.var="snstv", plot=F, smooth=T, prob=T)
Difference <- partial(error98_fitgrid, pred.var="CC_snstv_diff", plot=F, smooth=T, prob=T)
Slope <- partial(error98_fitgrid, pred.var="slope", plot=F, smooth=T, prob=T)

par(mfrow=c(1,3), mar=c(2,2,1.5,1), oma=c(1,1,1,1), xpd=F)
scatter.smooth(Sensitivity$snstv, Sensitivity$yhat, pch=NA,
               xlab=expression("Sensitivity"),ylab="",
               lpars=list(lwd=4)); grid(lwd=2)
scatter.smooth(Difference$CC_snstv_diff, Difference$yhat, pch=NA,
               xlab="Difference", ylab="",
               lpars=list(lwd=4)); grid(lwd=2)
scatter.smooth(Slope$slope, Slope$yhat, pch=NA,
               xlab="Canopy X11.cover", ylab="",
               lpars=list(lwd=4)); grid(lwd=2)

#Regression 50 rf
Sensitivity <- partial(error50_fitgrid, pred.var="snstv", plot=F, smooth=T, prob=T)
Difference <- partial(error50_fitgrid, pred.var="CC_snstv_diff", plot=F, smooth=T, prob=T)
Slope <- partial(error50_fitgrid, pred.var="slope", plot=F, smooth=T, prob=T)

par(mfrow=c(1,3), mar=c(2,2,1.5,1), oma=c(1,1,1,1), xpd=F)
scatter.smooth(Sensitivity$snstv, Sensitivity$yhat, pch=NA,
               xlab=expression("Sensitivity"),ylab="",
               lpars=list(lwd=4)); grid(lwd=2)
scatter.smooth(Difference$CC_snstv_diff, Difference$yhat, pch=NA,
               xlab="Difference", ylab="",
               lpars=list(lwd=4)); grid(lwd=2)
scatter.smooth(Slope$slope, Slope$yhat, pch=NA,
               xlab="Canopy X11.cover", ylab="",
               lpars=list(lwd=4)); grid(lwd=2)
#### Data omission and testing on Test data set ####

#prediction for models
test$night<-ifelse(test$slr_l<0, paste0(1), paste(0))
test$night<-as.factor(test$night)
test$landforms<-as.factor(test$landforms)

test$rh98_GLM<-predict(rh98_mod, test, type="response")
test$rh50_GLM<-predict(rh50_mod, test, type="response")
test$rh98_Binomial<-predict(rh98GT10_mod, test, type="response")
test$rh50_Binomial<-predict(rh50GT10_mod, test, type="response")
test$rh98_Binomial_bi<-as.factor(ifelse(test$rh98_Binomial>= 0.5, 1, 0))
test$rh50_Binomial_bi<-as.factor(ifelse(test$rh50_Binomial>= 0.5, 1, 0))
test$rh50_Binomial_bi<-ordered(test$rh50_Binomial_bi, levels=c(0,1))

test$rh98_Tree<-predict(Tree_rh98_10, test, type="class")
test$rh50_Tree<-predict(Tree_rh50_10, test, type="class")
test$rh98_regTree<-predict(tree_regress98, test, type="vector")
test$rh50_regTree<-predict(tree_regress50, test, type="vector")

test$rh98_rf<-predict(error98GT10_fit_grid, test)
test$rh50_rf<-predict(error50GT10_fit_grid, test)
test$rf_98ref<-as.factor(ifelse(test$rh_98error10 == 0, "good", "high"))
test$rf_50ref<-as.factor(ifelse(test$rh_50error10 == 0, "good", "high"))
test$rh_98_rfReg<-predict(error98_fitgrid, test)
test$rh_50_rfReg<-predict(error50_fitgrid, test)
#Accuracy assessment
confusionMatrix(test$rh98_rf, test$rf_98ref) #Best
confusionMatrix(test$rh50_rf, test$rf_50ref)
confusionMatrix(test$rh98_Tree, as.factor(test$rh_98error10))
confusionMatrix(test$rh50_Tree, as.factor(test$rh_50error10))
confusionMatrix(test$rh98_Binomial_bi, as.factor(test$rh_98error10))
confusionMatrix(test$rh50_Binomial_bi, as.factor(test$rh_50error10))


cor(test$rh_98error, test$rh98_GLM)^2 # close 2nd
cor(test$rh_98error, test$rh_98_rfReg)^2 #Best
cor(test$rh_98error, test$rh98_regTree)^2


cor(test$rh_50error, test$rh50_GLM)^2 #Best
cor(test$rh_50error, test$rh_50_rfReg)^2 #Close 2nd
cor(test$rh_50error, test$rh50_regTree)^2

plot(test$rh98_GLM~test$rh_98error)
plot(test$rh50_GLM~test$rh_50error)
plot(test$rh_98_rfReg~test$rh_98error)
plot(test$rh_98_rfReg~test$rh_50error)
plot(test$rh98_regTree~test$rh_98error)
plot(test$rh50_regTree~test$rh_50error)

#Calculate values to MS
cor(L2A_compare1$rh_50error, L2A_compare1$rh_98error, method = "pearson")

#### Derrived Metrics ####
L2A_compare_trunk<-L2A_compare1
L2A_compare_trunk[,c(173:273)][L2A_compare_trunk[,c(173:273)] < 0] <-0
L2A_compare_trunk[,c(16:116)][L2A_compare_trunk[,c(16:116)] < 0] <-0
#write.csv(L2A_compare_trunk, "G:/Thesis/Spatial/ALS_Fire/L2A_collocated_trunk.csv")


L2A_compare1$rh_sum98<-rowSums((L2A_compare_trunk[,c(16:114)]))
L2A_compare1$rhGauss_sum98<-rowSums((L2A_compare_trunk[,c(173:271)]))

rh_sum<-ggplot(L2A_compare1) + geom_point(aes(x=rh_sum98, y=rhGauss_sum98)) + 
  theme_bw() + theme(legend.position = "none") +
  scale_x_continuous(limits = c(0,4000), expand = c(0,0), breaks = seq(0,4000,1000)) +
  scale_y_continuous (limits = c(0,4000), expand = c(0,0), breaks = seq(0,4000,1000)) +
  xlab(bquote('rh'['sum'])) + ylab(bquote('rh'['sum']))
rh_sum

L2A_compare1$CR_gedi<-((L2A_compare_trunk$rh_98-L2A_compare_trunk$rh_25)/L2A_compare_trunk$rh_98)
L2A_compare1$CR_sim<-((L2A_compare_trunk$X112.rhGauss.98-L2A_compare_trunk$X39.rhGauss.25)/L2A_compare_trunk$X112.rhGauss.98)

L2A_compare1$CR50_gedi<-((L2A_compare_trunk$rh_98-L2A_compare_trunk$rh_50)/L2A_compare_trunk$rh_98)
L2A_compare1$CR50_sim<-((L2A_compare_trunk$X112.rhGauss.98-L2A_compare_trunk$X64.rhGauss.50)/L2A_compare_trunk$X112.rhGauss.98)


cr<-ggplot(L2A_compare1) + geom_point(aes(x=CR_gedi, y=CR_sim)) + 
  theme_bw() + theme(legend.position = "none") +
  scale_x_continuous(limits = c(0.25,1.01), expand = c(0,0), breaks = seq(0,1,0.5)) +
  scale_y_continuous (limits = c(0.25,1.01), expand = c(0,0), breaks = seq(0,1,0.5)) +
  xlab("Canopy Ratio") + ylab("Canopy Ratio")

ggplot(L2A_compare1) + geom_point(aes(x=CR50_gedi, y=CR50_sim)) + 
  theme_bw() + theme(legend.position = "none") +
  scale_x_continuous(limits = c(0.25,1.01), expand = c(0,0), breaks = seq(0,1,0.5)) +
  scale_y_continuous (limits = c(0.25,1.01), expand = c(0,0), breaks = seq(0,1,0.5)) +
  xlab("Canopy Ratio") + ylab("Canopy Ratio")

L2A_compare1$R98_50_gedi<-((L2A_compare_trunk$rh_98/L2A_compare_trunk$rh_50))
L2A_compare1$R98_50_sim<-((L2A_compare_trunk$X112.rhGauss.98/L2A_compare_trunk$X64.rhGauss.50))

r<-ggplot(L2A_compare1) + geom_point(aes(x=R98_50_gedi, y=R98_50_sim)) + 
  theme_bw() + theme(legend.position = "none") +
  scale_x_continuous(limits = c(0,100), expand = c(0,0), breaks = seq(0,100,25)) +
  scale_y_continuous (limits = c(0,100), expand = c(0,0), breaks = seq(0,100,25)) +
  xlab("rh98:rh50") + ylab("rh98:rh50")

compound_error<-ggarrange(rh_sum, r, cr, 
                          nrow = 3,
                          ncol = 1)
compound_error

cor(L2A_compare1$rh_sum98, L2A_compare1$rhGauss_sum98)
cor(L2A_compare1$CR_sim, L2A_compare1$CR_gedi)
cor(L2A_compare1$CR50_sim, L2A_compare1$CR50_gedi)

write.csv(L2A_compare1[,c("rh_sum98","rhGauss_sum98","CR_sim","CR_gedi","CR50_sim","CR50_gedi","R98_50_gedi","R98_50_sim")],
          "G:/Thesis/Spatial/LiDAR/Derrived_GEO_v4.csv")

#### Data subsetting according to Best models
error_summary2<-as.data.frame(rep(c(1:100),7))
colnames(error_summary2)<-c("rh")
error_summary2$RMSE<-0
error_summary2$rsq<-0
error_summary2$RMSEpct<-0
error_summary2$Bias<-0
error_summary2$MAE<-0
error_summary2$MAEpct<-0

for (i in 1:100) {
  error_summary2[i,2]<-sqrt(mean((L2A_compareWflags[,c((173+i))] - L2A_compareWflags[,c((15+i))])^2))
  rss <- sum((L2A_compareWflags[,c((15+i))]- L2A_compareWflags[,c((173+i))])^ 2)  ## residual sum of squares
  tss <- sum((L2A_compareWflags[,c((173+i))] - mean(L2A_compareWflags[,c((173+i))])) ^ 2)  ## total sum of squares
  error_summary2[i,3] <- 1 - rss/tss
  error_summary2[i,4] <- ((error_summary2[i,2]/mean(L2A_compareWflags[,c((173+i))]))*100)
  error_summary2[i,5]<-(sum(L2A_compareWflags[,c((173+i))] - L2A_compareWflags[,c((15+i))])/nrow(L2A_compareWflags))
  error_summary2[i,6]<-(sum(abs(L2A_compareWflags[,c((173+i))] - L2A_compareWflags[,c((15+i))])))/nrow(L2A_compareWflags)
  error_summary2[i,7] <- ((error_summary2[i,6]/mean(L2A_compareWflags[,c((173+i))]))*100)
}

for (i in 1:100) {
  error_summary2[((100+i)),2]<-sqrt(mean((test[,c((173+i))] - test[,c((15+i))])^2))
  rss <- sum((test[,c((15+i))]- test[,c((173+i))])^ 2)  ## residual sum of squares
  tss <- sum((test[,c((173+i))] - mean(test[,c((173+i))])) ^ 2)  ## total sum of squares
  error_summary2[((100+i)),3] <- 1 - rss/tss
  error_summary2[((100+i)),4] <- ((error_summary2[((100+i)),2]/mean(test[,c((173+i))]))*100)
  error_summary2[((100+i)),5]<-(sum(test[,c((173+i))] - test[,c((15+i))])/nrow(test))
  error_summary2[((100+i)),6]<-(sum(abs(test[,c((173+i))] - test[,c((15+i))])))/nrow(test)
  error_summary2[((100+i)),7] <- ((error_summary2[((100+i)),6]/mean(test[,c((173+i))]))*100)
}

error_summary2$Data<-rep(c("All Geolocated Data","Geolocated with Flagged Data Removed*",
                           "98 GLM",
                           "98 RF",
                           "50 GLM",
                           "50 RF",
                           "98 RF class"), each=100)
error_summary2$Data_short<-rep(c("Full Geolocation","Geolocation Corrected*",
                                 "98 GLM",
                                 "98 RF",
                                 "50 GLM",
                                 "50 RF",
                                 "98 RF class"), each=100)
error_summary2$Data_short<-as.factor(error_summary2$Data_short)
levels(error_summary2$Data_short)
error_summary2$Data_short<-ordered(error_summary2$Data_short, levels=c("Full Geolocation","Geolocation Corrected*",
                                                                       "98 GLM",
                                                                       "98 RF",
                                                                       "50 GLM",
                                                                       "50 RF",
                                                                       "98 RF class"))

list<-c("blah","blah","rh98_GLM","rh_98_rfReg","rh50_GLM","rh_50_rfReg","rh98_rf")
for (j in 3:(length(list)-1)) {
  for (i in 1:100) {
    temp_data<-subset(test, test[,c(list[j])]<10 & test[,c(list[j])]>-10)
    error_summary2[(((j-1)*100)+i),2]<-sqrt(mean((temp_data[,c((173+i))] - temp_data[,c((15+i))])^2))
    rss <- sum((temp_data[,c((15+i))]- temp_data[,c((173+i))])^ 2)  ## residual sum of squares
    tss <- sum((temp_data[,c((173+i))] - mean(temp_data[,c((173+i))])) ^ 2)  ## total sum of squares
    error_summary2[(((j-1)*100)+i),3] <- 1 - rss/tss
    error_summary2[(((j-1)*100)+i),4] <- ((error_summary2[(((j-1)*100)+i),2]/mean(temp_data[,c((173+i))]))*100)
    error_summary2[(((j-1)*100)+i),5]<-(sum(temp_data[,c((173+i))] - temp_data[,c((15+i))])/nrow(temp_data))
    error_summary2[(((j-1)*100)+i),6]<-(sum(abs(temp_data[,c((173+i))] - temp_data[,c((15+i))])))/nrow(temp_data)
    error_summary2[(((j-1)*100)+i),7] <- ((error_summary2[(((j-1)*100)+i),6]/mean(temp_data[,c((173+i))]))*100)
  }
}

for (j in (length(list)-1):length(list)) {
  for (i in 1:100) {
    temp_data<-subset(test, test[,c(list[j])]=="good")
    error_summary2[(((j-1)*100)+i),2]<-sqrt(mean((temp_data[,c((173+i))] - temp_data[,c((15+i))])^2))
    rss <- sum((temp_data[,c((15+i))]- temp_data[,c((173+i))])^ 2)  ## residual sum of squares
    tss <- sum((temp_data[,c((173+i))] - mean(temp_data[,c((173+i))])) ^ 2)  ## total sum of squares
    error_summary2[(((j-1)*100)+i),3] <- 1 - rss/tss
    error_summary2[(((j-1)*100)+i),4] <- ((error_summary2[(((j-1)*100)+i),2]/mean(temp_data[,c((173+i))]))*100)
    error_summary2[(((j-1)*100)+i),5]<-(sum(temp_data[,c((173+i))] - temp_data[,c((15+i))])/nrow(temp_data))
  }
}

library(RColorBrewer)
RMSE_plot<-ggplot(error_summary2, aes(x=rh, y=RMSE, col=Data_short, alpha=0.25)) + 
  scale_x_continuous(limits = c(0,105), expand = c(0,0), breaks = seq(0,100,25)) +
  scale_y_continuous (limits = c(0,7), expand = c(0,0), breaks = seq(0,6,2)) +
  geom_point(aes(col=Data_short), size=2) +geom_line(aes(col=Data_short), size=2) + theme_pubr() + scale_colour_brewer(palette = "Paired") + 
  theme(
    legend.position = c(.95, .3),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6,),
    legend.title=element_blank(),
    text = element_text(size = 20)
  ) + guides(alpha = "none") +
  ylab("RMSE") + xlab("Relative Height (rh)")
RMSE_plot  

RMSEpct_plot<-ggplot(error_summary2, aes(x=rh, y=RMSEpct, col=Data_short, alpha=0.25)) + 
  scale_x_continuous(limits = c(0,105), expand = c(0,0), breaks = seq(0,100,25)) +
  scale_y_continuous (limits = c(-2,100), expand = c(0,0), breaks = seq(0,100,25)) +
  geom_point(aes(col=Data_short), size=2) +geom_line(aes(col=Data_short), size=2) + theme_pubr() + scale_colour_brewer(palette = "Paired") + 
  theme(
    legend.position = "none",
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6,),
    legend.title=element_blank(),
    text = element_text(size = 20)
  ) + guides(alpha = "none") +
  ylab("RMSE%") + xlab("Relative Height (rh)")
RMSEpct_plot  

Bias_plot<-ggplot(error_summary2, aes(x=rh, y=Bias, col=Data_short, alpha=0.25)) + 
  scale_x_continuous(limits = c(0,105), expand = c(0,0), breaks = seq(0,100,25)) +
  scale_y_continuous (limits = c(0,3), expand = c(0,0), breaks = seq(0,3,1)) +
  geom_point(aes(col=Data_short), size=2) +geom_line(aes(col=Data_short), size=2) + theme_pubr() + scale_colour_brewer(palette = "Paired") + 
  theme(
    legend.position = "none",
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6,),
    legend.title=element_blank(),
    text = element_text(size = 20)
  ) + guides(alpha = "none") +
  ylab("Bias") + xlab("Relative Height (rh)")
Bias_plot  

Rsq_plot<-ggplot(error_summary2, aes(x=rh, y=rsq, col=Data_short, alpha=0.9)) + 
  scale_x_continuous(limits = c(0,105), expand = c(0,0), breaks = seq(0,100,25)) +
  scale_y_continuous (limits = c(0,1), expand = c(0,0), breaks = seq(0,1,0.2)) +
  geom_point(aes(col=Data_short), size=2) +geom_line(aes(col=Data_short), size=2) + theme_pubr() + scale_colour_brewer(palette = "Paired")+ 
  theme(
    legend.position = "none",
    text = element_text(size = 20)
  ) + guides(alpha = "none") +
  ylab("R-Squared") + xlab("Relative Height (rh)")
Rsq_plot

ggarrange(Rsq_plot, RMSE_plot, Bias_plot,
          ncol = 3,
          nrow = 1, 
          common.legend = TRUE, 
          legend = "top")


#### Filtering based on rh error and geolocation
differences<-as.data.frame(rep(c("Geolocation", "rh50", "rh75","rh98"), each=2))

colnames(differences)<-c("Filters")
differences$rh<-rep(c(50, 98),4)
differences$Mean<-0
differences$StandardDeviation<-0
differences$RMSE<-0
differences$pct<-0
differences$n<-0
differences$Bias<-0

#All Data
differences[1,3]<-mean(L2A_compare1$rh_50error)
differences[1,4]<-sd(L2A_compare1$rh_50error)
differences[1,5]<-sqrt(mean((L2A_compare1[,c(223)] - L2A_compare1[,c(66)])^2))
differences[2,3]<-mean(L2A_compare1$rh_98error)
differences[2,4]<-sd(L2A_compare1$rh_98error)
differences[2,5]<-sqrt(mean((L2A_compare1[,c(271)] - L2A_compare1[,c(114)])^2))
differences[1,6]<-1-(nrow(L2A_compare1)/1191)
differences[1,7]<-nrow(L2A_compare1)
differences[1,8]<-(sum(L2A_compare1[,c((223))] - L2A_compare1[,c((66))])/nrow(L2A_compare1))
differences[2,8]<-(sum(L2A_compare1[,c((271))] - L2A_compare1[,c((114))])/nrow(L2A_compare1))


list<-c("rh_75error10","rh_98error10","rh_50error10")
for (i in 1:length(list)) {
  temp_data<-subset(L2A_compare1, L2A_compare1[,c(list[i])]!=1)
  differences[((i*2)+1),3]<-mean(temp_data$rh_50error)
  differences[((i*2)+1),4]<-sd(temp_data$rh_50error)
  differences[((i*2)+1),5]<-sqrt(mean((temp_data[,c(223)] - temp_data[,c(66)])^2))
  differences[((i*2)+2),3]<-mean(temp_data$rh_98error)
  differences[((i*2)+2),4]<-sd(temp_data$rh_98error)
  differences[((i*2)+2),5]<-sqrt(mean((temp_data[,c(271)] - temp_data[,c(114)])^2))
  differences[((i*2)+1),6]<-1-(nrow(temp_data)/1191)
  differences[((i*2)+1),7]<-nrow(temp_data)
  differences[((i*2)+1),8]<-(sum(temp_data[,c((223))] - temp_data[,c((66))])/nrow(temp_data))
  differences[((i*2)+2),8]<-(sum(temp_data[,c((271))] - temp_data[,c((114))])/nrow(temp_data))
}

#differences$range98<-paste0(substr(differences$Mean98-2*differences$StandardDeviation98, 1,6)," : ",substr(differences$Mean98+2*differences$StandardDeviation98, 1, 4))
#differences$range50<-paste0(substr(differences$Mean50-2*differences$StandardDeviation50, 1,6)," : ",substr(differences$Mean50+2*differences$StandardDeviation50, 1, 4))

differences$range<-paste0(substr(differences$Mean-2*differences$StandardDeviation, 1,6)," : ",substr(differences$Mean+2*differences$StandardDeviation, 1, 4))
View(differences)

# Plots of R-squared and RMSE through Canopy 
error_summary2<-as.data.frame(rep(c(1:100),5))
colnames(error_summary2)<-c("rh")
error_summary2$RMSE<-0
error_summary2$rsq<-0
error_summary2$RMSEpct<-0
error_summary2$Bias<-0
error_summary2$MAE<-0
error_summary2$MAEpct<-0


for (i in 1:100) {
  error_summary2[i,2]<-sqrt(mean((L2A_compareWflags[,c((173+i))] - L2A_compareWflags[,c((15+i))])^2))
  rss <- sum((L2A_compareWflags[,c((15+i))]- L2A_compareWflags[,c((173+i))])^ 2)  ## residual sum of squares
  tss <- sum((L2A_compareWflags[,c((173+i))] - mean(L2A_compareWflags[,c((173+i))])) ^ 2)  ## total sum of squares
  error_summary2[i,3] <- 1 - rss/tss
  error_summary2[i,4] <- ((error_summary2[i,2]/mean(L2A_compareWflags[,c((173+i))]))*100)
  error_summary2[i,5]<-(sum(L2A_compareWflags[,c((173+i))] - L2A_compareWflags[,c((15+i))])/nrow(L2A_compareWflags))
  error_summary2[i,6]<-(sum(abs(L2A_compareWflags[,c((173+i))] - L2A_compareWflags[,c((15+i))])))/nrow(L2A_compareWflags)
  error_summary2[i,7] <- ((error_summary2[i,6]/mean(L2A_compareWflags[,c((173+i))]))*100)
}

for (i in 1:100) {
  error_summary2[((100+i)),2]<-sqrt(mean((L2A_compare1[,c((173+i))] - L2A_compare1[,c((15+i))])^2))
  rss <- sum((L2A_compare1[,c((15+i))]- L2A_compare1[,c((173+i))])^ 2)  ## residual sum of squares
  tss <- sum((L2A_compare1[,c((173+i))] - mean(L2A_compare1[,c((173+i))])) ^ 2)  ## total sum of squares
  error_summary2[((100+i)),3] <- 1 - rss/tss
  error_summary2[((100+i)),4] <- ((error_summary2[((100+i)),2]/mean(L2A_compare1[,c((173+i))]))*100)
  error_summary2[((100+i)),5]<-(sum(L2A_compare1[,c((173+i))] - L2A_compare1[,c((15+i))])/nrow(L2A_compare1))
  error_summary2[((100+i)),6]<-(sum(abs(L2A_compare1[,c((173+i))] - L2A_compare1[,c((15+i))])))/nrow(L2A_compare1)
  error_summary2[((100+i)),7] <- ((error_summary2[((100+i)),6]/mean(L2A_compare1[,c((173+i))]))*100)
}

error_summary2$Data<-rep(c("All Geolocated Data","Geolocated with Flagged Data Removed*",
                           "RH_75 Error>10 Removed",
                           "RH_98 Error>10 Removed",
                           "RH_50 Error>10 Removed"), each=100)
error_summary2$Data_short<-rep(c("Full Geolocation","Geolocation Corrected*",
                                 "RH_75 Error>10 Removed",
                                 "RH_98 Error>10 Removed",
                                 "RH_50 Error>10 Removed"), each=100)
error_summary2$Data_short<-as.factor(error_summary2$Data_short)
levels(error_summary2$Data_short)
error_summary2$Data_short<-ordered(error_summary2$Data_short, levels=c("Full Geolocation","Geolocation Corrected*",
                                                                       "RH_98 Error>10 Removed",
                                                                       "RH_75 Error>10 Removed",
                                                                       "RH_50 Error>10 Removed"))

list<-c("flag","No Flags","rh_75error10","rh_98error10","rh_50error10")
for (j in 3:length(list)) {
  for (i in 1:100) {
    temp_data<-subset(L2A_compare1, L2A_compare1[,c(list[j])]==0)
    error_summary2[(((j-1)*100)+i),2]<-sqrt(mean((temp_data[,c((173+i))] - temp_data[,c((15+i))])^2))
    rss <- sum((temp_data[,c((15+i))]- temp_data[,c((173+i))])^ 2)  ## residual sum of squares
    tss <- sum((temp_data[,c((173+i))] - mean(temp_data[,c((173+i))])) ^ 2)  ## total sum of squares
    error_summary2[(((j-1)*100)+i),3] <- 1 - rss/tss
    error_summary2[(((j-1)*100)+i),4] <- ((error_summary2[(((j-1)*100)+i),2]/mean(temp_data[,c((173+i))]))*100)
    error_summary2[(((j-1)*100)+i),5]<-(sum(temp_data[,c((173+i))] - temp_data[,c((15+i))])/nrow(temp_data))
    error_summary2[(((j-1)*100)+i),6]<-(sum(abs(temp_data[,c((173+i))] - temp_data[,c((15+i))])))/nrow(temp_data)
    error_summary2[(((j-1)*100)+i),7] <- ((error_summary2[(((j-1)*100)+i),6]/mean(temp_data[,c((173+i))]))*100)
  }
}

write.csv(error_summary2, file = "G:/Thesis/Spatial/LiDAR/Error_Summary_GEO_v6.csv")

library(RColorBrewer)
RMSE_plot<-ggplot(error_summary2, aes(x=rh, y=RMSE, col=Data_short, alpha=0.25)) + 
  scale_x_continuous(limits = c(0,105), expand = c(0,0), breaks = seq(0,100,25)) +
  scale_y_continuous (limits = c(0,7), expand = c(0,0), breaks = seq(0,6,2)) +
  geom_point(aes(col=Data_short), size=2) +geom_line(aes(col=Data_short), size=2) + #theme_pubr() + 
  scale_colour_brewer(palette = "Paired") + 
  theme(
    legend.position = c(.95, .3),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6,),
    legend.title=element_blank(),
    text = element_text(size = 20)
  ) + guides(alpha = "none") +
  ylab("RMSE") + xlab("Relative Height (rh)")
RMSE_plot  

RMSEpct_plot<-ggplot(error_summary2, aes(x=rh, y=RMSEpct, col=Data_short, alpha=0.25)) + 
  scale_x_continuous(limits = c(0,105), expand = c(0,0), breaks = seq(0,100,25)) +
  scale_y_continuous (limits = c(-2,100), expand = c(0,0), breaks = seq(0,100,25)) +
  geom_point(aes(col=Data_short), size=2) +geom_line(aes(col=Data_short), size=2) + theme_pubr() + scale_colour_brewer(palette = "Paired") + 
  theme(
    legend.position = "none",
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6,),
    legend.title=element_blank(),
    text = element_text(size = 20)
  ) + guides(alpha = "none") +
  ylab("RMSE%") + xlab("Relative Height (rh)")
RMSEpct_plot  

Bias_plot<-ggplot(error_summary2, aes(x=rh, y=Bias, col=Data_short, alpha=0.25)) + 
  scale_x_continuous(limits = c(0,105), expand = c(0,0), breaks = seq(0,100,25)) +
  scale_y_continuous (limits = c(0,3), expand = c(0,0), breaks = seq(0,3,1)) +
  geom_point(aes(col=Data_short), size=2) +geom_line(aes(col=Data_short), size=2) + theme_pubr() + scale_colour_brewer(palette = "Paired") + 
  theme(
    legend.position = "none",
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6,),
    legend.title=element_blank(),
    text = element_text(size = 20)
  ) + guides(alpha = "none") +
  ylab("Bias") + xlab("Relative Height (rh)")
Bias_plot  

Rsq_plot<-ggplot(error_summary2, aes(x=rh, y=rsq, col=Data_short, alpha=0.9)) + 
  scale_x_continuous(limits = c(0,105), expand = c(0,0), breaks = seq(0,100,25)) +
  scale_y_continuous (limits = c(0,1), expand = c(0,0), breaks = seq(0,1,0.2)) +
  geom_point(aes(col=Data_short), size=2) +geom_line(aes(col=Data_short), size=2) + theme_pubr() + scale_colour_brewer(palette = "Paired")+ 
  theme(
    legend.position = "none",
    text = element_text(size = 20)
  ) + guides(alpha = "none") +
  ylab("R-Squared") + xlab("Relative Height (rh)")
Rsq_plot

ggarrange(Rsq_plot, RMSE_plot, Bias_plot,
          ncol = 3,
          nrow = 1, 
          common.legend = TRUE, 
          legend = "top")

# Box plots of error through canopy
library(reshape)
L2A_error<-(L2A_compare1[,c(16:116)] - L2A_compare1[,c(173:273)])
L2A_error_melt<-melt(L2A_error)
L2A_error_melt$rh<-rep(0:100, each=nrow(L2A_error))
L2A_error_melt$rh<-as.factor(L2A_error_melt$rh)
ggplot(L2A_error_melt, aes(x=rh, y=value)) + geom_boxplot(outlier.shape = 1) + theme_pubr() +
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=0.2) + ylab("Error (GEDI - GEDIsim)")

ggplot(L2A_error_melt, aes(x=rh, y=value)) + geom_violin(fill="darkolivegreen") + geom_boxplot(width=0.2) + theme_pubr() +
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=0.2) + ylab("Error (GEDI - GEDIsim)")

ggplot(L2A_error_melt, aes(x=rh, y=value)) + geom_boxplot()+ geom_violin(fill=NA)  + theme_pubr() +
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=0.2) + ylab("Error (GEDI - GEDIsim)")


L2A_error<-(L2A_compare1[,c(16:116)] - L2A_compare1[,c(173:273)])
L2A_error_melt<-melt(L2A_error)
L2A_error_melt$rh<-rep(0:100, each=nrow(L2A_error))
L2A_error_melt$rh<-as.factor(L2A_error_melt$rh)
ggplot(L2A_error_melt, aes(x=rh, y=value)) + geom_boxplot(outlier.shape = 1) + theme_pubr() + 
  stat_summary(fun.y=mean, geom="point", shape=20, size=2, col="red") +
  geom_hline(yintercept=0, linetype="solid", color = "black", size=0.5) + ylab("Error (GEDI - GEDIsim)") +
  scale_y_continuous (limits = c(-32,32), expand = c(0,0), breaks = seq(-30,30,5)) +
  scale_x_discrete(name="Relative Height (rh)", labels=c(0, "", "", "", "", 5,
                                                         "", "", "", "", 10,
                                                         "", "", "", "", 15,
                                                         "", "", "", "", 20,
                                                         "", "", "", "", 25,
                                                         "", "", "", "", 30,
                                                         "", "", "", "", 35,
                                                         "", "", "", "", 40,
                                                         "", "", "", "", 45,
                                                         "", "", "", "", 50,
                                                         "", "", "", "", 55,
                                                         "", "", "", "", 60,
                                                         "", "", "", "", 65,
                                                         "", "", "", "", 70,
                                                         "", "", "", "", 75,
                                                         "", "", "", "", 80,
                                                         "", "", "", "", 85,
                                                         "", "", "", "", 90,
                                                         "", "", "", "", 95,
                                                         "", "", "", "", 100))

#### rh of GEDI to sim by rh10 step ####

d10<-get_density(L2A_compare1$rh_10, L2A_compare1$X24.rhGauss.10, n=50)
P10<-ggplot(L2A_compare1) + geom_point(aes(x=rh_10, y=X24.rhGauss.10, color=d10)) + 
  geom_abline(intercept = 0, slope = 1, size=1) + scale_color_viridis() +
  theme_bw() + theme(legend.position = "none") + 
  scale_x_continuous(limits = c(-10,30), expand = c(0,0), breaks = seq(-10,30,10)) +
  scale_y_continuous (limits = c(-10,30), expand = c(0,0), breaks = seq(-10,30,10))+ 
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=0.2)+ 
  geom_vline(xintercept=0, linetype="dashed", color = "red", size=0.2) +
  xlab("rh 10 (m)") + ylab("rh 10 (m)")
d20<-get_density(L2A_compare1$rh_20, L2A_compare1$X34.rhGauss.20, n=50)
P20<-ggplot(L2A_compare1) + geom_point(aes(x=rh_20, y=X34.rhGauss.20, color=d20)) + 
  geom_abline(intercept = 0, slope = 1, size=1) + scale_color_viridis() +
  theme_bw() + theme(legend.position = "none") + 
  scale_x_continuous(limits = c(-10,30), expand = c(0,0), breaks = seq(-10,30,10)) +
  scale_y_continuous (limits = c(-10,30), expand = c(0,0), breaks = seq(-10,30,10))+ 
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=0.2)+ 
  geom_vline(xintercept=0, linetype="dashed", color = "red", size=0.2) +
  xlab("rh 20 (m)") + ylab("rh 20 (m)")
d30<-get_density(L2A_compare1$rh_30, L2A_compare1$X44.rhGauss.30, n=50)
P30<-ggplot(L2A_compare1) + geom_point(aes(x=rh_30, y=X44.rhGauss.30, color=d30)) + 
  geom_abline(intercept = 0, slope = 1, size=1) + scale_color_viridis() +
  theme_bw() + theme(legend.position = "none") + 
  scale_x_continuous(limits = c(-5,45), expand = c(0,0), breaks = seq(0,40,10)) +
  scale_y_continuous (limits = c(-5,45), expand = c(0,0), breaks = seq(0,40,10))+ 
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=0.2)+ 
  geom_vline(xintercept=0, linetype="dashed", color = "red", size=0.2) +
  xlab("rh 30 (m)") + ylab("rh 30 (m)")
d40<-get_density(L2A_compare1$rh_40, L2A_compare1$X54.rhGauss.40, n=50)
P40<-ggplot(L2A_compare1) + geom_point(aes(x=rh_40, y=X54.rhGauss.40, color=d40)) + 
  geom_abline(intercept = 0, slope = 1, size=1) + scale_color_viridis() +
  theme_bw() + theme(legend.position = "none")+ 
  scale_x_continuous(limits = c(-5,45), expand = c(0,0), breaks = seq(0,40,10)) +
  scale_y_continuous (limits = c(-5,45), expand = c(0,0), breaks = seq(0,40,10))+ 
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=0.2)+ 
  geom_vline(xintercept=0, linetype="dashed", color = "red", size=0.2) +
  xlab("rh 40 (m)") + ylab("rh 40 (m)")
d50<-get_density(L2A_compare1$rh_50, L2A_compare1$X64.rhGauss.50, n=50)
P50<-ggplot(L2A_compare1) + geom_point(aes(x=rh_50, y=X64.rhGauss.50, color=d50)) + 
  geom_abline(intercept = 0, slope = 1, size=1) + scale_color_viridis() +
  theme_bw() + theme(legend.position = "none")+ 
  scale_x_continuous(limits = c(-5,50), expand = c(0,0), breaks = seq(0,50,10)) +
  scale_y_continuous (limits = c(-5,50), expand = c(0,0), breaks = seq(0,50,10))+ 
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=0.2)+ 
  geom_vline(xintercept=0, linetype="dashed", color = "red", size=0.2) +
  xlab("rh 50 (m)") + ylab("rh 50 (m)")
d60<-get_density(L2A_compare1$rh_60, L2A_compare1$X74.rhGauss.60, n=50)
P60<-ggplot(L2A_compare1) + geom_point(aes(x=rh_60, y=X74.rhGauss.60, color=d60)) + 
  geom_abline(intercept = 0, slope = 1, size=1) + scale_color_viridis() +
  theme_bw() + theme(legend.position = "none") + 
  scale_x_continuous(limits = c(-5,50), expand = c(0,0), breaks = seq(0,50,10)) +
  scale_y_continuous (limits = c(-5,50), expand = c(0,0), breaks = seq(0,50,10))+ 
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=0.2)+ 
  geom_vline(xintercept=0, linetype="dashed", color = "red", size=0.2) +
  xlab("rh 60 (m)") + ylab("rh 60 (m)")
d70<-get_density(L2A_compare1$rh_70, L2A_compare1$X84.rhGauss.70, n=50)
P70<-ggplot(L2A_compare1) + geom_point(aes(x=rh_70, y=X84.rhGauss.70, color=d70)) + 
  geom_abline(intercept = 0, slope = 1, size=1) + scale_color_viridis() +
  theme_bw() + theme(legend.position = "none") + 
  scale_x_continuous(limits = c(-5,50), expand = c(0,0), breaks = seq(0,50,10)) +
  scale_y_continuous (limits = c(-5,50), expand = c(0,0), breaks = seq(0,50,10))+ 
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=0.2)+ 
  geom_vline(xintercept=0, linetype="dashed", color = "red", size=0.2) +
  xlab("rh 70 (m)") + ylab("rh 70 (m)")
d80<-get_density(L2A_compare1$rh_80, L2A_compare1$X94.rhGauss.80, n=50)
P80<-ggplot(L2A_compare1) + geom_point(aes(x=rh_80, y=X94.rhGauss.80, color=d80)) + 
  geom_abline(intercept = 0, slope = 1, size=1) + scale_color_viridis() +
  theme_bw() + theme(legend.position = "none")   + 
  scale_x_continuous(limits = c(-5,50), expand = c(0,0), breaks = seq(0,50,10)) +
  scale_y_continuous (limits = c(-5,50), expand = c(0,0), breaks = seq(0,50,10)) + 
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=0.2)+ 
  geom_vline(xintercept=0, linetype="dashed", color = "red", size=0.2) +
  xlab("rh 80 (m)") + ylab("rh 80 (m)")
d90<-get_density(L2A_compare1$rh_90, L2A_compare1$X104.rhGauss.90, n=50)
P90<-ggplot(L2A_compare1) + geom_point(aes(x=rh_90, y=X104.rhGauss.90, color=d90)) + 
  geom_abline(intercept = 0, slope = 1, size=1) + scale_color_viridis() +
  theme_bw() + theme(legend.position = "none")  + 
  scale_x_continuous(limits = c(0,60), expand = c(0,0), breaks = seq(0,60,10)) +
  scale_y_continuous (limits = c(0,60), expand = c(0,0), breaks = seq(0,60,10)) + 
  geom_hline(yintercept=0.1, linetype="dashed", color = "red", size=0.2)+ 
  geom_vline(xintercept=0.1, linetype="dashed", color = "red", size=0.2) +
  xlab("rh 90 (m)") + ylab("rh 90 (m)")
d98<-get_density(L2A_compare1$rh_98, L2A_compare1$X112.rhGauss.98, n=50)
P98<-ggplot(L2A_compare1) + geom_point(aes(x=rh_98, y=X112.rhGauss.98, color=d98)) + 
  geom_abline(intercept = 0, slope = 1, size=1) + scale_color_viridis() + 
  theme_bw() + theme(legend.position = "none") + 
  scale_x_continuous(limits = c(0,60), expand = c(0,0), breaks = seq(0,60,10)) +
  scale_y_continuous (limits = c(0,60), expand = c(0,0), breaks = seq(0,60,10))+ 
  geom_hline(yintercept=0.1, linetype="dashed", color = "red", size=0.2)+ 
  geom_vline(xintercept=0.1, linetype="dashed", color = "red", size=0.2) +
  xlab("rh 98 (m)") + ylab("rh 98 (m)")

rh2by5<-ggarrange(P98, P90, P80, P70, 
                  P60, P50, P40, P30,
                  P20, P10,
                  ncol = 2, nrow = 5)
annotate_figure(rh2by5,
                top = text_grob("GEDI Measured rh Values", size = 18), 
                left = text_grob(bquote('GEDI'['sim']*' rh Values'), size = 18, rot = 90))
