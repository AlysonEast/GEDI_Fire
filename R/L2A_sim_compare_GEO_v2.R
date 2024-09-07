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

file_list <- list.files(path="/media/aly/Bridger/Thesis/gediSimulator/GEDI_Data/Zone19/sim/metrics/", full.names=FALSE)
path<-"/media/aly/Bridger/Thesis/gediSimulator/GEDI_Data/Zone19/sim/metrics/"
RH_sim <- read.delim(file=paste0(path,file_list[1]), sep = ",", header = TRUE)
RH_sim_colnames<-colnames(RH_sim)
for (i in 1:1){
  RH_sim <- read.delim(file=paste0(path,file_list[i]), sep = " ")
  colnames(RH_sim)<-RH_sim_colnames
  print(file_list[i])
}
for (i in 2:length(file_list)){
  temp_data <- read.delim(file=paste0(path,file_list[i]), sep = " ")
  colnames(temp_data)<-RH_sim_colnames
  RH_sim <- rbind(RH_sim, temp_data)
  print(file_list[i])
}
RH_sim<-RH_sim[,1:483]
#write.csv(RH_sim, "/media/aly/Bridger/Thesis/Spatial/Scratch/Collocated_19_v4.csv") #

file_list <- list.files(path="/media/aly/Bridger/Thesis/gediSimulator/GEDI_Data/Zone21/sim/metrics/", full.names=FALSE)
path<-"/media/aly/Bridger/Thesis/gediSimulator/GEDI_Data/Zone21/sim/metrics/"
RH_sim <- read.delim(file=paste0(path,file_list[1]), sep = ",", header = TRUE)
RH_sim_colnames<-colnames(RH_sim)
for (i in 1:1){
  RH_sim <- read.delim(file=paste0(path,file_list[i]), sep = " ")
  colnames(RH_sim)<-RH_sim_colnames
  print(file_list[i])
}
for (i in 2:length(file_list)){
  temp_data <- read.delim(file=paste0(path,file_list[i]), sep = " ")
  colnames(temp_data)<-RH_sim_colnames
  RH_sim <- rbind(RH_sim, temp_data)
  print(file_list[i])
}
RH_sim<-RH_sim[,1:483]
#write.csv(RH_sim, "/media/aly/Bridger/Thesis/Spatial/Scratch/Collocated_21_v4.csv")

#Read in L2A data from shapfile
L2A<-readOGR(dsn="/media/aly/Bridger/Thesis/Spatial/LiDAR", layer = "L2A_ALS_overlap_All_wFID")
#View(L2A@data)
#L2A_df<-read.csv("/media/aly/Bridger//Thesis/Spatial/LiDAR/L2A_ALS_overlap_All_wFID_Z21.csv")

# Reading in the Geolocation corrected data
Collo19<-readOGR(dsn="/media/aly/Bridger/Thesis/Spatial/LiDAR", layer = "Collocate_19_v4_w_L2A_join")
Collo21<-readOGR(dsn="/media/aly/Bridger/Thesis/Spatial/LiDAR", layer = "Collocate_21_v4_w_L2A_join")
#ColloAdd<-readOGR(dsn="/media/aly/Bridger/Thesis/Spatial/LiDAR/Shps", layer = "Collocated_addon")

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
LC18_1<-raster("/media/aly/Bridger/Thesis/Spatial/Rasters/MAPBIOMAS/LandCover/2018/mapbiomas-amazon-collection-30-matogrossobrasil-2018.tif")
LC18_2<-raster("/media/aly/Bridger/Thesis/Spatial/Rasters/MAPBIOMAS/LandCover/2018/mapbiomas-amazon-collection-30-para-2018.tif")
LC18_3<-raster("/media/aly/Bridger/Thesis/Spatial/Rasters/MAPBIOMAS/LandCover/2018/mapbiomas-amazon-collection-30-acrebrasil-2018.tif")

LC19<-raster("/media/aly/Bridger/Thesis/Spatial/Rasters/MAPBIOMAS/LandCover/2019/LC19.tif")

LC20_1<-raster("/media/aly/Bridger/Thesis/Spatial/Rasters/MAPBIOMAS/LandCover/2020/mapbiomas-amazon-collection-30-matogrossobrasil-2020.tif")
LC20_2<-raster("/media/aly/Bridger/Thesis/Spatial/Rasters/MAPBIOMAS/LandCover/2020/mapbiomas-amazon-collection-30-para2020.tif")
LC20_3<-raster("/media/aly/Bridger/Thesis/Spatial/Rasters/MAPBIOMAS/LandCover/2020/mapbiomas-amazon-collection-30-acrebrasil-2020.tif")

Slope_1<-raster("/media/aly/Bridger/Thesis/Spatial/Rasters/Terrain/slope-0000000000-0000032768-002.tif")
Slope_2<-raster("/media/aly/Bridger/Thesis/Spatial/Rasters/Terrain/slope-0000000000-0000000000-003.tif")
Slope_3<-raster("/media/aly/Bridger/Thesis/Spatial/Rasters/Terrain/slope-0000032768-0000032768.tif")
landforms<-raster("/media/aly/Bridger/Thesis/Spatial/Rasters/Terrain/landforms.tif")


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

M_CCI_18<-raster("/media/aly/Bridger/Thesis/Spatial/Rasters/CCI/BurnCat_18_MODIS_CCI.tif")
MB_18<-raster("/media/aly/Bridger/Thesis/Spatial/Rasters/MAPBIOMAS/Fire/2018/Mapbio18-0000065536-0000065536.tif")

Modis19<-raster("/media/aly/Bridger/Thesis/Spatial/M19_DOY.tif")
CCI19<-raster("/media/aly/Bridger/Thesis/Spatial/Rasters/CCI/CCI19_DOY.tif")
MB_19<-raster("/media/aly/Bridger/Thesis/Spatial/MAPBIOMAS-EXPORT/MAPBIOMAS_Fire.tif")

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

# #Making L2A data comparable to collocated data
# L2A@data$ID<-c(1:nrow(L2A@data))
# L2A@data$ID<-L2A@data$ID-1
# L2A_df<-L2A@data

table(L2A_spdf$distance)
table(L2A_spdf$FID)
str(L2A_spdf$FID)
#Creating unique Pairs
library(tidyverse)
library(dplyr)
L2A_spdf_unique<-L2A_spdf %>%
  group_by(FID) %>%
  arrange(distance) %>%
  slice(1)
hist(L2A_spdf_unique$distance)
L2A_spdf_unique<-subset(L2A_spdf_unique, distance<=40)

hist(L2A_spdf_unique$distance)
mean(L2A_spdf_unique$distance)
sd(L2A_spdf_unique$distance)


out_spdf<-Collo[Collo$X..1.wave. %in% L2A_spdf_unique$X..1.wave., ]
writeOGR(out_spdf, dsn="/media/aly/Bridger/Thesis/Spatial/LiDAR/", layer = "test", driver = "ESRI Shapefile", overwrite_layer = TRUE)

hist(L2A_spdf_unique$distance)
mean(L2A_spdf_unique$distance)

L2A_compare<-L2A_spdf_unique

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
dim(L2A_compare)
table(L2A_compare$Burn_18)
table(L2A_compare$Burn_19)
table(L2A_compare$LC_d18_19)
L2A_compare1<-subset(L2A_compare, Burn_18!=1 & Burn_19<1 & LC_d18_19==0)
dim(L2A_compare1)
table(L2A_compare1$LC19)
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


# Create a beam strenght classifcation
L2A_compare1$BeamType<- ifelse(L2A_compare1$BEAM=="BEAM0101"|
                                 L2A_compare1$BEAM=="BEAM0110"|
                                 L2A_compare1$BEAM=="BEAM1000"|
                                 L2A_compare1$BEAM=="BEAM1011", 
                               print("Full"), print("Coverage")) 
table(L2A_compare1$BeamType)

#check for orbits with fewer than 10 footprints and remove thos manually
table(L2A_compare1$X418.filen)
L2A_compare1<-subset(L2A_compare1,
                     X418.filen!="./GEDI_Data/Zone19/sim/sim_GEDI01_B_2019326073535_O05347_04_T03877_02_005_01_V002.h5" &
                       X418.filen!="./GEDI_Data/Zone19/sim/sim_GEDI01_B_2020366031529_O11623_01_T06348_02_005_02_V002.h5" &
                       X418.filen!="./GEDI_Data/Zone21/sim/sim_GEDI01_B_2019220124906_O03706_01_T03670_02_005_01_V002.h5" &
                       X418.filen!="./GEDI_Data/Zone21/sim/sim_GEDI01_B_2020179044014_O08725_01_T01436_02_005_01_V002.h5" &
                       X418.filen!="./GEDI_Data/Zone21/sim/sim_GEDI01_B_2020190235733_O08908_01_T00013_02_005_01_V002.h5" )
table(L2A_compare1$X418.filen)

#remove low beam density and low point density footprint. 
L2A_compare1<-subset(L2A_compare1, 
                     X434.point>=5 &
                     X435.beamD>=5)
dim(L2A_compare1)

#make hour a numeric value
L2A_compare1$CC_snstv_diff<-L2A_compare1$snstv-L2A_compare1$X11.cover
L2A_compare1$day<-as.numeric(L2A_compare1$day)
L2A_compare1$date_index<-ifelse(L2A_compare1$year==2019, print(L2A_compare1$day), 
                                print(L2A_compare1$day+365))
L2A_compare1$LC18<-as.factor(L2A_compare1$LC18)

colnames(L2A_compare1)[2:(length(RH_sim_colnames)+1)]<-RH_sim_colnames
colnames(L2A_compare1)[15:100]<-substr(colnames(L2A_compare1)[15:100],5,20)
colnames(L2A_compare1)[101:115]<-substr(colnames(L2A_compare1)[101:115],6,20)

#error calc
L2A_compare1$rh_98error<-(L2A_compare1$rh_98 - L2A_compare1$rhGauss.98)
L2A_compare1$rh_50error<-(L2A_compare1$rh_50 - L2A_compare1$rhGauss.50)

#Removing by Quality flag ####
L2A_compare1<-as.data.frame(L2A_compare1)
L2A_compare1<-L2A_compare1[-111,]

L2A_compareWflags<-L2A_compare1
dim(L2A_compare1)
L2A_compare1<-subset(L2A_compare1, dgrd_==0)
dim(L2A_compare1)
L2A_compare1<-subset(L2A_compare1, elv__==0)
dim(L2A_compare1)


#plotting metric aggrement with density as the color
get_density <- function(x, y, ...) {
  dens <- MASS::kde2d(x, y, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}
R<-expression(R^2)
#### Create Filtering Metrics ####
### Data subsetting according to literature standard
#night time
L2A_compare1$night<-ifelse(L2A_compare1$slr_l<0, print(1), print(0))

#full strength at night (Liu et al 2021)
L2A_compare1$Liu<-ifelse(L2A_compare1$slr_l<0 & L2A_compare1$BeamType=="Full", print(1), print(0))

#remove cover beams for coverage over 95
L2A_compare1$HighCC<-ifelse(L2A_compare1$X11.cover<0.95, print(1),
                            ifelse(L2A_compare1$X11.cover>0.95 & L2A_compare1$BeamType=="Full", print(1), print(0)))

#Sensitivity Canopy Cover difference
L2A_compare1$cc_snstv<-ifelse(L2A_compare1$CC_snstv_diff<0, print(0), print(1))

#Slope under 30degrees
L2A_compare1$Slope30<-ifelse(L2A_compare1$slope<30, print(1), print(0))

#sensitivities over .95
L2A_compare1$snstv95<-ifelse(L2A_compare1$snstv>0.95, print(1), print(0))

max(L2A_compare1$day) # Nov 24
min(L2A_compare1$day) # April 20
plot(L2A_error_gauss$X112.rhGau~L2A_compare1$day)
abline(v=278, col="red", lty=2)
abline(v=288, col="red", lty=2)
abline(v=90, col="blue")
abline(v=335, col="blue")
abline(v=182, col="brown")
abline(v=304, col="brown")

plot(L2A_compare1$elv_l~L2A_compare1$X2.true.gr)
plot(L2A_compare1$dgt__~L2A_compare1$X2.true.gr)

#### Read in uncorrected data and merge it ####
L2A_noGeo<-read.csv("/media/aly/Bridger/Thesis/Data/L2A_No_Geolocation_2023.csv")
L2A_noGeo<-L2A_noGeo[L2A_noGeo$FID %in% L2A_compare1$FID, ]
dim(L2A_noGeo)
dim(L2A_compare1)

L2A_compare1<-as.data.frame(L2A_compare1)
L2A_error_gauss<-(L2A_compare1[,c(499:599)]-L2A_compare1[,c(15:115)])
L2A_error_real<-(L2A_compare1[,c(318:418)] - L2A_compare1[,c(499:599)])

# Calculate error ####
library(Metrics)
#318:418 rhreal
#499:599 gedi
#15:115 Gaus

error_summary_noGeo<-as.data.frame(rep(c(1:100),2))
colnames(error_summary_noGeo)<-c("rh")
error_summary_noGeo$RMSE<-0
error_summary_noGeo$rsq<-0
error_summary_noGeo$RMSEpct<-0
error_summary_noGeo$Bias<-0
error_summary_noGeo$MAE<-0
error_summary_noGeo$MAEpct<-0

for (i in 1:100) {
  error_summary_noGeo[i,2]<-sqrt(mean((L2A_noGeo[,c((180+i))] - L2A_noGeo[,c((17+i))])^2))
  rss <- sum((L2A_noGeo[,c((17+i))]- L2A_noGeo[,c((180+i))])^ 2)  ## residual sum of squares
  tss <- sum((L2A_noGeo[,c((180+i))] - mean(L2A_noGeo[,c((180+i))])) ^ 2)  ## total sum of squares
  error_summary_noGeo[i,3] <- 1 - rss/tss
  error_summary_noGeo[i,4] <- ((error_summary_noGeo[i,2]/mean(L2A_noGeo[,c((180+i))]))*100)
  error_summary_noGeo[i,5]<-(sum(L2A_noGeo[,c((17+i))] - L2A_noGeo[,c((180+i))])/nrow(L2A_noGeo))
  error_summary_noGeo[i,6]<-(sum(abs(L2A_noGeo[,c((180+i))] - L2A_noGeo[,c((17+i))])))/nrow(L2A_noGeo)
  error_summary_noGeo[i,7] <- ((error_summary_noGeo[i,6]/mean(L2A_noGeo[,c((180+i))]))*100)
}
for (i in 1:100) {
  error_summary_noGeo[((100+i)),2]<-sqrt(mean((L2A_compare1[,c((15+i))] - L2A_compare1[,c((499+i))])^2))
  rss <- sum((L2A_compare1[,c((499+i))]- L2A_compare1[,c((15+i))])^ 2)  ## residual sum of squares
  tss <- sum((L2A_compare1[,c((15+i))] - mean(L2A_compare1[,c((15+i))])) ^ 2)  ## total sum of squares
  error_summary_noGeo[((100+i)),3] <- 1 - rss/tss
  error_summary_noGeo[((100+i)),4] <- ((error_summary_noGeo[((100+i)),2]/mean(L2A_compare1[,c((15+i))]))*100)
  error_summary_noGeo[((100+i)),5]<-(sum(L2A_compare1[,c((499+i))]-L2A_compare1[,c((15+i))])/nrow(L2A_compare1))
  error_summary_noGeo[((100+i)),6]<-(sum(abs(L2A_compare1[,c((15+i))] - L2A_compare1[,c((499+i))])))/nrow(L2A_compare1)
  error_summary_noGeo[((100+i)),7] <- ((error_summary_noGeo[((100+i)),6]/mean(L2A_compare1[,c((15+i))]))*100)
}

error_summary_noGeo$Data<-rep(c("No Geolocation Correction","Geolocation Corrected"), each=100)
error_summary_noGeo$Data_short<-rep(c("No Geolocation Correction","Geolocation Corrected"), each=100)
error_summary_noGeo$Data_short<-as.factor(error_summary_noGeo$Data_short)
levels(error_summary_noGeo$Data_short)
error_summary_noGeo$Data_short<-ordered(error_summary_noGeo$Data_short, levels=c("No Geolocation Correction","Geolocation Corrected"))

library(RColorBrewer)
RMSE_plot<-ggplot(error_summary_noGeo, aes(x=rh, y=RMSE, col=Data_short, alpha=0.25)) + 
  scale_x_continuous(limits = c(0,105), expand = c(0,0), breaks = seq(0,100,25)) +
  scale_y_continuous (limits = c(0,8.1), expand = c(0,0), breaks = seq(0,8,2)) +
  geom_point(aes(col=Data_short), size=2) +geom_line(aes(col=Data_short), size=2) + theme_pubr() + scale_colour_brewer(palette = "Paired") + 
  theme(
    legend.position = c(.9, .4),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(1, 1, 1, 1,),
    legend.title=element_blank(),
    text = element_text(size = 18)
  ) + guides(alpha = "none") +
  ylab("RMSE (m)") + xlab("Relative Height (RH)") + aes(group=rev(Data_short))
RMSE_plot 

RMSEpct_plot<-ggplot(error_summary_noGeo, aes(x=rh, y=RMSEpct, col=Data_short, alpha=0.25)) + 
  scale_x_continuous(limits = c(0,105), expand = c(0,0), breaks = seq(0,100,25)) +
  scale_y_continuous (limits = c(18,103), expand = c(0,0), breaks = seq(20,100,20)) +
  geom_point(aes(col=Data_short), size=2) +geom_line(aes(col=Data_short), size=2) + theme_pubr() + scale_colour_brewer(palette = "Paired") + 
  theme(
    legend.position = "none",
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6,),
    legend.title=element_blank(),
    text = element_text(size = 20)
  ) + guides(alpha = "none") +
  ylab("RMSE %") + xlab("Relative Height (RH)") + aes(group=rev(Data_short))
RMSEpct_plot  

Bias_plot<-ggplot(subset(error_summary_noGeo, Data_short!="Slopes < 30"), aes(x=rh, y=Bias, col=Data_short, alpha=0.25)) + 
  scale_x_continuous(limits = c(0,105), expand = c(0,0), breaks = seq(0,100,25)) +
  scale_y_continuous (limits = c(-3,0.1), expand = c(0,0), breaks = seq(-3,0,1)) +
  geom_point(aes(col=Data_short), size=2) +geom_line(aes(col=Data_short), size=2) + theme_pubr() + scale_colour_brewer(palette = "Paired") + 
  theme(
    legend.position = "none",
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(1, 1, 1, 1,),
    legend.title=element_blank(),
    text = element_text(size = 20)
  ) + guides(alpha = "none") +
  ylab("Bias (m)") + xlab("Relative Height (RH)") + aes(group=rev(Data_short))
Bias_plot  

Rsq_plot<-ggplot(subset(error_summary_noGeo, Data_short!="Slopes < 30"), aes(x=rh, y=rsq, col=Data_short, alpha=0.9)) + 
  scale_x_continuous(limits = c(0,105), expand = c(0,0), breaks = seq(0,100,25)) +
  scale_y_continuous (limits = c(0,0.85), expand = c(0,0), breaks = seq(0,0.8,0.2)) +
  geom_point(aes(col=Data_short), size=2) +geom_line(aes(col=Data_short), size=2) + theme_pubr() + scale_colour_brewer(palette = "Paired")+ 
  theme(
    legend.position = "none",
    text = element_text(size = 20)
  ) + guides(alpha = "none") +
  ylab(parse(text=paste(R))) + xlab("Relative Height (RH)") + aes(group=rev(Data_short))
Rsq_plot

MAE_plot<-ggplot(subset(error_summary_noGeo, Data_short!="Slopes < 30"), aes(x=rh, y=MAE, col=Data_short, alpha=0.9)) + 
  scale_x_continuous(limits = c(0,105), expand = c(0,0), breaks = seq(0,100,25)) +
  scale_y_continuous (limits = c(0,8.1), expand = c(0,0), breaks = seq(0,8,2)) +
  geom_point(aes(col=Data_short), size=2) +geom_line(aes(col=Data_short), size=2) + theme_pubr() + scale_colour_brewer(palette = "Paired")+ 
  theme(
    legend.position = "none",
    text = element_text(size = 20)
  ) + guides(alpha = "none") +
  ylab("MAE (m)") + xlab("Relative Height (RH)") + aes(group=rev(Data_short))
MAE_plot

MAEpct_plot<-ggplot(subset(error_summary_noGeo, Data_short!="Slopes < 30"), aes(x=rh, y=MAEpct, col=Data_short, alpha=0.9)) + 
  scale_x_continuous(limits = c(0,105), expand = c(0,0), breaks = seq(0,100,25)) +
  scale_y_continuous (limits = c(10,100), expand = c(0,0), breaks = seq(0,100,20)) +
  geom_point(aes(col=Data_short), size=2) +geom_line(aes(col=Data_short), size=2) + theme_pubr() + scale_colour_brewer(palette = "Paired")+ 
  theme(
    legend.position = "none",
    text = element_text(size = 20)
  ) + guides(alpha = "none") +
  ylab("MAE %") + xlab("Relative Height (RH)") + aes(group=rev(Data_short))
MAEpct_plot

#png(file="/media/aly/Bridger/Thesis/Figures/Final/SRS/Figure6.png", units="in", width=12, height=10, res=300)
ggpubr::ggarrange(Bias_plot, RMSE_plot, MAE_plot,
                  Rsq_plot, RMSEpct_plot, MAEpct_plot,
                  ncol = 3, 
                  nrow = 2, 
                  common.legend = FALSE)
dev.off()


#algorithm selection
error_summary_RealvGauss<-as.data.frame(rep(c(1:100),4))
colnames(error_summary_RealvGauss)<-c("rh")
error_summary_RealvGauss$RMSE<-0
error_summary_RealvGauss$rsq<-0
error_summary_RealvGauss$RMSEpct<-0
error_summary_RealvGauss$Bias<-0
error_summary_RealvGauss$MAE<-0
error_summary_RealvGauss$MAEpct<-0

for (i in 1:100) {
  error_summary_RealvGauss[i,2]<-sqrt(mean((L2A_compare1[,c((15+i))] - L2A_compare1[,c((499+i))])^2))
  rss <- sum((L2A_compare1[,c((499+i))]- L2A_compare1[,c((15+i))])^ 2)  ## residual sum of squares
  tss <- sum((L2A_compare1[,c((15+i))] - mean(L2A_compare1[,c((15+i))])) ^ 2)  ## total sum of squares
  error_summary_RealvGauss[i,3] <- 1 - rss/tss
  error_summary_RealvGauss[i,4] <- ((error_summary_RealvGauss[((100+i)),2]/mean(L2A_compare1[,c((15+i))]))*100)
  error_summary_RealvGauss[i,5]<-(sum(L2A_compare1[,c((499+i))]-L2A_compare1[,c((15+i))])/nrow(L2A_compare1))
  error_summary_RealvGauss[i,6]<-(sum(abs(L2A_compare1[,c((15+i))] - L2A_compare1[,c((499+i))])))/nrow(L2A_compare1)
  error_summary_RealvGauss[i,7] <- ((error_summary_RealvGauss[((100+i)),6]/mean(L2A_compare1[,c((15+i))]))*100)
}

for (i in 1:100) {
  error_summary_RealvGauss[((100+i)),2]<-sqrt(mean((L2A_compare1[,c((318+i))] - L2A_compare1[,c((499+i))])^2))
  rss <- sum((L2A_compare1[,c((499+i))]- L2A_compare1[,c((318+i))])^ 2)  ## residual sum of squares
  tss <- sum((L2A_compare1[,c((318+i))] - mean(L2A_compare1[,c((318+i))])) ^ 2)  ## total sum of squares
  error_summary_RealvGauss[((100+i)),3] <- 1 - rss/tss
  error_summary_RealvGauss[((100+i)),4] <- ((error_summary_RealvGauss[((100+i)),2]/mean(L2A_compare1[,c((318+i))]))*100)
  error_summary_RealvGauss[((100+i)),5]<-(sum(L2A_compare1[,c((499+i))]-L2A_compare1[,c((318+i))])/nrow(L2A_compare1))
  error_summary_RealvGauss[((100+i)),6]<-(sum(abs(L2A_compare1[,c((318+i))] - L2A_compare1[,c((499+i))])))/nrow(L2A_compare1)
  error_summary_RealvGauss[((100+i)),7] <- ((error_summary_RealvGauss[((100+i)),6]/mean(L2A_compare1[,c((318+i))]))*100)
}

for (i in 1:100) {
  error_summary_RealvGauss[((200+i)),2]<-sqrt(mean((L2A_compare1[,c((116+i))] - L2A_compare1[,c((499+i))])^2))
  rss <- sum((L2A_compare1[,c((499+i))]- L2A_compare1[,c((116+i))])^ 2)  ## residual sum of squares
  tss <- sum((L2A_compare1[,c((116+i))] - mean(L2A_compare1[,c((116+i))])) ^ 2)  ## total sum of squares
  error_summary_RealvGauss[((200+i)),3] <- 1 - rss/tss
  error_summary_RealvGauss[((200+i)),4] <- ((error_summary_RealvGauss[((100+i)),2]/mean(L2A_compare1[,c((116+i))]))*100)
  error_summary_RealvGauss[((200+i)),5]<-(sum(L2A_compare1[,c((499+i))]-L2A_compare1[,c((116+i))])/nrow(L2A_compare1))
  error_summary_RealvGauss[((200+i)),6]<-(sum(abs(L2A_compare1[,c((116+i))] - L2A_compare1[,c((499+i))])))/nrow(L2A_compare1)
  error_summary_RealvGauss[((200+i)),7] <- ((error_summary_RealvGauss[((100+i)),6]/mean(L2A_compare1[,c((116+i))]))*100)
}

for (i in 1:100) {
  error_summary_RealvGauss[((300+i)),2]<-sqrt(mean((L2A_compare1[,c((217+i))] - L2A_compare1[,c((499+i))])^2))
  rss <- sum((L2A_compare1[,c((499+i))]- L2A_compare1[,c((217+i))])^ 2)  ## residual sum of squares
  tss <- sum((L2A_compare1[,c((217+i))] - mean(L2A_compare1[,c((217+i))])) ^ 2)  ## total sum of squares
  error_summary_RealvGauss[((300+i)),3] <- 1 - rss/tss
  error_summary_RealvGauss[((300+i)),4] <- ((error_summary_RealvGauss[((100+i)),2]/mean(L2A_compare1[,c((217+i))]))*100)
  error_summary_RealvGauss[((300+i)),5]<-(sum(L2A_compare1[,c((499+i))]-L2A_compare1[,c((217+i))])/nrow(L2A_compare1))
  error_summary_RealvGauss[((300+i)),6]<-(sum(abs(L2A_compare1[,c((217+i))] - L2A_compare1[,c((499+i))])))/nrow(L2A_compare1)
  error_summary_RealvGauss[((300+i)),7] <- ((error_summary_RealvGauss[((100+i)),6]/mean(L2A_compare1[,c((217+i))]))*100)
}


error_summary_RealvGauss$Data<-rep(c("Gaussian","RhReal","Max","Inf"), each=100)
error_summary_RealvGauss$Data_short<-rep(c("Gaussian","RhReal","Max","Inf"), each=100)
error_summary_RealvGauss$Data_short<-as.factor(error_summary_RealvGauss$Data_short)
levels(error_summary_RealvGauss$Data_short)
error_summary_RealvGauss$Data_short<-ordered(error_summary_RealvGauss$Data_short, levels=c("Gaussian","RhReal","Max","Inf"))

library(RColorBrewer)
RMSE_plot<-ggplot(error_summary_RealvGauss, aes(x=rh, y=RMSE, col=Data_short, alpha=0.25)) + 
  scale_x_continuous(limits = c(0,105), expand = c(0,0), breaks = seq(0,100,25)) +
  scale_y_continuous (limits = c(0,8.1), expand = c(0,0), breaks = seq(0,8,2)) +
  geom_point(aes(col=Data_short), size=2) +geom_line(aes(col=Data_short), size=2) + theme_pubr() + scale_colour_brewer(palette = "Paired") + 
  theme(
    legend.position = c(1, .5),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(1, 1, 1, 1,),
    legend.title=element_blank(),
    text = element_text(size = 18)
  ) + guides(alpha = "none") +
  ylab("RMSE (m)") + xlab("Relative Height (RH)") + aes(group=rev(Data_short))
RMSE_plot 

RMSEpct_plot<-ggplot(error_summary_RealvGauss, aes(x=rh, y=RMSEpct, col=Data_short, alpha=0.25)) + 
  scale_x_continuous(limits = c(0,105), expand = c(0,0), breaks = seq(0,100,25)) +
  scale_y_continuous (limits = c(18,103), expand = c(0,0), breaks = seq(20,100,20)) +
  geom_point(aes(col=Data_short), size=2) +geom_line(aes(col=Data_short), size=2) + theme_pubr() + scale_colour_brewer(palette = "Paired") + 
  theme(
    legend.position = "none",
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6,),
    legend.title=element_blank(),
    text = element_text(size = 20)
  ) + guides(alpha = "none") +
  ylab("RMSE %") + xlab("Relative Height (RH)") + aes(group=rev(Data_short))
RMSEpct_plot  

Bias_plot<-ggplot(subset(error_summary_RealvGauss, Data_short!="Slopes < 30"), aes(x=rh, y=Bias, col=Data_short, alpha=0.25)) + 
  scale_x_continuous(limits = c(0,105), expand = c(0,0), breaks = seq(0,100,25)) +
    scale_y_continuous (limits = c(-5,0.1), expand = c(0,0), breaks = seq(-5,0,1)) +
  geom_point(aes(col=Data_short), size=2) +geom_line(aes(col=Data_short), size=2) + theme_pubr() + scale_colour_brewer(palette = "Paired") + 
  theme(
    legend.position = "none",
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(1, 1, 1, 1,),
    legend.title=element_blank(),
    text = element_text(size = 20)
  ) + guides(alpha = "none") +
  ylab("Bias (m)") + xlab("Relative Height (RH)") + aes(group=rev(Data_short))
Bias_plot  

Rsq_plot<-ggplot(subset(error_summary_RealvGauss, Data_short!="Slopes < 30"), aes(x=rh, y=rsq, col=Data_short, alpha=0.9)) + 
  scale_x_continuous(limits = c(0,105), expand = c(0,0), breaks = seq(0,100,25)) +
  scale_y_continuous (limits = c(0,0.85), expand = c(0,0), breaks = seq(0,0.8,0.2)) +
  geom_point(aes(col=Data_short), size=2) +geom_line(aes(col=Data_short), size=2) + theme_pubr() + scale_colour_brewer(palette = "Paired")+ 
  theme(
    legend.position = "none",
    text = element_text(size = 20)
  ) + guides(alpha = "none") +
  ylab(parse(text=paste(R))) + xlab("Relative Height (RH)") + aes(group=rev(Data_short))
Rsq_plot

MAE_plot<-ggplot(subset(error_summary_RealvGauss, Data_short!="Slopes < 30"), aes(x=rh, y=MAE, col=Data_short, alpha=0.9)) + 
  scale_x_continuous(limits = c(0,105), expand = c(0,0), breaks = seq(0,100,25)) +
  scale_y_continuous (limits = c(0,8.1), expand = c(0,0), breaks = seq(0,8,2)) +
  geom_point(aes(col=Data_short), size=2) +geom_line(aes(col=Data_short), size=2) + theme_pubr() + scale_colour_brewer(palette = "Paired")+ 
  theme(
    legend.position = "none",
    text = element_text(size = 20)
  ) + guides(alpha = "none") +
  ylab("MAE (m)") + xlab("Relative Height (RH)") + aes(group=rev(Data_short))
MAE_plot

MAEpct_plot<-ggplot(subset(error_summary_RealvGauss, Data_short!="Slopes < 30"), aes(x=rh, y=MAEpct, col=Data_short, alpha=0.9)) + 
  scale_x_continuous(limits = c(0,105), expand = c(0,0), breaks = seq(0,100,25)) +
  scale_y_continuous (limits = c(10,100), expand = c(0,0), breaks = seq(0,100,20)) +
  geom_point(aes(col=Data_short), size=2) +geom_line(aes(col=Data_short), size=2) + theme_pubr() + scale_colour_brewer(palette = "Paired")+ 
  theme(
    legend.position = "none",
    text = element_text(size = 20)
  ) + guides(alpha = "none") +
  ylab("MAE %") + xlab("Relative Height (RH)") + aes(group=rev(Data_short))
MAEpct_plot

#png(file="/media/aly/Bridger/Thesis/Figures/Final/SRS/Algorithm_comparison.png", units="in", width=12, height=10, res=300)
ggpubr::ggarrange(Bias_plot, RMSE_plot, MAE_plot,
                  Rsq_plot, RMSEpct_plot, MAEpct_plot,
                  ncol = 3, 
                  nrow = 2, 
                  common.legend = FALSE)
dev.off()

#Gaussian Lagorithm, Filtering.
RMSE<-sqrt(mean((L2A_compare1[,c(499)] - L2A_compare1[,c(15)])^2))
rss <- sum((L2A_compare1[,c((499))]- L2A_compare1[,c((115))])^ 2)  ## residual sum of squares
tss <- sum((L2A_compare1[,c((115))] - mean(L2A_compare1[,c((115))])) ^ 2)  ## total sum of squares
1 - rss/tss
error_summary2<-as.data.frame(rep(c(1:100),9))
colnames(error_summary2)<-c("rh")
error_summary2$RMSE<-0
error_summary2$rsq<-0
error_summary2$RMSEpct<-0
error_summary2$Bias<-0
error_summary2$MAE<-0
error_summary2$MAEpct<-0

for (i in 1:100) {
  error_summary2[i,2]<-sqrt(mean((L2A_compareWflags[,c((499+i))] - L2A_compareWflags[,c((15+i))])^2))
  rss <- sum((L2A_compareWflags[,c((15+i))]- L2A_compareWflags[,c((499+i))])^ 2)  ## residual sum of squares
  tss <- sum((L2A_compareWflags[,c((499+i))] - mean(L2A_compareWflags[,c((499+i))])) ^ 2)  ## total sum of squares
  error_summary2[i,3] <- 1 - rss/tss
  error_summary2[i,4] <- ((error_summary2[i,2]/mean(L2A_compareWflags[,c((499+i))]))*100)
  error_summary2[i,5]<-(sum(L2A_compareWflags[,c((15+i))] - L2A_compareWflags[,c((499+i))])/nrow(L2A_compareWflags))
  error_summary2[i,6]<-(sum(abs(L2A_compareWflags[,c((499+i))] - L2A_compareWflags[,c((15+i))])))/nrow(L2A_compareWflags)
  error_summary2[i,7] <- ((error_summary2[i,6]/mean(L2A_compareWflags[,c((499+i))]))*100)
}

for (i in 1:100) {
  error_summary2[((100+i)),2]<-sqrt(mean((L2A_compare1[,c((499+i))] - L2A_compare1[,c((15+i))])^2))
  rss <- sum((L2A_compare1[,c((15+i))]- L2A_compare1[,c((499+i))])^ 2)  ## residual sum of squares
  tss <- sum((L2A_compare1[,c((499+i))] - mean(L2A_compare1[,c((499+i))])) ^ 2)  ## total sum of squares
  error_summary2[((100+i)),3] <- 1 - rss/tss
  error_summary2[((100+i)),4] <- ((error_summary2[((100+i)),2]/mean(L2A_compare1[,c((499+i))]))*100)
  error_summary2[((100+i)),5]<-(sum(L2A_compare1[,c((15+i))]-L2A_compare1[,c((499+i))])/nrow(L2A_compare1))
  error_summary2[((100+i)),6]<-(sum(abs(L2A_compare1[,c((499+i))] - L2A_compare1[,c((15+i))])))/nrow(L2A_compare1)
  error_summary2[((100+i)),7] <- ((error_summary2[((100+i)),6]/mean(L2A_compare1[,c((499+i))]))*100)
}

error_summary2$Data<-rep(c("All Data","Flagged Data Removed*","Truncated", "Exclude Daytime Samples",
                           "Exclude Daytime & Coverage Beam Samples", "Sensitivity > 0.95", 
                           "Exclude Coverage Beams For Canopy Cover > 95%",
                           "Slopes >30 Degrees",
                           "Sensitivity < Canopy Cover"), each=100)
error_summary2$Data_short<-rep(c("All Data","Flags Removed*","Truncated", "Night",
                                 "Night w/ Power Beams", "Sensitivity > 0.95", 
                                 "No Coverage w/ Cover > 0.95",
                                 "Slopes < 30",
                                 "Sensitivity < Canopy Cover"), each=100)
error_summary2$Data_short<-as.factor(error_summary2$Data_short)
levels(error_summary2$Data_short)
error_summary2$Data_short<-ordered(error_summary2$Data_short, levels=c("All Data","Flags Removed*","Truncated", "Night",
                                                                       "Night w/ Power Beams", "Sensitivity > 0.95", 
                                                                       "No Coverage w/ Cover > 0.95",
                                                                       "Slopes < 30",
                                                                       "Sensitivity < Canopy Cover"))

L2A_compare_trunk<-L2A_compare1
L2A_compare_trunk[,c(499:599)][L2A_compare_trunk[,c(499:599)] < 0] <-0
L2A_compare_trunk[,c(15:115)][L2A_compare_trunk[,c(15:115)] < 0] <-0
L2A_compare_trunk[,c(318:418)][L2A_compare_trunk[,c(318:418)] < 0] <-0

for (i in 1:100) {
  error_summary2[(200+i),2]<-sqrt(mean((L2A_compare_trunk[,c((499+i))] - L2A_compare_trunk[,c((15+i))])^2))
  rss <- sum((L2A_compare_trunk[,c((15+i))]- L2A_compare_trunk[,c((499+i))])^ 2)  ## residual sum of squares
  tss <- sum((L2A_compare_trunk[,c((499+i))] - mean(L2A_compare_trunk[,c((499+i))])) ^ 2)  ## total sum of squares
  error_summary2[(200+i),3] <- 1 - rss/tss
  error_summary2[((200+i)),4] <- ((error_summary2[((200+i)),2]/mean(L2A_compare_trunk[,c((499+i))]))*100)
  error_summary2[((200+i)),5]<-(sum(L2A_compare_trunk[,c((15+i))]-L2A_compare_trunk[,c((499+i))])/nrow(L2A_compare_trunk))
  error_summary2[((200+i)),6]<-(sum(abs(L2A_compare_trunk[,c((499+i))] - L2A_compare_trunk[,c((15+i))])))/nrow(L2A_compare_trunk)
  error_summary2[((200+i)),7] <- ((error_summary2[((200+i)),6]/mean(L2A_compare_trunk[,c((499+i))]))*100)
}

list<-c("flag","No Flags","night","Liu","snstv95","HighCC","Slope30","cc_snstv")
for (j in 3:length(list)) {
  for (i in 1:100) {
    temp_data<-subset(L2A_compare1, L2A_compare1[,c(list[j])]==1)
    error_summary2[((j*100)+i),2]<-sqrt(mean((temp_data[,c((499+i))] - temp_data[,c((15+i))])^2))
    rss <- sum((temp_data[,c((15+i))]- temp_data[,c((499+i))])^ 2)  ## residual sum of squares
    tss <- sum((temp_data[,c((499+i))] - mean(temp_data[,c((499+i))])) ^ 2)  ## total sum of squares
    error_summary2[((j*100)+i),3] <- 1 - rss/tss
    error_summary2[((j*100)+i),4] <- ((error_summary2[((j*100)+i),2]/mean(temp_data[,c((499+i))]))*100)
    error_summary2[((j*100)+i),5]<-(sum(temp_data[,c((15+i))]-temp_data[,c((499+i))])/nrow(temp_data))
    error_summary2[((j*100)+i),6]<-(sum(abs(temp_data[,c((499+i))] - temp_data[,c((15+i))])))/nrow(temp_data)
    error_summary2[((j*100)+i),7] <- ((error_summary2[((j*100)+i),6]/mean(temp_data[,c((499+i))]))*100)
  }
}

error_summary2$Bias<-(error_summary2$Bias*-1)

library(RColorBrewer)
RMSE_plot<-ggplot(subset(error_summary2, Data_short!="Slopes < 30"), aes(x=rh, y=RMSE, col=Data_short, alpha=0.25)) + 
  scale_x_continuous(limits = c(0,105), expand = c(0,0), breaks = seq(0,100,25)) +
  scale_y_continuous (limits = c(0,8.1), expand = c(0,0), breaks = seq(0,8,2)) +
  geom_point(aes(col=Data_short), size=2) +geom_line(aes(col=Data_short), size=2) + theme_pubr() + scale_colour_brewer(palette = "Paired") + 
  theme(
    legend.position = "none",
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(1, 1, 1, 1,),
    legend.title=element_blank(),
    text = element_text(size = 18)
  ) + guides(alpha = "none") +
  ylab("RMSE (m)") + xlab("Relative Height (RH)") + aes(group=rev(Data_short))
RMSE_plot 

RMSEpct_plot<-ggplot(subset(error_summary2, Data_short!="Slopes < 30"), aes(x=rh, y=RMSEpct, col=Data_short, alpha=0.25)) + 
  scale_x_continuous(limits = c(0,105), expand = c(0,0), breaks = seq(0,100,25)) +
  scale_y_continuous (limits = c(18,103), expand = c(0,0), breaks = seq(20,100,20)) +
  geom_point(aes(col=Data_short), size=2) +geom_line(aes(col=Data_short), size=2) + theme_pubr() + scale_colour_brewer(palette = "Paired") + 
  theme(
    legend.position = "none",
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6,),
    legend.title=element_blank(),
    text = element_text(size = 20)
  ) + guides(alpha = "none") +
  ylab("RMSE %") + xlab("Relative Height (RH)") + aes(group=rev(Data_short))
RMSEpct_plot  

Bias_plot<-ggplot(subset(error_summary2, Data_short!="Slopes < 30"), aes(x=rh, y=Bias, col=Data_short, alpha=0.25)) + 
  scale_x_continuous(limits = c(0,105), expand = c(0,0), breaks = seq(0,100,25)) +
#  scale_y_continuous (limits = c(-3,0.1), expand = c(0,0), breaks = seq(-3,0,1)) +
  geom_point(aes(col=Data_short), size=2) +geom_line(aes(col=Data_short), size=2) + theme_pubr() + scale_colour_brewer(palette = "Paired") + 
  theme(
    legend.position = c(.92, .5),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(1, 1, 1, 1,),
    legend.title=element_blank(),
    text = element_text(size = 18)
  ) + guides(alpha = "none") +
  ylab("Bias (m)") + xlab("Relative Height (RH)") + aes(group=rev(Data_short))
Bias_plot  

Rsq_plot<-ggplot(subset(error_summary2, Data_short!="Slopes < 30"), aes(x=rh, y=rsq, col=Data_short, alpha=0.9)) + 
  scale_x_continuous(limits = c(0,105), expand = c(0,0), breaks = seq(0,100,25)) +
  scale_y_continuous (limits = c(0,0.9), expand = c(0,0), breaks = seq(0,0.8,0.2)) +
  geom_point(aes(col=Data_short), size=2) +geom_line(aes(col=Data_short), size=2) + theme_pubr() + scale_colour_brewer(palette = "Paired")+ 
  theme(
    legend.position = "none",
    text = element_text(size = 20)
  ) + guides(alpha = "none") +
  ylab(parse(text=paste(R))) + xlab("Relative Height (RH)") + aes(group=rev(Data_short))
Rsq_plot

MAE_plot<-ggplot(subset(error_summary2, Data_short!="Slopes < 30"), aes(x=rh, y=MAE, col=Data_short, alpha=0.9)) + 
  scale_x_continuous(limits = c(0,105), expand = c(0,0), breaks = seq(0,100,25)) +
  scale_y_continuous (limits = c(0,8.1), expand = c(0,0), breaks = seq(0,8,2)) +
  geom_point(aes(col=Data_short), size=2) +geom_line(aes(col=Data_short), size=2) + theme_pubr() + scale_colour_brewer(palette = "Paired")+ 
  theme(
    legend.position = "none",
    text = element_text(size = 20)
  ) + guides(alpha = "none") +
  ylab("MAE (m)") + xlab("Relative Height (RH)") + aes(group=rev(Data_short))
MAE_plot

MAEpct_plot<-ggplot(subset(error_summary2, Data_short!="Slopes < 30"), aes(x=rh, y=MAEpct, col=Data_short, alpha=0.9)) + 
  scale_x_continuous(limits = c(0,105), expand = c(0,0), breaks = seq(0,100,25)) +
  scale_y_continuous (limits = c(10,100), expand = c(0,0), breaks = seq(0,100,20)) +
  geom_point(aes(col=Data_short), size=2) +geom_line(aes(col=Data_short), size=2) + theme_pubr() + scale_colour_brewer(palette = "Paired")+ 
  theme(
    legend.position = "none",
    text = element_text(size = 20)
  ) + guides(alpha = "none") +
  ylab("MAE %") + xlab("Relative Height (RH)") + aes(group=rev(Data_short))
MAEpct_plot

#tiff(file="/media/aly/Bridger/Thesis/Figures/Final/SRS/Figure7.tiff", units="in", width=12, height=10, res=300)
ggpubr::ggarrange(Bias_plot, RMSE_plot, MAE_plot,
                  Rsq_plot, RMSEpct_plot, MAEpct_plot,
                  ncol = 3, 
                  nrow = 2, 
                  common.legend = FALSE)
dev.off()

#Values for MS Tables####

min(error_summary_noGeo[10:100,3] - error_summary_noGeo[110:200,3])
which.min(error_summary_noGeo[10:100,3] - error_summary_noGeo[110:200,3])

error_summary_noGeo[1:100,2] - error_summary_noGeo[101:200,2]
error_summary_noGeo[1:100,5] - error_summary_noGeo[101:200,5]

dim(subset(L2A_compare1, night==1))
dim(subset(L2A_compare1, Liu==1))
dim(subset(L2A_compare1, HighCC==1))
dim(subset(L2A_compare1, snstv95==1))
dim(subset(L2A_compare1, Slope30==1))
dim(subset(L2A_compare1, cc_snstv==1))

(dim(L2A_compare1)-dim(subset(L2A_compare1, night==1)))/dim(L2A_compare1)
(dim(L2A_compare1)-dim(subset(L2A_compare1, Liu==1)))/dim(L2A_compare1)
(dim(L2A_compare1)-dim(subset(L2A_compare1, snstv95==1)))/dim(L2A_compare1)
(dim(L2A_compare1)-dim(subset(L2A_compare1, HighCC==1)))/dim(L2A_compare1)
(dim(L2A_compare1)-dim(subset(L2A_compare1, Slope30==1)))/dim(L2A_compare1)
(dim(L2A_compare1)-dim(subset(L2A_compare1, cc_snstv==1)))/dim(L2A_compare1)

head(error_summary2)

error_summary_noGeo[50,]
paste0(substr(mean(L2A_noGeo$rh_50error)-(2*(sd(L2A_noGeo$rh_50error))), 1,6)," : ",
       substr(mean(L2A_noGeo$rh_50error)+(2*(sd(L2A_noGeo$rh_50error))), 1, 4))
error_summary_noGeo[98,]
paste0(substr(mean(L2A_noGeo$rh_98error)-(2*(sd(L2A_noGeo$rh_98error))), 1,6)," : ",
       substr(mean(L2A_noGeo$rh_98error)+(2*(sd(L2A_noGeo$rh_98error))), 1, 4))

#with flags
error_summary2[50,]
paste0(substr(mean(L2A_compareWflags$rh_50error)-(2*(sd(L2A_compareWflags$rh_50error))), 1,6)," : ",substr(mean(L2A_compareWflags$rh_50error)+(2*(sd(L2A_compareWflags$rh_50error))), 1, 4))
error_summary2[98,]
paste0(substr(mean(L2A_compareWflags$rh_98error)-(2*(sd(L2A_compareWflags$rh_98error))), 1,6)," : ",substr(mean(L2A_compareWflags$rh_98error)+(2*(sd(L2A_compareWflags$rh_98error))), 1, 4))

error_summary2[150,]
paste0(substr(mean(L2A_compare1$rh_50error)-(2*(sd(L2A_compare1$rh_50error))), 1,6)," : ",
       substr(mean(L2A_compare1$rh_50error)+(2*(sd(L2A_compare1$rh_50error))), 1, 4))
error_summary2[198,]
paste0(substr(mean(L2A_compare1$rh_98error)-(2*(sd(L2A_compare1$rh_98error))), 1,6)," : ",
       substr(mean(L2A_compare1$rh_98error)+(2*(sd(L2A_compare1$rh_98error))), 1, 4))

error_summary2[250,]
paste0(substr(mean(L2A_compare_trunk$rh_50error)-(2*(sd(L2A_compare_trunk$rh_50error))), 1,6)," : ",
       substr(mean(L2A_compare_trunk$rh_50error)+(2*(sd(L2A_compare_trunk$rh_50error))), 1, 4))
error_summary2[198,]
paste0(substr(mean(L2A_compare_trunk$rh_98error)-(2*(sd(L2A_compare_trunk$rh_98error))), 1,6)," : ",
       substr(mean(L2A_compare_trunk$rh_98error)+(2*(sd(L2A_compare_trunk$rh_98error))), 1, 4))

temp_data<-subset(L2A_compare1, night==1)
error_summary2[350,]
paste0(substr(mean(temp_data$rh_50error)-(2*(sd(temp_data$rh_50error))), 1,6)," : ",
       substr(mean(temp_data$rh_50error)+(2*(sd(temp_data$rh_50error))), 1, 4))
error_summary2[398,]
paste0(substr(mean(temp_data$rh_98error)-(2*(sd(temp_data$rh_98error))), 1,6)," : ",
       substr(mean(temp_data$rh_98error)+(2*(sd(temp_data$rh_98error))), 1, 4))

temp_data<-subset(L2A_compare1, Liu==1)
error_summary2[450,]
paste0(substr(mean(temp_data$rh_50error)-(2*(sd(temp_data$rh_50error))), 1,6)," : ",
       substr(mean(temp_data$rh_50error)+(2*(sd(temp_data$rh_50error))), 1, 4))
error_summary2[498,]
paste0(substr(mean(temp_data$rh_98error)-(2*(sd(temp_data$rh_98error))), 1,6)," : ",
       substr(mean(temp_data$rh_98error)+(2*(sd(temp_data$rh_98error))), 1, 4))

temp_data<-subset(L2A_compare1, snstv95==1)
error_summary2[550,]
paste0(substr(mean(temp_data$rh_50error)-(2*(sd(temp_data$rh_50error))), 1,6)," : ",
       substr(mean(temp_data$rh_50error)+(2*(sd(temp_data$rh_50error))), 1, 4))
error_summary2[498,]
paste0(substr(mean(temp_data$rh_98error)-(2*(sd(temp_data$rh_98error))), 1,6)," : ",
       substr(mean(temp_data$rh_98error)+(2*(sd(temp_data$rh_98error))), 1, 4))

temp_data<-subset(L2A_compare1, HighCC==1)
error_summary2[650,]
paste0(substr(mean(temp_data$rh_50error)-(2*(sd(temp_data$rh_50error))), 1,6)," : ",
       substr(mean(temp_data$rh_50error)+(2*(sd(temp_data$rh_50error))), 1, 4))
error_summary2[698,]
paste0(substr(mean(temp_data$rh_98error)-(2*(sd(temp_data$rh_98error))), 1,6)," : ",
       substr(mean(temp_data$rh_98error)+(2*(sd(temp_data$rh_98error))), 1, 4))

temp_data<-subset(L2A_compare1, Slope30==1)
error_summary2[750,]
paste0(substr(mean(temp_data$rh_50error)-(2*(sd(temp_data$rh_50error))), 1,6)," : ",
       substr(mean(temp_data$rh_50error)+(2*(sd(temp_data$rh_50error))), 1, 4))
error_summary2[798,]
paste0(substr(mean(temp_data$rh_98error)-(2*(sd(temp_data$rh_98error))), 1,6)," : ",
       substr(mean(temp_data$rh_98error)+(2*(sd(temp_data$rh_98error))), 1, 4))

temp_data<-subset(L2A_compare1, cc_snstv==1)
error_summary2[850,]
paste0(substr(mean(temp_data$rh_50error)-(2*(sd(temp_data$rh_50error))), 1,6)," : ",
       substr(mean(temp_data$rh_50error)+(2*(sd(temp_data$rh_50error))), 1, 4))
error_summary2[898,]
paste0(substr(mean(temp_data$rh_98error)-(2*(sd(temp_data$rh_98error))), 1,6)," : ",
       substr(mean(temp_data$rh_98error)+(2*(sd(temp_data$rh_98error))), 1, 4))


#### Figures ####
library(reshape)
L2A_error_melt<-melt(L2A_error_gauss)
L2A_error_melt$rh<-rep(0:100, each=nrow(L2A_error_gauss))
L2A_error_melt$rh<-as.factor(L2A_error_melt$rh)
L2A_error_melt$rh_bin<-c(rep(1, each=nrow(L2A_error_gauss)),
                         rep(1:19, each=nrow(L2A_error_gauss)*5), 
                         rep(20, each=nrow(L2A_error_gauss)*4), 
                         rep(21, each=nrow(L2A_error_gauss)))
L2A_error_melt$rh_bin<-as.factor(L2A_error_melt$rh_bin)
bins<-L2A_error_melt[c(nrow(L2A_error_gauss)+1:nrow(L2A_error_melt)),]
bins<-na.omit(L2A_error_melt)

tiff(file="/media/aly/Bridger/Thesis/Figures/Final/SRS/Figure5.tiff", units="in", width=12, height=10, res=300)
ggplot(bins, aes(x=rh_bin, y=value)) + geom_boxplot(outlier.shape = 1) + theme_pubr() + 
  stat_summary(fun.y=mean, geom="point", shape=20, size=2, col="red") +
  geom_hline(yintercept=0, linetype="solid", color = "black", size=0.5) + ylab("Error (GEDI - GEDIsim) (m)") +
  scale_y_continuous (limits = c(-32,32), expand = c(0,0), breaks = seq(-30,30,5)) +
  scale_x_discrete(name="Relative Height Bins", labels=c("0-5","6-10",
                                                         "11-15","16-20",
                                                         "21-25","26-30",
                                                         "31-35","36-40",
                                                         "41-45","46-50",
                                                         "51-55","56-60",
                                                         "61-65","66-70",
                                                         "71-75","76-80",
                                                         "81-85","86-90",
                                                         "91-95","96-99",
                                                         "100")) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
  theme(text = element_text(size=20))
dev.off()

d0<-get_density(L2A_compare1$rh_0, L2A_compare1$rhGauss.0, n=50)
P0<-ggplot(L2A_compare1) + geom_point(aes(x=rh_0, y=rhGauss.0, color=d0)) + 
  geom_abline(intercept = 0, slope = 1, size=1) + scale_color_viridis() +
  theme_bw() + theme(legend.position = "none") +
  scale_x_continuous(limits = c(-20,1), expand = c(0,0), breaks = seq(-20,0,5)) +
  scale_y_continuous (limits = c(-20,1), expand = c(0,0), breaks = seq(-20,0,5))+
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=0.2)+ 
  geom_vline(xintercept=0, linetype="dashed", color = "red", size=0.2) +
  xlab("RH 0 (m)") + ylab("RH 0 (m)") +
  geom_text(x=Inf, y=-Inf, hjust=1, vjust=-.25, 
            label=parse(text=paste(R,":", substr(error_summary2[101,3],1,5))))
d10<-get_density(L2A_compare1$rh_10, L2A_compare1$rhGauss.10, n=50)
P10<-ggplot(L2A_compare1) + geom_point(aes(x=rh_10, y=rhGauss.10, color=d10)) + 
  geom_abline(intercept = 0, slope = 1, size=1) + scale_color_viridis() +
  theme_bw() + theme(legend.position = "none") + 
  scale_x_continuous(limits = c(-10,25), expand = c(0,0), breaks = seq(-10,30,10)) +
  scale_y_continuous (limits = c(-10,25), expand = c(0,0), breaks = seq(-10,30,10))+ 
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=0.2)+ 
  geom_vline(xintercept=0, linetype="dashed", color = "red", size=0.2) +
  xlab("RH 10 (m)") + ylab("RH 10 (m)") +
  geom_text(x=Inf, y=-Inf, hjust=1, vjust=-.25, 
            label=parse(text=paste(R,":", substr(error_summary2[110,3],1,5))))
d20<-get_density(L2A_compare1$rh_20, L2A_compare1$rhGauss.20, n=50)
P20<-ggplot(L2A_compare1) + geom_point(aes(x=rh_20, y=rhGauss.20, color=d20)) + 
  geom_abline(intercept = 0, slope = 1, size=1) + scale_color_viridis() +
  theme_bw() + theme(legend.position = "none") + 
  scale_x_continuous(limits = c(-10,40), expand = c(0,0), breaks = seq(-10,40,10)) +
  scale_y_continuous (limits = c(-10,40), expand = c(0,0), breaks = seq(-10,40,10))+ 
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=0.2)+ 
  geom_vline(xintercept=0, linetype="dashed", color = "red", size=0.2) +
  xlab("RH 20 (m)") + ylab("RH 20 (m)")+
  geom_text(x=Inf, y=-Inf, hjust=1, vjust=-.25, 
            label=parse(text=paste(R,":", substr(error_summary2[120,3],1,4))))
d30<-get_density(L2A_compare1$rh_30, L2A_compare1$rhGauss.30, n=50)
P30<-ggplot(L2A_compare1) + geom_point(aes(x=rh_30, y=rhGauss.30, color=d30)) + 
  geom_abline(intercept = 0, slope = 1, size=1) + scale_color_viridis() +
  theme_bw() + theme(legend.position = "none") + 
  scale_x_continuous(limits = c(-10,40), expand = c(0,0), breaks = seq(-10,40,10)) +
  scale_y_continuous (limits = c(-10,40), expand = c(0,0), breaks = seq(-10,40,10))+ 
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=0.2)+ 
  geom_vline(xintercept=0, linetype="dashed", color = "red", size=0.2) +
  xlab("RH 30 (m)") + ylab("RH 30 (m)")+
  geom_text(x=Inf, y=-Inf, hjust=1, vjust=-.25, 
            label=parse(text=paste(R,":", substr(error_summary2[130,3],1,4))))
d40<-get_density(L2A_compare1$rh_40, L2A_compare1$rhGauss.40, n=50)
P40<-ggplot(L2A_compare1) + geom_point(aes(x=rh_40, y=rhGauss.40, color=d40)) + 
  geom_abline(intercept = 0, slope = 1, size=1) + scale_color_viridis() +
  theme_bw() + theme(legend.position = "none")+ 
  scale_x_continuous(limits = c(-5,50), expand = c(0,0), breaks = seq(0,50,10)) +
  scale_y_continuous (limits = c(-5,50), expand = c(0,0), breaks = seq(0,50,10))+ 
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=0.2)+ 
  geom_vline(xintercept=0, linetype="dashed", color = "red", size=0.2) +
  xlab("RH 40 (m)") + ylab("RH 40 (m)")+
  geom_text(x=Inf, y=-Inf, hjust=1, vjust=-.25, 
            label=parse(text=paste(R,":", substr(error_summary2[140,3],1,4))))
d50<-get_density(L2A_compare1$rh_50, L2A_compare1$rhGauss.50, n=50)
P50<-ggplot(L2A_compare1) + geom_point(aes(x=rh_50, y=rhGauss.50, color=d50)) + 
  geom_abline(intercept = 0, slope = 1, size=1) + scale_color_viridis() +
  theme_bw() + theme(legend.position = "none")+ 
  scale_x_continuous(limits = c(-5,50), expand = c(0,0), breaks = seq(0,50,10)) +
  scale_y_continuous (limits = c(-5,50), expand = c(0,0), breaks = seq(0,50,10))+ 
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=0.2)+ 
  geom_vline(xintercept=0, linetype="dashed", color = "red", size=0.2) +
  xlab("RH 50 (m)") + ylab("RH 50 (m)")+
  geom_text(x=Inf, y=-Inf, hjust=1, vjust=-.25, 
            label=parse(text=paste(R,":", substr(error_summary2[150,3],1,4))))
d60<-get_density(L2A_compare1$rh_60, L2A_compare1$rhGauss.60, n=50)
P60<-ggplot(L2A_compare1) + geom_point(aes(x=rh_60, y=rhGauss.60, color=d60)) + 
  geom_abline(intercept = 0, slope = 1, size=1) + scale_color_viridis() +
  theme_bw() + theme(legend.position = "none") + 
  scale_x_continuous(limits = c(-5,50), expand = c(0,0), breaks = seq(0,50,10)) +
  scale_y_continuous (limits = c(-5,50), expand = c(0,0), breaks = seq(0,50,10))+ 
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=0.2)+ 
  geom_vline(xintercept=0, linetype="dashed", color = "red", size=0.2) +
  xlab("RH 60 (m)") + ylab("RH 60 (m)")+
  geom_text(x=Inf, y=-Inf, hjust=1, vjust=-.25, 
            label=parse(text=paste(R,":", substr(error_summary2[160,3],1,4))))
d70<-get_density(L2A_compare1$rh_70, L2A_compare1$rhGauss.70, n=50)
P70<-ggplot(L2A_compare1) + geom_point(aes(x=rh_70, y=rhGauss.70, color=d70)) + 
  geom_abline(intercept = 0, slope = 1, size=1) + scale_color_viridis() +
  theme_bw() + theme(legend.position = "none") + 
  scale_x_continuous(limits = c(-5,50), expand = c(0,0), breaks = seq(0,50,10)) +
  scale_y_continuous (limits = c(-5,50), expand = c(0,0), breaks = seq(0,50,10))+ 
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=0.2)+ 
  geom_vline(xintercept=0, linetype="dashed", color = "red", size=0.2) +
  xlab("RH 70 (m)") + ylab("RH 70 (m)")+
  geom_text(x=Inf, y=-Inf, hjust=1, vjust=-.25, 
            label=parse(text=paste(R,":", substr(error_summary2[170,3],1,4))))
d80<-get_density(L2A_compare1$rh_80, L2A_compare1$rhGauss.80, n=50)
P80<-ggplot(L2A_compare1) + geom_point(aes(x=rh_80, y=rhGauss.80, color=d80)) + 
  geom_abline(intercept = 0, slope = 1, size=1) + scale_color_viridis() +
  theme_bw() + theme(legend.position = "none")   + 
  scale_x_continuous(limits = c(-1,60), expand = c(0,0), breaks = seq(0,60,10)) +
  scale_y_continuous (limits = c(-1,60), expand = c(0,0), breaks = seq(0,60,10)) + 
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=0.2)+ 
  geom_vline(xintercept=0, linetype="dashed", color = "red", size=0.2) +
  xlab("RH 80 (m)") + ylab("RH 80 (m)")+
  geom_text(x=Inf, y=-Inf, hjust=1, vjust=-.25, 
            label=parse(text=paste(R,":", substr(error_summary2[180,3],1,4))))
d90<-get_density(L2A_compare1$rh_90, L2A_compare1$rhGauss.90, n=50)
P90<-ggplot(L2A_compare1) + geom_point(aes(x=rh_90, y=rhGauss.90, color=d90)) + 
  geom_abline(intercept = 0, slope = 1, size=1) + scale_color_viridis() +
  theme_bw() + theme(legend.position = "none")  + 
  scale_x_continuous(limits = c(-1,60), expand = c(0,0), breaks = seq(0,60,10)) +
  scale_y_continuous (limits = c(-1,60), expand = c(0,0), breaks = seq(0,60,10)) + 
  geom_hline(yintercept=0.1, linetype="dashed", color = "red", size=0.2)+ 
  geom_vline(xintercept=0.1, linetype="dashed", color = "red", size=0.2) +
  xlab("RH 90 (m)") + ylab("RH 90 (m)")+
  geom_text(x=Inf, y=-Inf, hjust=1, vjust=-.25, 
            label=parse(text=paste(R,":", substr(error_summary2[190,3],1,4))))
d98<-get_density(L2A_compare1$rh_98, L2A_compare1$rhGauss.98, n=50)
P98<-ggplot(L2A_compare1) + geom_point(aes(x=rh_98, y=rhGauss.98, color=d98)) + 
  geom_abline(intercept = 0, slope = 1, size=1) + scale_color_viridis() + 
  theme_bw() + theme(legend.position = "none") + 
  scale_x_continuous(limits = c(0,60), expand = c(0,0), breaks = seq(0,60,10)) +
  scale_y_continuous (limits = c(0,60), expand = c(0,0), breaks = seq(0,60,10))+ 
  geom_hline(yintercept=0.1, linetype="dashed", color = "red", size=0.2)+ 
  geom_vline(xintercept=0.1, linetype="dashed", color = "red", size=0.2) +
  xlab("RH 98 (m)") + ylab("RH 98 (m)")+
  geom_text(x=Inf, y=-Inf, hjust=1, vjust=-.25, 
            label=parse(text=paste(R,":", substr(error_summary2[198,3],1,4))))
d100<-get_density(L2A_compare1$r_100, L2A_compare1$rhGauss.100, n=50)
P100<-ggplot(L2A_compare1) + geom_point(aes(x=r_100, y=rhGauss.100, color=d100)) + 
  geom_abline(intercept = 0, slope = 1, size=1) + scale_color_viridis() + 
  theme_bw() + theme(legend.position = "none") +
  scale_x_continuous(limits = c(0,60), expand = c(0,0), breaks = seq(0,60,10)) +
  scale_y_continuous (limits = c(0,60), expand = c(0,0), breaks = seq(0,60,10))+
  geom_hline(yintercept=0.1, linetype="dashed", color = "red", size=0.2)+ 
  geom_vline(xintercept=0.1, linetype="dashed", color = "red", size=0.2) +
  xlab("RH 100 (m)") + ylab("RH 100 (m)")+
  geom_text(x=Inf, y=-Inf, hjust=1, vjust=-.25, 
            label=parse(text=paste(R,":", substr(error_summary2[200,3],1,4))))

rh2by5<-ggarrange(P100, P98, P90, P80, 
                  P70, P60, P50, P40, 
                  P30, P20, P10, P0,
                  ncol = 2, nrow = 6)
annotate_figure(rh2by5,
                top = text_grob("GEDI RH Values", size = 18), 
                left = text_grob(bquote('GEDI'['sim']*' RH Values'), size = 18, rot = 90))

tiff(file="/media/aly/Bridger/Thesis/Figures/Final/SRS/Figure4.tiff", units="in", width=4.5, height=13.2, res=300)
annotate_figure(rh2by5,
                top = text_grob("GEDI RH Values", size = 18), 
                left = text_grob(bquote('GEDI'['sim']*' RH Values'), size = 18, rot = 90))
dev.off()
png(file="/media/aly/Bridger/Thesis/Figures/Final/SRS/Figure4.png", units="in", width=4.5, height=13.2, res=300)
annotate_figure(rh2by5,
                top = text_grob("GEDI RH Values", size = 18), 
                left = text_grob(bquote('GEDI'['sim']*' RH Values'), size = 18, rot = 90))
dev.off()


derrived<-L2A_compare1[,c("rh_sum98","rhGauss_sum98","CR_sim","CR_gedi","CR50_sim","CR50_gedi","R98_50_gedi","R98_50_sim")]

rh_sum<-ggplot(derrived) + geom_point(aes(x=rh_sum98, y=rhGauss_sum98, col=data, alpha=0.95), size=1.5) + 
  theme_pubr() + theme(legend.position = "none") +
  scale_x_continuous(limits = c(0,4200), expand = c(0,0), breaks = seq(0,4000,1000)) +
  scale_y_continuous (limits = c(0,4000), expand = c(0,0), breaks = seq(0,4000,1000)) +
  scale_colour_brewer(palette = "Paired", labels = c("Geolocation Corrected", "No Correction"), direction = -1) +
  #scale_color_manual(values = c("#2ca02c", "#892560"), labels = c("Geolocation Corrected", "No Correction")) + 
  labs(color = "") + scale_alpha(guide = 'none') +
  xlab(bquote('RH'['sum'])) + ylab(bquote('RH'['sum']))


cr<-ggplot(derrived) + geom_point(aes(x=CR_gedi, y=CR_sim, col=data, alpha=0.95), size=1.5) + 
  theme_pubr() + theme(legend.position = "none") +
  scale_x_continuous(limits = c(0.25,1.01), expand = c(0,0), breaks = seq(0,1,0.5)) +
  scale_y_continuous (limits = c(0.25,1.01), expand = c(0,0), breaks = seq(0,1,0.5)) +
  scale_colour_brewer(palette = "Paired", labels = c("Geolocation Corrected", "No Correction"), direction = -1) +
  #  scale_color_manual(values = c("#2ca02c", "#892560"), labels = c("Geolocation Corrected", "No Correction")) +
  labs(color = "") + scale_alpha(guide = 'none') +
  xlab("Canopy Ratio") + ylab("Canopy Ratio")


r<-ggplot(derrived) + geom_point(aes(x=R98_50_gedi, y=R98_50_sim, col=data, alpha=0.95), size=1.5) + 
  theme_pubr() + theme(legend.position = "none") +
  scale_x_continuous(limits = c(0,105), expand = c(0,0), breaks = seq(0,100,25)) +
  scale_y_continuous (limits = c(0,100), expand = c(0,0), breaks = seq(0,100,25)) +
  scale_colour_brewer(palette = "Paired", labels = c("Geolocation Corrected", "No Correction"), direction = -1)+
  #  scale_color_manual(values = c("#2ca02c", "#892560"), labels = c("Geolocation Corrected", "No Correction")) +
  labs(color = "") + scale_alpha(guide = 'none') +
  xlab("RH98:RH50") + ylab('RH98:RH50')

compound_error<-ggarrange(rh_sum, r,cr, 
                          nrow = 1,
                          ncol = 3, common.legend = TRUE)

annotate_figure(compound_error,
                bottom = text_grob("GEDI", size = 18), 
                left = text_grob(bquote('GEDI'['sim']), size = 18, rot = 90))

tiff(file="../Figures/Final/SRS/Figure8.tiff", units="in", width=10, height=4, res=300)
annotate_figure(compound_error,
                bottom = text_grob("GEDI", size = 18), 
                left = text_grob(bquote('GEDI'['sim']), size = 18, rot = 90))
dev.off()


#Calculate values to MS
cor(L2A_compare1$rh_50error, L2A_compare1$rh_98error, method = "pearson")

#### Derrived Metrics ####
L2A_compare_trunk<-L2A_compare1
L2A_compare_trunk[,c(173:273)][L2A_compare_trunk[,c(173:273)] < 0] <-0
L2A_compare_trunk[,c(16:116)][L2A_compare_trunk[,c(16:116)] < 0] <-0
#write.csv(L2A_compare_trunk, "/media/aly/Bridger/Thesis/Spatial/ALS_Fire/L2A_collocated_trunk.csv")


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
          "/media/aly/Bridger/Thesis/Spatial/LiDAR/Derrived_GEO_v4.csv")



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

write.csv(error_summary2, file = "/media/aly/Bridger/Thesis/Spatial/LiDAR/Error_Summary_GEO_v6.csv")

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

d10<-get_density(L2A_compare1$rh_10, L2A_compare1$X24.rhGaus, n=50)
P10<-ggplot(L2A_compare1) + geom_point(aes(x=rh_10, y=X24.rhGaus, color=d10)) + 
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
