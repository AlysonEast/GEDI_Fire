library(sp)
library(raster)
library(rgdal)
library(lidR)
library(ggplot2)
library(hdf5r)
library(devtools)
library(rGEDI)
library(ggpubr)
library(viridis)
library(effects)
library(matrixStats)
.rs.unloadPackage("tidyr")

#### Readin gin the data ####
#Read in GEDI sim data from file directory
file_list <- list.files(path="../Spatial/LiDAR/RH/", full.names=FALSE)
path<-"../Spatial/LiDAR/RH/"

for (i in 1:1){
  RH_sim <- read.csv(paste0(path,file_list[i]))
  print(file_list[i])
}
for (i in 2:length(file_list)){
  temp_data <- read.csv(paste0(path,file_list[i]))
  RH_sim <- rbind(RH_sim, temp_data)
  print(file_list[i])
}

#Read in L2A data from shapfile
L2A<-readOGR(dsn="../Spatial/LiDAR", layer = "L2A_ALS_overlap_All_wFID")
#bring in Land cover rasters
LC18_1<-raster("../Spatial/Rasters/MAPBIOMAS/LandCover/2018/mapbiomas-amazon-collection-30-matogrossobrasil-2018.tif")
LC18_2<-raster("../Spatial/Rasters/MAPBIOMAS/LandCover/2018/mapbiomas-amazon-collection-30-para-2018.tif")
LC18_3<-raster("../Spatial/Rasters/MAPBIOMAS/LandCover/2018/mapbiomas-amazon-collection-30-acrebrasil-2018.tif")
LC20_1<-raster("../Spatial/Rasters/MAPBIOMAS/LandCover/2020/mapbiomas-amazon-collection-30-matogrossobrasil-2020.tif")
LC20_2<-raster("../Spatial/Rasters/MAPBIOMAS/LandCover/2020/mapbiomas-amazon-collection-30-para2020.tif")
LC20_3<-raster("../Spatial/Rasters/MAPBIOMAS/LandCover/2020/mapbiomas-amazon-collection-30-acrebrasil-2020.tif")
Slope_1<-raster("../Spatial/Rasters/Terrain/slope-0000000000-0000032768-002.tif")
Slope_2<-raster("../Spatial/Rasters/Terrain/slope-0000000000-0000000000-003.tif")
Slope_3<-raster("../Spatial/Rasters/Terrain/slope-0000032768-0000032768.tif")
landforms<-raster("../Spatial/Rasters/Terrain/landforms.tif")


c(colnames(L2A@data),"LC18_1")
L2A_add<-extract(LC18_1, L2A, sp=TRUE)
L2A_add<-extract(LC18_2, L2A_add, sp=TRUE)
L2A_add<-extract(LC18_3, L2A_add, sp=TRUE)
L2A_add<-extract(LC20_1, L2A_add, sp=TRUE)
L2A_add<-extract(LC20_2, L2A_add, sp=TRUE)
L2A_add<-extract(LC20_3, L2A_add, sp=TRUE)
L2A_add<-extract(Slope_1, L2A_add, sp=TRUE)
L2A_add<-extract(Slope_2, L2A_add, sp=TRUE)
L2A_add<-extract(Slope_3, L2A_add, sp=TRUE)
L2A_add<-extract(landforms, L2A_add, sp=TRUE)
colnames(L2A_add@data)<-c(colnames(L2A@data),"LC18_1","LC18_2","LC18_3","LC20_1","LC20_2","LC20_3",
                          "Slope_1","Slope_2","Slope_3","landforms")

L2A_add@data[,c(159:161)][is.na(L2A_add@data[,c(159:161)])]<-0
L2A@data$LC18<-rowSums((L2A_add@data[,c(159:161)]))
table(L2A@data$LC18)
L2A_add@data[,c(162:164)][is.na(L2A_add@data[,c(162:164)])]<-0
L2A@data$LC20<-rowSums((L2A_add@data[,c(162:164)]))
table(L2A@data$LC20)
L2A_add@data[,c(165:167)][is.na(L2A_add@data[,c(165:167)])]<-0
L2A@data$slope<-rowSums((L2A_add@data[,c(165:167)]))
hist(L2A@data$slope)

M_CCI_18<-raster("../Spatial/Rasters/CCI/BurnCat_18_MODIS_CCI.tif")
MB_18<-raster("../Spatial/Rasters/MAPBIOMAS/Fire/2018/Mapbio18-0000065536-0000065536.tif")

L2A_add<-extract(M_CCI_18, L2A_add, sp=TRUE)
L2A_add<-extract(MB_18, L2A_add, sp=TRUE)

L2A@data$Burn_18<-rowMaxs(as.matrix(L2A_add@data[,c(169,170)]))
table(L2A@data$Burn_18)
L2A@data$landforms<-L2A_add@data$landforms


L2A_df<-L2A@data
L2A_df$LC_d18_19<-L2A_df$LC19-L2A_df$LC18
L2A_df$LC_d18_20<-L2A_df$LC20-L2A_df$LC18

#Merge L2a and GEDI sim data
L2A_compare<-merge(L2A_df, RH_sim, by.x="FID", by.y="wave.ID")

#write a shapefile with just paired points
#L2A_spdf<-merge(L2A, RH_sim, by.x="FID", by.y="wave.ID", all.x=FALSE)
L2A_spdf <- L2A[L2A$FID %in% L2A_compare$FID, ]
#rgdal::writeOGR(L2A_spdf, dsn="../Spatial/LiDAR", layer = "ALS_Compare_v3", driver = "ESRI Shapefile")

#Summarize land use and changed in paired data####
hist(as.numeric(L2A_compare$Dy_cd))

hist(L2A_compare$CCI19_D)
table(L2A_compare$CCI19_D)
table(L2A_compare$CCI19_B)

table(L2A_compare$MAPBI)

table(L2A_compare$M19_D)
table(L2A_compare$M19_B)

L2A_compare$B19<-L2A_compare$M19_D+L2A_compare$MAPBI+L2A_compare$CCI19_D
table(L2A_compare$B19)
L2A_compare[,c("B19")][L2A_compare[,c("B19")] > 0] <-1
hist(L2A_compare$B19)


table(subset(L2A_compare, M20_B=="Post-Burn")$M20_D)

table(L2A_compare$M20_D)
table(L2A_compare$M20_B)
table(subset(L2A_compare, M20_B=="Post-Burn")$M20_D) #2020 burns not a problem due to sample date

table(L2A_compare$Yr_Ls)
hist(L2A_compare$Yr_Ls)
table(subset(L2A_compare, CCI19_D>0)$Yr_Ls)

table(L2A_compare$LC19)
table(L2A_compare$LC18)
table(L2A_compare$LC_d18_19)
L2A_compare[,c("LC_d18_19")][L2A_compare[,c("LC_d18_19")] > 0] <-1
L2A_compare[,c("LC_d18_19")][L2A_compare[,c("LC_d18_19")] < 0] <-1
table(L2A_compare$LC_d18_20)
L2A_compare[,c("LC_d18_20")][L2A_compare[,c("LC_d18_20")] > 0] <-1
L2A_compare[,c("LC_d18_20")][L2A_compare[,c("LC_d18_20")] < 0] <-1


L2A_compare_burn<-subset(L2A_compare, CCI19_D>0 | MAPBI>0 | M19_D>0) #306 points that burned between ALS and GEDI!!
table(L2A_compare_burn$LC19)

#### Omitting data based on temporal change####
L2A_compare1<-subset(L2A_compare, CCI19_D==0 & MAPBI==0 & M19_D==0 & Yr_Ls<18 & LC_d18_19==0)
table(L2A_compare1$Yr_Ls)
table(L2A_compare1$LC19)
table(L2A_compare1$LC_d18_20)
table(subset(L2A_compare1, LC_d18_20!=0)$year)
dim(L2A_compare1)
L2A_compare1<-subset(L2A_compare1, year==2019 | year==2020 & LC_d18_20==0)
dim(L2A_compare1)
table(L2A_compare1$Burn_18)
L2A_compare1<-subset(L2A_compare1, LC18==3 | LC18==14 | LC18==12)

burn19<-ggplot(L2A_compare, aes(x=B19)) + geom_histogram(binwidth = .5, fill="#993333", col="black", alpha=.9) +
  xlab("Burned in 2019") + theme_classic()  +
  theme(text = element_text(size = 20))
burn19

Loss<-ggplot(L2A_compare, aes(x=Yr_Ls)) + geom_histogram(binwidth = 1, fill="#993333", col="black", alpha=.9) +
  xlab("Year of Forest Loss") + theme_classic()+
  theme(text = element_text(size = 20))
Loss

LC_Change19<-ggplot(L2A_compare, aes(x=LC_d18_19)) + geom_histogram(binwidth = 0.5, fill="#993333", col="black", alpha=.9) +
  xlab("Landcover Change 2018 - 2019") + theme_classic() +
  theme(text = element_text(size = 20))
LC_Change19

LC_Change20<-ggplot(L2A_compare, aes(x=LC_d18_20)) + geom_histogram(binwidth = 0.5, fill="#993333", col="black", alpha=.9) +
  xlab("Landcover Change 2018 - 2020") + theme_classic()+
  theme(text = element_text(size = 20))
LC_Change20

ggarrange(burn19, Loss, LC_Change19, LC_Change20, 
          ncol=2, nrow=2)


LCsum<-as.data.frame(table(L2A_compare$LC18))
colnames(LCsum)<-c("Landcover","Count")
LCsum$Landcover<-c("Forest","Wetland","Grassland","Agriculture","Water")
LCsum$pct<-LCsum$Count/(length(L2A_compare$FID))*100
LCsum

LC18_i<-ggplot(LCsum, aes(x="",y=Count, fill=Landcover)) + geom_bar(stat = "identity") +
  xlab("Landcover") + theme_void() + coord_polar("y", start=0) + theme(axis.text.x=element_blank()) + 
  scale_fill_manual(values=c("#A8A800", #Agriculture
                             "#267300", #Forest
                             "#FFEBAF",
                             "#73DFFF",
                             "#73B2FF")) + #Wetland?
  theme(text = element_text(size = 20)) + ggtitle("Before Data Exclusion")

LCsum<-as.data.frame(table(L2A_compare1$LC18))
colnames(LCsum)<-c("Landcover","Count")
LCsum$Landcover<-c("Forest","Wetland","Grassland","Agriculture","Water")
LCsum$pct<-LCsum$Count/(length(L2A_compare1$FID))*100
LCsum

LC18_f<-ggplot(LCsum, aes(x="",y=Count, fill=Landcover)) + geom_bar(stat = "identity") +
  xlab("Landcover") + theme_void() + coord_polar("y", start=0) + theme(axis.text.x=element_blank()) + 
  scale_fill_manual(values=c("#A8A800", #Agriculture
                             "#267300", #Forest
                             "#FFEBAF",
                             "#73DFFF",
                             "#73B2FF")) + #Wetland?
  theme(text = element_text(size = 20))  + ggtitle("After Data Exclusion")
LC18

ggarrange(LC18_i, LC18_f, 
          ncol = 2)

lost<-length(L2A_compare$FID)-length(L2A_compare1$FID) 
length(L2A_compare$FID)
lost/length(L2A_compare$FID)*100

L2A_spdf_2 <- L2A[L2A$FID %in% L2A_compare1$FID, ]

#L2A_df, RH_sim, by.x="FID", by.y="wave.ID"
# L2A_spdf_2@data<-merge(L2A_spdf_2@data, RH_sim, by.x="FID", by.y="wave.ID")
# rgdal::writeOGR(L2A_spdf_2, dsn="../Spatial/LiDAR", layer = "ALS_Compare_v3_Rmvd", driver = "ESRI Shapefile")
# write.csv(L2A_compare1, "../Spatial/LiDAR/errors_to_merge.csv")
#### Adding in catigorical and derrived metrics ####

#Make a temp dataset and set all negitive values of rh to 0
L2A_compare2<-L2A_compare1
L2A_compare2[,c(179:279)][L2A_compare2[,c(179:279)] < 0] <-0
L2A_compare2[,c(280:380)][L2A_compare2[,c(280:380)] < 0] <-0
L2A_compare2[,c(381:481)][L2A_compare2[,c(381:481)] < 0] <-0
#Take RH sum
L2A_compare1$rhGauss_sum<-rowSums((L2A_compare2[,c(179:279)]))
L2A_compare1$rhMax_sum<-rowSums((L2A_compare2[,c(280:380)]))
L2A_compare1$rhinfl_sum<-rowSums((L2A_compare2[,c(381:481)]))

plot(L2A_compare1$rh_sm~L2A_compare1$rhGauss_sum)
plot(L2A_compare1$rh_sm~L2A_compare1$rhMax_sum)
plot(L2A_compare1$rh_sm~L2A_compare1$rhinfl_sum)

# Create a beam strenght classifcation
L2A_compare1$BeamType<- ifelse(L2A_compare1$BEAM=="BEAM0101"|
                                L2A_compare1$BEAM=="BEAM0110"|
                                L2A_compare1$BEAM=="BEAM1000"|
                                L2A_compare1$BEAM=="BEAM1011", 
                              print("Full"), print("Coverage")) 
table(L2A_compare1$BeamType)

#Create rh_sum metrics that goe from 0 to 98 instead of 0 to 100
L2A_compare2[,c(16:114)][L2A_compare2[,c(16:114)] < 0] <-0
L2A_compare1$rh_sum98<-rowSums((L2A_compare2[,c(16:114)]))
L2A_compare1$rhGauss_sum98<-rowSums((L2A_compare2[,c(179:277)]))

#Take out beams with known issuse
#L2A_compare1<-subset(L2A_compare1, BEAM!="BEAM0000" &BEAM!="BEAM0001")

#make hour a numeric value
L2A_compare1$hour<-as.numeric(L2A_compare1$hour)
L2A_compare1$time<-as.numeric(paste0(as.numeric(L2A_compare1$hour)+(as.numeric(L2A_compare1$min)/60)))
L2A_compare1$CC_snstv_diff<-L2A_compare1$snstv-L2A_compare1$cover
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
L2A_error<-(L2A_compare1[,c(16:116)] - L2A_compare1[,c(179:279)])

RMSE<-sqrt(mean((L2A_compare1[,c(179)] - L2A_compare1[,c(16)])^2))
rss <- sum((L2A_compare1[,c((279))]- L2A_compare1[,c((116))])^ 2)  ## residual sum of squares
tss <- sum((L2A_compare1[,c((116))] - mean(L2A_compare1[,c((116))])) ^ 2)  ## total sum of squares
1 - rss/tss

error_summary<-as.data.frame(c(1:98))
colnames(error_summary)<-c("rh")
error_summary$RMSE<-0
error_summary$rsq<-0
for (i in 1:98) {
  error_summary[i,2]<-sqrt(mean((L2A_compare1[,c((178+i))] - L2A_compare1[,c((15+i))])^2))
  rss <- sum((L2A_compare1[,c((178+i))]- L2A_compare1[,c((15+i))])^ 2)  ## residual sum of squares
  tss <- sum((L2A_compare1[,c((15+i))] - mean(L2A_compare1[,c((15+i))])) ^ 2)  ## total sum of squares
  error_summary[i,3] <- 1 - rss/tss
}
par(mfrow=c(2,1))
plot(rh~RMSE, data=error_summary)
plot(rh~rsq, data=error_summary)


L2A_compare1$rh_50error<-(L2A_compare1$rh_50 - L2A_compare1$rhGauss.50)
L2A_compare1$rh_75error<-(L2A_compare1$rh_75 - L2A_compare1$rhGauss.75)
L2A_compare1$rh_98error<-(L2A_compare1$rh_98 - L2A_compare1$rhGauss.98)
L2A_compare1$rh_sumerror<-(L2A_compare1$rh_sum98 - L2A_compare1$rhGauss_sum98)

par(mfrow=c(2,2))
hist(L2A_compare1$rh_50error)
hist(L2A_compare1$rh_75error)
hist(L2A_compare1$rh_98error)
hist(L2A_compare1$rh_sumerror)

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


table(L2A_compare1$high_error)

L2A_compare1$Forest<-ifelse(L2A_compare1$LC18=="3", print(1),print(0))
L2A_compare1$Forest<-as.factor(L2A_compare1$Forest)
L2A_compare1$slct_<-as.factor(L2A_compare1$slct_)

#Removing by Quality flag ####
L2A_compareWflags<-L2A_compare1
L2A_compare1<-subset(L2A_compare1, dgrd_==0)
L2A_compare1<-subset(L2A_compare1, elv__==0)

error_summary2<-as.data.frame(c(1:98))
colnames(error_summary2)<-c("rh")
error_summary2$RMSE<-0
error_summary2$rsq<-0
for (i in 1:98) {
  error_summary2[i,2]<-sqrt(mean((L2A_compare1[,c((178+i))] - L2A_compare1[,c((15+i))])^2))
  rss <- sum((L2A_compare1[,c((178+i))]- L2A_compare1[,c((15+i))])^ 2)  ## residual sum of squares
  tss <- sum((L2A_compare1[,c((15+i))] - mean(L2A_compare1[,c((15+i))])) ^ 2)  ## total sum of squares
  error_summary2[i,3] <- 1 - rss/tss
}

#plotting the change in error after quality filtering
par(mfrow=c(2,2))
plot(rh~RMSE, data=error_summary, xlim = c(3,6.5))
plot(rh~rsq, data=error_summary, xlim = c(0,1))
plot(rh~RMSE, data=error_summary2, xlim = c(3,6.5))
plot(rh~rsq, data=error_summary2, xlim = c(0,1))

par(mfrow=c(1,2))
plot(rh~RMSE, data=error_summary, xlim = c(3,6.5))
points(rh~RMSE, data=error_summary2, col="red", xlim = c(3,6.5))
plot(rh~rsq, data=error_summary, xlim = c(0,1))
points(rh~rsq, data=error_summary2, xlim = c(0,1), col="red")

#### Data subsetting according to literature standard
#night time
L2A_compare1$night<-ifelse(L2A_compare1$slr_l<0, print(1), print(0))

#full strength at night (Liu et al 2021)
L2A_compare1$Liu<-ifelse(L2A_compare1$slr_l<0 & L2A_compare1$BeamType=="Full", print(1), print(0))

#remove cover beams for coverage over 95
L2A_compare1$HighCC<-ifelse(L2A_compare1$cover<0.95, print(1),
                         ifelse(L2A_compare1$cover>0.95 & L2A_compare1$BeamType=="Full", print(1), print(0)))

#Sensitivity Canopy Cover difference
L2A_compare1$cc_snstv<-ifelse(L2A_compare1$CC_snstv_diff<0, print(0), print(1))

#Slope under 30degrees
L2A_compare1$Slope30<-ifelse(L2A_compare1$slope<30, print(1), print(0))

#sensitivities over .95
L2A_compare1$snstv95<-ifelse(L2A_compare1$snstv>0.95, print(1), print(0))


#Statistically different values?####



#### Calc different values ####
cor(L2A_compare1$rh_50error, L2A_compare1$rh_98error, method = "pearson")
L2A_compare1$rh_25error<-(L2A_compare1$rh_25 - L2A_compare1$rhGauss.25)
cor(L2A_compare1$rh_25error, L2A_compare1$rh_98error, method = "pearson")
cor(L2A_compare1$rh_50error, L2A_compare1$rh_25error, method = "pearson")

cor()

hist(L2A_compareWflags$slope)
hist(L2A_compare1$slope)
hist(subset(L2A_compareWflags, dgrd_==1 | elv__==1)$slope)

#### Summarize differences under different filters####

differences<-as.data.frame(rep(c("All Data", "Exclude Flags*", "Truncated",
                             "Exclude Daytime Samples",
                             "Exclude Daytime & Coverage Beam Samples",
                             "Sensitivity > 0.95",
                             "Exclude Coverage Beams for Canopy Cover > 95%",
                             "Slope < 30 Degrees",
                             "Exclude Sensitivity < Canopy Cover"), each=2))
colnames(differences)<-c("Filters")
differences$rh<-rep(c(50, 98),9)
differences$Mean<-0
differences$StandardDeviation<-0
differences$RMSE<-0
differences$pct<-0
differences$n<-0
differences$Bias

#All Data
differences[1,3]<-mean(L2A_compareWflags$rh_50error)
differences[1,4]<-sd(L2A_compareWflags$rh_50error)
differences[1,5]<-sqrt(mean((L2A_compareWflags[,c(229)] - L2A_compareWflags[,c(66)])^2))
differences[2,3]<-mean(L2A_compareWflags$rh_98error)
differences[2,4]<-sd(L2A_compareWflags$rh_98error)
differences[2,5]<-sqrt(mean((L2A_compareWflags[,c(277)] - L2A_compareWflags[,c(114)])^2))
differences[1,6]<-(nrow(L2A_compareWflags)/nrow(L2A_compareWflags))
differences[1,7]<-nrow(L2A_compareWflags)
differences[1,8]<-(sum(L2A_compareWflags[,c((229))] - L2A_compareWflags[,c((66))])/nrow(L2A_compareWflags))
differences[2,8]<-(sum(L2A_compareWflags[,c((277))] - L2A_compareWflags[,c((114))])/nrow(L2A_compareWflags))
  
#Flags removed
differences[3,3]<-mean(L2A_compare1$rh_50error)
differences[3,4]<-sd(L2A_compare1$rh_50error)
differences[3,5]<-sqrt(mean((L2A_compare1[,c(229)] - L2A_compare1[,c(66)])^2))
differences[4,3]<-mean(L2A_compare1$rh_98error)
differences[4,4]<-sd(L2A_compare1$rh_98error)
differences[4,5]<-sqrt(mean((L2A_compare1[,c(277)] - L2A_compare1[,c(114)])^2))
differences[3,6]<-(nrow(L2A_compare1)/nrow(L2A_compare1))
differences[3,7]<-nrow(L2A_compare1)
differences[3,8]<-(sum(L2A_compare1[,c((229))] - L2A_compare1[,c((66))])/nrow(L2A_compare1))
differences[4,8]<-(sum(L2A_compare1[,c((277))] - L2A_compare1[,c((114))])/nrow(L2A_compare1))

#Truncated
L2A_compare_trunk<-L2A_compare1
L2A_compare_trunk[,c(179:279)][L2A_compare_trunk[,c(179:279)] < 0] <-0
L2A_compare_trunk[,c(16:116)][L2A_compare_trunk[,c(16:116)] < 0] <-0

differences[5,3]<-mean(L2A_compare_trunk$rh_50error)
differences[5,4]<-sd(L2A_compare_trunk$rh_50error)
differences[5,5]<-sqrt(mean((L2A_compare_trunk[,c(229)] - L2A_compare_trunk[,c(66)])^2))
differences[6,3]<-mean(L2A_compare_trunk$rh_98error)
differences[6,4]<-sd(L2A_compare_trunk$rh_98error)
differences[6,5]<-sqrt(mean((L2A_compare_trunk[,c(277)] - L2A_compare_trunk[,c(114)])^2))
differences[5,6]<-(nrow(L2A_compare_trunk)/nrow(L2A_compare_trunk))
differences[5,7]<-nrow(L2A_compare1)
differences[5,8]<-(sum(L2A_compare_trunk[,c((229))] - L2A_compare_trunk[,c((66))])/nrow(L2A_compare_trunk))
differences[6,8]<-(sum(L2A_compare_trunk[,c((277))] - L2A_compare_trunk[,c((114))])/nrow(L2A_compare_trunk))


for (i in 4:length(list)) {
  temp_data<-subset(L2A_compare1, L2A_compare1[,c(list[i])]==1)
  differences[(2*i),3]<-mean(temp_data$rh_98error)
  differences[(2*i),4]<-sd(temp_data$rh_98error)
  differences[(2*i),5]<-sqrt(mean((temp_data[,c(277)] - temp_data[,c(114)])^2))
  differences[(2*i-1),3]<-mean(temp_data$rh_50error)
  differences[(2*i-1),4]<-sd(temp_data$rh_50error)
  differences[(2*i-1),5]<-sqrt(mean((temp_data[,c(229)] - temp_data[,c(66)])^2))
  differences[(2*i-1),6]<-1-((nrow(temp_data)/nrow(L2A_compare1)))
  differences[(2*i-1),7]<-nrow(temp_data)
  differences[(2*i-1),8]<-(sum(temp_data[,c((229))] - temp_data[,c((66))])/nrow(temp_data))
  differences[(2*i),8]<-(sum(temp_data[,c((277))] - temp_data[,c((114))])/nrow(temp_data))
}

#differences$range98<-paste0(substr(differences$Mean98-2*differences$StandardDeviation98, 1,6)," : ",substr(differences$Mean98+2*differences$StandardDeviation98, 1, 4))
#differences$range50<-paste0(substr(differences$Mean50-2*differences$StandardDeviation50, 1,6)," : ",substr(differences$Mean50+2*differences$StandardDeviation50, 1, 4))

differences$range<-paste0(substr(differences$Mean-2*differences$StandardDeviation, 1,6)," : ",substr(differences$Mean+2*differences$StandardDeviation, 1, 4))
View(differences)

# Creating a tidy plot of differences
library(gt)
library(tidyverse)
library(glue)
# differences[,c("Filters","rh","Mean","range","n","pct")] %>%
#   gt() %>%
#   tab_header(
#     title = "Error Associated with data exclusion" #,
# #    subtitle = glue("{start_date} to {end_date}")
#   ) %>%
#   fmt_markdown(
#     columns = c(Filters)
#   ) %>%
#   tab_spanner( 
#     label = "Mean", 
#     columns = c(50, 98)
#   )
#   fmt_percent(
#     columns = pct
#   )

# Ridgeline plost of error
L2A_error<-(L2A_compare1[,c(16:116)] - L2A_compare1[,c(179:279)])
L2A_error$FID<-L2A_compare1$FID
library(reshape)
L2A_error_melt<-melt(L2A_error, id="FID")
L2A_error_melt$data<-"Flags Removed"
#L2A_error_melt$variable<-as.character(L2A_error_melt$variable)
#L2A_error_melt$variable[L2A_error_melt$variable=="r_100"]<-"rh_100"
#L2A_error_melt$rh<-substr(L2A_error_melt$variable, 4, nchar(L2A_error_melt$variable))
#L2A_error_melt$rh<-as.numeric(L2A_error_melt$rh)


L2A_errorflag<-(L2A_compareWflags[,c(16:116)] - L2A_compareWflags[,c(179:279)])
L2A_errorflag$FID<-L2A_compareWflags$FID
L2A_errorflag_melt<-melt(L2A_errorflag, id="FID")
L2A_errorflag_melt$data<-"All Data"
L2A_error_melt<-rbind(L2A_error_melt, L2A_errorflag_melt)


L2A_error_melt_5<-subset(L2A_error_melt, variable=="rh_0" | variable=="rh_5" | variable=="rh_10" |
                           variable=="rh_15" | variable=="rh_20" | variable=="rh_25" | variable=="rh_30" |
                           variable=="rh_35" | variable=="rh_40" | variable=="rh_45" | variable=="rh_50" |
                           variable=="rh_55" | variable=="rh_60" | variable=="rh_65" | variable=="rh_70" |
                           variable=="rh_75" | variable=="rh_80" | variable=="rh_85" | variable=="rh_90" |
                           variable=="rh_95" | variable=="rh_98")

library(ggridges)
ggplot(L2A_error_melt_5, aes(y=variable, x=value, color=data)) +
  stat_density_ridges(quantile_lines = TRUE, quantiles = c(0.05, 0.95), fill=NA, size=1) + theme_pubr()
  # geom_density_ridges(fill=NA, 
  #                     quantile_lines = TRUE) + theme_bw()
  


# Plots of R-squared and RMSE through Canopy ####
error_summary2<-as.data.frame(rep(c(1:100),9))
colnames(error_summary2)<-c("rh")
error_summary2$RMSE<-0
error_summary2$rsq<-0
error_summary2$RMSEpct<-0
error_summary2$Bias<-0
error_summary2$MAE<-0
error_summary2$MAEpct<-0

for (i in 1:100) {
  error_summary2[i,2]<-sqrt(mean((L2A_compareWflags[,c((179+i))] - L2A_compareWflags[,c((16+i))])^2))
  rss <- sum((L2A_compareWflags[,c((16+i))]- L2A_compareWflags[,c((179+i))])^ 2)  ## residual sum of squares
  tss <- sum((L2A_compareWflags[,c((179+i))] - mean(L2A_compareWflags[,c((179+i))])) ^ 2)  ## total sum of squares
  error_summary2[i,3] <- 1 - rss/tss
  error_summary2[i,4] <- ((error_summary2[i,2]/mean(L2A_compareWflags[,c((179+i))]))*100)
  error_summary2[i,5]<-(sum(L2A_compareWflags[,c((16+i))] - L2A_compareWflags[,c((179+i))])/nrow(L2A_compareWflags))
  error_summary2[i,6]<-(sum(abs(L2A_compareWflags[,c((179+i))] - L2A_compareWflags[,c((16+i))])))/nrow(L2A_compareWflags)
  error_summary2[i,7] <- ((error_summary2[i,6]/mean(L2A_compareWflags[,c((179+i))]))*100)
}

for (i in 1:100) {
  error_summary2[((100+i)),2]<-sqrt(mean((L2A_compare1[,c((179+i))] - L2A_compare1[,c((16+i))])^2))
  rss <- sum((L2A_compare1[,c((16+i))]- L2A_compare1[,c((179+i))])^ 2)  ## residual sum of squares
  tss <- sum((L2A_compare1[,c((179+i))] - mean(L2A_compare1[,c((179+i))])) ^ 2)  ## total sum of squares
  error_summary2[((100+i)),3] <- 1 - rss/tss
  error_summary2[((100+i)),4] <- ((error_summary2[((100+i)),2]/mean(L2A_compare1[,c((179+i))]))*100)
  error_summary2[((100+i)),5]<-(sum(L2A_compare1[,c((16+i))]-L2A_compare1[,c((179+i))])/nrow(L2A_compare1))
  error_summary2[((100+i)),6]<-(sum(abs(L2A_compare1[,c((179+i))] - L2A_compare1[,c((16+i))])))/nrow(L2A_compare1)
  error_summary2[((100+i)),7] <- ((error_summary2[((100+i)),6]/mean(L2A_compare1[,c((179+i))]))*100)
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

for (i in 1:100) {
  error_summary2[(200+i),2]<-sqrt(mean((L2A_compare_trunk[,c((179+i))] - L2A_compare_trunk[,c((16+i))])^2))
  rss <- sum((L2A_compare_trunk[,c((16+i))]- L2A_compare_trunk[,c((179+i))])^ 2)  ## residual sum of squares
  tss <- sum((L2A_compare_trunk[,c((179+i))] - mean(L2A_compare_trunk[,c((179+i))])) ^ 2)  ## total sum of squares
  error_summary2[(200+i),3] <- 1 - rss/tss
  error_summary2[((200+i)),4] <- ((error_summary2[((200+i)),2]/mean(L2A_compare_trunk[,c((179+i))]))*100)
  error_summary2[((200+i)),5]<-(sum(L2A_compare_trunk[,c((16+i))]-L2A_compare_trunk[,c((179+i))])/nrow(L2A_compare_trunk))
  error_summary2[((200+i)),6]<-(sum(abs(L2A_compare_trunk[,c((179+i))] - L2A_compare_trunk[,c((16+i))])))/nrow(L2A_compare_trunk)
  error_summary2[((200+i)),7] <- ((error_summary2[((200+i)),6]/mean(L2A_compare_trunk[,c((179+i))]))*100)
}

list<-c("flag","No Flags","night","Liu","snstv95","HighCC","Slope30","cc_snstv")
for (j in 3:length(list)) {
  for (i in 1:100) {
    temp_data<-subset(L2A_compare1, L2A_compare1[,c(list[j])]==1)
    error_summary2[((j*100)+i),2]<-sqrt(mean((temp_data[,c((179+i))] - temp_data[,c((16+i))])^2))
    rss <- sum((temp_data[,c((16+i))]- temp_data[,c((179+i))])^ 2)  ## residual sum of squares
    tss <- sum((temp_data[,c((179+i))] - mean(temp_data[,c((179+i))])) ^ 2)  ## total sum of squares
    error_summary2[((j*100)+i),3] <- 1 - rss/tss
    error_summary2[((j*100)+i),4] <- ((error_summary2[((j*100)+i),2]/mean(temp_data[,c((179+i))]))*100)
    error_summary2[((j*100)+i),5]<-(sum(temp_data[,c((16+i))]-temp_data[,c((179+i))])/nrow(temp_data))
    error_summary2[((j*100)+i),6]<-(sum(abs(temp_data[,c((179+i))] - temp_data[,c((16+i))])))/nrow(temp_data)
    error_summary2[((j*100)+i),7] <- ((error_summary2[((j*100)+i),6]/mean(temp_data[,c((179+i))]))*100)
  }
}
library(RColorBrewer)
RMSE_plot<-ggplot(subset(error_summary2, Data_short!="Slopes < 30"), aes(x=rh, y=RMSE, col=Data_short, alpha=0.25)) + 
  scale_x_continuous(limits = c(0,105), expand = c(0,0), breaks = seq(0,100,25)) +
  scale_y_continuous (limits = c(0,7), expand = c(0,0), breaks = seq(0,6,2)) +
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

Rsq_plot<-ggplot(subset(error_summary2, Data_short!="Slopes < 30"), aes(x=rh, y=rsq, col=Data_short, alpha=0.9)) + 
  scale_x_continuous(limits = c(0,105), expand = c(0,0), breaks = seq(0,100,25)) +
  scale_y_continuous (limits = c(0,0.85), expand = c(0,0), breaks = seq(0,0.8,0.2)) +
  geom_point(aes(col=Data_short), size=2) +geom_line(aes(col=Data_short), size=2) + theme_pubr() + scale_colour_brewer(palette = "Paired")+ 
  theme(
    legend.position = "none",
    text = element_text(size = 20)
  ) + guides(alpha = "none") +
  ylab(parse(text=paste(R))) + xlab("Relative Height (RH)") + aes(group=rev(Data_short))
Rsq_plot

MAE_plot<-ggplot(subset(error_summary2, Data_short!="Slopes < 30"), aes(x=rh, y=MAE, col=Data_short, alpha=0.9)) + 
  scale_x_continuous(limits = c(0,105), expand = c(0,0), breaks = seq(0,100,25)) +
  scale_y_continuous (limits = c(0,6.1), expand = c(0,0), breaks = seq(0,6,2)) +
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


ggpubr::ggarrange(Bias_plot, RMSE_plot, MAE_plot,
          Rsq_plot, RMSEpct_plot, MAEpct_plot,
          ncol = 3, 
          nrow = 2, 
          common.legend = FALSE)
#png(file="../Figures/Final/Final/Filtering_error_summary.png", width=12, height=10, units = "in", res=120) #res in ppi 300dpi = 118 ppi
ggpubr::ggarrange(Bias_plot, RMSE_plot, MAE_plot,
                  Rsq_plot, RMSEpct_plot, MAEpct_plot,
                  ncol = 3, 
                  nrow = 2, 
                  common.legend = FALSE)
dev.off()

#tiff(file="../Figures/Final/RSE/Figure6.tiff", units="in", width=12, height=10, res=300)
ggpubr::ggarrange(Bias_plot, RMSE_plot, MAE_plot,
                  Rsq_plot, RMSEpct_plot, MAEpct_plot,
                  ncol = 3, 
                  nrow = 2, 
                  common.legend = FALSE)
dev.off()


#Summary of error values
mean(L2A_compare1$rh_98error)
mean(L2A_compare1$rh_50error)

#PLots with collocated data
collocate_error<-read.csv("../Spatial/LiDAR/Error_Summary_GEO_v6.csv")
dim(collocate_error)
dim(error_summary2)
colnames(collocate_error)
colnames(error_summary2)
collocate_error<-collocate_error[,-1]
collocate_error$Bias<-(collocate_error$Bias*-1)
collocate_error<-rbind(collocate_error, error_summary2[c(101:200),])

collocate_error[,"Data_short"][collocate_error[,"Data_short"]=="Flags Removed*"]<-"No Geolocation Correction"
collocate_error<-collocate_error[-c(1:100),]
  
# collocate_error$Data_short<-as.factor(collocate_error$Data_short)
# levels(collocate_error$Data_short)
# collocate_error$Data_short<-ordered(collocate_error$Data_short, levels=c("No Geolocation Correction",
#                                                                          "Full Geolocation", 
#                                                                          "Geolocation Corrected*",
#                                                                          "RH_50 Error>10 Removed",
#                                                                          "RH_75 Error>10 Removed",
#                                                                          "RH_98 Error>10 Removed"))

collocate_error<-subset(collocate_error, Data_short=="Geolocation Corrected*" | Data_short=="No Geolocation Correction")
collocate_error$Data_short<-as.factor(collocate_error$Data_short)
collocate_error$Data_short<-ordered(collocate_error$Data_short, levels=c("No Geolocation Correction",
                                                                          "Geolocation Corrected*"))


RMSE_plot<-ggplot(collocate_error, aes(x=rh, y=RMSE, col=Data_short, alpha=0.25)) + 
  scale_x_continuous(limits = c(0,105), expand = c(0,0), breaks = seq(0,100,25)) +
  scale_y_continuous (limits = c(2,8.4), expand = c(0,0), breaks = seq(2,8,2)) +
  geom_point(aes(col=Data_short), size=2) +geom_line(aes(col=Data_short), size=2) + theme_pubr() + scale_colour_brewer(palette = "Paired") + 
  theme(
    legend.position = "none",
    text = element_text(size = 20)
  ) + guides(alpha = "none") +
  ylab("RMSE (m)") + xlab("Relative Height (RH)")
RMSE_plot  

RMSEpct_plot<-ggplot(collocate_error, aes(x=rh, y=RMSEpct, col=Data_short, alpha=0.25)) + 
  scale_x_continuous(limits = c(0,105), expand = c(0,0), breaks = seq(0,100,25)) +
  scale_y_continuous (limits = c(15,100), expand = c(0,0), breaks = seq(20,100,20)) +
  geom_point(aes(col=Data_short), size=2) +geom_line(aes(col=Data_short), size=2) + theme_pubr() + scale_colour_brewer(palette = "Paired") + 
  theme(
    legend.position = "none",
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6,),
    legend.title=element_blank(),
    text = element_text(size = 20)
  ) + guides(alpha = "none") +
  ylab("RMSE%") + xlab("Relative Height (RH)")
RMSEpct_plot  

Bias_plot<-ggplot(collocate_error, aes(x=rh, y=Bias, col=Data_short, alpha=0.25)) + 
  scale_x_continuous(limits = c(0,105), expand = c(0,0), breaks = seq(0,100,25)) +
  scale_y_continuous (limits = c(-3.2, 0.05), expand = c(0,0), breaks = seq(-3,0,1)) +
  geom_point(aes(col=Data_short), size=2) +geom_line(aes(col=Data_short), size=2) + theme_pubr() + scale_colour_brewer(palette = "Paired") + 
  theme(
    legend.position = "none",
    text = element_text(size = 20)
  ) + guides(alpha = "none") +
  ylab("Bias (m)") + xlab("Relative Height (RH)")
Bias_plot  

Rsq_plot<-ggplot(collocate_error, aes(x=rh, y=rsq, col=Data_short, alpha=0.9)) + 
  scale_x_continuous(limits = c(0,105), expand = c(0,0), breaks = seq(0,100,25)) +
  scale_y_continuous (limits = c(0,.83), expand = c(0,0), breaks = seq(0,1,0.2)) +
  geom_point(aes(col=Data_short), size=2) +geom_line(aes(col=Data_short), size=2) + theme_pubr() + scale_colour_brewer(palette = "Paired")+ 
  theme(
    legend.position = "none",
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(1, 1, 1, 1,),
    legend.title=element_blank(),
    text = element_text(size = 20)
  ) + guides(alpha = "none") +
  ylab(parse(text=paste(R))) + xlab("Relative Height (RH)")
Rsq_plot

MAE_plot<-ggplot(collocate_error, aes(x=rh, y=MAE, col=Data_short, alpha=0.9)) + 
  scale_x_continuous(limits = c(0,105), expand = c(0,0), breaks = seq(0,100,25)) +
  scale_y_continuous (limits = c(2,8.4), expand = c(0,0), breaks = seq(0,8,2)) +
  geom_point(aes(col=Data_short), size=2) +geom_line(aes(col=Data_short), size=2) + theme_pubr() + scale_colour_brewer(palette = "Paired")+ 
  theme(
    legend.position = c(.93, .98),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(1, 1, 1, 1,),
    legend.title=element_blank(),
    text = element_text(size = 20)
  ) + guides(alpha = "none") +
  ylab("MAE (m)") + xlab("Relative Height (RH)") + aes(group=rev(Data_short))
MAE_plot

MAEpct_plot<-ggplot(collocate_error, aes(x=rh, y=MAEpct, col=Data_short, alpha=0.9)) + 
  scale_x_continuous(limits = c(0,105), expand = c(0,0), breaks = seq(0,100,25)) +
  scale_y_continuous (limits = c(10,100), expand = c(0,0), breaks = seq(0,100,20)) +
  geom_point(aes(col=Data_short), size=2) +geom_line(aes(col=Data_short), size=2) + theme_pubr() + scale_colour_brewer(palette = "Paired")+ 
  theme(
    legend.position = "none",
    text = element_text(size = 20)
  )+ guides(alpha = "none") +
  ylab("MAE %") + xlab("Relative Height (RH)") + aes(group=rev(Data_short))
MAEpct_plot

ggarrange(Rsq_plot, RMSEpct_plot, 
          Bias_plot, RMSE_plot,
          ncol = 2,
          nrow = 2, 
          common.legend = FALSE)

#png(file="../Figures/Final/Final/Geolocation_error_summary.png", width=12, height=10, units = "in", res=120) #res in ppi 300dpi = 118 ppi
tiff(file="../Figures/Final/SRS/Figure7.tiff", units="in", width=12, height=10, res=300)
ggpubr::ggarrange(Bias_plot, RMSE_plot, MAE_plot,
                  Rsq_plot, RMSEpct_plot, MAEpct_plot,
                  ncol = 3, 
                  nrow = 2, 
                  common.legend = FALSE)
dev.off()



#Plot of all bzaseline error on one plot####
Baseline<-subset(error_summary2, Data_short=="Flags Removed*")
Baseline$rsq<-Baseline$rsq*100
Baseline_melt<-melt(Baseline, id=c("rh", "Data", "Data_short"))
Baseline_melt$units<-ifelse(Baseline_melt$variable=="RMSE"|
                              Baseline_melt$variable=="Bias"|
                              Baseline_melt$variable=="MAE", paste("m"),paste("%"))

#png(file="../Figures/Final/Baseline_Error.png", width=6, height=8, units = "in", res=120) #res in ppi 300dpi = 118 ppi
ggpubr::ggarrange(
ggplot(subset(Baseline_melt, units=="m"), aes(x=rh, y=value), fill="#1f78b4") +
  geom_point(aes(shape=variable), size=1.5, col="#1f78b4") +geom_line(aes(linetype=variable),col="#1f78b4") + 
  scale_x_continuous(limits = c(0,105), expand = c(0,0), breaks = seq(0,100,25)) +
  scale_y_continuous (limits = c(-3,8), expand = c(0,0), breaks = seq(-2,8,2))  + 
  labs(shape="", linetype="") +
  scale_shape_manual(labels=c("RMSE","Bias","MAE"), values=c(16,15,17)) +
  theme_pubr(),
ggplot(subset(Baseline_melt, units=="%"), aes(x=rh, y=value), fill="#1f78b4") +
  geom_line(aes(linetype=variable), col="#1f78b4") + 
  geom_point(aes(shape=variable), size=1.5, col="#1f78b4") +
  scale_x_continuous(limits = c(0,105), expand = c(0,0), breaks = seq(0,100,25)) +
  labs(shape="", linetype="") + xlab("Relative Height (rh)") + ylab("Percent") +
  scale_shape_manual(labels=(c("R-Squared","RMSE %","MAE %")), values=c(4,16,17)) +
  scale_linetype_discrete(labels=(c("R-Squared","RMSE %","MAE %"))) +
  scale_y_continuous(limits = c(0,100), expand = c(0,0), breaks = seq(0,100,25),
                     name = "Percent",
                     sec.axis = sec_axis( trans=~./100, name="R-Squared")) +
  theme_pubr(),
nrow = 2
)
dev.off()

#print differences in error values between geolocated and non-geolocated data ####
collocate_error[c(1:100),"rsq"]
collocate_error[c(101:200),"rsq"]
diff<-collocate_error[c(1:100),"rsq"]-collocate_error[c(101:200),"rsq"]
plot(diff, ylim=c(-0.2,0.1))
abline(h=0)
max(diff[12:100])
which.max(diff[12:100])+11
min((diff[12:99]))
which.min(diff[12:99])+11

collocate_error[c(1:100),c("rh","rsq")] #rh 27 for collocated 31
collocate_error[c(201:100),c("rh","rsq")] #rh 43 for uncollocated 42


collocate_error[c(1:100),"RMSE"]
collocate_error[c(101:200),"RMSE"]
diff<-collocate_error[c(1:100),"RMSE"]-collocate_error[c(101:200),"RMSE"]
par(mfrow=c(1,1))
plot(diff)
abline(h=0)
min(diff)
which.min(diff)
max(diff)

collocate_error[c(1:100),"Bias"]
collocate_error[c(401:500),"Bias"]
diff<-collocate_error[c(1:100),"Bias"]-collocate_error[c(401:500),"Bias"]
diff
par(mfrow=c(1,1))
plot(diff)
abline(h=0.5)
min(diff)
which.min(diff)

#print differences in error values between geolocated and rh filtered data
collocate_error[c(1:100),"rsq"]
collocate_error[c(201:300), "rsq"] #98
collocate_error[c(101:200), "rsq"] #75
collocate_error[c(301:400), "rsq"] #50

diff98<-collocate_error[c(201:300), "rsq"]-collocate_error[c(1:100),"rsq"]
diff75<-collocate_error[c(101:200), "rsq"]-collocate_error[c(1:100),"rsq"]
diff50<-collocate_error[c(301:400), "rsq"]-collocate_error[c(1:100),"rsq"]
temp<-as.data.frame(cbind(diff98, diff75, diff50))
diffavg<-rowMeans(temp)
diffavg
plot(diffavg, ylim=c(0,max(diffavg)))
max(diffavg[12:100])
which.max(diffavg[12:100])+11
min(diffavg[12:99])
which.min(diffavg[12:100])+11

plot(diffavg, ylim=c(0,max(diff50)))
points(diff98, add=T, col="red")
points(diff75, add=T, col="blue")
points(diff50, add=T, col="dark green")

diff98<-collocate_error[c(201:300), "RMSE"]-collocate_error[c(1:100),"RMSE"]
diff75<-collocate_error[c(101:200), "RMSE"]-collocate_error[c(1:100),"RMSE"]
diff50<-collocate_error[c(301:400), "RMSE"]-collocate_error[c(1:100),"RMSE"]
temp<-as.data.frame(cbind(diff98, diff75, diff50))
diffmax<-rowMaxs(as.matrix(temp))
diffmax
max(diffmax)
diffmin<-rowMins(as.matrix(temp))
diffmin
min(diffmin)


plot(diff98, col="red")
points(diff75, col="blue")
points(diff50, col="dark green")



# Boxplots of error with rh
L2A_error<-(L2A_compare1[,c(16:116)] - L2A_compare1[,c(179:279)])
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
L2A_error_melt$rh_bin<-c(rep(1, each=nrow(L2A_error)),rep(1:19, each=nrow(L2A_error)*5), rep(20, each=nrow(L2A_error)*4), rep(21, each=nrow(L2A_error)))
L2A_error_melt$rh_bin<-as.factor(L2A_error_melt$rh_bin)
bins<-L2A_error_melt[c(nrow(L2A_error)+1:nrow(L2A_error_melt)),]
bins<-na.omit(L2A_error_melt)
out<-ggplot(bins, aes(x=rh_bin, y=value)) + geom_boxplot(outlier.shape = 1) + theme_pubr() + 
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
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
#png(file="../Figures/Final/rh_error_dist_bins.png", width=5.5, height=4, units = "in", res=120) #res in ppi 300dpi = 118 ppi
tiff(file="../Figures/Final/RSE/Figure5.tiff", units="in", width=12, height=10, res=300)
out
dev.off()

#### plots ####
#one line
RMSE_plot<-ggplot(subset(error_summary2, Data_short=="Flags Removed*"), aes(x=rh, y=RMSE, alpha=0.25), col="#1b63a5") + 
  scale_x_continuous(limits = c(0,105), expand = c(0,0), breaks = seq(0,100,25)) +
  scale_y_continuous (limits = c(0,7), expand = c(0,0), breaks = seq(0,6,2)) +
  geom_point(size=2, col="#1b63a5") +geom_line(size=2, col="#1b63a5") + theme_pubr() +# scale_colour_brewer(palette = "Paired") + 
  theme(
    legend.position = "none",
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(1, 1, 1, 1,),
    legend.title=element_blank(),
    text = element_text(size = 18)
  ) + guides(alpha = "none") +
  ylab("RMSE (m)") + xlab("Relative Height (rh)") + aes(group=rev(Data_short))
RMSE_plot 

RMSEpct_plot<-ggplot(subset(error_summary2, Data_short=="Flags Removed*"), aes(x=rh, y=RMSEpct, col=Data_short, alpha=0.25)) + 
  scale_x_continuous(limits = c(0,105), expand = c(0,0), breaks = seq(0,100,25)) +
  scale_y_continuous (limits = c(18,103), expand = c(0,0), breaks = seq(20,100,20))+
  geom_point(size=2, col="#1b63a5") +geom_line(size=2, col="#1b63a5") + theme_pubr() +
  theme(
    legend.position = "none",
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6,),
    legend.title=element_blank(),
    text = element_text(size = 20)
  ) + guides(alpha = "none") +
  ylab("RMSE%") + xlab("Relative Height (rh)") + aes(group=rev(Data_short))
RMSEpct_plot  

Bias_plot<-ggplot(subset(error_summary2, Data_short=="Flags Removed*"), aes(x=rh, y=Bias, col=Data_short, alpha=0.25)) + 
  scale_x_continuous(limits = c(0,105), expand = c(0,0), breaks = seq(0,100,25)) +
  scale_y_continuous (limits = c(-3,0.2), expand = c(0,0), breaks = seq(-3,0,1))+
  geom_point(size=2, col="#1b63a5") +geom_line(size=2, col="#1b63a5") + theme_pubr() +
  theme(
    legend.position = "none",
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(1, 1, 1, 1,),
    legend.title=element_blank(),
    text = element_text(size = 20)
  ) + guides(alpha = "none") +
  ylab("Bias (m)") + xlab("Relative Height (rh)") + aes(group=rev(Data_short))
Bias_plot  

Rsq_plot<-ggplot(subset(error_summary2, Data_short=="Flags Removed*"), aes(x=rh, y=rsq, col=Data_short, alpha=0.9)) + 
  scale_x_continuous(limits = c(0,105), expand = c(0,0), breaks = seq(0,100,25)) +
  scale_y_continuous (limits = c(0,0.85), expand = c(0,0), breaks = seq(0,0.8,0.2)) +
  geom_point(size=2, col="#1b63a5") +geom_line(size=2, col="#1b63a5") + theme_pubr() +
  theme(
    legend.position = "none",
    text = element_text(size = 20)
  ) + guides(alpha = "none") +
  ylab("R-Squared") + xlab("Relative Height (rh)") + aes(group=rev(Data_short))
Rsq_plot

ggarrange(Rsq_plot, RMSEpct_plot, 
          Bias_plot, RMSE_plot,
          ncol = 2,
          nrow = 2, 
          common.legend = FALSE)
#png(file="../Figures/Final/error_summary_1line.png", width=12, height=11, units = "in", res=120) #res in ppi 300dpi = 118 ppi
ggarrange(Rsq_plot, RMSEpct_plot,
          Bias_plot, RMSE_plot,
          ncol = 2,
          nrow = 2,
          common.legend = FALSE)
dev.off()


#juse geo and no geolocation correction

RMSE_plot<-ggplot(subset(collocate_error, 
                         Data_short=="No Geolocation Correction"| Data_short=="Geolocation Corrected*"), 
                  aes(x=rh, y=RMSE, col=Data_short, alpha=0.25)) + 
  scale_x_continuous(limits = c(0,105), expand = c(0,0), breaks = seq(0,100,25)) +
  scale_y_continuous (limits = c(2,8.2), expand = c(0,0), breaks = seq(2,8,2)) +
  geom_point(aes(col=Data_short), size=2) +geom_line(aes(col=Data_short), size=2) + theme_pubr() + scale_colour_brewer(palette = "Paired") + 
  theme(
    legend.position = "none",
    text = element_text(size = 20)
  ) + guides(alpha = "none") +
  ylab("RMSE (m)") + xlab("Relative Height (rh)")
RMSE_plot  

RMSEpct_plot<-ggplot(subset(collocate_error, 
                            Data_short=="No Geolocation Correction"| Data_short=="Geolocation Corrected*"),
                     aes(x=rh, y=RMSEpct, col=Data_short, alpha=0.25)) + 
  scale_x_continuous(limits = c(0,105), expand = c(0,0), breaks = seq(0,100,25)) +
  scale_y_continuous (limits = c(15,103), expand = c(0,0), breaks = seq(20,100,20)) +
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

Bias_plot<-ggplot(subset(collocate_error, 
                         Data_short=="No Geolocation Correction"| Data_short=="Geolocation Corrected*"), 
                  aes(x=rh, y=Bias, col=Data_short, alpha=0.25)) + 
  scale_x_continuous(limits = c(0,105), expand = c(0,0), breaks = seq(0,100,25)) +
  scale_y_continuous (limits = c(-3.2, 0.05), expand = c(0,0), breaks = seq(-3,0,1)) +
  geom_point(aes(col=Data_short), size=2) +geom_line(aes(col=Data_short), size=2) + theme_pubr() + scale_colour_brewer(palette = "Paired") + 
  theme(
    legend.position = c(.7, .93),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(1, 1, 1, 1,),
    legend.title=element_blank(),
    text = element_text(size = 20)
  ) + guides(alpha = "none") +
  ylab("Bias (m)") + xlab("Relative Height (rh)")
Bias_plot  

Rsq_plot<-ggplot(subset(collocate_error, 
                        Data_short=="No Geolocation Correction"| Data_short=="Geolocation Corrected*"),
                 aes(x=rh, y=rsq, col=Data_short, alpha=0.9)) + 
  scale_x_continuous(limits = c(0,105), expand = c(0,0), breaks = seq(0,100,25)) +
  scale_y_continuous (limits = c(0,1.03), expand = c(0,0), breaks = seq(0,1,0.2)) +
  geom_point(aes(col=Data_short), size=2) +geom_line(aes(col=Data_short), size=2) + theme_pubr() + scale_colour_brewer(palette = "Paired")+ 
  theme(
    legend.position = "none",
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(1, 1, 1, 1,),
    legend.title=element_blank(),
    text = element_text(size = 20)
  ) + guides(alpha = "none") +
  ylab("R-Squared") + xlab("Relative Height (rh)")
Rsq_plot

#png(file="../Figures/Final/error_summary_geo_2lines.png", width=12, height=11, units = "in", res=120) #res in ppi 300dpi = 118 ppi
ggarrange(Rsq_plot, RMSEpct_plot, 
          Bias_plot, RMSE_plot,
          ncol = 2,
          nrow = 2, 
          common.legend = FALSE)
dev.off()



d0<-get_density(L2A_compare1$rh_0, L2A_compare1$rhGauss.0, n=50)
P0<-ggplot(L2A_compare1) + geom_point(aes(x=rh_0, y=rhGauss.0, color=d0)) + 
  geom_abline(intercept = 0, slope = 1, size=1) + scale_color_viridis() +
  theme_bw() + theme(legend.position = "none") +
  xlim(-15,30) + ylim(-15,30) +
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=0.2)+ 
  geom_vline(xintercept=0, linetype="dashed", color = "red", size=0.2) +
  xlab("rh 0 (m)") + ylab("rh 0 (m)")
  #geom_label(x=(50/8), y=45,nudge_x = 0.1, label=paste0("RMSE: ",substr(error_summary2[1,2],1,4),"\n","R^2: ",substr(error_summary2[1,3],1,4)))
d5<-get_density(L2A_compare1$rh_5, L2A_compare1$rhGauss.5, n=50)
P5<-ggplot(L2A_compare1) + geom_point(aes(x=rh_5, y=rhGauss.5, color=d5)) + 
  geom_abline(intercept = 0, slope = 1, size=1) + scale_color_viridis() +
  theme_bw() + theme(legend.position = "none") +
  xlim(-15,30) + ylim(-15,30)+
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=0.2)+ 
  geom_vline(xintercept=0, linetype="dashed", color = "red", size=0.2) +
  xlab("rh 5 (m)") + ylab("rh 5 (m)")
  #geom_label(x=(50/8), y=45,nudge_x = 0.1, label=paste0("RMSE: ",substr(error_summary2[5,2],1,4),"\n","R^2: ",substr(error_summary2[5,3],1,4)))
d10<-get_density(L2A_compare1$rh_10, L2A_compare1$rhGauss.10, n=50)
P10<-ggplot(L2A_compare1) + geom_point(aes(x=rh_10, y=rhGauss.10, color=d10)) + 
  geom_abline(intercept = 0, slope = 1, size=1) + scale_color_viridis() +
  theme_bw() + theme(legend.position = "none") +
  xlim(-15,30) + ylim(-15,30)+
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=0.2)+ 
  geom_vline(xintercept=0, linetype="dashed", color = "red", size=0.2) +
  xlab("rh 10 (m)") + ylab("rh 10 (m)")
  #geom_label(x=(50/8), y=45,nudge_x = 0.1, label=paste0("RMSE: ",substr(error_summary2[10,2],1,4),"\n","R^2: ",substr(error_summary2[10,3],1,4)))
d15<-get_density(L2A_compare1$rh_15, L2A_compare1$rhGauss.15, n=50)
P15<-ggplot(L2A_compare1) + geom_point(aes(x=rh_15, y=rhGauss.15, color=d15)) + 
  geom_abline(intercept = 0, slope = 1, size=1) + scale_color_viridis() +
  theme_bw() + theme(legend.position = "none") +
  xlim(-15,30) + ylim(-15,30)+
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=0.2)+ 
  geom_vline(xintercept=0, linetype="dashed", color = "red", size=0.2) +
  xlab("rh 15 (m)") + ylab("rh 15 (m)")
 # geom_label(x=(50/8), y=45,nudge_x = 0.1, label=paste0("RMSE: ",substr(error_summary2[15,2],1,4),"\n","R^2: ",substr(error_summary2[15,3],1,4)))
d20<-get_density(L2A_compare1$rh_20, L2A_compare1$rhGauss.20, n=50)
P20<-ggplot(L2A_compare1) + geom_point(aes(x=rh_20, y=rhGauss.20, color=d20)) + 
  geom_abline(intercept = 0, slope = 1, size=1) + scale_color_viridis() +
  theme_bw() + theme(legend.position = "none") +
   xlim(-15,30) + ylim(-15,30)+
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=0.2)+ 
  geom_vline(xintercept=0, linetype="dashed", color = "red", size=0.2) +
  xlab("rh 20 (m)") + ylab("rh 20 (m)")
  #geom_label(x=(50/8), y=45,nudge_x = 0.1, label=paste0("RMSE: ",substr(error_summary2[20,2],1,4),"\n","R^2: ",substr(error_summary2[20,3],1,4)))
d25<-get_density(L2A_compare1$rh_25, L2A_compare1$rhGauss.25, n=50)
P25<-ggplot(L2A_compare1) + geom_point(aes(x=rh_25, y=rhGauss.25, color=d25)) + 
  geom_abline(intercept = 0, slope = 1, size=1) + scale_color_viridis() +
  theme_bw() + theme(legend.position = "none")+
  xlim(-5,50) + ylim(-5,50)+
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=0.2)+ 
  geom_vline(xintercept=0, linetype="dashed", color = "red", size=0.2) +
  xlab("rh 25 (m)") + ylab("rh 25 (m)")
 # geom_label(x=(50/8), y=45,nudge_x = 0.1, label=paste0("RMSE: ",substr(error_summary2[25,2],1,4),"\n","R^2: ",substr(error_summary2[25,3],1,4)))
d30<-get_density(L2A_compare1$rh_30, L2A_compare1$rhGauss.30, n=50)
P30<-ggplot(L2A_compare1) + geom_point(aes(x=rh_30, y=rhGauss.30, color=d30)) + 
  geom_abline(intercept = 0, slope = 1, size=1) + scale_color_viridis() +
  theme_bw() + theme(legend.position = "none")+
  xlim(-5,50) + ylim(-5,50)+
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=0.2)+ 
  geom_vline(xintercept=0, linetype="dashed", color = "red", size=0.2) +
  xlab("rh 30 (m)") + ylab("rh 30 (m)")
  #geom_label(x=(50/8), y=45,nudge_x = 0.1, label=paste0("RMSE: ",substr(error_summary2[30,2],1,4),"\n","R^2: ",substr(error_summary2[30,3],1,4)))
d35<-get_density(L2A_compare1$rh_35, L2A_compare1$rhGauss.35, n=50)
P35<-ggplot(L2A_compare1) + geom_point(aes(x=rh_35, y=rhGauss.35, color=d35)) + 
  geom_abline(intercept = 0, slope = 1, size=1) + scale_color_viridis() +
  theme_bw() + theme(legend.position = "none")+
  xlim(-5,50) + ylim(-5,50)+
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=0.2)+ 
  geom_vline(xintercept=0, linetype="dashed", color = "red", size=0.2) +
  xlab("rh 35 (m)") + ylab("rh 35 (m)")
  #geom_label(x=(50/8), y=45,nudge_x = 0.1, label=paste0("RMSE: ",substr(error_summary2[35,2],1,4),"\n","R^2: ",substr(error_summary2[35,3],1,4)))
d40<-get_density(L2A_compare1$rh_40, L2A_compare1$rhGauss.40, n=50)
P40<-ggplot(L2A_compare1) + geom_point(aes(x=rh_40, y=rhGauss.40, color=d40)) + 
  geom_abline(intercept = 0, slope = 1, size=1) + scale_color_viridis() +
  theme_bw() + theme(legend.position = "none")+
  xlim(-5,50) + ylim(-5,50)+
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=0.2)+ 
  geom_vline(xintercept=0, linetype="dashed", color = "red", size=0.2) +
  xlab("rh 40 (m)") + ylab("rh 40 (m)")
  #geom_label(x=(50/8), y=45,nudge_x = 0.1, label=paste0("RMSE: ",substr(error_summary2[40,2],1,4),"\n","R^2: ",substr(error_summary2[40,3],1,4)))
d45<-get_density(L2A_compare1$rh_45, L2A_compare1$rhGauss.45, n=50)
P45<-ggplot(L2A_compare1) + geom_point(aes(x=rh_45, y=rhGauss.45, color=d45)) + 
  geom_abline(intercept = 0, slope = 1, size=1) + scale_color_viridis() +
  theme_bw() + theme(legend.position = "none")+
  xlim(0,50) + ylim(0,50)+
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=0.2)+ 
  geom_vline(xintercept=0, linetype="dashed", color = "red", size=0.2) +
  xlab("rh 45 (m)") + ylab("rh 45 (m)")
 # geom_label(x=(50/8), y=45,nudge_x = 0.1, label=paste0("RMSE: ",substr(error_summary2[45,2],1,4),"\n","R^2: ",substr(error_summary2[45,3],1,4)))
d50<-get_density(L2A_compare1$rh_50, L2A_compare1$rhGauss.50, n=50)
P50<-ggplot(L2A_compare1) + geom_point(aes(x=rh_50, y=rhGauss.50, color=d50)) + 
  geom_abline(intercept = 0, slope = 1, size=1) + scale_color_viridis() +
  theme_bw() + theme(legend.position = "none")+
  xlim(0,50) + ylim(0,50) +
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=0.2)+ 
  geom_vline(xintercept=0, linetype="dashed", color = "red", size=0.2) +
  xlab("rh 50 (m)") + ylab("rh 50 (m)")
  #geom_label(x=(50/8), y=45,nudge_x = 0.1, label=paste0("RMSE: ",substr(error_summary2[50,2],1,4),"\n","R^2: ",substr(error_summary2[50,3],1,4)))
d55<-get_density(L2A_compare1$rh_55, L2A_compare1$rhGauss.55, n=50)
P55<-ggplot(L2A_compare1) + geom_point(aes(x=rh_55, y=rhGauss.55, color=d55)) + 
  geom_abline(intercept = 0, slope = 1, size=1) + scale_color_viridis() +
  theme_bw() + theme(legend.position = "none")+
  xlim(0,50) + ylim(0,50) +
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=0.2)+ 
  geom_vline(xintercept=0, linetype="dashed", color = "red", size=0.2) +
  xlab("rh 55 (m)") + ylab("rh 55 (m)")
  #geom_label(x=(50/8), y=45,nudge_x = 0.1, label=paste0("RMSE: ",substr(error_summary2[55,2],1,4),"\n","R^2: ",substr(error_summary2[55,3],1,4)))
d60<-get_density(L2A_compare1$rh_60, L2A_compare1$rhGauss.60, n=50)
P60<-ggplot(L2A_compare1) + geom_point(aes(x=rh_60, y=rhGauss.60, color=d60)) + 
  geom_abline(intercept = 0, slope = 1, size=1) + scale_color_viridis() +
  theme_bw() + theme(legend.position = "none") +
  xlim(0,50) + ylim(0,50)+
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=0.2)+ 
  geom_vline(xintercept=0, linetype="dashed", color = "red", size=0.2) +
  xlab("rh 60 (m)") + ylab("rh 60 (m)")
  #geom_label(x=(50/8), y=45,nudge_x = 0.1, label=paste0("RMSE: ",substr(error_summary2[60,2],1,4),"\n","R^2: ",substr(error_summary2[60,3],1,4)))
d65<-get_density(L2A_compare1$rh_65, L2A_compare1$rhGauss.65, n=50)
P65<-ggplot(L2A_compare1) + geom_point(aes(x=rh_65, y=rhGauss.65, color=d65)) + 
  geom_abline(intercept = 0, slope = 1, size=1) + scale_color_viridis() +
  theme_bw() + theme(legend.position = "none") +
  xlim(0,60) + ylim(0,60)+
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=0.2)+ 
  geom_vline(xintercept=0, linetype="dashed", color = "red", size=0.2) +
  xlab("rh 65 (m)") + ylab("rh 65 (m)")
 # geom_label(x=(50/8), y=45,nudge_x = 0.1, label=paste0("RMSE: ",substr(error_summary2[65,2],1,4),"\n","R^2: ",substr(error_summary2[65,3],1,4)))
d70<-get_density(L2A_compare1$rh_70, L2A_compare1$rhGauss.70, n=50)
P70<-ggplot(L2A_compare1) + geom_point(aes(x=rh_70, y=rhGauss.70, color=d70)) + 
  geom_abline(intercept = 0, slope = 1, size=1) + scale_color_viridis() +
  theme_bw() + theme(legend.position = "none") +
  xlim(0,60) + ylim(0,60)+
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=0.2)+ 
  geom_vline(xintercept=0, linetype="dashed", color = "red", size=0.2) +
  xlab("rh 70 (m)") + ylab("rh 70 (m)")
  #geom_label(x=(50/8), y=45,nudge_x = 0.1, label=paste0("RMSE: ",substr(error_summary2[70,2],1,4),"\n","R^2: ",substr(error_summary2[70,3],1,4)))
d75<-get_density(L2A_compare1$rh_75, L2A_compare1$rhGauss.75, n=50)
P75<-ggplot(L2A_compare1) + geom_point(aes(x=rh_75, y=rhGauss.75, color=d75)) + 
  geom_abline(intercept = 0, slope = 1, size=1) + scale_color_viridis() +
  theme_bw() + theme(legend.position = "none") +
  xlim(0,60) + ylim(0,60)+
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=0.2)+ 
  geom_vline(xintercept=0, linetype="dashed", color = "red", size=0.2) +
  xlab("rh 75 (m)") + ylab("rh 75 (m)")
  #geom_label(x=(50/8), y=45,nudge_x = 0.1, label=paste0("RMSE: ",substr(error_summary2[75,2],1,4),"\n","R^2: ",substr(error_summary2[75,3],1,4)))
d80<-get_density(L2A_compare1$rh_80, L2A_compare1$rhGauss.80, n=50)
P80<-ggplot(L2A_compare1) + geom_point(aes(x=rh_80, y=rhGauss.80, color=d80)) + 
  geom_abline(intercept = 0, slope = 1, size=1) + scale_color_viridis() +
  theme_bw() + theme(legend.position = "none") +
  xlim(0,60) + ylim(0,60)+
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=0.2)+ 
  geom_vline(xintercept=0, linetype="dashed", color = "red", size=0.2) +
  xlab("rh 80 (m)") + ylab("rh 80 (m)")
  #geom_label(x=(50/8), y=45,nudge_x = 0.1, label=paste0("RMSE: ",substr(error_summary2[80,2],1,4),"\n","R^2: ",substr(error_summary2[80,3],1,4)))
d85<-get_density(L2A_compare1$rh_85, L2A_compare1$rhGauss.85, n=50)
P85<-ggplot(L2A_compare1) + geom_point(aes(x=rh_85, y=rhGauss.85, color=d85)) + 
  geom_abline(intercept = 0, slope = 1, size=1) + scale_color_viridis() +
  theme_bw() + theme(legend.position = "none") +
  xlim(0,60) + ylim(0,60)+
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=0.2)+ 
  geom_vline(xintercept=0, linetype="dashed", color = "red", size=0.2) +
  xlab("rh 85 (m)") + ylab("rh 85 (m)")
  #geom_label(x=(50/8), y=45,nudge_x = 0.1, label=paste0("RMSE: ",substr(error_summary2[85,2],1,4),"\n","R^2: ",substr(error_summary2[85,3],1,4)))
d90<-get_density(L2A_compare1$rh_90, L2A_compare1$rhGauss.90, n=50)
P90<-ggplot(L2A_compare1) + geom_point(aes(x=rh_90, y=rhGauss.90, color=d90)) + 
  geom_abline(intercept = 0, slope = 1, size=1) + scale_color_viridis() +
  theme_bw() + theme(legend.position = "none") +
  xlim(0,60) + ylim(0,60) +
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=0.2)+ 
  geom_vline(xintercept=0, linetype="dashed", color = "red", size=0.2) +
  xlab("rh 90 (m)") + ylab("rh 90 (m)")
  #geom_label(x=(50/8), y=45,nudge_x = 0.1, label=paste0("RMSE: ",substr(error_summary2[90,2],1,4),"\n","R^2: ",substr(error_summary2[90,3],1,4)))
d95<-get_density(L2A_compare1$rh_95, L2A_compare1$rhGauss.95, n=50)
P95<-ggplot(L2A_compare1) + geom_point(aes(x=rh_95, y=rhGauss.95, color=d95)) + 
  geom_abline(intercept = 0, slope = 1, size=1) + scale_color_viridis() +
  theme_bw() + theme(legend.position = "none") +
  xlim(0,60) + ylim(0,60)+
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=0.2)+ 
  geom_vline(xintercept=0, linetype="dashed", color = "red", size=0.2) +
  xlab("rh 95 (m)") + ylab("rh 95 (m)")
  #geom_label(x=(50/8), y=45,nudge_x = 0.1, label=paste0("RMSE: ",substr(error_summary2[95,2],1,4),"\n","R^2: ",substr(error_summary2[95,3],1,4)))
d98<-get_density(L2A_compare1$rh_98, L2A_compare1$rhGauss.98, n=50)
P98<-ggplot(L2A_compare1) + geom_point(aes(x=rh_98, y=rhGauss.98, color=d98)) + 
  geom_abline(intercept = 0, slope = 1, size=1) + scale_color_viridis() + 
  theme_bw() + theme(legend.position = "none") +
  xlim(0,60) + ylim(0,60)+
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=0.2)+ 
  geom_vline(xintercept=0, linetype="dashed", color = "red", size=0.2) +
  xlab("rh 98 (m)") + ylab("rh 98 (m)")
 # geom_label(x=(50/8), y=45,nudge_x 
rh4by5<-ggarrange(P98, P95, P90, P85,
                  P80, P75, P70, P65, 
                  P60, P55, P50, P45, 
                  P40, P35, P30, P25,
                  P20, P15, P10, P5,
                  ncol = 4, nrow = 5)
annotate_figure(rh4by5,
                top = text_grob("GEDI Measured rh Values", size = 18), 
                left = text_grob(bquote('GEDI'['sim']*' rh Values'), size = 18, rot = 90))


#### rh of GEDI to sim by rh10 step ####
R<-expression(R^2)

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

#png(file="../Figures/Final/Final/GEDI_vs_GEDIsim_2x6.png", width=4.5, height=13.2, units = "in", res=120) #res in ppi 300dpi = 118 ppi
#tiff(file="../Figures/Final/RSE/Figure4.tiff", units="in", width=4.5, height=13.2, res=300)
annotate_figure(rh2by5,
                top = text_grob("GEDI RH Values", size = 18), 
                left = text_grob(bquote('GEDI'['sim']*' RH Values'), size = 18, rot = 90))
dev.off()

#### Correlation in derived GEDI Metrics ####
L2A_compare1$CR_gedi<-((L2A_compare_trunk$rh_98-L2A_compare_trunk$rh_25)/L2A_compare_trunk$rh_98)
L2A_compare1$CR_sim<-((L2A_compare_trunk$rhGauss.98-L2A_compare_trunk$rhGauss.25)/L2A_compare_trunk$rhGauss.98)
L2A_compare1$CR50_gedi<-((L2A_compare_trunk$rh_98-L2A_compare_trunk$rh_50)/L2A_compare_trunk$rh_98)
L2A_compare1$CR50_sim<-((L2A_compare_trunk$rhGauss.98-L2A_compare_trunk$rhGauss.50)/L2A_compare_trunk$rhGauss.98)
L2A_compare1$R98_50_gedi<-((L2A_compare_trunk$rh_98/L2A_compare_trunk$rh_50))
L2A_compare1$R98_50_sim<-((L2A_compare_trunk$rhGauss.98/L2A_compare_trunk$rhGauss.50))

geo_derrived<-read.csv("../Spatial/LiDAR/Derrived_GEO_v4.csv")
geo_derrived$data<-"geo"
derrived_raw<-L2A_compare1[,c("rh_sum98","rhGauss_sum98","CR_sim","CR_gedi","CR50_sim","CR50_gedi","R98_50_gedi","R98_50_sim")]
derrived<-derrived_raw
derrived$data<-"raw"

L2A_compare_forest<-subset(L2A_compare1, Forest==1)
derrived_forest<-L2A_compare_forest[,c("rh_sum98","rhGauss_sum98","CR_sim","CR_gedi","CR50_sim","CR50_gedi","R98_50_gedi","R98_50_sim")]

#r_squared
cor(geo_derrived$rh_sum98, geo_derrived$rhGauss_sum98) #0.86
cor(geo_derrived$CR_sim, geo_derrived$CR_gedi) # 0.67
cor(geo_derrived$CR50_sim, geo_derrived$CR50_gedi) # 0.77

cor(derrived_raw$rh_sum98, derrived_raw$rhGauss_sum98) # 0.82
cor(derrived_raw$CR_sim, derrived_raw$CR_gedi) # 0.67
cor(derrived_raw$CR50_sim, derrived_raw$CR50_gedi) # 0.74

cor(derrived_forest$rh_sum98, derrived_forest$rhGauss_sum98) # 0.71
cor(derrived_forest$CR_sim, derrived_forest$CR_gedi) # 0.58
cor(derrived_forest$CR50_sim, derrived_forest$CR50_gedi) # 0.61

#RMSE
sqrt(mean((geo_derrived$rhGauss_sum98 - geo_derrived$rh_sum98)^2)) #399
sqrt(mean((geo_derrived$CR_gedi - geo_derrived$CR_sim)^2)) #0.16
sqrt(mean((geo_derrived$CR50_gedi - geo_derrived$CR50_sim)^2)) #0.17

sqrt(mean((derrived_raw$rhGauss_sum98 - derrived_raw$rh_sum98)^2)) #428.8318
sqrt(mean((derrived_raw$CR_gedi - derrived_raw$CR_sim)^2)) # 0.1593211
sqrt(mean((derrived_raw$CR50_gedi - derrived_raw$CR50_sim)^2)) # 0.1779196

sqrt(mean((derrived_forest$rhGauss_sum98 - derrived_forest$rh_sum98)^2)) #456
sqrt(mean((derrived_forest$CR_gedi - derrived_forest$CR_sim)^2)) # 0.166
sqrt(mean((derrived_forest$CR50_gedi - derrived_forest$CR50_sim)^2)) # 0.177

#RMSE$
(sqrt(mean((geo_derrived$rhGauss_sum98 - geo_derrived$rh_sum98)^2)))/mean(geo_derrived$rhGauss_sum98)*100 #32%
(sqrt(mean((geo_derrived$CR_gedi - geo_derrived$CR_sim)^2)))/mean(geo_derrived$CR_sim)*100 #23%
(sqrt(mean((geo_derrived$CR50_gedi - geo_derrived$CR50_sim)^2)))/mean(geo_derrived$CR50_sim)*100 #33%

(sqrt(mean((derrived_raw$rhGauss_sum98 - derrived_raw$rh_sum98)^2)))/mean(derrived_raw$rhGauss_sum98)*100 #35%
(sqrt(mean((derrived_raw$CR_gedi - derrived_raw$CR_sim)^2)))/mean(derrived_raw$CR_sim)*100 # 22%
(sqrt(mean((derrived_raw$CR50_gedi - derrived_raw$CR50_sim)^2)))/mean(derrived_raw$CR50_sim)*100 #34%

derrived<-rbind(derrived, geo_derrived[,-1])

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

# exp1<-expression(paste('rh'['sum']*"=  ",sum(italic(rh)[italic(i)], italic(i) == 0, italic(98))))
# Sum_eq<-ggplot() + theme_void() + annotate("text", x= 0, y=0, 
#                                          label= exp1, size = 7)
# exp2<-expression(paste("rh98:rh50 =   ", frac(italic(rh)[98], italic(rh)[50])))
# r_eq<-ggplot() + theme_void() + annotate("text", x= 0, y=0, 
#                                            label= exp2, size = 7)
# exp3<-expression(paste("Canopy \n Ratio", " =   ", frac((italic(rh)[98] - italic(rh)[25]), italic(rh)[98])))
# cr_eq<-ggplot() + theme_void() + annotate("text", x= 0, y=0, 
#                                          label= exp3, size = 7)

compound_error<-ggarrange(rh_sum, r,cr, 
          nrow = 1,
          ncol = 3, common.legend = TRUE)

annotate_figure(compound_error,
                bottom = text_grob("GEDI", size = 18), 
                left = text_grob(bquote('GEDI'['sim']), size = 18, rot = 90))


#png(file="../Figures/Final/Final/Derrived_Metrics.png", width=9, height=4, units = "in", res=120) #res in ppi 300dpi = 118 ppi
tiff(file="../Figures/Final/SRS/Figure8.tiff", units="in", width=10, height=4, res=300)
annotate_figure(compound_error,
                bottom = text_grob("GEDI", size = 18), 
                left = text_grob(bquote('GEDI'['sim']), size = 18, rot = 90))
dev.off()


rss <- sum((L2A_compare1$rh_75 - L2A_compare1$rhGauss.75) ^ 2)  ## residual sum of squares
tss <- sum((L2A_compare1$rhGauss.75 - mean(L2A_compare1$rhGauss.75)) ^ 2)  ## total sum of squares
rsq <- 1 - rss/tss
RMSE<-sqrt(mean((L2A_compare1$rhGauss.75 - L2A_compare1$rh_75)^2)) #RMSE
d_75<-get_density(L2A_compare1$rh_75, L2A_compare1$rhGauss.75, n=100)
B<-ggplot(L2A_compare1) + geom_point(aes(x=rh_75, y=rhGauss.75, color=d_75)) + 
  geom_abline(intercept = 0, slope = 1, size=1) + scale_color_viridis() +  
  theme_bw() + xlim(0,60) + ylim(0,60) + 
  geom_label(x=(60/8), y=55, nudge_x = 0.1, label=paste0("RMSE: ",substr(RMSE,1,4),"\n","R^2: ",substr(rsq,1,4)))

summary(lm(rh_98~rhGauss.98, data = L2A_compare1))
rss <- sum((L2A_compare1$rh_98 - L2A_compare1$rhGauss.98) ^ 2)  ## residual sum of squares
tss <- sum((L2A_compare1$rhGauss.98 - mean(L2A_compare1$rhGauss.98)) ^ 2)  ## total sum of squares
rsq <- 1 - rss/tss
RMSE<-sqrt(mean((L2A_compare1$rhGauss.98 - L2A_compare1$rh_98)^2)) #RMSE
d_100<-get_density(L2A_compare1$rh_98, L2A_compare1$rhGauss.98, n=100)
C<-ggplot(L2A_compare1) + geom_point(aes(x=rh_98, y=rhGauss.98, color=d_100)) + 
  geom_abline(intercept = 0, slope = 1, size=1) + scale_color_viridis() +  
  theme_bw() + xlim(0,60) + ylim(0,60) + 
  geom_label(x=(60/8), y=55, nudge_x = 0.1, label=paste0("RMSE: ",substr(RMSE,1,4),"\n","R^2: ",substr(rsq,1,4)))
C

rss <- sum((L2A_compare1$rh_sum98 - L2A_compare1$rhGauss_sum98) ^ 2)  ## residual sum of squares
tss <- sum((L2A_compare1$rhGauss_sum98 - mean(L2A_compare1$rhGauss_sum98)) ^ 2)  ## total sum of squares
rsq <- 1 - rss/tss
RMSE<-sqrt(mean((L2A_compare1$rhGauss_sum98 - L2A_compare1$rh_sum98)^2)) #RMSE
d_sum<-get_density(L2A_compare1$rh_sum98, L2A_compare1$rhGauss_sum98, n=100)
D<-ggplot(L2A_compare1) + geom_point(aes(x=rh_sum98, y=rhGauss_sum98, color=d_sum)) + 
  geom_abline(intercept = 0, slope = 1, size=1) + scale_color_viridis() +  
  theme_bw() + xlim(0,4000) + ylim(0,4000) + 
  geom_label(x=(4000/8), y=3700, nudge_x = 0.1,label=paste0("RMSE: ",substr(RMSE,1,4),"\n","R^2: ",substr(rsq,1,4)))
D

ggarrange(A, B, C, D, 
          ncol=2, nrow=2)

#Just forest plots
# Plots of R-squared and RMSE through Canopy ####
error_summary_forest<-subset(error_summary2, Data_short=="Flags Removed*")

L2A_compare_forest<-subset(L2A_compare1, Forest==1)
for (i in 1:100) {
  error_summary_forest[((100+i)),2]<-sqrt(mean((L2A_compare_forest[,c((179+i))] - L2A_compare_forest[,c((16+i))])^2))
  rss <- sum((L2A_compare_forest[,c((16+i))]- L2A_compare_forest[,c((179+i))])^ 2)  ## residual sum of squares
  tss <- sum((L2A_compare_forest[,c((179+i))] - mean(L2A_compare_forest[,c((179+i))])) ^ 2)  ## total sum of squares
  error_summary_forest[((100+i)),3] <- 1 - rss/tss
  error_summary_forest[((100+i)),4] <- ((error_summary_forest[((100+i)),2]/mean(L2A_compare_forest[,c((179+i))]))*100)
  error_summary_forest[((100+i)),5]<-(sum(L2A_compare_forest[,c((16+i))]-L2A_compare_forest[,c((179+i))])/nrow(L2A_compare_forest))
}
L2A_compare_forest<-subset(L2A_compare1, Forest!=1)
for (i in 1:100) {
  error_summary_forest[((200+i)),2]<-sqrt(mean((L2A_compare_forest[,c((179+i))] - L2A_compare_forest[,c((16+i))])^2))
  rss <- sum((L2A_compare_forest[,c((16+i))]- L2A_compare_forest[,c((179+i))])^ 2)  ## residual sum of squares
  tss <- sum((L2A_compare_forest[,c((179+i))] - mean(L2A_compare_forest[,c((179+i))])) ^ 2)  ## total sum of squares
  error_summary_forest[((200+i)),3] <- 1 - rss/tss
  error_summary_forest[((200+i)),4] <- ((error_summary_forest[((200+i)),2]/mean(L2A_compare_forest[,c((179+i))]))*100)
  error_summary_forest[((200+i)),5]<-(sum(L2A_compare_forest[,c((16+i))]-L2A_compare_forest[,c((179+i))])/nrow(L2A_compare_forest))
}


error_summary_forest$Data<-rep(c("Flagged Data Removed*","Forest","Ag or Grassland"), each=100)
error_summary_forest$Data_short<-rep(c("Flags Removed*","Forest","Ag or Grassland"), each=100)
error_summary_forest$Data_short<-as.factor(error_summary_forest$Data_short)
error_summary_forest$rh<-rep(c(1:100))

library(RColorBrewer)
RMSE_plot<-ggplot(error_summary_forest, aes(x=rh, y=RMSE, col=Data_short, alpha=0.25)) + 
  scale_x_continuous(limits = c(0,105), expand = c(0,0), breaks = seq(0,100,25)) +
  scale_y_continuous (limits = c(0,7), expand = c(0,0), breaks = seq(0,6,2)) +
  geom_point(aes(col=Data_short), size=2) +geom_line(aes(col=Data_short), size=2) + theme_pubr() + scale_colour_brewer(palette = "Paired") + 
  theme(
    legend.position = c(1, .5),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(1, 1, 1, 1,),
    legend.title=element_blank(),
    text = element_text(size = 18)
  ) + guides(alpha = "none") +
  ylab("RMSE (m)") + xlab("Relative Height (rh)") + aes(group=rev(Data_short))
RMSE_plot 

RMSEpct_plot<-ggplot(error_summary_forest, aes(x=rh, y=RMSEpct, col=Data_short, alpha=0.25)) + 
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
  ylab("RMSE%") + xlab("Relative Height (rh)") + aes(group=rev(Data_short))
RMSEpct_plot  

Bias_plot<-ggplot(error_summary_forest, aes(x=rh, y=Bias, col=Data_short, alpha=0.25)) + 
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
  ylab("Bias (m)") + xlab("Relative Height (rh)") + aes(group=rev(Data_short))
Bias_plot  

Rsq_plot<-ggplot(error_summary_forest, aes(x=rh, y=rsq, col=Data_short, alpha=0.9)) + 
  scale_x_continuous(limits = c(0,105), expand = c(0,0), breaks = seq(0,100,25)) +
  scale_y_continuous (limits = c(0,0.85), expand = c(0,0), breaks = seq(0,0.8,0.2)) +
  geom_point(aes(col=Data_short), size=2) +geom_line(aes(col=Data_short), size=2) + theme_pubr() + scale_colour_brewer(palette = "Paired")+ 
  theme(
    legend.position = "none",
    text = element_text(size = 20)
  ) + guides(alpha = "none") +
  ylab("R-Squared") + xlab("Relative Height (rh)") + aes(group=rev(Data_short))
Rsq_plot

ggarrange(Rsq_plot, RMSEpct_plot, 
          Bias_plot, RMSE_plot,
          ncol = 2,
          nrow = 2, 
          common.legend = FALSE)
# #png(file="../Figures/Final/Filtering_error_summary.png", width=12, height=11, units = "in", res=120) #res in ppi 300dpi = 118 ppi
# ggarrange(Rsq_plot, RMSEpct_plot,
#           Bias_plot, RMSE_plot,
#           ncol = 2,
#           nrow = 2,
#           common.legend = FALSE)
# dev.off()

d10<-get_density(L2A_compare_forest$rh_10, L2A_compare_forest$rhGauss.10, n=50)
P10<-ggplot(L2A_compare_forest) + geom_point(aes(x=rh_10, y=rhGauss.10, color=d10)) + 
  geom_abline(intercept = 0, slope = 1, size=1) + scale_color_viridis() +
  theme_bw() + theme(legend.position = "none") + 
  scale_x_continuous(limits = c(-10,30), expand = c(0,0), breaks = seq(-10,30,10)) +
  scale_y_continuous (limits = c(-10,30), expand = c(0,0), breaks = seq(-10,30,10))+ 
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=0.2)+ 
  geom_vline(xintercept=0, linetype="dashed", color = "red", size=0.2) +
  xlab("rh 10 (m)") + ylab("rh 10 (m)")
d20<-get_density(L2A_compare_forest$rh_20, L2A_compare_forest$rhGauss.20, n=50)
P20<-ggplot(L2A_compare_forest) + geom_point(aes(x=rh_20, y=rhGauss.20, color=d20)) + 
  geom_abline(intercept = 0, slope = 1, size=1) + scale_color_viridis() +
  theme_bw() + theme(legend.position = "none") + 
  scale_x_continuous(limits = c(-10,30), expand = c(0,0), breaks = seq(-10,30,10)) +
  scale_y_continuous (limits = c(-10,30), expand = c(0,0), breaks = seq(-10,30,10))+ 
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=0.2)+ 
  geom_vline(xintercept=0, linetype="dashed", color = "red", size=0.2) +
  xlab("rh 20 (m)") + ylab("rh 20 (m)")
d30<-get_density(L2A_compare_forest$rh_30, L2A_compare_forest$rhGauss.30, n=50)
P30<-ggplot(L2A_compare_forest) + geom_point(aes(x=rh_30, y=rhGauss.30, color=d30)) + 
  geom_abline(intercept = 0, slope = 1, size=1) + scale_color_viridis() +
  theme_bw() + theme(legend.position = "none") + 
  scale_x_continuous(limits = c(-5,45), expand = c(0,0), breaks = seq(0,40,10)) +
  scale_y_continuous (limits = c(-5,45), expand = c(0,0), breaks = seq(0,40,10))+ 
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=0.2)+ 
  geom_vline(xintercept=0, linetype="dashed", color = "red", size=0.2) +
  xlab("rh 30 (m)") + ylab("rh 30 (m)")
d40<-get_density(L2A_compare_forest$rh_40, L2A_compare_forest$rhGauss.40, n=50)
P40<-ggplot(L2A_compare_forest) + geom_point(aes(x=rh_40, y=rhGauss.40, color=d40)) + 
  geom_abline(intercept = 0, slope = 1, size=1) + scale_color_viridis() +
  theme_bw() + theme(legend.position = "none")+ 
  scale_x_continuous(limits = c(-5,45), expand = c(0,0), breaks = seq(0,40,10)) +
  scale_y_continuous (limits = c(-5,45), expand = c(0,0), breaks = seq(0,40,10))+ 
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=0.2)+ 
  geom_vline(xintercept=0, linetype="dashed", color = "red", size=0.2) +
  xlab("rh 40 (m)") + ylab("rh 40 (m)")
d50<-get_density(L2A_compare_forest$rh_50, L2A_compare_forest$rhGauss.50, n=50)
P50<-ggplot(L2A_compare_forest) + geom_point(aes(x=rh_50, y=rhGauss.50, color=d50)) + 
  geom_abline(intercept = 0, slope = 1, size=1) + scale_color_viridis() +
  theme_bw() + theme(legend.position = "none")+ 
  scale_x_continuous(limits = c(-5,50), expand = c(0,0), breaks = seq(0,50,10)) +
  scale_y_continuous (limits = c(-5,50), expand = c(0,0), breaks = seq(0,50,10))+ 
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=0.2)+ 
  geom_vline(xintercept=0, linetype="dashed", color = "red", size=0.2) +
  xlab("rh 50 (m)") + ylab("rh 50 (m)")
d60<-get_density(L2A_compare_forest$rh_60, L2A_compare_forest$rhGauss.60, n=50)
P60<-ggplot(L2A_compare_forest) + geom_point(aes(x=rh_60, y=rhGauss.60, color=d60)) + 
  geom_abline(intercept = 0, slope = 1, size=1) + scale_color_viridis() +
  theme_bw() + theme(legend.position = "none") + 
  scale_x_continuous(limits = c(-5,50), expand = c(0,0), breaks = seq(0,50,10)) +
  scale_y_continuous (limits = c(-5,50), expand = c(0,0), breaks = seq(0,50,10))+ 
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=0.2)+ 
  geom_vline(xintercept=0, linetype="dashed", color = "red", size=0.2) +
  xlab("rh 60 (m)") + ylab("rh 60 (m)")
d70<-get_density(L2A_compare_forest$rh_70, L2A_compare_forest$rhGauss.70, n=50)
P70<-ggplot(L2A_compare_forest) + geom_point(aes(x=rh_70, y=rhGauss.70, color=d70)) + 
  geom_abline(intercept = 0, slope = 1, size=1) + scale_color_viridis() +
  theme_bw() + theme(legend.position = "none") + 
  scale_x_continuous(limits = c(-5,50), expand = c(0,0), breaks = seq(0,50,10)) +
  scale_y_continuous (limits = c(-5,50), expand = c(0,0), breaks = seq(0,50,10))+ 
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=0.2)+ 
  geom_vline(xintercept=0, linetype="dashed", color = "red", size=0.2) +
  xlab("rh 70 (m)") + ylab("rh 70 (m)")
d80<-get_density(L2A_compare_forest$rh_80, L2A_compare_forest$rhGauss.80, n=50)
P80<-ggplot(L2A_compare_forest) + geom_point(aes(x=rh_80, y=rhGauss.80, color=d80)) + 
  geom_abline(intercept = 0, slope = 1, size=1) + scale_color_viridis() +
  theme_bw() + theme(legend.position = "none")   + 
  scale_x_continuous(limits = c(-5,50), expand = c(0,0), breaks = seq(0,50,10)) +
  scale_y_continuous (limits = c(-5,50), expand = c(0,0), breaks = seq(0,50,10)) + 
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=0.2)+ 
  geom_vline(xintercept=0, linetype="dashed", color = "red", size=0.2) +
  xlab("rh 80 (m)") + ylab("rh 80 (m)")
d90<-get_density(L2A_compare_forest$rh_90, L2A_compare_forest$rhGauss.90, n=50)
P90<-ggplot(L2A_compare_forest) + geom_point(aes(x=rh_90, y=rhGauss.90, color=d90)) + 
  geom_abline(intercept = 0, slope = 1, size=1) + scale_color_viridis() +
  theme_bw() + theme(legend.position = "none")  + 
  scale_x_continuous(limits = c(0,60), expand = c(0,0), breaks = seq(0,60,10)) +
  scale_y_continuous (limits = c(0,60), expand = c(0,0), breaks = seq(0,60,10)) + 
  geom_hline(yintercept=0.1, linetype="dashed", color = "red", size=0.2)+ 
  geom_vline(xintercept=0.1, linetype="dashed", color = "red", size=0.2) +
  xlab("rh 90 (m)") + ylab("rh 90 (m)")
d98<-get_density(L2A_compare_forest$rh_98, L2A_compare_forest$rhGauss.98, n=50)
P98<-ggplot(L2A_compare_forest) + geom_point(aes(x=rh_98, y=rhGauss.98, color=d98)) + 
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
