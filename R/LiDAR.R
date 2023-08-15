library(sp)
library(raster)
library(rgdal)
library(lidR)
library(ggplot2)
library(hdf5r)
library(devtools)
#install.packages("rGEDI", repos="http://R-Forge.R-project.org")
library(rGEDI)
library(maptools)



L2A<-readOGR(dsn="G:/Thesis/Spatial/LiDAR", layer = "ALS_Overlap_wFID")
L2A_pts<-readOGR(dsn="G:/Thesis/Spatial/LiDAR", layer = "L2A_ALS_overlap_addon")
dim(L2A)
dim(L2A_pts)
L2A_pts$FID<-c((max(L2A$FID)+1):(max(L2A$FID)+length(L2A_pts$sht_n)))

df<-cbind(colnames(L2A@data), colnames(L2A_pts@data))
colnames(L2A@data)<-colnames(L2A_pts@data)
L2A_pts<-spRbind(L2A_pts, L2A)
#writeOGR(L2A_pts,dsn="G:/Thesis/Spatial/LiDAR", layer = "L2A_ALS_overlap_All_wFID", driver = "ESRI Shapefile")

LAS_list<-c("ST3_A01_2018_P01_LAS",
  "ST3_A01_2018_P02_LAS", "ST3_A01_2018_P04_LAS", "ST3_A01_2018_P05_LAS", "ST3_A01_2018_P06_LAS",
  "ST3_A01_2018_P07_LAS", "ST3_A01_2018_P08_LAS","ST3_A01_2018_P09_LAS","ST3_A01_2018_P10_LAS",
  "ST3_A01_2018_P11_LAS", "ST3_A01_2018_P12_LAS", "ST3_A01_2018_P13_LAS", "ST3_A01_2018_P15_LAS",
  "ST3_A01_2018_P18_LAS", "ST3_A01_2018_P20_LAS", "ST3_A01_2018_P22_LAS", "ST3_A01_2018_P24_LAS",
  "ST3_A01_2018_P26_LAS",  "ST3_A01_2018_P27_LAS", "ST3_A01_2018_P28_LAS", "ST3_A01_2018_P29_LAS",
  "ST3_A01_2018_P30_LAS", "ST3_A01_2018_P28_LAS",
  "ANA_A01_2018_LAS", "BON_A01_2018_LAS","FN1_A01_2018_LAS","FN2_A01_2018_LAS",
  "FNA_A01_2018_LAS", "HUM_A01_2018_LAS", "RIB_A01_2018_LAS", 
  "TAL_A01_2018_LAS", "TAP_A02_2018_LAS", 
  "TAP_A03_2018_LAS", "TAP_A04_2018_LAS", "TAP_A05_2018_LAS", "TAP_A06_2018_LAS")
UTMzones<-c("Zone21",
  "Zone21","Zone21","Zone21","Zone21",
  "Zone21","Zone21","Zone21","Zone21",
  "Zone21","Zone21","Zone21","Zone21",
  "Zone21","Zone21","Zone21","Zone21",
  "Zone21","Zone21","Zone21","Zone21",
  "Zone21","Zone21",
  "Zone21","Zone19","Zone21","Zone21",
  "Zone21","Zone19","Zone19",
  "Zone19","Zone21",
  "Zone21","Zone21","Zone21","Zone21")

for (h in 1:length(LAS_list)) {
file_list <- list.files(path=paste0("G:/Thesis/Spatial/LiDAR/Zones/",UTMzones[h],"/",LAS_list[h]), full.names=FALSE)

for (i in 1:length(file_list)) {
  print(file_list[i])
las1 <- readLAS(paste0("G:/Thesis/Spatial/LiDAR/Zones/",UTMzones[h],"/",LAS_list[h],"/",file_list[i]))
sps <- as(extent(las1), 'SpatialPolygons')
proj4string(sps)<-CRS(paste0("+proj=utm +zone=",substr(UTMzones[h],5,6)," +datum=WGS84 +south")) #This is for zone 19 utm projects

L2A_pts_UTM <-spTransform(L2A_pts, crs(sps))
L2A_pts_in<-L2A_pts_UTM[sps, ]

textOut<-cbind(L2A_pts_in@data$FID, L2A_pts_in@coords[,1], L2A_pts_in@coords[,2])
textOut<-as.data.frame(textOut)
textOut$V1<-as.numeric(textOut$V1)
textOut$V2<-as.numeric(textOut$V2)
textOut$V3<-as.numeric(textOut$V3)
#write.table(textOut, file = "G:/Thesis/Spatial/LiDAR/texts/BON_A01_2018_LAS_0.txt", sep = " ", row.names = FALSE, col.names = FALSE)
skip_to_next <- FALSE
tryCatch(wf<-gediWFSimulator(input=paste0("G:/Thesis/Spatial/LiDAR/Zones/",UTMzones[h],"/",LAS_list[h],"/",file_list[i]),
                    output = paste0("G:/Thesis/Spatial/LiDAR/SIM/",substr(file_list[i],1,nchar(file_list[i])-4),"_pt1"),
                    waveID = textOut[1,1],
                    coords = c(textOut[1,2],textOut[1,3])), error = function(e) {skip_to_next<<-TRUE})

if(skip_to_next==FALSE) {
rh<-gediWFMetrics(input= wf, 
                  outRoot = paste0("G:/Thesis/Spatial/LiDAR/RH/Temp/",substr(file_list[i],1,nchar(file_list[i])-4),"_pt1"),
                  rhRes = 1)
print(textOut[1,1])
for (j in 2:length(textOut$V1)) {
skip_2<-FALSE
tryCatch(wf_temp<-gediWFSimulator(input=paste0("G:/Thesis/Spatial/LiDAR/Zones/",UTMzones[h],"/",LAS_list[h],"/",file_list[i]),
                    output = paste0("G:/Thesis/Spatial/LiDAR/SIM/",substr(file_list[i],1,nchar(file_list[i])-4),"_pt",j),
                    waveID = textOut[j,1],
                    coords = c(textOut[j,2],textOut[j,3])), error = function(e) {skip_2<<-TRUE})
if(skip_2==FALSE) {
rh_temp<-gediWFMetrics(input= wf_temp, 
                  outRoot = paste0("G:/Thesis/Spatial/LiDAR/RH/Temp/",substr(file_list[i],1,nchar(file_list[i])-4),"_pt",j),
                  rhRes = 1)
rh<-rbind(rh, rh_temp)
print(textOut[j,1])}
}
write.csv(rh, paste0("G:/Thesis/Spatial/LiDAR/RH/",substr(file_list[i],1,nchar(file_list[i])-4),".csv")) 
}
}
}
