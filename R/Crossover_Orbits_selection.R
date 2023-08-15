library(sp)
library(raster)
library(rgdal)
library(ggplot2)
library(devtools)
library(ggpubr)
library(dplyr)


L2A_crossover<-readOGR(dsn="G:/Thesis/Spatial/InFire", layer = "Fire_inROI_Overlap_edit")
L2A_crossover<-read.csv("G:/Thesis/Spatial/InFire/paired_crossovers_list.csv")

orbit_pairs<-as.data.frame(table(L2A_crossover$orb_orb))
set.seed(123)
orbit_pairs_sub<-as.data.frame(sample(orbit_pairs$Var1, 50))

Orbits1<-as.data.frame(substr(orbit_pairs_sub$`sample(orbit_pairs$Var1, 50)`,1,6))
colnames(Orbits1)<-c("orbits")
nchar(paste0(orbit_pairs_sub[1,1]))/2
Orbits2<-as.data.frame(substr(orbit_pairs_sub$`sample(orbit_pairs$Var1, 50)`,22,27))
colnames(Orbits2)<-c("orbits")

Orbits<-rbind(Orbits1, Orbits2)
Orbits_unique<-as.data.frame(table(Orbits$orbits))

Data_list<-read.csv("G:/GEDI/GEDIfinder/L2A/gedifinder_L2A_v002_8_25_21.csv", header = F)

nchar(Data_list[1,1])
Data_list[1,1]
Data_list$orbit<-substr(Data_list$V1,93,98)


Data_list_sub<-Data_list[Data_list$orbit %in% Orbits_unique$Var1,]
Data_list_sub$file<-substr(Data_list_sub$V1,70,123)

write.csv(Data_list_sub[,1],"G:/Thesis/Spatial/Crossovers/dataDownloadForCrossovers.csv")
