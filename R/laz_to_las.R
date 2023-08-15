library(sp)
library(raster)
library(rgdal)
library(lidR)
library(ggplot2)
library(hdf5r)
library(devtools)
#devtools::install_git("https://github.com/carlos-alberto-silva/rGEDI", dependencies = TRUE)
library(rGEDI)

#for each in list, read in all data with that name
list<-c("ANA_A01_2018_LAS", "BON_A01_2018_LAS") #,"FN1_A01_2018_LAS","FN2_A01_2018_LAS",
        # "FNA_A01_2018_LAS", "HUM_A01_2018_LAS", "RIB_A01_2018_LAS", "ST3_A01_2018_P01_LAS",
        # "ST3_A01_2018_P02_LAS", "ST3_A01_2018_P04_LAS", "ST3_A01_2018_P05_LAS", "ST3_A01_2018_P06_LAS",
        # "ST3_A01_2018_P07_LAS", "ST3_A01_2018_P08_LAS","ST3_A01_2018_P09_LAS","ST3_A01_2018_P10_LAS",
        # "ST3_A01_2018_P11_LAS", "ST3_A01_2018_P12_LAS", "ST3_A01_2018_P13_LAS", "ST3_A01_2018_P15_LAS",
        #  "ST3_A01_2018_P18_LAS", "ST3_A01_2018_P20_LAS", "ST3_A01_2018_P22_LAS", "ST3_A01_2018_P24_LAS",
        #  "ST3_A01_2018_P26_LAS",  "ST3_A01_2018_P27_LAS", "ST3_A01_2018_P28_LAS", "ST3_A01_2018_P29_LAS",
        #  "ST3_A01_2018_P30_LAS", "ST3_A01_2018_P28_LAS", "TAL_A01_2018_LAS", "TAP_A02_2018_LAS", 
        #  "TAP_A03_2018_LAS", "TAP_A04_2018_LAS", "TAP_A05_2018_LAS", "TAP_A06_2018_LAS") # Order this by region and make region list
UTMzones<-c("Zone21","Zone19")#,#"Zone21","Zone21",
            # "Zone21","Zone19","Zone19","Zone21",
            # "Zone21","Zone21","Zone21","Zone21",
            # "Zone21","Zone21","Zone21","Zone21",
            # "Zone21","Zone21","Zone21","Zone21",
            # "Zone21","Zone21","Zone21","Zone21",
            # "Zone21","Zone21","Zone21","Zone21",
            # "Zone21","Zone21","Zone19","Zone21",
            # "Zone21","Zone21","Zone21","Zone21") #Make a list of zones that matches the order of regions above
            
path<-paste0("G:/Thesis/Spatial/LiDAR/Data/")

for (i in 1:length(list)) {
#create a list of the files from your target directory
  templist <- list.files(path=path, pattern=paste0("\\",list[i]), full.names=FALSE)
  templist
  dir.create(paste0("G:/Thesis/Spatial/LiDAR/Zones/",UTMzones[i],"/",list[i]), showWarnings = FALSE)
for (j in 1:length(templist)){
  lasTemp<-readLAS(paste0("G:/Thesis/Spatial/LiDAR/Data/",templist[j]))
  writeLAS(lasTemp, paste0("G:/Thesis/Spatial/LiDAR/Zones/",UTMzones[i],"/",list[i],"/",substr(templist[j],1,nchar(templist[j])-4),".las"), index = FALSE)
  print(templist[j])
}
    }

#Burn assessment folder ####
path<-paste0("G:/Thesis/Spatial/LiDAR/Burn_Assessment/LAZ/")
templist <- list.files(path=path, full.names=FALSE)

for (j in 1:length(templist)){
  lasTemp<-readLAS(paste0("G:/Thesis/Spatial/LiDAR/Burn_Assessment/LAZ/",templist[j]))
  writeLAS(lasTemp, paste0("G:/Thesis/Spatial/LiDAR/Burn_Assessment/",substr(templist[j],1,nchar(templist[j])-4),".las"), index = FALSE)
  print(templist[j])
}

