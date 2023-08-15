
#bringing in the data
L2A_pt<-readOGR(dsn="G:/Thesis/Spatial/LiDAR", layer = "L2A_ALS_overlap_All_wFID")

file_list <- list.files(path="G:/Thesis/Spatial/LiDAR/RH/", full.names=FALSE)
path<-"G:/Thesis/Spatial/LiDAR/RH/"

for (i in 1:1){
  RH_sim <- read.csv(paste0(path,file_list[i]))
  print(file_list[i])
  RH_sim$ALS_Area<-print(file_list[i])
}
for (i in 2:length(file_list)){
  temp_data <- read.csv(paste0(path,file_list[i]))
  temp_data$ALS_Area<-print(file_list[i])
  RH_sim <- rbind(RH_sim, temp_data)
  print(file_list[i])
}

L2A_pt_df<-L2A_pt@data

L2A_overlap<-merge(L2A_pt_df, RH_sim, by.x="FID", by.y="wave.ID")

#Defining ALS Regions
L2A_overlap$ALS_Areas<-paste0(substr(L2A_overlap$ALS_Area, 1, (nchar(L2A_overlap$ALS_Area)-6)))
table(L2A_overlap$ALS_Areas)

#Creating codes to compare to file links
L2A_overlap$code<-paste0(L2A_overlap$year,L2A_overlap$day,L2A_overlap$hour,L2A_overlap$min,L2A_overlap$mi)#,"_",
#                           L2A_overlap$orbit,"_",L2A_overlap$track,"_",L2A_overlap$PPDS,"_",L2A_overlap$GOC_S,"_",L2A_overlap$Versn,".h5")

#Braking the data up by projection zones
L2A_overlap_Zone19<-subset(L2A_overlap, ALS_Areas=="BON_A01_2018_LAS" | ALS_Areas=="BON_A01_2018_LAS_" | 
                             ALS_Areas=="HUM_A01_2018_LAS" | ALS_Areas=="HUM_A01_2018_LAS_" | 
                             ALS_Areas=="TAL_A01_2018_LAS")
L2A_overlap_Zone21<-subset(L2A_overlap, ALS_Areas!="BON_A01_2018_LAS" & ALS_Areas!="BON_A01_2018_LAS_" & 
                             ALS_Areas!="HUM_A01_2018_LAS" & ALS_Areas!="HUM_A01_2018_LAS_" & 
                             ALS_Areas!="TAL_A01_2018_LAS")

#Read in file links
Data_list<-read.csv("G:/Thesis/Spatial/LiDAR/Orbits/Full_region_L1B_02_18_22.csv", header = F)

nchar(Data_list[1,1])
nchar(L2A_overlap$code)
Data_list[1,1]

Data_list$File<-substr(Data_list$V1,nchar(Data_list$V1)-47,nchar(Data_list$V1)-35)
Data_list[1,2]
#Find files that overlap ALS pts
Data_list_sub19<-Data_list[Data_list$File %in% L2A_overlap_Zone19$code,]
write.csv(Data_list_sub19[,1],"G:/Thesis/Spatial/LiDAR/dataDownloadALSOrbits_zone19_v2.csv")

Data_list_sub21<-Data_list[Data_list$File %in% L2A_overlap_Zone21$code,]
write.csv(Data_list_sub21[,1],"G:/Thesis/Spatial/LiDAR/dataDownloadALSOrbits_zone21_v2.csv")


# Finding L2B datasets


