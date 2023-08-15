file_list <- list.files(path="G:/Thesis/Spatial/LiDAR/SIM_geo/Version_3_shifts/", full.names=FALSE)
#file_list<- file_list[3:46]
file_list
path<-"G:/Thesis/Spatial/LiDAR/SIM_geo/Version_3_shifts/"

RH_sim <- read.delim(file=paste0(path,file_list[length(file_list)]), sep = " ", header = FALSE)
min(na.omit(RH_sim$V9))
shift<-subset(RH_sim, RH_sim$V9==min(na.omit(RH_sim$V9)))

for (i in 1:(length(file_list)-1)){
  RH_sim <- read.delim(file=paste0(path,file_list[i]), sep = " ", header = FALSE)
  temp_data<-subset(RH_sim, RH_sim$V9==min(na.omit(RH_sim$V9)))
  ifelse(dim(temp_data)[1]==1, 
         shift <- rbind(temp_data, shift),
         shift <- rbind(c(0,0,0,0,0,0,0,0,0), shift))
  print(file_list[i])
}

shift_formated<-shift[,c(3,5,7,9)]
colnames(shift_formated)<-c("x","y","z","fsig")
str(shift_formated)
shift_formated$x<-as.numeric(shift_formated$x)
shift_formated$y<-as.numeric(shift_formated$y)

hist(shift_formated$z)
shift_formated$dist<-sqrt(((shift_formated$x^2)+(shift_formated$y^2)))
shift_formated$sign<-ifelse(shift_formated$x>0, 1, -1)
shift_formated$dist_sign<-shift_formated$dist*shift_formated$sign
  
hist(shift_formated$dist_sign)
mean(shift_formated$dist_sign)
sd(shift_formated$dist_sign)

hist(shift_formated[-30,]$dist_sign)
mean(shift_formated[-30,]$dist_sign)
sd(shift_formated[-30,]$dist_sign)
