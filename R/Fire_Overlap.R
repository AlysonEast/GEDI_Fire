library(sp)
library(raster)
library(RANN)
library(rgdal)
library(rgeos)
library(geosphere)
library(matrixStats)

command_args <- commandArgs(trailingOnly = TRUE)
# Define command line arguments as a variable
n <- command_args[1]
n

L2A<-readOGR(dsn = "/mnt/lustrefs/scratch/v38p156/Fire_Summary_v002/Fire_Regions/Burn_Pts", layer = paste0("L2A_InFire_",n))

table(L2A$M19_F)
L2A_m19<-subset(L2A, M19_F>0)
L2A_pre19<-subset(L2A_m19, M19_B=="Pre-Burn")
L2A_post19<-subset(L2A_m19, M19_B=="Post-Burn")


d <- distm(L2A_pre19, L2A_post19)
df<-as.data.frame(d)
df$min<-rowMins(d)
hist(df$min)
L2A_pre19$dist<-df$min

d <- distm(L2A_post19, L2A_pre19)
df<-as.data.frame(d)
df$min<-rowMins(d)
hist(df$min)
L2A_post19$dist<-df$min

L2A_pre19_overlap<-subset(L2A_pre19, dist<=90)
L2A_post19_overlap<-subset(L2A_post19, dist<=90)

writeOGR()

L2A_m20<-subset(L2A, M20_F>0)
L2A_pre20<-subset(L2A_m20, M20_B=="Pre-Burn")
L2A_post20<-subset(L2A_m20, M20_B=="Post-Burn")

d <- distm(L2A_pre20, L2A_post20)
df<-as.data.frame(d)
df$min<-rowMins(d)
hist(df$min)
L2A_pre20$dist<-df$min

d <- distm(L2A_post20, L2A_pre20)
df<-as.data.frame(d)
df$min<-rowMins(d)
hist(df$min)
L2A_post20$dist<-df$min

L2A_pre20_overlap<-subset(L2A_pre20, dist<=90)
L2A_post20_overlap<-subset(L2A_post20, dist<=90)
writeOGR(dsn="/mnt/lustrefs/scratch/v38p156/Fire_Summary_v002/Fire_Regions/M19_Burn_Overlap", layer = paste0("M19_Burn_",n), 
         driver = "ESRI Shapefile")