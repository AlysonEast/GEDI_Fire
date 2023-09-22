#!/usr/bin/env Rscript
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
library(rgeos)
#Setting up and writing out data for simulation in C on Hyalite####
Tal_b<-readOGR(dsn = "G:/Thesis/Spatial/ALS_Fire", layer = "Talisma_burn_samples")
Tal_nb<-readOGR(dsn = "G:/Thesis/Spatial/ALS_Fire", layer = "Talisma_noburn_samples")
Bon_b<-readOGR(dsn = "G:/Thesis/Spatial/ALS_Fire", layer = "Bonal_burn_samples")
Bon_nb<-readOGR(dsn = "G:/Thesis/Spatial/ALS_Fire", layer = "Bonal_noburn_samples")

RIB_b<-readOGR(dsn = "G:/Thesis/Spatial/ALS_Fire", layer = "RIB_burn_samples")
RIB_nb<-readOGR(dsn = "G:/Thesis/Spatial/ALS_Fire", layer = "RIB_noburn_samples")
Hum_b<-readOGR(dsn = "G:/Thesis/Spatial/ALS_Fire", layer = "Humaita_burn_samples")
Hum_nb<-readOGR(dsn = "G:/Thesis/Spatial/ALS_Fire", layer = "Humaita_noburn_samples")

Tal_b$ROI<-"TAL"
Tal_b$Burned<-1

Tal_nb$ROI<-"TAL"
Tal_nb$Burned<-0

Bon_b$ROI<-"BON"
Bon_b$Burned<-1

Bon_nb$ROI<-"BON"
Bon_nb$Burned<-0

RIB_b$ROI<-"RIB"
RIB_b$Burned<-1

RIB_nb$ROI<-"RIB"
RIB_nb$Burned<-0

Hum_b$ROI<-"HUM"
Hum_b$Burned<-1

Hum_nb$ROI<-"HUM"
Hum_nb$Burned<-0
View(Tal_b@data)
View(Hum_b@data)

#Making samples balanced
# 1: 0-250m, 2: 250-500m, 3: 500-1000m, 4: 1000+m

#Talisma
table(Tal_b$CID)
table(Tal_nb$CID)

Tal_nb_1<-subset(Tal_nb, CID=="1")
set.seed(123)
Tal_nb_1<-Tal_nb_1[sample(nrow(Tal_nb_1), 8), ]
Tal_nb<-rbind(subset(Tal_nb, CID!="1"), Tal_nb_1)
table(Tal_nb$CID)

#Bonal
Bon_b$CID<-as.integer(Bon_b$CID)
Bon_b$CID<-(Bon_b$CID+1)
Bon_b$CID<-as.factor(Bon_b$CID)
Bon_nb$CID<-as.integer(Bon_nb$CID)
Bon_nb$CID<-(Bon_nb$CID+1)
Bon_nb$CID<-as.factor(Bon_nb$CID)

table(Bon_b$CID)
table(Bon_nb$CID)

Bon_nb_3<-subset(Bon_nb, CID=="3")
set.seed(123)
Bon_nb_3<-Bon_nb_3[sample(nrow(Bon_nb_3), 23), ]
Bon_nb<-rbind(subset(Bon_nb, CID!="3"), Bon_nb_3)
table(Bon_nb$CID)
table(Bon_b$CID)

#Rio Branco
RIB_b$CID<-as.integer(RIB_b$CID)
RIB_b$CID<-(RIB_b$CID+2)
RIB_b$CID<-as.factor(RIB_b$CID)
RIB_nb$CID<-as.integer(RIB_nb$CID)
RIB_nb$CID<-(RIB_nb$CID+1)
RIB_nb$CID<-as.factor(RIB_nb$CID)
table(RIB_b$CID)
table(RIB_nb$CID)

RIB_nb_2<-subset(RIB_nb, CID=="2")
set.seed(123)
RIB_nb_2<-RIB_nb_2[sample(nrow(RIB_nb_2), 15), ]
RIB_nb<-rbind(subset(RIB_nb, CID!="1" & CID!="2"), RIB_nb_2)
table(RIB_nb$CID)
table(RIB_b$CID)

#Humaita
Hum_b$CID<-as.integer(Hum_b$CID)
Hum_b$CID<-(Hum_b$CID+1)
Hum_b$CID<-as.factor(Hum_b$CID)
table(Hum_b$CID)
table(Hum_nb$CID)

Hum_nb_1<-subset(Hum_nb, CID=="1")
Hum_nb_3<-subset(Hum_nb, CID=="3")
set.seed(123)
Hum_nb_1<-Hum_nb_1[sample(nrow(Hum_nb_1), 9), ]
set.seed(123)
Hum_nb_3<-Hum_nb_3[sample(nrow(Hum_nb_3), 23), ]
Hum_nb<-rbind(subset(Hum_nb, CID=="2"), Hum_nb_1, Hum_nb_3)
table(Hum_nb$CID)
table(Hum_b$CID)


Samples<-rbind(Tal_b, Tal_nb, Bon_b, Bon_nb, RIB_b, RIB_nb, Hum_b, Hum_nb)
Collo19<-readOGR(dsn="G:/Thesis/Spatial/LiDAR/Shps", layer = "Collocated_Zone19_v2")

#Making samples balanced

Samples_proj<-spTransform(Samples, crs(Collo19))


Samples_df<-as.data.frame(Samples_proj)
#write.csv(Samples_df, "G:/Thesis/Spatial/ALS_Fire/ALS_Samples.csv")
#write.table(Samples_df[,c(4,5)], "G:/Thesis/Spatial/ALS_Fire/ALS_Sample_coords.txt", sep = " ", col.names = FALSE, row.names = FALSE)

table(Bon_b$CID)

#-----------------------------------------------------------------------------------------------------------------#
#################################### WOrk flow now moves into hyalite to handle C program and Simulation Quickly###
#-----------------------------------------------------------------------------------------------------------------#

#Read in simulated data from the points####

RH_sim <- read.delim(file="G:/Thesis/Spatial/LiDAR/SIM_fire/Burn_sim.metric.txt", sep = ",", header = TRUE)
RH_sim_colnames<-colnames(RH_sim)


RH_sim <- read.delim(file="G:/Thesis/Spatial/LiDAR/SIM_fire/Burn_sim.metric.txt", sep = " ")
RH_sim<-RH_sim[,c(1:469)]
colnames(RH_sim)<-RH_sim_colnames


#mering with spdf
Samples$id1<-paste0(round(Samples_df$coords.x1, digits = 6))
Samples$id2<-paste0(round(Samples_df$coords.x2, digits = 6))
Samples$id1_nchar<-nchar(Samples$id1)
Samples$id2_nchar<-nchar(Samples$id2)
table(Samples$id1_nchar)
table(Samples$id2_nchar)

Samples_df2<-Samples@data

Samples$id3<-ifelse(Samples$id1_nchar==13, paste0(Samples$id1), 
                    ifelse(Samples$id1_nchar==12, paste0(Samples$id1,"0"),
                           ifelse(Samples$id1_nchar==11, paste0(Samples$id1,"00"), 
                                  paste0(paste0(Samples$id1,"000")))))
table(nchar(Samples$id3))

table(Samples$id2_nchar)
Samples$id4<-ifelse(Samples$id2_nchar==14, paste0(Samples$id2), 
                    ifelse(Samples$id2_nchar==13, paste0(Samples$id2,"0"),
                           ifelse(Samples$id2_nchar==12, paste0(Samples$id2,"00"), 
                                  paste0(paste0(Samples$id2,"000")))))
table(nchar(Samples$id4))

Samples$id<-paste0(Samples$id3,".",Samples$id4)

table(Samples$id)
table(RH_sim$X..1.wave.ID)
Samples_df<-Samples@data

Samples_full<-merge(Samples, RH_sim, by.x="id", by.y="X..1.wave.ID", all.y=TRUE)
View(Samples_full@data) 
# 612482.740612.8865704.140458 , is 612482.740612.8865704.140459
# 611415.59000.8866152.398017 , 611415.590000.8866152.398017
# 611592.608613.8866800.854142 ,  611592.608613.8866800.854143
# 688271.647978.8908926.315330 , 688271.647978.8908926.315329
# 689044.528959.8908887.333082 not matched.. 689044.528959.8908887.333083
# 656197.948903.8902747.731213
View(RH_sim)

Samples_df["id"][Samples_df["id"] =="612482.740612.8865704.140458"]<-"612482.740612.8865704.140459"
Samples_df["id"][Samples_df["id"] =="611415.59000.8866152.398017"]<-"611415.590000.8866152.398017"
Samples_df["id"][Samples_df["id"] =="611592.608613.8866800.854142"]<-"611592.608613.8866800.854143"
Samples_df["id"][Samples_df["id"] =="688271.647978.8908926.315330"]<-"688271.647978.8908926.315329"
Samples_df["id"][Samples_df["id"] =="689044.528959.8908887.333082"]<-"689044.528959.8908887.333083"
Samples_df["id"][Samples_df["id"] =="656197.948903.8902747.731213"]<-"656197.948903.8902747.731214"
Samples$id<-Samples_df$id
Samples_full<-merge(Samples, RH_sim, by.x="id", by.y="X..1.wave.ID", all.y=TRUE)
View(Samples_full@data)

par(mfrow=c(1,1))
plot(Samples_full, col=Samples_full$Burned)
#writeOGR(Samples_full, dsn="G:/THesis/Spatial/ALS_Fire/Final_Samples", layer = "Stratified_Burn_Samples", driver = "ESRI Shapefile")

#Samples_full<-readOGR(dsn="G:/THesis/Spatial/ALS_Fire/Final_Samples", layer = "Stratified_Burn_Samples")
Talisma<-subset(Samples_full, ROI=="TAL")
Bonal<-subset(Samples_full, ROI=="BON")
RIB<-subset(Samples_full, ROI=="RIB")
Humaita<-subset(Samples_full, ROI=="HUM")

Full<-Samples_full@data

library(ggplot2)
library(reshape)
Talisma_full<-Talisma@data
Talisma_rh<-Talisma@data[,c(23:123)]
Talisma_melt<-melt(Talisma_full[,c(4,23:123)], id="Burned")
Talisma_melt$rh<-rep(0:100, each=nrow(Talisma_full))

plot(Talisma_melt$value~Talisma_melt$rh, pch=21, col=Talisma_melt$Burned)
ggplot(Talisma_melt, aes(y=value, x=rh, col=Burned)) + geom_jitter()

Bonal_full<-Bonal@data
Bonal_melt<-melt(Bonal_full[,c(4,23:123)], id="Burned")
Bonal_melt$rh<-rep(0:100, each=nrow(Bonal_full))

plot(Bonal_melt$value~Bonal_melt$rh, pch=21, col=Bonal_melt$Burned)
ggplot(Bonal_melt, aes(y=value, x=rh, col=Burned)) + geom_jitter()

RIB_full<-RIB@data
RIB_melt<-melt(RIB_full[,c(4,23:123)], id="Burned")
RIB_melt$rh<-rep(0:100, each=nrow(RIB_full))

plot(RIB_melt$value~RIB_melt$rh, pch=21, col=RIB_melt$Burned)
ggplot(RIB_melt, aes(y=value, x=rh, col=Burned)) + geom_jitter()

Humaita_full<-Humaita@data
Humaita_melt<-melt(Humaita_full[,c(4,23:123)], id="Burned")
Humaita_melt$rh<-rep(0:100, each=nrow(Humaita_full))

plot(Humaita_melt$value~Humaita_melt$rh, pch=21, col=Humaita_melt$Burned)
ggplot(Humaita_melt, aes(y=value, x=rh, col=Burned)) + geom_jitter()


library(ggridges)
Bonal_melt$Burned<-as.factor(Bonal_melt$Burned)
Humaita_melt$Burned<-as.factor(Humaita_melt$Burned)
Talisma_melt$Burned<-as.factor(Talisma_melt$Burned)
RIB_melt$Burned<-as.factor(RIB_melt$Burned)

Bonal_melt$ROI<-"Bonal"
Talisma_melt$ROI<-"Talisma"
RIB_melt$ROI<-"RIB"
Humaita_melt$ROI<-"Humaita"

Full_melt<-rbind(Bonal_melt, Talisma_melt, RIB_melt, Humaita_melt)
Full_melt$ROI<-ordered(Full_melt$ROI, levels = c("Bonal", "Talisma", "RIB","Humaita"))

Bonal_melt_trunk<-Bonal_melt
Bonal_melt_trunk[,c("value")][Bonal_melt_trunk[,c("value")]<0]<-0
Talisma_melt_trunk<-Talisma_melt
Talisma_melt_trunk[,c("value")][Talisma_melt_trunk[,c("value")]<0]<-0
RIB_melt_trunk<-RIB_melt
RIB_melt_trunk[,c("value")][RIB_melt_trunk[,c("value")]<0]<-0
Humaita_melt_trunk<-Humaita_melt
Humaita_melt_trunk[,c("value")][Humaita_melt_trunk[,c("value")]<0]<-0

#GAM's for each site####

library(mgcv)
library(visreg)

#Bonal
gam_bon_5<-gam(value~s(rh, by=Burned, k=5)+Burned, data=Bonal_melt_trunk, family = nb)#, method = "REML")
gam_bon_4<-gam(value~s(rh, by=Burned, k=4)+Burned, data=Bonal_melt_trunk, family = nb)#, method = "REML")
gam_bon_3<-gam(value~s(rh, by=Burned, k=3)+Burned, data=Bonal_melt_trunk, family = nb)#, method = "REML")
gam_bon_2<-gam(value~s(rh, by=Burned, k=2)+Burned, data=Bonal_melt_trunk, family = nb)#, method = "REML")

AIC(gam_bon_5, gam_bon_4, gam_bon_3, gam_bon_2)

gam_bon<-gam_bon_5

anova(gam_bon)
gam_bon
summary(gam_bon)
k.check(gam_bon)
par(mfrow=c(1,2))
plot(gam_bon, rug=TRUE)
visreg(gam_bon, xvar = "rh",
       by = "Burned", data = Bonal_melt_trunk)
library(tidymv)
plot_smooths(model = gam_bon,
             series = rh,
             comparison = Burned, 
             transform = exp) + theme_pubr()

gam_bon_data<-as.data.frame(rep(c(0:100), each=2))
colnames(gam_bon_data)<-c("rh")
gam_bon_data$Burned<-rep(c("0","1"), 101)

gam_bon_data$predict<-predict.gam(gam_bon, gam_bon_data, type = "response", se.fit=TRUE)$fit
gam_bon_data$se<-predict.gam(gam_bon, gam_bon_data, type = "response", se.fit=TRUE)$se.fit

gam_bon_data_cast<-cast(gam_bon_data, rh~Burned, value="predict")
gam_bon_data_cast_se<-cast(gam_bon_data, rh~Burned, value = "se")
gam_bon_data_cast$difference<-gam_bon_data_cast$`0`-gam_bon_data_cast$`1`
gam_bon_data_cast$se_burned<-gam_bon_data_cast_se$`1`
gam_bon_data_cast$se_unburned<-gam_bon_data_cast_se$`0`
gam_bon_data_cast$ci_upper_burned<-gam_bon_data_cast$`1`+(2*gam_bon_data_cast$se_burned)
gam_bon_data_cast$ci_lower_burned<-gam_bon_data_cast$`1`-(2*gam_bon_data_cast$se_burned)
gam_bon_data_cast$ci_upper_unburned<-gam_bon_data_cast$`0`+(2*gam_bon_data_cast$se_burned)
gam_bon_data_cast$ci_lower_unburned<-gam_bon_data_cast$`0`-(2*gam_bon_data_cast$se_burned)

par(mfrow=c(1,1))
plot(gam_bon_data_cast$difference~gam_bon_data_cast$rh)

#Talisma
gam_tal_5<-gam(value~s(rh, by=Burned, k=5)+Burned, data=Talisma_melt_trunk, family = nb)#, method = "REML")
gam_tal_4<-gam(value~s(rh, by=Burned, k=4)+Burned, data=Talisma_melt_trunk, family = nb)#, method = "REML")
gam_tal_3<-gam(value~s(rh, by=Burned, k=3)+Burned, data=Talisma_melt_trunk, family = nb)#, method = "REML")
gam_tal_2<-gam(value~s(rh, by=Burned, k=2)+Burned, data=Talisma_melt_trunk, family = nb)#, method = "REML")

AIC(gam_tal_5, gam_tal_4, gam_tal_3, gam_tal_2)

gam_tal<-gam_tal_5

gam_tal
anova(gam_tal)
summary(gam_tal)
k.check(gam_tal)
par(mfrow=c(1,3))
visreg(gam_tal, xvar = "rh",
       by = "Burned", data = Talisma_melt,
       method = "REML")
plot_smooths(model = gam_tal,
             series = rh,
             comparison = Burned, 
             transform = exp) + theme_pubr()

gam_tal_data<-as.data.frame(rep(c(0:100), each=2))
colnames(gam_tal_data)<-c("rh")
gam_tal_data$Burned<-rep(c("0","1"), 101)

gam_tal_data$predict<-predict.gam(gam_tal, gam_tal_data, type = "response", se.fit=TRUE)$fit
gam_tal_data$se<-predict.gam(gam_tal, gam_tal_data, type = "response", se.fit=TRUE)$se.fit

gam_tal_data_cast<-cast(gam_tal_data, rh~Burned, value="predict")
gam_tal_data_cast_se<-cast(gam_tal_data, rh~Burned, value = "se")
gam_tal_data_cast$difference<-gam_tal_data_cast$`0`-gam_tal_data_cast$`1`
gam_tal_data_cast$se_burned<-gam_tal_data_cast_se$`1`
gam_tal_data_cast$se_unburned<-gam_tal_data_cast_se$`0`
gam_tal_data_cast$ci_upper_burned<-gam_tal_data_cast$`1`+(2*gam_tal_data_cast$se_burned)
gam_tal_data_cast$ci_lower_burned<-gam_tal_data_cast$`1`-(2*gam_tal_data_cast$se_burned)
gam_tal_data_cast$ci_upper_unburned<-gam_tal_data_cast$`0`+(2*gam_tal_data_cast$se_burned)
gam_tal_data_cast$ci_lower_unburned<-gam_tal_data_cast$`0`-(2*gam_tal_data_cast$se_burned)

par(mfrow=c(1,1))
plot(gam_tal_data_cast$difference~gam_tal_data_cast$rh)

#RIB
gam_rib_5<-gam(value~s(rh, by=Burned, k=5)+Burned, data=RIB_melt_trunk, family = nb)#, method = "REML")
gam_rib_4<-gam(value~s(rh, by=Burned, k=4)+Burned, data=RIB_melt_trunk, family = nb)#, method = "REML")
gam_rib_3<-gam(value~s(rh, by=Burned, k=3)+Burned, data=RIB_melt_trunk, family = nb)#, method = "REML")
gam_rib_2<-gam(value~s(rh, by=Burned, k=2)+Burned, data=RIB_melt_trunk, family = nb)#, method = "REML")

AIC(gam_rib_5, gam_rib_4, gam_rib_3, gam_rib_2)

gam_rib<-gam_rib_5

gam_rib<-gam(value~s(rh, by=Burned, k=5)+Burned, data=RIB_melt_trunk, family = nb)
gam_rib
anova(gam_rib)
summary(gam_rib)
k.check(gam_rib)
par(mfrow=c(1,3))
plot(gam_rib)
visreg(gam_rib, xvar = "rh",
       by = "Burned", data = RIB_melt,
       method = "REML")
plot_smooths(model = gam_rib,
             series = rh,
             comparison = Burned,
             transform = exp) + theme_pubr()

gam_rib_data<-as.data.frame(rep(c(0:100), each=2))
colnames(gam_rib_data)<-c("rh")
gam_rib_data$Burned<-rep(c("0","1"), 101)

gam_rib_data$predict<-predict.gam(gam_rib, gam_rib_data, type = "response", se.fit=TRUE)$fit
gam_rib_data$se<-predict.gam(gam_rib, gam_rib_data, type = "response", se.fit=TRUE)$se.fit

gam_rib_data_cast<-cast(gam_rib_data, rh~Burned, value="predict")
gam_rib_data_cast_se<-cast(gam_rib_data, rh~Burned, value = "se")
gam_rib_data_cast$difference<-gam_rib_data_cast$`0`-gam_rib_data_cast$`1`
gam_rib_data_cast$se_burned<-gam_rib_data_cast_se$`1`
gam_rib_data_cast$se_unburned<-gam_rib_data_cast_se$`0`
gam_rib_data_cast$ci_upper_burned<-gam_rib_data_cast$`1`+(2*gam_rib_data_cast$se_burned)
gam_rib_data_cast$ci_lower_burned<-gam_rib_data_cast$`1`-(2*gam_rib_data_cast$se_burned)
gam_rib_data_cast$ci_upper_unburned<-gam_rib_data_cast$`0`+(2*gam_rib_data_cast$se_burned)
gam_rib_data_cast$ci_lower_unburned<-gam_rib_data_cast$`0`-(2*gam_rib_data_cast$se_burned)

par(mfrow=c(1,1))
plot(gam_rib_data_cast$difference~gam_rib_data_cast$rh)

#Humaita
gam_hum_5<-gam(value~s(rh, by=Burned, k=5)+Burned, data=Humaita_melt_trunk, family = nb)#, method = "REML")
gam_hum_4<-gam(value~s(rh, by=Burned, k=4)+Burned, data=Humaita_melt_trunk, family = nb)#, method = "REML")
gam_hum_3<-gam(value~s(rh, by=Burned, k=3)+Burned, data=Humaita_melt_trunk, family = nb)#, method = "REML")
gam_hum_2<-gam(value~s(rh, by=Burned, k=2)+Burned, data=Humaita_melt_trunk, family = nb)#, method = "REML")

AIC(gam_hum_5, gam_hum_4, gam_hum_3, gam_hum_2)

gam_hum<-gam_hum_5

gam_hum<-gam(value~s(rh, by=Burned, k=5)+Burned, data=subset(Humaita_melt_trunk, rh<99), family = nb)
gam_hum
anova(gam_hum)
summary(gam_hum)
par(mfrow=c(1,3))
plot(gam_hum)
visreg(gam_hum, xvar = "rh",
       by = "Burned", data = Humaita_melt,
       method = "REML")
plot_smooths(model = gam_hum,
             series = rh,
             comparison = Burned,
             transform = exp) + theme_pubr()

gam_hum_data<-as.data.frame(rep(c(0:100), each=2))
colnames(gam_hum_data)<-c("rh")
gam_hum_data$Burned<-rep(c("0","1"), 101)

gam_hum_data$predict<-predict.gam(gam_hum, gam_hum_data, type = "response", se.fit=TRUE)$fit
gam_hum_data$se<-predict.gam(gam_hum, gam_hum_data, type = "response", se.fit=TRUE)$se.fit

gam_hum_data_cast<-cast(gam_hum_data, rh~Burned, value="predict")
gam_hum_data_cast_se<-cast(gam_hum_data, rh~Burned, value = "se")
gam_hum_data_cast$difference<-gam_hum_data_cast$`0`-gam_hum_data_cast$`1`
gam_hum_data_cast$se_burned<-gam_hum_data_cast_se$`1`
gam_hum_data_cast$se_unburned<-gam_hum_data_cast_se$`0`
gam_hum_data_cast$ci_upper_burned<-gam_hum_data_cast$`1`+(2*gam_hum_data_cast$se_burned)
gam_hum_data_cast$ci_lower_burned<-gam_hum_data_cast$`1`-(2*gam_hum_data_cast$se_burned)
gam_hum_data_cast$ci_upper_unburned<-gam_hum_data_cast$`0`+(2*gam_hum_data_cast$se_burned)
gam_hum_data_cast$ci_lower_unburned<-gam_hum_data_cast$`0`-(2*gam_hum_data_cast$se_burned)



par(mfrow=c(1,1))
plot(gam_hum_data_cast$difference~gam_hum_data_cast$rh)

#putting all of the predictions together
gam_bon_data_cast$ROI<-"Bonal"
gam_tal_data_cast$ROI<-"Talmisma"
gam_rib_data_cast$ROI<-"Rio Branco"
gam_hum_data_cast$ROI<-"Humaita"

gam_predictions<-rbind(gam_bon_data_cast, gam_tal_data_cast, gam_rib_data_cast, gam_hum_data_cast)
gam_predictions$diff_ci_upper<-(gam_predictions$ci_upper_burned-gam_predictions$ci_lower_burned)+gam_predictions$difference
gam_predictions$diff_ci_lower<-(gam_predictions$ci_lower_burned-gam_predictions$ci_upper_burned)+gam_predictions$difference
gam_predictions$ROI<-ordered(gam_predictions$ROI, levels=c("Bonal","Talmisma","Rio Branco","Humaita"))
gam_predictions$sig_diff<-ifelse(gam_predictions$diff_ci_lower<=0, FALSE, TRUE)
# plot of differences

png(file="G:/Thesis/Figures/change.png", width=7, height=7.5, units = "in", res=120) #res in ppi 300dpi = 118 ppi
ggplot(gam_predictions) +
  geom_ribbon(aes(ymin= diff_ci_lower, ymax= diff_ci_upper, y=difference, x=rh, fill=sig_diff), alpha=0.2, show.legend = FALSE) + 
  geom_line(aes(x=rh, y=difference), alpha=1) + 
  # scale_fill_binned(type = "viridis",
  #                        breaks = c(.001, .01, .05, .5),
  #                        limits = c(0, max(Wilcox_canopy$p_value)),
  #                     guide = guide_coloursteps(even.steps = TRUE,
  #                                               show.limits = TRUE)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous (limits = c(0,105), expand = c(0,0), breaks = seq(0,100,25)) +
  ylab("Difference between GAM trends (m)") +
  # scale_color_manual(values = c("#018571", "#a6611a"), labels = c("Unburned", "Burned")) +
  theme_pubr() + facet_wrap(~ROI, ncol = 2) +theme(legend.position = "right")
dev.off()
#Percent Change

gam_predictions$pct_change<-(gam_predictions$difference/gam_predictions$`0`)*100
gam_predictions$pct_change_CI_upper<-(gam_predictions$diff_ci_upper/gam_predictions$`0`)*100
gam_predictions$pct_change_CI_lower<-(gam_predictions$diff_ci_lower/gam_predictions$`0`)*100


png(file="G:/Thesis/Figures/pct_change.png", width=7, height=7.5, units = "in", res=120) #res in ppi 300dpi = 118 ppi
ggplot(gam_predictions) +
  geom_ribbon(aes(ymin= pct_change_CI_lower, ymax= pct_change_CI_upper, y=pct_change, x=rh, fill=sig_diff), alpha=0.2, show.legend = FALSE) + 
  geom_line(aes(x=rh, y=pct_change), alpha=1) + 
  # scale_fill_binned(type = "viridis",
  #                        breaks = c(.001, .01, .05, .5),
  #                        limits = c(0, max(Wilcox_canopy$p_value)),
  #                     guide = guide_coloursteps(even.steps = TRUE,
  #                                               show.limits = TRUE)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous (limits = c(0,105), expand = c(0,0), breaks = seq(0,100,25)) +
  ylab("Percent Change") +
  # scale_color_manual(values = c("#018571", "#a6611a"), labels = c("Unburned", "Burned")) +
  theme_pubr() + facet_wrap(~ROI, ncol = 2) +theme(legend.position = "right")
dev.off()



png(file="G:/Thesis/Figures/GAMS.png", width=7, height=7.5, units = "in", res=120) #res in ppi 300dpi = 118 ppi
ggarrange(
ggplot(data = Bonal_melt_trunk, aes(rh, value)) + 
  geom_jitter(data = Bonal_melt_trunk, aes(rh, value, fill=Burned, color =NA),shape=21, alpha=.3) +
  scale_fill_manual(values = c("#80cdc1", "#dfc27d"), labels = c("0", "1")) +
  geom_ribbon(data= gam_bon_data_cast, aes(ymin=ci_lower_unburned, ymax=ci_upper_unburned, y=`0`, x=rh), fill="#018571", alpha=.5)+ 
  geom_ribbon(data= gam_bon_data_cast, aes(ymin=ci_lower_burned, ymax=ci_upper_burned, y=`1`, x=rh), fill="#a6611a", alpha=.5)+ 
  geom_line(data= gam_bon_data_cast, aes(rh, `0`), color="#018571", size=1)+ 
  geom_line(data= gam_bon_data_cast, aes(rh, `1`), color="#a6611a", size=1)+ 
  scale_y_continuous (limits = c(0,50), expand = c(0,0), breaks = seq(0,50,10)) +
  scale_x_continuous (limits = c(0,105), expand = c(0,0), breaks = seq(0,100,25)) +
  theme_pubr()+ labs(title = "Bonal") +
  scale_color_manual(values = c("#018571", "#a6611a"), labels = c("0", "1")) + 
  labs(col="Burned") + xlab("rh")+ ylab("Height"),
ggplot(data = Talisma_melt_trunk, aes(rh, value)) + 
  geom_jitter(data = Talisma_melt_trunk, aes(rh, value, fill=Burned, color =NA),shape=21, alpha=.2)  +
  scale_fill_manual(values = c("#80cdc1", "#dfc27d"), labels = c("0", "1")) +
  geom_ribbon(data= gam_tal_data_cast, aes(ymin=ci_lower_unburned, ymax=ci_upper_unburned, y=`0`, x=rh), fill="#018571", alpha=.5)+ 
  geom_ribbon(data= gam_tal_data_cast, aes(ymin=ci_lower_burned, ymax=ci_upper_burned, y=`1`, x=rh), fill="#a6611a", alpha=.5)+ 
  geom_line(data= gam_tal_data_cast, aes(rh, `0`), color="#018571", size=1)+ 
  geom_line(data= gam_tal_data_cast, aes(rh, `1`), color="#a6611a", size=1)+ 
  scale_y_continuous (limits = c(0,50), expand = c(0,0), breaks = seq(0,50,10)) +
  scale_x_continuous (limits = c(0,105), expand = c(0,0), breaks = seq(0,100,25)) +
  theme_pubr()+ labs(title = "Talisma") +
  scale_color_manual(values = c("#018571", "#a6611a"), labels = c("0", "1")) + 
  labs(col="Burned") + xlab("rh")+ ylab("Height"),
ggplot(data = RIB_melt_trunk, aes(rh, value)) + 
  geom_jitter(data = RIB_melt_trunk, aes(rh, value, fill=Burned, color =NA),shape=21, alpha=.2)  +
  scale_fill_manual(values = c("#80cdc1", "#dfc27d"), labels = c("0", "1")) +
  geom_ribbon(data= gam_rib_data_cast, aes(ymin=ci_lower_unburned, ymax=ci_upper_unburned, y=`0`, x=rh), fill="#018571", alpha=.5)+ 
  geom_ribbon(data= gam_rib_data_cast, aes(ymin=ci_lower_burned, ymax=ci_upper_burned, y=`1`, x=rh), fill="#a6611a", alpha=.5)+ 
  geom_line(data= gam_rib_data_cast, aes(rh, `0`), color="#018571", size=1)+ 
  geom_line(data= gam_rib_data_cast, aes(rh, `1`), color="#a6611a", size=1)+ 
  scale_y_continuous (limits = c(0,50), expand = c(0,0), breaks = seq(0,50,10)) +
  scale_x_continuous (limits = c(0,105), expand = c(0,0), breaks = seq(0,100,25)) +
  theme_pubr()+ labs(title = "RIB") +
  scale_color_manual(values = c("#018571", "#a6611a"), labels = c("0", "1")) + 
  labs(col="Burned") + xlab("rh")+ ylab("Height"),
ggplot(data = Humaita_melt_trunk, aes(rh, value)) + 
  geom_jitter(data = Humaita_melt_trunk, aes(rh, value, fill=Burned, color =NA),shape=21, alpha=.2)  +
  scale_fill_manual(values = c("#80cdc1", "#dfc27d"), labels = c("0", "1")) +
  geom_ribbon(data= gam_hum_data_cast, aes(ymin=ci_lower_unburned, ymax=ci_upper_unburned, y=`0`, x=rh), fill="#018571", alpha=.5)+ 
  geom_ribbon(data= gam_hum_data_cast, aes(ymin=ci_lower_burned, ymax=ci_upper_burned, y=`1`, x=rh), fill="#a6611a", alpha=.5)+ 
  geom_line(data= gam_hum_data_cast, aes(rh, `0`), color="#018571", size=1)+ 
  geom_line(data= gam_hum_data_cast, aes(rh, `1`), color="#a6611a", size=1)+ 
  scale_y_continuous (limits = c(0,50), expand = c(0,0), breaks = seq(0,50,10)) +
  scale_x_continuous (limits = c(0,105), expand = c(0,0), breaks = seq(0,100,25)) +
  theme_pubr()+ labs(title = "Humaita") +
  scale_color_manual(values = c("#018571", "#a6611a"), labels = c("0", "1")) + 
  labs(col="Burned") + xlab("rh")+ ylab("Height"),
ncol = 2, nrow = 2, common.legend = TRUE)
dev.off()

# Comparing GAM differences to GEDI RMSE
GEDI<-read.csv("G:/Thesis/Spatial/ALS_Fire/L2A_collocated_trunk.csv")
error_summary<-as.data.frame(rep(c(0:100),1))
colnames(error_summary)<-c("rh")
error_summary$RMSE<-0
for (i in 1:101) {
  error_summary[i,2]<-sqrt(mean((GEDI[,c((173+i))] - GEDI[,c((16+i))])^2))
  # rss <- sum((L2A_compareWflags[,c((15+i))]- L2A_compareWflags[,c((173+i))])^ 2)  ## residual sum of squares
  # tss <- sum((L2A_compareWflags[,c((173+i))] - mean(L2A_compareWflags[,c((173+i))])) ^ 2)  ## total sum of squares
  # error_summary2[i,3] <- 1 - rss/tss
  # error_summary2[i,4] <- ((error_summary2[i,2]/mean(L2A_compareWflags[,c((173+i))]))*100)
  # error_summary2[i,5]<-(sum(L2A_compareWflags[,c((173+i))] - L2A_compareWflags[,c((15+i))])/nrow(L2A_compareWflags))
}

png(file="G:/Thesis/Figures/Accuracy_vs_diff.png", width=6, height=5, units = "in", res=120) #res in ppi 300dpi = 118 ppi
ggplot(gam_predictions) +
  geom_ribbon(aes(ymin= diff_ci_lower, ymax= diff_ci_upper, y=difference, x=rh, fill=ROI), alpha=0.2) + 
  geom_line(aes(x=rh, y=difference, col=ROI), alpha=1) + 
  # scale_fill_binned(type = "viridis",
  #                        breaks = c(.001, .01, .05, .5),
  #                        limits = c(0, max(Wilcox_canopy$p_value)),
  #                     guide = guide_coloursteps(even.steps = TRUE,
  #                                               show.limits = TRUE)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous (limits = c(0,105), expand = c(0,0), breaks = seq(0,100,25)) +
  scale_y_continuous(limits = c(-2,8), breaks = seq(-2,8,2),
                     name = "Difference between GAM trends (m)", 
                     sec.axis = sec_axis(trans = ~., name="GEDI  RMSE (m)",
                                         breaks = seq(-2,8,2))) +
  ylab("Difference between GAM trends (m)") +
  geom_line(data= error_summary, aes(x=rh, y=RMSE), size=.75) +
  # geom_point(data= error_summary, aes(x=rh, y=RMSE)) +
  # scale_color_manual(values = c("#018571", "#a6611a"), labels = c("Unburned", "Burned")) +
  theme_pubr() +theme(legend.position = "top", legend.title = element_blank())
dev.off()

#calculateing differences
subset(gam_predictions, ROI=="Bonal")[,"diff_ci_lower"]
plot(error_summary$RMSE-subset(gam_predictions, ROI=="Bonal")[,"diff_ci_lower"])
abline(h=0)
# Plotting Density Plots####
p<-ggplot(subset(Full_melt_trunk, rh==0 | rh==5 | rh==10 |rh==15 |rh==20 |
                rh==25 |rh==30 |rh==35 |rh==40 | rh==45 |rh==50 |
                rh==55 |rh==60 |rh==65 |rh==70 | rh==75 |rh==80 |
                rh==85 |rh==90 |rh==95 |rh==98), aes(y=as.factor(rh), x=value, fill=Burned), alpha=.3) +
  geom_density_ridges(scale=2 , bandwidth = 2, alpha=.4) + #
  scale_y_discrete(expand = c(0, 0), name = "Relative Height") +
  scale_x_continuous(expand = c(0, 0), name = "Height (m)") +
  scale_fill_manual(values = c("#018571", "#a6611a"), labels = c("Unburned", "Burned"),name = "Status") +
  # scale_fill_cyclical(
  #   breaks = c("98 0", "98 1"),
  #   labels = c(`98 0` = "Unburned", `98 1` = "Burned"),
  #   values = c("#018571", "#a6611a", "#80cdc1", "#dfc27d"),
  #   name = "Status", guide = "legend"
  # ) +
  geom_vline(xintercept = 0, linetype ="dashed") +
#  scale_fill_manual(values = c("#018571", "#a6611a"), labels = c("Unburned", "Burned")) + 
  facet_wrap(~ROI, nrow=1) +
  theme_pubr()

# png(file="G:/Thesis/Figures/Density_bySiteAndBurn.png", width=10, height=7, units = "in", res=120) #res in ppi 300dpi = 118 ppi
p
dev.off()

mean_median_sd <- function(x){
  mean_x <- mean(x$nominate_dim2)
  median_x <- median(x$nominate_dim2)
  sd_x <- sd(x$nominate_dim2)
  results <- data.frame(congress=unique(x$congress), party_code=unique(x$party_code),
                        chamber=unique(x$chamber), mean=mean_x, median=median_x, sd=sd_x)
  return(results)
}


full_means<-cast(Full_melt, ROI+Burned~rh, median)
full_sd<-cast(Full_melt, ROI+Burned~rh, sd)
full_means_melt<-melt(full_means, id=c("ROI", "Burned"))
full_sd_melt<-melt(full_sd, id=c("ROI", "Burned"))
colnames(full_sd_melt)<-c("ROI", "Burned", "SD", "rh")
colnames(full_means_melt)<-c("ROI", "Burned", "mean", "rh")


full_summary_melt<-cbind(full_means_melt, full_sd_melt[,"SD"])
colnames(full_summary_melt)<-c("ROI", "Burned", "mean", "rh", "SD")


Diff<-cast(Full_melt, rh+ROI~Burned, mean)
Diff$diff<-Diff$`0`-Diff$`1`

ggplot(full_summary_melt) +
  geom_point(aes(x=rh, y=mean, color=Burned)) + 
  geom_ribbon(aes(ymin= mean - SD, ymax= mean + SD, y=mean, x=rh, fill=Burned, color=Burned), alpha=0.3) + 
  scale_fill_manual(values = c("#018571", "#a6611a"), labels = c("Unburned", "Burned")) + 
  scale_color_manual(values = c("#018571", "#a6611a"), labels = c("Unburned", "Burned")) + 
  #geom_smooth(data=Full_melt, se=FALSE, method = "gam", aes(x=rh, y=mean, color=Burned)) + 
  theme_pubr() + facet_wrap(~ROI)

ggplot(full_summary_melt) +
  geom_point(aes(x=rh, y=SD, color=Burned)) +
  scale_fill_manual(values = c("#018571", "#a6611a"), labels = c("Unburned", "Burned")) + 
  scale_color_manual(values = c("#018571", "#a6611a"), labels = c("Unburned", "Burned")) + 
  #geom_smooth(data=Full_melt, se=FALSE, method = "gam", aes(x=rh, y=mean, color=Burned)) + 
  theme_pubr() + facet_wrap(~ROI)

ggplot(Diff) +
  geom_point(aes(x=rh, y=diff)) +
  ylab("Difference Between Burned and Unburned Mean (m)") +
  #geom_smooth(data=Full_melt, se=FALSE, method = "gam", aes(x=rh, y=mean, color=Burned)) + 
  theme_pubr() + facet_wrap(~ROI)


# library(mgcv)
# m_bonal<-gam(value~s(rh, k=5) + s(rh, by = Burned, k=5), data = Bonal_melt)
# summary(m_bonal)
# par(mfrow=c(1,3))
# plot(m_bonal)
# 
# library(tidymv)
# plot_smooths(model = m_bonal, 
#              series = rh, 
#              comparison = Burned) + theme_pubr() +
#   scale_fill_manual(values = c("#018571", "#a6611a"), labels = c("Unburned", "Burned")) + 
#   scale_color_manual(values = c("#018571", "#a6611a"), labels = c("Unburned", "Burned"))
# 
# m_talisma<-gam(value~s(rh, k=5) + s(rh, by = Burned, k=5), data = Talisma_melt)
# summary(m_talisma)
# par(mfrow=c(1,3))
# plot(m_talisma)
# 
# plot_smooths(model = m_talisma, 
#              series = rh, 
#              comparison = Burned) + theme_pubr() +
#   scale_fill_manual(values = c("#018571", "#a6611a"), labels = c("Unburned", "Burned")) + 
#   scale_color_manual(values = c("#018571", "#a6611a"), labels = c("Unburned", "Burned"))


Wilcox_canopy<-as.data.frame(0)
Wilcox_canopy$p_value<-0
Wilcox_canopy$parameter<-0
Wilcox_canopy$estimate<-0
Wilcox_canopy$lower<-0
Wilcox_canopy$upper<-0
Wilcox_canopy$ROI<-NA

Full$ROI<-as.factor(Full$ROI)
Full_trunk<-Full
Full_trunk[,c(23:123)][Full_trunk[,c(23:123)]<0]<-0
Wilcox_temp<-Wilcox_canopy

for (j in levels(Full_trunk$ROI)) {
  for (i in 1:101) {
    Wilcox_temp[i,1]<-(i-1)
    Wilcox_temp[i,7]<-j
    burn_temp<-subset(Full_trunk, Burned==1 & ROI==j)[,c(23+i)]
    noburn_temp<-subset(Full_trunk, Burned==0 & ROI==j)[,c(23+i)]
    test_temp<-wilcox.test(noburn_temp, burn_temp, alternative = "two.sided", conf.int = TRUE)
    Wilcox_temp[i,2]<-test_temp$p.value
    Wilcox_temp[i,3]<-test_temp$statistic
    Wilcox_temp[i,4]<-test_temp$estimate
    Wilcox_temp[i,5]<-test_temp$conf.int[1]
    Wilcox_temp[i,6]<-test_temp$conf.int[2]
  }
  Wilcox_canopy<-rbind(Wilcox_canopy, Wilcox_temp)
}
Wilcox_canopy<-Wilcox_canopy[-1,]
colnames(Wilcox_canopy)<-c("rh",colnames(Wilcox_canopy)[2:7])
colnames(Wilcox_canopy)
Wilcox_canopy$`p-value`<-ifelse(Wilcox_canopy$p_value<.0001, paste0("***"), 
                          ifelse(Wilcox_canopy$p_value<.001, paste0("**"),
                                 ifelse(Wilcox_canopy$p_value<.01, paste0("*"), 
                                        ifelse(Wilcox_canopy$p_value<.05, paste0("."), paste0(" ")))))
Wilcox_canopy$`p-value`<-ordered(Wilcox_canopy$`p-value`, levels=c(" ",".","*","**","***"))
Wilcox_canopy$ROI<-as.character(Wilcox_canopy$ROI)
levels(Wilcox_canopy$ROI)
Wilcox_canopy$ROI<-ordered(Wilcox_canopy$ROI, levels=c("BON", "TAL", "RIB", "HUM"))


p<- ggplot(Wilcox_canopy) +
  geom_ribbon(aes(ymin= lower, ymax= upper, y=estimate, x=rh), alpha=0.2) + 
  geom_point(aes(x=rh, y=estimate, fill=`p-value`), alpha=1, shape=21, size=3) + 
  # scale_fill_binned(type = "viridis",
  #                        breaks = c(.001, .01, .05, .5),
  #                        limits = c(0, max(Wilcox_canopy$p_value)),
  #                     guide = guide_coloursteps(even.steps = TRUE,
  #                                               show.limits = TRUE)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous (limits = c(0,102), expand = c(0,0), breaks = seq(0,100,25)) +
  ylab("Estimated Difference in Medians (m)") +
  scale_fill_manual(values = c("#ffffcc", "#a1dab4", "#41b6c4", "#2c7fb8","#253494")) +
  # scale_color_manual(values = c("#018571", "#a6611a"), labels = c("Unburned", "Burned")) +
  theme_pubr() + facet_wrap(~ROI, ncol = 2) +theme(legend.position = "right")

png(file="G:/Thesis/Figures/Wilcoxin.png",
    width=8, height=7, units = "in", res=120) #res in ppi 300dpi = 118 ppi
p
dev.off()


Full_melt_trunk<-Full_melt
Full_melt_trunk[,3][Full_melt_trunk[,3]<0]<-0
full_means_trunk<-cast(Full_melt_trunk, ROI+Burned~rh, median)
full_means_trunk_melt<-melt(full_means_trunk, id=c("ROI", "Burned"))

wilcoxon_medians<-subset(full_means_trunk_melt, Burned=="0")
wilcoxon_medians$ROI_code<-rep(c("BON","TAL","RIB","HUM"), (nrow(wilcoxon_medians)/4))
wilcoxon_medians$UID<-paste0(wilcoxon_medians$ROI_code,"_",wilcoxon_medians$rh)

Wilcox_canopy$UID<-paste0(Wilcox_canopy$ROI,"_",Wilcox_canopy$rh)

Wilcox_canopy<-merge(Wilcox_canopy, wilcoxon_medians[,c(3,6)], by="UID")
Wilcox_canopy$pct_chg<-Wilcox_canopy$estimate/Wilcox_canopy$value
Wilcox_canopy$pct_chg_lower<-Wilcox_canopy$lower/Wilcox_canopy$value
Wilcox_canopy$pct_chg_upper<-Wilcox_canopy$upper/Wilcox_canopy$value


Wilcox_canopy[,c(11)][Wilcox_canopy[,c(11)]==Inf]<-0
Wilcox_canopy[,c(11)][Wilcox_canopy[,c(11)]==-Inf]<-0


Wilcox_canopy[,c(12)][Wilcox_canopy[,c(12)]==Inf]<-NA
Wilcox_canopy[,c(12)][Wilcox_canopy[,c(12)]==-Inf]<-NA
Wilcox_canopy[,c(13)][Wilcox_canopy[,c(13)]==Inf]<-NA
Wilcox_canopy[,c(13)][Wilcox_canopy[,c(13)]==-Inf]<-NA
Wilcox_canopy[,c(13)][Wilcox_canopy[,c(13)]>1.5]<-1.5

png(file="G:/Thesis/Figures/Wilcoxin_pctChg.png",
    width=8, height=7, units = "in", res=120) #res in ppi 300dpi = 118 ppi

ggplot(Wilcox_canopy) +
  geom_ribbon(aes(ymin= pct_chg_lower, ymax= pct_chg_upper, y=pct_chg, x=rh), alpha=0.2) + 
  geom_point(aes(x=rh, y=pct_chg, fill=`p-value`), alpha=1, shape=21, size=3) + 
  # scale_fill_binned(type = "viridis",
  #                        breaks = c(.001, .01, .05, .5),
  #                        limits = c(0, max(Wilcox_canopy$p_value)),
  #                     guide = guide_coloursteps(even.steps = TRUE,
  #                                               show.limits = TRUE)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous (limits = c(0,102), expand = c(0,0), breaks = seq(0,100,25)) +
  scale_y_continuous (limits = c(-.25,1.5), expand = c(0,0), breaks = seq(-.25,1.5,.25)) +
  ylab("percent change in Medians") +
  scale_fill_manual(values = c("#ffffcc", "#a1dab4", "#41b6c4", "#2c7fb8","#253494")) +
  # scale_color_manual(values = c("#018571", "#a6611a"), labels = c("Unburned", "Burned")) +
  theme_pubclean() + facet_wrap(~ROI, ncol = 2) +theme(legend.position = "right")
dev.off()
#### rh_sum distribution ####

Bonal_trunk<-Bonal_full
Bonal_trunk[,c(23:123)][Bonal_trunk[,c(23:123)]<0]<-0
Talisma_trunk<-Talisma_full
Talisma_trunk[,c(23:123)][Talisma_trunk[,c(23:123)]<0]<-0
Humaita_trunk<-Humaita_full
Humaita_trunk[,c(23:123)][Humaita_trunk[,c(23:123)]<0]<-0
RIB_trunk<-RIB_full
RIB_trunk[,c(23:123)][RIB_trunk[,c(23:123)]<0]<-0

Bonal_full$rh_sum<-rowSums(Bonal_trunk[,c(23:121)])
Talisma_full$rh_sum<-rowSums(Talisma_trunk[,c(23:121)])
RIB_full$rh_sum<-rowSums(RIB_trunk[,c(23:121)])
Humaita_full$rh_sum<-rowSums(Humaita_trunk[,c(23:121)])

#WIlcoxon test for rh_sum
library(ggpubr)
ggqqplot(Full_w_derrived$rh_sum)
shapiro.test(Full_w_derrived$rh_sum)

bonalsum_wilcox<-wilcox.test(Bonal_full$rh_sum~Bonal_full$Burned, alternative = "two.sided", conf.int = TRUE)
bonalsum_wilcox$p.value
bonalsum_wilcox$estimate
bonalsum_wilcox$conf.int[c(1:2)]

talismasum_wilcox<-wilcox.test(Talisma_full$rh_sum~Talisma_full$Burned, alternative = "two.sided", conf.int = TRUE)
talismasum_wilcox$p.value
talismasum_wilcox$estimate
talismasum_wilcox$conf.int[c(1:2)]

RIBsum_wilcox<-wilcox.test(RIB_full$rh_sum~RIB_full$Burned, alternative = "two.sided", conf.int = TRUE)
RIBsum_wilcox$p.value
RIBsum_wilcox$estimate
RIBsum_wilcox$conf.int[c(1:2)]

humaitasum_wilcox<-wilcox.test(Humaita_full$rh_sum~Humaita_full$Burned, alternative = "two.sided", conf.int = TRUE)
humaitasum_wilcox$p.value
humaitasum_wilcox$estimate
humaitasum_wilcox$conf.int[c(1:2)]

Bonal_full$canopy_ratio<-(Bonal_trunk$X112.rhGauss.98-Bonal_trunk$X39.rhGauss.25)/Bonal_trunk$X112.rhGauss.98
Talisma_full$canopy_ratio<-(Talisma_trunk$X112.rhGauss.98-Talisma_trunk$X39.rhGauss.25)/Talisma_trunk$X112.rhGauss.98
RIB_full$canopy_ratio<-(RIB_trunk$X112.rhGauss.98-RIB_trunk$X39.rhGauss.25)/RIB_trunk$X112.rhGauss.98
Humaita_full$canopy_ratio<-(Humaita_trunk$X112.rhGauss.98-Humaita_trunk$X39.rhGauss.25)/Humaita_trunk$X112.rhGauss.98

Full_w_derrived<-rbind(Bonal_full, Talisma_full, RIB_full, Humaita_full)
Full_w_derrived$Burned<-as.factor(Full_w_derrived$Burned)
Full_w_derrived$ROI<-ordered(Full_w_derrived$ROI, levels=c("BON", "TAL", "RIB", "HUM"))

library(plyr)
mu <- ddply(Full_w_derrived, c("Burned","ROI"), summarise, grp.mean=median(rh_sum))
mu$Burned<-as.factor(mu$Burned)
p<-ggplot(Full_w_derrived, aes(rh_sum, fill=as.factor(Burned))) +
  geom_density(alpha=0.5) + 
  geom_vline(data= mu, aes(xintercept = grp.mean, col=Burned)) +
  scale_color_manual(values = c("#018571", "#a6611a"), labels = c("Unburned", "Burned"), ) +
  scale_fill_manual(values = c("#018571", "#a6611a"), labels = c("Unburned", "Burned"), ) + 
  labs(fill = " ") + labs(col = " ") + xlab(bquote('rh'['sum'])) + 
  scale_x_continuous (limits = c(0,(max(Full_w_derrived$rh_sum)+200)), expand = c(0,0), breaks = seq(0,3000,1000)) +
  theme_pubr() + facet_wrap(~ROI)

#png(file="G:/Thesis/Figures/rh_sumBurn.png",
#    width=5, height=6, units = "in", res=120) #res in ppi 300dpi = 118 ppi
p
dev.off()

mu <- ddply(Full_w_derrived, c("Burned","ROI"), summarise, grp.mean=median(canopy_ratio))
mu$Burned<-as.factor(mu$Burned)
p<-ggplot(Full_w_derrived, aes(canopy_ratio, fill=as.factor(Burned))) +
  geom_density(alpha=0.5) + 
  geom_vline(data= mu, aes(xintercept = grp.mean, col=Burned)) +
  scale_fill_manual(values = c("#018571", "#a6611a"), labels = c("Unburned", "Burned")) + 
  scale_color_manual(values = c("#018571", "#a6611a"), labels = c("Unburned", "Burned"), ) + 
  labs(fill = " ") +labs(col = " ") + xlab("Canopy Ratio") +
  theme_pubr() + facet_wrap(~ROI)

#png(file="G:/Thesis/Figures/Canopy_RatioBurn.png",
#    width=5, height=6, units = "in", res=120) #res in ppi 300dpi = 118 ppi
p
dev.off()


#Plotting rh profiles
SIM<-Full[,c(1,3,4,23:123)]
SIM_cast<-melt(SIM, id=c("id","ROI","Burned"))
SIM_cast$rh<-rep(0:100, each=nrow(SIM))
SIM_cast$rh<-as.numeric(SIM_cast$rh)
SIM_cast$Burned<-as.factor(SIM_cast$Burned)
FID_list<-SIM$id
SIM_cast$ROI<-as.factor(SIM_cast$ROI)

for (i in 1:length(FID_list)) {
  paired_ment_0<-subset(SIM_cast, id==FID_list[i])
  p<-ggplot(data=paired_ment_0, aes(x=rh, y=value, color=Burned, fill=Burned), alpha=0.5, position = "identity") +
    geom_bar(stat="identity", alpha=0.1,position="identity") + theme_bw() +
    labs(title = paste0(paired_ment_0$ROI)) 
  p
  ggsave(filename = paste0(paired_ment_0[2,2],paired_ment_0[2,3],"___",FID_list[i],".png"), path = "G:/Thesis/Figures/Burned_rh", width = 5, height = 3, dpi = 300)
}

ggplot(data=SIM_cast, aes(x=rh, y=value, color=Burned), fill=NA, alpha=0.1, position = "identity") +
  geom_bar(stat="identity", alpha=0.1,position="identity") + theme_bw() + facet_wrap(~ROI+Burned, ncol = 2)

SIM_cast_trunc<-SIM_cast
SIM_cast_trunc[,c("value")][SIM_cast_trunc[,c("value")]<0]<-0

ggplot(data=subset(SIM_cast_trunc, rh<100), aes(x=rh, y=value, color=Burned, group=id), position = "identity") +
  geom_line() + theme_bw() + facet_wrap(~ROI+Burned, ncol = 4)



#Inflection points

library(inflection)

SIM$type<-NA
SIM$index<-NA

SIM$ESE_mean<-NA
SIM$ESE_sd<-NA
SIM$ESE_CIl<-NA
SIM$ESE_CIu<-NA

SIM$EDE_mean<-NA
SIM$EDE_sd<-NA
SIM$EDE_CIl<-NA
SIM$EDE_CIu<-NA

ID_List<-SIM$id
for (i in 1:length(ID_List)) {
  test<-subset(SIM_cast, id==ID_List[i])
  check<-check_curve(test$rh, test$value)
  SIM[i,c("type")]<-check$ctype
  SIM[i,c("index")]<-check$index
  p<-findipiterplot(test$rh, test$value, index = check$index, plots = FALSE, ci = TRUE, doparallel = FALSE)
  # SIM[i,c("ESE_mean")]<-p$aesmout[1]
  # SIM[i,c("ESE_sd")]<-p$aesmout[2]
  # SIM[i,c("ESE_CIl")]<-p$aesmout[3]
  # SIM[i,c("ESE_CIu")]<-p$aesmout[4]
  # 
  # SIM[i,c("EDE_mean")]<-p$aedmout[1]
  # SIM[i,c("EDE_sd")]<-p$aedmout[2]
  # SIM[i,c("EDE_CIl")]<-p$aedmout[3]
  # SIM[i,c("EDE_CIu")]<-p$aedmout[4]
  # r1<-findiplist(test$rh, test$value, index = 1, doparallel = FALSE)
  # SIM[i,c("ESE_1")]<-r1[1,3]
  # SIM[i,c("EDE_1")]<-r1[2,3]
  # r0<-findiplist(test$rh, test$value, index = 0, doparallel = FALSE)
  # SIM[i,c("ESE_0")]<-r0[1,3]
  # SIM[i,c("EDE_0")]<-r0[2,3]
  SIM[i,c("ESE_mean")]<-p$first[1,3]
  SIM[i,c("EDE_mean")]<-p$first[2,3]
  ggplot(data=test, aes(x=rh, y=value), position = "identity") +
    geom_line() + theme_bw() + 
    geom_vline(xintercept = p$first[1,3]) + geom_vline(xintercept = p$first[1,3]) + 
    labs(title = paste0(check$index," ",check$ctype))
  ggsave(filename = paste0(ID_List[i],".png"), path = "G:/Thesis/Figures/Burned_rh", width = 5, height = 3, dpi = 300)
}


SIM[,"EDE_mean"][SIM[,"EDE_mean"]=="NaN"]<-NA

mu <- ddply(SIM, c("Burned","ROI"), summarise, grp.mean=median(ESE_mean))
mu$Burned<-as.factor(mu$Burned)
ggplot(SIM, aes(ESE_mean, fill=as.factor(Burned))) +
  geom_density(alpha=0.5) + 
  geom_vline(data= mu, aes(xintercept = grp.mean, col=Burned)) +
  scale_fill_manual(values = c("#018571", "#a6611a"), labels = c("Unburned", "Burned")) + 
  scale_color_manual(values = c("#018571", "#a6611a"), labels = c("Unburned", "Burned"), ) + 
  labs(fill = " ") +labs(col = " ") + xlab("ESE Inflection Point") +
  theme_pubr() + facet_wrap(~ROI)

Sim_na<-na.omit(SIM)

mu <- ddply(SIM[complete.cases(SIM$EDE_mean),], c("Burned","ROI"), summarise, grp.mean=median(EDE_mean))
mu$Burned<-as.factor(mu$Burned)
ggplot(SIM, aes(EDE_mean, fill=as.factor(Burned))) +
  geom_density(alpha=0.5) + 
  geom_vline(data= mu, aes(xintercept = grp.mean, col=Burned)) +
  scale_fill_manual(values = c("#018571", "#a6611a"), labels = c("Unburned", "Burned")) + 
  scale_color_manual(values = c("#018571", "#a6611a"), labels = c("Unburned", "Burned"), ) + 
  labs(fill = " ") +labs(col = " ") + xlab("EDE Inflection Point") +
  theme_pubr() + facet_wrap(~ROI)


#Disscriminate analysis####
for (i in c(seq(0,90,10))) {
pairs.panels(Full_w_derrived[,c((23+i):(33+i))], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)
}

library(plotly)
Tal_burned_matrix<-subset(Talisma_melt, Burned==1)[,c(3:4)]
Tal_unburned_matrix<-subset(Talisma_melt, Burned==0)[,c(3:4)]

den3d <- kde2d(Tal_burned_matrix$rh, Tal_burned_matrix$value)
den3d_unburned<-kde2d(Tal_unburned_matrix$rh, Tal_unburned_matrix$value)


fig_tal <- plot_ly(showscale = TRUE) %>%
  add_surface(x=den3d$x, y=den3d$y, z=den3d$z, colorscale = list(c(0,1),c("rgb(255,112,184)","rgb(128,0,64)"))) %>%
  add_surface(x=den3d_unburned$x, y=den3d_unburned$y, z=den3d_unburned$z, colorscale = list(c(0,1),c("rgb(107,184,214)","rgb(0,90,124)")))


Bon_burned_matrix<-subset(Bonal_melt, Burned==1)[,c(3:4)]
Bon_unburned_matrix<-subset(Bonal_melt, Burned==0)[,c(3:4)]

den3d <- kde2d(Bon_burned_matrix$rh, Bon_burned_matrix$value)
den3d_unburned<-kde2d(Bon_unburned_matrix$rh, Bon_unburned_matrix$value)

fig_bon <- plot_ly(showscale = TRUE) %>%
  add_surface(x=den3d$x, y=den3d$y, z=den3d$z, colorscale = list(c(0,1),c("rgb(255,112,184)","rgb(128,0,64)"))) %>%
  add_surface(x=den3d_unburned$x, y=den3d_unburned$y, z=den3d_unburned$z, colorscale = list(c(0,1),c("rgb(107,184,214)","rgb(0,90,124)")))

fig_bon

# Split the data into training (80%) and test set (20%)

Talisma_DA<-Talisma@data[,c(4,23:123)]

set.seed(123)
smp_size_Talisma <- floor(0.75 * nrow(Talisma_DA))
train_ind_Talisma <- sample(nrow(Talisma_DA), size = smp_size_Talisma)
train_Talisma.df <- as.data.frame(Talisma_DA[train_ind_Talisma, ])
train_Talisma.df$Burned<-as.factor(train_Talisma.df$Burned)
test_Talisma.df <- as.data.frame(Talisma_DA[-train_ind_Talisma, ])

library(MASS)
library(klaR)
Talisma_LDA<-lda(Burned~., data = train_Talisma.df)
Talisma_QDA<-qda(Burned~., data = train_Talisma.df)
Talisma_RDA<-rda(Burned~., data = train_Talisma.df)

Talisma_LDA
Talisma_RDA

plot(Talisma_LDA$scaling)
plot(Talisma_LDA)

Talisma_LDA_pred<-predict(Talisma_LDA, test_Talisma.df)
mean(Talisma_LDA_pred$class==test_Talisma.df$Burned)

Talisma_RDA_pred<-predict(Talisma_RDA, test_Talisma.df)
mean(Talisma_RDA_pred$class==test_Talisma.df$Burned)

RIB_DA<-RIB@data[,c(4,23:123)]

set.seed(123)
smp_size_RIB <- floor(0.75 * nrow(RIB_DA))
train_ind_RIB <- sample(nrow(RIB_DA), size = smp_size_RIB)
train_RIB.df <- as.data.frame(RIB_DA[train_ind_RIB, ])
test_RIB.df <- as.data.frame(RIB_DA[-train_ind_RIB, ])

RIB_LDA<-lda(Burned~., data = train_RIB.df)
RIB_RDA<-rda(Burned~., data = train_RIB.df)

RIB_LDA
RIB_RDA

plot(RIB_LDA)
plot(RIB_LDA$scaling)

RIB_LDA_pred<-predict(RIB_LDA, test_RIB.df)
mean(RIB_LDA_pred$class==test_RIB.df$Burned)

RIB_RDA_pred<-predict(RIB_RDA, test_RIB.df)
mean(RIB_RDA_pred$class==test_RIB.df$Burned)


Bonal_DA<-Bonal@data[,c(4,23:123)]

set.seed(123)
smp_size_Bonal <- floor(0.75 * nrow(Bonal_DA))
train_ind_Bonal <- sample(nrow(Bonal_DA), size = smp_size_Bonal)
train_Bonal.df <- as.data.frame(Bonal_DA[train_ind_Bonal, ])
test_Bonal.df <- as.data.frame(Bonal_DA[-train_ind_Bonal, ])

Bonal_LDA<-lda(Burned~., data = train_Bonal.df)
Bonal_RDA<-rda(Burned~., data = train_Bonal.df)

Bonal_LDA
Bonal_RDA

plot(Bonal_LDA)
plot(Bonal_LDA$scaling)

Bonal_LDA_pred<-predict(Bonal_LDA, test_Bonal.df)
mean(Bonal_LDA_pred$class==test_Bonal.df$Burned)

Bonal_RDA_pred<-predict(Bonal_RDA, test_Bonal.df)
mean(Bonal_RDA_pred$class==test_Bonal.df$Burned)

#Backup from consol

library(egg)

> for (i in c("BON","TAL","RIB","HUM")) {
  +   temp<-subset(LAI_DF, ROI==i & LAI=="30 to 40")
  +   print(i)
  +   print(wilcox.test(temp$value~temp$Burned, alternative = "two.sided", conf.int = TRUE))
  + }

> for (i in c("BON","TAL","RIB","HUM")) {
  +   temp<-subset(LAI_DF, ROI==i & LAI=="20 to 30")
  +   print(i)
  +   print(wilcox.test(temp$value~temp$Burned, alternative = "two.sided", conf.int = TRUE))
  + }

> for (i in c("BON","TAL","RIB","HUM")) {
  +   temp<-subset(LAI_DF, ROI==i & LAI=="10 to 20")
  +   print(i)
  +   print(wilcox.test(temp$value~temp$Burned, alternative = "two.sided", conf.int = TRUE))
  + }

sig_list<-as.data.frame(cbind(c(" ", " ", " ", " ",
                                +                               "****", "****", " "," ",
                                +                               "****", "*", "*", "**",
                                +                               "**", " ", " ", "*"),
                              +                      rep(c("BON","TAL","RIB","HUM")),
                              +                      rep(c("0 to 10", "10 to 20", "20 to 30", "30 to 40"), each=4)))
> colnames(sig_list)<-c("label","ROI","LAI")
> sig_list$Burned<-0
> sig_list$Burned<-as.factor(sig_list$Burned)

> lai_plot<-ggplot(LAI_DF, aes(value, fill=as.factor(Burned))) +
  +   geom_density(alpha=0.5) + 
  +   geom_vline(data= mu, aes(xintercept = grp.mean, col=Burned)) +
  +   scale_color_manual(values = c("#018571", "#a6611a"), labels = c("Unburned", "Burned"), ) +
  +   scale_fill_manual(values = c("#018571", "#a6611a"), labels = c("Unburned", "Burned"), ) + 
  +   labs(fill = " ") + labs(col = " ") + xlab("LAI") + ylab("density") + 
  +   geom_text(data = sig_list, aes(label=label, x=Inf, y=Inf), hjust =1.5, vjust = 1.5, size=5) +
  +   #  scale_x_continuous (limits = c(0,(max(Full_w_derrived$X455.gLAI0t10))), expand = c(0,0), breaks = seq(0,3000,1000)) +
  +   theme_pubr() + facet_grid2(LAI~ROI, scales = "free", independent = "x")
> lai_plot
