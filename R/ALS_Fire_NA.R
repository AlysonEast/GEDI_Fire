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
Bonal_melt_trunk[,c("value")][Bonal_melt_trunk[,c("value")]<0]<-NA
Talisma_melt_trunk<-Talisma_melt
Talisma_melt_trunk[,c("value")][Talisma_melt_trunk[,c("value")]<0]<-NA
RIB_melt_trunk<-RIB_melt
RIB_melt_trunk[,c("value")][RIB_melt_trunk[,c("value")]<0]<-NA
Humaita_melt_trunk<-Humaita_melt
Humaita_melt_trunk[,c("value")][Humaita_melt_trunk[,c("value")]<0]<-NA

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

#Plotting GAM graphs####
#putting all of the predictions together
gam_bon_data_cast$ROI<-"Bonal"
gam_tal_data_cast$ROI<-"Talisma"
gam_rib_data_cast$ROI<-"Rio Branco"
gam_hum_data_cast$ROI<-"Humaita"

gam_predictions<-rbind(gam_bon_data_cast, gam_tal_data_cast, gam_rib_data_cast, gam_hum_data_cast)
gam_predictions$diff_ci_upper<-(gam_predictions$ci_upper_burned-gam_predictions$ci_lower_burned)+gam_predictions$difference
gam_predictions$diff_ci_lower<-(gam_predictions$ci_lower_burned-gam_predictions$ci_upper_burned)+gam_predictions$difference
gam_predictions$ROI<-ordered(gam_predictions$ROI, levels=c("Bonal","Talisma","Rio Branco","Humaita"))
gam_predictions$sig_diff<-ifelse(gam_predictions$diff_ci_lower<=0, FALSE, TRUE)

gam_predictions[204,14]<-NA
gam_predictions[203,14]<-NA

gam_predictions$time<-ifelse(gam_predictions$ROI=="Bonal", paste0("Burned in 2010"),
                             ifelse(gam_predictions$ROI=="Talisma", paste0("Burned in 2010"),
                                    paste0("Burned in 2005")))

gam_predictions$ROI<-ordered(gam_predictions$ROI, levels=c("Bonal","Talisma","Rio Branco","Humaita"))


# plot of differences
library(ggh4x)
#png(file="G:/Thesis/Figures/change_NA.png", width=7, height=7.5, units = "in", res=120) #res in ppi 300dpi = 118 ppi
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
  xlab(expression("Percent of total Wavefrom Energy"~("RH"['%']))) +
  # scale_color_manual(values = c("#018571", "#a6611a"), labels = c("Unburned", "Burned")) +
  theme_pubr() + facet_nested_wrap(~factor(time, levels = c("Burned in 2010","Burned in 2005"))+ROI, nrow = 2) +
  theme(legend.position = "right")
dev.off()
#Percent Change

gam_predictions$pct_change<-(gam_predictions$difference/gam_predictions$`0`)*100
gam_predictions$pct_change_CI_upper<-(gam_predictions$diff_ci_upper/gam_predictions$`0`)*100
gam_predictions$pct_change_CI_lower<-(gam_predictions$diff_ci_lower/gam_predictions$`0`)*100


#png(file="G:/Thesis/Figures/pct_change_NA.png", width=7, height=7.5, units = "in", res=120) #res in ppi 300dpi = 118 ppi
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
  xlab(expression("Percent of total Wavefrom Energy"~("RH"['%']))) +
  # scale_color_manual(values = c("#018571", "#a6611a"), labels = c("Unburned", "Burned")) +
  theme_pubr() + facet_nested_wrap(~factor(time, levels = c("Burned in 2010","Burned in 2005"))+ROI, nrow = 2) +
  theme(legend.position = "right")
dev.off()



fig<-ggpubr::ggarrange(
  ggplot(data = Bonal_melt_trunk, aes(rh, value)) + 
    geom_jitter(data = Bonal_melt_trunk, aes(rh, value, fill=Burned, color =NA),shape=21, alpha=.3) +
    scale_fill_manual(values = c("#80cdc1", "#dfc27d"), labels = c("Unburned", "Burned")) +
    geom_ribbon(data= gam_bon_data_cast, aes(ymin=ci_lower_unburned, ymax=ci_upper_unburned, y=`0`, x=rh), fill="#018571", alpha=.5)+ 
    geom_ribbon(data= gam_bon_data_cast, aes(ymin=ci_lower_burned, ymax=ci_upper_burned, y=`1`, x=rh), fill="#a6611a", alpha=.5)+ 
    geom_line(data= gam_bon_data_cast, aes(rh, `0`), color="#018571", size=.5)+ 
    geom_line(data= gam_bon_data_cast, aes(rh, `1`), color="#a6611a", size=1)+ 
    scale_y_continuous (limits = c(0,50), expand = c(0,0), breaks = seq(0,50,10)) +
    scale_x_continuous (limits = c(0,105), expand = c(0,0), breaks = seq(0,100,25)) +
    theme_pubr()+ labs(title = "Bonal") + 
    scale_color_manual(values = c("#018571", "#a6611a"), labels = c("Unburned", "Burned")) + 
    labs(col=" ") +labs(fill="") + xlab("")+ ylab(""),
  ggplot(data = Talisma_melt_trunk, aes(rh, value)) + 
    geom_jitter(data = Talisma_melt_trunk, aes(rh, value, fill=Burned, color =NA),shape=21, alpha=.2)  +
    scale_fill_manual(values = c("#80cdc1", "#dfc27d"), labels = c("Unburned", "Burned")) +
    geom_ribbon(data= gam_tal_data_cast, aes(ymin=ci_lower_unburned, ymax=ci_upper_unburned, y=`0`, x=rh), fill="#018571", alpha=.5)+ 
    geom_ribbon(data= gam_tal_data_cast, aes(ymin=ci_lower_burned, ymax=ci_upper_burned, y=`1`, x=rh), fill="#a6611a", alpha=.5)+ 
    geom_line(data= gam_tal_data_cast, aes(rh, `0`), color="#018571", size=1)+ 
    geom_line(data= gam_tal_data_cast, aes(rh, `1`), color="#a6611a", size=1)+ 
    scale_y_continuous (limits = c(0,50), expand = c(0,0), breaks = seq(0,50,10)) +
    scale_x_continuous (limits = c(0,105), expand = c(0,0), breaks = seq(0,100,25)) +
    theme_pubr()+ labs(title = "Talisma") +
    scale_color_manual(values = c("#018571", "#a6611a"), labels = c("Unburned", "Burned")) + 
    labs(col="") +labs(fill="")+ xlab("")+ ylab(""),
  ggplot(data = RIB_melt_trunk, aes(rh, value)) + 
    geom_jitter(data = RIB_melt_trunk, aes(rh, value, fill=Burned, color =NA),shape=21, alpha=.2)  +
    scale_fill_manual(values = c("#80cdc1", "#dfc27d"), labels = c("Unburned", "Burned")) +
    geom_ribbon(data= gam_rib_data_cast, aes(ymin=ci_lower_unburned, ymax=ci_upper_unburned, y=`0`, x=rh), fill="#018571", alpha=.5)+ 
    geom_ribbon(data= gam_rib_data_cast, aes(ymin=ci_lower_burned, ymax=ci_upper_burned, y=`1`, x=rh), fill="#a6611a", alpha=.5)+ 
    geom_line(data= gam_rib_data_cast, aes(rh, `0`), color="#018571", size=1)+ 
    geom_line(data= gam_rib_data_cast, aes(rh, `1`), color="#a6611a", size=1)+ 
    scale_y_continuous (limits = c(0,50), expand = c(0,0), breaks = seq(0,50,10)) +
    scale_x_continuous (limits = c(0,105), expand = c(0,0), breaks = seq(0,100,25)) +
    theme_pubr()+ labs(title = "RIB") +
    scale_color_manual(values = c("#018571", "#a6611a"), labels = c("Unburned", "Burned")) + 
    labs(col="") +labs(fill="") + xlab("")+ ylab(""),
  ggplot(data = Humaita_melt_trunk, aes(rh, value)) + 
    geom_jitter(data = Humaita_melt_trunk, aes(rh, value, fill=Burned, color =NA),shape=21, alpha=.2)  +
    scale_fill_manual(values = c("#80cdc1", "#dfc27d"), labels = c("Unburned", "Burned")) +
    geom_ribbon(data= gam_hum_data_cast, aes(ymin=ci_lower_unburned, ymax=ci_upper_unburned, y=`0`, x=rh), fill="#018571", alpha=.5)+ 
    geom_ribbon(data= gam_hum_data_cast, aes(ymin=ci_lower_burned, ymax=ci_upper_burned, y=`1`, x=rh), fill="#a6611a", alpha=.5)+ 
    geom_line(data= gam_hum_data_cast, aes(rh, `0`), color="#018571", size=1)+ 
    geom_line(data= gam_hum_data_cast, aes(rh, `1`), color="#a6611a", size=1)+ 
    scale_y_continuous (limits = c(0,50), expand = c(0,0), breaks = seq(0,50,10)) +
    scale_x_continuous (limits = c(0,105), expand = c(0,0), breaks = seq(0,100,25)) +
    theme_pubr()+ labs(title = "Humaita") +
    scale_color_manual(values = c("#018571", "#a6611a"), labels = c("Unburned", "Burned")) + 
    labs(col="") +labs(fill="") + xlab("")+ ylab(""),
  ncol = 1, nrow = 4, common.legend = TRUE)

#png(file="G:/Thesis/Figures/GAMS_NA.png", width=7, height=7.5, units = "in", res=120) #res in ppi 300dpi = 118 ppi

annotate_figure(fig, left = text_grob("Height Return (m)", rot = 90, vjust = 2),
                bottom = text_grob(expression("Percent of total Wavefrom Energy"~("rh"['%'])), vjust = -.5))
dev.off()

#### Summarizing GAMS####
subset(gam_predictions, ROI=="Bonal")[,"diff_ci_lower"]
plot(error_summary$RMSE-subset(gam_predictions, ROI=="Bonal")[,"diff_ci_lower"])
abline(h=0)


# Comparing GAM differences to GEDI RMSE and R squared####
GEDI<-read.csv("G:/Thesis/Spatial/ALS_Fire/L2A_collocated_trunk.csv")
error_summary<-as.data.frame(rep(c(0:100),1))
colnames(error_summary)<-c("rh")
error_summary$RMSE<-0
error_summary$rsq<-0
error_summary$Bias<-0
error_summary$MAE<-0

for (i in 1:101) {
  error_summary[i,2]<-sqrt(mean((GEDI[,c((173+i))] - GEDI[,c((16+i))])^2))
  rss <- sum((GEDI[,c((15+i))]- GEDI[,c((173+i))])^ 2)  ## residual sum of squares
  tss <- sum((GEDI[,c((173+i))] - mean(GEDI[,c((173+i))])) ^ 2)  ## total sum of squares
  error_summary[i,3] <- 1 - rss/tss
  # error_summary2[i,4] <- ((error_summary2[i,2]/mean(L2A_compareWflags[,c((173+i))]))*100)
  error_summary[i,4]<-(sum(GEDI[,c((173+i))] - GEDI[,c((16+i))])/nrow(GEDI))
  error_summary[i,5]<-(sum(abs(GEDI[,c((173+i))] - GEDI[,c((16+i))])))/nrow(GEDI)
}
error_summary[,c("rsq")][error_summary[,c("rsq")]==-Inf]<-NA
error_summary$rsq_for_plot<-error_summary$rsq*10
error_summary$Zero<-0
#Thresholds and summary of GAM and Error####
error_plot<-error_summary[,c(1,2,4,5,6)]
error_plot<-melt(error_plot, id="rh")

#png(file="G:/Thesis/Figures/Accuracy_vs_diff_NA.png", width=7, height=5, units = "in", res=120) #res in ppi 300dpi = 118 ppi
dif<-ggplot(gam_predictions) +
  geom_ribbon(aes(ymin= diff_ci_lower, ymax= diff_ci_upper, y=difference, x=rh, fill=ROI), alpha=0.2) + 
  geom_line(aes(x=rh, y=difference, col=ROI), alpha=1) + 
  # geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous (limits = c(0,105), expand = c(0,0), breaks = seq(0,100,25)) +
  scale_y_continuous(limits = c(-2,8), breaks = seq(-2,8,2),
                     name = "Difference between GAM trends (m) \n or \n GEDI RMSE / Bias / MAE (m)", 
                     sec.axis = sec_axis(trans = ~ . /10, name="GEDI  R-squared",
                                         breaks = seq(0,1,0.2))) +
  ylab("Difference between GAM trends (m)")  +
  xlab(expression("Percent of total Wavefrom Energy"~("RH"['%']))) +
  theme_pubr() +theme(legend.position = "top", legend.title = element_blank()) +
  guides(fill = guide_legend(nrow = 2))+
  guides(col = guide_legend(nrow = 2))

dif + geom_line(data= error_plot, aes(x=rh, y=value, linetype=variable, size=variable)) +
  scale_linetype_manual(values=c("solid", "dotted", "dashed", "longdash"), labels = c("RMSE","Absolute Bias","MAE", "R-squared")) +
  scale_size_manual(values=c(.5, .8, .75, .5), labels = c("RMSE","Absolute Bias", "MAE", "R-squared")) +
  guides(linetype = guide_legend(nrow = 2)) 

dev.off()

# Plotting Density Plots####
Full_melt_trunk<-rbind(Talisma_melt_trunk, Bonal_melt_trunk, RIB_melt_trunk, Humaita_melt_trunk)
Full_melt_trunk$time<-ifelse(Full_melt_trunk$ROI=="Bonal", paste0("Burned in 2010"),
                             ifelse(Full_melt_trunk$ROI=="Talisma", paste0("Burned in 2010"),
                                    paste0("Burned in 2005")))
Full_melt_trunk$ROI<-as.factor(Full_melt_trunk$ROI)
Full_melt_trunk$ROI<-ordered(Full_melt_trunk$ROI, levels = c("Bonal", "Talisma", "RIB","Humaita"))

p<-ggplot(subset(Full_melt_trunk, rh==0 | rh==5 | rh==10 |rh==15 |rh==20 |
                   rh==25 |rh==30 |rh==35 |rh==40 | rh==45 |rh==50 |
                   rh==55 |rh==60 |rh==65 |rh==70 | rh==75 |rh==80 |
                   rh==85 |rh==90 |rh==95 |rh==98), aes(y=as.factor(rh), x=value, fill=Burned), alpha=.3) +
  geom_density_ridges(scale=2 , bandwidth = 2, alpha=.4) + #
  scale_y_discrete(expand = c(0, 0), name = expression("Percent of total Wavefrom Energy "~("rh"['%']))) +
  scale_x_continuous(expand = c(0, 0), name = "Height Return (m)") +
  scale_fill_manual(values = c("#018571", "#a6611a"), labels = c("Unburned", "Burned"),name = "Status") +
  # scale_fill_cyclical(
  #   breaks = c("98 0", "98 1"),
  #   labels = c(`98 0` = "Unburned", `98 1` = "Burned"),
  #   values = c("#018571", "#a6611a", "#80cdc1", "#dfc27d"),
  #   name = "Status", guide = "legend"
  # ) +
  geom_vline(xintercept = 0, linetype ="dashed") +
  #  scale_fill_manual(values = c("#018571", "#a6611a"), labels = c("Unburned", "Burned")) + 
  facet_nested_wrap(~factor(time, levels = c("Burned in 2010","Burned in 2005"))+ROI, 
                    ncol = 1,strip.position = "right") +
  theme_pubr() + coord_flip()

#png(file="G:/Thesis/Figures/Density_bySiteAndBurn_rotate.png", width=10, height=8, units = "in", res=120) #res in ppi 300dpi = 118 ppi
p
dev.off()

library(gridExtra)
library(grid)

g1<-ggplot(subset(Full_melt_trunk, rh==0 | rh==5 | rh==10 |rh==15 |rh==20 |
                    rh==25 |rh==30 |rh==35 |rh==40 | rh==45 |rh==50 |
                    rh==55 |rh==60 |rh==65 |rh==70 | rh==75 |rh==80 |
                    rh==85 |rh==90 |rh==95 |rh==98), aes(y=as.factor(rh), x=value, fill=Burned), alpha=.3) +
  geom_density_ridges(scale=2 , bandwidth = 2, alpha=.4) + #
  scale_y_discrete(expand = c(0, 0), name=NULL) +
  scale_x_continuous(expand = c(0, 0), name = NULL) +
  scale_fill_manual(values = c("#018571", "#a6611a"), labels = c("Unburned", "Burned"),name = NULL) +
  geom_vline(xintercept = 0, linetype ="dashed") +
  facet_nested_wrap(~factor(time, levels = c("Burned in 2010","Burned in 2005"))+ROI, 
                    ncol = 1,strip.position = "right") +
  theme_pubr() + coord_flip()
g2<-ggplot(data = Bonal_melt_trunk, aes(rh, value)) + 
  geom_jitter(data = Bonal_melt_trunk, aes(rh, value, fill=Burned, color =NA),shape=21, alpha=.3) +
  scale_fill_manual(values = c("#80cdc1", "#dfc27d"), labels = c("Unburned", "Burned")) +
  geom_ribbon(data= gam_bon_data_cast, aes(ymin=ci_lower_unburned, ymax=ci_upper_unburned, y=`0`, x=rh), fill="#018571", alpha=.5)+ 
  geom_ribbon(data= gam_bon_data_cast, aes(ymin=ci_lower_burned, ymax=ci_upper_burned, y=`1`, x=rh), fill="#a6611a", alpha=.5)+ 
  geom_line(data= gam_bon_data_cast, aes(rh, `0`), color="#018571", size=.5)+ 
  geom_line(data= gam_bon_data_cast, aes(rh, `1`), color="#a6611a", size=1)+ 
  scale_y_continuous (limits = c(0,50), expand = c(0,0), breaks = seq(0,50,10)) +
  scale_x_continuous (limits = c(0,105), expand = c(0,0), breaks = seq(0,100,25)) +
  theme_pubr()+ #labs(title = "Bonal") + 
  scale_color_manual(values = c("#018571", "#a6611a"), labels = c("Unburned", "Burned")) + 
  labs(col=" ") +labs(fill="") + xlab("")+ ylab("")
g3<- ggplot(data = Talisma_melt_trunk, aes(rh, value)) + 
  geom_jitter(data = Talisma_melt_trunk, aes(rh, value, fill=Burned, color =NA),shape=21, alpha=.2)  +
  scale_fill_manual(values = c("#80cdc1", "#dfc27d"), labels = c("Unburned", "Burned")) +
  geom_ribbon(data= gam_tal_data_cast, aes(ymin=ci_lower_unburned, ymax=ci_upper_unburned, y=`0`, x=rh), fill="#018571", alpha=.5)+ 
  geom_ribbon(data= gam_tal_data_cast, aes(ymin=ci_lower_burned, ymax=ci_upper_burned, y=`1`, x=rh), fill="#a6611a", alpha=.5)+ 
  geom_line(data= gam_tal_data_cast, aes(rh, `0`), color="#018571", size=1)+ 
  geom_line(data= gam_tal_data_cast, aes(rh, `1`), color="#a6611a", size=1)+ 
  scale_y_continuous (limits = c(0,50), expand = c(0,0), breaks = seq(0,50,10)) +
  scale_x_continuous (limits = c(0,105), expand = c(0,0), breaks = seq(0,100,25)) +
  theme_pubr()+ #labs(title = "Talisma") +
  scale_color_manual(values = c("#018571", "#a6611a"), labels = c("Unburned", "Burned")) + 
  labs(col="") +labs(fill="")+ xlab("")+ ylab("") +
  theme(legend.position = "none")
g4<-ggplot(data = RIB_melt_trunk, aes(rh, value)) + 
  geom_jitter(data = RIB_melt_trunk, aes(rh, value, fill=Burned, color =NA),shape=21, alpha=.2)  +
  scale_fill_manual(values = c("#80cdc1", "#dfc27d"), labels = c("Unburned", "Burned")) +
  geom_ribbon(data= gam_rib_data_cast, aes(ymin=ci_lower_unburned, ymax=ci_upper_unburned, y=`0`, x=rh), fill="#018571", alpha=.5)+ 
  geom_ribbon(data= gam_rib_data_cast, aes(ymin=ci_lower_burned, ymax=ci_upper_burned, y=`1`, x=rh), fill="#a6611a", alpha=.5)+ 
  geom_line(data= gam_rib_data_cast, aes(rh, `0`), color="#018571", size=1)+ 
  geom_line(data= gam_rib_data_cast, aes(rh, `1`), color="#a6611a", size=1)+ 
  scale_y_continuous (limits = c(0,50), expand = c(0,0), breaks = seq(0,50,10)) +
  scale_x_continuous (limits = c(0,105), expand = c(0,0), breaks = seq(0,100,25)) +
  theme_pubr()+ #labs(title = "RIB") +
  scale_color_manual(values = c("#018571", "#a6611a"), labels = c("Unburned", "Burned")) + 
  labs(col="") +labs(fill="") + xlab("")+ ylab("") + 
  theme(legend.position = "none")
g5<-ggplot(data = Humaita_melt_trunk, aes(rh, value)) + 
  geom_jitter(data = Humaita_melt_trunk, aes(rh, value, fill=Burned, color =NA),shape=21, alpha=.2)  +
  scale_fill_manual(values = c("#80cdc1", "#dfc27d"), labels = c("Unburned", "Burned")) +
  geom_ribbon(data= gam_hum_data_cast, aes(ymin=ci_lower_unburned, ymax=ci_upper_unburned, y=`0`, x=rh), fill="#018571", alpha=.5)+ 
  geom_ribbon(data= gam_hum_data_cast, aes(ymin=ci_lower_burned, ymax=ci_upper_burned, y=`1`, x=rh), fill="#a6611a", alpha=.5)+ 
  geom_line(data= gam_hum_data_cast, aes(rh, `0`), color="#018571", size=1)+ 
  geom_line(data= gam_hum_data_cast, aes(rh, `1`), color="#a6611a", size=1)+ 
  scale_y_continuous (limits = c(0,50), expand = c(0,0), breaks = seq(0,50,10)) +
  scale_x_continuous (limits = c(0,105), expand = c(0,0), breaks = seq(0,100,25)) +
  theme_pubr()+ #labs(title = "Humaita") +
  scale_color_manual(values = c("#018571", "#a6611a"), labels = c("Unburned", "Burned")) + 
  labs(col="") +labs(fill="") + xlab("")+ ylab("") + 
  theme(legend.position = "none")
  # annotate_figure(fig, left = text_grob("Height Return (m)", rot = 90, vjust = 2),
  #                   bottom = text_grob(expression("Percent of total Wavefrom Energy"~("rh"['%'])), vjust = -.5))


biggrid<-ggarrange(g1,
          ggarrange(g2,g3,g4,g5, ncol = 1, common.legend = TRUE),
          ncol = 2, 
          widths = c(2,1),
          common.legend = TRUE)

png(file="G:/Thesis/Figures/Big_rh_grid.png", width=10, height=8, units = "in", res=120) #res in ppi 300dpi = 118 ppi
annotate_figure(biggrid, left = text_grob("Height Return (m)", rot = 90, vjust = .3),
                bottom = text_grob(expression("Percent of total Wavefrom Energy"~("RH"['%'])), vjust = 0.5))
dev.off()                
          
#### rh_sum distribution ####
Bonal_trunk<-Bonal_full
Bonal_trunk[,c(23:123)][Bonal_trunk[,c(23:123)]<0]<-NA
Talisma_trunk<-Talisma_full
Talisma_trunk[,c(23:123)][Talisma_trunk[,c(23:123)]<0]<-NA
Humaita_trunk<-Humaita_full
Humaita_trunk[,c(23:123)][Humaita_trunk[,c(23:123)]<0]<-NA
RIB_trunk<-RIB_full
RIB_trunk[,c(23:123)][RIB_trunk[,c(23:123)]<0]<-NA

Bonal_full$rh_sum<-rowSums(Bonal_trunk[,c(23:121)],na.rm = TRUE)
Talisma_full$rh_sum<-rowSums(Talisma_trunk[,c(23:121)],na.rm = TRUE)
RIB_full$rh_sum<-rowSums(RIB_trunk[,c(23:121)],na.rm = TRUE)
Humaita_full$rh_sum<-rowSums(Humaita_trunk[,c(23:121)],na.rm = TRUE)

Bonal_full$Burned<-as.factor(Bonal_full$Burned)

#WIlcoxon test for rh_sum
library(ggpubr)

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

Full_w_derrived<-rbind(Bonal_full, Talisma_full, RIB_full, Humaita_full)
Full_w_derrived$Burned<-as.factor(Full_w_derrived$Burned)
Full_w_derrived$ROI<-ordered(Full_w_derrived$ROI, levels=c("BON", "TAL", "RIB", "HUM"))
Full_w_derrived$time<-ifelse(Full_w_derrived$ROI=="BON", paste0("Burned in 2010"),
                             ifelse(Full_w_derrived$ROI=="TAL", paste0("Burned in 2010"),
                                    paste0("Burned in 2005")))

library(plyr)
mu <- ddply(Full_w_derrived, c("Burned","ROI"), summarise, grp.mean=median(rh_sum))
mu$Burned<-as.factor(mu$Burned)
mu$time<-ifelse(mu$ROI=="BON", paste0("Burned in 2010"),
                ifelse(mu$ROI=="TAL", paste0("Burned in 2010"),
                       paste0("Burned in 2005")))

p<-ggplot(Full_w_derrived, aes(rh_sum, fill=as.factor(Burned))) +
  geom_density(alpha=0.5) + 
  geom_vline(data= mu, aes(xintercept = grp.mean, col=Burned)) +
  scale_color_manual(values = c("#018571", "#a6611a"), labels = c("Unburned", "Burned"), ) +
  scale_fill_manual(values = c("#018571", "#a6611a"), labels = c("Unburned", "Burned"), ) + 
  labs(fill = " ") + labs(col = " ") + xlab(bquote('rh'['sum'])) + ylab("Density") +
  scale_x_continuous (limits = c(0,(max(Full_w_derrived$rh_sum)+200)), expand = c(0,0), breaks = seq(0,3000,1000)) +
  theme_pubr() + facet_nested_wrap(~factor(time, levels = c("Burned in 2010","Burned in 2005"))+ROI, nrow = 2)

# png(file="G:/Thesis/Figures/rh_sumBurn_NA.png",
#     width=5, height=6, units = "in", res=120) #res in ppi 300dpi = 118 ppi
p
dev.off()

# Leaf Area Index
LAI_DF<-Full_w_derrived[,c(3:4,461:464)]
n<-nrow(LAI_DF)
LAI_DF<-melt(LAI_DF, id=c("ROI","Burned"))
LAI_DF$LAI<-rep(c("0 to 10", "10 to 20", "20 to 30", "30 to 40"), each=n)
LAI_DF$ROI<-factor(LAI_DF$ROI, levels=c("BON","TAL","RIB","HUM"))

library(egg)
for (i in c("BON","TAL","RIB","HUM")) {
  temp<-subset(LAI_DF, ROI==i & LAI=="30 to 40")
  print(i)
  print(wilcox.test(temp$value~temp$Burned, alternative = "two.sided", conf.int = TRUE))
  }

for (i in c("BON","TAL","RIB","HUM")) {
  temp<-subset(LAI_DF, ROI==i & LAI=="20 to 30")
  print(i)
  print(wilcox.test(temp$value~temp$Burned, alternative = "two.sided", conf.int = TRUE))
  }

for (i in c("BON","TAL","RIB","HUM")) {
  temp<-subset(LAI_DF, ROI==i & LAI=="10 to 20")
  print(i)
  print(wilcox.test(temp$value~temp$Burned, alternative = "two.sided", conf.int = TRUE))
}

for (i in c("BON","TAL","RIB","HUM")) {
  temp<-subset(LAI_DF, ROI==i & LAI=="0 to 10")
  print(i)
  print(wilcox.test(temp$value~temp$Burned, alternative = "two.sided", conf.int = TRUE))
}

sig_list<-as.data.frame(cbind(c(" ", " ", " ", " ",
                               "****", "****", " "," ",
                               "****", "*", "*", "**",
                               "**", " ", " ", "*"),
                               rep(c("BON","TAL","RIB","HUM")),
                               rep(c("0 to 10", "10 to 20", "20 to 30", "30 to 40"), each=4)))
colnames(sig_list)<-c("label","ROI","LAI")
sig_list$Burned<-0
sig_list$Burned<-as.factor(sig_list$Burned)

scale_list<-as.data.frame(cbind(c(" ", " ", " ", " ",
                                "****", "****", " "," ",
                                "****", "*", "*", "**",
                                "**", " ", " ", "*"),
                              rep(c("BON","TAL","RIB","HUM")),
                              rep(c("0 to 10", "10 to 20", "20 to 30", "30 to 40"), each=4)))
colnames(sig_list)<-c("label","ROI","LAI")
sig_list$Burned<-0
sig_list$Burned<-as.factor(sig_list$Burned)

library(ggh4x)
mu <- ddply(LAI_DF, c("Burned","ROI","LAI"), summarise, grp.mean=median(value))
mu$Burned<-as.factor(mu$Burned)
mu$ROI<-factor(mu$ROI, levels=c("BON","TAL","RIB","HUM"))

LAI_DF$time<-ifelse(LAI_DF$ROI=="BON", paste0("Burned in 2010"),
                    ifelse(LAI_DF$ROI=="TAL", paste0("Burned in 2010"),
                           paste0("Burned in 2005")))
LAI_DF$time<-as.factor(LAI_DF$time)
LAI_DF$ROI<-as.factor(LAI_DF$ROI)
LAI_DF$units<-c("Canopy Height Bins (m)")

sig_list$time<-ifelse(sig_list$ROI=="BON", paste0("Burned in 2010"),
                      ifelse(sig_list$ROI=="TAL", paste0("Burned in 2010"),
                             paste0("Burned in 2005")))
sig_list$units<-c("Canopy Height Bins (m)")
mu$time<-ifelse(mu$ROI=="BON", paste0("Burned in 2010"),
                ifelse(mu$ROI=="TAL", paste0("Burned in 2010"),
                       paste0("Burned in 2005")))
mu$units<-c("Canopy Height Bins (m)")

lai_plot<-ggplot(LAI_DF, aes(value, fill=as.factor(Burned))) +
  geom_density(alpha=0.5) + 
 geom_vline(data=mu, aes(xintercept = grp.mean, col=Burned)) +
 scale_color_manual(values = c("#018571", "#a6611a"), labels = c("Unburned", "Burned"), ) +
 scale_fill_manual(values = c("#018571", "#a6611a"), labels = c("Unburned", "Burned"), ) +
 labs(fill = " ") + labs(col = " ") +
  xlab(expression("PAI (m"^2*"m"^-2*")")) + ylab("Density") + 
  geom_text(data = sig_list, aes(label=label, x=Inf, y=Inf), hjust =1.5, vjust = 1.5, size=5) +
  #  scale_x_continuous (limits = c(0,(max(Full_w_derrived$X455.gLAI0t10))), expand = c(0,0), breaks = seq(0,3000,1000)) +
  theme_pubr() + facet_nested(units+LAI~factor(time, levels = c("Burned in 2010","Burned in 2005"))+
                                 factor(ROI, levels = c("BON","TAL","RIB","HUM")), 
                             scales = "free", independent = "x", as.table = F, drop = TRUE) +
  facetted_pos_scales(x=list(LAI == "30 to 40"~scale_x_continuous(breaks = seq(0,2,1)), 
                             LAI == "20 to 30"~scale_x_continuous(breaks = seq(0,2,1)),
                             LAI == "10 to 20"~scale_x_continuous(breaks = seq(0,3,1)),
                             LAI == "0 to 10"~scale_x_continuous(breaks = seq(0,10,2))),
                      y=list(LAI == "30 to 40"~scale_y_continuous(breaks = seq(0,10,5)), 
                             LAI == "20 to 30"~scale_y_continuous(breaks = seq(0,2,1)),
                             LAI == "10 to 20"~scale_y_continuous(breaks = seq(0,1,0.5)),
                             LAI == "0 to 10"~scale_y_continuous(breaks = seq(0,0.3,0.1)))) +
  theme(plot.margin = unit(c(1,3,1,1), "lines")) +
  theme(axis.title.y.right = element_text("Height Bins (m)")) +
  theme(text = element_text(size = 20))

# png(file="G:/Thesis/Figures/LAI_NA2.png",
#         width=10, height=10, units = "in", res=120) #res in ppi 300dpi = 118 ppi
lai_plot
dev.off()

