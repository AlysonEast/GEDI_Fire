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

RH_sim <- read.delim(file="G:/Thesis/Spatial/LiDAR/SIM_fire/Burn_sim_LAI_5.metric.txt", sep = ",", header = TRUE)
RH_sim_colnames<-colnames(RH_sim)


RH_sim <- read.delim(file="G:/Thesis/Spatial/LiDAR/SIM_fire/Burn_sim_LAI_5.metric.txt", sep = " ")
RH_sim<-RH_sim[,c(1:514)]
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

#Structuring the data
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

Bonal_full<-Bonal@data
Bonal_melt<-melt(Bonal_full[,c(4,23:123)], id="Burned")
Bonal_melt$rh<-rep(0:100, each=nrow(Bonal_full))

RIB_full<-RIB@data
RIB_melt<-melt(RIB_full[,c(4,23:123)], id="Burned")
RIB_melt$rh<-rep(0:100, each=nrow(RIB_full))

Humaita_full<-Humaita@data
Humaita_melt<-melt(Humaita_full[,c(4,23:123)], id="Burned")
Humaita_melt$rh<-rep(0:100, each=nrow(Humaita_full))

#melt
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

Full_melt_trunk<-rbind(Talisma_melt_trunk, Bonal_melt_trunk, RIB_melt_trunk, Humaita_melt_trunk)
Full_melt_trunk$time<-ifelse(Full_melt_trunk$ROI=="Bonal", paste0("Burned in 2010"),
                             ifelse(Full_melt_trunk$ROI=="Talisma", paste0("Burned in 2010"),
                                    paste0("Burned in 2005")))
Full_melt_trunk$ROI<-as.factor(Full_melt_trunk$ROI)
Full_melt_trunk$ROI<-ordered(Full_melt_trunk$ROI, levels = c("Bonal", "Talisma", "RIB","Humaita"))

Full_w_derrived<-rbind(Bonal_full, Talisma_full, RIB_full, Humaita_full)
Full_w_derrived$Burned<-as.factor(Full_w_derrived$Burned)
Full_w_derrived$ROI<-ordered(Full_w_derrived$ROI, levels=c("BON", "TAL", "RIB", "HUM"))
Full_w_derrived$time<-ifelse(Full_w_derrived$ROI=="BON", paste0("Burned in 2010"),
                             ifelse(Full_w_derrived$ROI=="TAL", paste0("Burned in 2010"),
                                    paste0("Burned in 2005")))

# Leaf Area Index
LAI_DF<-Full_w_derrived[,c(3:4,470:482)] #482
n<-nrow(LAI_DF)
LAI_DF<-melt(LAI_DF, id=c("ROI","Burned"))
LAI_DF$LAI<-rep(c("0 to 5", "5 to 10", "10 to 15", "15 to 20", "20 to 25","25 to 30", "30 to 35",
                  "35 to 40", "40 to 45","45 to 50","50 to 55","55 to 60", "60 to 65"), each=n) #
LAI_DF$ROI<-factor(LAI_DF$ROI, levels=c("BON","TAL","RIB","HUM"))
LAI_DF[,c("value")][LAI_DF[,c("value")]==0]<-NA

LAI_DF$time<-ifelse(LAI_DF$ROI=="BON", paste0("Burned in 2010"),
                    ifelse(LAI_DF$ROI=="TAL", paste0("Burned in 2010"),
                           paste0("Burned in 2005")))
LAI_DF$time<-as.factor(LAI_DF$time)
LAI_DF$ROI<-as.factor(LAI_DF$ROI)
LAI_DF$units<-c("Canopy Height Bins (m)")
LAI_DF$LAI<-ordered(LAI_DF$LAI, levels=c("0 to 5", "5 to 10", "10 to 15", "15 to 20", "20 to 25","25 to 30", "30 to 35",
                                         "35 to 40", "40 to 45","45 to 50","50 to 55","55 to 60", "60 to 65"))

table(LAI_DF$LAI)
table(na.omit(LAI_DF)$LAI)
table(na.omit(LAI_DF)$LAI, na.omit(LAI_DF)$Burned)

table(na.omit(subset(LAI_DF, ROI=="BON"))$LAI, na.omit(subset(LAI_DF, ROI=="BON"))$Burned)
table(na.omit(subset(LAI_DF, ROI=="TAL"))$LAI, na.omit(subset(LAI_DF, ROI=="TAL"))$Burned)
table(na.omit(subset(LAI_DF, ROI=="HUM"))$LAI, na.omit(subset(LAI_DF, ROI=="HUM"))$Burned)
table(na.omit(subset(LAI_DF, ROI=="RIB"))$LAI, na.omit(subset(LAI_DF, ROI=="RIB"))$Burned)



LAI_DF_sub<-subset(LAI_DF, LAI!= "40 to 45" & LAI!="45 to 50" & LAI!="50 to 55" & 
                     LAI!="55 to 60" & LAI!="60 to 65")

library(egg)
Results<-as.data.frame(cbind(rep(c("BON","TAL","RIB","HUM")), 
                             rep(c("0 to 5", "5 to 10", "10 to 15", "15 to 20", "20 to 25","25 to 30", "30 to 35",
                                   "35 to 40", "40 to 45","45 to 50","50 to 55","55 to 60", "60 to 65"), each=4)))
bins<-c("0 to 5", "5 to 10", "10 to 15", "15 to 20", "20 to 25","25 to 30", "30 to 35",
        "35 to 40", "40 to 45","45 to 50","50 to 55","55 to 60", "60 to 65")
sites<-c("BON","TAL","RIB","HUM")

for (h in 1:length(bins)) {
for (i in 1:length(sites)) {
  tryCatch({
    temp<-subset(LAI_DF, ROI==sites[i] & LAI==bins[h])
    store<-wilcox.test(temp$value~temp$Burned, alternative = "two.sided", conf.int = TRUE)
    Results[(((h-1)*4)+i),3]<-store$p.value
    Results[(((h-1)*4)+i),4]<-store$estimate
    Results[(((h-1)*4)+i),5]<-store$conf.int[1]
    Results[(((h-1)*4)+i),6]<-store$conf.int[2]}, error=function(e){print("ERROR")})
}
}

colnames(Results)<-c("ROI","LAI","p-value","estimate","conf_lower","conf_upper")
Results$label<-ifelse(Results$`p-value`<=0.0001, paste0("****"),
                      ifelse(Results$`p-value`<=0.001, paste0("***"),
                             ifelse(Results$`p-value`<=0.01, paste0("**"),
                                    ifelse(Results$`p-value`<=0.05, paste0("*"), paste0("")))))

Results$Burned<-0
Results$Burned<-as.factor(Results$Burned)
Results$time<-ifelse(Results$ROI=="BON", paste0("Burned in 2010"),
                      ifelse(Results$ROI=="TAL", paste0("Burned in 2010"),
                             paste0("Burned in 2005")))
Results$units<-c("Canopy Height Bins (m)")

library(ggh4x)
library(plyr)
sig_list<-Results[c(1:32),]
sig_list$LAI<-ordered(sig_list$LAI, levels=c("0 to 5", "5 to 10", "10 to 15", "15 to 20", "20 to 25",
                                               "25 to 30", "30 to 35","35 to 40"))
mu <- ddply(LAI_DF_sub, c("Burned","ROI","LAI"), summarise, grp.mean=median(value, na.rm = T))
mu$Burned<-as.factor(mu$Burned)
mu$ROI<-factor(mu$ROI, levels=c("BON","TAL","RIB","HUM"))
mu$time<-ifelse(mu$ROI=="BON", paste0("Burned in 2010"),
                ifelse(mu$ROI=="TAL", paste0("Burned in 2010"),
                       paste0("Burned in 2005")))
mu$units<-c("Canopy Height Bins (m)")

LAI_DF_sub$LAI<-ordered(LAI_DF_sub$LAI, levels=c("0 to 5", "5 to 10", "10 to 15", "15 to 20", "20 to 25",
                                                 "25 to 30", "30 to 35","35 to 40"))

lai_plot<-ggplot(LAI_DF_sub, aes(value, fill=as.factor(Burned))) +
  geom_density(alpha=0.5) + 
  geom_vline(data=mu, aes(xintercept = grp.mean, col=Burned)) +
  scale_color_manual(values = c("#018571", "#a6611a"), labels = c("Unburned", "Burned"), ) +
  scale_fill_manual(values = c("#018571", "#a6611a"), labels = c("Unburned", "Burned"), ) +
  labs(fill = " ") + labs(col = " ") +
  xlab(expression("PAI (m"^2*"m"^-2*")")) + ylab("Density") + 
  geom_text(data = sig_list, aes(label=label, x=Inf, y=Inf), hjust =1.5, vjust = 1.5, size=5) +
  #  scale_x_continuous (limits = c(0,(max(Full_w_derrived$X455.gLAI0t10))), expand = c(0,0), breaks = seq(0,3000,1000)) +
  theme_pubr() + facet_nested(LAI~factor(time, levels = c("Burned in 2010","Burned in 2005"))+
                                factor(ROI, levels = c("BON","TAL","RIB","HUM")),
                              scales = "free", independent = "x", as.table = F, drop = TRUE) +
  facetted_pos_scales(x=list(LAI == "30 to 35"~scale_x_continuous(breaks = seq(0,1,0.2)),
                             LAI == "25 to 30"~scale_x_continuous(breaks = seq(0,1,0.2)),
                             LAI == "20 to 25"~scale_x_continuous(breaks = seq(0,1,0.5)),
                             LAI == "15 to 20"~scale_x_continuous(breaks = seq(0,1.5,0.5)),
                             LAI == "10 to 15"~scale_x_continuous(breaks = seq(0,2,1)),
                             LAI == "5 to 10"~scale_x_continuous(breaks = seq(0,4,1)),
                             LAI == "0 to 5"~scale_x_continuous(breaks = seq(0,6,2))),
                      y=list(LAI == "45 to 50"~scale_y_continuous(breaks = seq(0,15000,5000)),
                             LAI == "40 to 45"~scale_y_continuous(breaks = seq(0,2000,1000)),
                             LAI == "35 to 40"~scale_y_continuous(breaks = seq(0,100,50)),
                             LAI == "30 to 35"~scale_y_continuous(breaks = seq(0,8,4)),
                             LAI == "25 to 30"~scale_y_continuous(breaks = seq(0,5,2.5)),
                             LAI == "20 to 25"~scale_y_continuous(breaks = seq(0,3,1)),
                             LAI == "15 to 20"~scale_y_continuous(breaks = seq(0,3,1)),
                             LAI == "10 to 15"~scale_y_continuous(breaks = seq(0,3,1)),
                             LAI == "5 to 10"~scale_y_continuous(breaks = seq(0,1.5,.5)),
                             LAI == "0 to 5"~scale_y_continuous(breaks = seq(0,0.2,.2)))) +
#  theme(plot.margin = unit(c(1,3,1,1), "lines")) +
  theme(axis.title.y.right = element_text("Height Bins (m)")) +
  theme(text = element_text(size = 12))
# png(file="G:/Thesis/Figures/LAI_NA2.png",
#    width=10, height=10, units = "in", res=120) #res in ppi 300dpi = 118 ppi
lai_plot
dev.off()

Results$LAI<-ordered(Results$LAI, levels =c("0 to 5", "5 to 10", "10 to 15", "15 to 20", "20 to 25","25 to 30", "30 to 35",
                                            "35 to 40", "40 to 45","45 to 50","50 to 55","55 to 60", "60 to 65"))
Results<-na.omit(Results)

# png(file="G:/Thesis/Figures/LAI_diff.png",
#     width=11, height=10, units = "in", res=120) #res in ppi 300dpi = 118 ppi
ggplot(data = Results, aes(x=estimate, y=LAI)) + 
  geom_point() + 
  geom_errorbar(aes(xmin=conf_lower, xmax=conf_upper), width=.2,
                position=position_dodge(0.05)) +
  geom_vline(xintercept=0, linetype="dashed") + 
  ylab("Height Bins") +
  xlab("Estimated difference in PAI") +
  facet_nested_wrap(~factor(time, levels = c("Burned in 2010","Burned in 2005"))+
                 factor(ROI, levels = c("BON","TAL","RIB","HUM")),
               scales = "free", drop = TRUE, nrow = 2) + 
  theme_pubr()
#dev.off()

mu_unbruned<-subset(mu, Burned==1)
mu_unbruned$code<-paste0(mu_unbruned$ROI, mu_unbruned$LAI)
Results$code<-paste0(Results$ROI, Results$LAI)

Results_pct<-merge(Results, mu_unbruned[,c(4,7)], by="code")
Results_pct$Percent<-(Results_pct$estimate/Results_pct$grp.mean)
Results_pct$Percent_lowerCI<-(Results_pct$conf_lower/Results_pct$grp.mean)
Results_pct$Percent_upperCI<-(Results_pct$conf_upper/Results_pct$grp.mean)
Results_pct$Percent_upperCI


ggplot(data = Results_pct, aes(x=Percent, y=LAI)) + 
  geom_point() + 
  geom_errorbar(aes(xmin=Percent_lowerCI, xmax=Percent_upperCI), width=.2,
                position=position_dodge(0.05)) +
  geom_vline(xintercept=0, linetype="dashed") + 
  ylab("Height Bins") +
  xlab("Estimated percent change in PAI") +
  xlim(-0.5,3) +
  facet_nested_wrap(~factor(time, levels = c("Burned in 2010","Burned in 2005"))+
                      factor(ROI, levels = c("BON","TAL","RIB","HUM")),
                    scales = "free", drop = TRUE, nrow = 2) + 
  theme_pubr()
