library(tidyr)
library(tidyverse)
library(reshape)

rm(list=ls())
fire_points<-readOGR(dsn="G:/Thesis/Spatial/InFire", layer="Fire_inROI_Overlap_edit")
full<-fire_points@data
full<-read.csv("G:/Thesis/Spatial/InFire/Fire_inROI_overlap_edit.csv")
hist(full$dgrd_)
table(full$BEAM)
#full<-subset(full, dgrd_==0)

table(full$CCI19_B)

full$ratio<-(full$rh_98/full$rh_50)

pre<-subset(full, CCI19_B=="Pre-Burn")
post<-subset(full, CCI19_B=="Post-Burn")

hist(table(post$NEAR_FID))
post$duplicate<-duplicated(post$NEAR_FID)
table(post$duplicate)

post_unique<-post %>%
  group_by(NEAR_FID) %>%
  arrange(NEAR_DIST) %>%
  slice(1)
post_unique$duplicate<-duplicated(post_unique$NEAR_FID)
table(post_unique$duplicate)

pre_sub<-pre[is.element(pre$FID, post_unique$NEAR_FID),]

paired<-merge(pre_sub, post_unique, by.x="FID", by.y="NEAR_FID")
hist(table(paired$orb_trac.y), breaks=100)
paired$cross<-duplicated(paired$orb_trac.x)
table(paired$cross)
paired_cross<-subset(paired, cross==TRUE)
hist(table(paired_cross$orb_trac.x), breaks=100)

paired$orb_orb<-paste0(paired$orb_trac.x,"-",paired$orb_trac.y)
crossovers<-as.data.frame(table(paired$orb_orb))
paired_crossovers<-merge(paired, crossovers, by.x="orb_orb", by.y="Var1",all=TRUE)
paired_crossovers_listx<-paired_crossovers[,c(2,1,330)]
paired_crossovers_listy<-paired_crossovers[,c(165,1,330)]
colnames(paired_crossovers_listx)<-colnames(paired_crossovers_listy)
paired_crossovers_list<-rbind(paired_crossovers_listx, paired_crossovers_listy)
write.csv(paired_crossovers_list,"G:/Thesis/Spatial/InFire/paired_crossovers_list.csv")

# Fire_points_add<-merge(fire_points, paired_crossovers_list, by.x="sht_n", by.y="sht_n.y", all.x=FALSE)
# writeOGR(Fire_points_add, dsn="G:/Thesis/Spatial/InFire", layer="Fire_inROI_Overlap_crossover", driver = "ESRI Shapefile")

# paired$SL<-paired$rh_sm.x-paired$rh_sm.y
# paired$RSL<-paired$SL/paired$rh_sm.x
# hist(paired$SL)
# hist(paired$RSL, breaks = 100)
# hist(subset(paired, dist.y<12.5)$SL)
# hist(subset(paired, dist.y<5)$SL)
# hist(subset(paired, dist.y<2)$SL)

# hist(paired$rh_sm.x, ylim = c(0,8000))
# hist(paired$rh_sm.y, add=TRUE, col=alpha("red",0.4), breaks = 25)

# paired$SL2<-paired$ratio.y-paired$ratio.x
# hist(paired$SL2, breaks=100)
# hist(subset(paired, dist.y<12.5)$SL2)
# hist(subset(paired, dist.y<5)$SL2)
# hist(subset(paired, dist.y<2)$SL2)

hist(paired$ratio.y, xlim = c(-250, 300), breaks = 250)
hist(paired$ratio.x, xlim = c(-250, 300), add=TRUE, col=alpha("red",0.4), breaks = 100)

hist(full$rh_50)
hist(pre_unique$rh_50, ylim = c(0,3000))
hist(post_sub$rh_50, add=TRUE, col=alpha("red",0.4))

write.csv(paired,"G:/Thesis/Data/M19_overlap_paired.csv")

goodfire<-read.csv("G:/Thesis/Data/Fire_Sample.csv")
paired_sub<-paired[is.element(paired$FID, goodfire$FID),]

paired_sub<-paired_sub[,c(1,12:112,162:262)]
paired_ment<-melt(paired_sub, id="FID")
#paired_ment$rh<-rep(0:100, each=14477)
paired_ment$rh<-rep(0:100, each=275)
#paired_ment$stat<-rep(c("pre","post"), each=1462177)
paired_ment$stat<-rep(c("pre","post"), each=(55550/2))

list<-paired_ment[c(1:275),]$FID

paired_sub2<-paired[is.element(paired$FID, goodfire$FID),]
paired_sub2<-paired_sub2[,c(1,302)]
paired_ment2<-melt(paired_sub2, id="FID")
#p<-ggplot(data=paired, aes(x=rh_98.x, y=SL, color=dgrd_.x), alpha=0.5, position = "identity") +
#  geom_point(stat="identity",position="identity") + theme_bw()# +scale_color_gradientn(colours = rainbow(5))
#p
library(ggplot2)

for (i in 1:275) {
  paired_ment_0<-subset(paired_ment, FID==list[i])
  p<-ggplot(data=paired_ment_0, aes(x=rh, y=value, color=stat, fill=stat), alpha=0.5, position = "identity") +
    geom_bar(stat="identity", alpha=0.1,position="identity") +
    scale_color_manual(values=c("red", "dark green"))  +scale_fill_manual(values=c("red", "dark green")) + theme_bw() +
    ggtitle(paste0("SL =",paired_ment2[c(i),]$value))
  p
  ggsave(filename = paste0("hist_",list[i],".png"), path = "G:/Thesis/Figures/Test", width = 5, height = 3, dpi = 300)

  }

# down to 30m resolution

full_30<-read.csv("G:/Thesis/Data/M19_overlap_scratch_2.csv")
full_30<-subset(full_30, dgrd_==0)
table(full_30$RASTERVALU)
full_id<-full[,c(1,3)]
full_30<-merge(full_30, full_id, by="sht_n")
full_30<-subset(full_30, RASTERVALU==1)

pre_30<-subset(full_30, CCI19_B=="Pre-Burn")
post_30<-subset(full_30, CCI19_B=="Post-Burn")

hist(table(pre_30$NEAR_DIST))
pre_30$duplicate<-duplicated(pre_30$NEAR_FID)
table(pre_30$duplicate)

pre_unique_30<-pre_30 %>%
  group_by(NEAR_FID) %>%
  arrange(NEAR_DIST) %>%
  slice(1)
pre_unique_30$duplicate<-duplicated(pre_unique_30$NEAR_FID)
table(pre_unique_30$duplicate)

post_sub_30<-post_30[is.element(post_30$FID, pre_unique_30$NEAR_FID),]

paired_30<-merge(post_sub_30, pre_unique_30, by.x="FID", by.y="NEAR_FID")
paired_30$SL<-paired_30$rh_sm.y-paired_30$rh_sm.x
paired_30$RSL<-paired_30$SL/paired_30$rh_sm.y
hist(paired_30$SL)
hist(paired_30$RSL, xlim = c(-2, 2), breaks = 100)
hist(subset(paired_30, dist.y<12.5)$SL)
hist(subset(paired_30, dist.y<5)$SL)

hist(paired_30$rh_sm.y, ylim = c(0,1500))
hist(paired_30$rh_sm.x, add=TRUE, col=alpha("red",0.4))
