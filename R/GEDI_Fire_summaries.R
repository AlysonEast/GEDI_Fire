
M19<-read.csv("G:/Thesis/Data/M19.csv")
hist(M19$pct)
hist(subset(M19, pct!=0 & pct!=100)$pct)
hist(M19$ppm_pre)
hist(subset(M19, ppm_pre!=0)$ppm_pre)
hist(M19$ppm_post)
max(M19$area)
min(M19$area)
sqrt(min(M19$area))
9/min(M19$area)
9/min(M19$area)*1000000

M19$ppkm_pre<-1000000*M19$ppm_pre
M19$ppkm_post<-1000000*M19$ppm_post
M19$areakm<-(M19$area/1000000)
hist(M19$ppkm_post)
min(M19$ppkm_pre)

M19_good<-subset(M19, ppkm_post>30 & ppkm_pre>30)
hist(M19_good$ppkm_pre)
hist(M19_good$ppkm_post)
hist(M19_good$pct)
hist(M19_good$areakm)
hist(M19$areakm)
max(M19_good$areakm)
max(M19$areakm)
M19_good_if<-subset(M19, ppkm_post>30)

#Good
2653
#Good w/ Modeling
27340
#dif
27340-2653

#write.csv(M19_good, "G:/Thesis/Data/M19_good.csv")

M20<-read.csv("G:/Thesis/Data/M20.csv")
hist(M20$pct)
hist(M20$ppm_pre)
hist(M20$ppm_post)

M20$ppkm_pre<-1000000*M20$ppm_pre
M20$ppkm_post<-1000000*M20$ppm_post
M20$areakm<-(M20$area/1000000)
hist(M20$ppkm_post)
max(M20$ppkm_post)


M20_good<-subset(M20, ppkm_post>30 & ppkm_pre>30)
hist(M20_good$ppkm_pre)
hist(M20_good$ppkm_post)
hist(M20_good$pct)
hist(M20_good$area)
M20_good$areakm<-(M20_good$area/1000000)
hist(M20_good$areakm)
hist(M20$areakm)
max(M20_good$areakm)
max(M20$areakm)

#pct of fires good for 2019
2653/85599*100
#pct of fires good for 2020
295/101013*100


write.csv(M20_good, "G:/Thesis/Data/M20_good.csv")

M20_list<-M20_good$M20_FID

test <- M20[M20$M20_FID %in% M20_list,]

Atlas<-read.csv("G:/Thesis/Data/Atlas.csv")
hist(Atlas$pct)
hist(Atlas$pt_per_m_pre)
hist(Atlas$pt_per_m_post)

Atlas_good<-subset(Atlas, pt_per_m_post>30 & pt_per_m_pre>30)
hist(Atlas_good$pt_per_m_pre)
hist(Atlas_good$pt_per_m_post)
hist(Atlas_good$pct)
hist(Atlas_good$size)
hist(Atlas$size)
max(Atlas_good$size)
max(Atlas$size)

664/183192*100

r79<-read.csv("G:/Thesis/Data/MODIS_2019_Region79.csv")
r79_full<-read.csv("G:/Thesis/Data/R79_ptData_v001.csv")
r79_fire<-subset(r79_full, M19_F==169509)
r79_fire_rh<-r79_fire[,c(12:112,140)]
library(reshape)
r79_summary<-melt(r79_fire_rh, id=c("M19_B"))
r79_summary2<-cast(r79_summary, M19_B~variable, mean)
r79_summary3<-melt(r79_summary2, id=c("M19_B"))
r79_summary3$rh<-c(rep(0:100, each = 2))
plot(r79_summary3, value~rh, col=r79_summary3$M19_B)

library(ggplot2)
p<-ggplot(data=r79_summary3, aes(x=rh, y=value, color=M19_B, fill=M19_B), alpha=0.5, position = "identity") +
  geom_bar(stat="identity", alpha=0.1,position="identity") +
  scale_color_manual(values=c("red", "dark green"))  +scale_fill_manual(values=c("red", "dark green")) + theme_bw()
p
ggsave(filename = "Fire169509_rh.jpeg", path = "G:/Thesis/Figures", width = 5, height = 3, dpi = 300)


r79_fire<-subset(r79_full, M19_F==170341)
r79_fire_rh<-r79_fire[,c(12:112,140)]

r79_summary<-melt(r79_fire_rh, id=c("M19_B"))
r79_summary2<-cast(r79_summary, M19_B~variable, mean)
r79_summary3<-melt(r79_summary2, id=c("M19_B"))
r79_summary3$rh<-c(rep(0:100, each = 2))

p<-ggplot(data=r79_summary3, aes(x=rh, y=value, color=M19_B, fill=M19_B), alpha=0.5, position = "identity") +
  geom_bar(stat="identity", alpha=0.1,position="identity") +
  scale_color_manual(values=c("red", "dark green"))  +scale_fill_manual(values=c("red", "dark green")) + theme_bw()
p
ggsave(filename = "Fire170341_rh.jpeg", path = "G:/Thesis/Figures", width = 5, height = 3, dpi = 300)

r79_fire<-subset(r79_full, M19_F==170343)
r79_fire_rh<-r79_fire[,c(12:112,140)]

r79_summary<-melt(r79_fire_rh, id=c("M19_B"))
r79_summary2<-cast(r79_summary, M19_B~variable, mean)
r79_summary3<-melt(r79_summary2, id=c("M19_B"))
r79_summary3$rh<-c(rep(0:100, each = 2))

p<-ggplot(data=r79_summary3, aes(x=rh, y=value, color=M19_B, fill=M19_B), alpha=0.5, position = "identity") +
  geom_bar(stat="identity", alpha=0.1,position="identity") +
  scale_color_manual(values=c("red", "dark green"))  +scale_fill_manual(values=c("red", "dark green")) + theme_bw()
p
ggsave(filename = "Fire170343_rh.jpeg", path = "G:/Thesis/Figures", width = 5, height = 3, dpi = 300)


r79_fire<-subset(r79_full, M19_F==171874)
r79_fire_rh<-r79_fire[,c(12:112,140)]

r79_summary<-melt(r79_fire_rh, id=c("M19_B"))
r79_summary2<-cast(r79_summary, M19_B~variable, mean)
r79_summary3<-melt(r79_summary2, id=c("M19_B"))
r79_summary3$rh<-c(rep(0:100, each = 2))

p<-ggplot(data=r79_summary3, aes(x=rh, y=value, color=M19_B, fill=M19_B), alpha=0.5, position = "identity") +
  geom_bar(stat="identity", alpha=0.1,position="identity") +
  scale_color_manual(values=c("red", "dark green"))  +scale_fill_manual(values=c("red", "dark green")) + theme_bw()
p
ggsave(filename = "Fire171874_rh.jpeg", path = "G:/Thesis/Figures", width = 5, height = 3, dpi = 300)
