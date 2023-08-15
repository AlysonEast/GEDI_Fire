
#Run scrip L2A_sim_compare.r firest to get the L2A_compare1 dataset. 

GEDI<-L2A_compare1[,c(1,16:116)]
SIM<-L2A_compare1[,c(1,179:279)]

library(reshape)
GEDI_cast<-melt(GEDI, id="FID")
GEDI_cast$rh<-substr(GEDI_cast$variable,4,6)
GEDI_cast$Data<-"GEDI"
GEDI_cast<-subset(GEDI_cast, rh!="00")

SIM_cast<-melt(SIM, id="FID")
SIM_cast$rh<-substr(SIM_cast$variable,9,11)
SIM_cast$Data<-"SIM"
SIM_cast<-subset(SIM_cast, rh!="100")


full<-rbind(SIM_cast,GEDI_cast)
str(full)
full$rh<-as.numeric(full$rh)

FID_list<-GEDI$FID

for (i in 1:length(FID_list)) {
paired_ment_0<-subset(full, FID==FID_list[i])
p<-ggplot(data=paired_ment_0, aes(x=rh, y=value, color=Data, fill=Data), alpha=0.5, position = "identity") +
  geom_bar(stat="identity", alpha=0.1,position="identity") +
  scale_color_manual(values=c("red", "blue"))  +scale_fill_manual(values=c("red", "blue")) + theme_bw()
p
ggsave(filename = paste0("hist_",FID_list[i],".png"), path = "G:/Thesis/Figures/GEDI_SIM_compare", width = 5, height = 3, dpi = 300)
}
