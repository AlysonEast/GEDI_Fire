gedi_list_1<-read.csv("G:/GEDI/GEDIfinder/L2A/gedifinder_L2A_v002_8_25_21.csv",header = FALSE)

gedi_list_1$group<-c(rep(48, 56),rep(47:1, each = 100))

gedi_sub<-subset(gedi_list_1, group==1)

for (i in 1:47) {
  gedi_sub[[i]]<-subset(gedi_list_1, group==i)
}

colnames(gedi_sub)<-c(1:47)


for (i in 1:47) {
  write1<-gedi_sub[,i]
  write2<-write1[,1]
  write.table(write2,sep=",", paste0("G:/GEDI/GEDIfinder/L2A/subsets_v002/gedi_sub", i, ".csv"), row.names = FALSE, col.names = FALSE)
}

gedi_sub48<-subset(gedi_list_1, group==48)
write.table(gedi_sub48[,1],sep=",", paste0("G:/GEDI/GEDIfinder/L2A/subsets_v002/gedi_sub", 48, ".csv"), row.names = FALSE, col.names = FALSE)
