gedi_list_1<-read.csv("G:/GEDI/GEDIfinder/L2A/gedifinder_L2A_v001_6_8_21.csv",header = FALSE)

gedi_list_1$group<-c(rep(22, 89),rep(21:1, each = 100))

gedi_sub<-subset(gedi_list_1, group==1)

for (i in 1:21) {
  gedi_sub[[i]]<-subset(gedi_list_1, group==i)
}

colnames(gedi_sub)<-c(1:21)

for (i in 1:21) {
  write.csv(gedi_sub[,i], paste0("G:/GEDI/GEDIfinder/L2A/subsets/gedi_sub", i, ".csv"), row.names = FALSE)
}


#Para
gedi_list_1<-read.csv("G:/GEDI/GEDIfinder/L2A/Para_5_3_22.csv",header = FALSE)

gedi_list_1$group<-c(rep(21, 13),rep(20:1, each = 50))

gedi_sub<-subset(gedi_list_1, group==1)

for (i in 1:20) {
  gedi_sub[[i]]<-subset(gedi_list_1, group==i)
}

colnames(gedi_sub)<-c(1:20)

for (i in 1:20) {
  write.table(gedi_sub[,i]$V1, paste0("G:/GEDI/GEDIfinder/L2A/Para_subsets/gedi_sub", i, ".csv"), row.names = FALSE, col.names = FALSE, sep=",")
}

gedi_sub2<-subset(gedi_list_1, group==22)
write.table(gedi_sub2$V1, paste0("G:/GEDI/GEDIfinder/L2A/Para_subsets/gedi_sub", 21, ".csv"), row.names = FALSE, col.names = FALSE, sep=",")
