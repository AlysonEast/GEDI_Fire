gedi_list_1<-read.csv("G:/GEDI/GEDIfinder/L2B/L2B_list_11_19_2021.csv",header = FALSE)

gedi_list_1$group<-c(rep(152, 18),rep(151:1, each = 50))

gedi_sub<-subset(gedi_list_1, group==1)

for (i in 1:151) {
  gedi_sub[[i]]<-subset(gedi_list_1, group==i)
}

colnames(gedi_sub)<-c(1:151)


for (i in 1:151) {
  write1<-gedi_sub[,i]
  write2<-write1[,1]
  write.table(write2,sep=",", paste0("G:/GEDI/GEDIfinder/L2B/Subsets/gedi_sub", i, ".csv"), row.names = FALSE, col.names = FALSE)
}

gedi_sub152<-subset(gedi_list_1, group==152)
write.table(gedi_sub152[,1],sep=",", paste0("G:/GEDI/GEDIfinder/L2A/Subsets/gedi_sub", 152, ".csv"), row.names = FALSE, col.names = FALSE)
