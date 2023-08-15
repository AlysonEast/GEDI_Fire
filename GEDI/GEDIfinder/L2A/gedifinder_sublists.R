gedi_list_1<-read.csv("G:/GEDI/GEDIfinder/L2A/gedifinder_L2A_v001_6_8_21.csv",header = FALSE)

gedi_list_1$group<-c(rep(88, 14),rep(87:1, each = 25))

gedi_sub<-subset(gedi_list_1, group==1)

for (i in 1:87) {
  gedi_sub[[i]]<-subset(gedi_list_1, group==i)
}

colnames(gedi_sub)<-c(1:87)

# for (i in 1:67) {
#   write1<-gedi_sub[,i]
#   write2<-write1[,1]
#   write.table(write2,sep=",", paste0("G:/GEDI/GEDIfinder/L2A/subsets/gedi_sub", i, ".csv"), row.names = FALSE, col.names = FALSE)
# }

for (i in 68:87) {
  write1<-gedi_sub[,i]
  write2<-write1[,1]
  write.table(write2,sep=",", paste0("G:/GEDI/GEDIfinder/L2A/subsets/gedi_sub", i, ".csv"), row.names = FALSE, col.names = FALSE)
}

gedi_sub88<-subset(gedi_list_1, group==88)
write.table(gedi_sub88[,1],sep=",", paste0("G:/GEDI/GEDIfinder/L2A/subsets/gedi_sub", 88, ".csv"), row.names = FALSE, col.names = FALSE)
