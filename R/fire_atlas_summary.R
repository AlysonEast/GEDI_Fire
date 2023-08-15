library(rgdal)

fire_spdf<-readOGR(dsn = "G:/Thesis", layer = "fire_atlas_events_20201220")

fire_df<-fire_spdf@data

type_conf<-table(fire_df$confidence, fire_df$fire_type)
type_conf

table(fire_df$protected)

PA_fires<-subset(fire_df, protected>0)
dim(PA_fires)
dim(fire_df)
57532/571542

hist(fire_df$size)
fire_df$size_m<-(fire_df$size*1000)
hist(fire_df$size_m)

fire_df_forest<-subset(fire_df, fire_type==3 | fire_type==4)
table(fire_df_forest$size_m)
hist(fire_df_forest$size_m)
