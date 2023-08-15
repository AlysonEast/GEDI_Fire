
library(lidR)
library(sf)

las <- readLAS(paste0("G:/Thesis/Spatial/LiDAR/Zones/Zone19/BON_A01_2018_LAS/BON_A01_2018_LAS_2.las"))
sps <- as(extent(las), 'SpatialPolygons')
proj4string(sps)<-CRS(paste0("+proj=utm +zone=19 +datum=WGS84 +south")) #This is for zone 19 utm projects

plot(las)
plot(x,y,195, add=TRUE)
sps@bbox
#Finding a point in the middle of the LAS area
x<-mean(sps@bbox[1,c(1,2)])
y<-mean(sps@bbox[2,c(1,2)])
x1<-689518
y1<-8908134

p <- plot(las)
#ttops <- find_localmaxima(las, w=25)
#add_treetops3d(p, ttops)

coord<-as.data.frame(cbind(x,y))
sp <- SpatialPoints(coord)
sp@proj4string<-CRS(proj4string(sps))
dat_sf <- st_as_sf(sp, coords = c("x", "y"), crs = 32721)

footprint <- st_buffer(dat_sf, dist = 12.5)

plot(footprint)

las_footprint<-clip_circle(las, xcenter = 689518, ycenter = 8908134, radius = 12.5)
plot(las_footprint,
     bg = "white",
     axis=TRUE,
     color="Z",
     colorPalette = forest.colors(10))
oldpar<-par()
par(mfrow=c(2,2), mar=c(4,4,0,0), oma=c(0,0,1,1),cex.axis = 1.2)
par(oldpar)
library(plot3D)

scatter3D(
  las_footprint@data$X,las_footprint@data$Y,las_footprint@data$Z,
  pch = 16, main="",
  cex = 0.5,#bty = "g",col.panel =NA,
  #ticktype = "detailed",
  phi = 5,alpha=.8,theta=65,
  col=forest.colors(10),
  xlab="UTM Easting (m)", ylab="UTM Northing (m)", zlab="Elevation (m)",
  clab="Elevation (m)")


install.packages("https://cran.r-project.org/src/contrib/Archive/rGEDI/rGEDI_0.1.11.tar.gz", repos = NULL)
library(rGEDI)
wf<-gediWFSimulator(input=paste0("G:/Thesis/Spatial/LiDAR/Zones/Zone19/BON_A01_2018_LAS/BON_A01_2018_LAS_2.las"),
                    output = paste0("G:/Thesis/Spatial/LiDAR/SIM/example"),
                    coords = c(x1,y1))
rh<-gediWFMetrics(input= wf, 
                  outRoot = paste0("G:/Thesis/Spatial/LiDAR/example"),
                  rhRes = 1)
PAI<-gediWFMetrics(input= wf, 
                  outRoot = paste0("G:/Thesis/Spatial/LiDAR/example"),
                  laiRes = 5)
library(reshape)
rh_melt<-melt(rh[,c(1,14:114)], id="wave ID")
rh_melt$rh<-c(0:100)
rh_melt$height_m<-rh_melt$value+rh$gHeight

PAI_df<-as.data.frame(cbind(Heights = c(0,10,20,30),
                      PAI = c(PAI[1,132],PAI[1,133],PAI[1,134],PAI[1,135])))
PAI_plot<-matrix(c(10,10,10,10),nrow = 4)
colnames(PAI_plot)<-"PAI"
rownames(PAI_plot)<-c("0-10","10-20","20-30","30-40")
barplot(PAI_plot, 
        col=c("#006400","#399B39","#90EE90","#56B656"),
        border="white", 
        space=0.04, 
        font.axis=2, 
        xlab="")



par(oldpar)
#png(file="G:/Thesis/Figures/Final/RSE//GEDI_example.png", width=11, height=6, units = "in", res=120) #res in ppi 300dpi = 118 ppi
tiff(file="G:/Thesis/Figures/Final/RSE/Figure2.tiff", units="in", width=11, height=6, res=300)

par(mfrow=c(1,2))
#layout(matrix(c(1,2,3,4), 1, 4, byrow=TRUE), widths = c(4,4,3,1))
scatter3D(
  las_footprint@data$X,las_footprint@data$Y,las_footprint@data$Z,
  pch = 16, main="",
  cex = 0.5,#bty = "g",col.panel =NA,
  #ticktype = "detailed",
  colkey = list(side = 2, length = 0.55),
  phi = 5,alpha=.8,theta=65,
  col=forest.colors(10),
  xlab="UTM Easting (m)", ylab="UTM Northing (m)", zlab="Elevation (m)",
  clab="Elevation (m)")
title(outer = TRUE,"A",font=2, line=-2,adj  = 0.05, cex.main=2)
par(mai=c(1,1,1,1))
plot(wf, relative=TRUE, polygon=TRUE, type="l", lwd=2, col="orange",
     xlab="Relative Height (RH) Percentile", ylab="Elevation (m)", ylim=c(145,200))
abline(h=rh$gHeight, lty=2, col="brown", lwd=2)
abline(h=rh$`true top`, lty=2, col="brown", lwd=2)
axis(side = 4, 
     at = c(rh$gHeight,rh$gHeight+10,rh$gHeight+20,rh$gHeight+30,rh$gHeight+40,rh$gHeight+50), 
     labels = c("0", "10", "20", "30","40","50"))      
mtext("Height Return (m)", side = 4, line = 3)
axis(side = 3, at = c(0,20,40,60,80,100), labels = c("0", "0.2", "0.4", "0.6", "0.8", "1.0"))      
mtext("Normalized Waveform Amplitude", side = 3, line = 2)
text(70, 155, "Ground Return", font=2)
text(20, 197, "Canopy Height", font=2)
lines(rh_melt$rh, rh_melt$height_m, type="l", lwd=3, xlab="", ylab="", axis=FALSE)
text(0,0.5,"B",cex=1.1,font=2)
title(outer = TRUE,"B",font=2, line=-2,adj= .55, cex.main=2)
# barplot(PAI_plot, 
#         col=c("#006400","#399B39","#90EE90","#56B656"),
#         border="white", 
#         space=0.04, 
#         font.axis=2, 
#         xlab="", ylab = "Height Bins (m)")
# legend.scale(c(0, 1), col = rev(forest.colors(6)), horizontal = FALSE, breaks = c(.1,.2,.3,.4,.5,.6,.7))
dev.off()


#Exploring footprints ####
las <- readLAS(paste0("G:/Thesis/Spatial/LiDAR/Zones/Zone19/BON_A01_2018_LAS/BON_A01_2018_LAS_5.las"))
sps <- as(extent(las), 'SpatialPolygons')
proj4string(sps)<-CRS(paste0("+proj=utm +zone=19 +datum=WGS84 +south")) #This is for zone 19 utm projects

#edge young forest
x2<-688128
y2<-8909137

#talldense
x3<-689080
y3<-8907721

x<-688758
y<-8907664

dat_sf <- st_as_sf(sp, coords = c("x", "y"), crs = 32721)

footprint <- st_buffer(dat_sf, dist = 12.5)

plot(footprint)

las_footprint<-clip_circle(las, xcenter = x, ycenter = y, radius = 12.5)
plot(las_footprint,
     bg = "white",
     axis=TRUE,
     color="Z",
     colorPalette = forest.colors(10))
oldpar<-par()
par(mfrow=c(2,2), mar=c(4,4,0,0), oma=c(0,0,1,1),cex.axis = 1.2)
par(oldpar)
library(plot3D)

scatter3D(
  las_footprint@data$X,las_footprint@data$Y,las_footprint@data$Z,
  pch = 16, main="",
  cex = 0.5,#bty = "g",col.panel =NA,
  #ticktype = "detailed",
  phi = 5,alpha=.8,theta=65,
  col=forest.colors(10),
  xlab="UTM Easting (m)", ylab="UTM Northing (m)", zlab="Elevation (m)",
  clab="Elevation (m)")

#new plots####

#OG edge tree
las <- readLAS(paste0("G:/Thesis/Spatial/LiDAR/Zones/Zone19/BON_A01_2018_LAS/BON_A01_2018_LAS_2.las"))
sps <- as(extent(las), 'SpatialPolygons')
proj4string(sps)<-CRS(paste0("+proj=utm +zone=19 +datum=WGS84 +south")) #This is for zone 19 utm projects
x1<-689518
y1<-8908134
las_footprint<-clip_circle(las, xcenter = x1, ycenter = y1, radius = 12.5)


wf<-gediWFSimulator(input=paste0("G:/Thesis/Spatial/LiDAR/Zones/Zone19/BON_A01_2018_LAS/BON_A01_2018_LAS_2.las"),
                    output = paste0("G:/Thesis/Spatial/LiDAR/SIM/example"),
                    coords = c(x1,y1))
rh<-gediWFMetrics(input= wf, 
                  outRoot = paste0("G:/Thesis/Spatial/LiDAR/example"),
                  rhRes = 1)
PAI<-gediWFMetrics(input= wf, 
                   outRoot = paste0("G:/Thesis/Spatial/LiDAR/example"),
                   laiRes = 5, laiH = 45)
library(reshape)
rh_melt<-melt(rh[,c(1,14:114)], id="wave ID")
rh_melt$rh<-c(0:100)
rh_melt$height_m<-rh_melt$value+rh$gHeight


pai_melt<-melt(PAI[,c(1,138:147)], id="wave ID")
pai_melt$Height_bin<-c("0 to 5","5 to 10","10 to 15",
                       "15 to 20","20 to 25","25 to 30",
                       "30 to 35","35 to 40","40 to 45",
                       "45 to 50")
pai_melt$meters<-5

pai_melt$Height_bin<-ordered(pai_melt$Height_bin, levels=rev(c("0 to 5","5 to 10","10 to 15",
                                                           "15 to 20","20 to 25","25 to 30",
                                                           "30 to 35","35 to 40","40 to 45",
                                                           "45 to 50")))
pai_melt[10,3]<-NA


# png(file="G:/Thesis/Figures/GEDI_data_ex/GEDI_PAI_edgeTrees_scaled.png", width=1.75, height=4, units = "in", res=120) #res in ppi 300dpi = 118 ppi
ggplot(data = pai_melt, aes(x=`wave ID`, y=meters, col=Height_bin, fill=value)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_color_manual(values = c("#FFFFFF","#FCFCFC","#FAFAFA",
                               "#F6F6F6","#F7F7F7","#F3F3F3",
                               "#FCFFF9","#FAFFF6","#FBFEF9",
                               "white"),
                    labels = c("0 to 5","5 to 10","10 to 15",
                               "15 to 20","20 to 25","25 to 30",
                               "30 to 35","35 to 40","40 to 45",
                               "45 to 50"),) +
  theme_pubr() +
  guides(col="none") + xlab("") + 
  theme(axis.text.x = element_blank(), axis.ticks = element_blank()) + 
  labs(fill = "PAI") + ylab("Height (m)") +
  scale_fill_gradientn(colors = rev(forest.colors(10)), limits=c(0,1.2), na.value = "white") + #limits sets scale
  theme(legend.position="right")
dev.off()

wf<-gediWFSimulator(input=paste0("G:/Thesis/Spatial/LiDAR/Zones/Zone19/BON_A01_2018_LAS/BON_A01_2018_LAS_2.las"),
                    output = paste0("G:/Thesis/Spatial/LiDAR/SIM/example"),
                    coords = c(x1,y1))

png(file="G:/Thesis/Figures/GEDI_data_ex/GEDI_example_edgeTrees.png", width=11, height=6, units = "in", res=120) #res in ppi 300dpi = 118 ppi
par(mfrow=c(1,2))
scatter3D(
  las_footprint@data$X,las_footprint@data$Y,las_footprint@data$Z,
  pch = 16, main="",
  cex = 0.5,#bty = "g",col.panel =NA,
  #ticktype = "detailed",
  colkey = list(side = 2, length = 0.55),
  phi = 5,alpha=.8,theta=65,
  col=forest.colors(10),
  xlab="UTM Easting (m)", ylab="UTM Northing (m)", zlab="Elevation (m)",
  clab="Elevation (m)")
#title(outer = TRUE,"A",font=2, line=-2,adj  = 0.05, cex.main=2)
par(mai=c(1,1,1,1))
plot(wf, relative=TRUE, polygon=TRUE, type="l", lwd=2, col="orange",
     xlab="Relative Height (rh) Percentile", ylab="Return Height (m)", ylim=c(150,205),yaxt="n")
axis(side = 2, 
     at = c(rh$gHeight,rh$gHeight+10,rh$gHeight+20,rh$gHeight+30,rh$gHeight+40,rh$gHeight+50), 
     labels = c("0", "10", "20", "30","40","50"))      
abline(h=rh$gHeight, lty=2, col="brown", lwd=2)
abline(h=rh$`true top`, lty=2, col="brown", lwd=2)
axis(side = 3, at = c(0,20,40,60,80,100), labels = c("0", "0.2", "0.4", "0.6", "0.8", "1.0"))      
mtext("Normalized Waveform Amplitude", side = 3, line = 2)
text(70, 155, "Ground Return", font=2)
text(20, 197, "Canopy Height", font=2)
lines(rh_melt$rh, rh_melt$height_m, type="l", lwd=3, xlab="", ylab="", axis=FALSE)
text(0,0.5,"B",cex=1.1,font=2)
# title(outer = TRUE,"B",font=2, line=-2,adj= .55, cex.main=2)
dev.off()


#Dense Interior forest
las <- readLAS(paste0("G:/Thesis/Spatial/LiDAR/Zones/Zone19/BON_A01_2018_LAS/BON_A01_2018_LAS_3.las"))
sps <- as(extent(las), 'SpatialPolygons')
proj4string(sps)<-CRS(paste0("+proj=utm +zone=19 +datum=WGS84 +south")) #This is for zone 19 utm projects
x3<-687447
y3<-8906533
las_footprint<-clip_circle(las, xcenter = x3, ycenter = y3, radius = 12.5)

wf<-gediWFSimulator(input=paste0("G:/Thesis/Spatial/LiDAR/Zones/Zone19/BON_A01_2018_LAS/BON_A01_2018_LAS_3.las"),
                    output = paste0("G:/Thesis/Spatial/LiDAR/SIM/example"),
                    coords = c(x3,y3))
rh<-gediWFMetrics(input= wf, 
                  outRoot = paste0("G:/Thesis/Spatial/LiDAR/example"),
                  rhRes = 1)
PAI<-gediWFMetrics(input= wf, 
                   outRoot = paste0("G:/Thesis/Spatial/LiDAR/example"),
                   laiRes = 5, laiH = 50)

library(reshape)
rh_melt<-melt(rh[,c(1,14:114)], id="wave ID")
rh_melt$rh<-c(0:100)
rh_melt$height_m<-rh_melt$value+rh$gHeight

pai_melt<-melt(PAI[,c(1,139:148)], id="wave ID")
pai_melt$Height_bin<-c("0 to 5","5 to 10","10 to 15",
                       "15 to 20","20 to 25","25 to 30",
                       "30 to 35","35 to 40","40 to 45",
                       "45 to 50")
pai_melt$meters<-5

pai_melt$Height_bin<-ordered(pai_melt$Height_bin, levels=rev(c("0 to 5","5 to 10","10 to 15",
                                                               "15 to 20","20 to 25","25 to 30",
                                                               "30 to 35","35 to 40","40 to 45",
                                                               "45 to 50")))


# png(file="G:/Thesis/Figures/GEDI_data_ex/GEDI_PAI_denseInterior.png", width=1.75, height=4, units = "in", res=120) #res in ppi 300dpi = 118 ppi
ggplot(data = pai_melt, aes(x=`wave ID`, y=meters, col=Height_bin, fill=value)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_color_manual(values = c("#FFFFFF","#FCFCFC","#FAFAFA",
                                "#F6F6F6","#F7F7F7","#F3F3F3",
                                "#FCFFF9","#FAFFF6","#FBFEF9",
                                "white"),
                     labels = c("0 to 5","5 to 10","10 to 15",
                                "15 to 20","20 to 25","25 to 30",
                                "30 to 35","35 to 40","40 to 45",
                                "45 to 50"),) +
  theme_pubr() +
 guides(col="none") + xlab("") +
  theme(axis.text.x = element_blank(), axis.ticks = element_blank()) + 
  labs(fill = "PAI") + ylab("Height (m)") +
  scale_fill_gradientn(colors = rev(forest.colors(10))) + 
  theme(legend.position="right")
dev.off()


wf<-gediWFSimulator(input=paste0("G:/Thesis/Spatial/LiDAR/Zones/Zone19/BON_A01_2018_LAS/BON_A01_2018_LAS_5.las"),
                    output = paste0("G:/Thesis/Spatial/LiDAR/SIM/example"),
                    coords = c(x3,y3))

png(file="G:/Thesis/Figures/GEDI_data_ex/GEDI_example_Dense_interior.png", width=11, height=6, units = "in", res=120) #res in ppi 300dpi = 118 ppi
par(mfrow=c(1,2))
scatter3D(
  las_footprint@data$X,las_footprint@data$Y,las_footprint@data$Z,
  pch = 16, main="",
  cex = 0.5,#bty = "g",col.panel =NA,
  #ticktype = "detailed",
  colkey = list(side = 2, length = 0.55),
  phi = 5,alpha=.8,theta=65,
  col=forest.colors(10),
  xlab="UTM Easting (m)", ylab="UTM Northing (m)", zlab="Elevation (m)",
  clab="Elevation (m)")
#title(outer = TRUE,"A",font=2, line=-2,adj  = 0.05, cex.main=2)
par(mai=c(1,1,1,1))
plot(wf, relative=TRUE, polygon=TRUE, type="l", lwd=2, col="orange",
     xlab="Relative Height (rh) Percentile", ylab="Return Height (m)", ylim=c(150,205),yaxt="n")
axis(side = 2, 
     at = c(rh$gHeight,rh$gHeight+10,rh$gHeight+20,rh$gHeight+30,rh$gHeight+40,rh$gHeight+50), 
     labels = c("0", "10", "20", "30","40","50"))      
abline(h=rh$gHeight, lty=2, col="brown", lwd=2)
abline(h=rh$`true top`, lty=2, col="brown", lwd=2)
axis(side = 3, at = c(0,20,40,60,80,100), labels = c("0", "0.2", "0.4", "0.6", "0.8", "1.0"))      
mtext("Normalized Waveform Amplitude", side = 3, line = 2)
text(70, 155, "Ground Return", font=2)
text(20, 197, "Canopy Height", font=2)
lines(rh_melt$rh, rh_melt$height_m, type="l", lwd=3, xlab="", ylab="", axis=FALSE)
text(0,0.5,"B",cex=1.1,font=2)
# title(outer = TRUE,"B",font=2, line=-2,adj= .55, cex.main=2)
dev.off()

#Young Edge
las <- readLAS(paste0("G:/Thesis/Spatial/LiDAR/Zones/Zone19/BON_A01_2018_LAS/BON_A01_2018_LAS_8.las"))
sps <- as(extent(las), 'SpatialPolygons')
proj4string(sps)<-CRS(paste0("+proj=utm +zone=19 +datum=WGS84 +south")) #This is for zone 19 utm projects
x2<-688128
y2<-8909137
las_footprint<-clip_circle(las, xcenter = x2, ycenter = y2, radius = 12.5)

wf<-gediWFSimulator(input=paste0("G:/Thesis/Spatial/LiDAR/Zones/Zone19/BON_A01_2018_LAS/BON_A01_2018_LAS_8.las"),
                    output = paste0("G:/Thesis/Spatial/LiDAR/SIM/example"),
                    coords = c(x2,y2))
rh<-gediWFMetrics(input= wf, 
                  outRoot = paste0("G:/Thesis/Spatial/LiDAR/example"),
                  rhRes = 1)
PAI<-gediWFMetrics(input= wf, 
                   outRoot = paste0("G:/Thesis/Spatial/LiDAR/example"),
                   laiRes = 5, laiH = 50)
library(reshape)
rh_melt<-melt(rh[,c(1,14:114)], id="wave ID")
rh_melt$rh<-c(0:100)
rh_melt$height_m<-rh_melt$value+rh$gHeight


pai_melt<-melt(PAI[,c(1,139:148)], id="wave ID")
pai_melt$Height_bin<-c("0 to 5","5 to 10","10 to 15",
                       "15 to 20","20 to 25","25 to 30",
                       "30 to 35","35 to 40","40 to 45",
                       "45 to 50")
pai_melt$meters<-5

pai_melt$Height_bin<-ordered(pai_melt$Height_bin, levels=rev(c("0 to 5","5 to 10","10 to 15",
                                                               "15 to 20","20 to 25","25 to 30",
                                                               "30 to 35","35 to 40","40 to 45",
                                                               "45 to 50")))
pai_melt[6:10,3]<-NA


png(file="G:/Thesis/Figures/GEDI_data_ex/GEDI_PAI_youngEdge_scaled.png", width=1.75, height=4, units = "in", res=120) #res in ppi 300dpi = 118 ppi
ggplot(data = pai_melt, aes(x=`wave ID`, y=meters, col=Height_bin, fill=value)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_color_manual(values = c("#FFFFFF","#FCFCFC","#FAFAFA",
                                "#F6F6F6","#F7F7F7","#F3F3F3",
                                "#FCFFF9","#FAFFF6","#FBFEF9",
                                "white"),
                     labels = c("0 to 5","5 to 10","10 to 15",
                                "15 to 20","20 to 25","25 to 30",
                                "30 to 35","35 to 40","40 to 45",
                                "45 to 50"),) +
  theme_pubr() +
  guides(col="none") + xlab("") +
  theme(axis.text.x = element_blank(), axis.ticks = element_blank()) + 
  labs(fill = "PAI") + ylab("Height (m)") +
  scale_fill_gradientn(colors = rev(forest.colors(10)), limits=c(0,1.2), na.value = "white") + 
  theme(legend.position="right")
dev.off()

wf<-gediWFSimulator(input=paste0("G:/Thesis/Spatial/LiDAR/Zones/Zone19/BON_A01_2018_LAS/BON_A01_2018_LAS_8.las"),
                    output = paste0("G:/Thesis/Spatial/LiDAR/SIM/example"),
                    coords = c(x2,y2))

png(file="G:/Thesis/Figures/GEDI_data_ex/GEDI_example_young_edge.png", width=11, height=6, units = "in", res=120) #res in ppi 300dpi = 118 ppi
par(mfrow=c(1,2))
scatter3D(
  las_footprint@data$X,las_footprint@data$Y,las_footprint@data$Z,
  pch = 16, main="",
  cex = 0.5,#bty = "g",col.panel =NA,
  #ticktype = "detailed",
  colkey = list(side = 2, length = 0.55),
  phi = 5,alpha=.8,theta=65,
  col=forest.colors(10),
  xlab="UTM Easting (m)", ylab="UTM Northing (m)", zlab="Elevation (m)",
  clab="Elevation (m)")
#title(outer = TRUE,"A",font=2, line=-2,adj  = 0.05, cex.main=2)
par(mai=c(1,1,1,1))
plot(wf, relative=TRUE, polygon=TRUE, type="l", lwd=2, col="orange",
     xlab="Relative Height (rh) Percentile", ylab="Return Height (m)", ylim=c(144,199),yaxt="n")
axis(side = 2, 
     at = c(rh$gHeight,rh$gHeight+10,rh$gHeight+20,rh$gHeight+30,rh$gHeight+40,rh$gHeight+50), 
     labels = c("0", "10", "20", "30","40","50"))      
abline(h=rh$gHeight, lty=2, col="brown", lwd=2)
abline(h=rh$`true top`, lty=2, col="brown", lwd=2)
axis(side = 3, at = c(0,20,40,60,80,100), labels = c("0", "0.2", "0.4", "0.6", "0.8", "1.0"))      
mtext("Normalized Waveform Amplitude", side = 3, line = 2)
text(70, 149, "Ground Return", font=2)
text(20, 171, "Canopy Height", font=2)
lines(rh_melt$rh, rh_melt$height_m, type="l", lwd=3, xlab="", ylab="", axis=FALSE)
text(0,0.5,"B",cex=1.1,font=2)
# title(outer = TRUE,"B",font=2, line=-2,adj= .55, cex.main=2)
dev.off()
