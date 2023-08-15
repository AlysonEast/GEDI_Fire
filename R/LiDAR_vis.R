library(sp)
library(raster)
library(rgdal)
library(lidR)
library(ggplot2)
library(hdf5r)
library(devtools)
#install.packages("rGEDI", repos="http://R-Forge.R-project.org")
library(rGEDI)
library(maptools)
library(webshot2)
library(magick)
library(rgl)

las1 <- readLAS(paste0("G:/Thesis/Spatial/LiDAR/Zones/Zone19/BON_A01_2018_LAS/BON_A01_2018_LAS_0.las"))
plot(las1)

las_cat <- readLAScatalog("G:/Thesis/Spatial/LiDAR/Zones/Zone19/BON_A01_2018_LAS/")
plot(las_cat)

las2 <- readLAS(paste0("G:/Thesis/Spatial/LiDAR/Zones/Zone19/BON_A01_2018_LAS/BON_A01_2018_LAS_1.las")) #little bit of edge
plot(las2)
movie3d(spin3d(axis = c(0, -.5, 1)), duration = 10, fps = 10, convert = FALSE,
        dir = "G:/Thesis/Figures/Movie_Scratch", movie = "las_3", webshot = FALSE)


list.files(path='G:/Thesis/Figures/Movie_Scratch', full.names = TRUE) %>% 
  image_read() %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=1) %>% # animates, can opt for number of loops
  image_write("G:/Thesis/Figures/las_3.gif") # write to current dir

las3 <- readLAS(paste0("G:/Thesis/Spatial/LiDAR/Zones/Zone19/BON_A01_2018_LAS/BON_A01_2018_LAS_3.las"))
plot(las3)
