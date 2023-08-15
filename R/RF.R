library(lme4)
library(raster)
library(rgdal)
library(rgeos)
library(dismo)
library(spatial.tools)
library(tidyverse)

command_args <- commandArgs(trailingOnly = TRUE)
# Define command line arguments as a variable
n <- command_args[1]

#Read in data
L2A<-readOGR(paste0("/mnt/lustrefs/scratch/v38p156/Fire_Summary/Fire_regions/L2A_Fire_",n,".geojson"))

#### Model preperation ####
#Create Year since burn
B2000<-raster("/mnt/lustrefs/scratch/v38p156/Rasters/RF/MODIS_Annual/B2000.tif")
B2001<-raster("/mnt/lustrefs/scratch/v38p156/Rasters/RF/MODIS_Annual/B2001.tif")
B2002<-raster("/mnt/lustrefs/scratch/v38p156/Rasters/RF/MODIS_Annual/B2002.tif")
B2003<-raster("/mnt/lustrefs/scratch/v38p156/Rasters/RF/MODIS_Annual/B2003.tif")
B2004<-raster("/mnt/lustrefs/scratch/v38p156/Rasters/RF/MODIS_Annual/B2004.tif")
B2005<-raster("/mnt/lustrefs/scratch/v38p156/Rasters/RF/MODIS_Annual/B2005.tif")
B2006<-raster("/mnt/lustrefs/scratch/v38p156/Rasters/RF/MODIS_Annual/B2006.tif")
B2007<-raster("/mnt/lustrefs/scratch/v38p156/Rasters/RF/MODIS_Annual/B2007.tif")
B2008<-raster("/mnt/lustrefs/scratch/v38p156/Rasters/RF/MODIS_Annual/B2008.tif")
B2009<-raster("/mnt/lustrefs/scratch/v38p156/Rasters/RF/MODIS_Annual/B2009.tif")
B2010<-raster("/mnt/lustrefs/scratch/v38p156/Rasters/RF/MODIS_Annual/B2010.tif")
B2011<-raster("/mnt/lustrefs/scratch/v38p156/Rasters/RF/MODIS_Annual/B2011.tif")
B2012<-raster("/mnt/lustrefs/scratch/v38p156/Rasters/RF/MODIS_Annual/B2012.tif")
B2013<-raster("/mnt/lustrefs/scratch/v38p156/Rasters/RF/MODIS_Annual/B2013.tif")
B2014<-raster("/mnt/lustrefs/scratch/v38p156/Rasters/RF/MODIS_Annual/B2014.tif")
B2015<-raster("/mnt/lustrefs/scratch/v38p156/Rasters/RF/MODIS_Annual/B2015.tif")
B2016<-raster("/mnt/lustrefs/scratch/v38p156/Rasters/RF/MODIS_Annual/B2016.tif")
B2017<-raster("/mnt/lustrefs/scratch/v38p156/Rasters/RF/MODIS_Annual/B2017.tif")
B2018<-raster("/mnt/lustrefs/scratch/v38p156/Rasters/RF/MODIS_Annual/B2018.tif")
B2019<-raster("/mnt/lustrefs/scratch/v38p156/Rasters/RF/MODIS_Annual/B2019.tif")
B2020<-raster("/mnt/lustrefs/scratch/v38p156/Rasters/RF/MODIS_Annual/B2020.tif")

files<-c(B2000, B2001, B2002, B2003, B2004, B2005, B2006, B2007, B2008, B2009, B2010, 
         B2011, B2012, B2013, B2014, B2015, B2016, B2017, B2018)
time_past<-c(19:1)
for (i in 1:19) {
  m<-c(0,.9,0,1,400,time_past[i])
  rmatrix<-matrix(m, ncol = 3, byrow = TRUE)
  paste0(files[i],"_reclass")<-reclassify(files[i],rmatrix)
}

Burn_Stack<-stack(B2000_reclass, B2001_reclass, B2002_reclass, B2003_reclass, B2004_reclass, B2005_reclass, 
                  B2006_reclass, B2007_reclass, B2008_reclass, B2009_reclass, B2010_reclass, B2011_reclass, 
                  B2012_reclass, B2013_reclass, B2014_reclass, B2015_reclass, B2016_reclass, B2017_reclass, 
                  B2018_reclass)
Since_Burn<-maxValue(Burn_Stack)

# Read in Raster predictor Data
VIIRS_<-raster("/mnt/lustrefs/scratch/v38p156/Rasters/RF/VIIRS/VIIRS_.tif")

Height_2012<-raster("/mnt/lustrefs/scratch/v38p156/Rasters/RF/Hansen/height.tif")
Cover_2019<-raster("/mnt/lustrefs/scratch/v38p156/Rasters/RF/Hansen/Cover.tif")

# Project the shapefile into the projection of the raster inputs
L2A <- spTransform(L2A, CRS=proj4string(VIIRS_))


# Create raster stack of predictor variables
predictor_stack <- stack(Since_Burn, Height_2012, Cover_2019, VIIRS_, )

# Extract predictor data
.rs.unloadPackage("tidyr")
L2A_full <- extract(predictor_stack, L2A, sp=TRUE)

# Model likelihood of PIAL presence
L2A_full_df <- L2A_full_df@data
L2A_rf_df <- L2A_full_df[,c(10,11:21)] #Update the columns to pull for the modeling matrix

#### Modeling for small size class####
library(caret)
set.seed(1234)
inTraining<- createDataPartition(L2A_rf_df$PA_pial, p = 0.85, list=F) #Figure out what p should be set at
training<- L2A_rf_df[inTraining,]; training <- training[,c(1:ncol(training))]
testing<- L2A_rf_df[-inTraining,]; testing <- testing[,c(1:ncol(testing))]

fitControl <- trainControl(method="repeatedcv", number=10, repeats=10, 
                           classProbs=FALSE, savePredictions="all", allowParallel=TRUE)
gbmGrid <- expand.grid(interaction.depth = c(5,7,9,11), 
                       n.trees = (1:100)*50, 
                       shrinkage = c(0.1, 0.01, 0.001), 
                       n.minobsinnode = 10)
set.seed(5678)
fit_grid<- train(PA_pial ~ ., data = training, method = "gbm", trControl = fitControl,
                            verbose=T, tuneGrid=gbmGrid, metric="ROC")
fit_grid$results
max(fit_grid$results$ROC)

library(spatstat)
auc(fit_grid$results$ROC)
# Find thresholds: 1) sensitivity = specificity, 2) max Kappa, 3) max TSS
library(e1071)
threshold_stats <- thresholder(fit_grid, threshold=seq(0.1,1,by=0.001),final=TRUE)
threshold_equal <- threshold_stats[which.min(abs(threshold_stats$Specificity - 
                                                                    threshold_stats$Sensitivity)),]$prob_threshold
threshold_kappa <- threshold_stats[which.max(threshold_stats$Kappa),]$prob_threshold
threshold_tss <- threshold_stats[which.max(threshold_stats$Specificity + threshold_stats$Sensitivity - 1),]$prob_threshold

print("Equal")
threshold_equal
print("kappa")
threshold_kappa
print("tss")
threshold_tss

Regions<-readOGR()
Region<-Region[n]

predictor_stack_region<-mask(predictor_stack, Region)

map <- predict(predictor_stack, fit_grid)
plot(map)
writeRaster(map, paste0("/mnt/lustrefs/scratch/v38p156/Fire_Summary/RF/RF_",n))