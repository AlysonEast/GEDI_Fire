#!/bin/bash

zone=19
awk 'BEGIN {FS=","}; {print $2" "$3}'  GEDI_coords_zone${zone}.csv | sed '1d'>coordlist.txt

gediRat -inList ALS_files_${zone}.txt -hdf -l1b -listCoord coordlist.txt -checkCover -output /media/aly/Bridger/Thesis/gediSimulator/GEDI_Data/Zone${zone}/L2A/Simulated/zone${zone}_waves_sim.h5 -ground

gediMetric -input /media/aly/Bridger/Thesis/gediSimulator/GEDI_Data/Zone${zone}/L2A/Simulated/zone${zone}_waves_sim.h5 -readHDFgedi -ground -outRoot /media/aly/Bridger/Thesis/gediSimulator/GEDI_Data/Zone${zone}/L2A/Simulated/metrics -wrieGauss -rhres 1 -laires 5

