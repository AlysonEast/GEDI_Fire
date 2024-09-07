#!/bin/bash

zone=19

conda activate gedi
python ../GEDI/Python/GEDI_Subsetter.py --dir /media/aly/Bridger/Thesis/gediSimulator/GEDI_Data/Zone21/L2A/ --roi /media/aly/Bridger/Thesis/Spatial/LiDAR/Bounds/ALS_bounds_dissolve.geojson
