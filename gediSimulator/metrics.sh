#!/bin/bash

zone=21

for sim_file in ./GEDI_Data/Zone${zone}/sim/*.h5
do
	name=`echo ${sim_file} | awk -F "/" '{print $5}'`
        echo "${name}"
	gediMetric -input ${sim_file} -readHDFgedi -ground -outRoot /media/aly/Bridger/Thesis/gediSimulator/GEDI_Data/Zone${zone}/sim/metrics/${name} -wrieGauss -rhres 1 -laires 5
done


