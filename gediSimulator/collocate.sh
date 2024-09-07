#!/bin/bash

zone=19


for gedi_file in ./GEDI_Data/Zone${zone}/*.h5
do
	name=`echo ${gedi_file} | awk -F "/" '{print $4}'`
	echo "${name} -${zone}" 
	collocateWaves -listALS ALS_files_${zone}.txt -gedi ${gedi_file} -readHDFgedi -fixFsig -solveCofG -writeWaves ./GEDI_Data/Zone${zone}/sim/sim_${name} -aEPSG 327${zone} -maxShift 10 -step 3 -simplex -maxIter 300 -minDense 3 -minSense 0.9 -checkCover -leaveEmpty

done
