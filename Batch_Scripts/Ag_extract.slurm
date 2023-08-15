#!/bin/bash
#SBATCH -J FireEx.R # Name for your job
#SBATCH -n 8 # Number of tasks when using MPI. Default is 1, and SLURM assumes the usage of 1 cpu per task.
#SBATCH -N 1 # Number of nodes to spread cores across - default is 1 - if you are not using MPI this should likely be 1
#SBATCH --mem 75000 # Megabytes of memory requested. Default is 2000/task.
#SBATCH -t 24:00:00 # Runtime in days-hours:minutes:seconds. 
#SBATCH -p defq # Partition to submit to the standard compute node partition(defq) or the express node partition(express)
#SBATCH -o FireEx-%j.out.txt # Standard output (stdout) goes to this file (what would print to the screen if you were running the command locally)
#SBATCH -e FireEx-%j.err.txt # Standard error (stderr) goes to this file (errors that would print to the screen if you were running the command locally)
#SBATCH --mail-user alyson.east@montana.edu # this is the email you wish to be notified.
#SBATCH --mail-type ALL # this specifies what events you should get an email about ALL will alert you of job beginning, completion, failure, etc.
#SBATCH -D /mnt/lustrefs/scratch/v38p156/Datasets/Scripts/ # added working directory
n=14
module load R/3.5.3
module load GDAL/2.1.0-foss-2016b
module load PROJ/5.0.0-foss-2018b

Rscript Ag_Extract.R $n