#!/bin/bash
#SBATCH -J GEDI_Subsetter.py # Name for your job
#SBATCH -n 8 # Number of tasks when using MPI. Default is 1, and SLURM assumes the usage of 1 cpu per task.
#SBATCH -N 1 # Number of nodes to spread cores across - default is 1 - if you are not using MPI this should likely be 1
#SBATCH --mem 24000 # Megabytes of memory requested. Default is 2000/task.
#SBATCH -t 20:00:00 # Runtime in days-hours:minutes:seconds. 
#SBATCH -p defq # Partition to submit to the standard compute node partition(defq) or the express node partition(express)
#SBATCH -o example-%j.out.txt # Standard output (stdout) goes to this file (what would print to the screen if you were running the command locally)
#SBATCH -e example-%j.err.txt # Standard error (stderr) goes to this file (errors that would print to the screen if you were running the command locally)
#SBATCH --mail-user alyson.east@montana.edu # this is the email you wish to be notified.
#SBATCH --mail-type ALL # this specifies what events you should get an email about ALL will alert you of job beginning, completion, failure, etc.
#SBATCH -D /mnt/lustrefs/scratch/v38p156 # added working directory

module load python/3.7.0 
module load Anaconda3/5.1.0
source activate /mnt/lustrefs/scratch/v38p156/gedi
export HDF5_USE_FILE_LOCKING=FALSE

python /mnt/lustrefs/scratch/v38p156/GEDI_Subsetter.py --dir /mnt/lustrefs/scratch/v38p156/L2A2 --roi /mnt/lustrefs/scratch/v38p156/MLB_simple.shp 