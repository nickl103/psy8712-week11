#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks=40
#SBATCH --mem=16gb
#SBATCH -t 00:30:00
#SBATCH --mail-user=nickl103@umn.edu
#SBATCH -p msismall
cd ~/psy8712-week11
module load R/4.3.0-openblas
Rscript week11-cluster/week11-cluster.R