#!/bin/bash
#SBATCH --mem-per-cpu=4000
#SBATCH --time=48:00:00
#SBATCH --job-name=stan_algae
#SBATCH --mail-user=pablo.gutierrez1@upr.edu
#SBATCH --mail-type=ALL
#SBATCH --workdir=/work/rthomas/oospina/QPAFeb17
#SBATCH --output=slurm_output.txt
#SBATCH --ntasks=24
#SBATCH --partition=batch

Rscript --vanilla QPA_Algae_auto.R QPA_Feb17.csv sources_QPA_Feb.csv QPA_FoodWeb.stan

