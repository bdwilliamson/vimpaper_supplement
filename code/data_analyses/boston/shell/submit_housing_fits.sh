#!/bin/sh
#SBATCH -o iotrash/slurm-%A_%a.out
#SBATCH -e iotrash/slurm-%A_%a.out

sbatch --array=1-16 ./call_housing_fits.sh