#!/bin/sh
#SBATCH -o iotrash/slurm-%A_%a.out
#SBATCH -e iotrash/slurm-%A_%a.out

sbatch ./call_housing_full.sh