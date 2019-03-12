#!/bin/sh
module load R/3.4.2-foss-2016b-fh1 # edit this line for your particular cluster

sbatch --array=1-16 ./call_boston_fits.sh