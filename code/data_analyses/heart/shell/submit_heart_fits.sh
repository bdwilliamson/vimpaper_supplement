#!/bin/bash
module load R/3.4.2-foss-2016b-fh1 # edit this line for your particular cluster

sbatch --array=1-11 ./call_heart_fits.sh