#!/bin/bash
module load R/3.2.2-foss-2016b-fh1 # edit this line for your particular cluster

## first the alternative
sbatch --array=1-280 --time=8:00:00 ./call_sim_loess_0.sh 0

## now the null
sbatch --array=1-280 --time=8:00:00 ./call_sim_loess_0.sh 1