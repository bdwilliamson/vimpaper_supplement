#!/bin/bash
ml R/3.4.2-foss-2016b-fh1

## first the alternative
sbatch -M beagle --array=1-560 --time=8:00:00 ./call_sim_cv.sh 0
## now the null
sbatch -M beagle --array=1-560 --time=8:00:00 ./call_sim_cv.sh 1