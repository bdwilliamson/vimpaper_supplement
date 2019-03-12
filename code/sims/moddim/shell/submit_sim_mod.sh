#!/bin/bash
ml Python/2.7.14-foss-2016b-fh1

sbatch -M beagle --array=1-400 --time=20:00:00 ./call_sim_mod.sh
