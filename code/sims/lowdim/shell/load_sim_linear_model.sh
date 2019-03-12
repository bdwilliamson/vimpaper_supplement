#!/bin/sh

Rscript load_sim_linear_model.R 0 0 50 $1 &

Rscript load_sim_linear_model.R 1 0 50 $1
