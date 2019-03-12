README.txt

This directory contains the code and output for analyzing the South African heart disease study data.

output/ contains the .Rdata files created using the Super Learner, created in heart_sl_fit.R and housing_sl_fits.R.
plots/ contains the figures created in heart_analysis_new_param.R

heart_sl_fit.R is meant to be run on a cluster managed by SLURM; it fits the Super Learner with a library of learners (GAMs, random forests, elastic net, gradient boosted trees), and uses cross-validation to determine the optimal weighted combination of these learners.

heart_sl_fits.R is also meant to be run on a cluster managed by SLURM; it fits Super Learner using the full fit from the previous script as outcome, and gets estimates for removing each individual variable, and the two groups defined in the manuscript. 