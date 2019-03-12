## Numerical experiments for the `vimp` paper

This repository contains code to reproduce the numerical experiments in "Nonparametric variable importance assessment using machine learning techniques" by Williamson, Gilbert, Simon, and Carone. All analyses were implemented in the freely available R and Python programming languages.

This README file provides an overview of the code available in the repository.  

-----

## The `lowdim` directory

All analyses in this directory are run in R. The necessary code for this analysis is located in `lowdim/R` and `lowdim/shell`. In these files, we assume that you use a Linux cluster with the Slurm batch scheduling system. If you use a difference batch scheduling system, the individual code files are flagged with the line where you can change batch variables. If you prefer to run the analyses locally, you may -- however, these analyses will then take a large amount of time.

### Reproducing Sections 3.1 and 3.2: experiments with a low-dimensional vector of features

To reproduce the analyses, run the following code in sequence:

1. `./lowdim/shell/submit_sim_loess_0.sh`: submits experiments for both section 3.1 (with no null features) and 3.2 (with one null feature); the code the performs these analyses is in 
    * `sim_loess_0.R`: sets up simulation parameters, runs the simulation for multiple Monte-Carlo replications
    * `sim_loess_0_data.R`: generate a dataset for the simulation
    * `sim_loess_0_ests.R`: calculate the point and interval estimators
    * `sim_loess_0_naives.R`: calculate the naive ANOVA-based estimator of importance (for bootstrapping) 
2. `./lowdim/shell/load_sim_lowdim.sh`: loads the results of the experiment, and produces plots


### Reproducing Section S3.2: experiments with cross-validation on a low-dimensional vector of covariates

To reproduce the analyses, run the following code in sequence:

1. `./lowdim/shell/submit_sim_loess_0.sh`: submits experiments for both section 3.1 (with no null features) and 3.2 (with one null feature); the code the performs these analyses is in 
    * `sim_cv.R`: sets up simulation parameters, runs the simulation for multiple Monte-Carlo replications
    * `sim_loess_0_data.R`: generate a dataset for the simulation
    * `sim_cv_ests.R`: calculate the point and interval estimators using cross-validation
    * `sim_cv_helpers.R`: helper functions for doing cross-validation
2. `./lowdim/shell/load_sim_cv.sh`: loads the results of the experiment, and produces plots


-----

## The `moddim` directory

All analyses in this directory are run in Python. The necessary code for this analysis is located in `moddim/R` and `moddim/shell`. In these files, we assume that you use a Linux cluster with the Slurm batch scheduling system. If you use a difference batch scheduling system, the individual code files are flagged with the line where you can change batch variables. If you prefer to run the analyses locally, you may -- however, these analyses will then take a large amount of time.

### Reproducing Sections 3.3 and S4: experiments with a moderate-dimensional vector of features

To reproduce the analyses, run the following code in sequence:

1. 