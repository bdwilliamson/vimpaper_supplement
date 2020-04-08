## Numerical experiments for the `vimp` paper

This repository contains code to reproduce the numerical experiments in Williamson, Gilbert, Carone, and Simon (*Biometrics*, 2020) ["Nonparametric variable importance assessment using machine learning techniques"](). All analyses were implemented in the freely available R and Python programming languages, and use the R package `vimp` version 1.1.6 and Python package `vimpy` version 1.0.0.

This README file provides an overview of the code available in the repository.  

-----

## The `lowdim` directory

All analyses in this directory are run in R. The necessary code for this analysis is located in `lowdim/R` and `lowdim/shell`. In these files, we assume that you use a Linux cluster with the Slurm batch scheduling system. If you use a difference batch scheduling system, the individual code files are flagged with the line where you can change batch variables. If you prefer to run the analyses locally, you may -- however, these analyses will then take a large amount of time.

The true values of the parameter of interest may be calculated analytically.

### Reproducing Sections 3.1 and 3.2: experiments with a low-dimensional vector of features

These analyses are run in R version 3.2.2. To reproduce the analyses, run the following code in sequence:

1. `./lowdim/shell/submit_sim_loess_0.sh`: submits experiments for both section 3.1 (with no null features) and 3.2 (with one null feature); the code that performs these analyses is in
    * `sim_loess_0.R`: sets up simulation parameters, runs the simulation for multiple Monte-Carlo replications
    * `sim_loess_0_data.R`: generate a dataset for the simulation
    * `sim_loess_0_ests.R`: calculate the point and interval estimators
    * `sim_loess_0_naives.R`: calculate the naive ANOVA-based estimator of importance (for bootstrapping)
2. `./lowdim/shell/load_sim_lowdim.sh`: loads the results of the experiment, and produces plots


### Reproducing Section S3.2: experiments with cross-validation on a low-dimensional vector of covariates

These analyses are run in R version 3.4.2. To reproduce the analyses, run the following code in sequence:

1. `./lowdim/shell/submit_sim_cv.sh`: submits experiments for both section 3.1 (with no null features) and 3.2 (with one null feature); the code that performs these analyses is in
    * `sim_cv.R`: sets up simulation parameters, runs the simulation for multiple Monte-Carlo replications
    * `sim_loess_0_data.R`: generate a dataset for the simulation
    * `sim_cv_ests.R`: calculate the point and interval estimators using cross-validation
    * `sim_cv_helpers.R`: helper functions for doing cross-validation
2. `./lowdim/shell/load_sim_cv.sh`: loads the results of the experiment, and produces plots


-----

## The `moddim` directory

All analyses in this directory are run in Python 2.7. The necessary code for this analysis is located in `moddim/R` and `moddim/shell`. In these files, we assume that you use a Linux cluster with the Slurm batch scheduling system. If you use a difference batch scheduling system, the individual code files are flagged with the line where you can change batch variables. If you prefer to run the analyses locally, you may -- however, these analyses will then take a large amount of time.

The true values of the parameter of interest may be calculated analytically in this section; however, it is a non-trivial task to do so. We computed the true values using Mathematica 11, and have saved the true values as .rds and .Rdata files in this directory.

### Reproducing Sections 3.3 and S4: experiments with a moderate-dimensional vector of features

To reproduce the analyses, first create a virtual environment (e.g., using `virtualenv`) within the `sims` directory and install the `vimpy` package. For example, if you already have `virtualenv` installed, you may run

```
virtualenv venv
venv/bin/activate

python pip install vimpy
```

Then run the following code in sequence:

1. `./moddim/shell/submit_sim_mod.sh`: submits experiments to Slurm cluster; the code that performs the analyses is:
    * `sim_moddim.py`: runs the simulation for a number of Monte-Carlo replicates
    * `sim_moddim_ests.py`: computes the naive and corrected estimators of variable importance
2. `./moddim/R/load_sim_moddim.R`: loads results of the experiment and produces plots
