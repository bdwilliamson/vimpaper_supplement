## Data analyses in the R-squared `vimp` paper

This repository contains code to reproduce the data analyses in "Nonparametric variable importance assessment using machine learning techniques" by Williamson, Gilbert, Carone, and Simon (*Biometrics*, 2020). All analyses were implemented in the freely available R and Python programming languages, and use the R package `vimp` version 1.1.6 and Python package `vimpy` version 1.0.0.

This README file provides an overview of the code available in the repository. All analyses in this directory are run in R. The necessary code for this analysis is located in `lowdim/R` and `lowdim/shell`. In these files, we assume that you use a Linux cluster with the Slurm batch scheduling system. If you use a difference batch scheduling system, the individual code files are flagged with the line where you can change batch variables. If you prefer to run the analyses locally, you may -- however, these analyses will then take a large amount of time.

-----

## The `heart` directory


### Reproducing Section 4: results from the South African heart disease study data

These analyses are run in R version 3.2.2. To reproduce the analyses, run the following code in sequence:

1. `./heart/shell/submit_heart_full.sh`: computes the estimator of the regression function based on all covariates using the Super Learner; the relevant code is in `heart/R/heart_sl_fit.R`
2. `./heart/shell/submit_heart_fits.sh`: computes the estimators of the reduced regression functions based on the results from (1) and the sequential regression procedure; the relevant code is in `heart/R/heart_sl_fits.R`
2. `./heart/R/heart_data_analysis.R`: loads in the results, computes variable importance point and interval estimators, and produces plots

-----

## The `boston` directory

### Reproducing Section S5: results from the Boston housing study data

These analyses are run in R version 3.2.2. To reproduce the analyses, run the following code in sequence:

1. `./boston/shell/submit_boston_full.sh`: computes the estimator of the regression function based on all covariates using the Super Learner; the relevant code is in `boston/R/boston_sl_fit.R`
2. `./boston/shell/submit_boston_fits.sh`: computes the estimators of the reduced regression functions based on the results from (1) and the sequential regression procedure; the relevant code is in `boston/R/boston_sl_fits.R`
2. `./boston/R/boston_data_analysis.R`: loads in the results, computes variable importance point and interval estimators, and produces plots
