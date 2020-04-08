## Supplementary materials for the R-squared `vimp` paper

This repository contains code to reproduce the analyses in "Nonparametric variable importance assessment using machine learning techniques" by Williamson, Gilbert, Carone, and Simon (*Biometrics*, 2020). All analyses were implemented in the freely available R and Python programming languages, and use the R package `vimp` version 1.1.6 and Python package `vimpy` version 1.0.0.

This README file provides an overview of the code available in the repository.  

-----

## Code directory

We have separated our code further into two sub-directories based on the two main objectives of the manuscript:

1. Numerical experiments to evaluate the operating characteristics of our proposed method under varying data-generating mechanisms. This is further divided into experiments with a low-dimensional vector of covariates and experiments with a moderate-dimensional vector of covariates.
2. An analysis of the South African heart disease study data and the Boston housing data.

Within each sub-directory, we further subdivide the code into an R directory (hosting all of the R code for the analysis), a shell directory (hosting all of the code for batch submission to a high-performance cluster computing environment), and a python directory (hosting all of the python code for the analysis). All analyses were performed on a Linux cluster using the Slurm batch scheduling system. If you use a difference batch scheduling system, the individual code files are flagged with the line where you can change batch variables. If you prefer to run the analyses locally, you may -- however, these analyses will then take a large amount of time.

-----

## Issues

If you encounter any bugs or have any specific questions about the analysis, please
[file an issue](https://github.com/bdwilliamson/vimpaper_supplementary/issues).
