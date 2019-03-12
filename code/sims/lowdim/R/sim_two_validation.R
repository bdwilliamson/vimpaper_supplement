#!/usr/local/bin/Rscript

library(methods)
###########################################################################################################
##
## FILE:    sim_two_validation.R
##
## CREATED: 21 August 2018
##
## AUTHOR:  Brian Williamson
##
## PURPOSE: cv one-step, with donsker class condition relaxed by using 2 validation sets
## 
############################################################################################################

# get command line argument
args <- commandArgs(TRUE)
null <- as.logical(as.integer(args[1]))

## Function to perform K-fold Cross Validation
## Args: n - the number of rows in the data set
##       k - the number of folds
## Returns: a matrix that determines whether the given observation is train or test
##          for each fold
cv_2 <- function(n, k){
  ## generate a matrix of zeros
  folds <- matrix(0, nrow = n, ncol = k)
  ## randomly set 2 values in each row to 1 (these are test)
  sample_vec <- c(rep(0, k-2), 1, 2)
  folds_randomized <- t(apply(folds, 1, function(x) sample(sample_vec)))
  return(folds_randomized)
}
cv <- function(n, k){
  ## generate the k folds
  
  subsets <- replicate(1, sample(n))
  which <- rep(seq_len(k), length.out = n)
  folds <- list(n = n, K = k, subsets = subsets, which = which)
  return(folds)
}


## Run the simulation
## ARGS:      n - the sample size
##           bw - the matrix of bandwidths for the model. If NULL, one will be created and returned.
##            p - the number of features - always 2 for smooth case
##            j - the feature to remove
##            h - the bandwidth to try
##            b - the number of bootstrap reps
## RETURNS: the estimators for one run of the simulation 
do.one <- function(n, p = 2, null, j, h.f.l, h.m.l, b){
  
  ## generate the data
  dat <- gen_data(n, p, null)
  
  ## calculate the estimators
  estVec <- calculateEsts(dat$dat, dat$truth[j], n, j, h.f.l, h.m.l, b)
  
  return(estVec)
}

job.id <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
hs.df <- read.csv("oracle_bandwidths_fine_spread.csv")
hs.df.2 <- read.csv("oracle_bandwidths_fine_spread_smalln.csv")
hs.df.full.init <- rbind(hs.df, hs.df.2)
hs.df.full <- hs.df.full.init[order(hs.df.full.init$n), ]
B <- 50
b <- 1000
hs.df.full$seed <- hs.df.full$n + job.id
hs.df.full$B <- B
hs.df.full$p <- 2

# I have 10 jobs for each n, j combination
# which is 28*10 = 280 jobs total
# I want the first 10 to correspond to setting 1, and so on
get_current <- function(job_id) {
  vec <- rep(1:28, each = 20)
  idx <- vec[job_id]
  return(idx)
}
current <- hs.df.full[get_current(job.id), ]

# h.f.l <- seq(0.0475, 0.14, by = 0.0025)
# h.m.l <- seq(0.0475, 0.14, by = 0.0025)
h.f.l <- seq(0.1, 0.8, by = 0.05)
h.m.l <- seq(0.1, 0.8, by = 0.05)


# load the R package
library("vimp")
source("sim_lowdim_data.R")
source("sim_ests_two_validation_1.R")
source("sim_helpers.R")

set.seed(current$seed)
system.time(out <- replicate(current$B, do.one(current$n, current$p, null, current$j, h.f.l, 
                                               h.m.l, b)))
saveRDS(out, file = paste0("sim_cv_two_validation_null_", as.numeric(null),"_output_t_", job.id, ".rds"))