#!/usr/local/bin/Rscript

library(methods)
###########################################################################################################
##
## FILE:    sim_linear_model.R
##
## CREATED: 24 August 2018
##
## AUTHOR:  Brian Williamson
##
## PURPOSE: compare linear regression to my approach, both with and without interactions, 
##          both under the null and not
############################################################################################################

# get command line argument
args <- commandArgs(TRUE)
null <- as.logical(as.integer(args[1]))
interaction <- as.logical(as.integer(args[2]))
B <- as.integer(args[3])
loess_ord <- as.integer(args[4])

if (loess_ord == 999) {
  loess_ord <- "sl"
}

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
do.one <- function(n, p = 2, null, interaction, j, h.f.l, h.m.l, b, ord){
  
  ## generate the data
  dat <- gen_data(n, p, interaction, null)
  
  ## calculate the estimators
  estVec <- calculateEsts(dat$dat, dat$truth[j], n, j, h.f.l, h.m.l, b, ord, interaction)
  
  return(estVec)
}

job_id <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
hs_df <- read.csv("oracle_bandwidths_fine_spread.csv")
hs_df_2 <- read.csv("oracle_bandwidths_fine_spread_smalln.csv")
hs_df_full_init <- rbind(hs_df, hs_df_2)
hs_df_full <- hs_df_full_init[order(hs_df_full_init$n), ]
b <- 1000
hs_df_full$seed <- hs_df_full$n + job_id + hs_df_full$j
hs_df_full$B <- B
hs_df_full$p <- 2

# I have 10 jobs for each n, j combination
# which is 28*10 = 280 jobs total
# I want the first 10 to correspond to setting 1, and so on
get_current <- function(job_id, B) {
  vec <- rep(1:28, each = 1000/B)
  idx <- vec[job_id]
  return(idx)
}
current <- hs_df_full[get_current(job_id, B), ]

## define loess with interactions for Super Learner
SL.loess.interaction <- function (Y, X, newX, family, obsWeights, span = 0.75, l.family = "gaussian", degree = 0, ...) 
{
  if (family$family == "gaussian") {
    fit.loess <- loess(Y~.^2, 
                       data = X, family = l.family, span = span, control = loess.control(surface = "direct"), 
                       weights = obsWeights,
                       degree = degree)
  }
  if (family$family == "binomial") {
    stop("family = binomial() not currently implemented for SL.loess")
  }
  pred <- predict(fit.loess, newdata = newX)
  fit <- list(object = fit.loess)
  out <- list(pred = pred, fit = fit)
  class(out$fit) <- c("SL.loess")
  return(out)
}

if (loess_ord == 0) {
  h.f.l <- seq(0.025, 0.555, by = 0.05)
  h.m.l <- seq(0.025, 0.555, by = 0.05)
} else if (loess_ord == 1) {
  h.f.l <- seq(0.3, 0.8, by = 0.05)
  h.m.l <- seq(0.3, 0.8, by = 0.05)
} else {
  library("SuperLearner")
  library("xgboost")
  library("gam")
  library("splines")
  library("foreach")
  if (current$n < 1000) {
    shrink <- 0.1
  } else if (current$n < 5000) {
    shrink <- 0.01
  } else {
    shrink <- 0.001
  }
  tune_params <- list(max_depth = 1, shrinkage = shrink)
  learners <- create.Learner("SL.xgboost", tune = tune_params, detailed_names = TRUE, name_prefix = "xgb")
  tune_params_2 <- list(degree = c(0, 1))
  learners_2 <- create.Learner("SL.loess.interaction", tune = tune_params_2, detailed_names = TRUE, name_prefix = "loess")
  h.f.l <- h.m.l <- c(learners$names, learners_2$names, "SL.mean")
}


# load the R package
library("vimp")
source("sim_lowdim_lm_data.R")
source("sim_ests_lm.R")
source("sim_helpers.R")

set.seed(current$seed)
system.time(out <- replicate(current$B, do.one(current$n, current$p, null, interaction, current$j, h.f.l, 
                                               h.m.l, b, loess_ord)))
saveRDS(out, file = paste0("sim_lm_null_", as.numeric(null), 
                           "_interaction_", as.numeric(interaction), 
                           "_output_t_", job_id, "_", loess_ord, ".rds"))