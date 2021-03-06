#!/usr/local/bin/Rscript

library(methods)
###########################################################################################################
##
## FILE:    sim_loess_0.R
##
## CREATED: 01 March 2017
##
## AUTHOR:  Brian Williamson
##
## PURPOSE: look at coverage of confidence intervals and bias for loess and np, based on cv bandwidths
##          using the standardized parameter of interest, prop. variability explained version
##          Now just doing loess with lower order
## UPDATES
## DDMMYY   INIT   COMMENTS
## ------   ----   --------
## 030317   BDW    Changed ranges
############################################################################################################

# get command line argument
args <- commandArgs(TRUE)
null <- as.logical(as.integer(args[1]))

## Function to perform K-fold Cross Validation
## Args: n - the number of rows in the data set
##       k - the number of folds
## Returns: a list with n, the number of folds
##          and which fold each observation belongs to
cv <- function(n, k){
  ## generate the k folds
  
  subsets <- replicate(1, sample(n))
  which <- rep(seq_len(k), length.out = n)
  folds <- list(n = n, K = k, subsets = subsets, which = which)
  return(folds)
}

## create SL wrapper for degree zero loess
SL.loess.0 <- function (Y, X, newX, family, obsWeights, span = 0.75, l.family = "gaussian", 
    ...) 
{
    if (family$family == "gaussian") {
        fit.loess <- suppressWarnings(loess(as.formula(paste("Y~", names(X))), 
            data = X, family = l.family, span = span, control = loess.control(surface = "direct"), 
            weights = obsWeights, degree = 0))
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
hs.df <- read.csv("../oracle_bandwidths_fine_spread.csv")
hs.df.2 <- read.csv("../oracle_bandwidths_fine_spread_smalln.csv")
hs.df.full.init <- rbind(hs.df, hs.df.2)
hs.df.full <- hs.df.full.init[order(hs.df.full.init$n), ]
B <- 100
b <- 1000
hs.df.full$seed <- hs.df.full$n + job.id
hs.df.full$B <- B
hs.df.full$p <- 2

# I have 10 jobs for each n, j combination
# which is 28*10 = 280 jobs total
# I want the first 10 to correspond to setting 1, and so on
get_current <- function(job_id) {
  vec <- rep(1:28, each = 10)
  idx <- vec[job_id]
  return(idx)
}
current <- hs.df.full[get_current(job.id), ]

getRange <- function(x) {
  return(seq(x-0.01, x+0.01, by = 0.0025))
}
h.f.l <- seq(0.0475, 0.14, by = 0.0025)
h.m.l <- seq(0.0475, 0.14, by = 0.0025)

if(h.f.l[1] <= 0) h.f.l[1] <- 0.0025
if(h.m.l[1] <= 0) h.m.l[1] <- 0.0025

# load the R package
library("vimp")
source("sim_loess_0_naives.R")
source("sim_loess_0_data.R")
source("sim_loess_0_ests.R")
set.seed(current$seed)
system.time(out <- replicate(current$B, do.one(current$n, current$p, null, current$j, h.f.l, 
                          h.m.l, b)))
saveRDS(out, file = paste0("sim_loess_0_null_", as.numeric(null),"_output_t_", job.id, ".rds"))