#!/usr/local/bin/Rscript

library(methods)
###########################################################################################################
##
## FILE:    sim_loess_0_compare_lm.R
##
## CREATED: 31 December 2018
##
## AUTHOR:  Brian Williamson
##
## PURPOSE: compare simple OLS to proposed estimator, for simple low-dimensional setting
## 
## UPDATES
## DDMMYY   INIT   COMMENTS
## ------   ----   --------
############################################################################################################

## set up static args
# job_ids <- 1:280
B <- 100
b <- 1000
ns <- rep(c(100, 300, 500, 700, seq(1000, 10000, by = 1000)), each = 2)
js <- rep(c(1, 2), length(ns)/2)


## function that gets an OLS-based estimate of R^2
get_lm_ests <- function(data, indices, j) {
    ## set up stuff for boot
    d <- data[indices, ]
    small_d <- d[, -j]
    names(small_d) <- c("x", "y")

    ## fit the linear models
    mod <- lm(y ~ ., data = d)
    small_mod <- lm(y ~ ., data = small_d)
    
    ## get estimators
    r2 <- 1 - mean((d$y - fitted(mod))^2)/mean((d$y - mean(d$y))^2)
    red_r2 <- 1 - mean((d$y - fitted(small_mod))^2)/mean((d$y - mean(d$y))^2)
    return(r2 - red_r2)
}

get_current <- function(job_id) {
  vec <- rep(1:28, each = 10)
  idx <- vec[job_id]
  return(idx)
}

## function that performs one simulation replicate
## ARGS: dat - the dataset
##         j - the covariate to remove
##         b - the number of bootstrap replicates
do_one <- function(dat, j, b) {
    ## set up the data for the given j
    small.dat <- dat[,-j]
    names(small.dat) <- c("x", "y")
    
    
    ## calculate the OLS-based difference in R^2
    simple_r2 <- get_lm_ests(dat, 1:dim(dat)[1], j)

    ## bootstrap a CI
    simple_r2s <- boot::boot(data = dat, statistic = get_lm_ests, R = b, j = j)
    simple_r2s_ci_init <- boot::boot.ci(simple_r2s, type = "perc")
    simple_r2_ci <- simple_r2s_ci_init$percent[4:5]

    ## return output
    out <- c(simple_r2, simple_r2_ci)
    names(out) <- c("ols.r2", "ols.r2.cil", "ols.r2.ciu")
    return(out)
}

# for (i in job_ids) {
i <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
    ## get current sample size, covariate of interest
    current_n <- ns[get_current(i)]
    current_j <- js[get_current(i)]

    ## start the simulation
    set.seed(current_n + i)
    ## read in data
    dat <- readRDS(paste0("sim_lowdim_dataset_n_", current_n, "_j_", current_j, ".rds"))
    system.time(out <- sapply(1:B, function(x) do_one(dat[[ifelse(i %% 10 > 0, i %% 10, 10)]][[x]]$dat, current_j, b), simplify = FALSE))    
    out_df <- do.call(rbind.data.frame, out)
    names(out_df) <- c("ols.r2", "ols.r2.cil", "ols.r2.ciu")
    out_df$cover <- out_df$ols.r2.cil <= dat[[1]][[1]]$truth[current_j] & out_df$ols.r2.ciu >= dat[[1]][[1]]$truth[current_j]
    ## save it off
    saveRDS(out, file = paste0("sim_loess_0_null_0_output_lm_t_", i, ".rds"))
# }

