#!/usr/local/bin/Rscript
##############################################################################################################################
##
## FILE:    load_sim_cv.R
##
## CREATED: 19 Jan 2018
##
## AUTHOR:  Brian Williamson
##
## PURPOSE: Load the results from the alternative hypothesis sims, standardized parameter,
##          proportion of total variability explained
##
##                           UPDATES
## DDMMYY    INIT    COMMENTS
## ---------------------------
##############################################################################################################################
library(readr)
source("simulation_helper_functions.R")

# get command line argument
if (is.na(Sys.getenv("RSTUDIO", unset = NA))) {
  args <- commandArgs(TRUE)
  null <- as.logical(as.integer(args[1]))
}


get_current <- function(job_id) {
  vec <- rep(1:28, each = 20)
  idx <- vec[job_id]
  return(idx)
}


## First read in all of the data
## Everything is the same but the n
#ns <- seq(1000, 10000, by = 1000)
ns <- c(100, 300, 500, 700, seq(1000, 10000, by = 1000))
js <- c(1,2)

hs.df <- read.csv("../oracle_bandwidths_fine_spread.csv")
hs.df.2 <- read.csv("../oracle_bandwidths_fine_spread_smalln.csv")
hs.df.full.init <- rbind(hs.df, hs.df.2)
hs.df.full <- hs.df.full.init[order(hs.df.full.init$n), ]

## load in the data from small sample size calculations
output.lo <- data.frame(bias = rep(0, 2*dim(hs.df.full)[1]), variance = rep(0, 2*dim(hs.df.full)[1]),
                        mse = rep(0, 2*dim(hs.df.full)[1]), sd = rep(0, 2*dim(hs.df.full)[1]), n = rep(0, 2*dim(hs.df.full)[1]), 
                        j = rep(0, 2*dim(hs.df.full)[1]), cover = rep(0, 2*dim(hs.df.full)[1]))

vec <- 1:560
files <- as.list(paste("./output/sim_cv_two_validation_null_", as.numeric(null), "_output_t_", vec, ".rds", sep = ""))

read_func <- function(file) {
  return(tryCatch(readRDS(file), error = function(e) NA))
}

idxs <- list(1:28, 1:28)
idx <- unlist(idxs[ifelse(null == 1, 2, 1)])
for (i in idx) {
  ## load the data for each n, j (there are 10 jobs for each)
  output_n_j <- lapply(files[get_current(vec) == i], read_func)
  ## put into a dataframe, paste by columns
  out <- do.call("cbind.data.frame", output_n_j)
  
  ## get current n, j, etc
  cur <- hs.df.full[i, ]
  
  # accidentally plugged in the wrong truth; fix this FOR NULL ONLY
  if (null == 1) {
    truths <- c((500/729)/(1+500/729), 0)
    out[4, ] <- rep(truths[ifelse(i %% 2 == 1, 1, 2)], dim(out)[2])
  }
  
  # calculate bias, sd, etc.
  current.lo <- calculateVimpaperBias(out, n = cur$n, j = cur$j)
  
  ## calculate coverage of CIs
  cover.o.lo <- mean(apply(out, 2, function(x) x[6] <= x[4] & x[7] >= x[4]))

  ## add on coverage
  current.lo$cover <- c(NA, cover.o.lo)
  
  ## add to output matrix
  output.lo[1:2 + 2*(i - 1), ] <- current.lo
}

## tack on names
output.lo$type <- c("naive", "onestep")

## order by n
output.lo <- output.lo[order(output.lo$n), ]
# remove the zeros
output.lo <- output.lo[output.lo$n != 0, ]
## save it off
saveRDS(output.lo, paste0("output_loess_cv_two_validation_null_", as.numeric(null), ".rds"))
##########################################################
## PLOTS FOR LOCAL LINEAR LOESS SMOOTHING
##########################################################
#############################
## bias plot with error bars
#############################
fig.width <- 2590
fig.height <- fig.width

png(paste0("plots/bias_vs_n_lowdim_cv_two_validation_null_", as.numeric(null), ".png"), width = fig.width, height = fig.height, units = "px", res = 300)
plotSummaryLowdim(output.lo, type = "bias", ylim = c(-2, 3), ests = c("Proposed", "Naive"), pch = list(c(16, 8), c(18, 9)),
                  lgnd.pch = c(16, 18, 16, 8), lgnd.txt = c("j = 1", "j = 2"), lgnd.col = c("blue", "red", "black", "black"), est.type = c("onestep", "naive"),
                  cex = 1.5, cex.lab = 1.25, cex.axis = 1.25, lgnd.cex = 1)
dev.off()

#############################
## coverage
#############################
png(paste0("plots/cover_vs_n_lowdim_cv_two_validation_null_", as.numeric(null), ".png"), width = fig.width, height = fig.height, units = "px", res = 300)
plotSummaryLowdim(output.lo, type = "coverage", ylim = c(0, 1), ests = c("Proposed", "Naive"), pch = list(c(16, 8), c(18, 9)),
                  lgnd.pch = c(16, 18, 16, 8), lgnd.txt = c("j = 1", "j = 2"), lgnd.col = c("blue", "red", "black", "black"), est.type = c("onestep", "naive"),
                  lgnd.pos = "bottomright", lgnd.cex = 1,
                  cex = 1.5, cex.lab = 1.25, cex.axis = 1.25)
dev.off()

#############################
## mean squared error
#############################
png(paste0("plots/mse_vs_n_lowdim_cv_two_validation_null_", as.numeric(null), ".png"), width = fig.width, height = fig.height, units = "px", res = 300)
plotSummaryLowdim(output.lo, type = "mse", ylim = c(0, 2), ests = c("Proposed", "Naive"), pch = list(c(16, 8), c(18, 9)),
                  lgnd.pch = c(16, 18, 16, 8), lgnd.txt = c("j = 1", "j = 2"), lgnd.col = c("blue", "red", "black", "black"), est.type = c("onestep", "naive"),
                  lgnd.pos = "topright", lgnd.cex = 1.5,
                  cex = 1.5, cex.lab = 1.25, cex.axis = 1.25)
dev.off()

#############################
## variance
#############################
png(paste0("plots/var_vs_n_lowdim_cv_two_validation_null_", as.numeric(null), ".png"), width = fig.width, height = fig.height, units = "px", res = 300)
plotSummaryLowdim(output.lo, type = "variance", ylim = c(0, 2), ests = c("Proposed", "Naive"), pch = list(c(16, 8), c(18, 9)),
                  lgnd.pch = c(16, 18, 16, 8), lgnd.txt = c("j = 1", "j = 2"), lgnd.col = c("blue", "red", "black", "black"), est.type = c("onestep", "naive"),
                  lgnd.pos = "right", lgnd.cex = 1,
                  cex = 1.5, cex.lab = 1.25, cex.axis = 1.25)
dev.off()

#####################################################################################
##
## Ratio of variances
##
#####################################################################################
png(paste0("plots/var_ratio_vs_n_lowdim_cv_two_validation_null_", as.numeric(null), ".png"), width = fig.width, height = fig.height, units = "px", res = 300)
par(cex.axis = 1.6, cex.lab = 2, mar = c(5, 5, 0, 2) + 0.1)
plot(subset(output.lo, output.lo$type == "naive")$n, subset(output.lo, output.lo$type == "naive")$var/subset(output.lo, output.lo$type == "onestep")$var,
     ylim = c(0, 4), ylab = "Ratio of estimated variances (naive/proposed)", xlab = "n",
     pch = rep(c(16, 8), 8), col = "black", axes = FALSE, cex = 2)
title(main = "VARIANCE", line = -2, cex.main = 2)
abline(h = 0)
abline(h = 1, lty = 2)
axis(side = 1, at = c(100, 300, 500, 700, seq(1000, 10000, by=1000)))
axis(side = 2, at = seq(-5, 3, by = 1))
legend("topleft", legend = c("j=1", "j=2"), col = c("black", "black"), pch = c(16, 8), cex = 1.25)
box()
dev.off()
