#!/usr/local/bin/Rscript
##############################################################################################################################
##
## FILE:    load_sim_linear_model.R
##
## CREATED: 26 October 2018
##
## AUTHOR:  Brian Williamson
##
## PURPOSE: Load results from sims to test performance in a linear model, comparing
##          vanilla R^2 to population anova, population R^2, without correction
##
##                           UPDATES
## DDMMYY    INIT    COMMENTS
## ---------------------------
##############################################################################################################################
if (!is.na(Sys.getenv("RSTUDIO", unset = NA))) {
  setwd("~/Documents/Papers/vimpaper/sim_code/lowdim/")
  null <- 0
  interaction <- 0
  B <- 50
  loess_ord <- 0
}

library("readr")
source("~/Documents/Papers/vimextension/vimparameters/sim_code/vimext_simulation_helpers.R")

# get command line argument
if (is.na(Sys.getenv("RSTUDIO", unset = NA))) {
  args <- commandArgs(TRUE)
  null <- as.logical(as.integer(args[1]))
  interaction <- as.logical(as.integer(args[2]))
  B <- as.integer(args[3])
  loess_ord <- as.integer(args[4])
}

if (loess_ord == 999) {
  loess_ord <- "sl"
}


get_current <- function(job_id, B) {
  vec <- rep(1:28, each = 1000/B)
  idx <- vec[job_id]
  return(idx)
}


## First read in all of the data
## Everything is the same but the n
#ns <- seq(1000, 10000, by = 1000)
ns <- c(100, 300, 500, 700, seq(1000, 10000, by = 1000))
js <- c(1,2)
num_estimators <- 6

hs.df <- read.csv("./code/oracle_bandwidths_fine_spread.csv")
hs.df.2 <- read.csv("./code/oracle_bandwidths_fine_spread_smalln.csv")
hs.df.full.init <- rbind(hs.df, hs.df.2)
hs.df.full <- hs.df.full.init[order(hs.df.full.init$n), ]

## load in the data from small sample size calculations
output <- data.frame(bias = rep(0, num_estimators*dim(hs.df.full)[1]), variance = rep(0, num_estimators*dim(hs.df.full)[1]),
                        mse = rep(0, num_estimators*dim(hs.df.full)[1]), sd = rep(0, num_estimators*dim(hs.df.full)[1]), n = rep(0, num_estimators*dim(hs.df.full)[1]), 
                        j = rep(0, num_estimators*dim(hs.df.full)[1]), cover = rep(0, num_estimators*dim(hs.df.full)[1]))

## files to read in
vec <- 1:(28*(1000/B))
files <- as.list(paste("./output/sim_linear_model/sim_lm_null_", as.numeric(null), "_interaction_", as.numeric(interaction), "_output_t_", vec, "_", loess_ord, ".rds", sep = ""))

## function to read in files
read_func <- function(file) {
  return(tryCatch(readRDS(file), error = function(e) NA))
}
## set up row indices to look in for np anova, np r^2, linear model r^2
## indices are: truth, "naive", "corrected", "naive" cil, "naive" ciu, "corrected" cil, "corrected" ciu
indices_anova <- c(4, 2, 3, NA, NA, 6, 7)
indices_r2 <- c(4, 8, NA, 10, 11, NA, NA)
indices_lm <- c(4, 12, 15, 13, 14, 16, 17)
## load the data, put in output
idxs <- list(1:18, 1:18)
if (interaction) idxs <- list(1:28, 1:28)
idx <- unlist(idxs[ifelse(null == 1, 2, 1)])
for (i in idx) {
  ## load the data for each n, j (there are 10 jobs for each)
  output_n_j <- lapply(files[get_current(vec, B) == i], read_func)
  ## put into a dataframe, paste by columns
  out <- do.call("cbind.data.frame", output_n_j)
  
  ## get current n, j, etc
  cur <- hs.df.full[i, ]
  
  # calculate bias, sd, etc.
  current_anova <- calculateBias(out, n = cur$n, j = cur$j, indices = indices_anova)
  current_r2 <- calculateBias(out, n = cur$n, j = cur$j, indices = indices_r2)
  current_lm <- calculateBias(out, n = cur$n, j = cur$j, indices = indices_lm)
  
  ## calculate coverage of CIs
  cover_anova <- mean(apply(out, 2, function(x) x[indices_anova[6]] <= x[indices_anova[1]] & x[indices_anova[7]] >= x[indices_anova[1]]), na.rm = TRUE)
  cover_r2 <- mean(apply(out, 2, function(x) x[indices_r2[4]] <= x[indices_r2[1]] & x[indices_r2[5]] >= x[indices_r2[1]]), na.rm = TRUE)
  cover_lm_simple <- mean(apply(out, 2, function(x) x[indices_lm[4]] <= x[indices_lm[1]] & x[indices_lm[5]] >= x[indices_lm[1]]), na.rm = TRUE)
  cover_lm_interact <- mean(apply(out, 2, function(x) x[indices_lm[6]] <= x[indices_lm[1]] & x[indices_lm[7]] >= x[indices_lm[1]]), na.rm = TRUE)
  
  ## add on coverage
  current <- rbind(current_anova, current_r2, current_lm)
  current$cover <- c(NA, cover_anova, cover_r2, NA, cover_lm_simple, cover_lm_interact)
  
  ## add to output matrix
  output[1:num_estimators + num_estimators*(i - 1), ] <- current
}

## tack on names
output$type <- rep(c("naive_anova", "correct_anova", "naive_r2", "correct_r2", "lm_simple", "lm_interact"), dim(output)[1]/num_estimators)

## order by n
output <- output[order(output$n), ]

## remove zeroes
output <- output[rowSums(output[, 1:7], na.rm = TRUE) > 0, ]
##########################################################
## PLOTS 
##########################################################
c25 <- c("dodgerblue2","#E31A1C", # red
         "green4",
         "#6A3D9A", # purple
         "#FF7F00", # orange
         "black","gold1",
         "skyblue2","#FB9A99", # lt pink
         "palegreen2",
         "#CAB2D6", # lt purple
         "#FDBF6F", # lt orange
         "gray70", "khaki2",
         "maroon","orchid1","deeppink1","blue1","steelblue4",
         "darkturquoise","green1","yellow4","yellow3",
         "darkorange4","brown")[-2]
# pie(rep(1,25), col=c25)
fig.width <- 2590
fig.height <- fig.width
params <- c("np_r2", "np_anova", "lm_r2")
est_nms <- list(c("Proposed R^2"), c("Proposed ANOVA", "Naive ANOVA"), c("Simple LM R^2", "Interaction LM R^2"))
est_types <- list(c("naive_r2"), c("correct_anova", "naive_anova"), c("lm_simple", "lm_interact"))
pchs <- list(list(c(16, 8)), list(c(17, 2), c(15, 0)), list(c(13, 4), c(9, 7)))
lgnd_pchs <- list(c(16, 16, 8, 8), c(17, 15, 17, 2), c(13, 9, 13, 4))

#############################
## bias plot with error bars
#############################
for (i in 1:length(est_nms)) {
  png(paste0("plots/bias_vs_n_lowdim_lm_loess_", loess_ord, "_null_", as.numeric(null), "_interact_", as.numeric(interaction), "_", params[i], ".png"), width = fig.width, height = fig.height, units = "px", res = 300)
  lgnd_pos <- "bottomleft"
  if (i == 3 & interaction) lgnd_pos <- "right"
  plotSummaryLowdim(output, type = "bias", ylim = c(-4, 1), ests = est_nms[[i]], pch = pchs[[i]],
                    lgnd.pch = lgnd_pchs[[i]], lgnd.txt = c("j = 1", "j = 2"), lgnd.col = c(c25[i:(i+1)][1:length(est_nms[[i]])], rep("black", 2)), est.type = est_types[[i]],
                    cex = 1.5, cex.lab = 1.25, cex.axis = 1.25, lgnd.cex = 1, lgnd.pos = lgnd_pos)
  dev.off()
}

#############################
## coverage
#############################
for (i in 1:length(est_nms)) {
  png(paste0("plots/cover_vs_n_lowdim_lm_loess_", loess_ord, "_null_", as.numeric(null), "_interact_", as.numeric(interaction), "_", params[i], ".png"), width = fig.width, height = fig.height, units = "px", res = 300)
  lgnd_pos <- "bottomright"
  if (i == 3 & interaction) lgnd_pos <- "right"
  plotSummaryLowdim(output, type = "coverage", ylim = c(0, 1), ests = est_nms[[i]], pch = pchs[[i]],
                    lgnd.pch = lgnd_pchs[[i]], lgnd.txt = c("j = 1", "j = 2"), lgnd.col = c(c25[i:(i+1)][1:length(est_nms[[i]])], rep("black", 2)), est.type = est_types[[i]],
                    cex = 1.5, cex.lab = 1.25, cex.axis = 1.25, lgnd.cex = 1, lgnd.pos = lgnd_pos)
  dev.off()
}

#############################
## mean squared error
#############################
for (i in 1:length(est_nms)) {
  png(paste0("plots/mse_vs_n_lowdim_lm_loess_", loess_ord, "_null_", as.numeric(null), "_interact_", as.numeric(interaction), "_", params[i], ".png"), width = fig.width, height = fig.height, units = "px", res = 300)
  plotSummaryLowdim(output, type = "mse", ylim = c(0, 2), ests = est_nms[[i]], pch = pchs[[i]],
                    lgnd.pch = lgnd_pchs[[i]], lgnd.txt = c("j = 1", "j = 2"), lgnd.col = c(c25[i:(i+1)][1:length(est_nms[[i]])], rep("black", 2)), est.type = est_types[[i]],
                    cex = 1.5, cex.lab = 1.25, cex.axis = 1.25, lgnd.cex = 1, lgnd.pos = "topright")
  dev.off()
}

#############################
## variance
#############################
for (i in 1:length(est_nms)) {
  png(paste0("plots/var_vs_n_lowdim_lm_loess_", loess_ord, "_null_", as.numeric(null), "_interact_", as.numeric(interaction), "_", params[i], ".png"), width = fig.width, height = fig.height, units = "px", res = 300)
  plotSummaryLowdim(output, type = "variance", ylim = c(0, 2), ests = est_nms[[i]], pch = pchs[[i]],
                    lgnd.pch = lgnd_pchs[[i]], lgnd.txt = c("j = 1", "j = 2"), lgnd.col = c(c25[i:(i+1)][1:length(est_nms[[i]])], rep("black", 2)), est.type = est_types[[i]],
                    cex = 1.5, cex.lab = 1.25, cex.axis = 1.25, lgnd.cex = 1, lgnd.pos = "bottomleft")
  dev.off()
}

#####################################################################################
##
## Ratio of variances
##
#####################################################################################
# png(paste0("plots/var_ratio_vs_n_lowdim_cv_null_", as.numeric(null), ".png"), width = fig.width, height = fig.height, units = "px", res = 300)
# par(cex.axis = 1.6, cex.lab = 2, mar = c(5, 5, 0, 2) + 0.1)
# plot(subset(output.lo, output.lo$type == "naive")$n, subset(output.lo, output.lo$type == "naive")$var/subset(output.lo, output.lo$type == "onestep")$var,
#      ylim = c(0, 4), ylab = "Ratio of estimated variances (naive/proposed)", xlab = "n",
#      pch = rep(c(16, 8), 8), col = "black", axes = FALSE, cex = 2)
# title(main = "VARIANCE", line = -2, cex.main = 2)
# abline(h = 0)
# abline(h = 1, lty = 2)
# axis(side = 1, at = c(100, 300, 500, 700, seq(1000, 10000, by=1000)))
# axis(side = 2, at = seq(-5, 3, by = 1))
# legend("topleft", legend = c("j=1", "j=2"), col = c("black", "black"), pch = c(16, 8), cex = 1.25)
# box()
# dev.off()
