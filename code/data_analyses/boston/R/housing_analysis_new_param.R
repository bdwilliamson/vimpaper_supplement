############################################################################
##
## FILE: housing_analysis_new_param.R
##
## CREATED: 28 February 2017 by Brian Williamson
##
## PURPOSE: Analysis of the Boston housing data
##
## INPUTS: Boston housing data from MASS package
##
## OUTPUTS:
##
## UPDATES:
## DDMMYY INIT COMMENTS
## ------ ---- --------
## 130317 BDW  Added plots with grouped and individual features separate
## 050717 BDW  Updated directory structure; changed mention of "one-step" to "proposed"
## 210717 BDW  Updated plots with higher resolution
##             Updated to personal directory
############################################################################
## load variable importance functions
# setwd("C:/Users/Brian Williamson/Dropbox/Additive Models Project Summer 2015/manuscript")
# setwd("C:/Users/brianw26/Dropbox/Additive Models Project Summer 2015/manuscript")
setwd("~/Documents/Papers/vimpaper/sim_code/data_analyses/housing")

# source("../../variableImportance.R")
# source("../../variableImportanceSE.R")
# source("../../variableImportanceIC.R")
# source("../../variableImportanceCI.R")
# source("../../variableImportanceIC.diagnose.R")
# source("../../variableImportanceWinsor.R")
# source("../../winsorize.R")
library("vimp")


############################################
## BOSTON HOUSING DATA
############################################

library("MASS")

## load the housing data
data(Boston)

## get the number of samples and m
dim(Boston)
## now 506 samples
y <- Boston$medv
n <- dim(Boston)[1]

## 16 variables/groups we might be interested in
## load the full fit 
full.fit <- readRDS("output/housing_full_fit.Rdata")
## check calibration
mean(full.fit - y)

## load the small fits
small.access.fit <- readRDS("output/small_fit_access.Rdata")
small.age.fit <- readRDS("output/small_fit_age.Rdata")
small.black.fit <- readRDS("output/small_fit_black.Rdata")
small.chas.fit <- readRDS("output/small_fit_chas.Rdata")
small.crim.fit <- readRDS("output/small_fit_crim.Rdata")
small.dis.fit <- readRDS("output/small_fit_dis.Rdata")
small.indus.fit <- readRDS("output/small_fit_indus.Rdata")
small.lstat.fit <- readRDS("output/small_fit_lstat.Rdata")
small.neigh.fit <- readRDS("output/small_fit_neighborhood.Rdata")
small.ptratio.fit <- readRDS("output/small_fit_ptratio.Rdata")
small.nox.fit <- readRDS("output/small_fit_nox.Rdata")
small.rad.fit <- readRDS("output/small_fit_rad.Rdata")
small.rm.fit <- readRDS("output/small_fit_rm.Rdata")
small.str.fit <- readRDS("output/small_fit_structure.Rdata")
small.tax.fit <- readRDS("output/small_fit_tax.Rdata")
small.zin.fit <- readRDS("output/small_fit_zn.Rdata")

## check calibration
mean(small.access.fit - full.fit)
mean(small.access.fit - y)
mean(small.age.fit - full.fit)
mean(small.age.fit - y)
mean(small.black.fit - full.fit)
mean(small.black.fit - y)
mean(small.chas.fit - full.fit)
mean(small.chas.fit - y)
mean(small.crim.fit - full.fit)
mean(small.crim.fit - y)
mean(small.dis.fit - full.fit)
mean(small.dis.fit - y)
mean(small.indus.fit - full.fit)
mean(small.indus.fit - y)
mean(small.lstat.fit - full.fit)
mean(small.lstat.fit - y)
mean(small.neigh.fit - full.fit)
mean(small.neigh.fit - y)
mean(small.ptratio.fit - full.fit)
mean(small.ptratio.fit - y)
mean(small.nox.fit - full.fit)
mean(small.nox.fit - y)
mean(small.rad.fit - full.fit)
mean(small.rad.fit - y)
mean(small.rm.fit - full.fit)
mean(small.rm.fit - y)
mean(small.str.fit - full.fit)
mean(small.str.fit - y)
mean(small.tax.fit - full.fit)
mean(small.tax.fit - y)
mean(small.zin.fit - full.fit)
mean(small.zin.fit - y)

####################################################
## STANDARDIZED PARAMETER
####################################################
## removing all variables
est.all <- variableImportance(full.fit, mean(y), y, n, TRUE)

## Estimates (naive and onestep)
est.access.std <- variableImportance(full.fit, small.access.fit, y, n, TRUE, return_naive = TRUE)
est.age.std <- variableImportance(full.fit, small.age.fit, y, n, TRUE, return_naive = TRUE)
est.black.std <- variableImportance(full.fit, small.black.fit, y, n, TRUE, return_naive = TRUE)
est.chas.std <- variableImportance(full.fit, small.chas.fit, y, n, TRUE, return_naive = TRUE)
est.crim.std <- variableImportance(full.fit, small.crim.fit, y, n, TRUE, return_naive = TRUE)
est.dis.std <- variableImportance(full.fit, small.dis.fit, y, n, TRUE, return_naive = TRUE)
est.indus.std <- variableImportance(full.fit, small.indus.fit, y, n, TRUE, return_naive = TRUE)
est.lstat.std <- variableImportance(full.fit, small.lstat.fit, y, n, TRUE, return_naive = TRUE)
est.neigh.std <- variableImportance(full.fit, small.neigh.fit, y, n, TRUE, return_naive = TRUE)
est.ptratio.std <- variableImportance(full.fit, small.ptratio.fit, y, n, TRUE, return_naive = TRUE)
est.nox.std <- variableImportance(full.fit, small.nox.fit, y, n, TRUE, return_naive = TRUE)
est.rad.std <- variableImportance(full.fit, small.rad.fit, y, n, TRUE, return_naive = TRUE)
est.rm.std <- variableImportance(full.fit, small.rm.fit, y, n, TRUE, return_naive = TRUE)
est.str.std <- variableImportance(full.fit, small.str.fit, y, n, TRUE, return_naive = TRUE)
est.tax.std <- variableImportance(full.fit, small.tax.fit, y, n, TRUE, return_naive = TRUE)
est.zin.std <- variableImportance(full.fit, small.zin.fit, y, n, TRUE, return_naive = TRUE)

est.std <- rbind(est.access.std, est.age.std, est.black.std, est.chas.std, est.crim.std, est.dis.std, est.indus.std, 
                 est.lstat.std, est.neigh.std, est.ptratio.std, est.nox.std,
                 est.rad.std, est.rm.std, est.str.std, est.tax.std, est.zin.std)

## SEs
se.all <- variableImportanceSE(full.fit, mean(y), y, n, TRUE)

se.access.std <- variableImportanceSE(full.fit, small.access.fit, y, n, TRUE)
se.age.std <- variableImportanceSE(full.fit, small.age.fit, y, n, TRUE)
se.black.std <- variableImportanceSE(full.fit, small.black.fit, y, n, TRUE)
se.chas.std <- variableImportanceSE(full.fit, small.chas.fit, y, n, TRUE)
se.crim.std <- variableImportanceSE(full.fit, small.crim.fit, y, n, TRUE)
se.dis.std <- variableImportanceSE(full.fit, small.dis.fit, y, n, TRUE)
se.indus.std <- variableImportanceSE(full.fit, small.indus.fit, y, n, TRUE)
se.lstat.std <- variableImportanceSE(full.fit, small.lstat.fit, y, n, TRUE)
se.neigh.std <- variableImportanceSE(full.fit, small.neigh.fit, y, n, TRUE)
se.ptratio.std <- variableImportanceSE(full.fit, small.ptratio.fit, y, n, TRUE)
se.nox.std <- variableImportanceSE(full.fit, small.nox.fit, y, n, TRUE)
se.rad.std <- variableImportanceSE(full.fit, small.rad.fit, y, n, TRUE)
se.rm.std <- variableImportanceSE(full.fit, small.rm.fit, y, n, TRUE)
se.str.std <- variableImportanceSE(full.fit, small.str.fit, y, n, TRUE)
se.tax.std <- variableImportanceSE(full.fit, small.tax.fit, y, n, TRUE)
se.zin.std <- variableImportanceSE(full.fit, small.zin.fit, y, n, TRUE)

se.std <- c(se.access.std, se.age.std, se.black.std, se.chas.std, se.crim.std, se.dis.std, se.indus.std,
            se.lstat.std, se.neigh.std, se.ptratio.std, se.nox.std,
            se.rad.std, se.rm.std, se.str.std, se.tax.std, se.zin.std)

## CIs
ci.all <- variableImportanceCI(est.all[2], se.all, n, level = 0.95)

ci.access.std <- variableImportanceCI(est.access.std[2], se.access.std, n, level = 0.95)
ci.age.std <- variableImportanceCI(est.age.std[2], se.age.std, n, 0.95)
ci.black.std <- variableImportanceCI(est.black.std[2], se.black.std, n, 0.95)
ci.chas.std <- variableImportanceCI(est.chas.std[2], se.chas.std, n, 0.95)
ci.crim.std <- variableImportanceCI(est.crim.std[2], se.crim.std, n, 0.95)
ci.dis.std <- variableImportanceCI(est.dis.std[2], se.dis.std, n, 0.95)
ci.indus.std <- variableImportanceCI(est.indus.std[2], se.indus.std, n, 0.95)
ci.lstat.std <- variableImportanceCI(est.lstat.std[2], se.lstat.std, n, 0.95)
ci.neigh.std <- variableImportanceCI(est.neigh.std[2], se.neigh.std, n, 0.95)
ci.ptratio.std <- variableImportanceCI(est.ptratio.std[2], se.ptratio.std, n, 0.95)
ci.nox.std <- variableImportanceCI(est.nox.std[2], se.nox.std, n, 0.95)
ci.rad.std <- variableImportanceCI(est.rad.std[2], se.rad.std, n, 0.95)
ci.rm.std <- variableImportanceCI(est.rm.std[2], se.rm.std, n, 0.95)
ci.str.std <- variableImportanceCI(est.str.std[2], se.str.std, n, 0.95)
ci.tax.std <- variableImportanceCI(est.tax.std[2], se.tax.std, n, 0.95)
ci.zin.std <- variableImportanceCI(est.zin.std[2], se.zin.std, n, 0.95)

ci.std <- rbind(ci.access.std, ci.age.std, ci.black.std, ci.chas.std, ci.crim.std, ci.dis.std, ci.indus.std, 
                ci.lstat.std, ci.neigh.std, ci.ptratio.std, ci.nox.std,
                ci.rad.std, ci.rm.std, ci.str.std, ci.tax.std, ci.zin.std)

mat.std <- data.frame(cbind(est.std, se.std, ci.std))
names(mat.std) <- c("naive", "onestep", "se", "CIL", "CIU")
mat.std

## plot of each with 95% CI around it
## also plot with different orderings for naive vs onestep
## plot ordered by onestep, lowest on bottom
nms <- c("Access (group)", "Age", "Prop. black", "Charles riv.", "Crime", "Distance",
         "Prop. business", "Prop. lower status", "Neighborhood (group)",
         "Pupil-teacher ratio", "Nitrogen oxide", "Access to radial hwys",
         "Avg. num. rooms", "Structure (group)", "Property tax rate", "Prop. large zoned")
fig.width <- 2590
fig.height <- fig.width
## onestep
onestep.order.mat.std <- mat.std[order(mat.std$onestep), ]
png("plots/housing_std_onestep.png", width = fig.width, height = fig.height, units = "px", res = 300)
par(mar = c(5, 12, 4, 2) + 0.1)
plot(onestep.order.mat.std$onestep, 1:dim(mat.std)[1], pch = 16, xlab = "", ylab = "", 
     main = "PROPOSED", axes = FALSE,
     xlim = c(0, .2))
abline(v = 0, col = "red", lty = 2)
arrows(unlist(onestep.order.mat.std$CIL), 1:dim(mat.std)[1], unlist(onestep.order.mat.std$CIU), 1:dim(mat.std)[1],
       length = 0, angle = 90, lwd = 2)
title(ylab = "Group", line = -1)
title(xlab = "Estimate", line = 3)
axis(side = 1, at = seq(0, .2, .05))
axis(side = 2, at = 1:dim(mat.std)[1], label = nms[order(mat.std$onestep)], las = 2)
dev.off()

## orderings of naive
naive.order.mat.std <- mat.std[order(mat.std$naive), ]
png("plots/housing_std_naive.png", width = fig.width, height = fig.height, units = "px", res = 300)
par(mar = c(5, 12, 4, 2) + 0.1)
plot(naive.order.mat.std$naive, 1:dim(mat.std)[1], pch = 16, xlab = "Estimate", ylab = "", 
     main = "NAIVE", axes = FALSE,
     xlim = c(0, .2))
abline(v = 0, lty = 2, col = "red")
title(ylab = "Group", line = -1)
axis(side = 1, at = seq(0, .2, .05))
axis(side = 2, at = 1:dim(mat.std)[1], label = nms[order(mat.std$naive)], las = 2)
dev.off()

## table with estimates, SEs, and confidence intervals
library(xtable)
## make the output nice
output.mat.std <- onestep.order.mat.std[order(onestep.order.mat.std$onestep, decreasing = TRUE), c("onestep", "se.std", "CIL", "CIU")]
rownames(output.mat.std) <- nms[order(mat.std$onestep, decreasing = TRUE)]
colnames(output.mat.std)[1:2] <- c("Estimate", "SE")
## print the latex table
xtable(output.mat.std, title = "est_var_imp", caption = "Estimated standardized variable importance and standard errors with 95% confidence intervals", 
       digits = 6)

##################################################################
## PLOT WITH GROUPS AND INDIVIDUAL FEATURES SEPARATE
##################################################################
## get the subset with groups
group.std <- mat.std[c(1, 9, 11, 14), ]
nms.group <- c("Access", "Neighborhood", "Nitrogen Oxide", "Structure")
## order it
group.order.std <- group.std[order(group.std$onestep), ]

## get the subset with individual features
indiv.std <- mat.std[-c(1, 9, 11, 14), ]
nms.indiv <- nms[-c(1, 9, 11, 14)]
## order it
indiv.order.std <- indiv.std[order(indiv.std$onestep), ]

## onestep
png("plots/housing_separate_std_onestep.png", width = fig.width, height = fig.height, units = "px", res = 300)
par(mfrow = c(2, 1), cex = 2, cex.axis = 0.75)
par(mar = c(2, 8, 4, 2) + 0.1)
## first plot groups
plot(group.order.std$onestep, 1:dim(group.std)[1], pch = 16, xlab = "", ylab = "",
     main = "PROPOSED", axes = FALSE, xlim = c(0, 0.2))
arrows(unlist(group.order.std$CIL), 1:dim(group.std)[1],
       unlist(group.order.std$CIU), 1:dim(group.std)[1],
       length = 0, angle = 90, lwd = 2)
abline(v = 0, col = "red", lty = 2)
title(ylab = "Group", line = 7)
axis(side = 1, at = seq(0, .2, .05))
axis(side = 2, at = 1:dim(group.std)[1], label = nms.group[order(group.std$onestep)], las = 2)

## next plot individual features
par(mar = c(5, 8, 0, 2) + 0.1)
plot(indiv.order.std$onestep, 1:dim(indiv.std)[1], pch = 16, xlab = "", ylab = "", 
     main = "", axes = FALSE,
     xlim = c(0, .2))
abline(v = 0, col = "red", lty = 2)
arrows(unlist(indiv.order.std$CIL), 1:dim(indiv.std)[1], 
       unlist(indiv.order.std$CIU), 1:dim(indiv.std)[1],
       length = 0, angle = 90, lwd = 2)
title(ylab = "Feature", line = 7)
title(xlab = "Estimated variable importance", line = 3)
axis(side = 1, at = seq(0, .2, .05))
axis(side = 2, at = 1:dim(indiv.std)[1], label = nms.indiv[order(indiv.std$onestep)], las = 2)
dev.off()

## naive
group.naive.std <- group.std[order(group.std$naive), ]
indiv.naive.std <- indiv.std[order(indiv.std$naive), ]
png("plots/housing_separate_std_naive.png", width = fig.width, height = fig.height, units = "px", res = 300)
par(mfrow = c(2, 1), cex = 2, cex.axis = 0.75)
par(mar = c(2, 8, 4, 2) + 0.1)
## first plot groups
plot(group.naive.std$naive, 1:dim(group.std)[1], pch = 16, xlab = "", ylab = "",
     main = "NAIVE", axes = FALSE, xlim = c(0, 0.2))
abline(v = 0, col = "red", lty = 2)
title(ylab = "Group", line = 7)
axis(side = 1, at = seq(0, .2, .05))
axis(side = 2, at = 1:dim(group.std)[1], label = nms.group[order(group.std$naive)], las = 2)

## next plot individual features
par(mar = c(5, 8, 0, 2) + 0.1)
plot(indiv.naive.std$naive, 1:dim(indiv.std)[1], pch = 16, xlab = "", ylab = "", 
     main = "", axes = FALSE,
     xlim = c(0, .2))
abline(v = 0, col = "red", lty = 2)

title(ylab = "Feature", line = 7)
title(xlab = "Estimated variable importance", line = 3)
axis(side = 1, at = seq(0, .2, .05))
axis(side = 2, at = 1:dim(indiv.std)[1], label = nms.indiv[order(indiv.std$naive)], las = 2)
dev.off()