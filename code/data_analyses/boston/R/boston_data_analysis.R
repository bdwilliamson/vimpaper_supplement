############################################################################
##
## FILE: boston_analysis.R
##
## CREATED: 28 February 2017 by Brian Williamson
##
## PURPOSE: Analysis of the Boston housing data
##
## INPUTS: Boston housing data from MASS package
##
## OUTPUTS:
############################################################################
## load variable importance functions
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

####################################################
## COMPUTE VARIABLE IMPORTANCE
####################################################
## removing all variables
all <- vimp::vimp_regression(Y = y, f1 = full.fit, f2 = rep(mean(y), length(full.fit)), run_regression = FALSE)
est.all <- all$est
## individual/groups of variables
access <- vimp::vimp_regression(Y = y, f1 = full.fit, f2 = small.access.fit, s = c(8, 9), run_regression = FALSE)
age <- vimp::vimp_regression(Y = y, f1 = full.fit, f2 = small.age.fit, s = c(7), run_regression = FALSE)
black <- vimp::vimp_regression(Y = y, f1 = full.fit, f2 = small.black.fit, s = c(12),  run_regression = FALSE)
chas <- vimp::vimp_regression(Y = y, f1 = full.fit, f2 = small.chas.fit, s = c(4), run_regression = FALSE)
crim <- vimp::vimp_regression(Y = y, f1 = full.fit, f2 = small.crim.fit, s = c(1), run_regression = FALSE)
dis <- vimp::vimp_regression(Y = y, f1 = full.fit, f2 = small.dis.fit, s = c(8), run_regression = FALSE)
indus <- vimp::vimp_regression(Y = y, f1 = full.fit, f2 = small.indus.fit, s = c(3), run_regression = FALSE)
lstat <- vimp::vimp_regression(Y = y, f1 = full.fit, f2 = small.lstat.fit, s = c(13), run_regression = FALSE)
neigh <- vimp::vimp_regression(Y = y, f1 = full.fit, f2 = small.neigh.fit, s = c(1, 2, 3, 4, 10, 11, 12, 13), run_regression = FALSE)
ptratio <- vimp::vimp_regression(Y = y, f1 = full.fit, f2 = small.ptratio.fit, s = c(11), run_regression = FALSE)
nox <- vimp::vimp_regression(Y = y, f1 = full.fit, f2 = small.nox.fit, s = c(5), run_regression = FALSE)
rad <- vimp::vimp_regression(Y = y, f1 = full.fit, f2 = small.rad.fit, s = c(9), run_regression = FALSE)
rm <- vimp::vimp_regression(Y = y, f1 = full.fit, f2 = small.rm.fit, s = c(6), run_regression = FALSE)
str <- vimp::vimp_regression(Y = y, f1 = full.fit, f2 = small.str.fit, s = c(6, 7), run_regression = FALSE)
tax <- vimp::vimp_regression(Y = y, f1 = full.fit, f2 = small.tax.fit, s = c(10), run_regression = FALSE)
zin <- vimp::vimp_regression(Y = y, f1 = full.fit, f2 = small.zin.fit, s = c(2), run_regression = FALSE)

## Estimates (naive and onestep)
est.access.std <- cbind(access$est, access$naive)
est.age.std <- cbind(age$est, age$naive)
est.black.std <- cbind(black$est, black$naive)
est.chas.std <- cbind(chas$est, chas$naive)
est.crim.std <- cbind(crim$est, crim$naive)
est.dis.std <- cbind(dis$est, dis$naive)
est.indus.std <- cbind(indus$est, indus$naive)
est.lstat.std <- cbind(lstat$est, lstat$naive)
est.neigh.std <- cbind(neigh$est, neigh$naive)
est.ptratio.std <- cbind(ptratio$est, ptratio$naive)
est.nox.std <- cbind(nox$est, nox$naive)
est.rad.std <- cbind(rad$est, rad$naive)
est.rm.std <- cbind(rm$est, rm$naive)
est.str.std <- cbind(str$est, str$naive)
est.tax.std <- cbind(tax$est, tax$naive)
est.zin.std <- cbind(zin$est, zin$naive)

est.std <- rbind(est.access.std, est.age.std, est.black.std, est.chas.std, est.crim.std, est.dis.std, est.indus.std, 
                 est.lstat.std, est.neigh.std, est.ptratio.std, est.nox.std,
                 est.rad.std, est.rm.std, est.str.std, est.tax.std, est.zin.std)

## SEs
se.all <- all$se

se.access.std <- access$se
se.age.std <- age$se
se.black.std <- black$se
se.chas.std <- chas$se
se.crim.std <- crim$se
se.dis.std <- dis$se
se.indus.std <- indus$se
se.lstat.std <- lstat$se
se.neigh.std <- neigh$se
se.ptratio.std <- ptratio$se
se.nox.std <- nox$se
se.rad.std <- rad$se
se.rm.std <- rm$se
se.str.std <- str$se
se.tax.std <- tax$se
se.zin.std <- zin$se

se.std <- c(se.access.std, se.age.std, se.black.std, se.chas.std, se.crim.std, se.dis.std, se.indus.std,
            se.lstat.std, se.neigh.std, se.ptratio.std, se.nox.std,
            se.rad.std, se.rm.std, se.str.std, se.tax.std, se.zin.std)

## CIs
ci.all <- all$ci

ci.access.std <- access$ci
ci.age.std <- age$ci
ci.black.std <- black$ci
ci.chas.std <- chas$ci
ci.crim.std <- crim$ci
ci.dis.std <- dis$ci
ci.indus.std <- indus$ci
ci.lstat.std <- lstat$ci
ci.neigh.std <- neigh$ci
ci.ptratio.std <- ptratio$ci
ci.nox.std <- nox$ci
ci.rad.std <- rad$ci
ci.rm.std <- rm$ci
ci.str.std <- str$ci
ci.tax.std <- tax$ci
ci.zin.std <- zin$ci

ci.std <- rbind(ci.access.std, ci.age.std, ci.black.std, ci.chas.std, ci.crim.std, ci.dis.std, ci.indus.std, 
                ci.lstat.std, ci.neigh.std, ci.ptratio.std, ci.nox.std,
                ci.rad.std, ci.rm.std, ci.str.std, ci.tax.std, ci.zin.std)

mat.std <- data.frame(cbind(est.std, se.std, ci.std))
names(mat.std) <- c("onestep", "naive", "se", "CIL", "CIU")
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