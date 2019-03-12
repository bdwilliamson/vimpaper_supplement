############################################################################
##
## FILE: heart_data_analysis.R
##
## CREATED: 30 January 2016 by Brian Williamson
##
## PURPOSE: Data analysis of South African Heart disease and prostate data
##
## INPUTS: South Africa heart disease and prostate data from ESL
##
## OUTPUTS:
##
############################################################################
## load the super learner package, vimp package
library("vimp")
library("SuperLearner")

############################################
## SA HEART DATA
############################################

## load the heart data
heart <- read.csv("heart.csv", stringsAsFactors = FALSE)

## clean
heart$famhist <- ifelse(heart$famhist == "Present", 1, 0)
str(heart)
analysis <- heart[, -1]

## no missing data
sum(is.na(analysis))

## number of samples
dim(analysis)
## number of cases
sum(analysis$chd)
## 160 cases
heart <- analysis
x.heart <- heart[, -10]


## 9 variables
## load all of the fits
full.fit <- readRDS("output/full_fit.Rdata")
small.sbp.fit <-  readRDS("output/small_fit_sbp.Rdata")
small.tob.fit <- readRDS("output/small_fit_tob.Rdata")
small.ldl.fit <-  readRDS("output/small_fit_ldl.Rdata")
small.adi.fit <-  readRDS("output/small_fit_adi.Rdata")
small.fam.fit <-  readRDS("output/small_fit_fam.Rdata")
small.ta.fit <- readRDS("output/small_fit_ta.Rdata")
small.ob.fit <-  readRDS("output/small_fit_ob.Rdata")
small.alc.fit <-  readRDS("output/small_fit_alc.Rdata")
small.age.fit <-  readRDS("output/small_fit_age.Rdata")
small.bios.fit <-  readRDS("output/small_fit_bios.Rdata")
small.behav.fit <-  readRDS("output/small_fit_behav.Rdata")

n <- dim(heart)[1]
y <- heart$chd


## ----------------------------------------------------
## Compute point estimates, standard errors, CIs
## ----------------------------------------------------
sbp <- vimp::vimp_regression(Y = y, f1 = full.fit, f2 = small.sbp.fit, indx = 1, run_regression = FALSE)
tob <- vimp::vimp_regression(Y = y, f1 = full.fit, f2 = small.tob.fit, indx = 2, run_regression = FALSE)
ldl <- vimp::vimp_regression(Y = y, f1 = full.fit, f2 = small.ldl.fit, indx = 3, run_regression = FALSE)
adi <- vimp::vimp_regression(Y = y, f1 = full.fit, f2 = small.adi.fit, indx = 4, run_regression = FALSE)
fam <- vimp::vimp_regression(Y = y, f1 = full.fit, f2 = small.fam.fit, indx = 5, run_regression = FALSE)
ta <- vimp::vimp_regression(Y = y, f1 = full.fit, f2 = small.ta.fit, indx = 6, run_regression = FALSE)
ob <- vimp::vimp_regression(Y = y, f1 = full.fit, f2 = small.ob.fit, indx = 7, run_regression = FALSE)
alc <- vimp::vimp_regression(Y = y, f1 = full.fit, f2 = small.alc.fit, indx = 8, run_regression = FALSE)
age <- vimp::vimp_regression(Y = y, f1 = full.fit, f2 = small.age.fit, indx = 9, run_regression = FALSE)
bios <- vimp::vimp_regression(Y = y, f1 = full.fit, f2 = small.bios.fit, indx = c(1, 3, 4, 7, 9), run_regression = FALSE)
behav <- vimp::vimp_regression(Y = y, f1 = full.fit, f2 = small.behav.fit, indx = c(2, 5, 6, 8), run_regression = FALSE)

## Estimates (naive and onestep)
est.sbp.std <- cbind(sbp$est, sbp$naive)
est.tob.std <- cbind(tob$est, tob$naive)
est.ldl.std <- cbind(ldl$est, ldl$naive)
est.adi.std <- cbind(adi$est, adi$naive)
est.fam.std <- cbind(fam$est, fam$naive)
est.ta.std <- cbind(ta$est, ta$naive)
est.ob.std <- cbind(ob$est, ob$naive)
est.alc.std <- cbind(alc$est, alc$naive)
est.age.std <- cbind(age$est, age$naive)
est.bios.std <- cbind(bios$est, bios$naive)
est.behav.std <- cbind(behav$est, behav$naive)

est.std <- rbind(est.sbp.std, est.tob.std, est.ldl.std, est.adi.std, est.fam.std, est.ta.std, est.ob.std, 
             est.alc.std, est.age.std, est.bios.std, est.behav.std)

## SEs
se.sbp.std <- sbp$se
se.tob.std <- tob$se
se.ldl.std <- ldl$se
se.adi.std <- adi$se
se.fam.std <- fam$se
se.ta.std <- ta$se
se.ob.std <- ob$se
se.alc.std <- alc$se
se.age.std <- age$se
se.bios.std <- bios$se
se.behav.std <- behav$se

se.std <- c(se.sbp.std, se.tob.std, se.ldl.std, se.adi.std, se.fam.std, se.ta.std, se.ob.std,
            se.alc.std, se.age.std, se.bios.std, se.behav.std)

## CIs
ci.sbp.std <- sbp$ci
ci.tob.std <- tob$ci
ci.ldl.std <- ldl$ci
ci.adi.std <- adi$ci
ci.fam.std <- fam$ci
ci.ta.std <- ta$ci
ci.ob.std <- ob$ci
ci.alc.std <- alc$ci
ci.age.std <- age$ci
ci.bios.std <- bios$ci
ci.behav.std <- behav$ci

ci.std <- rbind(ci.sbp.std, ci.tob.std, ci.ldl.std, ci.adi.std, ci.fam.std, ci.ta.std, ci.ob.std, 
                ci.alc.std, ci.age.std, ci.bios.std, ci.behav.std)

mat.std <- data.frame(cbind(est.std, se.std, ci.std))
names(mat.std) <- c("naive", "onestep", "se", "CIL", "CIU")
mat.std

## plot of each with 95% CI around it
## also plot with different orderings for naive vs onestep
## plot ordered by onestep, lowest on bottom
nms <- c("SBP", "Tobacco", "LDL", "Adiposity", "Family history",
         "Type A behavior", "Obesity", "Alcohol",
         "Age", "Biological factors (group)", "Behavioral factors (group)")
fig.width <- 2590
fig.height <- fig.width
## onestep
onestep.order.mat.std <- mat.std[order(mat.std$onestep), ]
png("plots/heart_std_onestep.png", width = fig.width, height = fig.height, units = "px", res = 300)
par(mar = c(5, 12, 4, 2) + 0.1)
plot(onestep.order.mat.std$onestep, 1:dim(mat.std)[1], pch = 16, xlab = "", ylab = "", 
     main = "PROPOSED", axes = FALSE,
     xlim = c(0, 0.2))
abline(v = 0, col = "red", lty = 2)
arrows(unlist(onestep.order.mat.std$CIL), 1:dim(mat.std)[1], unlist(onestep.order.mat.std$CIU), 1:dim(mat.std)[1],
       length = 0, angle = 90, lwd = 2)
title(ylab = "Group", line = -1)
title(xlab = "Estimate", line = 3)
axis(side = 1, at = seq(0, 0.2, .05))
axis(side = 2, at = 1:dim(mat.std)[1], label = nms[order(mat.std$onestep)], las = 2)
dev.off()

## orderings of naive
naive.order.mat.std <- mat.std[order(mat.std$naive), ]
png("plots/heart_std_naive.png", width = fig.width, height = fig.height, units = "px", res = 300)
par(mar = c(5, 12, 4, 2) + 0.1)
plot(naive.order.mat.std$naive, 1:dim(mat.std)[1], pch = 16, xlab = "Estimate", ylab = "", 
     main = "NAIVE", axes = FALSE,
     xlim = c(0, .2))
abline(v = 0, lty = 2, col = "red")
title(ylab = "Group", line = -1)
axis(side = 1, at = seq(0, 0.2, .05))
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
group.std <- mat.std[c(10, 11), ]
nms.group <- c("Biological factors", "Behavioral factors")
## order it
group.order.std <- group.std[order(group.std$onestep), ]

## get the subset with individual features
indiv.std <- mat.std[-c(10, 11), ]
nms.indiv <- nms[-c(10, 11)]
## order it
indiv.order.std <- indiv.std[order(indiv.std$onestep), ]

## onestep
png("plots/heart_separate_std_onestep.png", width = fig.width, height = fig.height, units = "px", res = 300)
par(mfrow = c(2, 1), cex = 2)
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
png("plots/heart_separate_std_naive.png", width = fig.width, height = fig.height, units = "px", res = 300)
par(mfrow = c(2, 1), cex = 2)
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
