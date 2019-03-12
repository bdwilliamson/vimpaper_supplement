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
## UPDATES:
## DDMMYY INIT COMMENTS
## ------ ---- --------
## 130317 BDW  Added plotting separately for groups and individual features
## 050717 BDW  Updated directory structure; changed mention of "one-step" to "proposed"
## 210717 BDW  Updated plots with higher resolution
##             Updated to personal directory
############################################################################
## load variable importance functions
# setwd("C:/Users/Brian Williamson/Dropbox/Additive Models Project Summer 2015/manuscript/sa_heart_analysis")
# setwd("C:/Users/brianw26/Dropbox/Additive Models Project Summer 2015/manuscript/sa_heart_analysis")
setwd("~/Documents/Papers/vimpaper/sim_code/data_analyses/heart")

# source("../../variableImportance.R")
# source("../../variableImportanceSE.R")
# source("../../variableImportanceIC.R")
# source("../../variableImportanceCI.R")
# source("../../variableImportanceIC.diagnose.R")
# source("../../variableImportanceWinsor.R")
# source("../../winsorize.R")
## load the super learner package, vimp package
library("vimp")
library("SuperLearner")

# ## first boosted stumps
# SL.gbm.1 <- function(..., interaction.depth = 1) SL.gbm(..., interaction.depth = interaction.depth)
# # create.SL.gbm <- function(tune = list(interaction.depth = 1, n.trees = seq(100, 1500, 200), shrinkage = seq(.001, .3, .05))) {
# #   tuneGrid <- expand.grid(tune, stringsAsFactors = FALSE)
# #   for (mm in seq(nrow(tuneGrid))) {
# #     eval(parse(file = "", text = paste("SL.gbm.", mm, "<- function(..., interaction.depth = ", tuneGrid[mm, 1], ", n.trees = ", tuneGrid[mm, 2], ", shrinkage = ", tuneGrid[mm, 3], ") SL.gbm(..., interaction.depth = interaction.depth, n.trees = n.trees, shrinkage = shrinkage)", sep = "")), envir = .GlobalEnv)
# #   }
# #   invisible(TRUE)
# # }
# # create.SL.gbm()
# 
# ## second add more degrees of freedom to gam fitting
# SL.gam.3 <- function(..., deg.gam = 3) SL.gam(..., deg.gam = deg.gam)
# SL.gam.4 <- function(..., deg.gam = 4) SL.gam(..., deg.gam = deg.gam)
# SL.gam.5 <- function(..., deg.gam = 5) SL.gam(..., deg.gam = deg.gam)
# 
# ## add more levels of alpha for glmnet
# create.SL.glmnet <- function(alpha = c(0.25, 0.5, 0.75)) {
#   for (mm in seq(length(alpha))) {
#     eval(parse(file = "", text = paste('SL.glmnet.', alpha[mm], '<- function(..., alpha = ', alpha[mm], ') SL.glmnet(..., alpha = alpha)', sep = '')), envir = .GlobalEnv)
#   }
#   invisible(TRUE)
# }
# create.SL.glmnet()
# 
# ## add different things for randomForest
# create.SL.randomForest <- function(tune = list(mtry = c(1, 5, 7), nodesize = c(1, 5, 10))) {
#   tuneGrid <- expand.grid(tune, stringsAsFactors = FALSE)
#   for (mm in seq(nrow(tuneGrid))) {
#     eval(parse(file = "", text = paste("SL.randomForest.", mm, "<- function(..., mtry = ", tuneGrid[mm, 1], ", nodesize = ", tuneGrid[mm, 2], ") SL.randomForest(..., mtry = mtry, nodesize = nodesize)", sep = "")), envir = .GlobalEnv)
#   }
#   invisible(TRUE)
# }
# create.SL.randomForest()
# 
# ## create the learners library
# learners <- c("SL.gam", "SL.gam.3", "SL.gam.4", "SL.gam.5",
#               "SL.glmnet", "SL.glmnet.0.25", "SL.glmnet.0.5", "SL.glmnet.0.75",
#               "SL.randomForest", "SL.randomForest.1", "SL.randomForest.2", "SL.randomForest.3",
#               "SL.randomForest.4", "SL.randomForest.5", "SL.randomForest.6", "SL.randomForest.7",
#               "SL.randomForest.8", "SL.randomForest.9",
#               "SL.gbm.1")
# # learners.2 <- c("SL.gam", "SL.gam.3", "SL.gam.4", "SL.gam.5",
# #               "SL.glmnet", "SL.glmnet.0.25", "SL.glmnet.0.5", "SL.glmnet.0.75",
# #               "SL.randomForest", "SL.randomForest.1", "SL.randomForest.2", "SL.randomForest.3",
# #               "SL.randomForest.4", "SL.randomForest.5", "SL.randomForest.6", "SL.randomForest.7",
# #               "SL.randomForest.8", "SL.randomForest.9",
# #               paste("SL.gbm.", seq(nrow(tuneGrid)), sep = ""))



############################################
## SA HEART DATA
############################################

## load the heart data
heart <- read.csv("heart.csv", stringsAsFactors = FALSE)
# View(heart)

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


####################################################
## STANDARDIZED PARAMETER
####################################################
## Estimates (naive and onestep)
## Estimates (naive and onestep)
est.sbp.std <- variableImportance(full.fit, small.sbp.fit, y, n, TRUE, return_naive = TRUE)
est.tob.std <- variableImportance(full.fit, small.tob.fit, y, n, TRUE, return_naive = TRUE)
est.ldl.std <- variableImportance(full.fit, small.ldl.fit, y, n, TRUE, return_naive = TRUE)
est.adi.std <- variableImportance(full.fit, small.adi.fit, y, n, TRUE, return_naive = TRUE)
est.fam.std <- variableImportance(full.fit, small.fam.fit, y, n, TRUE, return_naive = TRUE)
est.ta.std <- variableImportance(full.fit, small.ta.fit, y, n, TRUE, return_naive = TRUE)
est.ob.std <- variableImportance(full.fit, small.ob.fit, y, n, TRUE, return_naive = TRUE)
est.alc.std <- variableImportance(full.fit, small.alc.fit, y, n, TRUE, return_naive = TRUE)
est.age.std <- variableImportance(full.fit, small.age.fit, y, n, TRUE, return_naive = TRUE)
est.bios.std <- variableImportance(full.fit, small.bios.fit, y, n, TRUE, return_naive = TRUE)
est.behav.std <- variableImportance(full.fit, small.behav.fit, y, n, TRUE, return_naive = TRUE)

est.std <- rbind(est.sbp.std, est.tob.std, est.ldl.std, est.adi.std, est.fam.std, est.ta.std, est.ob.std, 
             est.alc.std, est.age.std, est.bios.std, est.behav.std)

## SEs
se.sbp.std <- variableImportanceSE(full.fit, small.sbp.fit, y, n, TRUE)
se.tob.std <- variableImportanceSE(full.fit, small.tob.fit, y, n, TRUE)
se.ldl.std <- variableImportanceSE(full.fit, small.ldl.fit, y, n, TRUE)
se.adi.std <- variableImportanceSE(full.fit, small.adi.fit, y, n, TRUE)
se.fam.std <- variableImportanceSE(full.fit, small.fam.fit, y, n, TRUE)
se.ta.std <- variableImportanceSE(full.fit, small.ta.fit, y, n, TRUE)
se.ob.std <- variableImportanceSE(full.fit, small.ob.fit, y, n, TRUE)
se.alc.std <- variableImportanceSE(full.fit, small.alc.fit, y, n, TRUE)
se.age.std <- variableImportanceSE(full.fit, small.age.fit, y, n, TRUE)
se.bios.std <- variableImportanceSE(full.fit, small.bios.fit, y, n, TRUE)
se.behav.std <- variableImportanceSE(full.fit, small.behav.fit, y, n, TRUE)

se.std <- c(se.sbp.std, se.tob.std, se.ldl.std, se.adi.std, se.fam.std, se.ta.std, se.ob.std,
            se.alc.std, se.age.std, se.bios.std, se.behav.std)

## CIs
ci.sbp.std <- variableImportanceCI(est.sbp.std[2], se.sbp.std, n, level = 0.95)
ci.tob.std <- variableImportanceCI(est.tob.std[2], se.tob.std, n, 0.95)
ci.ldl.std <- variableImportanceCI(est.ldl.std[2], se.ldl.std, n, 0.95)
ci.adi.std <- variableImportanceCI(est.adi.std[2], se.adi.std, n, 0.95)
ci.fam.std <- variableImportanceCI(est.fam.std[2], se.fam.std, n, 0.95)
ci.ta.std <- variableImportanceCI(est.ta.std[2], se.ta.std, n, 0.95)
ci.ob.std <- variableImportanceCI(est.ob.std[2], se.ob.std, n, 0.95)
ci.alc.std <- variableImportanceCI(est.alc.std[2], se.alc.std, n, 0.95)
ci.age.std <- variableImportanceCI(est.age.std[2], se.age.std, n, 0.95)
ci.bios.std <- variableImportanceCI(est.bios.std[2], se.bios.std, n, 0.95)
ci.behav.std <- variableImportanceCI(est.behav.std[2], se.behav.std, n, 0.95)

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
