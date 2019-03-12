############################################################################
##
## FILE: housing_sl_fit.R
##
## CREATED: 6 January 2017 by Brian Williamson
##
## PURPOSE: fit super learner to the different groups of features
## INPUTS: boston housing data from the MASS package
##
## OUTPUTS: the SuperLearner fit on the full covariate matrix
##
## UPDATES:
## DDMMYY INIT COMMENTS
## ------ ---- --------
## 010917 BDW  Updated comments to be more readable
############################################################################

## load required libraries
library("SuperLearner")
library("methods")
library("MASS")

## load the housing data
data(Boston)

##############################################################
## create the Super Learner library
##############################################################

## helper function to create gradient boosted trees (R package "gbm"), with varying tuning parameters
create.SL.gbm <- function(tune = list(interaction.depth = 1, n.trees = c(100, 500, 1000), shrinkage = seq(.001, .3, .05))) {
  tuneGrid <- expand.grid(tune, stringsAsFactors = FALSE)
  for (mm in seq(nrow(tuneGrid))) {
    eval(parse(file = "", text = paste("SL.gbm.", mm, "<- function(..., interaction.depth = ", tuneGrid[mm, 1], ", n.trees = ", tuneGrid[mm, 2], ", shrinkage = ", tuneGrid[mm, 3], ") SL.gbm(..., interaction.depth = interaction.depth, n.trees = n.trees, shrinkage = shrinkage, n.cores = 1)", sep = "")), envir = .GlobalEnv)
  }
  invisible(TRUE)
}
## creates the gbm function objects
create.SL.gbm()

## create generalized additive models with three levels of degrees of freedom
SL.gam.3 <- function(..., deg.gam = 3) SL.gam(..., deg.gam = deg.gam)
SL.gam.4 <- function(..., deg.gam = 4) SL.gam(..., deg.gam = deg.gam)
SL.gam.5 <- function(..., deg.gam = 5) SL.gam(..., deg.gam = deg.gam)

## helper function to create elastic net instances with different alpha levels (R package "glmnet")
## alpha = 0 corresponds to lasso, alpha = 1 corresponds to ridge; others have both penalties
create.SL.glmnet <- function(alpha = c(0, 0.25, 0.5, 0.75, 1)) {
  for (mm in seq(length(alpha))) {
    eval(parse(file = "", text = paste('SL.glmnet.', alpha[mm], '<- function(..., alpha = ', alpha[mm], ') SL.glmnet(..., alpha = alpha)', sep = '')), envir = .GlobalEnv)
  }
  invisible(TRUE)
}
## creates glmnet function objects
create.SL.glmnet()

## helper function to create random forests (R package "randomForest") with varying tuning parameters
create.SL.randomForest <- function(tune = list(mtry = c(1, 5, 9), nodesize = c(1, 5, 9))) {
  tuneGrid <- expand.grid(tune, stringsAsFactors = FALSE)
  for (mm in seq(nrow(tuneGrid))) {
    eval(parse(file = "", text = paste("SL.randomForest.", mm, "<- function(..., mtry = ", tuneGrid[mm, 1], ", nodesize = ", tuneGrid[mm, 2], ") SL.randomForest(..., mtry = mtry, nodesize = nodesize)", sep = "")), envir = .GlobalEnv)
  }
  invisible(TRUE)
}
## creates random forest function objects
create.SL.randomForest()

## set up the learners library to pass to SuperLearner; note that all of these objects have been created by the helper functions
learners <- c("SL.gam", "SL.gam.3", "SL.gam.4", "SL.gam.5",
              "SL.glmnet.0", "SL.glmnet.0.25", "SL.glmnet.0.5", "SL.glmnet.0.75", "SL.glmnet.1",
              paste("SL.randomForest.", 1:9, sep = ""),
              paste("SL.gbm.", 1:18, sep = ""))

## set up predictors matrix; 14 is the column of the outcome
x <- Boston[, -14]

## set the seed
set.seed(4747)

###################################################################
## Fit Super Learner
###################################################################
## only use SL cv, no outer CV; this picks optimal combination, runs CV within algorithms that still require tuning parameters
system.time(full <- SuperLearner(Y = Boston$medv, X = x,
                    cvControl = list(V = 10), family = gaussian(), SL.library = learners))

## save off output
saveRDS(full, file = "housing_full_sl.Rdata")
full.fit <- predict(full)$pred
saveRDS(full.fit, file = "housing_full_fit.Rdata")

###################################################################
## Fit Super Learner with CV to assess performance
###################################################################
set.seed(dim(Boston)[1])
system.time(full.cv <- CV.SuperLearner(Y = Boston$medv, X = x,
             cvControl = list(V = 10), family = gaussian(), SL.library = learners))

## save it off
saveRDS(full.cv, file = "housing_full_cv.Rdata")

