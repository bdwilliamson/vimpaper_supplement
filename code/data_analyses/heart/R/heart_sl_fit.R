############################################################################
##
## FILE: heart_sl_fit.R
##
## CREATED: 6 January 2017 by Brian Williamson
##
## PURPOSE: fit super learner to the different groups of features
## INPUTS: South African Heart disease data
##
## OUTPUTS: the SuperLearner fit on the full covariate matrix
##
## UPDATES:
## DDMMYY INIT COMMENTS
## ------ ---- --------
## 170717 BDW  Re-created the file
############################################################################

## get dataset in the correct format
library("SuperLearner")
library("methods")

## load the heart data
heart <- read.csv("../heart.csv")

## create the Super Learner library
create.SL.gbm <- function(tune = list(interaction.depth = 1, n.trees = c(100, 500, 1000), shrinkage = seq(.001, .3, .05))) {
  tuneGrid <- expand.grid(tune, stringsAsFactors = FALSE)
  for (mm in seq(nrow(tuneGrid))) {
    eval(parse(file = "", text = paste("SL.gbm.", mm, "<- function(..., interaction.depth = ", tuneGrid[mm, 1], ", n.trees = ", tuneGrid[mm, 2], ", shrinkage = ", tuneGrid[mm, 3], ") SL.gbm(..., interaction.depth = interaction.depth, n.trees = n.trees, shrinkage = shrinkage, n.cores = 1)", sep = "")), envir = .GlobalEnv)
  }
  invisible(TRUE)
}
create.SL.gbm()

## second add more degrees of freedom to gam fitting
SL.gam.3 <- function(..., deg.gam = 3) SL.gam(..., deg.gam = deg.gam)
SL.gam.4 <- function(..., deg.gam = 4) SL.gam(..., deg.gam = deg.gam)
SL.gam.5 <- function(..., deg.gam = 5) SL.gam(..., deg.gam = deg.gam)

## add more levels of alpha for glmnet
create.SL.glmnet <- function(alpha = c(0, 0.25, 0.5, 0.75, 1)) {
  for (mm in seq(length(alpha))) {
    eval(parse(file = "", text = paste('SL.glmnet.', alpha[mm], '<- function(..., alpha = ', alpha[mm], ') SL.glmnet(..., alpha = alpha)', sep = '')), envir = .GlobalEnv)
  }
  invisible(TRUE)
}
create.SL.glmnet()

## add different things for randomForest
create.SL.randomForest <- function(tune = list(mtry = c(1, 5, 9), nodesize = c(1, 5, 9))) {
  tuneGrid <- expand.grid(tune, stringsAsFactors = FALSE)
  for (mm in seq(nrow(tuneGrid))) {
    eval(parse(file = "", text = paste("SL.randomForest.", mm, "<- function(..., mtry = ", tuneGrid[mm, 1], ", nodesize = ", tuneGrid[mm, 2], ") SL.randomForest(..., mtry = mtry, nodesize = nodesize)", sep = "")), envir = .GlobalEnv)
  }
  invisible(TRUE)
}
create.SL.randomForest()

learners <- c("SL.gam", "SL.gam.3", "SL.gam.4", "SL.gam.5",
              "SL.glmnet.0", "SL.glmnet.0.25", "SL.glmnet.0.5", "SL.glmnet.0.75", "SL.glmnet.1",
              paste("SL.randomForest.", 1:9, sep = ""),
              paste("SL.gbm.", 1:18, sep = ""))

## set up predictors matrix
if (names(heart)[1] == "row.names") {
  x <- heart[, -c(1, 11)]
} else {
  x <- heart[, -11]
}

## set the seed
set.seed(4747)

## do the fitting
## fit without stratified CV
system.time(full <- SuperLearner(Y = heart$chd, X = x,
                                 cvControl = list(V = 10), family = binomial(), SL.library = learners))

## save it off
saveRDS(full, file = "heart_full_sl.Rdata")
full.fit <- predict(full)$pred
saveRDS(full.fit, file = "heart_full_fit.Rdata")

## fit with CV
set.seed(dim(heart)[1])
system.time(full.cv <- CV.SuperLearner(Y = heart$chd, X = x,
                                       cvControl = list(V = 10), family = binomial(), SL.library = learners))

## save it off
saveRDS(full.cv, file = "heart_full_cv.Rdata")

