############################################################################
##
## FILE: heart_sl_fits.R
##
## CREATED: 6 January 2017 by Brian Williamson
##
## PURPOSE: fit super learner to the different groups of features
## INPUTS: South African Heart disease study data
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
heart <- read.csv("heart.csv")


## load the full fit
full.fit <- readRDS("heart_full_fit.Rdata")

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

## get correct one to remove based on id
job.id <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))

rem.mat <- matrix(c(1, rep(NA, 5),
                    2, rep(NA, 5),
                    3, rep(NA, 5),
                    4, rep(NA, 5),
                    5, rep(NA, 5),
                    6, rep(NA, 5),
                    7, rep(NA, 5),
                    8, rep(NA, 5),
                    9, rep(NA, 5),
                    c(1, 3, 4, 5, 7, 9),
                    c(c(2, 6, 8), rep(NA, 3))),
                  byrow = TRUE, ncol = 6)

current <- na.omit(rem.mat[job.id, ])

## get the correct name
nms <- c("sbp", "tob", "ldl", "adi", "fam", "ta", "ob", "alc",
         "age", "bios", "behav")
nm <- nms[job.id]

## set the seed
seeds <- c(5169, 6034, 7089, 6710, 5814, 9537, 6793, 9561, 7281,
           9560, 5752)
set.seed(seeds[job.id])

## fit the small ones
system.time(small <- SuperLearner(Y = full.fit, X = x[, -current],
                                  cvControl = list(V = 10), family = gaussian(),
                                  SL.library = learners))
small.fit <- predict(small)$pred
saveRDS(small, file = paste("small_", nm, ".Rdata", sep = ""))
saveRDS(small.fit, file = paste("small_fit_", nm, ".Rdata", sep = ""))