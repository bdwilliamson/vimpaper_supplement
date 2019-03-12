###########################################################################################################
##
## FILE:    sim_ests_cv_0.R
##
## CREATED: 19 January 2018
##
## AUTHOR:  Brian Williamson
##
## PURPOSE: estimates for cv one-step
############################################################################################################

## Now a function to compute each estimate.
## FUNCTION: calculateEsts
## ARGS:         data - the simulated data set
##              truth - the true value
##                  n - the sample size 
##                  j - the feature to remove from the model
##                h's - the various bandwidth vectors
##                  b - number of bootstrap reps
## RETURNS:  the one-step and naive estimates
calculateEsts <- function(data, truth, n, j, h.f.l, h.m.l){
  
  ## calculate the dimension
  p <- dim(data)[2] - 1
  
  x <- data[, 1:p]
  
  small.data <- data[,-j]
  names(small.data) <- c("x", "y")
  
  ## outer layer of cv for the estimators
  folds_outer <- cv(dim(data)[1], 5)
  cv_ests <- vector("numeric", 5)
  cv_updates <- vector("numeric", 5)
  ses_1 <- ses_2 <- vector("numeric", 5)
  for (l in 1:5) {
    # inner layer of cv for the bandwidths
    train_outer <- data[folds_outer$subsets[folds_outer$which != l], ]
    test_outer <- data[folds_outer$subsets[folds_outer$which == l], ]
    small_train_outer <- small.data[folds_outer$subsets[folds_outer$which != l], ]
    small_test_outer <- small.data[folds_outer$subsets[folds_outer$which == l], ]
    train_x <- train_outer[, 1:p]

    # do cross-validation for the full model
    fullmod <- get_mod_1(train_outer, 5, h.f.l)
    fullmod.lo <- fullmod$mod
    h.f.lo <- fullmod$h
    fhat.lo <- fitted(fullmod.lo)
  
    ## now run through for the small model, based on the fits from
    small_train_outer_dat <- data.frame(x = small_train_outer$x, y = fhat.lo)
    minusmod <- get_mod_1(small_train_outer_dat, 5, h.f.l)
    minusmod.lo <- minusmod$mod
    h.m.lo <- minusmod$h
    fhat.minus.lo <- fitted(minusmod.lo)

    ## get predicted values on the test data
    preds_full <- predict(fullmod.lo, test_outer[, 1:p])
    preds_red <- predict(minusmod.lo, test_outer[,1:p][,-j])
  
    ## calculate the estimates for naive and onestep
    # ests.lo <- variableImportance(preds_full, preds_red, test_outer$y, standardized = TRUE)
    cv_est <- mean((fhat.lo - fhat.minus.lo)^2, na.rm = TRUE)/mean((train_outer$y - mean(train_outer$y))^2)
    cv_ests[l] <- cv_est

    # update
    cv_update <- mean(vimp::vimp_update(preds_full, preds_red, test_outer$y, type = "regression", na.rm = TRUE), na.rm = TRUE)
    cv_updates[l] <- cv_update
    ses_1[l] <- sqrt(mean(vimp::vimp_update(preds_full, preds_red, test_outer$y, type = "regression", na.rm = TRUE)^2, na.rm = TRUE))/sqrt(dim(test_outer)[1])
    ses_2[l] <- sqrt(mean(vimp::vimp_update(preds_full, preds_red, test_outer$y, type = "regression", na.rm = TRUE)^2, na.rm = TRUE))
  }
  # take the average for the sample splitting estimate
  cv_est <- mean(cv_ests)
  cv_update <- mean(cv_updates)
  cv_onestep <- cv_est + cv_update

  ## calculate a CI (?)
  cv_se_1 <- mean(ses_1)
  cv_se_2 <- mean(ses_2)/sqrt(n)

  ci_1 <- vimp::vimp_ci(cv_onestep, cv_se_1, 0.95)
  ci_2 <- vimp::vimp_ci(cv_onestep, cv_se_2, 0.95)
  ## output
  out <- c(n, cv_est, cv_onestep, truth, cv_se_1, ci_1, cv_se_2, ci_2)
  names(out) <- c("n", "cv.est", "cv.onestep", "truth", "cv.se.1", "cv.cil.1", "cv.ciu.1", "cv.se.2", "cv.cil.2", "cv.ciu.2")
  return(out)
}