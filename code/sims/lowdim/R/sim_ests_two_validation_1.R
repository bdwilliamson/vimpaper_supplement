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
calculateEsts <- function(data, truth, n, j, h.f.l, h.m.l, b){
  
  ## calculate the dimension
  p <- dim(data)[2] - 1
  
  x <- data[, 1:p]
  
  small.data <- data[,-j]
  names(small.data) <- c("x", "y")
  
  ## outer layer of cv for the estimators
  folds_outer <- cv_2(n, 5)
  cv_ests <- vector("numeric", 5)
  cv_updates <- vector("numeric", 5)
  ses <- vector("numeric", 5)
  ses_2 <- vector("numeric", 5)
  for (l in 1:5) {
    # inner layer of cv for the bandwidths
    train_outer <- data[folds_outer[, l] == 0, ]
    test_outer_1 <- data[folds_outer[, l] == 1, ]
    test_outer_2 <- data[folds_outer[, l] == 2, ]
    small_train_outer <- small.data[folds_outer[, l] == 0, ]
    train_x <- train_outer[, 1:p]

    ## do cross-validation for the full model
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

    ## get predicted values on the test data, for cv naive
    preds_full_1 <- predict(fullmod.lo, test_outer_1[, 1:p])
    preds_red_1 <- predict(minusmod.lo, test_outer_1[,1:p][,-j])
    
    ## get predicted values on the second set of test data, for cv update
    preds_full_2 <- predict(fullmod.lo, test_outer_2[, 1:p])
    preds_red_2 <- predict(minusmod.lo, test_outer_2[,1:p][,-j])
  
    ## calculate the estimates for naive and onestep
    cv_est <- mean((preds_full_1 - preds_red_1)^2, na.rm = TRUE)/mean((test_outer_1$y - mean(test_outer_1$y))^2)
    cv_ests[l] <- cv_est

    # update
    update <- vimp::vimp_update(preds_full_2, preds_red_2, test_outer_2$y, na.rm = TRUE)
    cv_update <- mean(update, na.rm = TRUE)
    cv_updates[l] <- cv_update
    ses[l] <- sqrt(mean(update^2, na.rm = TRUE))
  }
  # take the average for the sample splitting estimate
  cv_est <- mean(cv_ests)
  cv_update <- mean(cv_updates)
  cv_onestep <- cv_est + cv_update

  ## calculate a CI
  cv_se <- mean(ses)/sqrt(n)
  ci <- vimp::vimp_ci(cv_onestep, cv_se, 0.95)
  
  ## output
  out <- c(n, cv_est, cv_onestep, truth, cv_se, ci)
  names(out) <- c("n", "cv.est", "cv.onestep", "truth", "cv.se", "cv.cil", "cv.ciu")
  return(out)
}