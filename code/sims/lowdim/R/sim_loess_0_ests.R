###########################################################################################################
##
## FILE:    sim_loess_0_ests.R
##
## CREATED: 01 March 2017
##
## AUTHOR:  Brian Williamson
##
## PURPOSE: look at coverage of confidence intervals and bias for loess and np, based on cv bandwidths
##          using the standardized parameter of interest, two-step estimating procedure
##          This POI is explaining proportion of total variability
##          Now we only fit loess, but fit lower order
## UPDATES
## DDMMYY   INIT   COMMENTS
## ------   ----   --------
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
  
  ## calculate the loess estimates, using cross-validation to get the correct bandwidth
  folds <- cv(n, 5)
  
  ## do cross-validation for the full model
  full.errs.lo <- matrix(0, ncol = 5, nrow = length(h.f.l))
  minus.errs.lo <- matrix(0, ncol = 5, nrow = length(h.m.l))
  for (i in 1:length(h.f.l)) {     ## fit the model with this span
    for (k in 1:5) {  ## do this for each fold in the CV set
      ## set up the training and test sets
      train <- data[folds$subsets[folds$which != k], ]
      test <- data[folds$subsets[folds$which == k], ]
      trainx <- train[, 1:p]
      
      ## fit the model on the training set
      mod.lo <- suppressWarnings(loess(y ~ V1 + V2, data = train, degree = 0, span = h.f.l[i]))

      ## predict on the test set
      preds.lo <- predict(mod.lo, newdata = test)
      fit.lo <- predict(mod.lo)
      
      ## figure out which x's are on the boundary of the sample space;
      ## don't want to use them for MSE
      scaled.x <- (test[, 1:p] +5)/10
      boundary.1 <- scaled.x[, 1] < 0.05 | scaled.x[, 1] > 0.95
      boundary.2 <- scaled.x[, 2] < 0.05 | scaled.x[, 2] > 0.95
      
      ## calculate the error
      err.lo <- mean((preds.lo[!boundary.1 & !boundary.2] - test$y[!boundary.1 & !boundary.2]) ^ 2, na.rm = TRUE)
      full.errs.lo[i,k] <- err.lo
    }
  }
  ## get the errors for each bandwidth
  fulls.lo <- apply(full.errs.lo, 1, mean)
  
  ## get the bandwidths that minimized test error
  ## if we are at the edge, move into the middle
  indx.f <- which.min(fulls.lo)
  h.f.lo <- h.f.l[indx.f]
  
  ## calculate the loess curve
  fullmod.lo <- suppressWarnings(loess(y ~ V1 + V2, data = data, degree = 0, span = h.f.lo))
  fhat.lo <- fitted(fullmod.lo)
  
  ## now run through for the small model, based on the fits from 
  ## bandwidth that minimized CV error on full model
  for (i in 1:length(h.f.l)) {
    for (k in 1:5) {
      ## set up the training and test sets
      train.s <- small.data[folds$subsets[folds$which != k], ]
      test.s <- small.data[folds$subsets[folds$which == k], ]
      names(test.s) <- names(train.s) <- c("x", "y")
      
      ## get fitted values on the training data
      fit.lo <- fhat.lo[folds$subsets[folds$which != k]]

      ## get test values
      test.lo <- fhat.lo[folds$subsets[folds$which == k]]

      ## fit the reduced model
      mod.lo <- suppressWarnings(loess(fit.lo ~ x, data = train.s, degree = 0, span = h.m.l[i]))
      
      ## predict on the test set
      preds.lo <- predict(mod.lo, newdata = test.s)
      
      ## get the ones we don't want to evaluate on
      scaled.x <- (test.s[, 1] + 5)/10
      boundary <- scaled.x < 0.05 | scaled.x > 0.95
      
      ## calculate the error
      err.lo <- mean((preds.lo[!boundary] - test.lo[!boundary]) ^ 2, na.rm = TRUE)
      minus.errs.lo[i,k] <- err.lo
    }
  }
  
  ## get the errors for each bandwidth
  minuses.lo <- apply(minus.errs.lo, 1, mean)

  ## get the bandwidths that minimized test error
  indx.m <- which.min(minuses.lo)
  h.m.lo <- h.m.l[indx.m]
  
  ## get the new data frame, without the jth feature
  newx <- x[,-j]
  newdata <- data[,-j]
  names(newdata)[1] <- "x"
  minusmod.lo <- suppressWarnings(loess(fhat.lo ~ x, data = newdata, degree = 0, span = h.m.lo))
  
  ## get the fitted values
  fhat.minus.lo <- fitted(minusmod.lo)
  
  ## calculate the estimates for naive and onestep
  ests.lo <- variableImportance(fhat.lo, fhat.minus.lo, data$y, standardized = TRUE)

  naive.lo <- mean((fhat.lo - fhat.minus.lo)^2)/mean((data$y - mean(data$y))^2)
  
  ## calculate the one step estimates
  onestep.lo <- ests.lo
  
  ## calculate the standard errors for loess
  onestep.lo.se <- variableImportanceSE(fhat.lo, fhat.minus.lo, data$y, standardized = TRUE)
  
  ## calculate the onestep CIs
  onestep.lo.ci <- variableImportanceCI(onestep.lo, onestep.lo.se, n = n, level = 0.95)
  
  ## Bootstrap CIs for the naive
  naives <- boot::boot(data = data, statistic = getNaives, R = b, n = n, j = j, h.f.l = h.f.lo, h.m.l = h.m.lo)
  naive.ci.init <- boot::boot.ci(naives, type = "perc")
  naive.ci <- naive.ci.init$percent[4:5]

  ## OLS estimator
  simple_r2 <- get_lm_ests(dat, 1:dim(dat)[1], j)

  ## bootstrap a CI
  simple_r2s <- boot::boot(data = dat, statistic = get_lm_ests, R = b, j = j)
  simple_r2s_ci_init <- boot::boot.ci(simple_r2s, type = "perc")
  simple_r2_ci <- simple_r2s_ci_init$percent[4:5]
  
  ## output
  out <- c(n, naive.lo, onestep.lo, truth, naive.ci, onestep.lo.se,
           onestep.lo.ci, simple_r2, simple_r2_ci)
  names(out) <- c("n", "naive.lo", "onestep.lo", "truth",  
                  "naive.lo.cil", "naive.lo.ciu", "onestep.lo.se", "onestep.lo.cil", "onestep.lo.ciu",
                  "ols.r2", "ols.r2.cil", "ols.r2.ciu")
  return(out)
}

## function that gets an OLS-based estimate of R^2
get_lm_ests <- function(data, indices, j) {
    ## set up stuff for boot
    d <- data[indices, ]
    small_d <- d[, -j]
    names(small_d) <- c("x", "y")

    ## fit the linear models
    mod <- lm(y ~ ., data = d)
    small_mod <- lm(y ~ ., data = small_d)
    
    ## get estimators
    r2 <- 1 - mean((d$y - fitted(mod))^2)/mean((d$y - mean(d$y))^2)
    red_r2 <- 1 - mean((d$y - fitted(small_mod))^2)/mean((d$y - mean(d$y))^2)
    return(r2 - red_r2)
}