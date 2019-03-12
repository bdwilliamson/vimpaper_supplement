###########################################################################################################
##
## FILE:    sim_ests_lm.R
##
## CREATED: 24 August 2018
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
calculateEsts <- function(data, truth, n, j, h.f.l, h.m.l, b, loess_ord, interaction){
  
  ## calculate the dimension
  p <- dim(data)[2] - 1
  
  x <- data[, 1:p]
  
  small.data <- data[,-j]
  names(small.data) <- c("x", "y")
  
  ## outer layer of cv for the estimators
  folds_outer <- cv_2(n, 5)
  cv_ests_anova <- vector("numeric", 5)
  cv_updates_anova <- vector("numeric", 5)
  ses_anova <- vector("numeric", 5)
  cv_ests_r2 <- vector("numeric", 5)
  ses_r2 <- vector("numeric", 5)
  for (l in 1:5) {
    # inner layer of cv for the bandwidths
    train_outer <- data[folds_outer[, l] == 0, ]
    test_outer_1 <- data[folds_outer[, l] == 1, ]
    test_outer_2 <- data[folds_outer[, l] == 2, ]
    small_train_outer <- small.data[folds_outer[, l] == 0, ]
    train_x <- train_outer[, 1:p]

    ## do cross-validation for the full model
    if (loess_ord == 0 | loess_ord == 1) {
      fullmod <- get_mod(train_outer, 5, h.f.l, loess_ord, interaction = TRUE)
      fullmod.lo <- fullmod$mod
      h.f.lo <- fullmod$h
      fhat.lo <- fitted(fullmod.lo)  
      
      ## reduced
      small_train_outer_dat <- data.frame(x = small_train_outer$x, y = fhat.lo)
      minusmod <- get_mod(small_train_outer_dat, 5, h.f.l, loess_ord)
      minusmod.lo <- minusmod$mod
      h.m.lo <- minusmod$h
      fhat.minus.lo <- fitted(minusmod.lo)
      
      cat("\n Full bandwidth:", h.f.lo, "\n")
      cat("\n Reduced bandwidth:", h.m.lo, "\n")
      
      ## get predicted values on the test data, for cv naive
      preds_full_1 <- predict(fullmod.lo, test_outer_1[, 1:p])
      preds_red_1 <- predict(minusmod.lo, test_outer_1[,1:p][,-j])
      
      ## get predicted values on the second set of test data, for cv update
      preds_full_2 <- predict(fullmod.lo, test_outer_2[, 1:p])
      preds_red_2 <- predict(minusmod.lo, test_outer_2[,1:p][,-j])
    } else {
      fullmod <- SuperLearner::SuperLearner(Y = train_outer$y, X = train_outer[, 1:p], SL.library = h.f.l, cvControl = list(V = 5), method = "method.NNLS2")
      full_fit <- fullmod$SL.predict
      minusmod <- SuperLearner::SuperLearner(Y = full_fit, X = train_outer[, 1:p][,-j,drop=FALSE], SL.library = h.f.l, cvControl = list(V = 5), method = "method.NNLS2")
      
      ## predictions
      preds_full_1 <- predict(fullmod, test_outer_1[, 1:p])$pred
      preds_red_1 <- predict(minusmod, test_outer_1[, 1:p][,-j,drop=FALSE])$pred
      preds_full_2 <- predict(fullmod, test_outer_2[, 1:p])$pred
      preds_red_2 <- predict(minusmod, test_outer_2[, 1:p][,-j,drop=FALSE])$pred
    }
  
    ## calculate the estimates for naive and onestep
    cv_est <- mean((preds_full_1 - preds_red_1)^2, na.rm = TRUE)/mean((test_outer_1$y - mean(test_outer_1$y))^2)
    cv_ests_anova[l] <- cv_est
    cv_ests_r2[l] <- (1 - mean((test_outer_1$y - preds_full_1)^2, na.rm = TRUE)/mean((test_outer_1$y - mean(test_outer_1$y, na.rm = TRUE))^2, na.rm = TRUE)) - (1 - mean((test_outer_1$y - preds_red_1)^2, na.rm = TRUE)/mean((test_outer_1$y - mean(test_outer_1$y, na.rm = TRUE))^2, na.rm = TRUE))

    # update
    update <- vimp::vimp_update(preds_full_2, preds_red_2, test_outer_2$y, na.rm = TRUE)
    cv_update <- mean(update, na.rm = TRUE)
    cv_updates_anova[l] <- cv_update
    ds_full <- ((test_outer_2$y - preds_full_2)^2 - mean(((test_outer_2$y - preds_full_2))^2, na.rm = TRUE))
    ds_red <- ((test_outer_2$y - preds_red_2)^2 - mean(((test_outer_2$y - preds_red_2))^2, na.rm = TRUE))
    naive_denom <- mean((test_outer_2$y - mean(test_outer_2$y, na.rm = TRUE))^2, na.rm = TRUE)
    d_denom <- (test_outer_2$y - mean(test_outer_2$y, na.rm = TRUE))^2 - naive_denom
    ic_full_mse <- ds_full/naive_denom - mean(((test_outer_2$y - preds_full_2))^2, na.rm = TRUE)/(naive_denom^2)*d_denom
    ic_red_mse <- ds_red/naive_denom - mean(((test_outer_2$y - preds_red_2))^2, na.rm = TRUE)/(naive_denom^2)*d_denom
    ic_full_r2 <- (-1)*ic_full_mse
    ic_red_r2 <- (-1)*ic_red_mse
    update_r2 <- ic_full_r2 - ic_red_r2
    ses_anova[l] <- sqrt(mean(update^2, na.rm = TRUE))
    ses_r2[l] <- sqrt(mean(update_r2^2, na.rm = TRUE))
  }
  # take the average for the sample splitting estimate; ANOVA
  cv_est_anova <- mean(cv_ests_anova)
  cv_update_anova <- mean(cv_updates_anova)
  cv_onestep_anova <- cv_est_anova + cv_update_anova

  ## naive estimator of R^2; same as onestep ANOVA without CV (with CV, may be slightly different)
  cv_est_r2 <- mean(cv_ests_r2)

  ## calculate a CI
  cv_se_anova <- mean(ses_anova)/sqrt(n)
  ci_anova <- vimp::vimp_ci(cv_onestep_anova, cv_se_anova, 0.95)
  cv_se_r2 <- mean(ses_r2)/sqrt(n)
  ci_r2 <- vimp::vimp_ci(cv_est_r2, cv_se_r2, 0.95)
  
  ## calculate the linear regression estimators
  simple_lm_mod_full <- lm(y ~ ., data = data)
  simple_lm_mod_reduced <- lm(y ~ ., data = small.data)
  interact_lm_mod_full <- lm(y ~ V1*V2, data = data)
  interact_lm_mod_reduced <- lm(y ~ x, data = small.data)
  r2_simple <- 1 - mean((data$y - fitted(simple_lm_mod_full))^2)/mean((data$y - mean(data$y))^2) - (1 - mean((data$y - fitted(simple_lm_mod_reduced))^2)/mean((data$y - mean(data$y))^2))
  r2_interact <- 1 - mean((data$y - fitted(interact_lm_mod_full))^2)/mean((data$y - mean(data$y))^2) - (1 - mean((data$y - fitted(interact_lm_mod_reduced))^2)/mean((data$y - mean(data$y))^2))
  ## get a bootstrap CI for the linear regression estimators
  simple_r2s <- boot::boot(data = data, statistic = get_lm_ests, R = b, interact = FALSE, j = j)
  simple_r2_init <- boot::boot.ci(simple_r2s, type = "perc")
  simple_r2_ci <- simple_r2_init$percent[4:5]
  interact_r2s <- boot::boot(data = data, statistic = get_lm_ests, R = b, interact = TRUE, j = j)
  interact_r2_init <- boot::boot.ci(interact_r2s, type = "perc")
  interact_r2_ci <- interact_r2_init$percent[4:5]
  
  ## output
  out <- c(n, cv_est_anova, cv_onestep_anova, truth, cv_se_anova, ci_anova,
           cv_est_r2, cv_se_r2, ci_r2, r2_simple, simple_r2_ci, 
           r2_interact, interact_r2_ci)
  names(out) <- c("n", "cv_est_anova", "cv_onestep_anova", "truth", "cv_se_anova",
   "cv_cil_anova", "cv_ciu_anova", "cv_est_r2", "cv_se_r2", "cv_cil_r2",
   "cv_ciu_r2", "r2_simple", "r2_simple_cil", "r2_simple_ciu", "r2_interact", "r2_interact_cil",
   "r2_interact_ciu")
  return(out)
}

get_lm_ests <- function(data, indices, j, interact) {
    ## set up stuff for boot
  d <- data[indices, ]
  small_d <- d[, -j]
  names(small_d) <- c("x", "y")

  ## fit the linear models
  if (!interact) {
    mod <- lm(y ~ ., data = d)
    small_mod <- lm(y ~ ., data = small_d)
  } else {
      mod <- lm(y ~ (V1 + V2)^4, data = d)
      small_mod <- lm(y ~ I(x^4) + I(x^3) + I(x^2) + I(x), data = small_d)
  }
  r2 <- 1 - mean((d$y - fitted(mod))^2)/mean((d$y - mean(d$y))^2)
  red_r2 <- 1 - mean((d$y - fitted(small_mod))^2)/mean((d$y - mean(d$y))^2)
  return(r2 - red_r2)
}


get_cv_ests <- function(data, indices, j) {
    ## set up stuff for boot
    d <- data[indices, ]
    small_d <- d[, -j]
    names(small_d) <- c("x", "y")
    n <- dim(d)[1]
    p <- dim(d)[2] - 1

    ## run the CV
    ## outer layer of cv for the estimators
  folds_outer <- cv_2(dim(d)[1], 5)
  cv_ests_anova <- vector("numeric", 5)
  cv_ests_r2 <- vector("numeric", 5)
  for (l in 1:5) {
    # inner layer of cv for the bandwidths
    train_outer <- d[folds_outer[, l] == 0, ]
    test_outer_1 <- d[folds_outer[, l] == 1, ]
    test_outer_2 <- d[folds_outer[, l] == 2, ]
    small_train_outer <- small_d[folds_outer[, l] == 0, ]
    train_x <- train_outer[, 1:p]

    ## do cross-validation for the full model
    fullmod <- tryCatch(get_mod(train_outer, 5, h.f.l), error = function(e) NA)
    if (any(is.na(fullmod))) {
        cv_ests_anova[l] <- NA
        cv_ests_r2[l] <- NA
    } else {
        fullmod.lo <- fullmod$mod
    h.f.lo <- fullmod$h
    fhat.lo <- fitted(fullmod.lo)
  
    ## now run through for the small model, based on the fits from
    small_train_outer_dat <- data.frame(x = small_train_outer$x, y = fhat.lo)
    minusmod <- get_mod(small_train_outer_dat, 5, h.f.l)
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
    cv_ests_anova[l] <- cv_est
    cv_ests_r2[l] <- (1 - mean((test_outer_1$y - preds_full_1)^2, na.rm = TRUE)/mean((test_outer_1$y - mean(test_outer_1$y, na.rm = TRUE))^2, na.rm = TRUE)) - (1 - mean((test_outer_1$y - preds_red_1)^2, na.rm = TRUE)/mean((test_outer_1$y - mean(test_outer_1$y, na.rm = TRUE))^2, na.rm = TRUE))
    }
    
  }
  # take the average for the sample splitting estimate
  cv_est_anova <- mean(cv_ests_anova, na.rm = TRUE)
  cv_est_r2 <- mean(cv_ests_r2, na.rm = TRUE)

  return(c(cv_est_anova, cv_est_r2))

} 