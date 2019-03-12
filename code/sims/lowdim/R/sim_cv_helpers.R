###########################################################################################################
##
## FILE:    sim_cv_helpers.R
##
## CREATED: 19 January 2018
##
## AUTHOR:  Brian Williamson
##
## PURPOSE: helper functions for cv simulation
############################################################################################################


# do cross-validation, return algorithm given data and number of folds
get_mod <- function(data, v, h, ord, interaction = FALSE) {
    # get the folds
    folds <- cv(dim(data)[1], v)

    # cross validate to get the algorithm
    errs <- matrix(0, ncol = 5, nrow = length(h))
    for (i in 1:length(h)) {
        for (k in 1:v) {
            ## set up the training and test sets
            train <- data[folds$subsets[folds$which != k], ]
            test <- data[folds$subsets[folds$which == k], ]
      
            ## fit the model on the training set
            if (dim(train)[2] > 2) {
              if (interaction) {
                mod.lo <- tryCatch(suppressWarnings(loess(y ~ .^2, data = train, degree = ord, span = h[i])),
                                   error = function(e) list(n = NA)) 
              } else {
                mod.lo <- tryCatch(suppressWarnings(loess(y ~ ., data = train, degree = ord, span = h[i])),
                                   error = function(e) list(n = NA))  
              }
            } else {
              mod.lo <- tryCatch(suppressWarnings(loess(y ~ x, data = train, degree = ord, span = h[i])),
                                 error = function(e) list(n = NA))
            }
            

            ## predict on the test set
            if (!is.na(mod.lo$n)) {
              preds.lo <- predict(mod.lo, newdata = test)
              
              ## figure out which x's are on the boundary of the sample space;
              ## don't want to use them for MSE
              scaled.x <- (test[, -dim(test)[2]]+1)/2
              boundary <- scaled.x < 0.02 | scaled.x > 0.98
              if (!is.null(dim(boundary))) {
                check <- apply(boundary, 1, function(x) !x[1] & !x[2])
              } else {
                check <- !boundary
              }
              
              ## calculate the error
              err <- mean((preds.lo[check] - test$y[check]) ^ 2, na.rm = TRUE)  
            } else {
              err <- NA
            }
            
            errs[i,k] <- err
        }
    }
    err <- apply(errs, 1, mean, na.rm = TRUE)
    indx <- which.min(err)
    h_min <- h[indx]
    if (interaction) {
      mod <- suppressWarnings(loess(y ~ .^2, data = data, degree = ord, span = h_min))
    } else {
      mod <- suppressWarnings(loess(y ~ ., data = data, degree = ord, span = h_min))  
    }
    return(list(mod = mod, h = h_min))
}