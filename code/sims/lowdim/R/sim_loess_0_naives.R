###########################################################################################################
##
## FILE:    sim_loess_0_naives.R
##
## CREATED: 01 March 2017
##
## AUTHOR:  Brian Williamson
##
## PURPOSE: look at coverage of confidence intervals and bias for loess and np, based on cv bandwidths
##          using the standardized parameter of interest, prop. of tot variability explained version
##          Now only using lower order loess
## UPDATES
## DDMMYY   INIT   COMMENTS
## ------   ----   --------
############################################################################################################


## Function to do the estimation and calculate the naive estimates for loess and np
## ARGS: same as calculateEsts, except no b
## RETURNS: naive estimates for loess and np
getNaives <- function(data, indices, n, j, h.f.l, h.m.l) {
  p <- dim(data)[2] - 1
  d <- data[indices, ]
  x <- d[, 1:p]
  
  ## calculate the loess curve
  fullmod.lo <- suppressWarnings(loess(y ~ V1 + V2, data = d, degree = 0, span = h.f.l))
  fhat.lo <- fitted(fullmod.lo)
  
  ## get the new data frame, without the jth feature
  newx <- x[,-j]
  newdata <- d[,-j]
  names(newdata)[1] <- "x"
  
  minusmod.lo <- suppressWarnings(loess(fhat.lo ~ x, data = newdata, degree = 0, span = h.m.l))
  
  ## get the fitted values for the reduced fit
  fhat.minus.lo <- fitted(minusmod.lo)
  
  ## calculate the naive estimate for np and loess
  naive.lo <- mean((fhat.lo - fhat.minus.lo)^2)/mean((data$y - mean(data$y))^2)
  
  return(naive.lo)
}