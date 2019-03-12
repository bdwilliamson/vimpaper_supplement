###########################################################################################################
##
## FILE:    sim_lowdim_data.R
##
## CREATED: 12 January 2018
##
## AUTHOR:  Brian Williamson
##
## PURPOSE: look at coverage of confidence intervals and bias for loess and np, based on cv bandwidths
##          using the standardized parameter of interest, prop. of total variability version
##          general function for low dimensional simulations
############################################################################################################

# generate data
gen_data <- function(n, p, null) {
  # generate X
  x <- replicate(p, runif(n, -1, 1))
  
  # apply the function to the x's
  # if null, only use one feature
  if (null) {
  	smooth <- (25/9)*(x[,1])^2
  	truths <- c((500/729)/(1+500/729), 0)
  } else {
  	smooth <- (x[,1])^2*(x[,1]+7/5) + (25/9)*(x[,2])^2	
  	truths <- c((2497/7875)/(1 + 2497/7875 + 500/729), (500/729)/(1 + 2497/7875 + 500/729))
  }
  
  ## generate Y ~ Normal (smooth, 1)
  y <- smooth + rnorm(n, 0, 1)
  
  return(list(dat = as.data.frame(cbind(x,y)), truth = truths))
}