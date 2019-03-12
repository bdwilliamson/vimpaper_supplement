###########################################################################################################
##
## FILE:    sim_lowdim_lm_data.R
##
## CREATED: 24 August 2018
##
## AUTHOR:  Brian Williamson
##
## PURPOSE: look at coverage of confidence intervals and scaled empirical bias for:
##          (a) main effects linear regression difference in R^2 and ANOVA
##          (b) main effects plus interaction linear regression difference in R^2 and ANOVA
##          (c) my approach with single flexible estimator (regular and CV)
##          (d) my approach with SuperLearner (regular and CV)
##          when (i) a linear model with no interaction is true, and (ii) a linear model with interaction is true
############################################################################################################

# generate data
gen_data <- function(n, p, interaction, null) {
  # generate X
  x <- replicate(p, runif(n, -1, 1))
  
  # apply the function to the x's
  # if null, only use one feature; if interaction, specify an interaction model 
  # (in null case, this is just a polynomial in a single x)
  if (null) {
    if (interaction) {
        conditional_mean <- x[, 1] ^ 4 
        truths <- c(25/241, 0)
    } else {
        conditional_mean <- 3*x[, 1]
        truths <- c(3/4, 0)
    }
  } else {
    if (interaction) {
        conditional_mean <- (x[, 1] + 0.5*x[, 2]) ^ 4
        truths <- c(11836/37723, 8251/37723)
    } else {
        conditional_mean <- 3*x[, 1] + x[, 2]
        truths <- c(9/13, 1/13)
    }
  }
  
  ## generate Y | X ~ Normal (conditional_mean, 1)
  y <- conditional_mean + rnorm(n, 0, 1)
  
  return(list(dat = as.data.frame(cbind(x,y)), truth = truths))
}