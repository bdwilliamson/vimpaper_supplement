##############################################################################################################################
##
## FILE:    simulation_helper_functions.R
##
## CREATED: 02 November 2017
##
## AUTHOR:  Brian Williamson
##
## PURPOSE: helper functions to make plotting etc. in the load* files simpler and
##          more readable.
##
##                           UPDATES
## DDMMYY    INIT    COMMENTS
## ---------------------------
##############################################################################################################################

###############################################################################
##
## GENERAL FUNCTIONS
##
###############################################################################
## function to plot bias, variance, mse, coverage, low dimensional
## ARGS: output - the data to summarize
##       type - the type of plot
## ylim, pch, - graphical parameters
## cex.main, cex.axis, 
## cex.lab, mar 
##       ests - the labels for the legend
##          B - the number of monte carlo reps (for error bars)
##          n - the vector of ns
##     lgnd.* - edit the legend
## 
## RETURNS: a plot of the given summary type (bias, variance, mse, coverage)
plotSummary <- function(output, type = "bias", ylim = c(-7.5, 3), 
                        pch = rep(c(16, 8), 8), pch.2 = NULL, cex.main = 2, main.add = NULL, B = 3000, ests = c("Proposed", "Naive"),
                        cex.axis = 1.6, cex.lab = 2, mar = c(5, 5, 0, 2) + 0.1, n = c(100, 300, 500, 700, seq(1000, 4000, by = 1000)),
                        lgnd.pos = "topleft", lgnd.txt = c("j=1", "j=2"), lgnd.col = c("blue", "red", "black", "black"),
                        lgnd.pch = c(19, 19, 16, 8), lgnd.cex = 1.25, print.type = TRUE, est.type = c("naive", "onestep", "tmle"),
                        cex = 1) {
  ## set the graphical parameters
  par(cex = cex,cex.axis = cex.axis, cex.lab = cex.lab, mar = mar, oma = c(0, 0, 0, 0))
  ## if pch and pch.2 aren't lists, make them lists
  if (!is.list(pch)) stop("Please enter a list for pch values")
  ## plot the results; plot differs for bias, variance, MSE, coverage
  if (type == "bias") {
    ## plot results for the first one
    plot(subset(output, output$type == est.type[1])$n, subset(output, output$type == est.type[1])$bias, 
         ylim = ylim, ylab = expression(paste(sqrt(n), " x est. ", bias[n])), xlab = "n",
         pch = pch[[1]], col = lgnd.col[1], axes = FALSE, cex = cex.main)
    
    ## plot results for the proposed estimator
    if (length(est.type) > 1) {
      for (i in 2:length(est.type)) {
        points(subset(output, output$type == est.type[i])$n, subset(output, output$type == est.type[i])$bias, 
               pch = pch[[i]], col = lgnd.col[i], cex = cex.main)   
      }
    } 
    
    ## monte-carlo error bars
    arrows(output$n[output$type %in% est.type], output$bias[output$type %in% est.type] - 1.96*output$sd[output$type %in% est.type]/sqrt(B),
           output$n[output$type %in% est.type], output$bias[output$type %in% est.type] + 1.96*output$sd[output$type %in% est.type]/sqrt(B),
           length = 0.05, angle = 90, code = 3, cex = cex.main)
    
    ## line at y = 0
    abline(h = 0)
    
  } else if (type == "variance") {        
    ## plot the naive
    plot(subset(output, output$type == est.type[1])$n, subset(output, output$type == est.type[1])$variance, 
         ylim = ylim, ylab = expression(paste(n, " x est. ", variance[n])), xlab = "n",
         pch = pch[[1]], col = lgnd.col[1], axes = FALSE, cex = cex.main)
    
    if (length(est.type) > 1) {
      for (i in 2:length(est.type)) {
        points(subset(output, output$type == est.type[i])$n, subset(output, output$type == est.type[i])$variance, pch = pch[[i]], col = lgnd.col[i], cex = cex.main)
      }
    }
    
    ## line at y = 0
    abline(h = 0)
    
    
  } else if (type == "coverage") { 
    ## plot for the naive
    plot(subset(output, output$type == est.type[1])$n, subset(output, output$type == est.type[1])$cover, 
         ylim = ylim, ylab = "Coverage", xlab = "n",
         pch = pch[[1]], col = lgnd.col[1], axes = FALSE, cex = cex.main)
    ## proposed
    if (length(est.type) > 1) {
      for (i in 2:length(est.type)) {
        points(subset(output, output$type == est.type[i])$n, subset(output, output$type == est.type[i])$cover, pch = pch[[i]], col = lgnd.col[i], cex = cex.main)
      }
    }
    
    ## line at nominal 95%
    abline(h = 0.95, lty = 2)
  } else if (type == "mse") {
    ## plot the naive estimator
    plot(subset(output, output$type == est.type[1])$n, subset(output, output$type == est.type[1])$mse, 
         ylim = ylim, ylab = expression(paste(n, " x est. ", MSE[n])), xlab = "n",
         pch = pch[[1]], col = lgnd.col[1], axes = FALSE, cex = cex.main)
    ## the proposed estimator
    if (length(est.type) > 1) {
      for (i in 2:length(est.type)) {
        points(subset(output, output$type == est.type[i])$n, subset(output, output$type == est.type[i])$mse, pch = pch[[i]], col = lgnd.col[i], cex = cex.main)
      }
    }
    
    arrows(output$n[output$type %in% est.type], output$mse[output$type %in% est.type] - 1.96*output$sd[output$type %in% est.type]/sqrt(B),
           output$n[output$type %in% est.type], output$mse[output$type %in% est.type] + 1.96*output$sd[output$type %in% est.type]/sqrt(B),
           length = 0.05, angle = 90, code = 3, cex = cex.main)
    ## line at y = 0
    abline(h = 0)
    ## legend
  }
  ## set the title
  if (print.type) {
    title(main = paste(toupper(type), main.add, sep = ""), line = -2, cex.main = cex.main)
  } else {
    tmp <- unlist(strsplit(main.add, ","))
    title(main = tmp, line = -2, cex.main = cex.main)
  }
  ## axis labels
  axis(side = 1, at = c(100, 300, 500, 700, seq(1000, 10000, by=1000)))
  if (type == "bias" | type == "variance" | type == "mse") {
    axis(side = 2, at = seq(-10, 70, by = 1))
  } else {
    axis(side = 2, at = seq(0, 1, 0.1))
  }
  ## legend
  ## first add on spaces corresponding to the length of pch
  if ((length(pch) > 1) & (length(pch[[1]]) > 1)) {
    for (i in 1:(length(pch) - 1)) {
      ests <- paste(" ", ests)
      lgnd.txt <- paste(" ", lgnd.txt)
    }
  }
  lgnd <- legend(lgnd.pos, legend = c(ests, lgnd.txt), col = lgnd.col, pch = lgnd.pch, cex = lgnd.cex)
  
  ## if second set of pch's, second call to legend
  if(length(pch) > 1 & length(pch[[1]]) > 1) {
    legend(lgnd$rect$left + 0.075*lgnd$rect$w, lgnd$rect$top, legend = rep("", length(c(ests, lgnd.txt))), 
           pch = c(unlist(lapply(pch, function(x) x[2]))[1:length(pch)], pch[[2]]), cex = lgnd.cex, bty = "n",
           col = lgnd.col)
    if (length(pch) > 2) {
      for (i in 3:length(pch)) {
        legend(lgnd$rect$left + 0.15*lgnd$rect$w + 0.07*lgnd$rect$w*(i-3), lgnd$rect$top - 0.5*lgnd$rect$h - 0.08*lgnd$rect$h*(length(pch) - 3), legend = rep("", 2), 
               pch = pch[[i]], cex = lgnd.cex, bty = "n",
               col = "black")
      }
    }
  }
  ## box around the plot
  box()
}
###############################################################################
##
## FUNCTIONS FOR LOW-DIMENSIONAL VECTOR OF FEATURES
##
###############################################################################
## calculate the bias for low-dimensional sims
## calculate bias for small n
calculateVimpaperBias <- function(out, n, j) {
  
  ## variance
  vars <- apply(out, 1, function(x) var(unlist(x), na.rm = TRUE))
  naiveVar <- vars[2]*n
  onestepVar <- vars[3]*n
  variance <- c(naiveVar, onestepVar)
  names(variance) <- c("naive", "onestep")
  
  ## mse
  naiveMSE <- mean(apply(out, 2, function(x) ((x[2] - x[4])^2)), na.rm = TRUE)*n
  onestepMSE <- mean(apply(out, 2, function(x) ((x[3] - x[4])^2)), na.rm = TRUE)*n
  mse <- c(naiveMSE, onestepMSE)
  names(mse) <- c("naive", "onestep")
  
  means <- apply(out, 1, function(x) mean(unlist(x), na.rm = TRUE))
  naiveBias <- (means[2] - means[4])*sqrt(n)
  onestepBias <- (means[3] - means[4])*sqrt(n)
  bias <- c(naiveBias, onestepBias)
  names(bias) <- c("naive", "onestep")
  
  ## naive sd
  sd.n <- sd(unlist(out[2,]), na.rm = TRUE)*sqrt(n)
  ## onstep sd
  sd.o <- sd(unlist(out[3,]), na.rm = TRUE)*sqrt(n)

  sd <- c(sd.n, sd.o)
  
  ## set up the return matrix
  ret <- data.frame(bias, variance, mse, sd, n, j)
  return(ret)
}

## indices is a vector for estimates and CIs
## 1 is truth
## 2 is naive est
## 3 is onestep est
## 4 is naive cil
## 5 is naive ciu
## 6 is onestep cil
## 7 is onestep ciu
calculateBias <- function(out, n, j, indices) {
  
  ## variance
  vars <- apply(out, 1, function(x) var(unlist(x), na.rm = TRUE))
  naiveVar <- vars[indices[2]]*n
  onestepVar <- vars[indices[3]]*n
  variance <- c(naiveVar, onestepVar)
  names(variance) <- c("naive", "onestep")
  
  ## mse
  naiveMSE <- mean(apply(out, 2, function(x) ((x[indices[2]] - x[indices[1]])^2)), na.rm = TRUE)*n
  onestepMSE <- mean(apply(out, 2, function(x) ((x[indices[3]] - x[indices[1]])^2)), na.rm = TRUE)*n
  mse <- c(naiveMSE, onestepMSE)
  names(mse) <- c("naive", "onestep")
  
  means <- apply(out, 1, function(x) mean(unlist(x), na.rm = TRUE))
  naiveBias <- (means[indices[2]] - means[indices[1]])*sqrt(n)
  onestepBias <- (means[indices[3]] - means[indices[1]])*sqrt(n)
  bias <- c(naiveBias, onestepBias)
  names(bias) <- c("naive", "onestep")
  
  ## naive sd
  sd.n <- sd(unlist(out[indices[2],]), na.rm = TRUE)*sqrt(n)
  ## onstep sd
  sd.o <- sd(unlist(out[indices[3],]), na.rm = TRUE)*sqrt(n)
  
  sd <- c(sd.n, sd.o)
  
  ## set up the return matrix
  ret <- data.frame(bias, variance, mse, sd, n, j)
  return(ret)
}

calculateVimextBias <- function(out, n, j) {
  
  ## variance
  vars <- apply(out, 1, function(x) var(unlist(x), na.rm = TRUE))
  variance <- vars[2]
  names(variance) <- "tmle"
  
  ## mse
  mse <- mean(apply(out, 2, function(x) ((x[2] - x[3])^2)), na.rm = TRUE)*n
  names(mse) <- "tmle"
  
  ## mse
  means <- apply(out, 1, function(x) mean(unlist(x), na.rm = TRUE))
  bias <- (means[2] - means[3])*sqrt(n)
  names(bias) <- "tmle"
  
  ## sd
  sd <- sd(unlist(out[2, ]), na.rm = TRUE)*sqrt(n)
  
  ## set up the return matrix
  ret <- data.frame(bias, variance, mse, sd, n, j)
  return(ret)
}

calculateJestBias <- function(out, n, j) {
  
  ## variance
  vars <- apply(out, 1, function(x) var(unlist(x), na.rm = TRUE))
  variance <- vars[c(3, 7, 11, 15)]
  names(variance) <- c("multi", "marg", "cf", "plug")
  
  ## mse
  mses <- rowMeans(apply(out, 2, function(x) ((x - x[2])^2)), na.rm = TRUE)*n
  mse <- mses[c(3, 7, 11, 15)]
  names(mse) <- c("multi", "marg", "cf", "plug")
  
  ## bias
  means <- apply(out, 1, function(x) mean(unlist(x), na.rm = TRUE))
  biases <- (means - means[2])*sqrt(n)
  bias <- biases[c(3, 7, 11, 15)]
  names(bias) <- c("multi", "marg", "cf", "plug")
  
  ## sd
  sds <- apply(out, 1, function(x) sd(unlist(x)))*sqrt(n)
  sd <- sds[c(3, 7, 11, 15)]
  names(sd) <- c("multi", "marg", "cf", "plug")
  
  ## set up the return matrix
  ret <- data.frame(bias, variance, mse, sd, n, j, type = c("multi", "marg", "cf", "plug"))
  return(ret)
}

## coverage
calculateCoverJest <- function(out, truth) {
  cis <- out[c(5, 6, 9, 10, 13, 14, 17, 18), ]
  cover <- cbind(cis[1, ] <= truth & cis[2, ] >= truth, 
             cis[3, ] <= truth & cis[4, ] >= truth,
             cis[5, ] <= truth & cis[6, ] >= truth,
             cis[7, ] <= truth & cis[8, ] >= truth)
  return(colMeans(cover))
}

## function to plot bias, variance, mse, coverage, low dimensional
## wrapper function around plotSummary
## ARGS: output - the data to summarize
##       type - the type of plot
## ylim, pch, - graphical parameters
## cex.main, cex.axis, 
## cex.lab, mar 
##       ests - the labels for the legend
##          B - the number of monte carlo reps (for error bars)
##          n - the vector of ns
##     lgnd.* - the legend
## 
## RETURNS: a plot of the given summary type (bias, variance, mse, coverage)
plotSummaryLowdim <- function(output, type = "bias", ylim = c(-7.5, 3), 
    pch = rep(c(16, 8), 8), pch.2 = NULL, cex.main = 2, main.add = NULL, B = 3000, ests = c("Proposed", "Naive"),
    cex.axis = 1.6, cex.lab = 2, mar = c(5, 5, 0, 2) + 0.1, n = c(100, 300, 500, 700, seq(1000, 4000, by = 1000)),
    lgnd.pos = "topleft", lgnd.txt = c("j=1", "j=2"), lgnd.col = c("blue", "red", "black", "black"),
    lgnd.pch = c(19, 19, 16, 8), lgnd.cex = 1.25, print.type = TRUE, est.type = c("onestep", "naive"),
    cex = 1) {

    plotSummary(output, type = type, ylim = ylim, pch = pch, pch.2 = pch.2, cex.main = cex.main, main.add = main.add, B = B, ests = ests,
        cex.axis = cex.axis, cex.lab = cex.lab, mar = mar, n = n, lgnd.pos = lgnd.pos, lgnd.txt = lgnd.txt,
        lgnd.col = lgnd.col, lgnd.pch = lgnd.pch, lgnd.cex = lgnd.cex, print.type = print.type, est.type = est.type, cex = cex)
}

###############################################################################
##
## FUNCTIONS FOR MODERATE-DIMENSIONAL VECTOR OF FEATURES
##
###############################################################################


## Calculate the bias, MSE, variance
## ARGS: out - the output of the simulation
##         n - the sample size
##        id - the situation (1-5)
##     truth - the truth to compare against
## RETURNS: the bias blown up by sqrt(n), variance blown up by n, MSE blown up by n
##          also coverage of confidence interval
## for the cv simulation, only do this for the one-step!
calculateBiasCoverHidimPy <- function(out, n, id, truth, extra = TRUE) {
  
  ## variance
  vars <- apply(out, 2, var)
  naive.var <- vars[1]*n
  onestep.var <- vars[4]*n
  variance <- c(naive.var, onestep.var)
  names(variance) <- c("naive", "onestep")
  
  ## mse
  naive.mse <- mean((out[, 1] - truth) ^ 2)*n
  onestep.mse <- mean((out[, 4] - truth) ^ 2)*n
  mse <- c(naive.mse, onestep.mse)
  names(mse) <- c("naive", "onestep")
  
  means <- apply(out, 2, mean)
  
  naive.bias <- (means[1] - truth)*sqrt(n)
  onestep.bias <- (means[4] - truth)*sqrt(n)
  bias <- c(naive.bias, onestep.bias)
  names(bias) <- c("naive", "onestep")
  
  ## sd
  naive.sd <- sd(out[, 1])*sqrt(n)
  onestep.sd <- sd(out[, 4])*sqrt(n)
  sd <- c(naive.sd, onestep.sd)
  names(sd) <- c("naive", "onestep")
  
  ## coverage
  naive.cover <- mean(out[, 2] <= truth & out[, 3] >= truth)
  onestep.cover <- mean(out[, 6] <= truth & out[, 7] >= truth)
  cover <- c(naive.cover, onestep.cover)
  
  ## r^2, predicted r^2
  if (extra) {
    r2.full <- mean(out[, 9])
    r2.small <- mean(out[, 10])
    r2.pred.full <- mean(out[, 11])
    r2.pred.small <- mean(out[, 12])
    r2.bayes <- ifelse(dim(out)[2] == 13, mean(out[, 13]), NA)
  
    ## set up the return matrix
    ret <- data.frame(bias, variance, mse, sd, cover, r2.full, r2.small, 
                    r2.pred.full, r2.pred.small, r2.bayes, n, id)
    
  } else {
    ret <- data.frame(bias, variance, mse, sd, cover, n, id)
  }
  
  ## set up the return matrix
  # ret <- data.frame(bias, variance, mse, sd, cover, n, id)
  return(ret)
}

calculateBiasVimextHidim <- function(out, n, id, truth) {
  
  ## variance
  vars <- apply(out, 2, var)
  variance <- vars[1]*n
  names(variance) <- "tmle"
  
  ## mse
  mse <- mean((out[, 1] - truth)^2)*n
  names(mse) <- "tmle"
  
  means <- apply(out, 2, mean)
  
  bias <- (means[1] - truth)*sqrt(n)
  names(bias) <- c("tmle")
  
  ## sd
  t.sd <- sd(out[, 1])*sqrt(n)
  names(t.sd) <- c("tmle")
  
  ## coverage
  cover <- mean(out[, 3] <= truth & out[, 4] >= truth)
  names(cover) <- "tmle"
  
  ## set up the return matrix
  ret <- data.frame(bias, variance, mse, t.sd, cover, n, id)
  
  ## set up the return matrix
  # ret <- data.frame(bias, variance, mse, sd, cover, n, id)
  return(ret)
}

calculateBiasModdim <- function(out, n, id, indices) {
  ## variance
  vars <- apply(out, 2, function(x) var(unlist(x), na.rm = TRUE))
  naiveVar <- vars[indices[2]]*n
  onestepVar <- vars[indices[3]]*n
  variance <- c(naiveVar, onestepVar)
  names(variance) <- c("naive", "onestep")
  
  ## mse
  naiveMSE <- mean(apply(out, 1, function(x) ((x[indices[2]] - x[indices[1]])^2)), na.rm = TRUE)*n
  onestepMSE <- mean(apply(out, 1, function(x) ((x[indices[3]] - x[indices[1]])^2)), na.rm = TRUE)*n
  mse <- c(naiveMSE, onestepMSE)
  names(mse) <- c("naive", "onestep")
  
  means <- apply(out, 2, function(x) mean(unlist(x), na.rm = TRUE))
  naiveBias <- (means[indices[2]] - means[indices[1]])*sqrt(n)
  onestepBias <- (means[indices[3]] - means[indices[1]])*sqrt(n)
  bias <- c(naiveBias, onestepBias)
  names(bias) <- c("naive", "onestep")
  
  ## naive sd
  sd.n <- sd(unlist(out[, indices[2]]), na.rm = TRUE)*sqrt(n)
  ## onstep sd
  sd.o <- sd(unlist(out[, indices[3]]), na.rm = TRUE)*sqrt(n)
  
  sd <- c(sd.n, sd.o)
  
  ## coverage
  cover_n <- mean(apply(out, 1, function(x) x[indices[4]] <= x[indices[1]] & x[indices[5]] >= x[indices[1]]), na.rm = TRUE)
  cover_o <- mean(apply(out, 1, function(x) x[indices[6]] <= x[indices[1]] & x[indices[7]] >= x[indices[1]]), na.rm = TRUE)
  
  ## add on coverage
  cover <- c(cover_n, cover_o)
  
  ## set up the return matrix
  ret <- data.frame(bias, variance, mse, sd, cover, n, id)
  return(ret)
}

## function to plot bias, variance, mse, coverage, moderate dimensional
## wrapper function around plotSummary
## ARGS: output - the data to summarize
##       type - the type of plot
## ylim, pch, - graphical parameters
## cex.main, cex.axis, 
## cex.lab, mar 
##       ests - the labels for the legend
##          B - the number of monte carlo reps (for error bars)
##          n - the vector of ns
##     lgnd.* - the legend
## 
## RETURNS: a plot of the given summary type (bias, variance, mse, coverage)
plotSummaryModdim <- function(output, type = "bias", ylim = c(-7.5, 3), 
    pch = 16, pch.2 = 8, cex.main = 2, main.add = ", s = 11", B = 500, ests = c("Naive", "Proposed"),
    cex.axis = 1.6, cex.lab = 2, mar = c(5, 5, 0, 2) + 0.1, n = c(100, 300, 500, 1000),
    lgnd.pos = "topleft", lgnd.txt = NULL, lgnd.col = c("blue", "red"),
    lgnd.pch = c(16, 8), lgnd.cex = 1.25, print.type = TRUE, est.type = c("naive", "onestep"),
    cex = 1) {

    plotSummary(output, type = type, ylim = ylim, pch = pch, pch.2 = pch.2, cex.main = cex.main, main.add = main.add, B = B, ests = ests,
        cex.axis = cex.axis, cex.lab = cex.lab, mar = mar, n = n, lgnd.pos = lgnd.pos, lgnd.txt = lgnd.txt,
        lgnd.col = lgnd.col, lgnd.pch = lgnd.pch, lgnd.cex = lgnd.cex, print.type = print.type, est.type = est.type, cex = cex)
}