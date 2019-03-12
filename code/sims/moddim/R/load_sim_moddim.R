###############################################################
##
## FILE:    load_sim_moddim.R
##
## CREATED: 6 February 2017
##
## AUTHOR:  Brian Williamson
##
## PURPOSE: Load the results from the high dimensional simulations, two-step estimating procedure
##
###############################################################
source("simulation_helper_functions.R")

## simulation parameters
## sample size, dimension
ns <- c(100, 300, 500, 1000)
p <- 15

## vector of j combinations
js <- rbind(11, 1:5, 6:10, 11:15, c(1, 2, 3, 6, 7))
## set up the simulation parameter matrix
df <- data.frame(js, set = "A")
df <- rbind(df, data.frame(js, set = "B"))
df$id <- rep(1:5, 2)


## truth for each setting
## truths for A
erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1

truth.A <- c(-(49/(9*exp(1))) + (49*(1 + exp(2)))/(18*exp(2)), ## A.1
             3/2 - (1 - 1/2*erf(1/sqrt(2)))^2 - erf(1/sqrt(2)) + 
               1/2*(-4*erf(1/(2*sqrt(2))) + 5*erf(1/sqrt(2))), ## A.2
             1185/1024 - 1/(4*pi), ## A.3
             -(49/(9*exp(1))) + (49*(1 + exp(2)))/(18*exp(2)), ## A.4
             2721/1024 + sqrt(2/pi) + 1/(
               4*pi) - (1 + 1/sqrt(2*pi) - 1/2*erf(1/sqrt(2)))^2 - 
               erf(1/sqrt(2)) - erf(1/sqrt(2))/sqrt(2*pi) + 
               1/2*(-4*erf(1/(2*sqrt(2))) + 5*erf(1/sqrt(2)))) ## A.5
truth.11.num <- readRDS("truth_11_num.Rdata")
truth.12367.num <- readRDS("truth_12367_num.rds")

truth.B <- c(truth.11.num,
             1.2451755545165184,
             1.3920344125019724,
             (49*(-1 + exp(1.0/4))*(exp(1.0/4) - cos(2)))/(18*sqrt(exp(1))),
             truth.12367.num)

truth.A.std <- truth.A/(sum(truth.A[2:4]) + 1)
truth.B.std <- truth.B/(sum(truth.B[2:4]) + 1)

df$truth <- c(truth.A.std, truth.B.std)

## set up output matrix
output <- data.frame(bias = rep(0, 2*4*dim(df)[1]), variance = rep(0, 2*4*dim(df)[1]),
                     mse = rep(0, 2*4*dim(df)[1]), sd = rep(0, 2*4*dim(df)[1]),
                     cover = rep(0, 2*4*dim(df)[1]), n = rep(0, 2*4*dim(df)[1]),
                     id = rep(0, 2*4*dim(df)[1]), setting = rep("0", 2*4*dim(df)[1]),
                     stringsAsFactors = FALSE)

## set up the grid for n, index, setting
get_current <- function(job_id) {
  vec <- rep(1:40, each = 10)
  idx <- vec[job_id]
  return(idx)
}
param_list <- expand.grid(set = unique(df$set), n = ns, id = 1:5)
param_grid <- param_list[order(param_list$set, param_list$n, param_list$id), ]
param_grid$truth <- c(rep(df$truth[df$set == "A"], length(ns)), rep(df$truth[df$set == "B"], length(ns)))

## list all of the files
vec <- 1:400
files <- as.list(paste("./output/sim_mod_output_t_", vec - 1, ".csv", sep = ""))


## load in the data, calculating coverage as we go
# currently don't have all of 9,10
for (t in 1:40) {
  ## load the data
  out_n_j_set <- lapply(files[get_current(vec) == t], read.csv)
  out <- do.call("rbind.data.frame", out_n_j_set)
  out2 <- out[, -1]
  ## calculate bias, variance, etc.
  current_set <- param_grid[t, ]
  current <- calculateBiasCoverHidimPy(out2, current_set$n, 
                                         current_set$id, 
                                         current_set$truth,
                                       extra = FALSE)
  current$setting <- as.character(current_set$set)
  ## add to output matrix
  output[1:2 + 2*(t-1), ] <- current
}

## add which type it is
output$type <- c("naive", "onestep")

## remove any zeros
output <- output[output$n != 0, ]

## save it off
saveRDS(output, "output_moddim_naive_proposed.rds")

## first split out by setting
setting.a <- subset(output, output$setting == "A")
setting.b <- subset(output, output$setting == "B")

## next split out by j
a.1 <- subset(setting.a, setting.a$id == 1)
a.2 <- subset(setting.a, setting.a$id == 2)
a.3 <- subset(setting.a, setting.a$id == 3)
a.4 <- subset(setting.a, setting.a$id == 4)
a.5 <- subset(setting.a, setting.a$id == 5)

b.1 <- subset(setting.b, setting.b$id == 1)
b.2 <- subset(setting.b, setting.b$id == 2)
b.3 <- subset(setting.b, setting.b$id == 3)
b.4 <- subset(setting.b, setting.b$id == 4)
b.5 <- subset(setting.b, setting.b$id == 5)

fig.width <- 2590
fig.height <- fig.width

# make a list of output datasets
lst <- as.list(c(paste0("a.", 1:5), paste0("b.", 1:5)))
output_list <- lapply(lst, function(x) eval(parse(text = x)))

# make a list of desired summaries
summary_list <- list("bias", "variance", "coverage")

# make a list of filenames to print to (one for each dataset/summary combination)
s_nms <- c("11", "1-5", "6-10", "11-15", "1-2-3-6-7")
# function to create a name for a given output dataset name (e.g., "a.1"),
# list of summaries, and vector of names
gen_plot_nm <- function(str1, str2, nms) {
  # paste on the appropriate combination
  tmp <- unlist(strsplit(str1, ".", fixed = TRUE))
  set <- tmp[1]
  id <- as.numeric(tmp[2])
  return(lapply(str2, function(x) paste0(x, "_vs_n_mod_set_", set, "_s_", nms[id], ".png")))
}
plot_name_list <- lapply(lst, gen_plot_nm, str2 = summary_list, nms = s_nms)
#------------------------------------------
# Produce the plots
#------------------------------------------

# function to produce a plot
main_adds <- paste0(", s = ", s_nms)
gen_bias_plot <- function(plot_nm, nms, adds, out) {
  tmp <- unlist(strsplit(unlist(strsplit(plot_nm, "_", fixed = TRUE)), ".", fixed = TRUE))
  plotSummaryModdim(out, type = "bias", ylim = c(min(out$bias) - 1, max(out$bias) + 1),
                    pch = list(16, 18), lgnd.pch = c(16, 18), est.type = c("onestep", "naive"),
                    main.add = adds[which(tmp[8] == nms)], ests = c("Proposed", "Naive"),
                    lgnd.pos = "bottomleft", lgnd.col = c("blue", "red"), lgnd.cex = 1,
                    cex = 1.5, cex.lab = 1.25, cex.axis = 1.25
                    )
}
gen_var_plot <- function(plot_nm, nms, adds, out) {
  tmp <- unlist(strsplit(unlist(strsplit(plot_nm, "_", fixed = TRUE)), ".", fixed = TRUE))
  plotSummaryModdim(out, type = "variance", ylim = c(min(out$variance) - 1, max(out$variance) + 1),
                    pch = list(16, 18), lgnd.pch = c(16, 18), est.type = c("onestep", "naive"),
                    main.add = adds[which(tmp[8] == nms)], ests = c("Proposed", "Naive"),
                    lgnd.pos = "bottomright", lgnd.col = c("blue", "red"), lgnd.cex = 1,
                    cex = 1.5, cex.lab = 1.25, cex.axis = 1.25
  )
}
gen_cov_plot <- function(plot_nm, nms, adds, out, wd) {
  tmp <- unlist(strsplit(unlist(strsplit(plot_nm, "_", fixed = TRUE)), ".", fixed = TRUE))
  plotSummaryModdim(out, type = "coverage", ylim = c(0, 1),
                    pch = list(16, 18), lgnd.pch = c(16, 18), est.type = c("onestep", "naive"),
                    main.add = adds[which(tmp[8] == nms)], ests = c("Proposed", "Naive"),
                    lgnd.pos = "right", lgnd.col = c("blue", "red"), lgnd.cex = 1,
                    cex = 1.5, cex.lab = 1.25, cex.axis = 1.25
  )
}
gen_plots <- function(plot_nms, nms, adds, out, wd, ht, unit, rs) {
  png(paste0("plots/", plot_nms[[1]]), width = wd, height = ht, units = unit, res = rs)
    gen_bias_plot(plot_nms[[1]], nms, adds, out)
  dev.off()
  png(paste0("plots/", plot_nms[[2]]), width = wd, height = ht, units = unit, res = rs)
    gen_var_plot(plot_nms[[2]], nms, adds, out)
  dev.off()
  png(paste0("plots/", plot_nms[[3]]), width = wd, height = ht, units = unit, res = rs)
    gen_cov_plot(plot_nms[[3]], nms, adds, out)
  dev.off()
}

#------------------------------------
# Now loop through
#------------------------------------
for (i in 1:length(output_list)) {
  gen_plots(plot_name_list[[i]], s_nms, main_adds, output_list[[i]], fig.width, fig.height, "px", 300)
}
