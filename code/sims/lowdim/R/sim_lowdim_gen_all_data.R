###########################################################################################################
##
## FILE:    generate_all_lowdim_data.R
##
## CREATED: 31 December 2018
##
## AUTHOR:  Brian Williamson
##
## PURPOSE: generate all data for lowdim sim, save off; for applying new estimators to old sims
############################################################################################################

## load required functions
source("sim_lowdim_data.R")

## set up static args
B <- 100
ns <- rep(c(100, 300, 500, 700, seq(1000, 10000, by = 1000)), each = 2)
js <- rep(c(1, 2), length(ns)/2)

## function to get current id
get_current <- function(job_id) {
  vec <- rep(1:28, each = 10)
  idx <- vec[job_id]
  return(idx)
}

## loop through and generate data, save as a list for each n
all_reps <- vector(mode = "list", length = 280)
for (i in 1:280) {
  ## generate data
  current_n <- ns[get_current(i)]
  set.seed(current_n + i)
  all_reps[[i]] <- sapply(1:B, function(x) gen_data(current_n, p = 2, null = FALSE), simplify = FALSE)
}

## make a list for each n
for (i in 1:28) {
  current_lst <- all_reps[get_current(1:280) == i]
  saveRDS(current_lst, paste0("sim_lowdim_dataset_n_", ns[i], "_j_", js[i], ".rds"))
}
