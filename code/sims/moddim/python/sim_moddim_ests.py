###########################################################################################################
##
## FILE:    sim_moddim_ests.py
##
## CREATED: 1 February 2017
##
## AUTHOR:  Brian Williamson
##
## PURPOSE: calculate estimates and cis in hidim sim, using two-step estimating procedure, POI that deals with
##          proportion of total variability explained
## UPDATES
## DDMMYY   INIT   COMMENTS
## ------   ----   --------
############################################################################################################

## FUNCTION: applyFuncs
## ARGS: x - an n by p matrix
## RETURNS: the conditional mean
def applyFuncs(x = None):
    import numpy as np
    
    f1 = np.where(np.logical_and(-2 <= x[:, 0], x[:, 0] < 2), np.floor(x[:, 0]), 0) 
    f2 = np.where(x[:, 1] <= 0, 1, 0)
    f3 = np.where(x[:, 2] > 0, 1, 0)
    
    f6 = np.absolute(x[:, 5]/4) ** 3
    f7 = np.absolute(x[:, 6]/4) ** 5
    
    f11 = (7./3)*np.cos(x[:, 10]/2)
    
    ret = f1 + f2 + f3 + f6 + f7 + f11
    
    return ret
    
    
    

## function to bootstrap
## FUNCTION: bootstrap
## ARGS: data - data to bootstrap
##          B - number of resampled datasets
## RETURNS: B boostrap datasets
def bootstrap(data, B):
    import numpy as np
    from sklearn.utils import resample

    boot = np.zeros((B, data.shape[0], data.shape[1]))
    for i in range(0, B):
        boot[i, ...] = resample(data, n_samples = data.shape[0])
    return boot


    
## FUNCTION: getNaives
## ARGS: dat - the data
##         n - the sample size
##         p - the number of features
##         j - the feature (or group of features) to remove
##     mod_f - the full adaboost model
##     mod_s - the small adaboost model
##       std - standardized or unstandardized parameter
## RETURNS: the naive estimate
def getNaives(dat, n = 1000, p = 15, j = None, mod_f = None, mod_s = None, std = bool('False')):
    import vimpy as vi    
    import numpy as np
    
    ## split the dataset into x and y, for ease
    x = dat[:, [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14]]
    ## delete the columns in j
    x_small = np.delete(x, j, 1)
    y = dat[:, p]
    
    ## use boosted trees! stumps as weak learners
    mod_f.fit(x, y)
    ## get the predictions
    full_fit = mod_f.predict(x)
    
    ## now fit the small model
    mod_s.fit(x_small, full_fit)
    ## get fitted values
    small_fit = mod_s.predict(x_small)
    
    ## calculate naive estimator
    ### set up the vimp object
    vimp = vi.vimp_regression(y, x, full_fit, small_fit, j)
    naive = np.array([vimp.plugin().naive_])
    return naive
    
    
    


## Next want to calculate the estimator on a single (or group of) covariate(s)
## Also want to calculate standard errors!
## Function: calculateEsts
## Args: dat - the dataset
##         n - the sample size
##         p - the dimension
##         j - the column(s) to leave out
##     truth - the truth, to compare to
##      test - another dataset, for predicted r^2
## Returns: the naive and one-step estimators, confidence intervals (based on bootstrap or theory)
def calculateEsts(dat, n = 1000, p = 15, j = None, truth = None, test = None, std = bool('True')):
    ## import libraries
    import vimpy as vi
    ## numpy gives arrays etc
    import numpy as np
    ## scipy gives more helpful functions
    import scipy as sp
    ## scipy.special gives erf
    from scipy.special import erf
    ## sklearn gives learning stuff
    import sklearn as sk
    ## allow to read in SGE task id
    import os
    ## allow dataframe creation for easy read/write
    import pandas as pd

    ## sklearn gives random forests, boosted trees, cross-validation
    from sklearn.ensemble import RandomForestRegressor, AdaBoostRegressor, GradientBoostingRegressor
    from sklearn.tree import DecisionTreeRegressor
    from sklearn.model_selection import KFold, cross_val_score, GridSearchCV
    from sklearn.utils import resample

    ## get the normal distribution from scipy
    from scipy.stats import norm

    ## plotting
    import matplotlib.pyplot as plt

    ## random number generation
    import random as rand

    ## math functions
    import math
    
    ## split the dataset into x and y, for ease
    x = dat[:, [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14]]
    x_small = np.delete(x, j, 1)
    y = dat[:, p]
    
    ## use CV to get the best number of trees and learning rate
    ntrees = np.arange(100, 3500, 500)
    lr = np.arange(.01, .5, .05)
    
    param_grid = [{'n_estimators':ntrees, 'learning_rate':lr}]
    
    ## grid search CV to get argmins
    cv_full = GridSearchCV(GradientBoostingRegressor(loss = 'ls', max_depth = 1), param_grid = param_grid, cv = 5)#, n_jobs = 1)
    cv_small = GridSearchCV(GradientBoostingRegressor(loss = 'ls', max_depth = 1), param_grid = param_grid, cv = 5)#, n_jobs = 1)
    
    ## fit the full regression
    cv_full.fit(x, y)
    ntree_f = cv_full.best_params_['n_estimators']
    lr_f = cv_full.best_params_['learning_rate']
    full_mod = GradientBoostingRegressor(loss = 'ls', learning_rate = lr_f, max_depth = 1, n_estimators = ntree_f)
    full_fit = cv_full.best_estimator_.predict(x)
    
    ## fit the reduced regression
    cv_small.fit(x_small, full_fit)
    small_fit = cv_small.best_estimator_.predict(x_small)
    
    ntree_s = cv_small.best_params_['n_estimators']
    lr_s = cv_small.best_params_['learning_rate']
    small_mod = GradientBoostingRegressor(loss = 'ls', learning_rate = lr_s, max_depth = 1, n_estimators = ntree_s)
    
    ## calculate estimators
    ### set up the vimp object
    vimp = vi.vimp_regression(y, x, full_fit, small_fit, j)
    ## get the naive estimator
    vimp.plugin()
    ## get the corrected estimator
    vimp.update()
    vimp.onestep_based_estimator()
    onestep = np.array([vimp.vimp_])
    naive = np.array([vimp.naive_])
    
    ## now do the bootstrap to get naive confidence interval
    boot = bootstrap(dat, 1000)
    # boot = np.random.choice(dat, size = dat.shape, replace = True)
    naives = [getNaives(x, mod_f = full_mod, mod_s = small_mod, j = j, std = std) for x in boot]
    naives_2 = np.concatenate(naives, axis = 0)
    naive_ord = np.sort(naives_2)
    k = int(np.floor((naive_ord.size + 1)*0.025))
    naive_ci = np.array([naive_ord[k - 1], naive_ord[naive_ord.size - k - 1]])
    ## calculate standard error for one-step
    vimp.onestep_based_se()
    onestep_se = vimp.se_
    
    ## calculate CI for one-step
    vimp.get_ci(level = 0.95)
    onestep_ci = vimp.ci_
    
    ret = {'naive':np.array(naive), 'naive.ci':naive_ci, 'onestep':onestep, 'onestep.se':onestep_se,'onestep.ci':onestep_ci, 'truth':truth}
    return ret