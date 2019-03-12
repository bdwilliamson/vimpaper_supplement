#!/usr/local/bin/python2.7

###########################################################################################################
##
## FILE:    simulation_hidim_twostep.py
##
## CREATED: 1 Dec 2016
##
## AUTHOR:  Brian Williamson
##
## PURPOSE: look at coverage of confidence intervals and bias for in the "high dimensional" setting,
##          using the two-step estimating procedure
## UPDATES
## DDMMYY   INIT   COMMENTS
## ------   ----   --------
############################################################################################################

## import libraries
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

## user defined functions
import variable_importance as vi
import variable_importance_se as se
import variable_importance_ci as ci
import variable_importance_ic as ic
import simulation_hidim_twostep_ests as est

## FUNCTION: applyFuncs
## ARGS: x - an n by p matrix
## RETURNS: the conditional mean
def applyFuncs(x = None):
    f1 = np.where(np.logical_and(-2 <= x[:, 0], x[:, 0] < 2), np.floor(x[:, 0]), 0) 
    f2 = np.where(x[:, 1] <= 0, 1, 0)
    f3 = np.where(x[:, 2] > 0, 1, 0)
    
    f6 = np.absolute(x[:, 5]/4) ** 3
    f7 = np.absolute(x[:, 6]/4) ** 5
    
    f11 = (7./3)*np.cos(x[:, 10]/2)
    
    ret = f1 + f2 + f3 + f6 + f7 + f11
    
    return ret

    
## FUNCTION: generateDataA
## ARGS:     n - the sample size
##           p - the number of features to include - defaults to 15
## RETURNS:  a data set from the generating mechanism specified in the document.
##           A: all independent
def generateDataA(n = 1000, p = 15):
    ## generate x
    x = np.zeros((n, p))
    for i in range(0, x.shape[1]) :
        x[:,i] = np.random.normal(0, 2, n)
    
    ## apply the functions
    mod = applyFuncs(x)
    
    ## get y
    y = mod + np.random.normal(0, 1, n)
    ## turn into a new array
    ret = np.zeros((n, p+1))
    ret[:, [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14]] = x
    ret[:, p] = y
    return ret
	

    
	
## FUNCTION: generateDataB
## ARGS:     n - the sample size
##           p - the number of features to include - defaults to 15
## RETURNS:  a data set from the generating mechanism specified in the document.
##           B: three independent groups of 5 dependent
def generateDataB(n = 1000, p = 15):
    ## generate x
    x = np.zeros((n, p))
    ## make the covariance matrices
    ## first the one with .15 off-diagonal
    sig1 = np.ones((5, 5))
    sig1 *= sig1*0.15
    np.fill_diagonal(sig1, 1)
    ## now with .5 off-diagonal
    sig2 = np.ones((5,5))
    sig2 *= sig2*0.5
    np.fill_diagonal(sig2, 1)
    ## now with 0.85 off-diagonal
    sig3 = np.ones((5,5))
    sig3 *= sig3*0.85
    np.fill_diagonal(sig3, 1)
    
    x[:, [0, 1, 2, 3, 4]] = np.random.multivariate_normal((0, 0, 0, 0, 0), sig1, n)
    x[:, [5, 6, 7, 8, 9]] = np.random.multivariate_normal((3, 3, 3, 3, 3), sig2, n)
    x[:, [10, 11, 12, 13, 14]] = np.random.multivariate_normal((-2, -2, -2, -2, -2), sig3, n)
    
    ## apply the functions
    mod = applyFuncs(x)
    
    ## get y
    y = mod + np.random.normal(0, 1, n)
    
    ## turn into a new array
    ret = np.zeros((n, p+1))
    ret[:, [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14]] = x
    ret[:, p] = y
    return ret
 
 

## FUNCTION: generateDataC
## ARGS:     n - the sample size
##           p - the number of features to include - defaults to 15
## RETURNS:  a data set from the generating mechanism specified in the document.
##           C: all dependent
## def generateDataC:
    ## do nothing for now


## Run the simulation once
## FUNCTION: doOne
## ARGS: n - the sample size
##       p - the number of features
##       j - the column(s) to leave out
## setting - the simulation setting, A-C
##      id - the index, 0 through 4
## RETURNS: the estimate and confidence interval for naive (bootstrap) and one-step (asymptotic)
def doOne(n = 1000, p = 15, j = None, setting = 'A', id = 0, std = bool('True')):
    import simulation_hidim_twostep_ests as est
    ## only works for A and B currently
    if setting[0] == 'A':
        dat = generateDataA(n, p)
        test = generateDataA(n, p)
        truth_1 = np.array([-(49.0/(9*np.exp(1))) + (49*(1.0 + np.exp(2)))/(18*np.exp(2)), (3.0/2) - (49.0/128)*(4*erf(1.0/(2*np.sqrt(2))) - 5*erf(1.0/np.sqrt(2))) - (1 - (7.0/16)*erf(1.0/np.sqrt(2))) ** 2 - (7.0/8)*erf(1.0/np.sqrt(2)), 1185.0/1024 - 1.0/(4*math.pi), -(49.0/(9*np.exp(1))) + (49*(1.0 + np.exp(2)))/(18*np.exp(2)), 2721.0/1024 + np.sqrt(2.0/math.pi) + 1.0/(4*math.pi) - (49.0/128)*(4*erf(1.0/(2*np.sqrt(2))) - 5*erf(1.0/np.sqrt(2))) - (1 + 1.0/np.sqrt(2*math.pi) - (7.0/16)*erf(1.0/np.sqrt(2)))**2 - (7.0/8)*erf(1.0/np.sqrt(2)) - (7*erf(1.0/np.sqrt(2)))/(8*np.sqrt(2*math.pi))])
        if (std):
            truth = truth_1/(np.sum(truth_1[0:3]) + 1)
            # truth = np.array([-(49.0/(9*np.exp(1))) + (49*(1.0 + np.exp(2)))/(18*np.exp(2)), (3.0/2) - (49.0/128)*(4*erf(1.0/(2*np.sqrt(2))) - 5*erf(1.0/np.sqrt(2))) - (1 - (7.0/16)*erf(1.0/np.sqrt(2))) ** 2 - (7.0/8)*erf(1.0/np.sqrt(2)), 1185.0/1024 - 1.0/(4*math.pi), -(49.0/(9*np.exp(1))) + (49*(1.0 + np.exp(2)))/(18*np.exp(2)), 2721.0/1024 + np.sqrt(2.0/math.pi) + 1.0/(4*math.pi) - (49.0/128)*(4*erf(1.0/(2*np.sqrt(2))) - 5*erf(1.0/np.sqrt(2))) - (1 + 1.0/np.sqrt(2*math.pi) - (7.0/16)*erf(1.0/np.sqrt(2)))**2 - (7.0/8)*erf(1.0/np.sqrt(2)) - (7*erf(1.0/np.sqrt(2)))/(8*np.sqrt(2*math.pi))])/(-(49.0/(9*np.exp(1))) + (49*(1.0 + np.exp(2)))/(18*np.exp(2)) + (3.0/2) - (49.0/128)*(4*erf(1.0/(2*np.sqrt(2))) - 5*erf(1.0/np.sqrt(2))) - (1 - (7.0/16)*erf(1.0/np.sqrt(2))) ** 2 - (7.0/8)*erf(1.0/np.sqrt(2)) + 1185.0/1024 - 1.0/(4*math.pi))
        else:
            truth = np.array([-(49.0/(9*np.exp(1))) + (49*(1.0 + np.exp(2)))/(18*np.exp(2)), (3.0/2) - (49.0/128)*(4*erf(1.0/(2*np.sqrt(2))) - 5*erf(1.0/np.sqrt(2))) - (1 - (7.0/16)*erf(1.0/np.sqrt(2))) ** 2 - (7.0/8)*erf(1.0/np.sqrt(2)), 1185.0/1024 - 1.0/(4*math.pi), -(49.0/(9*np.exp(1))) + (49*(1.0 + np.exp(2)))/(18*np.exp(2)), 2721.0/1024 + np.sqrt(2.0/math.pi) + 1.0/(4*math.pi) - (49.0/128)*(4*erf(1.0/(2*np.sqrt(2))) - 5*erf(1.0/np.sqrt(2))) - (1 + 1.0/np.sqrt(2*math.pi) - (7.0/16)*erf(1.0/np.sqrt(2)))**2 - (7.0/8)*erf(1.0/np.sqrt(2)) - (7*erf(1.0/np.sqrt(2)))/(8*np.sqrt(2*math.pi))])
    elif setting[0] == 'B':
        dat = generateDataB(n, p)
        test = generateDataB(n, p)
        ## update once we can get more precision on 2, 3, 5!
        truth_1 = np.array([(49*(-1 + np.exp(1.0/4))*(np.exp(1.0/4) - np.cos(2)))/(18*np.sqrt(np.exp(1))), 1.0592920571680529, 1.3920344125019724, (49*(-1 + np.exp(1.0/4))*(np.exp(1.0/4) - np.cos(2)))/(18*np.sqrt(np.exp(1))), 2.4513264696700254])
        if (std):
            truth = truth_1/(1 + np.sum(truth_1[0:3]))
            # truth = np.array([(49*(-1 + np.exp(1.0/4))*(np.exp(1.0/4) - np.cos(2)))/(18*np.sqrt(np.exp(1))), 1.0592920571680529, 1.3920344125019724, (49*(-1 + np.exp(1.0/4))*(np.exp(1.0/4) - np.cos(2)))/(18*np.sqrt(np.exp(1))), 2.4513264696700254])/((49*(-1 + np.exp(1.0/4))*(np.exp(1.0/4) - np.cos(2)))/(18*np.sqrt(np.exp(1))) + 1.0592920571680529 + 1.3920344125019724)
        else:
            truth = np.array([(49*(-1 + np.exp(1.0/4))*(np.exp(1.0/4) - np.cos(2)))/(18*np.sqrt(np.exp(1))), 1.0592920571680529, 1.3920344125019724, (49*(-1 + np.exp(1.0/4))*(np.exp(1.0/4) - np.cos(2)))/(18*np.sqrt(np.exp(1))), 2.4513264696700254])
    else:
        dat = generateDataC(n, p)
        test = generateDataC(n, p)
        if (std):
            truth = None
        else:
            ## Need to update with C5
            truth = np.array([(49*(-1 + np.exp(1.0/4))*(np.exp(1.0/4) - np.cos(2)))/(18*np.sqrt(np.exp(1))), 0.6872514457746866, 1.7602922878960034, (49*(-1 + np.exp(1.0/4))*(np.exp(1.0/4) - np.cos(2)))/(18*np.sqrt(np.exp(1))), 0])
    
    ## now calculate estimates and cis etc
    cols = ['naive', 'naive.cil', 'naive.ciu', 'onestep', 'onestep.se', 'onestep.cil', 'onestep.ciu', 'truth']
    estimates = pd.DataFrame(index = [0], columns = cols)
    estimates = estimates.fillna(0)
    estimates = estimates.astype(np.float)
    
    ## calculate the estimators
    ests = est.calculateEsts(dat, n, p, j, truth[id], test, std)
    estimates.loc[0] = [ests['naive'], ests['naive.ci'][0], ests['naive.ci'][1], ests['onestep'], ests['onestep.se'], ests['onestep.ci'][0], ests['onestep.ci'][1], ests['truth']]
    return estimates



#####################################################################################
## SET UP THE SIMULATION PARAMETERS
#####################################################################################

## job id
job_id = int(os.getenv('SLURM_ARRAY_TASK_ID')) - 1

## number of bootstrap replicates
B = 50

## sample size and dimension (2 since only running A and B for now)
# ns = np.tile([100, 100, 100, 100, 100, 300, 300, 300, 300, 300, 500, 500, 500, 500, 500, 1000, 1000, 1000, 1000, 1000], 2)
ns = [100, 300, 500, 1000]
p = 15

## array of j combinations
## note that each j is j-1 since python starts at 0
# js = np.zeros((10, 5))
js = np.zeros((5, 5))
js[0, :] = np.repeat([10], 5)
js[1, :] = np.array([0, 1, 2, 3, 4])
js[2, :] = np.array([5, 6, 7, 8, 9])
js[3, :] = np.array([10, 11, 12, 13, 14])
js[4, :] = np.array([0, 1, 2, 5, 6])
# js[5, :] = np.repeat([10], 5)
# js[6, :] = np.array([0, 1, 2, 3, 4])
# js[7, :] = np.array([5, 6, 7, 8, 9])
# js[8, :] = np.array([10, 11, 12, 13, 14])
# js[9, :] = np.array([0, 1, 2, 5, 6])

## set up indices
inds = np.array([0, 1, 2, 3, 4])

## run for setting A, B only for now
setts = ['A', 'B']
# setts = np.concatenate((np.repeat('A', 20), np.repeat('B', 20)))

## standardize or not
# std = np.concatenate((np.tile(False, 40), np.tile(True, 40)))
std = True

def get_current(idx):
    vec = range(40)
    vec2 = np.repeat(vec, 10)
    return vec2[idx]

## set up unique combinations of sett, index, n
param_lst = [(x, y, z) for x in setts for y in ns for z in inds]
## get the current settings
ind = get_current(job_id)

current = {'j':js[param_lst[ind][2], :], 'set':param_lst[ind][0], 'n':param_lst[ind][1], 'ind':param_lst[ind][2], 'std':std}

## set the seed
np.random.seed(current['n'] + ord(current['set'][0]) + job_id)

## run the simulation
## first set up output matrix
cols = ['naive', 'naive.cil', 'naive.ciu', 'onestep', 'onestep.se', 'onestep.cil', 'onestep.ciu', 'truth']
indx = range(0, B)
output = pd.DataFrame(index = indx, columns = cols)
output = output.fillna(0)
output = output.astype(np.float)
output_mat = output.as_matrix()
for i in range(0, B):
    out = doOne(n = current['n'], p = p, j = current['j'], setting = current['set'], id = current['ind'], std = current['std'])
    output_mat[i, :] = out

## save the output (create pandas dataframe, write to csv)
output.to_csv('sim_mod_output_t_' + str(job_id) + '.csv')
