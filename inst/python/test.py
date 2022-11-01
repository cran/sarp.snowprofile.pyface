#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Feb 16 02:47:50 2022

This script was used for some rudimentary tests for computation speed of p_unstable code
between native python code and python code called from within R (see `test.R`)

@author: fherla
"""

#%% initialize modules, path, and model
path = r'/home/flo/documents/sfu/code/rpackages_SARP/sarp.snowprofile.pyface/inst/python/'
path_data = r'/home/flo/documents/sfu/code/rpackages_SARP/sarp.snowprofile.pyface/inst/extdata/'

import joblib
import time
import datetime 
import sys
sys.path.append(path)
import getRF
import readProfile # function to read .pro file

feature_names = ['viscdefrate', 'rcflat', 'sphericity', 'grainsize', 'penetrationdepth','slab_rhogs']
model  = joblib.load(path + 'RF_model_pub.sav')

#%% read profile data
filename = '{}/snowprofile.pro'.format(path_data)
timestamp = datetime.datetime(2017,2,20,11,0)  #(year, month, day, hour, minute)
prof = readProfile.read_profile(filename,timestamp,remove_soil=True) 

#%% compute p_unstable
features = getRF.comp_features(prof, 0)
p_unstable = getRF.comp_rf_probability(features, model)



#%% benchmark time
start_time = time.monotonic()
p_unstable = getRF.comp_rf_probability(features, model)
print('seconds: ', time.monotonic() - start_time)
