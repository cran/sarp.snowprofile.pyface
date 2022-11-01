# -*- coding: utf-8 -*-
"""
Created on Feb 15 2022

@author: mayers, reassembled and slightly modified by fherla
"""

import numpy as np
import pandas as pd
# import time

###########################################################################################################
#####
#####   Name:       comp_rf_probability
#####
#####   Purpose:     given data frame of features and path to RF model, calculate RF probability for each layer
#####               
#####   Remarks:  
#####
###########################################################################################################
    
def comp_rf_probability(features, model):
    
    # make sure features are in right order for model
    features = features[['viscdefrate', 'rcflat', 'sphericity', 'grainsize', 'penetrationdepth','slab_rhogs']]
    P_unstable = np.repeat(np.nan,len(features))    
    
    ## compute p_unstable for all rows except the ones that contain any NA values:
    # start_time = time.monotonic()
    P_unstable[features.notnull().all(axis = 1)] = model.predict_proba(features[features.notnull().all(axis = 1)])[:,0]
    # print('seconds: ', time.monotonic() - start_time)


    return P_unstable


###########################################################################################################
#####
#####   Name:       comp_features
#####
#####   Purpose:     given SNOWPACK profile in readProfile format, calculate all necessary features for every single layer (from bottom to top of profile)
#####               
#####   Remarks:  don't change order of features in output (the RF model needs exactly this order)
#####
###########################################################################################################
    
def comp_features(prof, slopeangle):
    
    strength_s = prof['shear_strength']  
    gs = prof['grain_size']
    rho = prof['density']
    layer_top = prof['height']
    nlayers = len(rho)
    
    # 1. viscous deformation rate
    viscdefrate = prof['viscous_deformation_rate']
    
    # 2. critical crack length
    rcflat = rc_flat(layer_top, rho, gs, strength_s)
    
    # 3. sphericity
    sphericity = prof['sphericity']
    
    # 4. grainsize
    grainsize = gs
    
    # 5. penetration depth (calculation as in SP)
    pen_depth_new = comp_pendepth(prof,slopeangle)
    pen_depth_rep = np.repeat(pen_depth_new, nlayers)
    
    # 6. mean slab density divided by mean slab grain size
    thick = np.diff(np.concatenate((np.array([0]), layer_top)))
    slab_rhogs= np.array([np.sum( rho[i+1:]* thick[i+1:]/gs[i+1:])/np.sum(thick[i+1:]) if i<len(rho)-1 else np.nan for i in range(len(rho))] )
    
    # put all features together into one dataframe. 
    d = {'viscdefrate': viscdefrate,
         'rcflat': rcflat,
         'sphericity': sphericity,
         'grainsize': grainsize,
         'penetrationdepth': pen_depth_rep,
         'slab_rhogs': slab_rhogs}  #
    
    features = pd.DataFrame(data = d)
    
    return features 



###########################################################################################################
#####
#####   Name:       comp_pendepth
#####
#####   Purpose:     given SNOWPACK profile in readProfile format, calculate skier penetration depth
#####               
#####   Remarks: might slightly differ from SNOWPACK source code, but is correct with regard to publications Jamieson Johnston (1998)
#####               and Bellaire (2006)
###########################################################################################################
def comp_pendepth(prof, slopeangle):
    top_crust = 0
    thick_crust = 0
    rho_Pk = 0
    dz_Pk = 1.e-12
    crust = False
    e_crust = -999
    
    layer_top = prof['height']
    ee = len(layer_top)-1
    thick = np.diff(np.concatenate((np.array([0]), layer_top)))
    rho = prof['density']
    HS = layer_top[-1]
    graintype = prof['grain_type']  
    min_thick_crust = 3 #cm
    
    while (ee >= 0) & ((HS-layer_top[ee])<30):
        
        rho_Pk = rho_Pk + rho[ee]*thick[ee]
        dz_Pk = dz_Pk + thick[ee]
        
        if crust == False:
        ##Test for strong mf-crusts MFcr.
        ## Look for the first (from top) with thickness perp to slope > 3cm
            if (graintype[ee] == 772) & (rho[ee] >500.): ## very high density threshold, but implemented as this in SP
                if e_crust == -999:
                   e_crust = ee
                   top_crust = layer_top[ee]
                   thick_crust = thick_crust + thick[ee]
                elif (e_crust - ee) <2:
                   thick_crust = thick_crust + thick[ee]
                   e_crust = ee
            elif e_crust > 0:
               if thick_crust*np.cos(np.deg2rad(slopeangle)) > min_thick_crust:
                   crust = True
               else:
                   e_crust = -999
                   top_crust = 0
                   thick_crust = 0

        ee = ee-1
                         

    
    rho_Pk = rho_Pk/dz_Pk        #average density of the upper 30 cm slab
    return np.min([0.8*43.3/rho_Pk, (HS-top_crust)]) #NOTE  Pre-factor 0.8 introduced May 2006 by S. Bellaire , Pk = 34.6/rho_30
#original regression by Jamieson Johnston (1998)
     
     
     ###########################################################################################################
#####
#####   Name:       rc_flat
#####
#####   Purpose:     calculate critical crack length [m] (Richter 2019) for the flat field 
#####               given profile with arrays of layer tops, density rho, grain size gs, shear strength strength_s [kPa]   
#####
#####   Remarks:  rc is calculated for the flat, even if input profile is given at slope angle. 
#####             the formula is slope angle independent since SNOWPACK output refers to "gravitational layer height". 
#####               (--> normal stress in the flat is calculated exactly with this gravitational height)
#####               if formula was to be used for slopes, some cos, sin are needed!)
###########################################################################################################    
    
def rc_flat(layer_top, rho, gs, strength_s, rho_sl = np.nan):
    rho_ice = 917. #kg m-3
    gs_0 = 0.00125 #m
    rho_wl = np.array(rho)
    rho_sl = np.array(rho_sl)
    gs_wl = np.array(gs)*0.001 #[m]
    tau_p = np.array(strength_s)*1000. #[Pa]
    
    ## only compute rho_sl if not provided as input (this allows to precompute rho_sl and then stack profiles)
    if np.all(np.isnan(rho_sl)):
        thick = np.diff(np.concatenate((np.array([0]), layer_top)))
        rho_sl = np.array([ np.sum( rho[i+1:]* thick[i+1:])/np.sum(thick[i+1:]) if i<len(rho)-1 else np.nan for i in range(len(rho)) ] )
    ## suppress warnings if nan in divide (otherwise printed to R)
    np.seterr(divide='ignore', invalid='ignore')
    eprime = 5.07e9*(rho_sl/rho_ice)**5.13 / (1-0.2**2) #Eprime = E' = E/(1-nu**2) ; poisson ratio nu=0.2
    dsl_over_sigman = 1. / (9.81 * rho_sl) #D_sl/sigma_n = D_sl / (rho_sl*9.81*D_sl) = 1/(9.81*rho_sl)
    a = 4.6e-9
    b = -2.
    rc_flat = np.sqrt(a*( rho_wl/rho_ice * gs_wl/gs_0 )**b)*np.sqrt(2*tau_p*eprime*dsl_over_sigman)
    return rc_flat

     
