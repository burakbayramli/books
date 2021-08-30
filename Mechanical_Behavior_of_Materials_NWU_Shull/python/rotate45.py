#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import numpy as np

sig=np.zeros((3, 3))  #% create stress tensor and set to zero
sig[0, 0] = 5e6; # this is the only nonzero component

sigp=np.zeros((3, 3)) # initalize rotated streses to zero

phi = 45

theta = [[phi,90-phi,90], [90+phi,phi,90], [90,90,0]]
theta = np.deg2rad(theta)  # trig functions need angles in radians
for i in [0, 1, 2]:
    for j in [0, 1, 2]:
        for k in [0, 1, 2]:
            for l in [0, 1, 2]:
                sigp[i,j]=sigp[i,j]+np.cos(theta[i,k])*np.cos(theta[j,l])*sig[k,l]

print(sigp)  # display the transformed tensor components
