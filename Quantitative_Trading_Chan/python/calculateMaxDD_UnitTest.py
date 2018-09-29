# -*- coding: utf-8 -*-
"""
Created on Fri Sep 21 19:48:44 2018

@author: Ernest
"""
import numpy as np
from calculateMaxDD import calculateMaxDD
cumret=np.array([10, 9, 8, 7, 11, 9, 7, 5, 5, 12])
maxDD, maxDDD, i=calculateMaxDD(cumret)

assert(maxDD==-0.5)
assert(maxDDD==4)
assert(i==7)