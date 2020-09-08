#!/usr/bin/env python

# import useful modules
import matplotlib
import numpy as np
#from pylab import *
#from scipy.integrate import ode

class charge:
    def __init__(self, q, pos):
        self.q=q
        self.pos=pos

def E_point_charge(q, a, x, y):
    return q*(x-a[0])/((x-a[0])**2+(y-a[1])**2)**(1.5), \
        q*(y-a[1])/((x-a[0])**2+(y-a[1])**2)**(1.5)

def E_total(x, y, charges):
    Ex, Ey=0, 0
    for C in charges:
        E=E_point_charge(C.q, C.pos, x, y)
        Ex=Ex+E[0]
        Ey=Ey+E[1]
    return Ex, Ey
