#
#
#
#
import numpy as np

from math import exp
#from PrintSoln import *
#
#
#
# Module runge_kuta4
#
#
def integrate(F,x,y,xStop,h):

    def run_kut4(F,x,y,h):
        K0 = h*F(x,y)
        K1 = h*F(x + h/2.0, y + K0/2.0)
        K2 = h*F(x + h/2.0, y + K1/2.0)
        K3 = h*F(x + h, y + K2)
        return (K0 + 2.0*K1 + 2.0*K2 + K3)/6.0    

    X = []
    Y = []
    X.append(x)
    Y.append(y)
    while x < xStop:
        h = min(h,xStop - x)
        y = y + run_kut4(F,x,y,h)
        x = x + h
        X.append(x)
        Y.append(y)
    
    return np.array(X),np.array(Y)
#
#
#
#def F(x,y):
#    F=np.zeros((2))
#    F[0]=3.0*y[0]-4.0*exp(-x)
#    F[0]=y[1]
#    F[1]=-0.1*y[1]-x
#    return F
#
# 
#x = 0.0               # Start of Integration
#xStop = 2.0           # End of Integration
#y = np.array([0.0,1.0])  # Initial Value of {y}
#h = 0.25              # Step size
#freq = 1              # Printout frequency
#
#
#X,Y = integrate(F,x,y,xStop,h)
#PrintSoln(X,Y,freq)
#raw_input("Press Return to Exit")
#
#