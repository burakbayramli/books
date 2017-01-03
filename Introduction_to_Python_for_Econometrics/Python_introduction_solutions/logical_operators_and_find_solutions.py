"""Solutions for 'Logical Operators and Find' chapter.  

Solutions file used IPython demo mode.  To play, run

from IPython.lib.demo import Demo
demo = Demo('logical_operators_and_find.py')
 
and then call 

demo()

to play through the code in steps.
"""
# <demo> auto
from __future__ import print_function
import numpy as np
from numpy import mean, std
# <demo> --- stop ---
# Exercise 1
data = np.load('exercise3_compressed.npz')
dates = data['dates']
SP500 = data['SP500']
XOM  = data['XOM']

print("sum(SP500<0):")
print(sum(SP500<0))
print("sum(XOM<0):")
print(sum(XOM<0))
# <demo> --- stop ---
# Exercise 2
SP500big = SP500>(2*SP500.std())
SP500small = SP500<(-2*SP500.std())
print("mean(SP500[SP500big]):")
print(mean(SP500[SP500big]))
print("mean(SP500[SP500small]):")
print(mean(SP500[SP500small]))

XOMbig = XOM>(2*std(XOM))
XOMsmall = XOM<(-2*std(XOM))
print("mean(XOM[XOMbig]):")
print(mean(XOM[XOMbig]))
print("mean(XOM[XOMsmall]):")
print(mean(XOM[XOMsmall]))
# <demo> --- stop ---
# Exercise 3
bothNeg = np.logical_and(SP500<0,XOM<0)
data = np.vstack((SP500,XOM)).T
corr = np.corrcoef(data.T)
negCorr = np.corrcoef(data[bothNeg,:].T)

print("corr:")
print(corr)
print("negCorr:")
print(negCorr)
# <demo> --- stop ---
# Exercise 4
oneNeg = np.logical_or(SP500<0,XOM<0)
oneNegCorr = np.corrcoef(data[oneNeg,:].T)
print("oneNegCorr:")
print(oneNegCorr)


# <demo> --- stop ---
# Exercise 5
def myany(x):
    """Returns True if any value in the input is True
    """
    return not np.all(np.logical_not(x))
    
def myall(x):
    """Returns True if all values in the input is True
    """
    return not np.any(np.logical_not(x))    
    