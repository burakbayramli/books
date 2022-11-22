import matplotlib.pyplot as plt
from scipy.optimize import curve_fit
import numpy as np
def func(x,a,b):
    return a/b*(1-np.exp(-b*x)) 
xdata = np.array([0,0.0185,0.0468,0.0924,0.1795,0.2368])
ydata = np.array([0,35,90,150,210,230])
popt,pcov = curve_fit(func,xdata,ydata)
display("a value =",popt[0],"b value =",popt[1])
xfundata = np.array([i/20*0.25 for i in range(20)])
yfundata = func(xfundata,*popt)
plt.plot(xdata,ydata,'o',label='Data')
plt.plot(xfundata,yfundata,'r-',label='Best Fit: a=%5.1f, b=%5.5f'%tuple(popt))
plt.xlabel('x')
plt.ylabel('y')
plt.legend()
