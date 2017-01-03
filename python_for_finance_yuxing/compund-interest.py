import numpy as np
from matplotlib.pyplot import *
from pylab import *
pv = 1000
r=0.08
n=1000
t=linspace(0,n,n)
y1=np.ones(len(t))*pv # this is a horizontal line
y2=pv*(1+r*t)
y3=pv*(1+r)**t
title('Simple vs. compounded interest rates')
xlabel('Number of years')
ylabel('Values')
xlim(0,11)
ylim(800,2200)
plot(t,y1,'b-')
plot(t,y2,'g--')
plot(t,y3,'r-')
show()
