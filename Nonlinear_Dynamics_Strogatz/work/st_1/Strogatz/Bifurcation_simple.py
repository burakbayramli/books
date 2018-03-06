import numpy as np
import scipy as sp
import pylab as p

xa=.2
xb=1.99

C=np.linspace(xa,xb,100)
print( C)
i=1000
Y = np.ones(len(C))

# for x in range(iter):
#       Y = Y*C*(1-Y)#Y**2 - C   #get rid of early transients
fig, ax = p.subplots(figsize=(8,6)) 

for x in range(i): 
    Y = Y**2 - C
    ax.plot(C,Y, '.', color = 'g', markersize = .5)
ax.set_axis_off()
p.show()

#Explain range(iter)
#=================
# L = np.ones(5)
# lst = np.array([1,2,3,4,5])
# for i in range(iter):
#     L = L+lst
#     print('i',i,'L',L)