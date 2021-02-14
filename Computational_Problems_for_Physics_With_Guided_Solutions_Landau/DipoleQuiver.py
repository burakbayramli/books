""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""
    visualizing dipole field with streamplot & quiver (from stackflow)

import numpy as np
import matplotlib.pyplot as plt

#%% Plot the fields
X,Y = np.meshgrid( np.arange(-4,4,.2), np.arange(-4,4,.2) )
Ex = (X + 1)/((X+1)**2 + Y**2) - (X - 1)/((X-1)**2 + Y**2)
Ey = Y/((X+1)**2 + Y**2) - Y/((X-1)**2 + Y**2)

plt.figure()
plt.streamplot(X,Y,Ex,Ey)
plt.title('Streamplot')

plt.figure()
plt.quiver(X,Y,Ex,Ey,scale=50)
plt.title('Quiver')