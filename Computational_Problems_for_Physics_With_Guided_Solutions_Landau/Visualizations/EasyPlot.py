''' From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2012.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2012;
   MJ Paez, Univ Antioquia, 2007; and CC BORDEIANU, Univ Bucharest, 2012
   Support by National Science Foundation                              
'''
#EasyPlot.py Simple plot using matplotlib
from pylab import *                                   # to use matplotlib
Xmin = -5.0                                             # min value for x
Xmax = 5.0                                              # max value for x
Npoints=500
xincrement= (Xmax-Xmin)/Npoints                     # increment in x axis
x = arange(Xmin, Xmax, xincrement)       # for x in this range, increment
y = (sin(x))**2                                    # the function to plot
print "plotting"                                    # message to the user
plot(x, y, '-', lw=2)               # plot, with lines and linewidth=2pts
xlabel('x')                                  # labels for  axes and title
text(0.5,-0.1,'x axis divided in 500 intervals')
ylabel('f(x)')
title(' f(x) vs x')
grid(True)                                 # produce lines forming a grid
show()               
