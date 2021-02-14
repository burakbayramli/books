""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""

# GradeMatPlot.py: Matplotlib plot multi data sets 

import pylab as p                           # Matplotlib
from numpy import*

p.title('Grade Inflation')           # Title and labels
p.xlabel('Years in College')
p.ylabel('GPA')
xa = array([-1, 5])                  # For horizontal line
ya = array([0, 0])                                
p.plot(xa, ya)                       # Draw horizontal line                                             
x0 = array([0, 1, 2, 3, 4])            # Data set 0 points
y0 = array([-1.4, +1.1, 2.2, 3.3, 4.0])
p.plot(x0, y0, 'bo')            # Data set 0 = blue circles
p.plot(x0, y0, 'g')                     # Data set 0 = line
x1 = arange(0, 5, 1)                    # Data set 1 points 
y1 = array([4.0, 2.7, -1.8, -0.9, 2.6])              
p.plot(x1, y1, 'r')                                          
errTop = array([1.0, 0.3, 1.2, 0.4, 0.1]) # Asymm errors
errBot = array([2.0, 0.6, 2.3, 1.8, 0.4])                    
p.errorbar(x1, y1, [errBot, errTop], fmt = 'o') # Error bars
p.grid(True)                                    # Grid line
p.show()                          # Create plot on screen
