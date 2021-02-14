""" From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
   Support by National Science Foundation
"""
# Grade.py: Sample plotting of grades.  

import pylab as p                             # to use matplotlib library
from visual import *

p.figure()
p.title('Grade Inflation')
p.xlabel('Years in College')
p.ylabel('GPA')
xa = array([-1,5])                         # draw central horizontal line
ya = array([0,0])
p.plot(xa,ya)
#Data set 0
t = arange(0,5,1)                                     # range of x values
x0 = array([0,1,2,3,4])
y0 = array([-5.4,-4.1,-3.2,-2.3,-2.0])
#p.xlim(-1,5)
p.plot(x0,y0,'bo')
y1 = array([-3.6, -2.7, -1.8, -0.9, 0.6])                    # Data set 1
err1inf = array([0.4, 0.3, 0.6, 0.4, 0.4])
err1sup = array([0.4, 0.2, 0.3, 0.4, 0.5])        # asymmetric error bars
p.plot(x0,y1,'rx')
p.plot(t,y1,'r')
p.errorbar(t,y1,[err1inf,err1sup],fmt = 'o')
p.grid(True)
p.show()
