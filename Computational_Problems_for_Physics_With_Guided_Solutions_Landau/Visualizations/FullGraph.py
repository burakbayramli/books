""" From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
   Support by National Science Foundation """                             
   
# FullGraph.py:       2 - D graph with title and labels
from visual import*	 
from visual.graph import *

graph1 = gdisplay(x = 0, y = 0, width = 600, height = 450, 
          title ='f(x) vs. x', xtitle='x label', ytitle='y = f(x)', 
          xmax = 5., xmin = - 5., ymax = 1.2, ymin = - 1.2, 
          foreground = color.black, background = color.white)

funct1 = gcurve(color = color.cyan)

for x in arange( - 5.,  + 5, 0.1):
    funct1.plot(pos = (x, cos(x) )) 
