""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""

# Entropy.py Shannon Entropy with Logistic map using Tkinter

try:
    from tkinter import *
except:
    from Tkinter import *
import math
from numpy import zeros, arange

global Xwidth, Yheight
root = Tk(  );   root.title('Entropy versus mu ')
mumin = 3.5;  mumax = 4.0;  dmu = 0.25; nbin = 1000;  nmax = 100000
prob = zeros( (1000), float)                
minx=mumin;  maxx=mumax;  miny=0; maxy=2.5;  Xwidth=500; Yheight=500 

c = Canvas(root, width = Xwidth, height = Yheight)    # initialize canvas
c.pack()                                                    # pack canvas

Button(root, text = 'Quit', command = root.quit).pack()   # to begin quit

def world2sc(xl, yt, xr, yb):  # x - left, y - top, x - right, y - bottom
    maxx = Xwidth   # canvas width          _________________________
    maxy = Yheight  # canvas height         |      |    |tm         |
    lm = 0.10*maxx  # left margin           |   ___|____|_______ ___|
    rm = 0.90*maxx  # right margin          |lm |  |            |   |
    bm = 0.85*maxy  # bottom margin         |___|  |            |   |
    tm = 0.10*maxy  # top margin            |__ |__|____________|   |
    mx = (lm - rm)/(xl - xr) #              |   |  bm         rm|   |
    bx = (xl*rm - xr*lm)/(xl - xr) #        |   |__|____________|   |
    my = (tm - bm)/(yt - yb)       #        |                       |
    by = (yb*tm - yt*bm)/(yb - yt) #        |_______________________|
    linearTr = [mx, bx, my, by]                            # (maxx, maxy)
    return linearTr                      # returns a list with 4 elements

# Plot y, x, axes; world coord converted to canvas coordinates
def xyaxis(mx, bx, my, by):            # to be called after call workd2sc
    x1 = (int)(mx*minx + bx)             # minima and maxima converted to
    x2 = (int)(mx*maxx + bx)                         # canvas coordinades
    y1  = (int)(my*maxy + by)
    y2 = (int)(my*miny + by) 
    yc  = (int)(my*0.0 + by)
    c.create_line(x1, yc, x2, yc, fill = "red")            # x axis
    c.create_line(x1, y1, x1, y2, fill = 'red')          # y - axis
    for i in range (7):                                  # x tics
       x = minx + (i - 1)*0.1                         # world coordinates
       x1 = (int)(mx*x + bx)                    # canvas coord
       x2 = (int)(mx*minx + bx)
       y = miny + i*0.5                                # real coordinates
       y2 = (int)(my*y + by)                   # canvas coords
       c.create_line(x1, yc - 4, x1, yc + 4, fill = 'red')       # tics x
       c.create_line(x2 - 4, y2, x2 + 4, y2, fill = 'red')       # tics y
       c.create_text(x1 + 10, yc + 10, text = '%5.2f'% (x),\
		     fill = 'red', anchor = E)                   # x axis
       c.create_text(x2 + 30, y2, text = '%5.2f'% (y), fill = 'red',\
		    anchor = E)   # y axis
    c.create_text(70, 30, text = 'Entropy', fill = 'red', anchor = E)  
    c.create_text(420, yc - 10, text = 'mu', fill = 'red', anchor = E) 
    
mx, bx, my, by = world2sc(minx, maxy, maxx, miny)          # returns list
xyaxis(mx, bx, my, by)                                      # axes values
mu0 = mumin*mx + bx                                    
entr0 = my*0.0 + by                               
for mu in arange(mumin, mumax, dmu):                            # mu loop
    print(mu)
    for j in range(1, nbin):
        prob[j] = 0
    y  = 0.5
    for n in range(1, nmax + 1):
        y = mu*y*(1.0 - y)                # Logistic map, Skip transients
        if (n > 30000):
            ibin = int(y*nbin)  +  1
            prob[ibin]  +=  1
    entropy = 0.
    for ibin in range(1, nbin):
        if (prob[ibin]>0):
            entropy = entropy - (prob[ibin]/nmax)*math.log10(prob[ibin]/nmax)
    entrpc = my*entropy + by                   # entropy to canvas coords
    muc = mx*mu + bx                                # mu to canvas coords
    c.create_line(mu0, entr0, muc, entrpc, width = 1, fill = 'blue') 
    mu0 = muc                                # begin values for next line
    entr0 = entrpc
root.mainloop()                                  # makes effective events
