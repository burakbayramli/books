""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""
	 
# Bugs.py The Logistic map

from visual.graph import *
m_min = 1.0;      m_max = 4.0;       step = 0.01
graph1 = gdisplay(width=600, height=400, title='Logistic Map',
   xtitle='m', ytitle='x', xmax=4.0, xmin=1., ymax=1., ymin=0.)
pts = gdots(shape = 'round', size=1.5, color = color.green)
lasty = int(1000 * 0.5)         # Eliminates some points
count = 0                       # Plot every 2 iterations
for m in arange(m_min, m_max, step):
    y = 0.5
    for i in range(1,201,1):         # Avoid transients
        y = m*y*(1-y)   
    for i in range(201,402,1):
        y = m*y*( 1 - y) 
    for i in range(201, 402, 1):    # Avoid transients
        oldy=int(1000*y)
        y = m*y*(1 - y)   
        inty = int(1000 * y)
        if  inty != lasty and count%2 == 0:
            pts.plot(pos=(m,y))       # Avoid repeats
        lasty = inty
        count   += 1
