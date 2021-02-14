""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""
	 
# Bugs.py The Logistic map

from visual.graph import *
mu_min = 1.0; mu_max = 4.0; step = 0.01  #
lastx = int(1000 * 0.5)                # To avoid repeat x values
count = 0                              # Plot every 2 iterations

graph1 = gdisplay(width=600,height=400,title='Logistic Map',xtitle='mu',ytitle='x*',xmax=4.0,xmin=1.,ymax=1.,ymin=0.)
pts = gdots(shape = 'round', size = 1.5, color = color.green)

for mu in arange(mu_min, mu_max, step):
    x = 0.5                            # Start ea loop at x=0.5 
    for i in range(1,201,1):           # Avoid transients
        x = mu*x*(1 - x)   
    for i in range(201, 402, 1):       # Now can plot
        x = mu*x*(1 - x)   
        intx = int(1000 * x)           # Truncated x
        if  intx != lastx and count%2 == 0: # Avoid repeats
            pts.plot(pos=(mu,x))         # New x & even count
        lastx = intx
        count   += 1
