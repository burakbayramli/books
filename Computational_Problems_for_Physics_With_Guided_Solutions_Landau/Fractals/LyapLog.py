""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""

# LyapLog.py:                Lyapunov coef for logistic map

from visual import *
from visual.graph import *

m_min = 3.5;        m_max = 4.5;        step = 0.25
graph1 = gdisplay( title = 'Lyapunov coef (blue) for LogisticMap (red)', 
                   xtitle = 'm', ytitle = 'x , Lyap',
                   xmax=5.0, xmin=0, ymax = 1.0, ymin =  - 0.6)
funct1 = gdots(color = color.red)
funct2 = gcurve(color = color.yellow)

for m in arange(m_min, m_max, step):                             # m loop
    y = 0.5
    suma = 0.0
    for i in range(1, 401, 1):   y = m*y*(1 - y)        # Skip transients
    for i in range(402, 601, 1):
        y = m*y*(1 - y)
        funct1.plot(pos = (m, y) )
        suma = suma  +  log(abs(m*(1. - 2.*y) ))               # Lyapunov
    funct2.plot(pos = (m, suma/401) )                         # Normalize
