""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""

# HOwavePacket.py: HO wave packet in motion via Visual

from numpy import *
from visual.graph import *

# Constants & Initial Values
oneoverpi = 1/math.sqrt(math.pi); a = 1.        # Constants, m=1=hbar
xx = 5; tt = 0                             # Initial x & t (classical)
wavef = display(x=0, y=0, width=600, height=600, range=8)
plotob = curve(color=color.yellow,radius=0.1)
spring = helix(pos=(-4,-3,0), radius=0.4, color=color.white,
                  coils=10.4, axis=(10,0,0))        # Classical spring
mass = box(pos=(1,-3,0), length=1, width=1, height=1, color=color.yellow)

for t in arange(0,20,0.1):                                  # Time loop 
  xx = cos(tt)
  x  =  arange(-5.0, 5.0, 0.001)      
  rate(3)
  y = oneoverpi*exp(-(x-a*math.cos(t))**2)   
  plotob.x = x                                               # x coord
  plotob.y = 4*y                                             # y coord
  spring.axis = vector(4+xx,0,0)                # Classical oscillator
  mass.pos = (xx,-3,0)                            # Position oscillator
  tt = tt+0.1
