""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""

# DFTcomplex.py:  Discrete Fourier Transform wi Python complex math 

from visual import *;  from visual.graph import *
import cmath                                        # Complex math

N = 100;  twopi = 2.*pi;  h = twopi/N;  sq2pi = 1./sqrt(twopi)                        
y = zeros(N+1, float); Ycomplex = zeros(N, complex)       # Arrays

SignalGraph = gdisplay(x=0, y=0, width=600, height=250, title ='Signal y(t)',
    xtitle='x',ytitle='y(t)', xmax=2.*math.pi, xmin=0, ymax=30, ymin=-30)
SignalCurve = gcurve(color=color.yellow, display=SignalGraph)
TransformGraph = gdisplay(x=0,y=250,width=600,height=250,title ='Im Y(omega)',
       xtitle = 'x',ytitle='Im Y(omega)',xmax=10.,xmin=-1,ymax=100,ymin=-250)
TransformCurve = gvbars(delta = 0.05, color = color.red,
	display = TransformGraph) 

def Signal(y):                                        # Signal
    h = twopi/N;        x = 0. 
    for i in range(0, N+1):
       y[i] = 30*cos(x) + 60*sin(2*x) + 120*sin(3*x)  
       SignalCurve.plot(pos = (x, y[i]))                # Plot
       x += h
      
def DFT(Ycomplex):                                      # DFT
    for n in range(0, N):              
      zsum = complex(0.0, 0.0)                
      for  k in range(0, N):                              
          zexpo = complex(0, twopi*k*n/N)   # Complex exponent
          zsum += y[k]*exp(-zexpo)           
      Ycomplex[n] = zsum * sq2pi                                      
      if Ycomplex[n].imag != 0: 
      	      TransformCurve.plot(pos=(n,Ycomplex[n].imag)) 

Signal(y)                             # Generate signal 
DFT(Ycomplex)                         # Transform signal 
