""" From "A SURVEY OF COMPUTATIONAL PHYSICS", Python eBook Version
   by RH Landau, MJ Paez, and CC Bordeianu
   Copyright Princeton University Press, Princeton, 2011; Book  Copyright R Landau, 
   Oregon State Unv, MJ Paez, Univ Antioquia, C Bordeianu, Univ Bucharest, 2011.
   Support by National Science Foundation , Oregon State Univ, Microsoft Corp"""  
   
# DFT.py:  Discrete Fourier Transform
from visual import *
from visual.graph import *

# for the original signal
signgr = gdisplay(x=0,y=0,width=600,height=250,
         title='Original signal y(t)= 3 cos(wt)+2 cos(3wt)+ cos(5wt) ',\
			xtitle='x', ytitle='signal',xmax=2.*math.pi,xmin=0,ymax=7,ymin=-7)
sigfig = gcurve(color=color.yellow,display=signgr)
# For the imaginary part of the transform
imagr = gdisplay(x=0,y=250,width=600,height=250,\
								 title='Fourier transform imaginary part',xtitle='x',\
                  ytitle='Transf.Imag',xmax=10.,xmin=-1,ymax=20,ymin=-70)
impart = gvbars(delta=0.05,color=color.red,display=imagr)     # thin bars
# for the real part of the transform
# # for you to do
N = 10                                  # points for signal and transform
Np = N                                                 # global constants
signal = zeros((N+1),float)     
twopi = 2.*pi
sq2pi = 1./sqrt(twopi) 
h = twopi/N
dftimag = zeros((Np),float)              # contains im. part of transform

def f(signal):                                         # initial function
    step = twopi/N
    t= 0. 
    for  i in range(0,N+1):
       signal[i] = 3*sin(t*t*t)  
       sigfig.plot(pos=(t,signal[i]))                     # plot function
       t += step
      
def fourier(dftimag):                        # Discrete Fourier Transform
    for n in range(0,Np):                                # over frequency
      imag = 0.                                        # reset  variables
      for  k in range(0, N):                              # loop for sums
        imag += signal[k]*sin((twopi*k*n)/N) 
      dftimag[n] = -imag*sq2pi                     # imag. part transform
      if dftimag[n] !=0:                 # to plot if not too small trnsf
          impart.plot(pos=(n,dftimag[n]))                     # plot bars

f(signal) 
fourier(dftimag)
print "hola"
for i in range(0,N):
        if abs(dftimag[i])>5:
            print "i=",i , "dftimag ", dftimag[i]

