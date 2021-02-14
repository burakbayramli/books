""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""

#  FFT.py  FFT for complex numbers dtr[][2], returned in dtr  

from visual import *              
from visual.graph import *

N = 1024                     # 2**10 
signalgr = gdisplay(x=0,y=0,width=500,height=250, title='Gaussian signal',\
            xtitle='x', ytitle='y',xmax=25.0,xmin=-25.0,ymax=1,ymin=0)
sigpl   = gcurve(color=color.yellow,display=signalgr) 
transgr = gdisplay(x=0,y=250,width=500,height=250, title='FFT',\
            xtitle='w', ytitle='W',xmax=1,xmin=-1.0,ymax=300,ymin=-300)
trpl    = gvbars(delta=0.01,color=color.cyan,display=transgr)
tipl    = gvbars(delta=0.01,color=color.magenta,display=transgr)   # Im
invgr   = gdisplay(x=0,y=500,width=500,height=250, title='inverse TF',\
           xtitle='x',ytitle='y',xmax=25.0,xmin=-25.0,ymax=1,ymin=0)   
invpl   = gcurve(color=color.yellow,display=invgr)
max      = 2100                 
points   = 1026                                         
data     = zeros((max),float) 
dtr      = zeros((points,2),float)
sigma    = 3.0                                        # Width  
foursig2 = 4.0*sigma*sigma;
def gaussian():
    psr = zeros((N+1),float)
    psi = zeros((N+1),float)
    len = 50.0                                      # -25 <= x <= 25
    # m = N;
    a = 0.0                                         # Initial position
    dxx = 50.0/N                                    # x increment
    xx = -0.5*len                                      # left end
    for n in range(0,N):                              # Space loop
        xma2 = (xx-a)*(xx-a)/foursig2           
        psr[n] = exp(-xma2)               
        psi[n] = 0.0                                                                  
        dtr[n,0] = psr[n];                          # data
        dtr[n,1] = psi[n];                        
        sigpl.plot(pos=(xx, psr[n])) 
        xx += dxx;  
def fft(nn,isign):                                      # FFT of dtr[n,2]
    n = 2*nn
    for i in range(0,nn+1):                 
         j = 2*i+1
         data[j] = dtr[i,0]                       # Real dtr, odd data[j]
         data[j+1] = dtr[i,1]                  # Imag dtr, even data[j+1]
    j = 1                                     # Data in bit reverse order
    for i in range(1,n+2,2 ):
        if (i-j) < 0 :                # Reorder equivalent to bit reverse
            tempr     = data[j]
            tempi     = data[j+1]
            data[j]   = data[i]
            data[j+1] = data[i+1]
            data[i]   = tempr
            data[i+1] = tempi 
        m = n/2;
        while (m-2 > 0): 
            if  (j-m) <= 0 :
                break
            j = j-m
            m = m/2
        j = j+m;
        mmax = 2
    while (mmax-n)<0 :                                  # Begin transform
       istep = 2*mmax
       theta = 6.2831853/(1.0*isign*mmax)
       sinth = sin(theta/2.0)
       wstpr = -2.0*sinth**2
       wstpi = sin(theta)
       wr = 1.0
       wi = 0.0
       for m in range(1,mmax +1,2):  
           for i in range(m,n+1,istep):
               j = i+mmax
               tempr = wr*data[j]-wi*data[j+1]
               tempi = wr*data[j+1]+wi*data[j]
               data[j]   = data[i]-tempr
               data[j+1] = data[i+1]-tempi
               data[i]   = data[i]+tempr
               data[i+1] = data[i+1]+tempi        
           tempr = wr
           wr = wr*wstpr-wi*wstpi+wr
           wi = wi*wstpr+tempr*wstpi+wi;
       mmax = istep              
    for i in range(0,nn):
        j = 2*i+1
        dtr[i,0] = data[j]
        dtr[i,1] = data[j+1] 
nn = 1024                                                    # Power of 2                                      
isign = -1                           # -1 transform, +1 inverse transform
gaussian()                                               # call function
fft(nn, isign)                             # Call FFT, use global dtr[][]
L = 50.                                                # x from -25 to 25
dk = 2.*pi/L                  # to transform k into -pi*N/L =< k=< pi*N/L
for n in range (0,N):
    if n < N/2:              
        k = n*dk;                              # positve k's (first half)
    else:
        k = (n-N)*dk;                                      # negative k's
    trpl.plot(pos=(k,dtr[n,0]),display=transgr)
    if dtr[n,1] > 0.1 :
        print(n, dtr[n,1])                     # dtr[n,1]= Im part is zero
    tipl.plot(pos=(k,dtr[n,1]),display=transgr)                 # no plot
print ("Compute inverse FFT")                # its bars should be magenta
isign = 1
fft(nn,isign)
dxx = 50.0/N                                             # increment in x
xx  = -0.5*L 
for n in range (0,N):                  # divide inverse by N to normalize
    invpl.plot(pos=(xx,dtr[n,0]/N),display=invgr) 
    xx += dxx
print ("Done")
