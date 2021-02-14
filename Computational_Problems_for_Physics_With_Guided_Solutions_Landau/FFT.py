""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""

# FFT.py:  FFT for complex numbers in Y[][2], returned in Y

from numpy import *  
max = 2100;   points = 1026;  N = 100; Switch = -1    # Switch= -1: Y, 1: y
y = zeros(2*(N+4), float);  Y  = zeros((N+3,2), float)
  
def fft(N,Switch):                                      # FFT of Y[n,2]
    n = 2*N
    for i in range(0,N+1):                # Original y in Y to y
         j = 2*i+1
         y[j] = Y[i,0]                       # Real Y, odd y[j]
         y[j+1] = Y[i,1]                  # Imag Y, even y[j+1]
    j = 1                               # Place y in bit reverse order
    for i in range(1,n+2, 2):
        if (i-j) < 0 :                # Reorder equivalent to bit reverse
            tempr = y[j]
            tempi = y[j+1]
            y[j] = y[i]
            y[j+1] = y[i+1]
            y[i] = tempr
            y[i+1] = tempi 
        m = n/2;
        while (m-2 > 0): 
            if  (j-m) <= 0 :  break
            j = j-m
            m = m/2
        j = j+m;
                               
    print("\n Bit-reversed y(t)")
    for i in range(1,n+1,2): print("%2d y[%2d] %9.5f "%(i,i,y[i])) 
    mmax = 2
    while (mmax-n) < 0 :                                # Begin transform
       istep = 2*mmax
       theta = 6.2831853/(1.0*Switch*mmax)
       sinth = math.sin(theta/2.0)
       wstpr = -2.0*sinth**2
       wstpi = math.sin(theta)
       wr = 1.0
       wi = 0.0
       for m in range(1,mmax+1,2):  
           for i in range(m,n+1,istep):
               j = i+mmax
               tempr = wr*y[j]   -wi *y[j+1]
               tempi = wr*y[j+1] +wi *y[j]
               y[j]   = y[i]   -tempr
               y[j+1] = y[i+1] -tempi
               y[i]   = y[i]   +tempr
               y[i+1] = y[i+1] +tempi        
           tempr = wr
           wr = wr*wstpr - wi*wstpi + wr
           wi = wi*wstpr + tempr*wstpi + wi;
       mmax = istep              
    for i in range(0,N):
        j = 2*i+1
        Y[i,0] = y[j]
        Y[i,1] = y[j+1] 

print('\n Input   \n  i   Re y(t)   Im y(t)')
h = 2*pi/N;           x = 0.
for i in range(0,N+1):                               # Generate signal in Y
    Y[i,0] = 30*cos(x) + 60*sin(2*x) + 120*sin(3*x)             # Real part
    Y[i,1] = 0.                                                   # Im part
    x += h
    print(" %2d %9.5f %9.5f" %(i,Y[i,0],Y[i,1]))
fft(N, Switch)                             # Call FFT, use global Y[][]
print '\n Fourier Transform Y(omega)'
print("  i      ReY(omega)      ImY(omega)   ")
for i in range(0,N):  
    print(" %2d  %9.5f  %9.5f "%(i,Y[i,0],Y[i,1])) 
