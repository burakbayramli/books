""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""

# NoiseSyncFilter2.py: Noise removal wi Sinc Filter

from numpy import *;  import random

N = 512;  imax = 512;  maxnoise = 20 

fg = zeros((N+1,N+1),float)
fnoi = zeros((N+1,N+1),float)
y = zeros((N+1,N+1),float)
  
Input = open('Mariana.dat','r')  # Input image
Marnoi = open('MarianaNoise.pgm','w+t')
Out = open('Mariana.pgm','w+t')
Clean = open('MarianaCleand.pgm','w+t')

Marnoi.write("P2\n")            # Netpbm internal image code
Marnoi.write("512  512\n")      # Pixel dimensions of image
Marnoi.write("255\n")           # Byte scale, 255=white.
Out.write("P2\n");    Out.write("512  512\n");    Out.write("255\n")         
Clean.write("P2\n");  Clean.write("512  512\n");  Clean.write("255\n")            

def filter():                    # Low-pass windowed-sinc filter
    h = zeros((imax),float)
    m = 100                              # Filter length (101 points)
    fc = 0.07   
    for i in range(0,100):           # Low-pass filter kernel
        if ((i-(m//2)) == 0):  h[i] = 2*math.pi*fc 
        if ((i-(m//2))!= 0):   h[i] = sin(2*math.pi*fc*(i-m/2))/(i-m/2)
        h[i] = h[i]*(0.54 - 0.46*cos(2*math.pi*i/m))  # Hamming window   
    sum = 0.                             # Normalize low-pass filter kernel
    for i in range(0,100):  sum = sum + h[i]
    for i in range(0,100):  h[i] = h[i]/sum
    for j in range(0,imax):
      if j%100==0:  print('Waiting till reach 500, now at: ',j)
      for k in range(0,imax):  # Convolute input with filter
        y[k,j] = 0                   
        for i in range(0,100):
            if k > 99: y[k,j] = y[k,j] + fnoi[k-i,j]*h[i] 
        Clean.write("%4d"%(int(y[k,j]))) # Partially filtered
      Clean.write("\n")

for j in range(0,N):
    for i in range(0,N):
        fg[i,j] = int(Input.readline())  
        fnoi[i,j] = fg[i,j] + maxnoise*(2*random.random()-1) # Signal + noise
for j in range(0,N):        # Output in rows
        for i in range(0,N):
            Marnoi.write("%4d"%(int(fnoi[i,j])))
            Out.write("%4d"%(int(fg[i,j])))
        Marnoi.write("\n")
        Out.write("\n")           
Marnoi.closed;  Out.closed;  Input.closed;  Clean.closed
filter()   
print('MarianaCleaned written to disk\n All Done!')
