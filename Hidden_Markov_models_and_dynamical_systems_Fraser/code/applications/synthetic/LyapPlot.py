""" LyapPlot.py

Call with: python LyapPlot.py data_file

A script for makeing Lyapunov exponent plots for fig:benettin

"""
Copyright = '''
Copyright 2005, 2007, 2008 Andrew M. Fraser, and 2013 Andrew M. Fraser
and Los Alamos National Laboroatory

This file is part of hmmds3.

Hmmds3 is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

Hmmds3 is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

See the file gpl.txt in the root directory of the hmmds3 distribution
or see <http://www.gnu.org/licenses/>.
'''

import  sys, numpy, math, random, numpy.linalg as LA
assert len(sys.argv) == 2,"Should be invoked like: %s data_file"%sys.argv[0]
data_file = sys.argv[1]
import lorenz  # If not found, try env PYTHONPATH=some_dir python ...

RG = random.gauss
RR = random.random
random.seed(7)
# Parameters of the Lorenz system
s = 10.0
r = 28.0
b = 8.0/3
# Other parameters
DevEta = 1e-5 # Std dev of state noise
DIC = 1.0     # Size of cube of initial condidtions
Dqy = 1e-3    # Y Quantization size
# The smaller values of Nt and Nr require 31.8 seconds on cathcart
#Nt = 400     # Number of times
#Nr = 100     # Number of runs
# The larger values of Nt and Nr require 17:57 on cathcart
Nt = 1000     # Number of times
Nr = 1000     # Number of runs
ts = 0.15     # Time step
tr = 10.0     # Relax time

Rt = numpy.empty((Nt,Nr,3))
RAt = numpy.empty((Nt,Nr,3))
ic = [0.0, 0.1, 20.0]
result = lorenz.Lsteps(ic, lorenz.F, s,b,r,tr,2)
Aug = DevEta/Dqy                   # Augmentation of expansion by noise
for j in range(Nr):               # Loop over runs
    # Random initial condition in DIC cube
    ic = result[-1] + numpy.array([RR()*DIC,RR()*DIC,RR()*DIC])
    Q = numpy.mat(numpy.eye(3))  # Basis in tangent space
    for t in range(Nt):         # Loop over time steps
        ic,Tan = lorenz.Ltan_one(ic,s,b,r,ts)
        # Add state noise at each time step
        ic = ic + numpy.array([RG(0.0,DevEta),RG(0.0,DevEta),RG(0.0,DevEta)])
        TQ = numpy.mat(Tan)*Q
        Q,R = LA.qr(TQ) # QR decomposition
        for i in range(3):
            rii = R[i,i]
            if rii > 0:
                Rt[t,j,i] = math.log(rii)
                RAt[t,j,i] = math.log(rii+Aug)
            else:
                if rii < 0:
                    Rt[t,j,i] = math.log(-rii)
                    RAt[t,j,i] = math.log(Aug-rii)
                else:
                    print('at t=',t, 'i=', i, 'R[ii]=', ri)
                    raise SystemExit

sumF = Rt.cumsum(0)
sumFA = RAt.cumsum(0)
bottom = int(math.floor(0.05*Nr))# Index of 5% level for plots
top = int(math.ceil(0.95*Nr))    # Index of 95% level for plots Now
# write a file with the following 11 columns (LE is the estimate of
# the largest Lyapunov exponent and LEA is the largest augmented
# exponent):

# T+1, LE[run=0:3], LE_5%, LE_95% LEA[run=0:3], LEA_5%, LEA_95%
# 1    2 3 4        5      6      7 8 9         10      11
f = open(data_file,'w') # Open file for plot data
LE = numpy.zeros(Nr)  # Vector of Lyapunov exponents
LEA = numpy.zeros(Nr) # Vector of augmented exponents
for T in range(Nt):
    Tts = ts*(T+1)
    for j in range(Nr):
        LE[j] = sumF[T,j,0]/Tts
        LEA[j] = sumFA[T,j,0]/Tts
    print(T+1, LE[0], LE[1], LE[2], end=' ', file=f)
    LE.sort()
    print(LE[bottom], LE[top], end=' ', file=f)
    print(LEA[0], LEA[1], LEA[2], end=' ', file=f)
    LEA.sort()
    print(LEA[bottom], LEA[top], file=f)
f.close()

# Local Variables:
# mode: python
# End:
