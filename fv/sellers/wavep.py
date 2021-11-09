import matplotlib.pyplot as plt
import numpy as np
import sys

# Choose f i r s t order upwind or second order method

if len (sys.argv ) < 2 :
    print ("indicate ’ upwind ’ or ’ lw ’ " )
    sys.exit()
    
ax = -5
bx = 5.
tfinal = 3.
m = 100
dx = ( bx-ax ) /(m+1)
nsteps = 30
dt = tfinal / nsteps
cour = dt/dx
# Dam −break problem
hl = 3
hr = 1
ul = 0
ur = 0
g = 1

Q = np.zeros((2,m) )

Qnp1 = np.zeros(nsteps).tolist()
smax = np.zeros(nsteps)

# S e t ICs

Q[0,:int(m/2)] = hl
Q[0,int(m/2):] = hr
# Define f l u x f u n c t i o n
def flux (U) :
    q1 = U[ 0 ]
    q2 = U[ 1 ]
    f = np.zeros(2)
    f [ 0 ] = q2
    f [ 1 ] = np.power(q2,2)/q1 + g*np.power( q1 , 2 ) /2
    return f

# Time s t e p p i n g loop
for n in range ( nsteps ) :
    flux_arr=np.zeros ( ( 2 ,m) )
    Fcorr = np.zeros ( ( 2 ,m) )

for n in range (nsteps):
    flux_arr=np.zeros((2 ,m) )
    Fcorr = np.zeros ( ( 2 ,m) )    

    for i in range(1,m-1):
        him1 =Q[ 0 , i -1]
        hi=Q[ 0 , i ]
        hip1 =Q[ 0 , i +1]
        uim1 = Q[ 1 , i -1]/him1
        ui=Q[ 1 , i ]/ hi
        uip1 =Q[ 1 , i +1]/ hip1    






