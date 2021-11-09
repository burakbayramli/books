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


        # Roe−averaged q u a n t i t i e s a t l e f t c e l l boundary
        h_avg_m1 = ( him1 + hi ) /2
        u_avg_m1 = ( np.sqrt(him1 ) * uim1 + np.sqrt(hi)*ui ) /(np.sqrt( him1 ) +
                                                               np.sqrt( hi ) )
        c_m1 = np.sqrt(g*h_avg_m1 )
        lambda1_m1 = u_avg_m1 - c_m1
        lambda2_m1 = u_avg_m1 + c_m1


        # Roe−averaged q u a n t i t i e s a t r i g h t c e l l boundary
        h_avg_p1 = ( hip1+ hi ) /2
        u_avg_p1 = ( np.sqrt( hip1 ) * uip1 + np.sqrt(hi)*ui ) /(np.sqrt( hip1 ) +
                                                                 np.sqrt(hi ) )
        c_p1 = np.sqrt(g* h_avg_p1 )
        lambda1_p1 = u_avg_p1 - c_p1
        lambda2_p1 = u_avg_p1 + c_p1

        # Wave speeds
        sp1_m1 = np . maximum( lambda1_m1 , 0. )
        sp2_m1 = np . maximum( lambda2_m1 , 0. )
        sp1_p1 = np . minimum ( lambda1_p1 , 0. )
        sp2_p1 = np . minimum ( lambda2_p1 , 0. ) 
        
        delta_m1 = np.subtract(Q [: , i ] ,Q[:, i-1])
        delta_p1= np.subtract(Q[ : , i + 1 ] ,Q[ : , i ] )

        
        # Alpha−c o e f f i c i e n t s o f ( 1 5 . 3 9 ) p . 3 2 2 , LeVeque [ 2 ]
        alpha1_m1 = ( ( u_avg_m1+c_m1 ) * delta_m1 [ 0 ] - delta_m1 [ 1 ] )/2/c_m1
        alpha2_m1 = ( -(u_avg_m1-c_m1 ) * delta_m1 [ 0 ] + delta_m1 [ 1 ] )/2/c_m1

        alpha1_p1 = ( ( u_avg_p1+c_p1 ) * delta_p1[ 0 ] - delta_p1[ 1 ] )/2/c_p1
        alpha2_p1 = ( -( u_avg_p1-c_p1 ) * delta_p1[ 0 ] + delta_p1[ 1 ] )/2/c_p1        
