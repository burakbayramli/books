""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""
   
# TwoCharges.py:  Motion of 2 charges in 2 frames wi Visual

from visual.graph import *

scene = display(width = 700, height=300, range=1, background=(1,1,1),
           title="Frame O: Charges with Parallel Initial Velocities")
graf = curve(color=color.red)
r1 = vector(-.9,.2,0)
charge = sphere(pos=r1,color=color.red, radius=.02,make_trail=True)
r2 = vector(-.9,-.2,0)
charge2 = sphere(pos=r2, color=color.red, radius=.02, make_trail=True)
scene2 = display(y=700,width=700,height=300, range=1, background=(1,1,1),
               title= "Frame O':  Motion as Seen in Moving Frame")
r1p = r1;    r2p = r2
charge3 = sphere(color=color.red,radius=.02,make_trail=True,
	display=scene2)
charge4 = sphere(color=color.red,radius=.02,make_trail=True,
	display=scene2)
mu0 = 1;      e0 = 1;         q1 = q2 = 2;    m0 = 50.
beta = 0.3;   gamma = 1/sqrt(1-beta**2)  
m = m0*gamma;  u = vector(0.3,0,0)               # Vo in O of q1   
dt = 0.01;     dtp = dt*gamma                # Time step in O, O'
r1 = vector(-.9,.2,0);   r2 = vector(-.9,-.2,0)    # Initial 1, 2

def EulerPlusTF(u, r1, r2, beta):  # Euler ODE solve + Lorentz TF
    v2 = u;    v1 = u                                         
    for i in range (0,300):                         # Motion loop
       rate(100)            
       rr1 = r2-r1                                     # q2 wrt r1
       rr2 = -rr1                                      # q1 wrt r2
       rr = mag(rr1)             
       B1 =  q1*cross(v1,rr1)/(4*pi *rr**3)               # B at q2
       B2 =  q2*cross(v2,rr2)/(4*pi *rr**3)               # B at q1
       E1 = q1*rr1/(4*pi*rr**3)                           # E at q2
       E2 = q2*rr2/(4*pi*rr**3)                           # E at q1
       F1 = q2*(E1 + cross(v2,B1))                    # Force on q2
       F2 = q1*(E2 + cross(v1,B2))                    # Force on q1
       a2 = F1/m0;     a1 = F2/m0                                      
       v1 = v1 + a2*dt ;   v2 = v2 + a1*dt            
       x1 = r1.x;
       r1 = r1 + v2*dt;    r2 = r2 + v1*dt                 # Update
       x1 =r1.x;           x2 = r2.x                                                                     
       y1 = r1.y;          y2 = r2.y                                   
       t = i*dt                                          # Time in O
#      Now transform to O'
       x1p = (x1-beta*t)/(sqrt(1-beta*beta))             # TF x to x'
       y1p = y1                                          # TF y to y'
       charge3.pos = vector(x1p,y1p,0)
       x2p = (x2-beta*t)/(sqrt(1-beta*beta))  
       y2p = y2                               
       charge3.pos = vector(x1p,y1p,0)
       charge4.pos = vector(x2p,y2p,0)
       charge2.pos = r2
       charge.pos = r1
       
EulerPlusTF(u, r1, r2, beta)                          # Call animation
    
    
    
