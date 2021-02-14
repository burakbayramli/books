""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""

# CatFriction.py: Solve for wave on catenary with friction

from numpy import *

dt = 0.0001; dx = 0.01; T = 1; rho = 0.1
maxtime = 100; kappa = 30; D = T/(rho*9.8) 

x = zeros((512,3),float)
q = open('CatFriction.dat','w');  rr = open('CatFunct.dat','w+t')

for i in range (0,101): x[i][0] = -0.08*sin(pi*i*dx)  # IC
for i in range(1,100): # First step
    x[i][1] = ( dt*(T/rho)*(( x[i+1][0]-x[i][0] )
    	    /dx*( exp((i-50)*dx/D)
               -exp(-(i-50)*dx/D))/D +(exp((i-50)*dx/D)
               	+exp(-(i-50)*dx/D))* ( x[i+1][0]+x[i-1][0]
               -2.0*x[i][0] )/(pow(dx,2))  )
                -2*kappa*x[i][0]+2*x[i][0]/dt  )/(2*kappa+(2/dt))   	
for k in range (0,300): # Other steps
    for i in range(1,100):    
       x[i][2] = (dt*(T/rho)*((x[i+1][1]-x[i][1])
       	       /dx*(exp((i-50)*dx/D) -exp(-(i-50)*dx/D))/D 
       	       +(exp((i-50)*dx/D)+exp(-(i-50)*dx/D)) *
              (x[i+1][1]+x[i-1][1]-2.0*x[i][1])/(pow(dx,2)))
              -2*kappa*x[i][1]-(-2*x[i][1]+x[i][0])/dt)/(2*kappa+(1/dt))            
    for i in range(1,101):
        x[i][0] = x[i][1]
        x[i][1] = x[i][2]
    if (k%4==0 or k==0): 
      for i in range(0,100): 
          a1=exp((i-50.)*dx/D)
          a2=exp(-(i-50.)*dx/D)
          rr.write("%7.3f"%(D*(a1+a2)))
          rr.write("\n")
          q.write('%7.3f'%(x[i,2]))
          q.write("\n")
      q.write("\n");
      rr.write("\n");
rr.closed
q.closed	    
print("Data stored in CatFrict.dat and CatFunct.dat")


	
