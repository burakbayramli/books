""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""

# CavityFlow.py: solves Navier-Stokes equation for cavity flow

from numpy import *          # needed for zeros
 
Niter = 200;   Nx = 30;  h = 1;  Ny = 30;  J1 = 15;  J2 = 20
J3 = 6;  J4 = 11; V0 = 18.; omega = 0.1;   h2 = h*h;  nu = 25
iter = 0;    u0 = 3.;  R = V0*h/nu

u = zeros((Nx+1, Ny+1), float);  w = zeros((Nx+1, Ny+1), float)

def top():    
    for i in range(1,Nx+1):  
        u[i-1,Ny] = u[i,Ny]            # vy=-du/dx=0
        w[i,Ny] = 2*(u[i,Ny]-u[i,Ny-1])/h**2 -2*u0/h
def bottom():
    for i in range (1,Nx+1):        # Bottom
        w[i,0] = 2*(u[i,0]-u[i,1])/h**2
        u[i,1] = u[i,0]                # du/dy=0
        u[i-1,0] = u[i,0]              # du/dx=0
def borderleft():
    for j in range (1,Ny+1):           # Right
        if j < J1:  
             w[0,j] =  2*(u[0,j]-u[1,j])/h**2   # Below hole
             u[0,j] = u[1,j]
             u[0,j] = u[0,j-1]
        if j >= J1 and j <= J2:
            w[1,j] = w[0,j]
            u[0,j] = u[0,j-1]+V0*h
        if j > J2:
            u[0,j] = u[0,j-1]            # du/dy=0
            u[1,j] = u[0,j]              # du/dx=0
            w[0,j] =  2*(u[0,j]-u[1,j])/h**2    
def borderight():
    for j in range (1,Ny+1):    # Right
        if j< J3:
            w[Nx-1,j] =  2*(u[Nx-1,j]-u[Nx,j])/h**2  # Below hole
            u[Nx-1,j] = u[Nx,j]
            u[Nx,j] = u[Nx,j-1]         
        if j >= J3 and j <= J4:
            u[Nx,j] = u[Nx-1,j]
            u[Nx,j] = u[Nx,j-1]+V0*h
            w[Nx-1,j] = w[Nx,j]
        if j > J4:
            u[Nx,j] = u[Nx,j-1]
            u[Nx-1,j] = u[Nx,j]
            w[Nx-1,j] =  2*(u[Nx-1,j]-u[Nx,j])/h**2  
def Borders(iter):      # Method borders: init & B.C.
    top()
    bottom()                          
    borderight()                         
    borderleft()
def Relax(iter):
    Borders(iter)                           
    for  i in range(1, Nx):                 
      for  j in range (1, Ny):
                   r1 = omega*((u[i+1,j]+u[i-1,j]+u[i,j+1]
                   	   +u[i,j-1]+h*h*w[i, j])*0.25-u[i,j]) 
                   u[i,j] += r1
    if iter%100 == 0:  print( "Residual r1 ", r1)
    Borders(iter)   
    for  i in range(1, Nx):          # Relax stream function
      for  j in range (1, Ny):  
                  a1 = w[i+1, j] + w[i-1,j] + w[i,j+1] + w[i,j-1]
                  a2 = (u[i,j+1] - u[i,j-1])*(w[i+1,j] - w[i-1,j])
                  a3 = (u[i+1,j] - u[i-1,j])*(w[i,j+1] - w[i,j-1])
                  r2 = omega*((a1 + (R/4)*(a3 - a2) )/4.- w[i,j])
                  w[i,j] += r2
          
while (iter <=  Niter):    
    if iter %100 ==0:  print ("Iteration Number", iter)
    Relax(iter)
    iter   +=  1
utorr = open('Cavity.dat','w')    # Send data to disk  of u
for j in range(0, Ny+1):
    utorr.write("\n") 
    for i in range(0,Nx+1):     
        utorr.write("%10.3e  \n"%(u[i,j]))      
utorr.close()

                      
