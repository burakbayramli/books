""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""

# Torricelli.py: solves Navier-Stokes equation for orifice flow

from numpy import *          # Need for zeros
 
Niter = 700;  Ndown = 20;  Nx  =  17;  N2x = 2*Nx; Ny  = 156                           
Nb = 15;  h = 0.4;  h2 = h*h; g = 980.;  nu = 0.5; iter = 0;     
Vtop = 8.0e-4;    omega = 0.1;  R = Vtop*h/nu

u = zeros((Nx+1, Ny+1), float);  ua =zeros((N2x,Ny), float)     
w = zeros((Nx+1, Ny+1), float)       

Torri = open('Torri.dat','w');  uall = open('uall.dat','w')

def BelowHole():
    for i in range(Nb+1,Nx+1):   # Below orifice
        u[i,0] = u[i-1,1]    # du/dy =vx=0
        w[i-1,0] = w[i-1,1]  # Water is at floor  
        for j in range (0,Ndown+1):  
            if i==Nb:    vy = 0
            if i==Nx:    vy = -sqrt(2.0*g*h*(Ny+Nb-j))
            if i==Nx-1:  vy = -sqrt(2.0*g*h*(Ny+Nb-j))/2.
            u[i,j] = u[i-1,j]-vy*h      # du/dx=-vy    

def BorderRight():
    for j in range (1,Ny+1):    # Center orifice very sensitive 
        vy = -sqrt(2.0*g*h*(Ny-j))    
        u[Nx,j] = u[Nx-1,j]+vy*h     
        u[Nx,j] = u[Nx,j-1]          
        w[Nx,j] = -2*(u[Nx,j]-u[Nx,j-1])/h**2 

def BottomBefore():
    for i in range (1,Nb+1):       # Bottom,  before the hole
        u[i,Ndown] = u[i,Ndown-1]     
        w[i,Ndown] = -2*(u[i,0]-u[i,1])   

def Top():    
    for i in range(1,Nx):       # Top  
        u[i,Ny] = u[i,Ny-1]       
        w[i,Ny] = 0

def Left():        
    for  j in range (Ndown,Ny):     # Left wall
        w[0,j] =  -2*(u[0,j]-u[1,j])/h**2
        u[0,j] = u[1,j]   # du/dx=0

def Borders(iter):        # Method borders: init & B.C.
    BelowHole()
    BorderRight()              #right (center of hole)
    BottomBefore()             # Bottom before the hole
    Top()
    Left()
    
def Relax(iter):
    Borders(iter)                           
    for  i in range(1, Nx):                 
      for  j in range (1, Ny):
          if j<=Ndown:
              if i>Nb:
                   r1 = omega*((u[i+1,j]+u[i-1,j]+u[i,j+1]+u[i,j-1]
                   	   +h*h*w[i, j])*0.25-u[i,j]) 
                   u[i,j]+= r1
          if j>Ndown:
               r1 = omega*((u[i+1,j]+u[i-1,j]+u[i,j+1]+u[i,j-1]
               	       +h*h*w[i, j])*0.25-u[i,j]) 
               u[i,j]+= r1  
    if iter%50==0:
       print("Residual r1 ", r1)
    Borders(iter)   
    for  i in range(1, Nx):         # Relax stream function
      for  j in range (1, Ny):  
          if j<=Ndown:
              if i>=Nb:
                  a1 = w[i+1, j]+ w[i-1,j]+w[i,j+1]+ w[i,j-1]
                  a2 = (u[i,j+1]-u[i,j-1])*(w[i+1,j]-w[i-1,j])
                  a3 = (u[i+1,j]-u[i-1,j])*(w[i,j+1]-w[i,j-1])
                  r2 = omega*( (a1 + (R/4.)*(a3 - a2) )/4.0-w[i,j])
                  w[i,j]+=r2
          if j>Ndown:
               a1 = w[i+1, j]+ w[i-1,j]+w[i,j+1]+ w[i,j-1]
               a2 = (u[i,j+1]-u[i,j-1])*(w[i+1,j]-w[i-1,j])
               a3 = (u[i+1,j]-u[i-1,j])*(w[i,j+1]-w[i,j-1])
               r2 = omega*( (a1 + (R/4.)*(a3 - a2) )/4.0-w[i,j])
               w[i,j]+=r2

while (iter <=  Niter):    
    if iter %100 == 0:
        print ("Iteration", iter)  # iterations counted
    Relax(iter)
    iter   +=  1      # counter of iterations
for j in range(0,Ny): # Send w to disk in gnuplot format
    for i in range(0,Nx):
        Torri.write("%8.3e \n"%(w[i,j]))
    Torri.write("\n")    
Torri.close()
for j in range(0,Ny):  # Send symmetric tank data to disk 
    for i in range(0,N2x):
        if i <= Nx:
           ua[i,j] = u[i,j]
           uall.write("%8.3e \n"%(ua[i,j])) 
        if i > Nx:
            ua[i,j] = u[N2x-i,j]
            uall.write("%8.3e \n"%(ua[i,j]))            
    uall.write("\n")    
uall.close()
utorr = open('Torri.dat','w')    # Send u data to disk
for j in range(0,Ny):
    utorr.write("\n") 
    for i in range(0,Nx):     
        utorr.write("%10.3e  \n"%(u[i,j]))      
utorr.close()
