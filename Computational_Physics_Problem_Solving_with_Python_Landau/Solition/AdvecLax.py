""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""
	 
# AdvecLax.py:      Solve advection eqnt via Lax-Wendroff scheme
# du/dt+ c*d(u**2/2)/dx=0;   u(x,t=0)=exp(-300(x-0.12)**2)  

from visual.graph import *
m = 100                                       # No steps in x 
c = 1.;     dx = 1./m;    beta = 0.8          # beta = c*dt/dx
u = [0]*(m+1);                                # Initial Numeric   
u0 = [0]*(m+1);
uf = [0]*(m+1)    
dt = beta*dx/c;
T_final = 0.5;
n = int(T_final/dt)                             # N time steps

graph1 = gdisplay(width=600, height=500, xtitle = 'x', xmin=0,
	xmax=1,ymin=0, ymax=1, ytitle = 
	'u(x), Cyan=exact, Yellow=Numerical', title='Advect Eqn: 
	Initial (red), Exact (cyan),Numerical (yellow)')
initfn = gcurve(color = color.red);
exactfn = gcurve(color = color.cyan)
numfn = gcurve(color = color.yellow)       # Numerical solution

def plotIniExac():               # Plot initial & exact solution
   for i in range(0, m):                     
      x = i*dx 
      u0[i] = exp(-300.* (x - 0.12)**2)      # Gaussian initial
      initfn.plot(pos = (0.01*i, u0[i]) )     # Initial function
      uf[i] = exp(-300.*(x - 0.12 - c*T_final)**2) # Exact = cyan
      exactfn.plot(pos = (0.01*i, uf[i]) )
      rate(50)
plotIniExac()

def numerical():                # Finds Lax Wendroff solution
  for j in range(0, n+1):                       #  Time loop               
    for i in range(0, m - 1):                     #   x loop
        u[i + 1] = (1.-beta*beta)*u0[i+1]-(0.5*beta)*(1.-beta)*u0[i+2] \
                    +(0.5*beta)*(1. + beta)*u0[i]   # Algorithm
        u[0] = 0.;     u[m-1] = 0.;    u0[i] = u[i]           
numerical()                   
for j in range(0, m-1 ):
    rate(50)
    numfn.plot(pos = (0.01*j, u[j]) )        # Plot numeric soltn
