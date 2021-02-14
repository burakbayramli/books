""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""

# MD1.py          Molecular dynamics in 1D

from visual import *; from visual.graph import *; import random

scene = display(x=0,y=0,width=700,height=350,
	title='Molecular Dynamics', range=12)        # Spheres
sceneK = gdisplay(x=0,y=350,width=600,height=150,title='Average KE',
    ymin=0.0,ymax=0.3,xmin=0,xmax=100,xtitle='time',ytitle='KE avg')
Kavegraph=gcurve(color= color.red)  # plot KE
scenePE = gdisplay(x=0,y=500,width=600,height=150,title=
	'Pot Energy', ymin=-0.6,ymax=0.0,xmin=0,xmax=100,
	xtitle='time',ytitle='PE')
PEcurve = gcurve(color=color.cyan)                               
Natom = 8                                                
Nmax =  8                                                           
Tinit = 10.0                                       # T initial
t1 = 0
x  = zeros( (Nmax), float)                           
vx = zeros( (Nmax), float)                               
fx = zeros( (Nmax, 2), float)            
L = Natom                               # Length of atom chain
atoms = []                                              

def twelveran():              # Gaussian as average 12 randoms
    s = 0.0
    for i in range (1,13):
        s += random.random()
    return s/12.-0.5

def initialposvel():           # Initial positions, velocities
    i = -1
    for ix in range(0, L):                                             
            i = i + 1                     
            x[i] = ix                      
            vx[i] = twelveran()      
            vx[i] = vx[i]*sqrt(Tinit)
    for j in range(0,Natom):
        xc = 2*x[j] - 7   # Linear transform to place spheres
        atoms.append(sphere(pos=(xc,0), radius=0.5,color=color.red)) 

def sign(a, b):                                         
    if (b >=  0.0):
        return abs(a)
    else:
        return  - abs(a)
                
def Forces(t, PE):                                   # Forces
    r2cut = 9.                                       # Cutoff
    PE = 0.                   
    for i in range(0, Natom):  
        fx[i][t] = 0.0                          
    for i in range( 0, Natom-1 ):
          for j in range(i + 1, Natom):
              dx = x[i] - x[j]                         
              if (abs(dx) > 0.50*L):        
                  dx = dx - sign(L, dx) # Interact closest image
              r2 = dx*dx                    
              if (r2 < r2cut):              
                  if (r2 ==  0.):         # Avoid 0 denominator
                      r2 = 0.0001
                  invr2 = 1./r2   
                  wij =  48.*(invr2**3 - 0.5) *invr2**3   
                  fijx = wij*invr2*dx            
                  fx[i][t] = fx[i][t]  +  fijx    
                  fx[j][t] = fx[j][t]  -  fijx   
                  PE = PE + 4.*(invr2**3)*((invr2**3)-1.)
    return PE                               
		
def timevolution():
    t1=0
    t2 = 1
    h = 0.038                            # Unstable if larger
    hover2 = h/2.0
    KE = 0.0
    PE = 0.0
    initialposvel()                         
    PE = Forces(t1,PE)
    for i in range(0, Natom):               # Kinetic energy
        KE=KE+(vx[i]*vx[i])/2.0   
    t=0
    while t<100:                                 # Time loop
        rate(1)
        for i in range(0,  Natom):
            PE = Forces(t1,PE)
            x[i] = x[i] + h*(vx[i] + hover2*fx[i][t1])   
            if x[i] <= 0.:
               x[i] = x[i] + L             # Periodic bBC
            if x[i] >= L :
               x[i] = x[i] - L
            xc = 2*x[i] - 8              # Linear transform 
            atoms[i].pos=(xc,0)
        PE = 0.0    
        PE = Forces(t2,  PE)
        KE = 0.
        for  i in range(0 , Natom):
            vx[i] = vx[i] + hover2*(fx[i][t1] + fx[i][t2])
            KE = KE + (vx[i]*vx[i] )/2
        T=2*KE/(3*Natom)
        Itemp = t1
        t1 = t2
        t2 = Itemp
        Kavegraph.plot(pos=(t,KE))                # Plot KE
        PEcurve.plot(pos=(t,PE),display=scenePE)  # Plot PE
        t += 1
timevolution()        
