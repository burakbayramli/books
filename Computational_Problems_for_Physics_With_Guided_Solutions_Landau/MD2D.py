""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""

# MD2.py:           Molecular dynamics in 2D

from visual import *; from visual.graph import *; import random

scene = display(x=0,y=0,width=350,height=350, 
	title='Molecular Dynamics', range=10)
sceneK = gdisplay(x=0,y=350,width=600,height=150,title='Average KE',
     ymin=0.0,ymax=5.0,xmin=0,xmax=500,xtitle='time',ytitle='KE avg')
Kavegraph=gcurve(color= color.red)
sceneT = gdisplay(x=0,y=500,width=600,height=150,title='Average PE',
     ymin=-60,ymax=0.,xmin=0,xmax=500,xtitle='time',ytitle='PE avg')
Tcurve = gcurve(color=color.cyan)
Natom = 25
Nmax =  25
Tinit = 2.0
dens = 1.0                              # Density (1.20 for fcc)
t1 = 0
x  = zeros( (Nmax),    float)                        
y  = zeros( (Nmax),    float)                                 
vx = zeros( (Nmax),    float)                            
vy = zeros( (Nmax),    float)                 
fx = zeros( (Nmax, 2), float)                       
fy = zeros( (Nmax, 2), float)                       
L = int(1.*Natom**0.5)                        # Side of lattice
atoms=[] 

def twelveran():                # Average 12 rands for Gaussian
    s=0.0
    for i in range (1,13):
        s += random.random()
    return s/12.0-0.5

def initialposvel():                            # Initialize
    i = -1
    for ix in range(0, L):             # x->   0  1  2  3  4
        for iy in range(0, L):         # y=0   0  5  10 15 20 
            i = i + 1                  # y=1   1  6  11 16 21
            x[i]  = ix                 # y=2   2  7  12 17 22
            y[i]  = iy                 # y=3   3  8  13 18 23
            vx[i] = twelveran()        # y=4   4  9  14 19 24
            vy[i] = twelveran()        # numbering of 25 atoms
            vx[i] = vx[i]*sqrt(Tinit)  
            vy[i] = vy[i]*sqrt(Tinit)
    for j in range(0,Natom):
        xc = 2*x[j] - 4
        yc = 2*y[j] - 4
        atoms.append(sphere(pos=(xc,yc), radius=0.5,
        	color=color.red))
				
def sign(a, b):
    if (b >=  0.0):
        return abs(a)
    else:
        return  - abs(a)
                
def Forces(t, w, PE, PEorW):                            # Forces
    # invr2 = 0. 
    r2cut = 9.                       # Switch: PEorW = 1 for PE   
    PE = 0.                   
    for i in range(0, Natom):  
        fx[i][t] = fy[i][t]  = 0.0   
    for i in range( 0, Natom-1 ):
          for j in range(i + 1, Natom):
              dx = x[i] - x[j]                         
              dy = y[i] - y[j]                         
              if (abs(dx) > 0.50*L):          
                  dx = dx  -  sign(L, dx) # Closest image
              if (abs(dy) > 0.50*L):                          
                  dy = dy  -  sign(L, dy)
              r2 = dx*dx  +  dy*dy                         
              if (r2 < r2cut):                      
                  if (r2 ==  0.):   # To avoid 0 denominator
                      r2 = 0.0001
                  invr2 = 1./r2                      
                  wij =  48.*(invr2**3 - 0.5) *invr2**3  
                  fijx = wij*invr2*dx             
                  fijy = wij*invr2*dy            
                  fx[i][t] = fx[i][t]  +  fijx    
                  fy[i][t] = fy[i][t]  +  fijy
                  fx[j][t] = fx[j][t]  -  fijx         
                  fy[j][t] = fy[j][t]  -  fijy     
                  PE = PE + 4.*(invr2**3)*((invr2**3) - 1.)
                  w = w  +  wij
    if (PEorW ==  1):
        return PE
    else:
        return w
				
def timevolution():
    avT = 0.0
    avP = 0.0
    Pavg = 0.0
    avKE = 0.0
    avPE = 0.0
    t1 = 0
    PE = 0.0
    h = 0.031                                        # step
    hover2 = h/2.0
    # initial KE & PE via Forces
    KE = 0.0
    w = 0.0
    initialposvel()   
    for i in range(0,Natom): KE = KE+(vx[i]*vx[i]+vy[i]*vy[i])/2
     PE = Forces(t1,w,PE,1)
    time =1
    while 1:
        rate(100)
        for i in range(0,  Natom):
            PE = Forces(t1,w,PE,1)
            x[i] = x[i] + h*(vx[i] + hover2*fx[i][t1])   
            y[i] = y[i] + h*(vy[i] + hover2*fy[i][t1]);
            if x[i] <= 0.:
                x[i] = x[i] + L            # Periodic BC
            if x[i] >= L :
               x[i] = x[i] - L
            if y[i] <= 0.:
                y[i] = y[i] + L
            if y[i] >= L:
                y[i] = y[i] - L
            xc = 2*x[i] - 4
            yc = 2*y[i] - 4
            atoms[i].pos=(xc,yc)
        PE = 0.
        t2=1
        PE = Forces(t2, w, PE, 1)
        KE = 0.
        w = 0.
        for  i in range(0 , Natom):
            vx[i] = vx[i] + hover2*(fx[i][t1] + fx[i][t2])
            vy[i] = vy[i] + hover2*(fy[i][t1] + fy[i][t2])
            KE = KE + (vx[i]*vx[i] + vy[i]*vy[i])/2
        w = Forces(t2, w, PE, 2)
        P=dens*(KE+w)
        T=KE/(Natom)   # increment averages
        avT = avT + T                                        
        avP = avP + P                                           
        avKE = avKE + KE                                  
        avPE = avPE + PE                                
        time += 1
        t=time
        if (t==0):
            t=1
        Pavg  = avP /t
        eKavg = avKE /t 
        ePavg = avPE /t
        Tavg  = avT /t
        pre = (int)(Pavg*1000)
        Pavg = pre/1000.0
        kener = (int)(eKavg*1000)
        eKavg = kener/1000.0
        Kavegraph.plot(pos=(t,eKavg))
        pener = (int)(ePavg*1000)
        ePavg = pener/1000.0
        tempe = (int)(Tavg*1000000)
        Tavg = tempe/1000000.0
        Tcurve.plot(pos=(t,ePavg),display=sceneT)
        
timevolution()        
