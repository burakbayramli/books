""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""
   
# LorentzFields.py:  Lorentz TF of E, B & V wi Visual

from visual.graph import *

scene = display(width=700, height=400, range=100,title="In O, Pure Bz(Dots)")
graf = curve(color=color.red)
rr = vector(40,35,0)
charge = sphere(pos=rr,color=color.red, radius=2,make_trail=True)
scene2 = display(y=400,width=700,height=400,range=500,
	title="In O', Bz'(Dots) & Ex'")
charge2 = sphere(pos=rr,color=color.red, radius=2,make_trail=True)

B = vector(0,0,.1);  Bz = B.z                    # 3-D B in O, Bz 
m0 = 1; q = 1                                      # Mass, charge
beta =  0.9;  dt = 0.001                    # v/c, Time step in O
gamma = 1/sqrt(1.-beta**2) 

def plotB():                                     # Plot B as dots
    for i in range(-100,110,10):
        for j in range (-50,60,10): 
        	points(pos = (i,j,0),size=4,display=scene)

def plotBp():                                   # Plot B'  as dots
    for i in range(-500,501,8):
        for j in range (-50,60,8):  
        	points(pos=(i,j,0),size=1,display=scene2)

def Euler():                   # Euler method, solve Eq Mtn in O'
    V = vector(0.9,0,0);   Vx = V.x  ;   Vy = V.y       # Vo in O
    v = vector(beta,0,0)                              # Relative v
    r = vector(40,35,0)                                    # R(t=0)
    den = (1-Vx*beta)      
    Vxp = (V.x-beta)/den;  Vyp = Vy/den/gamma;  Vp = vector(Vxp,Vyp,0) 
    rp = r                                # Initial positions aligned
    for i in range (0,200000):                         # Motion loop
       Bzp = gamma*Bz;  Bp = vector(0,0,Bzp)                     # B' 
       Eyp = -gamma*beta*Bz;  Ep = vector(0,Eyp,0)               # E'
       F = V.cross(B)                                         # Force
       a = F/m0                                       # Acceleration  
       V = V + a*dt                  
       r = r + V*dt                  
       Fp = Vp.cross(Bp)+q*Ep                           # Force in O'
       m = m0*gamma                                      # Mass in S'
       ap = Fp/m                                 # Acceleration in O' 
       dtp = dt*gamma              
       Vp = Vp + ap*dtp                                          # V'
       rp = rp + Vp*dtp - v*dtp                                  # R'
       charge2.display = scene2                             # O' plot
       charge2.pos = vector(rp)
       charge.display = scene                                # O plot
       charge.pos = vector(r)        
            
plotB();   plotBp()                                      # Call plots
Euler()                                              # Begin animation
