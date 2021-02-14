""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""

# QuantumEigen.py:             Finds E and psi via rk4 + bisection
 
# mass/((hbar*c)**2)= 940MeV/(197.33MeV-fm)**2 =0.4829
# well width=20.0 fm, depth 10 MeV, Wave function not normalized.  

from visual import *

psigr = display(x=0,y=0,width=600,height=300, title='R & L Wavefunc')
Lwf = curve(x=list(range(502)),color=color.red)
Rwf = curve(x=list(range(997)),color=color.yellow)
eps       = 1E-3                                     # Precision
n_steps   = 501 
E         = -17.0                                      # E guess
h         = 0.04
count_max = 100
Emax      = 1.1*E                                     # E limits
Emin      = E/1.1            

def f(x, y, F,E):
    F[0] = y[1]
    F[1] = -(0.4829)*(E-V(x))*y[0]

def V(x):
    if (abs(x) < 10.):  return (-16.0)               # Well depth
    else:               return (0.)
				
def rk4(t, y,h,Neqs,E):
    F  = zeros((Neqs),float)
    ydumb     = zeros((Neqs),float)
    k1 = zeros((Neqs),float)
    k2 = zeros((Neqs),float)
    k3 = zeros((Neqs),float)
    k4 = zeros((Neqs),float)
    f(t, y, F,E)
    for i in range(0,Neqs):
        k1[i] = h*F[i]
        ydumb[i] = y[i] + k1[i]/2.
    f(t + h/2., ydumb, F,E)
    for i in range(0,Neqs):
        k2[i] = h*F[i]
        ydumb[i] = y[i] + k2[i]/2.
    f(t + h/2., ydumb, F,E)
    for i in range(0,Neqs):
        k3[i]=  h*F[i]
        ydumb[i] = y[i] + k3[i]
    f(t + h, ydumb, F,E);
    for i in range(0,Neqs):
        k4[i]=h*F[i]
        y[i]=y[i]+(k1[i]+2*(k2[i]+k3[i])+k4[i])/6.0
				
def diff(E, h):
    y = zeros((2),float)
    i_match = n_steps//3                     # Matching radius
    nL = i_match + 1  
    y[0] = 1.E-15;                           # Initial left wf
    y[1] = y[0]*sqrt(-E*0.4829)    
    for ix in range(0,nL + 1):
        x = h * (ix  -n_steps/2)
        rk4(x, y, h, 2, E)
    left = y[1]/y[0]                        # Log  derivative
    y[0] = 1.E-15;               # For even;  reverse for odd
    y[1] = -y[0]*sqrt(-E*0.4829)            # Initialize R wf
    for ix in range( n_steps,nL+1,-1):
        x = h*(ix+1-n_steps/2)
        rk4(x, y, -h, 2, E)
    right = y[1]/y[0]                         # Log derivative
    return( (left - right)/(left + right) )
		
def plot(E, h):                 # Repeat integrations for plot
    x = 0. 
    n_steps = 1501                       # # integration steps
    y = zeros((2),float)
    yL = zeros((2,505),float) 
    i_match = 500                             # Matching point
    nL = i_match + 1;  
    y[0] = 1.E-40                            # Initial left wf
    y[1] = -sqrt(-E*0.4829) *y[0]
    for ix in range(0,nL+1):                          
        yL[0][ix] = y[0]
        yL[1][ix] = y[1]
        x = h * (ix -n_steps/2)
        rk4(x, y, h, 2, E)
    y[0] = -1.E-15                # For even;  reverse for odd
    y[1] = -sqrt(-E*0.4829)*y[0]
    j=0
    for ix in range(n_steps -1,nL + 2,-1):          # right WF
        x = h * (ix + 1 -n_steps/2)             # Integrate in
        rk4(x, y, -h, 2, E)
        Rwf.x[j] = 2.*(ix + 1 -n_steps/2)-500.0
        Rwf.y[j] = y[0]*35e-9 +200
        j +=1
    x = x-h              
    normL = y[0]/yL[0][nL]
    j=0
    # Renormalize L wf & derivative
    for ix in range(0,nL+1):
        x = h * (ix-n_steps/2 + 1) 
        y[0] = yL[0][ix]*normL 
        y[1] = yL[1][ix]*normL
        Lwf.x[j] = 2.*(ix  -n_steps/2+1)-500.0
        Lwf.y[j] = y[0]*35e-9+200           # Factor for scale
        j +=1
for count in range(0,count_max+1):
    rate(1)                       # Slow rate to show changes
    # Iteration loop
    E = (Emax + Emin)/2.                    # Divide E range
    Diff = diff(E, h) 
    if (diff(Emax, h)*Diff > 0):  Emax = E  # Bisection algor
    else:                         Emin = E     
    if ( abs(Diff)  <  eps ):     break
    if count >3:                # First iterates too irregular
        rate(4)
        plot(E, h)
    elabel = label(pos=(700, 400), text='E=', box=0)
    elabel.text = 'E=%13.10f' %E
    ilabel = label(pos=(700, 600), text='istep=', box=0)
    ilabel.text = 'istep=%4s' %count
elabel = label(pos=(700, 400), text='E=', box=0)  # Last iteration
elabel.text = 'E=%13.10f' %E
ilabel = label(pos=(700, 600), text='istep=', box=0)
ilabel.text = 'istep=%4s' %count
        
print("Final eigenvalue E = ",E)
print("iterations, max = ",count)

