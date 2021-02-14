""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""

# QuantumEigenCall.py: Finds E & psi via rk4 + bisection
 
# m/(hbar*c)**2 = 940MeV/(197.33MeV-fm)**2 = 0.4829  
from visual import *; from rk4Algor import rk4Algor

psigr = display(x=0,y=0,width=600,height=300, title='R & L Psi')
Lwf = curve(x=list(range(502)),color=color.red)
Rwf = curve(x=list(range(997)),color=color.yellow)
eps = 1E-1; Nsteps = 501;  h=0.04; Nmax = 100 # Search Params
E = -17.; Emax = 1.1*E;  Emin = E/1.1       # Init E & limits
           
def f(x, y):                                    # RHS for ODE
    global E
    F = zeros((2), float)
    F[0] = y[1]
    F[1] = -(0.4829)*(E-V(x))*y[0]
    return F

def V(x):                                        # Potential 
    if (abs(x) < 10.):  return (-16.0)              
    else:               return (0.)
				
def diff(h):                           # Change in log deriv
    global E
    y = zeros((2),float)
    i_match = Nsteps//3                     # Matching radius
    nL = i_match + 1  
    y[0] = 1.E-15;                          # Initial left wf
    y[1] = y[0]*sqrt(-E*0.4829)    
    for ix in range(0,nL + 1):
        x = h * (ix  -Nsteps/2)
        y = rk4Algor(x, h, 2, y, f)
    left = y[1]/y[0]                       # Log  derivative
    y[0] = 1.E-15;         #  Slope for even; reverse if odd
    y[1] = -y[0]*sqrt(-E*0.4829)           # Initialize R wf
    for ix in range( Nsteps,nL+1,-1):
        x = h*(ix+1-Nsteps/2)
        y = rk4Algor(x, -h, 2, y, f)
    right = y[1]/y[0]                       # Log derivative
    return( (left - right)/(left + right) )
		
def plot(h):                   # Repeat integrations for plot
    global E
    x = 0. 
    Nsteps = 1501                         # Integration steps
    y = zeros((2),float)
    yL = zeros((2,505),float) 
    i_match = 500                           # Matching radius
    nL = i_match + 1;  
    y[0] = 1.E-40                           # Initial left wf
    y[1] = -sqrt(-E*0.4829) *y[0]
    for ix in range(0,nL+1):                          
        yL[0][ix] = y[0]
        yL[1][ix] = y[1]
        x = h * (ix -Nsteps/2)
        y = rk4Algor(x, h, 2, y, f)
    y[0] = -1.E-15         # - slope: even;  reverse if odd
    y[1] = -sqrt(-E*0.4829)*y[0]
    j=0
    for ix in range(Nsteps -1,nL + 2,-1):        # Right WF
        x = h * (ix + 1 -Nsteps/2)          # Integrate in
        y = rk4Algor(x, -h, 2, y, f) 
        Rwf.x[j] = 2.*(ix + 1 -Nsteps/2)-500.0
        Rwf.y[j] = y[0]*35e-9 +200
        j +=1
    x = x-h              
    normL = y[0]/yL[0][nL]
    j=0
    for ix in range(0,nL+1):   # Normalize L wf & derivative
        x = h * (ix-Nsteps/2 + 1) 
        y[0] = yL[0][ix]*normL 
        y[1] = yL[1][ix]*normL
        Lwf.x[j] = 2.*(ix  -Nsteps/2+1)-500.0
        Lwf.y[j] = y[0]*35e-9+200        # Factor for scale
        j +=1

for count in range(0,Nmax+1):              # Main program
    rate(1)                     # Slow rate shows changes
    E = (Emax + Emin)/2.                  # Bisec E range
    Diff = diff(h)
    Etemp = E
    E = Emax
    diffMax = diff(h)
    E = Etemp
    if (diffMax*Diff > 0):  Emax = E    # Bisection algor
    else:                   Emin = E
    print("Iteration, E =", count,  E)
    if ( abs(Diff)  <  eps ):     break
    if count >3:                            
        rate(4)
        plot(h)
    elabel = label(pos=(700, 400), text='E=', box=0)
    elabel.text = 'E=%13.10f' %E
    ilabel = label(pos=(700, 600), text='istep=', box=0)
    ilabel.text = 'istep=%4s' %count
elabel = label(pos=(700, 400), text='E=', box=0)    # Last
elabel.text = 'E=%13.10f' %E
ilabel      = label(pos=(700, 600), text='istep=', box=0)
ilabel.text = 'istep=%4s' %count   
print("Final eigenvalue E =", E)
print("Iterations = ",count,", max = ", Nmax)

