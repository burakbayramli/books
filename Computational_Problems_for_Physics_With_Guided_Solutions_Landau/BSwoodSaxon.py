""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia,
    C Bordeianu, Univ Bucharest, 2017.
    Please respect copyright & acknowledge our work."""

# BSwoodSaxon.py # Bound state in Woods-Saxon potential

from visual import *

psigr = display(x=0,y=0,width=600,height=500,
        title='Match of R & L Wave Functions @ Vertical Line',
                background=color.white, foreground=color.black)
psi =   curve(x=list(range(0,1000)),display=psigr,color=color.orange)
poten = curve(x=list(range(0,1000)),display=psigr)

dl = 1e-6; dE = 0.01; n = 1000; imax = 100                 
uL = zeros((n),float);   uR = zeros((n),float); potV = zeros((n),float)
k2L = zeros((n),float);  k2R = zeros((n),float)      # k**2 R & L

h=10.0/n; xL0 = 0; xR0 =  10.0         # leftmost, rightmost  x point
Emin = -5.5; Emax = -2.0                 # root between Emin and Emax
E = Emin                   # initial guess for energy
uL[0] = 0.0;  uL[1] = 0.00001; uR[0] = 0.0; uR[1] = 0.00001
im = 400;  nL = im+2; nr = n-im+1    # match point left and right wv
istep = 0

def V(x):                          #Potential :Woods Saxon
    V0 = 50
    a = 0.5
    R = 1.25*4                           #R =r0*A**(1/3), r0=1.25
    v =  -V0/(1.0+exp((x-R)/a))
    return v

def plotV():
    for i in range(0,n):
        x = i*h
        potV[i] = V(x)
        poten.x[i] = 200*x-1000
        poten.y[i] = 20*potV[i]+300

def setk2():                      #sets k2l = (sqrt(e-V))^2 and k2R
    for i in range(0,n):
       xL = xL0+i*h
       xR  =  xR0-i*h
       k2L[i]  =  E-V(xL)
       k2R[i] = E-V(xR)

def numerov (n,h,k2,u):            #Numerov algorithm can be used for
    b = (h**2)/12.0                  #left and right wave functions
    for i in range( 1,n-1):
        u[i+1] = (2*u[i]*(1.0-5.*b*k2[i])-(1.+b*k2[i-1])*u[i-1])/(1.+b*k2[i+1])

plotV()
setk2()
numerov (nL,h,k2L,uL)              #finds left wave function
numerov (nr,h,k2R,uR)              #finds right wave function
fact =  uR[nr-2]/uL[im]              # to Rescale  solution
for i  in range (0,nL):
    uL[i] = fact*uL[i]
f0 = (uR[nr-1]+uL[nL-1]-uR[nr-3]-uL[nL-3])/(2*h*uR[nr-2])  #  Log deriv

def normalize():
    asum = 0
    for i in range( 0,n):                     #to normalize wave function
        if i > im :
            uL[i] = uR[n-i-1]
            asum = asum + uL[i]*uL[i]
    asum = sqrt(h*asum);
    Elabel = label(pos=(-700, 500), text='e=', box=0,display=psigr)
    Elabel.text = 'E = %10.8f' %E
    ilabel = label(pos=(-750, 400), text='istep=', box=0,display=psigr)
    ilabel.text = 'istep=%4s' %istep
    label(pos=(-960,300),text='0',box=0)
    label(pos=(-930,-750),text='-50',box=0)
    label(pos=(-200,-750),text='r = 4 fm',box=0)
    label(pos=(900,250),text='r (fm)',box=0)
    label(pos=(940,390),text='10',box=0)
    for i in range(0,n):
        xL = xL0+i*h
        psi.y[i] = 100*uL[i]/asum           # wave function normalized
        #psi.x[j] = xL-500                  # For plotting psi
        xp = 200*xL-1000
        psi.x[i] = xp
        line  = curve(pos=[(-995,-700),(-995,300)],display=psigr)
        line = curve(pos=[(-200,-700),(-200,400)],color=color.red,display=psigr)

while abs(dE) > dl and istep < imax :   # Begin bisection algorithm
    rate(2)                             # to slowdown  animation
    E1 = E                              # guess for root
    E =   (Emin+Emax)/2                 # bisect interval
    for i in range(0,n):
        k2L[i] = k2L[i]+ E-E1
        k2R[i] = k2R[i]+ E-E1
    im = 500;
    nl = im+2
    nr = n-im+1;
    numerov (nl,h,k2L,uL)               # Wavefuntions for new k2L,k2R
    numerov (nr,h,k2R,uR)
    fact=uR[nr-2]/uL[im]
    for i in range(0,nL):
        uL[i] = fact*uL[i]
    f1 = (uR[nr-1]+uL[nl-1]-uR[nr-3]-uL[nl-3])/(2*h*uR[nr-2])      # Log deriv.
    rate(2)
    if f0*f1 < 0:                        # Bisection algorithm
        Emax = E
        dE = Emax-Emin
    else:
         Emin = E
         dE = Emax-Emin
         f0 = f1
    normalize()
    print ("Iteration number =",istep,", energy = ", E)
    istep = istep+1
curve(pos=[(-1000,e*20+300),(700,E*20+300)],color=color.green)

