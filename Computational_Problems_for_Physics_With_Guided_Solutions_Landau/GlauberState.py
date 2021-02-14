""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""

# GlauberState.py: Glauber's Coherent Quantum State

from numpy import *; from visual.graph import *

wavef = display(x=0,y=0,width=600,height=600,range=50)
plotob = curve(x=range(0,80),color=color.yellow,radius=0.2) 
sqpi = math.sqrt(math.pi)
E = 3. ;  alpha = sqrt(E-0.5)        # E, Coherent Eigenvalue                             
factr = math.exp(-0.5*alpha*alpha)

def Hermite(x, n):                      # Hermite polynomial
    if(n == 0):
        p = 1.0
    elif(n == 1):
        p = 2*x
    else:
        p0 = 1
        p1 = 2*x
        for i in range(1,n):
            p2 = 2*x*p1-2*i*p0
            p0 = p1
            p1 = p2
            p = p2
    return p

def glauber(x,t,nmax):                  # Coherent state
    Reterm = 0.0
    Imterm = 0.0
    factr = math.exp(-0.5*alpha*alpha)
    for n in range (0, nmax):
        fact = math.sqrt(1.0/(math.factorial(n)*sqpi*(2**n)))
        psin = fact*Hermite(x,n)*math.exp(-0.5*x*x)
        den = sqrt(math.factorial(n))
        num = factr*(alpha**n)*psin
        Reterm += num*(math.cos((n+0.5)*t))/den
        Imterm += num*(math.sin((n+0.5)*t))/den
    phi = math.sqrt(Reterm*Reterm+Imterm*Imterm)
    return phi

def motion(nmax):
    for t in arange(0,18.0,0.03):   
        xx = -8.0                     
        for i in range(0,80):
             y = glauber(xx,t,nmax) # Find coherent state
             plotob.x[i] = 4*xx       # Plot state
             plotob.y[i] = 15.0*y
             xx += 0.2
             
motion(20)                 # Parameter: max degree Hn
