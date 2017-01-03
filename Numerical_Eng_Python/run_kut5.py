## module run_kut5
''' X,Y = integrate(F,x,y,xStop,h,tol=1.0e-6).
    Adaptive Runge-Kutta method for solving the
    initial value problem {y}' = {F(x,{y})}, where
    {y} = {y[0],y[1],...y[n-1]}.
    x,y   = initial conditions
    xStop = terminal value of x
    h     = initial increment of x used in integration
    tol   = per-step error tolerance
    F     = user-supplied function that returns the
            array F(x,y) = {y'[0],y'[1],...,y'[n-1]}.
'''
from numarray import array,sum,zeros,Float64
from math import sqrt

def integrate(F,x,y,xStop,h,tol=1.0e-6):

    def run_kut5(F,x,y,h):
      # Runge-Kutta-Fehlberg formulas
        C = array([37./378, 0., 250./621, 125./594,          \
                   0., 512./1771])
        D = array([2825./27648, 0., 18575./48384,            \
                   13525./55296, 277./14336, 1./4])
        n = len(y)
        K = zeros((6,n),type=Float64)
        K[0] = h*F(x,y)
        K[1] = h*F(x + 1./5*h, y + 1./5*K[0])
        K[2] = h*F(x + 3./10*h, y + 3./40*K[0] + 9./40*K[1])
        K[3] = h*F(x + 3./5*h, y + 3./10*K[0]- 9./10*K[1]    \
               + 6./5*K[2])
        K[4] = h*F(x + h, y - 11./54*K[0] + 5./2*K[1]        \
               - 70./27*K[2] + 35./27*K[3]) 
        K[5] = h*F(x + 7./8*h, y + 1631./55296*K[0]          \
               + 175./512*K[1] + 575./13824*K[2]             \
               + 44275./110592*K[3] + 253./4096*K[4])
      # Initialize arrays {dy} and {E}
        E = zeros((n),type=Float64)
        dy = zeros((n),type=Float64)
      # Compute solution increment {dy} and per-step error {E}
        for i in range(6):
            dy = dy + C[i]*K[i]
            E = E + (C[i] - D[i])*K[i]
      # Compute RMS error e       
        e = sqrt(sum(E**2)/n)
        return dy,e
    
    X = []
    Y = []
    X.append(x)
    Y.append(y)
    stopper = 0  # Integration stopper(0 = off, 1 = on)
   
    for i in range(10000):
        dy,e = run_kut5(F,x,y,h)
      # Accept integration step if error e is within tolerance
        if  e <= tol:
            y = y + dy
            x = x + h
            X.append(x)
            Y.append(y)
          # Stop if end of integration range is reached
            if stopper == 1: break
      # Compute next step size from Eq. (7.24)     
        if e != 0.0:    
            hNext = 0.9*h*(tol/e)**0.2
        else: hNext = h
      # Check if next step is the last one; is so, adjust h
        if (h > 0.0) == ((x + hNext) >= xStop):
            hNext = xStop - x
            stopper = 1
        h = hNext
    return array(X),array(Y)
