## module taylor
''' X,Y = taylor(deriv,x,y,xStop,h).
    4th-order Taylor series method for solving the initial
    value problem {y}' = {F(x,{y})}, where
    {y} = {y[0],y[1],...y[n-1]}.
    x,y   = initial conditions
    xStop = terminal value of x
    h     = increment of x used in integration
    deriv = user-supplied function that returns the 4 x n array
        [y'[0]   y'[1]   y'[2] ...  y'[n-1]
         y"[0]   y"[1]   y"[2] ...  y"[n-1]
        y"'[0]  y"'[1]  y"'[2] ... y"'[n-1]
        y""[0]  y""[1]  y""[2] ... y""[n-1]]
'''
from numarray import array
def taylor(deriv,x,y,xStop,h):
    X = []
    Y = []
    X.append(x)
    Y.append(y)
    while x < xStop:            # Loop over integration steps
        h = min(h,xStop - x)
        D = deriv(x,y)          # Derivatives of y
        H = 1.0
        for j in range(4):      # Build Taylor series
            H = H*h/(j + 1)     
            y = y + D[j]*H      # H = h^j/j!
        x = x + h    
        X.append(x)             # Append results to
        Y.append(y)             # lists X and Y
    return array(X),array(Y)    # Convert lists into arrays  
