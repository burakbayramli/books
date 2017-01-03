## module brent
''' root = brent(f,a,b,tol=1.0e-9).
    Finds root of f(x) = 0 by combinig quadratic interpolation
    with bisection (simplified Brent's method).
    The root must be bracketed in (a,b).
    Calls user-supplied function f(x).
'''    
import error

def brent(f,a,b,tol=1.0e-9):
    x1 = a; x2 = b;
    f1 = f(x1)
    if f1 == 0.0: return x1
    f2 = f(x2)
    if f2 == 0.0: return x2
    if f1*f2 > 0.0: error.err('Root is not bracketed')
    x3 = 0.5*(a + b)    
    for i in range(30):
        f3 = f(x3)
        if abs(f3) < tol: return x3
      # Tighten the brackets on the root
        if f1*f3 < 0.0: b = x3       
        else: a = x3
        if (b - a) < tol*max(abs(b),1.0): return 0.5*(a + b)
      # Try quadratic interpolation
        denom = (f2 - f1)*(f3 - f1)*(f2 - f3)
        numer = x3*(f1 - f2)*(f2 - f3 + f1)       \
              + f2*x1*(f2 - f3) + f1*x2*(f3 - f1)
      # If division by zero, push x out of bounds
        try: dx = f3*numer/denom
        except ZeroDivisionError: dx = b - a
        x = x3 + dx
      # If iterpolation goes out of bounds, use bisection  
        if (b - x)*(x - a) < 0.0:
            dx = 0.5*(b - a)
            x = a + dx
      # Let x3 <-- x & choose new x1 and x2 so that x1 < x3 < x2         
        if x < x3:        
            x2 = x3; f2 = f3        
        else:              
            x1 = x3; f1 = f3
        x3 = x                      
    print 'Too many iterations in brent'
    
        
            
