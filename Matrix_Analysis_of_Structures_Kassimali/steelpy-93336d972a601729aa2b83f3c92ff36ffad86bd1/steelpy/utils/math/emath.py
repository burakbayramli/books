# emath module


def cbrt(x):
    """
    cbrt(x) = x^{1/3},  if x >= 0
      = -|x|^{1/3},  if x < 0
    """    
    from math import pow
    
    if x >= 0:
        return pow(x, 1.0/3.0)
    else:
        return -pow(abs(x), 1.0/3.0)


def rect(r, w, deg=0):    # radian if deg=0; degree if deg=1
    """
    Convert from polar (r,w) to rectangular (x,y)
    x = r cos(w)
    y = r sin(w)
    """    
    from math import cos, sin, pi
    if deg:
        w = pi * w / 180.0
    return r * cos(w), r * sin(w)


def polar(x, y, deg=0):   # radian if deg=0; degree if deg=1
    """
    Convert from rectangular (x,y) to polar (r,w)
    r = sqrt(x^2 + y^2)
    w = arctan(y/x) = [-\pi,\pi] = [-180,180]
    """    
    from math import hypot, atan2, pi
    
    if deg:
        return hypot(x, y), 180.0 * atan2(y, x) / pi
    else:
        return hypot(x, y), atan2(y, x) 

def quadratic(a, b, c=None):
    """
    x^2 + ax + b = 0  (or ax^2 + bx + c = 0)
    By substituting x = y-t and t = a/2, the equation reduces to 
    y^2 + (b-t^2) = 0 
      which has easy solution
    y = +/- sqrt(t^2-b)
    """
    import math, cmath
    
    if c:   # (ax^2 + bx + c = 0)
        a, b = b / float(a), c / float(a)
    
    t = a / 2.0
    r = t**2 - b
    
    if r >= 0:    # real roots
        y1 = math.sqrt(r)
    else:   # complex roots
        y1 = cmath.sqrt(r)
    
    y2 = -y1
    
    return y1 - t, y2 - t


def cubic(a, b, c, d=None):
    """
    x^3 + ax^2 + bx + c = 0  (or ax^3 + bx^2 + cx + d = 0)
    With substitution x = y-t and t = a/3, the cubic equation reduces to    
    y^3 + py + q = 0,
      where p = b-3t^2 and q = c-bt+2t^3.  Then, one real root y1 = u+v can
      be determined by solving 
    w^2 + qw - (p/3)^3 = 0
      where w = u^3, v^3.  From Vieta's theorem,
    y1 + y2 + y3 = 0
    y1 y2 + y1 y3 + y2 y3 = p
    y1 y2 y3 = -q,
    the other two (real or complex) roots can be obtained by solving
    y^2 + (y1)y + (p+y1^2) = 0
    """    
    from math import cos
    
    if d:     # (ax^3 + bx^2 + cx + d = 0)
        a, b, c = b / float(a), c / float(a), d / float(a)
    
    t = a / 3.0
    p, q = b - 3 * t**2, c - b * t + 2 * t**3
    u, v = quadratic(q, -(p/3.0)**3)
    
    if type(u) == type(0j): # complex cubic root
        r, w = polar(u.real, u.imag)
        y1 = 2 * cbrt(r) * cos(w / 3.0)
    
    else:     # real root
        y1 = cbrt(u) + cbrt(v)
    
    y2, y3 = quadratic(y1, p + y1**2)
    
    if isinstance(y2, complex):
        #print('stop') 
        y2 = y2.real
        y3 = y3.real
    
    return y1 - t, y2 - t, y3 - t
