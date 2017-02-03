
# This class is just a first attempt - see below for a class for
# real computations with interval arithmetics.

class IntervalMath:
    def __init__(self, lower, upper):
        self.lo = float(lower)
        self.up = float(upper)

    def __add__(self, other):
        a, b, c, d = self.lo, self.up, other.lo, other.up
        return IntervalMath(a + c, b + d)

    def __sub__(self, other):
        a, b, c, d = self.lo, self.up, other.lo, other.up
        return IntervalMath(a - d, b - c)

    def __mul__(self, other):
        a, b, c, d = self.lo, self.up, other.lo, other.up
        return IntervalMath(min(a*c, a*d, b*c, b*d),
                            max(a*c, a*d, b*c, b*d))

    def __div__(self, other):
        a, b, c, d = self.lo, self.up, other.lo, other.up
        # [c,d] cannot contain zero:
        if c*d <= 0:
            raise ValueError\
                  ('Interval %s cannot be denominator because '\
                   'it contains zero')
        return IntervalMath(min(a/c, a/d, b/c, b/d),
                            max(a/c, a/d, b/c, b/d))

    def __str__(self):
        return '[%g, %g]' % (self.lo, self.up)


def _verify1():
    I = IntervalMath
    a = I(-3,-2)
    b = I(4,5)
    print 'a =', a
    print 'b =', b
    expr = 'a+b', 'a-b', 'a*b', 'a/b'
    for e in expr:
        print '%s =' % e, eval(e)

if __name__ == '__main__':
    print 'Testing the rough, first version of class IntervalMath:'
    _verify1()
    print 'Testing a problematic code:'
    v0 = IntervalMath(4,5)
    t = 0.6
    g = 9.81
    try:
        y = v0*t - 0.5*g*t**2
    except AttributeError:
        print 'we lack __rmul__ ...'
    

class IntervalMath:
    """
    Class for computing with quantities specified by intervals
    (in which the true value of the quantity is guaranteed to lie).

    Examples:
    >>> from IntervalMath import IntervalMath as I
    >>> v0 = I(4, 6)
    >>> t = I(0.6, 0.7)
    >>> g = 9.81
    >>> y = v0*t - 0.5*t**2
    >>> print 'v0 =', v0, ', t =', t, ', y =', y
    v0 = [4, 6] , t = [0.6, 0.7] , y = [2.155, 4.02]
    >>> print float(y)  # mean value of y interval
    3.0875
    >>> # computing with mean values:
    >>> v0 = float(v0);  t = float(t);  y = v0*t - 0.5*t**2
    >>> print y
    3.03875

    >>> R = I(6*0.9, 6*1.1)   # 20 % error
    >>> V = (4./3)*pi*R**3
    >>> V
    IntervalMath(659.584, 1204.26)
    >>> print V
    [659.584, 1204.26]
    >>> print float(V)
    931.922044761
    >>> R = float(R)
    >>> V = (4./3)*pi*R**3
    >>> print V
    904.778684234
    """
    def __init__(self, lower, upper):
        """Create interval from lower and upper limits."""
        if lower <= upper:
            self.lo = float(lower)
            self.up = float(upper)
        else:
            raise ValueError\
                  ('lower limit %s must be smaller than upper limit %s' %
                   (lower, upper))

    @staticmethod
    def n2i(n):
        """Turn number or interval n into interval."""
        if isinstance(n, (int, float)):
            return IntervalMath(n, n)
        elif isinstance(n, IntervalMath):
            return n
        else:
            raise TypeError\
                  ('operand %s %s must be number of '\
                   'interval' % (n, type(n)))
        
    def _limits(self, other):
        other = IntervalMath.n2i(other)
        return self.lo, self.up, other.lo, other.up
        
    def __add__(self, other):
        a, b, c, d = self._limits(other)
        return IntervalMath(a + c, b + d)

    def __sub__(self, other):
        a, b, c, d = self._limits(other)
        return IntervalMath(a - d, b - c)

    def __mul__(self, other):
        a, b, c, d = self._limits(other)
        return IntervalMath(min(a*c, a*d, b*c, b*d),
                            max(a*c, a*d, b*c, b*d))

    def __div__(self, other):
        a, b, c, d = self._limits(other)
        # [c,d] cannot contain zero:
        if c*d <= 0:
            raise ValueError\
                  ('Interval %s cannot be denominator because '\
                  'it contains zero')
        return IntervalMath(min(a/c, a/d, b/c, b/d),
                            max(a/c, a/d, b/c, b/d))

    def __radd__(self, other):
        other = IntervalMath.n2i(other)
        return other + self

    def __rsub__(self, other):
        other = IntervalMath.n2i(other)        
        return other - self

    def __rmul__(self, other):  
        other = IntervalMath.n2i(other)
        return other*self

    def __rdiv__(self, other):
        other = IntervalMath.n2i(other)
        return other/self

    def __pow__(self, exponent):
        if isinstance(exponent, int):
            p = 1
            if exponent > 0:
                for i in range(exponent):
                    p = p*self
            elif exponent < 0:
                for i in range(-exponent):
                    p = p*self
                p = 1/p
            else:
                p = IntervalMath(1, 1)
            return p
        else:
            raise TypeError('exponent must int')

    def __eq__(self, other):
        return self.lo == other.lo and self.up == other.up

    def __neq__(self, other):
        return not self.__eq__(other)

    def __float__(self):
        return 0.5*(self.lo + self.up)

    def width_in_percent(self):
        """
        Return the width of the interval as percentage around the mean.
        >>> a = IntervalMath(10*0.9, 10*1.1)  # 10% to either side of 10
        >>> a.width_in_percent()
        20.0
        """
        m = float(self)
        w2 = m - self.lo
        p2 = w2/m*100
        return 2*p2

    def tolist(self):
        return [self.lo, self.up]
    
    def __str__(self):
        return '[%g, %g]' % (self.lo, self.up)

    def __repr__(self):
        return '%s(%g, %g)' % \
               (self.__class__.__name__, self.lo, self.up)


def _examples():
    I = IntervalMath

    v0 = I(4, 6)
    t = I(0.6, 0.7)
    g = 9.81
    y = v0*t - 0.5*t**2
    print 'v0 =', v0, ', t =', t, ', y =', y
    print 'mean y =', float(y)

    from math import pi
    R = I(6*0.9, 6*1.1)   # 20 % error
    V = (4./3)*pi*R**3
    print 'R =', R, 'V =', V
    print 'R width:', R.width_in_percent()
    print 'V width:', V.width_in_percent()
    

if __name__ == '__main__':
    print '\n\n'
    _examples()
