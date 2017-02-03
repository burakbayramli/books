class Complex:
    def __init__(self, real, imag=0.0):
        self.real = real
        self.imag = imag

    def __add__(self, other):
        return Complex(self.real + other.real,
                       self.imag + other.imag)

    def __sub__(self, other):
        return Complex(self.real - other.real,
                       self.imag - other.imag)

    def __mul__(self, other):
        return Complex(self.real*other.real - self.imag*other.imag,
                       self.imag*other.real + self.real*other.imag)

    def __div__(self, other):
        ar, ai, br, bi = self.real, self.imag, \
                         other.real, other.imag # short forms
        r = float(br**2 + bi**2)
        return Complex((ar*br+ai*bi)/r, (ai*br-ar*bi)/r)

    def __abs__(self):
        return sqrt(self.real**2 + self.imag**2)

    def __neg__(self):   # defines -c (c is Complex)
        return Complex(-self.real, -self.imag)

    def __eq__(self, other):
        return self.real == other.real and self.imag == other.imag

    def __ne__(self, other):
        return not self.__eq__(other)

    def __str__(self):
        return '(%g, %g)' % (self.real, self.imag)

    def __repr__(self):
        return 'Complex' + str(self)

    def __pow__(self, power):
        raise NotImplementedError\
              ('power operation is not yet impl. for Complex')

    def _illegal(self, op):
        print 'illegal operation "%s" for complex numbers' % op

    def __gt__(self, other):  self._illegal('>')

    def __ge__(self, other):  self._illegal('>=')

    def __lt__(self, other):  self._illegal('<')

    def __le__(self, other):  self._illegal('<=')

#    def __add__(self, other):
#        if isinstance(other, (float,int)):
#            other = Complex(other)
#        return Complex(self.real + other.real,
#                       self.imag + other.imag)
#
#    def __radd__(self, other):
#        return self.__add__(other)

    def __sub__(self, other):
        print 'in sub, self=%s, other=%s' % (self, other)
        if isinstance(other, (float,int)):
            other = Complex(other)
        return Complex(self.real - other.real,
                       self.imag - other.imag)

    def __rsub__(self, other):
        print 'in rsub, self=%s, other=%s' % (self, other)
        if isinstance(other, (float,int)):
            other = Complex(other)
        return other.__sub__(self)
