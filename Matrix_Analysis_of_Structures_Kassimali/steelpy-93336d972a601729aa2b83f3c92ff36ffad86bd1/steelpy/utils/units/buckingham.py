#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
This module implement:

- dimensional analysis
- formula validation
- units conversion
- error propagation

Developed by Massimo Di Pierro
https://github.com/mdipierro/buckingham

Projects gets it name from
http://en.wikipedia.org/wiki/Buckingham_%CF%80_theorem Buckingham
http://en.wikipedia.org/wiki/Buckingham_π_theorem

This version works with 3.x only

**Note
N/m^2  without conversion result is: g/(m*s^2)
"""

import re
import math
from typing import Dict, List, Tuple

#
#
__all__ : List[str] = ['Number', 'allunits', 'pm', 'exp', 'log', 'sin', 'cos']

class Fraction:
    @staticmethod
    def gcd(x:int, y:int) -> int:
        """
        """
        g = y
        while x > 0:
            g = x
            x = y % x
            y = g
        return g
    #
    def __init__(self, x:int, y:str ='1') -> None:
        """
        """
        z = (str(x)+'/'+str(y)).split('/')[:2]
        self.n, self.d = int(float(z[0])),int(float(z[1]))
        
        if self.d < 0:
            self.n,self.d = -self.n,-self.d
        m = self.n and self.gcd(abs(self.n),abs(self.d)) or 1
        self.n, self.d = self.n/m, self.d/m
    #
    def __add__(self,other):
        if not isinstance(other,Fraction):
            other=Fraction(other)
        return Fraction(self.n*other.d+other.n*self.d,self.d*other.d)
    #
    def __radd__(self,other):
        return self+other
    #
    def __sub__(self,other):
        if not isinstance(other,Fraction):
            other=Fraction(other)
        return Fraction(self.n*other.d-other.n*self.d,self.d*other.d)
    #
    def __mul__(self,other):
        if not isinstance(other,Fraction):
            other=Fraction(other)
        return Fraction(self.n*other.n,self.d*other.d)
    #
    def __rmul__(self,other):
        return self*other
    #
    def __truediv__(self,other):
        if not isinstance(other,Fraction):
            other=Fraction(other)
        return Fraction(self.n/other.d,self.d/other.n)
    #
    def __eq__(self,other):
        if not isinstance(other,Fraction):
            other=Fraction(other)
        return self.n*other.d==self.d*other.n
    #
    def __str__(self):
        if self.d == 0: 
            return '0'
        if self.d == 1: 
            return str(self.n)
        return '{:}/{:}'.format(self.n,self.d)
    #
    def __float__(self):
        if self.n == 0: 
            return 0.0
        return float(self.n)/self.d
    #
    def __pow__(self,other):
        """
        """
        #print('-->')
        #if not isinstance(other,Fraction):
        #    other=Fraction(other)
        #
        #sign = math.copysign(x, y)
        return Fraction(math.copysign(math.pow(abs(self.n), other), self.n), 
                        math.copysign(math.pow(abs(self.d), other), self.d))
#
UNITS = {
    'N': (1.0,0,0,0,0,0,0), # none
    'L': (1.0,1,0,0,0,0,0), # metre
    'T': (1.0,0,1,0,0,0,0), # second
    'M': (1.0,0,0,1,0,0,0), # gram
    'A': (1.0,0,0,0,1,0,0), # ampere
    'K': (1.0,0,0,0,0,1,0), # kelvin
    'R': (1.0,0,0,0,0,0,1), # radian (angle)
    'none': (1.0,0,0,0,0,0,0), # none
    'pure': (1.0,0,0,0,0,0,0), # none
    'metre':  (1.0,1,0,0,0,0,0), # metre
    'second': (1.0,0,1,0,0,0,0), # second
    'gram':   (1.0,0,0,1,0,0,0), # gram
    'ampere': (1.0,0,0,0,1,0,0), # ampere
    'kelvin': (1.0,0,0,0,0,1,0), # kelvin
    'radian': (1.0,0,0,0,0,0,1), # radian
    #
    'degree': (math.pi/180,0,0,0,0,0,1), # angle
    #'currency': (1.0,0,0,0,0,0,1), # currency
    #'coulomb': (1.0,0,1,0,1,0,0), # one ampere x 1 second
    'angstrom': (10**-10,1,0,0,0,0,0),
    'atm': (101325000.00,-1,-2,1,0,0,0),
    'au':  (149597870691.0,1,0,0,0,0,0),
    'bar': (100000000.0,-1,-2,1,0,0,0),
    'coulomb':(1.0,0,1,0,1,0,0), # one ampere x 1 second
    'day':(86400.0,0,1,0,0,0,0),
    'ev':(1.602176487e-16,2,-2,1,0,0,0),
    'eV':(1.602176487e-16,2,-2,1,0,0,0),
    'farad':(1000.0,-2,4,-1,2,0,0),
    'faraday':(9.64853399e4,0,1,0,1,0,0),    
    'foot':(381./1250.0,1,0,0,0,0,0),
    'hour':(3600.0,0,1,0,0,0,0),
    'henry':(1000.0,2,-2,1,-2,0,0),
    'hz':(1.0,0,-1,0,0,0,0),
    'inch':(127/5000.0,1,0,0,0,0,0),
    'joule':(1000.0,2,-2,1,0,0,0),
    'calorie':(4186.8,2,-2,1,0,0,0),
    'lightyear':(9460730472580800.0,1,0,0,0,0,0),
    'litre':(0.001,3,0,0,0,0,0),
    'mho':(0.001,-2,3,-1,2,0,0),
    'mile':(201168./125.0,1,0,0,0,0,0),
    'minute':(60.0,0,1,0,0,0,0),
    'mmhg':(133322.387415,-1,-2,1,0,0,0),
    'newton':(1000.0,1,-2,1,0,0,0),
    'ohm':(1000.0,2,-3,1,-2,0,0),
    'pascal':(1000.0,-1,-2,1,0,0,0),
    'lbf':(4448.2216152605,1,-2,1,0,0,0), # pound-force
    'pound':(453.59237,0,0,1,0,0,0),      # pound-mass
    'point':(127/360000.0,1,0,0,0,0,0),   # typography
    'psi':(6894757.29316836,-1,-2,1,0,0,0),
    'quart':((473176473./125000000000.0)/4.0,3,0,0,0,0,0),
    'siemens':(0.001,-2,3,-1,2,0,0),
    'volt':(1000.0,2,-3,1,-1,0,0),
    'watt':(1000.0,2,-3,1,0,0,0),
    'weber':(1000.0,2,-2,1,-2,0,0),
    'yard':(1143./1250.0,1,0,0,0,0,0),
    'year':(3944615652./125.0,0,1,0,0,0,0),
    'fermi':(10.0**-15,1,0,0,0,0,0),
}
#
def extend_units(units:Dict):
    scales:List[tuple] =  [
                ('yocto',10.0**-24),
                ('zepto',10.0**-21),
                ('atto',10.0**-18),
                ('femto',10.0**-15),
                ('pico',10.0**-12),
                ('nano',10.0**-9),
                ('micro',10.0**-6),
                ('milli',10.0**-3),
                ('centi',10.0**-2),
                ('deci',0.1),
                ('deca',10.0),
                ('hecto',10.0**2),
                ('kilo',10.0**3),
                ('mega',10.0**6),
                ('giga',10.0**9),
                ('tera',10.0**12),
                ('peta',10.0**15),
                ('exa',10.0**18),
                ('zetta',10.0**21),
                ('votta',10.0**24),
                ]
    keys = [key for key in units]
    for name, conversion in scales:
        for key in keys:
            if not key in ('N','M','T','M','A','K','D','none','None'):
                v = units[key]
                units[name+key] = (v[0]*conversion,v[1],v[2],v[3],v[4],v[5],v[6])
#
extend_units(UNITS)
#
def buckingham(units:str, d:Dict):
    """
    """
    items = units.split('/')
    numerator = items[0].split('*')
    denominator = items[1:]
    power = Fraction(0)
    
    for item in numerator:
        x = item.split('^')
        if not x[0] in d:
            raise Exception("Unknown units")
        if len(x) == 1: 
            power += d[x[0]]
        else: 
            power += d[x[0]]*Fraction(x[1])
    
    for item in denominator:
        x = item.split('^')
        if not x[0] in d:
            raise Exception("Unknown units")
        if len(x) == 1: 
            power -= d[x[0]]
        else: 
            power -= d[x[0]]*Fraction(x[1])
    
    return power
#
def int_safe(x):
    if not x: 
        return 0
    x = math.log10(x)
    i = int(x)
    if x == i or x > 0: 
        return i
    return i-1
#
#
def temperature_in(value:float, dims:str):
    """
    """
    _value = float(value)
    if re.match(r"\bfahrenheit\b", str(dims), re.IGNORECASE):
        dims = dims.replace('fahrenheit', 'kelvin')
        _value = (_value + 459.67) * 5/9
    
    elif re.match(r"\bcelsius\b", str(dims), re.IGNORECASE):
        dims = dims.replace('celsius', 'kelvin')        
        _value = (_value + 273.15)
    
    elif re.match(r"\brankine\b", str(dims), re.IGNORECASE):
        dims = dims.replace('rankine', 'kelvin')        
        _value *= 5/9
    
    return _value, dims
#
def temperature_out(value:float, dims:str):
    """
    """
    _value = value
    if re.match(r"\bfahrenheit\b", str(dims), re.IGNORECASE):
        dims = dims.replace('fahrenheit', 'kelvin')
        _value = (_value * 9/5 - 459.67)
    
    elif re.match(r"\bcelsius\b", str(dims), re.IGNORECASE):
        dims = dims.replace('celsius', 'kelvin')
        _value = (_value - 273.15)
    
    elif re.match(r"\rankine\b", str(dims), re.IGNORECASE):
        dims = dims.replace('rankine', 'kelvin')
        _value *= 9/5
    
    return _value, dims
#
#
class Number:
    """
    Example of dimensional analysis and conversions

    >>> a = Number(10,dims = 'metre/second')
    >>> b = Number(2,dims = 'yard/minute')
    >>> c = a + b
    >>> print(c.convert('kilometre/hour'))
    (36.109728 ± 0)

    >>> Number(1,dims = 'joule').convert('eV').value
    6.241509647120418e+18

    >>> print("%s %s %s" % (c.value, c.error, c.units()))
    10.03048 0.0 metre*second^-1

    Without conversion results are always in:
    metres, seconds, grams, ampere, kelvin, currency (and combination)

    The error is the standard deviation assuming the true value
    is Normal distributed

    Error propagation assumes independence

    >>> a = Number(1,dims = 'decimetre^3')
    >>> b = Number(2,dims = 'litre')
    >>> c = Number(5,dims = 'gram/centimetre^3')
    >>> d = (a + b)*c
    >>> print(d.convert('kilogram'))
    (15.000000 ± 0)

    Simplified syntax

    >>> globals().update(allunits())
    >>> length = (4 + pm(0.5)) * metre # (4±0.5)m
    >>> velocity = 5 * metre/second
    >>> time = length/velocity
    >>> print(time)
    (8.00 ± 1.00)/10

    Example of formula validation

    >>> a = Number(10,dims = 'metre/second')
    >>> b = Number(2,dims = 'yard^3')
    >>> c = a + b
    Traceback (most recent call last):
    ...
    RuntimeError: Incompatible Dimensions

    Examples of error propagation:

    >>> a = Number(10,error = 2,dims = 'metre/second') # (10±2)m/s
    >>> b = Number(5,error = 1,dims = 'hour')          # (5±1)h
    >>> c = a*b
    >>> print(c.convert('kilometre'))
    (1.800 ± 0.509)x10^2
    >>> print(c.convert('lightyear'))
    (1.903 ± 0.538)/10^11
    >>> print(c.convert('kilometre').value)
    180.0
    >>> print(c.convert('kilometre').error)
    50.91168824543142

    Examples of more complex formulas

    >>> c = a**4/(7*b)
    >>> print('%s %s' % (c, c.units()))
    (7.94 ± 6.54)/10^2 metre^4*second^-5

    For pure numbers sin, cos, exp, log are also defined
    You can use a.is_pure() to check if a number is pure.

    (last result is implicitly in metre**4/second**5)

    Latex Output


    """
    regex = re.compile('([a-zA-Z]+)(\^\-?\d+(/\d+)?)?(\*([a-zA-Z]+)(\^\-?\d+(/\d+)?)?)*(\/([a-zA-Z]+)(\^\-?\d+(/\d+)?)?)*')
    c_n = dict((key,value[0]) for key,value in UNITS.items())
    c_l = dict((key,value[1]) for key,value in UNITS.items())
    c_t = dict((key,value[2]) for key,value in UNITS.items())
    c_m = dict((key,value[3]) for key,value in UNITS.items())
    c_a = dict((key,value[4]) for key,value in UNITS.items())
    c_k = dict((key,value[5]) for key,value in UNITS.items())
    c_r = dict((key,value[6]) for key,value in UNITS.items())
    #
    def __init__(self, value:float, error:float = 0.0, dims:str = 'N'):
        """
        """
        if not isinstance(error,(int, float)):
            raise Exception("second argument must be the error")
        
        if isinstance(dims, tuple):
            dims = 'N*L^{:}*T^{:}*M^{:}*A^{:}*K^{:}*R^{:}'.format(*dims)
        dims = dims.replace(' ','')
        
        if not self.regex.match(dims):
            raise SyntaxError('Invalid Dims {:}'.format(dims))
        
        # check temperature units
        self.input_dims = dims
        value, dims = temperature_in(value, dims)
        
        n = eval(dims.replace('^','**'),self.c_n) or 1        
        l = buckingham(dims,self.c_l)
        t = buckingham(dims,self.c_t)
        m = buckingham(dims,self.c_m)
        a = buckingham(dims,self.c_a)
        k = buckingham(dims,self.c_k)
        r = buckingham(dims,self.c_r)
        self.value = float(value) * n
        self.error = float(error) * n
        self.dims = (l,t,m,a,k,r)
    #
    def __add__(self,other):
        """
        >>> a = Number(2,1)
        >>> b = Number(3,2)
        >>> print(a+b)
        5.00 ± 2.24
        """
        if not isinstance(other, Number):
            other=Number(other,0,self.dims)
        elif self.dims != other.dims:
            raise RuntimeError("Incompatible Dimensions")
        
        a, da = self.value,self.error
        b, db = other.value,other.error
        c = a + b
        if not self is other:
            dc = math.sqrt(da**2+db**2)
        else: dc = 2.*da
        
        return Number(c,dc,self.dims)
    #
    def __radd__(self,other):
        return self + other
    #
    def __sub__(self,other):
        """
        >>> a = Number(2,1)
        >>> b = Number(3,2)
        >>> print(a-b)
        -1.00 ± 2.24
        """
        if not isinstance(other,Number):
            other=Number(other,0,self.dims)
        elif self.dims != other.dims:
            raise RuntimeError("Incompatible Dimentions")
        a, da = self.value,self.error
        b, db = other.value,other.error
        c = a - b
        if not self is other:
            dc = math.sqrt(da**2+db**2)
        else: 
            dc = 0
        return Number(c,dc,self.dims)
    #
    def __lt__(self,other):
        """
        >>> print(Number(2,1) < Number(2,1))
        """
        if not isinstance(other,Number):
            other = Number(other)
        a,da = self.value,self.error
        b,db = other.value,other.error
        return a < b
    #
    def __abs__(self):
        """
        """
        a,da = self.value,self.error
        c = abs(a)
        return Number(c,da,self.dims)
    #
    def __gt__(self,other):
        """
        """
        if not isinstance(other,Number):
            other = Number(other)
        a,da = self.value,self.error
        b,db = other.value,other.error
        return a > b
    #
    def __le__(self,other):
        """
        >>> print(Number(2,1) <= Number(2,1))
        """
        if not isinstance(other,Number):
            other = Number(other)
        a,da = self.value,self.error
        b,db = other.value,other.error
        return a <= b  
    
    def __ge__(self,other):
        """
        """
        if not isinstance(other,Number):
            other = Number(other)
        a,da = self.value,self.error
        b,db = other.value,other.error
        return a >= b    
    #
    def __rsub__(self,other):
        x = self - other
        x.value *= -1
        return x
    #
    def __mul__(self,other):
        """
        >>> print(Number(2,1) * 1)
        2.00 ± 1.00
        """
        if not isinstance(other,Number):
            other = Number(other)
        a,da = self.value,self.error
        b,db = other.value,other.error
        c = a*b
        if not self is other:
            #dc = abs(c)*math.sqrt((da/a)**2+(db/b)**2)
            dc = math.sqrt((da*b)**2 + (db*a)**2)
        else: 
            dc = 2.0*da*a
        dims = tuple(self.dims[i] + other.dims[i] for i in range(6))
        return Number(c,dc,dims)
    #
    def __rmul__(self,other):
        """
        >>> print(1 * Number(2,1))
        2.00 ± 1.00
        """
        return self*other
    #
    def __truediv__(self,other):
        """
        >>> print(Number(2,1) / 1)
        2.00 ± 1.00
        """
        if not isinstance(other,Number):
            other = Number(other)
        a,da = self.value,self.error
        b,db = other.value,other.error
        c = a/b
        if not self is other:
            #try:
            #dc = abs(c)*math.sqrt((da/a)**2+(db/b)**2)
            dc = math.sqrt((da/b)**2 + ((a*db)/(b*b))**2)
            #except ZeroDivisionError: dc = 0
        else: 
            dc = 0.0
        dims = tuple(self.dims[i] - other.dims[i] for i in range(6))
        return Number(c,dc,dims)
    #
    def __rtruediv__(self,other):
        """
        >>> print(1 / Number(2,1))
        (5.00 ± 2.50)/10
        """
        if not isinstance(other, Number):
            other = Number(other)
        return other/self
    #
    def __pow__(self,other):
        """
        """
        if not isinstance(other,Number):
            other = Number(other)
        elif not other.is_pure():
            raise RuntimeError("Incompatible Dimentions")
        a, da = self.value,self.error
        b, db = other.value,other.error
        #
        if other.value < 1.0:
            dims = tuple(self.dims[i] ** other.value for i in range(6))
        else:
            dims = tuple(self.dims[i] * other.value for i in range(6))
        #
        c = a**b
        if not self is other:
            try:
                dc = abs(c)*math.sqrt((da*b/a)**2 + (db*math.log(abs(a)))**2)
                #dc = abs(c)*math.sqrt((da*b/a)**2 + (db*math.log(a))**2)
            except ZeroDivisionError:
                dc = 0
        else:
            dc = abs(c)*(da*abs(b/a) + db*math.log(a))
        return Number(c, dc, dims)
    #
    def convert(self,dims):
        """
        """
        # check temperature units
        self.value, dims = temperature_out(self.value, dims)
        other = Number(1.0, 0, dims)
        
        if self.dims != other.dims:
            raise RuntimeError("Incompatible Dimensions")
        return self/other
    #
    def as_string(self, decimals:int = 2):
        value, error, dims = self.value, self.error, self.dims
        if error:
            n = int_safe(abs(value))
            m = int_safe(error)
            value = value/(10**n)
            error = error/(10**n)
            i = str(max(n-m+2,decimals))
        else:
            return ("({:f} ± 0)").format(value)
        if n > 1:
            a = "({:{i}f} ± {:{i}f})x10^{:}".format(value,error,n,i='.'+i)
        elif n > 0:
            a = "({:{i}f} ± {:{i}f})x10".format(value,error,i='.'+i)
        elif n < -1:
            a = "({:{i}f} ± {:{i}f})/10^{:}".format(value,error,-n,i='.'+i)
        elif n < 0:
            a = "({:{i}f} ± {:{i}f})/10".format(value,error,i='.'+i)
        else:
            a = "{:{i}f} ± {:{i}f}".format(value,error,i='.'+i)
        return a
    #
    def as_latex(self,decimals:int = 2):
        value,error,dims = self.value,self.error,self.dims
        if error:
            n = int_safe(abs(value))
            m = int_safe(error)
            value = value/(10**n)
            error = error/(10**n)
            i = str(max(n-m+2,decimals))
        else:
            return ("({:f} \\pm 0)").format(value)
        if n > 1:
            a = "({:{i}f} \\pm {:{i}f})\\times 10^{{{:}}}".format(value,error,n,i='.'+i)
        elif n > 0:
            a = "({:{i}f} \\pm {:{i}f})\\times 10".format(value,error,i='.'+i)
        elif n < -1:
            a = "({:{i}f} \\pm {:{i}f})\\times 10^{{{:}}}".format(value,error,-n,i='.'+i)
        elif n < 0:
            a = "({:{i}f} \\pm {:{i}f})\\times 10^{-1}".format(value,error,-n,i='.'+i)
        else:
            a = "{:{i}f} \\pm {:{i}f}".format(value,error,i='.'+i)
        return a
    #
    def __str__(self):
        #if self.units() != 'none':
        #    value = self.convert(self.input_dims)
        #    return "{:} {:}".format(value, self.input_dims)
        return self.as_string()
    #
    def is_pure(self):
        return sum(x*x for x in self.dims) == 0
    #
    def purify(self, force:bool = False):
        if not force and not self.is_pure():
            raise RuntimeError("Try purify(force = True)")
        return Number(self.value, self.error, (0,0,0,0,0))
    #
    def units(self):
        if self.is_pure(): 
            return 'none'
        
        def fmt(a,n):
            if float(n) == 0: 
                return ''
            elif float(n) == 1: 
                return '*{:}'.format(a)
            else: 
                return '*{:}^{:1.0f}'.format(a,float(n))
        
        r = ''
        r += fmt('metre',self.dims[0])
        r += fmt('second',self.dims[1])
        r += fmt('gram',self.dims[2])
        r += fmt('ampere',self.dims[3])
        r += fmt('kelvin',self.dims[4])
        r += fmt('radian',self.dims[5])
        return r[1:]
    #
    def guess(self):
        """ """
        #raise RuntimeError("this function is a work in progress...")
        #raise RuntimeError("this function is a work in progress...")
    
        if self.is_pure(): 
            return 'pure'
        
        options={
            'length': (1,0,0,0,0,0),
            'area': (2,0,0,0,0,0),
            'volume': (3,0,0,0,0,0),
            'speed': (1,-1,0,0,0,0),
            'time': (0,1,0,0,0,0),
            'mass': (0,0,1,0,0,0),
            'current': (0,0,0,1,0,0),
            'charge': (0,0,1,1,0,0),
            'temperature': (0,0,0,0,1,0),
            'currency': (0,0,0,0,0,1),
            'pressure': (-1,-2,1,0,0,0),
            'energy':(2,-2,1,0,0,0),
            'frequency':(0,-1,0,0,0,0),
            'resistance':(2,-3,1,-2,0,0),
            'acceptance':(-2,4,-1,2,0,0),
            'conductance':(-2,3,-1,2,0,0),
            'voltage':(2,-3,1,-1,0,0),
            'power':(1,-3,1,0,0,0),
            'magnetic_flux':(2,-2,1,-2,0,0)}
        
        dims=self.dims
        r=''
        while True:            
            n=1
            #### fix this... compute n
            y = [i/n for i in dims]
            if not sum(y): 
                break
            def d1(u,w):
                return sum((u[i]-float(j))*(u[i]-float(j)) for i,j in enumerate(w))
            def d2(u,w):
                return sum((u[i]+float(j))*(u[i]+float(j)) for i,j in enumerate(w))
            matches = [(d1(x,y),key,+1) for key,x in options.items()]
            matches += [(d2(x,y),key,-1) for key,x in options.items()]
            matches.sort()
            key,sign = matches[0][1],matches[0][2]
            r += '*%s' % key
            if n*sign != 1:
                r+='^%s' % (n*sign)
            if matches[0]==0: 
                break
            dims = [(y[i]-j*sign) for i,j in enumerate(options[key])]
        return r[1:]
    #
#
def sin(x):
    if not isinstance(x,Number):
        return math.sin(x)
    if not x.is_pure():
        raise RuntimeError("Incompatible Dimensions")
        #raise Exception("Incompatible Dimensions")
    return Number(sin(x.value),abs(cos(x.value))*x.error,x.dims)
#
def cos(x):
    if not isinstance(x,Number):
        return math.cos(x)
    if not x.is_pure():
        raise RuntimeError("Incompatible Dimensions")
        #raise Exception("Incompatible Dimensions")
    return Number(cos(x.value),abs(sin(x.value))*x.error,x.dims)
#
def exp(x):
    if not isinstance(x,Number):
        return math.exp(x)
    if not x.is_pure():
        raise RuntimeError("Incompatible Dimensions")
        #raise Exception("Incompatible Dimensions")
    c = exp(x.value)
    return Number(c,abs(c)*x.error,x.dims)
#
def log(x):
    if not isinstance(x,Number):
        return math.log(x)
    if not x.is_pure():
        raise RuntimeError("Incompatible Dimensions")
        #raise Exception("Incompatible Dimensions")
    c = log(x.value)
    return Number(c,abs(x.error/x.value),x.dims)
#
def allunits():
    return dict((key,Number(1,0,key)) for key in UNITS.keys())
#
def pm(error):
    return Number(0,error)
#
if __name__ == '__main__':
    #
    import doctest
    #
    print(Number(0, dims="metre/second").convert('kilometre/hour'))
    print(Number(4,2) * Number(7,3))
    print(Number(4,2) / Number(7,3))
    #
    print(Number(1, dims="atm").convert("pascal").value)
    #
    x=Number(6.,dims='inch')
    print ('inch ',x.value, x.units())
    print(x)
    #
    _test = Number(1, dims='newton')
    print('newton',_test.convert("newton").value, _test.value, _test.units())
    #
    _test = Number(1, dims='litre')
    print('quart',_test.convert("quart").value, _test.value, _test.units())    
    #
    #
    Newton = Number(1, dims="newton")
    kg = Number(1, dims="kilogram")
    sec = Number(1, dims="second")
    metre = Number(1, dims="metre")
    mm = Number(1, dims="millimetre")
    grav = Number(32.17405, dims="foot/second^2")
    Fy = 1 * kg / (mm * sec**2)
    #Fy = 340 * Newton / mm**2
    print(Fy.convert("pound/second^2/inch"))
    #
    print(Fy.convert("pound/second^2/inch") / grav)
    #
    #    
    oC = Number(1, dims = 'fahrenheit')
    print(oC.units(), oC.value)
    print(oC.convert('rankine'))
    #
    test = oC/sec
    print(test)
    print(test.convert('celsius/hour'))
    #
    doctest.testmod()
    #