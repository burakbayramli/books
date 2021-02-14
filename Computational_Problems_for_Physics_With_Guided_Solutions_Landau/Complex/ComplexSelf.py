""" From "A SURVEY OF COMPUTATIONAL PHYSICS", Python eBook Version
   by RH Landau, MJ Paez, and CC Bordeianu
   Copyright Princeton University Press, Princeton, 2012; Book  Copyright R Landau, 
   Oregon State Unv, MJ Paez, Univ Antioquia, C Bordeianu, Univ Bucharest, 2012.
   Support by National Science Foundation , Oregon State Univ, Microsoft Corp"""  

# ComplexSelf.py: creates Complex class using self & other, no dummy, z's
from sys import version
if int(version[0])>2:   # raw_input deprecated in Python 3
    raw_input=input   
class Complex:
    
    def __init__(self, x, y):                         # class constructor
        self.re = x                                    # assign real part
        self.im = y                                    # assign imag part
        
    def addt(self, other):                           # adds self to other
       return Complex(self.re + other.re, self.im + other.im)
    
    def subt(self, other):                          # subtract self-other
        return Complex(self.re - other.re, self.im- other.im)
    
    def mult(self, other):                      # multiplies self * other
        return Complex(self.re*other.re -self.im*other.im,
                      self.re*other.im+self.im*other.re)  
    def __repr__ (self):                  # convert z to string for print
        return '(%f, %f) ' %(self.re, self.im)
    
print('Operations with two complex numbers\n')

z1 = Complex(2.0, 3.0)                             # first complex number
print('z1 =', z1)
z2 = Complex(4.0, 6.0)                                # other complex one
print("z2 =",z2)
z3 = Complex.addt(z1,z2)                                    # add z1 + z2
print("z1 + z2= ",z3)         
print('z1 - z2=', Complex.subt(z1,z2))    
print('z1 * z2=', Complex.mult(z1,z2)) 
print("Enter and return any character to quit")
s = raw_input()
