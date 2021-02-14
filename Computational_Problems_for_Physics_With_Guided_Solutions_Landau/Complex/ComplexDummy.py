""" From "A SURVEY OF COMPUTATIONAL PHYSICS", Python eBook Version
   by RH Landau, MJ Paez, and CC Bordeianu
   Copyright Princeton University Press, Princeton, 2012; Book  Copyright R Landau, 
   Oregon State Unv, MJ Paez, Univ Antioquia, C Bordeianu, Univ Bucharest, 2012.
   Support by National Science Foundation , Oregon State Univ, Microsoft Corp"""  

# ComplexDummy.py: Performs complex algebra with dummy intermediary

class Complex: 
    def __init__(z, x, y):                            # class constructor
        z.re = x                            # assign real part of complex
        z.im = y                            # assign imag part of complex
        
    def addt(z1, z2):                                      # adds z1 + z2
       return Complex(z1.re + z2.re, z1.im + z2.im)
    
    def subt(z1, z2):                                   # subtracts z1-z2
        return Complex(z1.re - z2.re, z1.im- z2.im)
    
    def mult(z1, z2):                                  # multiplies z1*z2
        return Complex(z1.re*z2.re -z1.im*z2.im,
                      z1.re*z2.im+z1.im*z2.re)
    
    def __repr__ (z):                  # convert z to string for printing
        return '(%f, %f) ' %(z.re, z.im)
    
print('Operations with two complex numbers\n')

z1 = Complex(2.0, 3.0)                         # the first complex number
print ( 'z1 =', z1)
z2 = Complex(4.0, 6.0)                                # other complex one
print("z2 =" , z2)
z3 = Complex.addt(z1,z2)                                  # add operation
print("z1 + z2= ",z3)         
print('z1 - z2=', Complex.subt(z1,z2))            # prints both re and im
print('z1 * z2=', Complex.mult(z1,z2))                   # multiplication
print("Enter and return any character to quit") 
s = raw_input()
