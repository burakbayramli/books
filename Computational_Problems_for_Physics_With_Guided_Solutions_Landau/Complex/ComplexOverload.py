""" From "A SURVEY OF COMPUTATIONAL PHYSICS", Python eBook Version
   by RH Landau, MJ Paez, and CC Bordeianu
   Copyright Princeton University Press, Princeton, 2012; Book  Copyright R Landau, 
   Oregon State Unv, MJ Paez, Univ Antioquia, C Bordeianu, Univ Bucharest, 2012.
   Support by National Science Foundation , Oregon State Univ, Microsoft Corp"""  

# ComplexOverload.py: performs complex arithmetic via operator overload

class Complex1:
    
    "Another class to add subtract and multiply complex numbers"
    "Uses overload of operators + - and * "
    
    def __init__(self,x,y):                           # class constructor
        self.re=x                                      # assing real part
        self.im=y                                      # assing imag part
        # print " en init ", self.re, "  ", self.im
        
    def __add__(self,other):                      # extend predefined add
       return Complex1(self.re + other.re, self.im + other.im)
    
    def __sub__(self,other):                 # extend predefined subtract
        return Complex1(self.re - other.re, self.im - other.im)
    
    def __mul__(self, other):                    # extend predefined mult
        return Complex1(self.re*other.re -self.im*other.im,
                       self.re*other.im + self.im*other.re)
    
    def __repr__ (self):    
        return '(%f , %f) ' %(self.re, self.im)
    
print('\n Operations with two complex numbers via operator overload\n')
z1 = Complex1(2.0,3.0)                             # first complex number
print('z1=', z1)
z2= Complex1(4.0,6.0)                             # other complex number
print("z2 = ",z2, "\n")
print("z1 + z2=", z1+z2)                              # use __add__ for +
print("z1 * z2=", z1*z2)                              # use __mul__ for *
print('z1 - z2=', z1-z2)                              # use __sub__ for -
print('z1 * z2=', z1*z2) 
print("Enter and return any character to quit")
s = raw_input()

