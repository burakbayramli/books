""" From "A SURVEY OF COMPUTATIONAL PHYSICS", Python eBook Version
   by RH Landau, MJ Paez, and CC Bordeianu
   Copyright Princeton University Press, Princeton, 2012; Book  Copyright R Landau, 
   Oregon State Unv, MJ Paez, Univ Antioquia, C Bordeianu, Univ Bucharest, 2012.
   Support by National Science Foundation , Oregon State Univ, Microsoft Corp"""  

''' KomplexPolar: complex numbes via modulus
    Cartesian/polar complex via package
    "typec = 0" -> polar representation, else: rectangular'''

import math

class Komplex:
    
    def __init__(self, x, y, typec):               # Kopmplex constructor
        if typec == 0:                                 # 0: for polar rep
            self.mod = x                                      # magnitude
            self.theta = y                                  # polar angle
        else:                                # rectangular representation
           self.re = x               
           self.im = y
           
    def getRe(self):                       # return rectangular real part
        return self.mod*math.cos(self.theta)
    
    def getIm(self):                       # return rectangular imag part
        return self.mod*math.sin(self.theta)
    
    def setRe(self):                      # returns rectangular real part
        re = self.mod*math.cos(self.theta)
        return re
    
    def setIm(self):                       # return rectangular imag part
        im = self.mod*math.sin(self.theta)   
        return im
    
    def add(self, other, typec):                # add two complex numbers
        if typec == 0:                             # polar representation
            tempMod = math.sqrt(self.mod*self.mod + other.mod*other.mod 
            + 2*self.mod*other.mod*math.cos(self.theta - other.theta) )
            angtheta  = math.atan2(self.mod*math.sin(self.theta) 
            +  other.mod*math.sin(other.theta), self.mod*math.cos(self.theta)
                                      +  other.mod*math.cos(other.theta) )
            return Komplex(tempMod, angtheta, 0) # sum in polar coord.
        else:                                  # sum in rectangular coord
            return Komplex(self.re  +  other.re, self.im  +  other.im, 1)
        
    def sub(self, other, typec):                    # now for subtraction
       if typec == 0:
          tempmod = math.sqrt(self.mod*self.mod  +   other.mod*other.mod  - 
          2*self.mod*other.mod*(math.cos(self.theta)*math.cos(other.theta)
           +  math.sin(self.theta)*math.sin(other.theta) ))
          y = self.mod*math.sin(self.theta) - other.mod*math.sin(other.theta)
          x = self.mod*math.cos(self.theta) - other.mod*math.cos(other.theta)
          angtheta =  math.atan2(y, x)
          return Komplex(tempmod, angtheta, 0)
       else:
          return Komplex(self.re  -  other.re, self.im  -  other.im, 1)
        
    def  div(self, other, typec):                # complex division z1/z2
         if typec == 0:
            return Komplex(self.mod/other.mod, self.theta-other.theta,0)
         else:
             numre = self.re*other.re  + self.im*other.im
             deno = other.re*other.re  +  other.im*other.im
             numim = self.im*other.re  - self.re*other.im
             return Komplex(numre/deno, numim/deno, 1)
            
    def mult(self, other, typec):                # complex multiplication
        if typec == 0:  
           return Komplex(self.mod*other.mod, self.theta+other.theta, 0)
        else:
            return Komplex(self.re*other.re - self.im*other.im, 
                   self.re*other.im  +  self.im*other.re, 1)
        
    def conj(self, typec):                                 # complex conj
        if typec == 0:
            self.mod = self.mod
            self.theta =  - self.theta
            return(self.mod, self.theta, 0)
        else:
            return Komplex(self.re,  - self.im, 1)
