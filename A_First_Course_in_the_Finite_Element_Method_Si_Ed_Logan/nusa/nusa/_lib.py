# ***********************************
#  Author: Pedro Jorge De Los Santos     
#  E-mail: delossantosmfq@gmail.com 
#  Blog: numython.github.io
#  License: MIT License
# ***********************************

class Material(object):
    def __init__(self,name,**kwargs):
        """
        
        Parameters
        ----------
        
        name :    Name of material
        
        
        **kwargs:
        
        E       :    Elastic modulus
        nu      :    Poisson ratio
        density :    Density
        
        """
        self.name = name
        self.__dict__.update(kwargs) # add all properties
    
    def __str__(self):
        return self.name 
        

class Section(object):
    def __init__(self):
        pass
        
    
class RectangularSection(Section):
    def __init__(self,width,height):
        Section.__init__(self)
        self.w = width
        self.h = height
    
    @property
    def A(self):
        return self.w*self.h
        
    @property
    def I(self):
        return (1/12)*self.w*self.h**3
        
    
class CircularSection(Section):
    def __init__(self,r):
        Section.__init__(self)
        self.r = r
    
    @property
    def A(self):
        from math import pi
        return (pi*self.r**2)
        
    @property
    def I(self):
        from math import pi
        return (pi/4)*self.r**4
