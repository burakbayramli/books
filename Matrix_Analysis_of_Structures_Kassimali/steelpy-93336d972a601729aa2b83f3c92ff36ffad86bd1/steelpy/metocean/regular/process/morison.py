#
# Copyright (c) 2009-2022 steelpy
#
# Python stdlib imports
from array import array
from dataclasses import dataclass
import math
from typing import NamedTuple, Tuple, Union, List, Dict

# package imports

@dataclass
class TubMember:
    
    D: float
    Z:float
    Lm:float
    nelev:float
    #
    @property
    def elev(self):
        """ """
        Lrange = self.Lm
        return [self.Z + (z/self.nelev) * Lrange 
                for z in range(self.nelev)]
    #
    def CdCm(self, Hts:float):
        """ """
        Cd = [0.65 if z > Hts else 1.05 
              for z in range(self.nelev)]
        #
        Cm = [1.60 if z > Hts else 1.20 
              for z in range(self.nelev)]
        return Cd, Cm
    #
    def MG(self, Hts:float):
        """Marine Growth """
        
        rm = [50 if z < -25 else 0 if z > Hts else 90  
              for z in range(self.nelev)]
        
        return rm
    #
    def Vc(self):
        """ Current Velocity"""
        pass
    #
    def Dt(self, rm:List):
        """Hydrodynamic diametre"""
        Dt = [self.D + 2*rm[z]
              for z in range(self.nelev)]
        #
        pi = math.pi
        At = [d**2 * pi/4 for d in Dt]
        return Dt, At
    #
    def vel_components(self):
        """Components of velocity local to the member"""
        #
        def cx(phi1, theta1):
            return math.sin(phi1) * math.cos(theta1)
        #
        def cy(phi1):
            return math.cos(phi1)
        #
        def cz(phi1, theta1):
            return math.sin(phi1) * math.sin(theta1)
        
    #
    def Un(self):
        """ """
        un = (self.Vc + velx - cx)