#
# Copyright (c) 2019-2023 steelpy
#
# Python stdlib imports
from __future__ import annotations
#from array import array
#from collections.abc import Mapping
from dataclasses import dataclass
from math import pi, sin, cos, sinh, cosh, sqrt
from typing import NamedTuple
#import re
#
# package imports
from .table2B import BeamLoad
from .table2C import BeamBendingSupports
#
#
#
# ---------------------------------------------------------------
#
class e_values(NamedTuple):
    """ ei values """
    e0: float
    e1: float
    e2: float
    e3: float
    e4: float
    e5: float
    e6: float
    Zeta: float
    Lambda: float
    Eta: float
#
#
@dataclass
class TableBasic:
    
    def ei(self, x: float, I: float, k: float = 0):
        """Values of ei
        
        x : is measured from the left end
        k : Elastic foundation modulus 
        """
        try: # simple beam case
            1 / self.P
            # set values
            return e_values(e0=0, e1=1, e2=x,
                            e3=x**2/2.0, e4=x**3/6.0,
                            e5=x**5/24.0, e6=x**5/120.0,
                            Zeta=0, Lambda=0, Eta=0)
        except ZeroDivisionError:
            try:
                1 / k
                print('elastic foundation')
                raise NotImplementedError('soil to be included')
            
            except ZeroDivisionError:
                alpha2 = self.P / (self.E * I)
                alpha = sqrt(alpha2)
                # Axial force 
                if self.P < 0: # Tension
                    #print('tension?')
                    return e_values(e0= -1*alpha*sinh(alpha*x),
                                    e1= cosh(alpha*x),
                                    e2= sinh(alpha*x)/alpha,
                                    e3= (cosh(alpha*x) - 1.0)/alpha**2,
                                    e4= (sinh(alpha*x) - alpha*x)/alpha**3,
                                    e5= (-1*alpha**2 * x**2 / 2.0 + cosh(alpha*x) - 1.0)/alpha**4,
                                    e6= (-1*alpha**3 * x**3 / 6.0 + sinh(alpha*x) - alpha*x)/alpha**5,
                                    Zeta=alpha, Lambda=0, Eta=0) 
                
                else: # compression
                    #print('compression?')
                    return e_values(e0= -1*alpha*sin(alpha*x),
                                    e1= cos(alpha*x),
                                    e2= sin(alpha*x)/alpha,
                                    e3= (1.0 - cos(alpha*x))/alpha**2,
                                    e4= (alpha*x - sin(alpha*x))/alpha**3,
                                    e5= (alpha**2 * x**2 / 2.0 + cos(alpha*x) - 1.0)/alpha**4,
                                    e6= (alpha**3 * x**3 / 6.0 + sin(alpha*x) - alpha*x)/alpha**5,
                                    Zeta=alpha2, Lambda=0, Eta=0)                    
    #
    def Pcr(self, I: float):
        """P critical"""
        Pcr = K1 * pi**2 * self.E * I / self.L ** 2
#
#
#
# ---------------------------------------------------------------
#    Pilkey 2nd ed
#    TABLE 11-3
#    PART A: BEAMS WITH AXIAL FORCES AND ELASTIC FOUNDATIONS: 
#    GENERAL RESPONSE EXPRESSIONS
# ---------------------------------------------------------------
#
#
#
@dataclass
class BendingGE(TableBasic):
    """
    Pilkey 2nd ed
    TABLE 11-3
    PART A: BEAMS WITH AXIAL FORCES AND ELASTIC FOUNDATIONS:
            GENERAL RESPONSE EXPRESSIONS
    """

    __slots__ = ['E', 'G', 
                 'FV', 'FM', 'Ftheta', 'Fw',
                 'V0', 'M0', 'theta0', 'w0']

    def __init__(self, E: float, G: float) -> None:
        """
        E : modulus of elasticity
        G : Shear modulus
        I : moment of inertia
        x is measured from the left end
        """
        self.E = E
        self.G = G
    #
    def load(self, FV: float, FM: float,
             Ftheta: float, Fw: float) -> None:
        """
        Load @ x
        """
        self.FV = FV
        self.FM = FM
        self.Ftheta = Ftheta
        self.Fw = Fw

    #
    def R0(self, V0: float, M0: float,
           theta0: float, w0: float) -> None:
        """ 
        Initial Parameters
        """
        self.V0 = V0
        self.M0 = M0
        self.theta0 = theta0
        self.w0 = w0

    #
    def V(self, x: float, I: float) -> float:
        """ Shear force"""
        EI = self.E * I
        func = self.ei(x=x, I=I, k=0)
        return (self.w0 * func.Lambda * EI * (func.e2 + func.Zeta * func.e4)
                - self.theta0 * func.Lambda * EI * func.e3
                + self.V0 * (func.e1 + func.Zeta * func.e3)
                + self.FV)

    #
    def M(self, x: float, I: float) -> float:
        """ Bending moment"""
        EI = self.E * I
        func = self.ei(x=x, I=I, k=0)
        return (self.w0 * func.Lambda * EI * func.e3
                + self.theta0 * EI * (func.e0 - func.Eta * func.e3)
                + self.V0 * func.e2
                + self.M0 * (func.e1 - func.Eta * func.e3)
                + self.FM)

    #
    def theta(self, x: float,  I: float) -> float:
        """ Slope"""
        EI = self.E * I
        func = self.ei(x=x, I=I, k=0)
        return (self.w0 * func.Lambda * func.e4
                + self.theta0 * (func.e1 - func.Eta * func.e3)
                + self.V0 * func.e3 / EI
                + self.M0 * (func.e2 - func.Eta * func.e4) / EI
                + self.Ftheta)

    #
    def w(self, x: float, I: float) -> float:
        """ Deflection"""
        EI = self.E * I
        func = self.ei(x=x, I=I, k=0)
        return (self.w0 * (func.e1 + func.Zeta * func.e3)
                - self.theta0 * func.e2
                - self.V0 * (func.e4 / EI - (func.e2 + func.Zeta * func.e4) / (self.G * As))
                - self.M0 * func.e3 / EI
                + self.Fw)
    #
    def response(self, x:float, I: float) -> list[float]:
        """
        x is measured from the left end
        I = moment of inertia
        
        results: 
        [V, M, theta, w]
        """ 
        return [self.V(x, I=I), self.M(x, I=I), self.theta(x, I=I), self.w(x, I=I)]
#
#
