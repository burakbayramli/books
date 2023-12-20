# 
# Copyright (c) 2019-2021 steelpy

# Python stdlib imports
import math
from typing import NamedTuple, List, Tuple

# package imports


#
class thetas(NamedTuple):
    """
    """
    theta:List
    theta_I:List
    theta_II:List
    theta_III:List
    span:List
#
class Thetas(NamedTuple):
    """
    """
    O:float
    I:float
    II:float
    III:float
    T:float
#
#
class beamEnds(NamedTuple):
    """
    """
    A : Tuple
    B : Tuple
#
#
def funtion_0(x, a):
    """
    """
    if x < a:
        return 0.0
    return 1.0
#
def funtion_n(x, a, n):
    """
    """
    if x < a:
        return 0.0
    return (x - a)**n
#
class Table_103:
    
    def __init__(self, item):
        """
        """
        self._section = item._section
        self._material = item._material
        self._units = item._units
        self.a = item.Xe
        self.span = item.L
        self._beam_support = self.restrain_1e # simply supported
    #
    def _get_parameters(self, a):
        """
        """
        self.beta = (self._section.K * self._material.G 
                     / (self._section.Cw * self._material.E))**0.50        
        #
        l = self.span
        _beta_a = self.beta * a
        self._A1 = math.cosh(_beta_a.value)
        self._A2 = math.sinh(_beta_a.value)
        #
        _beta_l = self.beta * l
        _la = (l - a)
        self._C1 = math.cosh(_beta_l.value)
        self._C2 = math.sinh(_beta_l.value)
        self._C3 = math.cosh(_beta_l.value) - 1.0
        self._C4 = math.sinh(_beta_l.value) - self.beta.value * _la.value
        #
        self._Ca1 = math.cosh(self.beta.value * _la.value)
        self._Ca2 = math.sinh(self.beta.value * _la.value)
        #
        self._Ca3 = math.cosh(self.beta.value * _la.value) - 1.0
        self._Ca4 = math.sinh(self.beta.value * _la.value) - self.beta.value * _la.value
        #
        self._Ca5 = self._Ca3 - self.beta.value**2 * _la.value**2 / 2.0
        self._Ca6 = self._Ca4 - self.beta.value**3 * _la.value**3 / 6.0
    #
    def restrain_1b(self, a):
        """
        Left end free to twist and warp
        Right end fixed (no twist or warp)
        """
        l = self.span
        self._get_parameters(a)
    #
    def restrain_1e(self, To, a):
        """
        Both ends free to warp but not twist
        """
        #
        l = self.span
        self._get_parameters(a)
        #
        TA = -1.0 * To * (1.0 - a.value / l.value)
        theta_A = 0
        theta_Ai = (To / (self._section.Cw * self._material.E * self.beta**2)
                    * (1.0 - a / l - self._Ca2 / self._C2))
        theta_Aii = 0
        theta_Aiii = 0
        #
        TB = -1.0 * To * a.value / l.value
        theta_B = 0
        theta_Bi = (To / (self._section.Cw * self._material.E * self.beta**2)
                    * (a / l - self._A2 / self._C2))
        theta_Bii = 0
        theta_Biii = To * self._A2 / (self._section.Cw * self._material.E * self._C2)
        #
        return beamEnds(A = Thetas(theta_A, theta_Ai.value, theta_Aii, theta_Aiii, TA.value),
                        B = Thetas(theta_B, theta_Bi.value, theta_Bii, theta_Biii, TB.value))
    #
    def restrain_1g(self, a):
        """
        Both ends fixed ( no warp or twist)
        """
        l = self.span
        self._get_parameters(a)   
    #
    @property
    def beam_support(self):
        """
        """
        return self._beam_support
    
    @beam_support.setter
    def beam_support(self, support):
        """
        """
        if 'free' in support:
            self._beam_support = self.restrain_1b  # 'cantilever'
        elif 'fixed' in support:
            self._beam_support = self.restrain_1g  #'fixed_both_ends'
    #
    def theta_function(self, x):
        """
        """
        a = self.a.value
        _beta_x = self.beta * x
        self._F1 = math.cosh(_beta_x.value)
        self._F2 = math.sinh(_beta_x.value)
        self._F3 = math.cosh(_beta_x.value) - 1.0
        self._F4 = math.sinh(_beta_x.value) - _beta_x.value
        #
        _xa = x.value - a
        self._Fa1 = funtion_0(x.value, a) * math.cosh(self.beta.value * _xa)
        self._Fa2 = math.sinh(self.beta.value * funtion_n(x.value, a, 1))
        self._Fa3 = funtion_0(x.value, a) * (math.cosh(self.beta.value * _xa) - 1.0)
        self._Fa4 = (math.sinh(self.beta.value * funtion_n(x.value, a, 1)) 
                     - (self.beta.value * funtion_n(x.value, a, 1)))
        #
        self._Fa5 = self._Fa3 - self.beta.value**2 * funtion_n(x.value, a, 2) / 2.0
        self._Fa6 = self._Fa4 - self.beta.value**3 * funtion_n(x.value, a, 3) / 6.0
        #
        #T = TA + To * funtion_0(x - a)
    #
    def get_thetas(self, To, x_vals):
        """
        """
        #
        l = self.span
        a = self.a
        b = (l - a)
        #
        _thetas = self._beam_support(To, self.a)
        #
        theta = []
        theta_i = []
        theta_ii = []
        theta_iii = []
        for x in x_vals:
            self.theta_function(x)
            theta.append(_thetas.A.O + _thetas.A.I * self._F2 / self.beta.value
                         + _thetas.A.II * self._F3 / self.beta.value**2
                         + (_thetas.A.T * self._F4 
                            / (self._section.Cw.value * self._material.E.value * self.beta.value**3))
                         + (To.value * self._Fa4 
                            / (self._section.Cw.value * self._material.E.value * self.beta.value**3)))
            
            theta_i.append(_thetas.A.I * self._F1 + _thetas.A.II * self._F2 / self.beta.value
                           + (_thetas.A.T * self._F3 
                              / (self._section.Cw.value * self._material.E.value * self.beta.value**2))
                           + (To.value * self._Fa3 
                              / (self._section.Cw.value * self._material.E.value * self.beta.value**2)))
            
            theta_ii.append(_thetas.A.II * self._F1 + _thetas.A.I * self._F2 * self.beta.value
                            + (_thetas.A.T * self._F2 
                               / (self._section.Cw.value * self._material.E.value * self.beta.value))
                            + (To.value * self._Fa2 
                               / (self._section.Cw.value * self._material.E.value * self.beta.value)))
            
            theta_iii.append((_thetas.A.I * self._F1 * self.beta.value**2 
                              + _thetas.A.II * self._F2 * self.beta.value)
                             + _thetas.A.T * self._F1 / (self._section.Cw.value * self._material.E.value)
                             + To.value * self._Fa1 / (self._section.Cw.value * self._material.E.value))
        #
        return thetas(theta, theta_i, theta_ii, theta_iii, x_vals)