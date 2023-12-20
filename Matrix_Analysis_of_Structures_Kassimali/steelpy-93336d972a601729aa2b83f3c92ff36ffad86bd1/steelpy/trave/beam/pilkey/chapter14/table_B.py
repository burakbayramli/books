#
# Copyright (c) 2019-2023 steelpy
#

# Python stdlib imports
from __future__ import annotations
#from bisect import bisect_right
from dataclasses import dataclass
from math import cosh, sinh, sqrt
#import re

# package imports

#
#
# ---------------------------------------------------------------
# Pilkey 2nd ed
# TABLE 14-1
# PART B: TWISTING OF THIN-WALLED BEAMS WITH ARBITRARY LOADING:
# LOADING FUNCTIONS
# ---------------------------------------------------------------
#
#
@dataclass
class ArbitraryLoading:
    __slots__ = ['L', 'L1']

    def __init__(self, L: float, L1: float) -> None:
        """
        """
        self.L: float = L
        self.L1: float = L1
    #
    def Fphi(self, x: float, E: float, G: float, J: float, Cw:float) -> float:
        """ """
        return 0

    def Fpsi(self, x: float, E: float, G: float, J: float, Cw:float) -> float:
        """ """
        return 0

    def FT(self, x: float, E: float, G: float, J: float, Cw:float) -> float:
        """ """
        return 0

    def FB(self, x: float, E: float, G: float, J: float, Cw:float) -> float:
        """ """
        return 0
    
    def FTw(self, x: float, E: float, G: float, J: float, Cw:float) -> float:
        """ """
        return 0    

    #
    def C(self, E: float, G: float, J: float, Cw:float):
        """ """
        #try:
        return sqrt(G * J / (E * Cw))
        #except ZeroDivisionError:
        #    return 0

    def function_n(self, step: float, n: int) -> float:
        """ <x-L>^n """
        if n < 0:
            return 0
        elif step < 0:
            return 0
        elif n == 0:
            return 1
        else:
            return step ** n
    
    #
    def __call__(self, x: float, E: float, G: float, J: float, Cw:float):
        """
        Formulas positive (+) is downwards
        
        Return:
        FT : Twisting moment
        FB : Bimoment
        Fpsi : Rate of angle of twist
        Fphi: Angle of twist
        Ftw : Warping torque
        
        [FT, FB, Fpsi, Fphi, Tw]
        """
        return [1 * self.FT(x, E, G, J, Cw),
                1 * self.FB(x, E, G, J, Cw), 
                1 * self.Fpsi(x, E, G, J, Cw),
                1 * self.Fphi(x, E, G, J, Cw),
                1 * self.FTw(x, E, G, J, Cw)]

#
#
#
@dataclass
class TOpenConcentrated(ArbitraryLoading):
    __slots__ = ['T', 'L', 'L1']

    def __init__(self, T: float, L: float, L1: float):
        """
        Concentrated Torque
        """
        super().__init__(L, L1)
        self.T: float = T
    #
    def Fphi(self, x: float, E: float, G: float, J: float, Cw:float) -> float:
        """ """
        step = x - self.L1
        C = self.C(E=E, G=G, J=J, Cw=Cw)
        Cfun = C * self.function_n(step, 1)
        #try:
        return self.T / (C * G * J) * (-1 * Cfun + sinh(Cfun))
        #except ZeroDivisionError:
        #    return 0
    
    #
    def Fpsi(self, x, E: float, G: float, J: float, Cw:float) -> float:
        """ """
        step = x - self.L1
        C = self.C(E=E, G=G, J=J, Cw=Cw)
        Cfun = C * self.function_n(step, 1)
        return (self.T / (G * J)
                * (self.function_n(step, 0) - cosh(Cfun)))
    
    #
    def FT(self, x, E: float, G: float, J: float, Cw:float) -> float:
        """ """
        step = x - self.L1
        return -1 * self.T * self.function_n(step, 0)
    
    #
    def FB(self, x, E: float, G: float, J: float, Cw:float) -> float:
        """ """
        step = x - self.L1
        C = self.C(E=E, G=G, J=J, Cw=Cw)
        #try:
        Cfun = C * self.function_n(step, 1)
        return -1 * self.T / C * sinh(Cfun) 
        #except ZeroDivisionError:
        #    return 0
#
#
@dataclass
class TOpenDistributed(ArbitraryLoading):
    __slots__ = ['T1', 'T2', 'L', 'L1', 'L2',
                 '_L3', '_slope']
    
    def __init__(self, T: float, T2: float,
                 L: float, L1: float, L2: float) -> None:
        """
        Distributed Torque
        """
        super().__init__(L, L1)
        self.T1: float = T
        self.T2: float = T2
        self.L2: float = L2
        #
        self.L3 = self.L - self.L2
        self._slope = (self.T2 - self.T1) / (self.L3 - self.L1)
    
    #
    def Fphi(self, x, E: float, G: float, J: float, Cw:float) -> float:
        """ """
        step1 = x - self.L1
        step2 = x - self.L3
        C = self.C(E=E, G=G, J=J, Cw=Cw)
        Cfun1 = C * self.function_n(step1, 1)
        Cfun2 = C * self.function_n(step2, 1)
        #
        t1 = (self.T1 / (C**2 * G * J)
              * (cosh(Cfun1)
                 - self.function_n(step1, 0)
                 - C**2 * self.function_n(step1, 2) / 2.0
                 - cosh(Cfun2)
                 + self.function_n(step2, 0)
                 + C**2 * self.function_n(step2, 2) / 2.0))
        #
        t2 = (self._slope / (E * Cw)
              * (sinh(Cfun1) / C
                 - self.function_n(step1, 1)
                 - C**2 * self.function_n(step1, 2) / 6.0
                 - sinh(Cfun2) / C
                 + self.function_n(step2, 1)
                 + C**2 * self.function_n(step2, 2) / 6.0))
        
        return t1 + t2
    
    #
    def Fpsi(self, x, E: float, G: float, J: float, Cw:float) -> float:
        """ """
        step1 = x - self.L1
        step2 = x - self.L3
        C = self.C(E=E, G=G, J=J, Cw=Cw)
        Cfun1 = C * self.function_n(step1, 1)
        Cfun2 = C * self.function_n(step2, 1)        
        #
        t1 = (self.T1 / (C * G * J)
              * (C * self.function_n(step1, 1)
                 - sinh(Cfun1)
                 - C * self.function_n(step2, 1)
                 + sinh(Cfun2)))
        #
        t2 = (-1 * self._slope / (C * G * J)
              * (cosh(Cfun1) / C
                 - self.function_n(step1, 0) / C
                 - C / 2.0 * self.function_n(step1, 2)
                 - cosh(Cfun2) / C
                 + self.function_n(step2, 0) / C
                 + C / 2.0 * self.function_n(step2, 2)))
    
        return t1 + t2
    
    #
    def FT(self, x, E: float, G: float, J: float, Cw:float) -> float:
        """ """
        step1 = x - self.L1
        step2 = x - self.L3
        #
        t1 = (-1 * self.T1
              * (self.function_n(step1, 1)
                 - self.function_n(step2, 1)))
        #
        t2 = (-1 * self._slope / 2
              * (self.function_n(step1, 2)
                 - self.function_n(step2, 2)))
        
        return t1 + t2
    #
    def FB(self, x, E: float, G: float, J: float, Cw:float) -> float:
        """ """
        step1 = x - self.L1
        step2 = x - self.L3
        C = self.C(E=E, G=G, J=J, Cw=Cw)
        Cfun1 = C * self.function_n(step1, 1)
        Cfun2 = C * self.function_n(step2, 1)        
        #
        t1 = (-1 * self.T1 / C**2
              * (cosh(Cfun1)
                 - self.function_n(step1, 0)
                 - cosh(Cfun2)
                 + self.function_n(step2, 0)))
        #
        t2 = (self._slope
              * (self.function_n(step1, 1)
                 - sinh(Cfun1) / C
                 - self.function_n(step2, 1)
                 + sinh(Cfun2) / C ))
        
        return t1 + t2
#
#
    