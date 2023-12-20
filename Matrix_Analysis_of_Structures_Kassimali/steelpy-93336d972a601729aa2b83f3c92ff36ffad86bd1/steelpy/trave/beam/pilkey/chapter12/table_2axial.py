#
# Copyright (c) 2019-2023 steelpy
#
# Python stdlib imports
from __future__ import annotations
from dataclasses import dataclass
from math import sinh, cosh, sqrt
#from typing import NamedTuple
import re
#
# package imports

#
#
#
# -----------------------------------------------------------------
#    Pilkey 2nd ed
#    TABLE 12-1
#    UNIFORM BARS WITH ARBITRARY LOADING
# -----------------------------------------------------------------
#
#
#
@dataclass
class AxialBarGE:
    """
    Pilkey 2nd ed
    TABLE 12-2
    UNIFORM BARS WITH ARBITRARY LOADING
    """
    #__slots__ = ['FV', 'FM', 'Ftheta', 'Fw',
    #             'V0', 'M0', 'theta0', 'w0', 'E']

    def __init__(self, E: float, A: float) -> None:
        """
        E : Elastic modulus
        G : Shear modulus
        """
        self.E = E
        self.A = A
    #
    def load(self, FP: float,
             blank1: float, blank2: float,
             Fu: float) -> None:
        """
        Load @ x
        """
        self.FP = FP
        self.Fu = Fu
    #
    def R0(self, P0: float,
           blank1: float, blank2: float,
           u0: float) -> None:
        """
        Initial Parameters
        """
        self.P0 = P0
        self.u0 = u0
    #
    def u(self, x: float, A: float) -> float:
        """ Axial Displacement (m)"""
        return self.u0 + self.P0 * x / (self.E * A) + self.Fu
    #
    def P(self, x: float, A: float) -> float:
        """ Axial Force (N)"""
        return self.P0 + self.FP
    #
    #
    def response(self, x:float) -> list[float]:
        """
        x : distance from end 1

        results:
        [P, blank, blank, u]
        """
        return [self.P(x, self.A), 0, 0, self.u(x, self.A)]
#
#
#
# ---------------------------------------------------------------
# Pilkey 2nd ed
# TABLE 12-2
# PART B: LOADING FUNCTIONS
# ---------------------------------------------------------------
#
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
    def Fu(self, x: float, E: float, A: float) -> float:
        """ """
        return 0

    def FP(self, x: float, E: float, A: float) -> float:
        """ """
        return 0

    #
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
    def __call__(self, x: float, E: float, A: float):
        """
        Formulas positive (+) is downwards 
        return: [FP, blank, blank, Fu]
        """
        return [1 * self.FP(x, E, A),
                0, 0,
                1 * self.Fu(x, E, A)]

#
#
@dataclass
class PBarConcentrated(ArbitraryLoading):
    __slots__ = ['P', 'L', 'L1']

    def __init__(self, P: float, L: float, L1: float):
        """
        Concentrated Torque
        """
        super().__init__(L, L1)
        self.P = P
    #
    def Fu(self, x: float, E: float, A: float) -> float:
        """ """
        step = x - self.L1
        return -1 * self.P * self.function_n(step, 1) / (E * A)
    #
    def FP(self, x: float, E: float, A: float) -> float:
        """ """
        step = x - self.L1
        return -1 * self.P * self.function_n(step, 0)
#
#
@dataclass
class PBarDistributed(ArbitraryLoading):
    __slots__ = ['P1', 'P2', 'L', 'L1', 'L2',
                 'L3', '_slope']
    
    def __init__(self, P: float, P2: float,
                 L: float, L1: float, L2: float) -> None:
        """
        Distributed Torque
        """
        super().__init__(L, L1)
        self.P1 = P
        self.P2 = P2
        self.L2 = L2
        #
        self.L3 = self.L - self.L2
        #self._slope = (self.P2 - self.P1) / (self.P3 - self.P1)
    #
    def px(self):
        """Beam Axial load"""
        # Calculate area
        h = self.L - self.L1 - self.L2
        m = (self.P1 + self.P2) / 2.0
        area = m * h
        try:
            if self.P1 > self.P2:
                Lcog = (self.P1 + 2*self.P2)/(self.P1 + self.P2) * h / 3.0
            else:
                Lcog = h - (2*self.P1 + self.P2)/(self.P1 + self.P2) * h / 3.0
        except ZeroDivisionError:
            return 0, 0
        #
        h = area / m
        mp = area / h
        L1 = max(self.L1 + Lcog - h / 2.0, 0)
        L2 = L1 + h
        #       
        return mp, L1, L2    
    #
    def Fu(self, x: float, E: float, A: float) -> float:
        """ """
        px, a1, a2 = self.px()
        step1 = x - a1
        step2 = x - a2
        return (-1 * px / (2 * E * A)
                * (self.function_n(step1, 1)**2
                   - self.function_n(step2, 1)**2))
    #
    def FP(self, x: float, E: float, A: float) -> float:
        """ """
        px, a1, a2 = self.px()
        step1 = x - a1
        step2 = x - a2
        return (-1 * px *
                (self.function_n(step1, 1)
                 - self.function_n(step2, 1)))
#
#
#
# ---------------------------------------------------------------
# Pilkey 2nd ed
# TABLE 12-2
# PART C: INITIAL PARAMETERS
# ---------------------------------------------------------------
#
#
# ---------------------------------------------------------------
#
#
@dataclass
class PBarSupports:
    """Beam Bending Initial Parameters """

    __slots__ = ['L', 'E', 'A', '_support0']

    def __init__(self, L:float, E: float, A: float):
        """
        L : beam lenght [m]
        E : Elastic modulus [Pa]
        A : Area [m2]
        """
        self.L = L
        self.E = E
        self.A = A
    #
    def supports(self, supp1:str, supp2:str,
                 k1:float|None=None, k2:float|None=None):
        """
        support1 : suppor 1 (left) [pinned/fixed/guide/free]
        support2 : suppor 2 (right) [pinned/fixed/guide/free]
        """
        self._support0 = self.support_func(supp1, supp2)
    #
    def support_func(self, supp1:str, supp2:str):
        """
        support1 : suppor 1 (left) [pinned/fixed/guide/free]
        support2 : suppor 2 (right) [pinned/fixed/guide/free]
        """
        if re.match(r"\b(pinn(ed)?)\b", supp1, re.IGNORECASE):
            if re.match(r"\b(pinn(ed)?)\b", supp2, re.IGNORECASE):
                return FixedFixed(self.L, self.E, self.A)
            
            elif re.match(r"\b(fix(ed)?|encastre(r)?)\b", supp2, re.IGNORECASE):
                return FixedFixed(self.L, self.E, self.A)
            
            elif re.match(r"\b(guide(d)?)\b", supp2, re.IGNORECASE):
                return FixedFixed(self.L, self.E, self.A)
            
            else:
                raise IOError("unstable")
        
        elif re.match(r"\b(fix(ed)?|encastre(r)?)\b", supp1, re.IGNORECASE):
            if re.match(r"\b(pinn(ed)?)\b", supp2, re.IGNORECASE):
                return FixedFixed(self.L, self.E, self.A)
            
            elif re.match(r"\b(fix(ed)?|encastre(r)?)\b", supp2, re.IGNORECASE):
                return FixedFixed(self.L, self.E, self.A)
            
            elif re.match(r"\b(guide(d)?)\b", supp2, re.IGNORECASE):
                return FixedFixed(self.L, self.E, self.A)
            
            elif re.match(r"\b(free)\b", supp2, re.IGNORECASE):
                return FixedFree(self.L, self.E, self.A)
            
            else:
                raise IOError(f"boundary {supp2} not supported")

        elif re.match(r"\b(free)\b", supp1, re.IGNORECASE):
            if re.match(r"\b(fix(ed)?|encastre(r)?)\b", supp2, re.IGNORECASE):
                return FreeFixed(self.L, self.E, self.A)


        elif re.match(r"\b(guide(d)?)\b", supp1, re.IGNORECASE):
            if re.match(r"\b(pinn(ed)?)\b", supp2, re.IGNORECASE):
                return FixedFixed(self.L, self.E, self.A)
            
            elif re.match(r"\b(fix(ed)?|encastre(r)?)\b", supp2, re.IGNORECASE):
                return FixedFixed(self.L, self.E, self.A)

            else:
                raise IOError("unstable")

        else:
            raise IOError(f"boundary {supp1} not supported")
    #
    def __call__(self, F_bar:list[float]):
        """
        I : moment of inertia [m^4]
        F_bar : [FV_bar, FM_bar, Ftheta_bar, Fw_bar]

        return :
        Initial Parameters x = 0 (support 1)
        [P0, blank, blank, u0]
        """
        return self._support0.initial_parameters(F_bar=F_bar)
#
#
@dataclass
class ArbitrarySupport:
    __slots__ = ['L', 'E', 'A','FP_bar', 'Fu_bar']

    def __init__(self, L:float, E: float, A: float) -> None:
        """
        Pilkey 2nd ed
        TABLE 11-2
        PART C: SIMPLE BEAMS WITH ARBITRARY LOADING: INITIAL PARAMETER
        """
        self.L:float = L
        self.E:float = E
        self.A:float = A
    #
    #
    @property
    def P0(self) -> float:
        """ Axial Force """
        return 0
    #
    #
    @property
    def u0(self) -> float:
        """ Axial Displacement """
        return 0
    #
    #
    #
    def Pbar(self, FP_bar:float,
             blank1: float, blank2: float,
             Fu_bar:float):
        """ """
        self.FP_bar = FP_bar
        self.Fu_bar = Fu_bar
    #
    def initial_parameters(self, F_bar:list[float]):
        """
        F_bar : [T0, B0, Psi0, Phi0]
        """
        # [FP,blank,blank,Fu]
        self.Pbar(*F_bar)
        return [self.P0, 0, 0, self.u0]
#
#
# ---------------------------------------------------------------
# Fixed
#
@dataclass
class FixedFixed(ArbitrarySupport):
    __slots__ = ['L', 'E', 'A', 
                 'FP_bar', 'Fu_bar']
    
    def __init__(self, L:float, E: float, A: float) -> None:
        """
        """
        super().__init__(L, E, A)
    #
    @property
    def P0(self) -> float:
        """ Twisting moment """
        return -1 * self.E * self.A / self.L * self.Fu_bar
    #
#
@dataclass
class FixedFree(ArbitrarySupport):
    __slots__ = ['L', 'E', 'A',
                 'FP_bar', 'Fu_bar']
    
    def __init__(self, L:float, E: float, A: float) -> None:
        """
        """
        super().__init__(L, E, A)
    #
    @property
    def P0(self) -> float:
        """ Twisting moment """
        return -1 * self.FP_bar
    #
#
# ---------------------------------------------------------------
# free
# ---------------------------------------------------------------
#
@dataclass
class FreeFixed(ArbitrarySupport):
    __slots__ = ['L', 'E', 'A', 
                 'FP_bar', 'Fu_bar']
    
    def __init__(self, L:float, E: float, A: float) -> None:
        """
        """
        super().__init__(L, E, A)
    #
    @property
    def u0(self) -> float:
        """ Angle of twist """
        return -1 * self.Fu_bar
    #
#