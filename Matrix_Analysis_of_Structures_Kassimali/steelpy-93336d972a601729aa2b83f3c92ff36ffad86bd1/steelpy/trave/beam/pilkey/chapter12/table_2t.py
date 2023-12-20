#
# Copyright (c) 2019-2023 steelpy
#
# Python stdlib imports
from __future__ import annotations
from dataclasses import dataclass
#from math import sinh, cosh, sqrt
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
class TorsionBarGE:
    """
    Pilkey 2nd ed
    TABLE 12-2
    UNIFORM BARS WITH ARBITRARY LOADING
    """

    #__slots__ = ['FV', 'FM', 'Ftheta', 'Fw',
    #             'V0', 'M0', 'theta0', 'w0', 'E']

    def __init__(self, E: float, G: float, J:float) -> None:
        """
        E : Elastic modulus
        G : Shear modulus
        """
        self.E = E
        self.G = G
        self.J = J
        #self.Cw = Cw
    #
    def load(self, FT: float, FB: float,
             Fpsi: float, Fphi: float,
             FTw: float) -> None:
        """
        Load @ x
        """
        self.FT = FT
        self.FB = FB
        self.Fphi = Fphi
    #
    def R0(self, T0: float, B0: float,
           psi0: float, phi0: float,
           Tw0: float) -> None:
        """
        Initial Parameters
        """
        self.T0 = T0
        self.B0 = B0
        self.Psi0 = psi0
        self.Phi0 = phi0
    #
    def phi(self, x: float, J: float) -> float:
        """ Angle of twist (rad)"""
        return self.Phi0 + self.T0 * x / (self.G * J) + self.Fphi
    #
    def psi(self, x: float, J: float) -> float:
        """ Rate of angle of twist (rad/m)"""
        return 0
    #
    def T(self, x: float) -> float:
        """ Total twisting moment (N-m)"""
        return self.T0 + self.FT
    #
    def B(self, x: float, J: float) -> float:
        """ Bimoment (N-m^2)"""
        return 0
    #
    def Tw(self, x: float, J: float) -> float:
        """ Warping torque (N-m)"""
        return 0
    #
    def response(self, x:float) -> list[float]:
        """
        x : distance from end 1

        results:
        [T, B, Psi, Phi, Tw]
        """
        return [self.T(x),
                self.B(x, self.J),
                self.psi(x, self.J),
                self.phi(x, self.J),
                self.Tw(x, self.J)]
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
    def Fphi(self, x: float, E: float, G: float, J: float) -> float:
        """ """
        return 0
    
    def Fpsi(self, x: float, E: float, G: float, J: float) -> float:
        """ """
        return 0    

    def FT(self, x: float, E: float, G: float, J: float) -> float:
        """ """
        return 0
    
    def FB(self, x: float, E: float, G: float, J: float) -> float:
        """ """
        return 0    

    def Tw(self, x: float, E: float, G: float, J: float) -> float:
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
    def __call__(self, x: float, E: float, G: float, J: float):
        """
        Formulas positive (+) is downwards 
        return: [FT, FB, Fpsi, Fphi, Tw]
        """
        return [1 * self.FT(x, E, G, J),
                self.FB(x, E, G, J),
                self.Fphi(x, E, G, J),
                1 * self.Fphi(x, E, G, J),
                self.Tw(x, E, G, J)]

#
#
@dataclass
class TBarConcentrated(ArbitraryLoading):
    __slots__ = ['T', 'L', 'L1']

    def __init__(self, T: float, L: float, L1: float):
        """
        Concentrated Torque
        """
        super().__init__(L, L1)
        self.T: float = T
    #
    def Fphi(self, x: float, E: float, G: float, J: float) -> float:
        """ """
        step = x - self.L1
        return -1 * self.T * self.function_n(step, 1) / (G * J)
    #
    def FT(self, x: float, E: float, G: float, J: float) -> float:
        """ """
        step = x - self.L1
        return -1 * self.T * self.function_n(step, 0)
#
#
@dataclass
class TBarDistributed(ArbitraryLoading):
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
    def mx(self):
        """Beam Axial load"""
        # Calculate area
        h = self.L - self.L1 - self.L2
        m = (self.T1 + self.T2) / 2.0
        area = m * h
        try:
            if self.T1 > self.T2:
                Lcog = (self.T1 + 2*self.T2)/(self.T1 + self.T2) * h / 3.0
            else:
                Lcog = h - (2*self.T1 + self.T2)/(self.T1 + self.T2) * h / 3.0
        except ZeroDivisionError:
            return 0, 0
        #
        h = area / m
        mt = area / h
        L1 = max(self.L1 + Lcog - h / 2.0, 0)
        L2 = L1 + h
        #       
        return mt, L1, L2    
    #
    #
    def Fphi(self, x: float, E: float, G: float, J: float) -> float:
        """ """
        mx, a1, a2 = self.mx()
        step1 = x - a1
        step2 = x - a2
        return (-1 * mx / (2 * G * J)
                * (self.function_n(step1, 1)**2
                   - self.function_n(step2, 1)**2))
    #
    def FT(self, x: float, E: float, G: float, J: float) -> float:
        """ """
        mx, a1, a2 = self.mx()
        step1 = x - a1
        step2 = x - a2
        return (-1 * mx *
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
class BTBarSupports:
    """Beam Bending Initial Parameters """

    __slots__ = ['L', 'E', 'G', 'J', '_support0']

    def __init__(self, L:float, J:float, 
                 E: float, G: float):
        """
        L : beam lenght [m]
        E : Elastic modulus [Pa] (default steel)
        G : Shear modulus (steel default)
        """
        self.L:float = L
        self.E:float = E
        self.G:float = G
        self.J:float = J
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
                return FixedFixed(self.L, self.E, self.G, J=self.J)
            
            elif re.match(r"\b(fix(ed)?|encastre(r)?)\b", supp2, re.IGNORECASE):
                return FixedFixed(self.L, self.E, self.G, J=self.J)
            
            elif re.match(r"\b(guide(d)?)\b", supp2, re.IGNORECASE):
                return FixedFixed(self.L, self.E, self.G, J=self.J)
            
            else:
                raise IOError("unstable")
        
        elif re.match(r"\b(fix(ed)?|encastre(r)?)\b", supp1, re.IGNORECASE):
            if re.match(r"\b(pinn(ed)?)\b", supp2, re.IGNORECASE):
                return FixedFixed(self.L, self.E, self.G, J=self.J)
            
            elif re.match(r"\b(fix(ed)?|encastre(r)?)\b", supp2, re.IGNORECASE):
                return FixedFixed(self.L, self.E, self.G, J=self.J)
            
            elif re.match(r"\b(guide(d)?)\b", supp2, re.IGNORECASE):
                return FixedFixed(self.L, self.E, self.G, J=self.J)
            
            elif re.match(r"\b(free)\b", supp2, re.IGNORECASE):
                return FixedFree(self.L, self.E, self.G, J=self.J)
            
            else:
                raise IOError(f"boundary {supp2} not supported")

        elif re.match(r"\b(free)\b", supp1, re.IGNORECASE):
            if re.match(r"\b(fix(ed)?|encastre(r)?)\b", supp2, re.IGNORECASE):
                return FreeFixed(self.L, self.E, self.G, J=self.J)


        elif re.match(r"\b(guide(d)?)\b", supp1, re.IGNORECASE):
            if re.match(r"\b(pinn(ed)?)\b", supp2, re.IGNORECASE):
                return FixedFixed(self.L, self.E, self.G, J=self.J)
            
            elif re.match(r"\b(fix(ed)?|encastre(r)?)\b", supp2, re.IGNORECASE):
                return FixedFixed(self.L, self.E, self.G, J=self.J)

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
        [T0, B0, Psi0, Phi0]
        """
        return self._support0.initial_parameters(F_bar=F_bar)
#
#
@dataclass
class ArbitrarySupport:
    __slots__ = ['L', 'E', 'G', 'J',
                 'FT_bar', 'FB_bar', 'Fpsi_bar', 'Fphi_bar']

    def __init__(self, L:float, E: float, G: float, J: float) -> None:
        """
        Pilkey 2nd ed
        TABLE 11-2
        PART C: SIMPLE BEAMS WITH ARBITRARY LOADING: INITIAL PARAMETER
        """
        self.L = L
        self.E = E
        self.G = G
        self.J = J
    #
    #
    @property
    def T0(self) -> float:
        """ Twisting moment """
        return 0
    #
    @property
    def B0(self) -> float:
        """ Bimoment """
        return 0
    #
    @property
    def Psi0(self) -> float:
        """ Rate of angle of twist """
        return 0
    #
    @property
    def Phi0(self) -> float:
        """ Angle of twist """
        return 0
    #
    #
    #
    def Tbar(self, FT_bar:float, FB_bar:float, 
             Fpsi_bar:float, Fphi_bar:float):
        """ """
        self.FT_bar = FT_bar
        self.FB_bar = FB_bar
        self.Fpsi_bar = Fpsi_bar
        self.Fphi_bar = Fphi_bar
    #
    def initial_parameters(self, F_bar:list[float]):
        """
        F_bar : [T0, B0, Psi0, Phi0]
        """
        # [FT,FB,Fpsi,Fphi]
        self.Tbar(*F_bar)
        return [self.T0, self.B0, self.Psi0, self.Phi0]
#
#
# ---------------------------------------------------------------
# Fixed
#
@dataclass
class FixedFixed(ArbitrarySupport):
    __slots__ = ['L', 'E', 'G', 'J',
                 'FT_bar', 'Fphi_bar']
    
    def __init__(self, L:float, E: float, G: float, J: float) -> None:
        """
        """
        super().__init__(L, E, G, J)
    #
    @property
    def T0(self) -> float:
        """ Twisting moment """
        return -1 * self.G * self.J / self.L * self.Fphi_bar
    #
#
@dataclass
class FixedFree(ArbitrarySupport):
    __slots__ = ['L', 'E', 'G', 'J',
                 'FT_bar', 'Fphi_bar']
    
    def __init__(self, L:float, E: float, G: float, J: float) -> None:
        """
        """
        super().__init__(L, E, G, J)
    #
    @property
    def T0(self) -> float:
        """ Twisting moment """
        return -1 * self.FT_bar
    #
#
# ---------------------------------------------------------------
# free
# ---------------------------------------------------------------
#
@dataclass
class FreeFixed(ArbitrarySupport):
    __slots__ = ['L', 'E', 'G', 'J', 
                 'FT_bar', 'Fphi_bar']
    
    def __init__(self, L:float, E: float, G: float, J: float) -> None:
        """
        """
        super().__init__(L, E, G, J)
    #
    @property
    def Phi0(self) -> float:
        """ Angle of twist """
        return -1 * self.Fphi_bar
    #
#