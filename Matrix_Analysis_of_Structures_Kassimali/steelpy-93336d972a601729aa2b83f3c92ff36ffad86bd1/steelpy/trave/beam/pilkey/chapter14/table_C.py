#
# Copyright (c) 2019-2023 steelpy
#
# Python stdlib imports
from __future__ import annotations
#from array import array
#from collections.abc import Mapping
from dataclasses import dataclass
from math import tanh, cosh, sinh, sqrt
#from typing import NamedTuple
import re
#
# package imports
#
#

# ---------------------------------------------------------------
#
# 
#
@dataclass
class BTOpenSupports:
    """Beam Bending Initial Parameters """

    __slots__ = ['L', 'E', 'G', 'J', 'Cw', 
                 '_support0']

    def __init__(self, L:float, J:float, Cw:float,
                 E: float, G: float): #
        """
        L : beam lenght [m]
        E : Elastic modulus [Pa] (default steel)
        G : Shear modulus (steel default)
        """
        self.L:float = L
        self.E:float = E
        self.G:float = G
        self.J:float = J
        self.Cw:float = Cw
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
                return PinnedPinned(self.L, self.E, self.G)
            
            elif re.match(r"\b(fix(ed)?|encastre(r)?)\b", supp2, re.IGNORECASE):
                return PinnedFixed(self.L, self.E, self.G)
            
            elif re.match(r"\b(guide(d)?)\b", supp2, re.IGNORECASE):
                return PinnedGuided(self.L, self.E, self.G)
            
            else:
                raise IOError("unstable")
        
        elif re.match(r"\b(fix(ed)?|encastre(r)?)\b", supp1, re.IGNORECASE):
            if re.match(r"\b(pinn(ed)?)\b", supp2, re.IGNORECASE):
                return FixedPinned(self.L, self.E, self.G)
            
            elif re.match(r"\b(fix(ed)?|encastre(r)?)\b", supp2, re.IGNORECASE):
                return FixedFixed(self.L, self.E, self.G)
            
            elif re.match(r"\b(guide(d)?)\b", supp2, re.IGNORECASE):
                return FixedGuided(self.L, self.E, self.G)
            
            elif re.match(r"\b(free)\b", supp2, re.IGNORECASE):
                return FixedFree(self.L, self.E, self.G)
            
            else:
                raise IOError(f"boundary {supp2} not supported")

        elif re.match(r"\b(free)\b", supp1, re.IGNORECASE):
            if re.match(r"\b(fix(ed)?|encastre(r)?)\b", supp2, re.IGNORECASE):
                return FreeFixed(self.L, self.E, self.G)


        elif re.match(r"\b(guide(d)?)\b", supp1, re.IGNORECASE):
            if re.match(r"\b(pinn(ed)?)\b", supp2, re.IGNORECASE):
                return GuidedPinned(self.L, self.E, self.G)
            
            elif re.match(r"\b(fix(ed)?|encastre(r)?)\b", supp2, re.IGNORECASE):
                return GuidedFixed(self.L, self.E, self.G)

            else:
                raise IOError("unstable")

        else:
            raise IOError(f"boundary {supp1} not supported")
    #
    def __call__(self, F_bar:list[float]):
        """
        I : moment of inertia [m^4]
        F_bar : [FV_bar, FM_bar, Ftheta_bar, Fw_bar, FTw_bar]

        return :
        Initial Parameters x = 0 (support 1)
        [T0, B0, Psi0, Phi0]
        """
        return self._support0.initial_parameters(J=self.J, Cw=self.Cw,
                                                 F_bar=F_bar)
#
#
# ---------------------------------------------------------------
# Pilkey 2nd ed
# TABLE 14-1
# PART C: TWISTING OF THIN-WALLED BEAMS WITH ARBITRARY LOADING:
# INITIAL PARAMETERS
# ---------------------------------------------------------------
#
#
#
@dataclass
class ArbitrarySupport:
    __slots__ = ['L', 'E', 'G', 'J', 'Cw', 'C', 
                 'FT_bar', 'FB_bar', 'Fpsi_bar',
                 'Fphi_bar', 'FTw_bar']

    def __init__(self, L:float, E: float, G: float) -> None:
        """
        Pilkey 2nd ed
        TABLE 11-2
        PART C: SIMPLE BEAMS WITH ARBITRARY LOADING: INITIAL PARAMETER
        """
        self.L:float = L
        self.E:float = E
        self.G:float = G
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
    @property
    def Tw0(self) -> float:
        """ Warping torque """
        return 0    
    #
    def Tbar(self, FT_bar:float, FB_bar:float, 
             Fpsi_bar:float, Fphi_bar:float,
             FTw_bar: float):
        """ """
        self.FT_bar = FT_bar
        self.FB_bar = FB_bar
        self.Fpsi_bar = Fpsi_bar
        self.Fphi_bar = Fphi_bar
        self.FTw_bar = FTw_bar
    #
    def initial_parameters(self, J: float, Cw:float, F_bar:list[float]):
        """
        F_bar : [T0, B0, Psi0, Phi0, Tw0]
        """
        self.J = J
        self.Cw = Cw
        #try:
        self.C = sqrt(self.G*J/(self.E*Cw))
        #except ZeroDivisionError:
        #    self.C = 0
        # [FT,FB,Fpsi,Fphi]
        self.Tbar(*F_bar)
        return [self.T0, self.B0, self.Psi0, self.Phi0, self.Tw0]
#
#
#
# ---------------------------------------------------------------
# Pinned
# ---------------------------------------------------------------
#
@dataclass
class PinnedPinned(ArbitrarySupport):
    __slots__ = ['L', 'E', 'G', 'J', 'Cw', 'C',
                 'FT_bar', 'FB_bar', 'Fpsi_bar', 'Fphi_bar']

    def __init__(self, L:float, E: float, G: float) -> None:
        """
        """
        super().__init__(L, E, G)
    #
    @property
    def T0(self) -> float:
        """ Twisting moment """
        return -1 / self.L * (self.G * self.J * self.Fphi_bar + self.FB_bar)
    #
    @property
    def Psi0(self) -> float:
        """ Rate of angle of twist """
        #try:
        CL = self.C * self.L
        #operator = CL / sinh(CL)
        #except ZeroDivisionError:
        #   operator = 0
        return (self.FB_bar / (self.G * self.J * self.L)
                * (- 1 +  CL / sinh(CL))
                + self.Fphi_bar / self.L)
#
#
@dataclass
class PinnedFixed(ArbitrarySupport):
    __slots__ = ['L', 'E', 'G', 'J', 'Cw', 'C',
                 'FT_bar', 'FB_bar', 'Fpsi_bar', 'Fphi_bar']
    
    def __init__(self, L:float, E: float, G: float) -> None:
        """
        """
        super().__init__(L, E, G)
    #
    @property
    def T0(self) -> float:
        """ Twisting moment """
        CL = self.C * self.L
        operator = (sinh(CL) -  CL * cosh(CL))
        return (self.G * self.J / operator
                * (self.Fphi_bar * self.C * cosh(CL)
                   + self.Psi0 * sinh(CL)))
    #
    @property
    def Psi0(self) -> float:
        """ Rate of angle of twist """
        CL = self.C * self.L
        operator = (sinh(CL) -  CL * cosh(CL))        
        return (1 / operator
                * ((CL - sinh(CL) * self.Fpsi_bar)
                   - self.C * (cosh(CL) - 1.0) * self.Fphi_bar))
#
#
@dataclass
class PinnedGuided(ArbitrarySupport):
    __slots__ = ['L', 'E', 'G', 'J', 'Cw', 'C',
                 'FT_bar', 'FB_bar', 'Fpsi_bar', 'Fphi_bar']
    
    def __init__(self, L:float, E: float, G: float) -> None:
        """
        """
        super().__init__(L, E, G)
    #
    @property
    def T0(self) -> float:
        """ Twisting moment """
        return -1 * self.FT_bar
    #
    @property
    def Psi0(self) -> float:
        """ Rate of angle of twist """
        CL = self.C * self.L
        #operator = (sinh(CL) -  CL * cosh(CL))
        return (-1 / cosh(CL)
                * (self.FT_bar / (self.G * self.J)
                   * (1.0 - cosh(CL)) + self.Fpsi_bar))
#
#
# ---------------------------------------------------------------
# Fixed
# ---------------------------------------------------------------
#
@dataclass
class FixedPinned(ArbitrarySupport):
    __slots__ = ['L', 'E', 'G', 'J', 'Cw', 'C',
             'FT_bar', 'FB_bar', 'Fpsi_bar', 'Fphi_bar']
    
    def __init__(self, L:float, E: float, G: float) -> None:
        """
        """
        super().__init__(L, E, G)
    #
    @property
    def T0(self) -> float:
        """ Twisting moment """
        CL = self.C * self.L
        #operator = (sinh(CL) -  CL * cosh(CL))
        return (self.Fphi_bar * self.E * self.Cw * self.C**3
                * cosh(CL) - self.C * (1.0 - cosh(CL) * self.FB_bar))
    #
    @property
    def B0(self) -> float:
        """ Bimoment """
        CL = self.C * self.L
        operator = (sinh(CL) -  CL * cosh(CL))
        return (1 / operator
                * ((CL - sinh(CL)) * self.FB_bar
                   - self.G * self.J * self.Fphi_bar * sinh(CL)))
#
@dataclass
class FixedFixed(ArbitrarySupport):
    __slots__ = ['L', 'E', 'G', 'J', 'Cw', 'C',
                 'FT_bar', 'FB_bar', 'Fpsi_bar', 'Fphi_bar']
    
    def __init__(self, L:float, E: float, G: float) -> None:
        """
        """
        super().__init__(L, E, G)
    #
    @property
    def T0(self) -> float:
        """ Twisting moment """
        CL = self.C * self.L
        operator =  cosh(CL)
        return (1 / operator
                * (self.G * self.J * self.C * self.Fphi_bar * sinh(CL)
                   - self.G * self.J * (1.0 - cosh(CL) * self.Psi0)))
    #
    @property
    def B0(self) -> float:
        """ Bimoment """
        CL = self.C * self.L
        operator =  cosh(CL)
        return (1 / operator
                * (self.E * self.Cw * self.C * (CL - sinh(CL)) * self.Psi0
                   - self.G * self.J * (cosh(CL) - 1.0) * self.Fphi_bar))
#
@dataclass
class FixedFree(ArbitrarySupport):
    __slots__ = ['L', 'E', 'G', 'J', 'Cw', 'C',
                 'FT_bar', 'FB_bar', 'Fpsi_bar', 'Fphi_bar']
    
    def __init__(self, L:float, E: float, G: float) -> None:
        """
        """
        super().__init__(L, E, G)
    #
    @property
    def T0(self) -> float:
        """ Twisting moment """
        return -1 * self.FT_bar
    #
    @property
    def B0(self) -> float:
        """ Bimoment """
        CL = self.C * self.L
        return (self.FT_bar / self.C * tanh(CL)
                - self.FB_bar / cosh(CL))
#
@dataclass
class FixedGuided(ArbitrarySupport):
    __slots__ = ['L', 'E', 'G', 'J', 'Cw', 'C',
                 'FT_bar', 'FB_bar', 'Fpsi_bar', 'Fphi_bar']
    
    def __init__(self, L:float, E: float, G: float) -> None:
        """
        """
        super().__init__(L, E, G)
    #
    @property
    def T0(self) -> float:
        """ Twisting moment """
        return -1 * self.FT_bar
    #
    @property
    def B0(self) -> float:
        """ Bimoment """
        CL = self.C * self.L
        return (- 1.0 / sinh(CL)
                * (self.FT_bar / self.C * (1.0 - cosh(CL))
                   + self.C * self.E * self.Cw * self.Fphi_bar))
#
# ---------------------------------------------------------------
# free
# ---------------------------------------------------------------
#
@dataclass
class FreeFixed(ArbitrarySupport):
    __slots__ = ['L', 'E', 'G', 'J', 'Cw', 'C',
                 'FT_bar', 'FB_bar', 'Fpsi_bar', 'Fphi_bar']
    
    def __init__(self, L:float, E: float, G: float) -> None:
        """
        """
        super().__init__(L, E, G)
    #
    @property
    def Psi0(self) -> float:
        """ Rate of angle of twist """
        CL = self.C * self.L
        return -2 * self.Fpsi_bar / (1 + cosh(CL))
    #
    @property
    def Phi0(self) -> float:
        """ Angle of twist """
        CL = self.C * self.L
        return (-1 * self.Fpsi_bar * sinh(CL) / (self.C * (1.0 + cosh(CL)))
                * (1.0 - CL) - self.Fphi_bar)
#
# ---------------------------------------------------------------
# guide
# ---------------------------------------------------------------
#
@dataclass
class GuidedPinned(ArbitrarySupport):
    __slots__ = ['L', 'E', 'G', 'J', 'Cw', 'C',
                 'FT_bar', 'FB_bar', 'Fpsi_bar', 'Fphi_bar']
    
    def __init__(self, L:float, E: float, G: float) -> None:
        """
        """
        super().__init__(L, E, G)
    #
    @property
    def B0(self) -> float:
        """ Bimoment """
        CL = self.C * self.L
        return -1 * self.FB_bar / cosh(CL)
    #
    @property
    def Phi0(self) -> float:
        """ Angle of twist """
        CL = self.C * self.L
        return (self.FB_bar / (self.G * self.J)
                * (1.0 - cosh(CL)) / cosh(CL)
                - self.Fphi_bar)
#
@dataclass
class GuidedFixed(ArbitrarySupport):
    __slots__ = ['L', 'E', 'G', 'J', 'Cw', 'C',
                 'FT_bar', 'FB_bar', 'Fpsi_bar', 'Fphi_bar']
    
    def __init__(self, L:float, E: float, G: float) -> None:
        """
        """
        super().__init__(L, E, G)
    #
    @property
    def B0(self) -> float:
        """ Bimoment """
        CL = self.C * self.L
        return -1.0 * self.Fpsi_bar * self.C * self.E * self.Cw / sinh(CL)
    #
    @property
    def Phi0(self) -> float:
        """ Angle of twist """
        CL = self.C * self.L
        return (self.Fpsi_bar / self.C * (1.0 - cosh(CL)) / sinh(CL)
                - self.Fphi_bar)


