#
# Copyright (c) 2019-2023 steelpy
#
# Python stdlib imports
from __future__ import annotations
#from array import array
#from collections.abc import Mapping
from dataclasses import dataclass
from math import factorial
from typing import NamedTuple
import re
#
# package imports
#
#

#
#
# ---------------------------------------------------------------
#
# 
#
@dataclass
class BeamBendingSupports:
    """Beam Bending Initial Parameters """

    __slots__ = ['L', 'E', 'I', '_support0']

    def __init__(self, L:float, I: float, E:float = 2.05e11): #
        """
        L : beam lenght [m]
        E : Elastic modulus [Pa] (default steel)
        """
        self.L:float = L
        self.I:float = I
        self.E:float = E
    #
    def supports(self, supp1:str, supp2:str,
                 k1:float|None=None, k2:float|None=None):
        """
        support1 : suppor 1 (left) [pinned/fixed/guide/spring]
        support2 : suppor 2 (right) [pinned/fixed/guide/spring]
        if spring :
        k1 : spring end 1 [N/m]
        k2 : spring end 2 [N/m]
        """
        self._support0 = self.support_func(supp1, supp2, k1, k2)
    #
    def support_func(self, supp1:str, supp2:str, 
                     k1:float|None=None, k2:float|None=None):
        """
        support1 : suppor 1 (left) [pinned/fixed/guide/spring]
        support2 : suppor 2 (right) [pinned/fixed/guide/spring]
        if spring :
        k1 : spring 1 [N/m]
        k2 : spring 2 [N/m]
        """
        if re.match(r"\b(pinn(ed)?)\b", supp1, re.IGNORECASE):
            if re.match(r"\b(pinn(ed)?)\b", supp2, re.IGNORECASE):
                return PinnedPinned(self.L, self.E)
            elif re.match(r"\b(fix(ed)?|encastre(r)?)\b", supp2, re.IGNORECASE):
                return PinnedFixed(self.L, self.E)
            elif re.match(r"\b(guide(d)?)\b", supp2, re.IGNORECASE):
                return PinnedGuided(self.L, self.E)
            elif re.match(r"\b(spring)\b", supp2, re.IGNORECASE):
                if not k2:
                    raise IOError("Spring k2 value missing")
                return PinnedSpring(self.L, self.E, k1, k2)
            else:
                raise IOError("unstable")
        
        elif re.match(r"\b(fix(ed)?|encastre(r)?)\b", supp1, re.IGNORECASE):
            if re.match(r"\b(pinn(ed)?)\b", supp2, re.IGNORECASE):
                return FixedPinned(self.L, self.E)
            elif re.match(r"\b(fix(ed)?|encastre(r)?)\b", supp2, re.IGNORECASE):
                return FixedFixed(self.L, self.E)
            elif re.match(r"\b(guide(d)?)\b", supp2, re.IGNORECASE):
                return FixedGuided(self.L, self.E)
            elif re.match(r"\b(free)\b", supp2, re.IGNORECASE):
                return FixedFree(self.L, self.E)
            elif re.match(r"\b(spring)\b", supp2, re.IGNORECASE):
                if not k2:
                    raise IOError("Spring k2 value missing")
                return FixedSpring(self.L, self.E, k1, k2)
            else:
                raise IOError(f"boundary {supp2} not supported")

        elif re.match(r"\b(free)\b", supp1, re.IGNORECASE):
            if re.match(r"\b(fix(ed)?|encastre(r)?)\b", supp2, re.IGNORECASE):
                return FreeFixed(self.L, self.E)
            elif re.match(r"\b(spring)\b", supp2, re.IGNORECASE):
                if not k2:
                    raise IOError("Spring k2 value missing")
                return FreeSpring(self.L, self.E, k1, k2)
            else:
                raise IOError("unstable")

        elif re.match(r"\b(guide(d)?)\b", supp1, re.IGNORECASE):
            if re.match(r"\b(pinn(ed)?)\b", supp2, re.IGNORECASE):
                return GuidedPinned(self.L, self.E)
            elif re.match(r"\b(fix(ed)?|encastre(r)?)\b", supp2, re.IGNORECASE):
                return GuidedFixed(self.L, self.E)
            elif re.match(r"\b(spring)\b", supp2, re.IGNORECASE):
                if not k2:
                    raise IOError("Spring k2 value missing")
                return GuidedSpring(self.L, self.E, k1, k2)
            else:
                raise IOError("unstable")

        elif re.match(r"\b(spring)\b", supp1, re.IGNORECASE):
            if not k1:
                raise IOError("Spring k1 value missing")
            #
            if re.match(r"\b(pinn(ed)?)\b", supp2, re.IGNORECASE):
                return SpringPinned(self.L, self.E, k1, k2)
            elif re.match(r"\b(fix(ed)?|encastre(r)?)\b", supp2, re.IGNORECASE):
                return SpringFixed(self.L, self.E, k1, k2)
            elif re.match(r"\b(guide(d)?)\b", supp2, re.IGNORECASE):
                return SpringGuided(self.L, self.E, k1, k2)
            elif re.match(r"\b(free)\b", supp2, re.IGNORECASE):
                return SpringFree(self.L, self.E, k1, k2)
            elif re.match(r"\b(spring)\b", supp2, re.IGNORECASE):
                if not k2:
                    raise IOError("Spring k2 value missing")
                return SpringSpring(self.L, self.E, k1, k2)
            else:
                raise IOError(f"boundary {supp2} not supported")
        
        else:
            raise IOError(f"boundary {supp1} not supported")
    #
    def __call__(self, F_bar:list[float]):
        """
        I : moment of inertia [m^4]
        F_bar : [FV_bar, FM_bar, Ftheta_bar, Fw_bar]

        return :
        Initial Parameters x = 0 (support 1)
        [V0, M0, theta0, w0]
        """
        return self._support0.initial_parameters(I=self.I, F_bar=F_bar)
#
# ---------------------------------------------------------------
# Pilkey 2nd ed
# TABLE 11-2
# PART C: SIMPLE BEAMS WITH ARBITRARY LOADING: 
# INITIAL PARAMETERS
# ---------------------------------------------------------------
#
#
@dataclass
class ArbitrarySupport:
    __slots__ = ['L', 'E', 'I', 
                 'FV_bar', 'FM_bar', 'Ftheta_bar', 'Fw_bar']
                 # '_response', 'V1', 'M1', 'w1', 'theta1']

    def __init__(self, L:float, E: float) -> None:
        """
        Pilkey 2nd ed
        TABLE 11-2
        PART C: SIMPLE BEAMS WITH ARBITRARY LOADING: INITIAL PARAMETER
        """
        self.L:float = L
        self.E:float = E
        #self.I:float = I
        #
    #
    @property
    def V0(self) -> float:
        """ Shear """
        return 0
    #
    @property
    def M0(self) -> float:
        """ Bending moment"""
        return 0
    #
    @property
    def theta0(self) -> float:
        """ Slope"""
        return 0
    #
    @property
    def w0(self) -> float:
        """ Deflection"""
        return 0
    #
    #
    def _spring_1(self, k1:float):
        """ """
        #
        A7 = 1 + k1 * self.L / (3*self.E*self.I)
        A8 = 1/self.L + k1 / (2*self.E*self.I)
        A9 = 1 + k1 * self.L / (4*self.E*self.I)
        A10 = 1 + k1 * self.L / (self.E*self.I)
        A11 = 3*self.E*self.I /self.L**3  + 3*k1 /self.L**2
        return  A7,A8,A9,A10,A11
    #
    def _spring_2(self, k2:float):
        """ """
        A1 = 1/self.L - k2 / (2*self.E*self.I)
        A2 = k2 * self.Ftheta_bar - self.FM_bar
        A3 = 1 - k2 * self.L / (3*self.E*self.I)
        A4 = 1 - k2 * self.L / (4*self.E*self.I)
        A5 = 1 - k2 * self.L / (2*self.E*self.I)
        A6 = 1 - k2 * self.L / (self.E*self.I)
        return A1, A2, A3, A4, A5, A6
    #
    #
    def Fbar(self, FV_bar:float, FM_bar:float, 
             Ftheta_bar:float, Fw_bar:float):
        """ """
        self.FV_bar = FV_bar
        self.FM_bar = FM_bar
        self.Ftheta_bar = Ftheta_bar
        self.Fw_bar = Fw_bar
    #
    def initial_parameters(self, I: float, F_bar:list[float]):
        """
        F_bar : [FV_bar, FM_bar, Ftheta_bar, Fw_bar]
        """
        self.I = I
        self.Fbar(*F_bar)
        return [self.V0, self.M0, self.theta0, self.w0]
#
#
# ---------------------------------------------------------------
# Pinned
# ---------------------------------------------------------------
#
@dataclass
class PinnedPinned(ArbitrarySupport):
    """ """
    __slots__ = [ 'E', 'I', 'FV', 'FM', 'Ftheta', 'Fw' ]

    def __init__(self, L:float, E: float,
                 k1:float|None=None, k2:float|None=None) -> None:
        """
        """
        super().__init__(L, E)
    #
    @property
    def V0(self) -> float:
        """ Shear force"""
        return -1*self.FM_bar/self.L
    #
    @property
    def theta0(self) -> float:
        """ Slope"""
        return self.Fw_bar/self.L + self.FM_bar * self.L/(6*self.E*self.I)
#
@dataclass
class PinnedFixed(ArbitrarySupport):
    """ """
    __slots__ = [ 'E', 'I', 'FV', 'FM', 'Ftheta', 'Fw' ]

    def __init__(self, L:float, E: float,
                 k1:float|None=None, k2:float|None=None) -> None:
        """
        """
        super().__init__(L, E)
    #
    @property
    def V0(self) -> float:
        """ Shear force"""
        return (-3*self.E*self.I/self.L**3 * self.Fw_bar
                - 3*self.E*self.I/self.L**2  * self.Ftheta_bar)
    #
    @property
    def theta0(self) -> float:
        """ Slope"""
        return 3*self.Fw_bar/(2*self.L) + 0.50 * self.Ftheta_bar
#
@dataclass
class PinnedGuided(ArbitrarySupport):
    """ """
    __slots__ = [ 'E', 'I', 'FV', 'FM', 'Ftheta', 'Fw' ]

    def __init__(self, L:float, E: float,
                 k1:float|None=None, k2:float|None=None) -> None:
        """
        """
        super().__init__(L, E)
    #
    @property
    def V0(self) -> float:
        """ Shear force"""
        return -1*self.FV_bar
    #
    @property
    def theta0(self) -> float:
        """ Slope"""
        return self.L**2/(2*self.E*self.I) * self.FV_bar - self.Ftheta_bar
#
@dataclass
class PinnedSpring(ArbitrarySupport):
    """ """
    __slots__ = ['E', 'I', 'FV', 'FM', 'Ftheta', 'Fw', 'k2', 'k1']

    def __init__(self, L:float, E: float,
                 k1:float|None, k2:float|None) -> None:
        """
        """
        super().__init__(L, E)
        # spring stiffness
        self.k1 = k1
        self.k2 = k2
    #
    @property
    def theta0(self) -> float:
        """ Bending moment"""
        A1, A2, A3, A4, A5, A6 = self._spring_2(self.k2)
        return (A1*self.Fw_bar - self.L*A2/(6*self.E*self.I))/A3
    
    #
    @property
    def V0(self) -> float:
        """ Shear """
        A1, A2, A3, A4, A5, A6 = self._spring_2(self.k2)
        return ((self.k2/self.L**2)* self.Fw_bar + A2/self.L)/A3
#
# ---------------------------------------------------------------
# Fixed
# ---------------------------------------------------------------
#
@dataclass
class FixedPinned(ArbitrarySupport):
    """ """
    __slots__ = [ 'E', 'I', 'FV', 'FM', 'Ftheta', 'Fw' ]

    def __init__(self, L:float, E: float,
                 k1:float|None=None, k2:float|None=None) -> None:
        """
        """
        super().__init__(L, E)
    #
    @property
    def V0(self) -> float:
        """ Shear force"""
        return -3*self.E*self.I/self.L**3 * self.Fw_bar - 3/(2*self.L) * self.FM_bar
    #
    @property
    def M0(self) -> float:
        """ Bending moment"""
        return 3*self.E*self.I/self.L**2 * self.Fw_bar + 0.50*self.FM_bar
#
@dataclass
class FixedFixed(ArbitrarySupport):
    """ """
    __slots__ = [ 'E', 'I', 'FV', 'FM', 'Ftheta', 'Fw' ]

    def __init__(self, L:float, E: float,
                 k1:float|None=None, k2:float|None=None) -> None:
        """
        """
        super().__init__(L, E)

    #
    @property
    def V0(self) -> float:
        """ Shear force"""
        return (-12 * self.E * self.I / self.L**3 * self.Fw_bar
                - 6 * self.E * self.I / self.L**2 * self.Ftheta_bar)

    #
    @property
    def M0(self) -> float:
        """ Bending moment"""
        return (6 * self.E * self.I / self.L**2 * self.Fw_bar
                + 2 * self.E * self.I / self.L * self.Ftheta_bar)
#
#
@dataclass
class FixedFree(ArbitrarySupport):
    """ """
    __slots__ = [ 'E', 'I', 'FV', 'FM', 'Ftheta', 'Fw' ]

    def __init__(self, L:float, E: float,
                 k1:float|None=None, k2:float|None=None) -> None:
        """
        """
        super().__init__(L, E)

    #
    @property
    def V0(self) -> float:
        """ Shear force"""
        return -1*self.FV_bar
    #
    @property
    def M0(self) -> float:
        """ Bending moment"""
        return self.L * self.FV_bar - self.FM_bar
#
@dataclass
class FixedGuided(ArbitrarySupport):
    """ """
    __slots__ = [ 'E', 'I', 'FV', 'FM', 'Ftheta', 'Fw' ]

    def __init__(self, L:float, E: float,
                 k1:float|None=None, k2:float|None=None) -> None:
        """
        """
        super().__init__(L, E)
    #
    @property
    def V0(self) -> float:
        """ Shear force"""
        return -1*self.FV_bar
    #
    @property
    def M0(self) -> float:
        """ Bending moment"""
        return - self.E*self.I/self.L * self.Ftheta_bar + 0.50 * self.FV_bar * self.L
#
@dataclass
class FixedSpring(ArbitrarySupport):
    """ """
    __slots__ = ['E', 'I', 'FV', 'FM', 'Ftheta', 'Fw', 'k2', 'k1']

    def __init__(self, L:float, E: float,
                 k1:float|None, k2:float|None) -> None:
        """
        """
        super().__init__(L, E)
        # spring stiffness
        self.k1 = k1
        self.k2 = k2
    #
    @property
    def V0(self) -> float:
        """ Shear """
        A1, A2, A3, A4, A5, A6 = self._spring_2(self.k2)
        return ((-3*self.E*self.I*A5 / self.L**3) * self.Fw_bar + 3*A2 / (2*self.L))/A4
    
    #
    @property
    def M0(self) -> float:
        """ Bending moment"""
        A1, A2, A3, A4, A5, A6 = self._spring_2(self.k2)
        return ((3*self.E*self.I / self.L**2 )* A6 * self.Fw_bar - A2/2) / A4
#
# ---------------------------------------------------------------
# free
# ---------------------------------------------------------------
#
@dataclass
class FreeFixed(ArbitrarySupport):
    """ """
    __slots__ = [ 'E', 'I', 'FV', 'FM', 'Ftheta', 'Fw' ]

    def __init__(self, L:float, E: float,
                 k1:float|None=None, k2:float|None=None) -> None:
        """
        """
        super().__init__(L, E)

    #
    @property
    def theta0(self) -> float:
        """ Bending moment"""
        return -1*self.Ftheta_bar
    
    #
    @property
    def w0(self) -> float:
        """ Shear force"""
        return -1*self.Fw_bar - self.L * self.Ftheta_bar
#
@dataclass
class FreeSpring(ArbitrarySupport):
    """ """
    __slots__ = ['E', 'I', 'FV', 'FM', 'Ftheta', 'Fw', 'k2', 'k1']

    def __init__(self, L:float, E: float,
                 k1:float|None, k2:float|None) -> None:
        """
        """
        super().__init__(L, E)
        # spring stiffness
        self.k1 = k1
        self.k2 = k2
    #
    @property
    def theta0(self) -> float:
        """ Slope"""
        A1, A2, A3, A4, A5, A6 = self._spring_2(self.k2)
        return -1*self.Fw_bar - self.L*A2/self.k2
    
    #
    @property
    def w0(self) -> float:
        """ Deflection"""
        A1, A2, A3, A4, A5, A6 = self._spring_2(self.k2)
        return - A2/self.k2
#
# ---------------------------------------------------------------
# guide
# ---------------------------------------------------------------
#
@dataclass
class GuidedPinned(ArbitrarySupport):
    """ """
    __slots__ = [ 'E', 'I', 'FV', 'FM', 'Ftheta', 'Fw' ]

    def __init__(self, L:float, E: float,
                 k1:float=0, k2:float=0) -> None:
        """
        """
        super().__init__(L, E)
    #
    @property
    def M0(self) -> float:
        """ Bending moment"""
        return -1* self.FM_bar
    
    #
    @property
    def w0(self) -> float:
        """ Shear force"""
        return -1* self.Fw_bar -  0.50  * self.L**2 / (2*self.E*self.I) * self.FM_bar
#
@dataclass
class GuidedFixed(ArbitrarySupport):
    """ """
    __slots__ = [ 'E', 'I', 'FV', 'FM', 'Ftheta', 'Fw' ]

    def __init__(self, L:float, E: float,
                 k1:float=0, k2:float=0) -> None:
        """
        """
        super().__init__(L, E)
    #
    @property
    def M0(self) -> float:
        """ Bending moment"""
        return - self.E*self.I/self.L * self.Ftheta_bar
    
    #
    @property
    def w0(self) -> float:
        """ Shear force"""
        return -1* self.Fw_bar -  0.50 * self.Ftheta_bar * self.L
#
@dataclass
class GuidedSpring(ArbitrarySupport):
    """ """
    __slots__ = ['E', 'I', 'FV', 'FM', 'Ftheta', 'Fw', 'k2', 'k1']

    def __init__(self, L:float, E: float,
                 k1:float|None, k2:float|None) -> None:
        """
        """
        super().__init__(L, E)
        # spring stiffness
        self.k1 = k1
        self.k2 = k2
    #
    @property
    def theta0(self) -> float:
        """ Slope"""
        A1, A2, A3, A4, A5, A6 = self._spring_2(self.k2)
        return (-A6 * self.Fw_bar + A2* (self.L**2 /(2*self.E*self.I))) / A6
    
    #
    @property
    def M0(self) -> float:
        """ Bending moment"""
        A1, A2, A3, A4, A5, A6 = self._spring_2(self.k2)
        return A2 / A6
#
# ---------------------------------------------------------------
# Spring
# ---------------------------------------------------------------
#
@dataclass
class SpringPinned(ArbitrarySupport):
    """ """
    __slots__ = ['E', 'I', 'FV', 'FM', 'Ftheta', 'Fw', 'k1']

    def __init__(self, L:float, E: float,
                 k1:float, k2:float|None=None) -> None:
        """
        """
        super().__init__(L, E)
        # spring stiffness
        self.k1 = k1
        #self.k2 = k2    
    #
    @property
    def theta0(self) -> float:
        """ Slope"""
        A7,A8,A9,A10,A11 = self._spring_1(self.k1)
        return (self.Fw_bar/self.L + self.L*self.FM_bar/(6*self.E*self.I))/A7
    
    #
    @property
    def V0(self) -> float:
        """ Shear """
        A7,A8,A9,A10,A11 = self._spring_1(self.k1)
        return -(self.k1/self.L**2 * self.Fw_bar + self.FM_bar * A8)/A7
    #
    @property
    def M0(self) -> float:
        """ Bending moment"""
        return self.k1 * self.theta0
#
@dataclass
class SpringFixed(ArbitrarySupport):
    """ """
    __slots__ = ['E', 'I', 'FV', 'FM', 'Ftheta', 'Fw', 'k1']

    def __init__(self, L:float, E: float,
                 k1:float, k2:float|None=None) -> None:
        """
        """
        super().__init__(L, E)
        # spring stiffness
        self.k1 = k1
        #self.k2 = k2
    #
    @property
    def theta0(self) -> float:
        """ Slope"""
        A7,A8,A9,A10,A11 = self._spring_1(self.k1)
        return (3*self.Fw_bar/(2*self.L) + self.Ftheta_bar/2) / A9
    
    #
    @property
    def V0(self) -> float:
        """ Shear """
        A7,A8,A9,A10,A11 = self._spring_1(self.k1)
        return -(A11 * self.Fw_bar + (3*self.E*self.I/self.L) * self.Ftheta_bar * A8)/A9
    #
    @property
    def M0(self) -> float:
        """ Bending moment"""
        return self.k1 * self.theta0
#
@dataclass
class SpringFree(ArbitrarySupport):
    """ """
    __slots__ = ['E', 'I', 'FV', 'FM', 'Ftheta', 'Fw', 'k1']

    def __init__(self, L:float, E: float,
                 k1:float, k2:float|None=None) -> None:
        """
        """
        super().__init__(L, E)
        # spring stiffness
        self.k1 = k1
        #self.k2 = k2    
    #
    @property
    def theta0(self) -> float:
        """ Slope"""
        #A7,A8,A9,A10,A11 = self._spring_1(self.k1, L)
        return 1/self.k1 * (self.FM_bar - self.L*self.FV_bar)
    
    #
    @property
    def V0(self) -> float:
        """ Shear """
        #A7,A8,A9,A10,A11 = self._spring_1(self.k1, L)
        return -1* self.FV_bar
    #
    @property
    def M0(self) -> float:
        """ Bending moment"""
        return self.k1 * self.theta0
#
@dataclass
class SpringGuided(ArbitrarySupport):
    """ """
    __slots__ = ['E', 'I', 'FV', 'FM', 'Ftheta', 'Fw', 'k1']

    def __init__(self, L:float, E: float,
                 k1:float, k2:float|None=None) -> None:
        """
        """
        super().__init__(L, E)
        # spring stiffness
        self.k1 = k1
        #self.k2 = k2
    #
    @property
    def theta0(self) -> float:
        """ Slope"""
        A7,A8,A9,A10,A11 = self._spring_1(self.k1)
        return (-1*self.Ftheta_bar + (self.L**2/(2*self.E*self.I) * self.FV_bar ))/ A10
    
    #
    @property
    def V0(self) -> float:
        """ Shear """
        #A7,A8,A9,A10,A11 = self._spring_1(self.k1, L)
        return -1* self.FV_bar
    #
    @property
    def M0(self) -> float:
        """ Bending moment"""
        return self.k1 * self.theta0
#
@dataclass
class SpringSpring(ArbitrarySupport):
    """ """
    __slots__ = ['E', 'I', 'FV', 'FM', 'Ftheta', 'Fw',
                 'k2', 'k1']

    def __init__(self, L:float, E: float,
                 k1:float, k2:float) -> None:
        """
        """
        super().__init__(L, E)
        # spring stiffness
        self.k1 = k1
        self.k2 = k2
    #
    @property
    def theta0(self) -> float:
        """ Slope"""
        A1, A2, A3, A4, A5, A6 = self._spring_2(self.k2)
        A12, A13 = self._spring_12(self.k1, self.k2, self.L)
        return (A5/self.L * self.Fw_bar - self.L*A2 / (6*self.E*self.I))/A13
    
    #
    @property
    def V0(self) -> float:
        """ Shear """
        A1, A2, A3, A4, A5, A6 = self._spring_2(self.k2)
        A7,A8,A9,A10,A11 = self._spring_1(self.k1)
        A12, A13 = self._spring_12(self.k1, self.k2, self.L)
        return (A12 * self.Fw_bar + A2 * A8) / A13
    #
    @property
    def M0(self) -> float:
        """ Bending moment"""
        return self.k1 * self.theta0
    #
    def _spring_12(self, k1:float, k2:float, L:float):
        """ """
        A12 = (k2 - k1)/L**2 +  k2*k1 / (self.E*self.I*L)
        A13 = (1 + k1 * L / (3*self.E*self.I) 
               - k2 * L / (3*self.E*self.I) 
               -  k1 * k2 * L**2 / (12* (self.E*self.I)**2))
        return  A12, A13    
#
# ---------------------------------------------------------------
#
class Parameters(NamedTuple):
    """ """
    A1:float
    A2:float
    A3:float
    A4:float
    A5:float
    A6:float
    #
    A7:float
    A8:float
    A9:float
    A10:float
    A11:float
    #
    A12:float
    A13:float
#
#