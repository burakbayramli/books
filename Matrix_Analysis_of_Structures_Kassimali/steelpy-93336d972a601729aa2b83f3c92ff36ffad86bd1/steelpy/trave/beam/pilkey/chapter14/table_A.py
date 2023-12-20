#
# Copyright (c) 2019-2023 steelpy
#
# Python stdlib imports
from __future__ import annotations
from dataclasses import dataclass
from math import sinh, cosh, sqrt
#from typing import NamedTuple
#import re
#
# package imports

#
#
#
# -----------------------------------------------------------------
#    Pilkey 2nd ed
#    TABLE 14-1
#    PART A: TWISTING OF THIN-WALLED BEAMS WITH ARBITRARY LOADINGS:
#    GENERAL RESPONSE EXPRESSIONS
# -----------------------------------------------------------------
#
#
#
@dataclass
class TorsionOpenGE:
    """
    Pilkey 2nd ed
    TABLE 14-1
    PART A: TWISTING OF THIN-WALLED BEAMS WITH ARBITRARY LOADINGS:
            GENERAL RESPONSE EXPRESSIONS
    """

    #__slots__ = ['FV', 'FM', 'Ftheta', 'Fw',
    #             'V0', 'M0', 'theta0', 'w0', 'E']

    def __init__(self, E: float, G: float, J:float, Cw:float) -> None:
        """
        E : Elastic modulus
        G : Shear modulus
        """
        self.E = E
        self.G = G
        self.J = J
        self.Cw = Cw
    #
    def load(self, FT: float, FB: float,
             Fpsi: float, Fphi: float, FTw: float) -> None:
        """
        Load at x distance from end 1
        
        FT : Twisting moment
        FB : Bimoment
        Fpsi : Rate of angle of twist
        Fphi: Angle of twist
        Ftw : Warping torque
        """
        self.FT = FT
        self.FB = FB
        self.Fpsi = Fpsi
        self.Fphi = Fphi
        self.FTw =  FTw
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
        self.Tw0 = Tw0
    #
    def C(self, J: float, Cw:float):
        """ """
        #try:
        return sqrt(self.G * J / (self.E * Cw))
        #except ZeroDivisionError:
        #    return 0
    #
    def phi(self, x: float, J: float, Cw:float) -> float:
        """ Angle of twist (rad)"""
        C = self.C(J=J, Cw=Cw)
        Cx = C * x
        return (self.Phi0 - self.Psi0 * sinh(Cx) / C
                + self.T0 * (Cx - sinh(Cx)) / (C * self.G * J)
                + self.B0 * (1.0 - cosh(Cx)) / (self.G * J)
                + self.Fphi)

    #
    def psi(self, x: float, J: float, Cw:float) -> float:
        """ Rate of angle of twist (rad/m)"""
        C = self.C(J=J, Cw=Cw)
        Cx = C * x
        return (self.Psi0 * cosh(Cx)
                - self.T0 * (1.0 - cosh(Cx)) / (self.G * J)
                + self.B0 * sinh(Cx) / (C * self.E * Cw)
                + self.Fpsi)

    #
    def T(self, x: float) -> float:
        """ Total twisting moment (N-m)"""
        return self.T0 + self.FT

    #
    def B(self, x: float, J: float, Cw:float) -> float:
        """ Bimoment (N-m^2)"""
        C = self.C(J=J, Cw=Cw)
        Cx = C * x
        return (self.Psi0 * C * self.E * Cw * sinh(Cx)
                + self.T0 * sinh(Cx) / C
                + self.B0 * cosh(Cx)
                + self.FB)

    #
    def Tw(self, x: float, J: float, Cw:float) -> float:
        """ Warping torque (N-m)"""
        C = self.C(J=J, Cw=Cw)
        Cx = C * x
        return (self.Psi0 * self.G * J * cosh(Cx)
                + self.T0 * cosh(Cx)
                + self.B0 * C * sinh(Cx)
                + self.FT + self.G * J * self.Fpsi)
    #
    def response(self, x:float) -> list[float]:
        """
        x : distance from end 1

        Return:
        FT : Twisting moment
        FB : Bimoment
        Fpsi : Rate of angle of twist
        Fphi: Angle of twist
        Ftw : Warping torque
        
        [T, B, Psi, Phi, Tw]
        """
        return [self.T(x),
                self.B(x, self.J, self.Cw),
                self.psi(x, self.J, self.Cw),
                self.phi(x, self.J, self.Cw),
                self.Tw(x, self.J, self.Cw)]
#
#