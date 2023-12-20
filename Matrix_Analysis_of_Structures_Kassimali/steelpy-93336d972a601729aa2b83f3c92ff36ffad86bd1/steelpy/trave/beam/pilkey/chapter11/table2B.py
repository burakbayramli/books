# 
# Copyright (c) 2019-2023 steelpy
# 

# Python stdlib imports
from __future__ import annotations
from bisect import bisect_right
from dataclasses import dataclass
from math import factorial, cosh, sinh
import re

# package imports
from steelpy.utils.math.vector import Vector

#
# ---------------------------------------------------------------
#
# bload = BeamLoad(L, E) #, I)
# beload['title'] = ['point', P, L1]
# beload['title'] = ['moment', M, L1]
# beload['title'] = ['torsion', T, L1]
# beload['title'] = ['linear', q1, L1, q2, L2]
#
# bload.point('title', P, L1)
# bload.moment('title', M, L1)
# bload.torsion('title', T, L1)
# bload.linear('title', q1, L1, q2, L2)
# load at x distance from end 0 (left support)
# result = bload(x, I, load_name)
#
#
#
class BeamLoad:
    __slots__ = ['L', 'E', 'I', 'L1', '_load', 
                 '_type', '_labels']

    def __init__(self, L: float, E: float = 2.05e11) -> None:
        """
        L : beam lenght [m]
        E : Elastic modulus [Pa] (default steel)
        """
        self.L: float = L
        self.E:float = E
        #
        self._load = []
        #
        self._labels:list[str|int] = []
        self._type:list[str] = []
    #
    #
    def __setitem__(self, load_name:str|int,
                    load:list[str|float]) -> None:
        """
        """
        load_type = load[0]

        if re.match(r"\b(point)\b", load_type, re.IGNORECASE):
            self.point(load_name=load_name, P=load[1], L1=load[2])

        elif re.match(r"\b(moment)\b", load_type, re.IGNORECASE):
            self.moment(load_name=load_name, m=load[1], L1=load[2])

        elif re.match(r"\b(udl|linear)\b", load_type, re.IGNORECASE):
            self.linear(load_name, *load[1:])

        elif re.match(r"\b(torsion)\b", load_type, re.IGNORECASE):
            self.torsion(load_name=load_name, To=load[1], L1=load[2])

        else:
            raise IOError(f"load type {load_type} no supported")

    
    def __getitem__(self, load_name:str|int):
        """
        """
        index = [i for i, item in enumerate(self._labels)
                 if item == load_name]
        #
        #load = []
        #for idx in index:
        #    load += self._load[idx]
        return [self._load[idx] for idx in index]
    #
    #
    def point(self, load_name:str|int, P:float, L1:float):
        """
               |
             P |
        o------V------------------o
        |                          |
        +  L1  +                   +
        """
        self._labels.append(load_name)
        self._load.append(Point(P=P, L=self.L, L1=L1))
        self._type.append("point")

    def moment(self, load_name:str|int, M: float,  L1: float):
        """
               
               M 
        o------@------------------o
        |                         |
        +  L1  +                  +
        """
        self._labels.append(load_name)
        self._load.append(Moment(m=M, L=self.L, L1=L1))
        self._type.append("moment")

    def linear(self, load_name:str|int, q1: float, L1: float, 
               q2: float|None=None, L2: float|None = None):
        """
                        |
             q1         | q2
        o------|        |----------o
        |                          |
        +  L1  +        +    L2    +
        """
        if not q2:
            q2 = q1

        if not L2:
            L2 = L1
        
        self._labels.append(load_name)
        self._load.append(Trapezoidal(q1=q1, q2=q2,
                                      L=self.L, L1=L1, L2=L2))
        self._type.append("linear")

    def torsion(self, load_name:str|int, T:float, L1:float):
        """
               T 
        o------|------------------o
        |                         |
        +  L1  +                  +
        """
        self._labels.append(load_name)
        self._load.append(TorsionPoint(To=T, L=self.L, L1=L1))
        self._type.append("torsion")
    #
    def __call__(self, x: float, I: float, 
                 load_list:str|int|list|None=None):
        """
        x : distance from end 1 [m]
        I : Moment of intertia [m^4]
        load_list : list of load to be included in calculation (default use all)

        return: 
        [V, M, w, theta]
        """
        self.I = I
        # [V, M, w, theta]
        load = Vector([0,0,0,0])
        if not load_list:
            for item in self._load:
                load += item(x=x, E=self.E, I=self.I)
        else:
            for load_name in load_list:
                new_load = self.__getitem__(load_name)
                for item in new_load:
                    load += item(x=x, E=self.E, I=self.I)
            #1/0
        return load

#
#
# ---------------------------------------------------------------
# Pilkey 2nd ed
# TABLE 11-2
# PART B: SIMPLE BEAMS WITH ARBITRARY LOADINGS: 
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
    def q(self, x: float) -> float:
        """ Loading Function"""
        return 0

    #
    def V(self, x: float) -> float:
        """ Shear Force"""
        return 0

    #
    def M(self, x: float) -> float:
        """ Bending Moment"""
        return 0

    #
    def T(self, x: float) -> float:
        """ Torque"""
        return 0

    #
    def theta(self, x: float, E: float, I: float) -> float:
        """ Slope = EIy' """
        return 0

    #
    def phi(self, x: float, E: float, G: float, Cw: float, K: float) -> float:
        """ angle of rotation at a distance x from the left end (radians) """
        return 0

    #
    def w(self, x: float, E: float, I: float) -> float:
        """ Deflection = EIy"""
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
    def max_steps(self):
        """ """
        return [self.L1]
    

    #
    def __call__(self, x: float, E: float, I: float):
        """ 
        Formulas positive (+) is downwards and therefore sign is changed to maintain compatibility
        return: [V, M, theta, w]
        """
        #return Vector([self.V(x), self.M(x),
        #               self.theta(x, E, I), self.w(x, E, I)])
        return [1 * self.V(x), 1 * self.M(x), 1 * self.theta(x, E, I), 1 * self.w(x, E, I)]
#
#
#
@dataclass
class Trapezoidal(ArbitraryLoading):
    __slots__ = ['q1', 'q2', 'L', 'L1', 'L2',
                 '_L3', '_slope']

    def __init__(self, q1: float, q2: float,
                 L: float, L1: float, L2: float) -> None:
        """
        """
        super().__init__(L, L1)
        self.q1: float = q1
        self.q2: float = q2
        self.L2: float = L2
        #
        self._L3 = self.L - self.L2
        self._slope = (self.q2 - self.q1) / (self._L3 - self.L1)

    #
    def q(self, x: float) -> float:
        """ Loading Function"""
        step1 = x - self.L1
        step2 = x - self._L3
        func1 = (self.function_n(step1, 1) - self.function_n(step2, 1)) * -self._slope
        func2 = -self.function_n(step1, 0) * self.q1 + self.function_n(step2, 0) * self.q2
        return func1 + func2

    #
    def V(self, x: float) -> float:
        """ Shear Force"""
        step1 = x - self.L1
        step2 = x - self._L3
        func1 = -self._slope / 2 * (self.function_n(step1, 2) - self.function_n(step2, 2))
        func2 = -self.function_n(step1, 1) * self.q1 + self.function_n(step2, 1) * self.q2
        return func1 + func2

    #
    def M(self, x: float) -> float:
        """ Bending Moment"""
        step1 = x - self.L1
        step2 = x - self._L3
        func1 = - (self._slope / factorial(3) * (self.function_n(step1, 3) - self.function_n(step2, 3)))
        func2 = -0.50 * (self.q1 * self.function_n(step1, 2) - self.q2 * self.function_n(step2, 2))
        return func1 + func2

    #
    def theta(self, x: float, E: float, I: float) -> float:
        """ Slope = EIy' """
        step1 = x - self.L1
        step2 = x - self._L3
        func1 = (self.function_n(step1, 4) - self.function_n(step2, 4)) * -self._slope / (factorial(4) * E * I)
        func2 = -1 / (factorial(3) * E * I) * (
                    self.function_n(step1, 3) * self.q1 - self.function_n(step2, 3) * self.q2)
        return func1 + func2

    #
    def w(self, x: float, E: float, I: float) -> float:
        """ Deflection = EIy"""
        step1 = x - self.L1
        step2 = x - self._L3
        func1 = (self.function_n(step1, 5) - self.function_n(step2, 5)) * self._slope / (factorial(5) * E * I)
        func2 = 1 / (factorial(4) * E * I) * (self.function_n(step1, 4) * self.q1 - self.function_n(step2, 4) * self.q2)
        return func1 + func2

    #
    def max_steps(self):
        """ """
        wl = self.L - self.L1 - self.L2
        try:
            1 / self.q1  # end 1
            try:
                1 / self.q2  # end 2
                if self.q1 == self.q2:  # uniform
                    a = self.L1 + wl / 2.0
                    b = self.L2 + wl / 2.0
                    maxM = (a + wl * (b - a) / (2 * self.L)) / self.L
                else:  # trapezoidal
                    qrad = [0.2, 0.4, 0.6, 0.8, 1.0]
                    xrad = [0.555, 0.536, 0.520, 0.508, 0.50]
                    interp = Interpolate(qrad, xrad)
                    #
                    if abs(self.q1) <= abs(self.q2):
                        rad = interp(self.q1 / self.q2)
                    else:
                        rad = interp(self.q2 / self.q1)
                        rad = 1 - rad
                    maxM = (self.L1 / self.L) + rad
            except ZeroDivisionError:  # triangular
                maxM = (self.L1 / self.L) + (1 - 0.5774)
        except ZeroDivisionError:  # triangular
            maxM = (self.L1 / self.L) + 0.5774
            #
        x_steps = [0, 1 / 4, 3 / 8, 2 / 4, 5 / 8, 3 / 4, 1, maxM]
        x_steps = sorted(list(set(x_steps)))
        x_steps = [item for item in x_steps if item <= 1]
        return x_steps
    #


#
@dataclass
class Point(ArbitraryLoading):
    __slots__ = ['P', 'L', 'L1']

    def __init__(self, P: float, L: float, L1: float):
        """
        """
        super().__init__(L, L1)
        self.P: float = P

    #
    def q(self, x: float) -> float:
        """ Loading Function"""
        step = x - self.L1
        return self.function_n(step, -1) * -self.P

    #
    def V(self, x: float) -> float:
        """ Shear Force"""
        step = x - self.L1
        return -self.P * self.function_n(step, 0)
        #

    def M(self, x: float) -> float:
        """ Bending Moment"""
        step = x - self.L1
        return -self.P * self.function_n(step, 1)

    #
    def theta(self, x: float, E: float, I: float) -> float:
        """ Slope = EIy' """
        step = x - self.L1
        return self.function_n(step, 2) * -self.P / (2 * E * I)

    #
    def w(self, x: float, E: float, I: float) -> float:
        """ Deflection = EIy"""
        step = x - self.L1
        return self.function_n(step, 3) * self.P / (factorial(3) * E * I)

#
#
@dataclass
class Moment(ArbitraryLoading):
    __slots__ = ['m', 'L', 'L1']

    def __init__(self, M: float, L: float, L1: float) -> None:
        """
        """
        super().__init__(L, L1)
        self.m: float = M

    #
    def M(self, x: float) -> float:
        """ Bending Moment"""
        step = x - self.L1
        return -self.m * self.function_n(step, 0)

    #
    def theta(self, x: float, E: float, I: float) -> float:
        """ Slope = EIy' """
        step = x - self.L1
        return -self.m / (E * I) * self.function_n(step, 1)

    #
    def w(self, x: float, E: float, I: float) -> float:
        """ Deflection = EIy"""
        step = x - self.L1
        return self.m / (2 * E * I) * self.function_n(step, 2)


#
#
@dataclass
class TorsionPoint(ArbitraryLoading):
    __slots__ = ['To', 'L', 'L1']

    def __init__(self, To: float, L: float, L1: float):
        """
        T :  Applied torsional load (force-length)

        """
        super().__init__(L, L1)
        self.To: float = To

    #
    def beta(self, E: float, G: float, Cw: float, K: float):
        """E : modulus of elasticity of the material.
        G : Modulus of rigidity (shear modulus) of the material.
        Cw : warping constant for the cross section.
        K : Torsional constant"""
        return (K * G / (Cw * E)) ** 0.50

    #
    @property
    def A(self):
        """ """
        l = self.L
        beta_a = self.beta * self.L1
        A1 = cosh(beta_a)
        A2 = sinh(beta_a)
        return [A1, A2]

    #
    @property
    def C(self):
        """ """
        beta_l = self.beta * self.L
        la = self.L - self.L1
        C1 = cosh(beta_l)
        C2 = sinh(beta_l)
        C3 = cosh(beta_l) - 1.0
        C4 = sinh(beta_l) - self.beta * la
        return [C1, C2, C3, C4]

    #
    @property
    def Ca(self):
        """ """
        la = self.L - self.L1
        Ca1 = cosh(self.beta * la)
        Ca2 = sinh(self.beta * la)
        #
        Ca3 = cosh(self.beta * la) - 1.0
        Ca4 = sinh(self.beta * la) - self.beta * la
        #
        Ca5 = Ca3 - self.beta ** 2 * la ** 2 / 2.0
        Ca6 = Ca4 - self.beta ** 3 * la ** 3 / 6.0
        return [Ca1, Ca2, Ca3, Ca4, Ca5, Ca6]

    #
    def F(self, x: float):
        """ x : distance from the left end"""
        # a = self.a.value
        beta_x = self.beta * x
        F1 = cosh(beta_x)
        F2 = sinh(beta_x)
        F3 = cosh(beta_x) - 1.0
        F4 = sinh(beta_x) - beta_x
        return [F1, F2, F3, F4]

    #
    def Fa(self, x: float):
        """ x : distance from the left end"""
        a = self.L1
        # xa = x - a
        step = x - a
        Fa1 = self.funtion_0(step, 0) * cosh(self.beta * step)
        Fa2 = sinh(self.beta * self.function_n(x, a, 1))
        Fa3 = self.funtion_0(step, 0) * (cosh(self.beta * step) - 1.0)
        Fa4 = (sinh(self.beta * self.function_n(step, 1))
               - (self.beta * self.function_n(step, 1)))
        Fa5 = Fa3 - self.beta ** 2 * self.function_n(step, 2) / 2.0
        Fa6 = Fa4 - self.beta ** 3 * self.function_n(step, 3) / 6.0
        return [Fa1, Fa2, Fa3, Fa4, Fa5, Fa6]

    #
    #
    def T(self, x: float) -> float:
        """ Torque"""
        step = x - self.L1
        return -self.To * self.function_n(step, 0)

    #
    def phi(self, x: float, E: float, G: float, Cw: float, K: float) -> float:
        """ x : distance from the left end
        E : modulus of elasticity of the material.
        G : Modulus of rigidity (shear modulus) of the material.
        Cw : warping constant for the cross section.
        K : Torsional constant
        phi : angle of rotation at a distance x from the left end (radians) """
        Ft = self.T(x)
        F1, F2, F3, F4 = self.F(x)
        Fa1, Fa2, Fa3, Fa4, Fa5, Fa6 = self.Fa(x)
        phi_0 = self.To / (self.Cw * self.E * self.beta ** 3) * Fa4 + Ft * F4 / (self.Cw * self.E * self.beta ** 3)
        phi_1 = self.To / (self.Cw * self.E * self.beta ** 2) * Fa3 + Ft * F3 / (self.Cw * self.E * self.beta ** 2)
        phi_2 = self.To / (self.Cw * self.E * self.beta) * Fa2 + Ft * F2 / (self.Cw * self.E * self.beta)
        phi_3 = self.To / (self.Cw * self.E) * Fa1 + Ft * F1 / (self.Cw * self.E)
        return [phi_0, phi_1, phi_2, phi_3]


#
#
class SingularFunction:
    __slots__ = ['_function', '_n', '_divisor', '_C']

    def __init__(self, function: float, power: int) -> None:
        """ """
        self._function = function
        self._n = power
        self._divisor = 1.0

    #
    # def __pow__(self, power:int, modulo=None):
    #    """ """
    #    self._power = power
    #    if power < 0:
    #        return 0
    #    elif self._function < 0:
    #        return 0
    #    elif power == 0:
    #        return 1
    #    else:
    #        return self._function**power
    #
    def __repr__(self):
        """Return a string representation of self."""
        return '<{:}>^{:}'.format(self._function, self._n)

    #
    def integration(self):
        """ """
        if self._n < 0:
            self._n += 1
        else:
            self._n += 1
            self._divisor = self._n

    #
    def differentiation(self):
        """ """
        if self._n < 0:
            self._n -= 1
        else:
            self._divisor /= self._n
            self._n -= 1


#
#
class Interpolate:
    def __init__(self, x_list, y_list):
        if any(y - x <= 0 for x, y in zip(x_list, x_list[1:])):
            raise ValueError("x_list must be in strictly ascending order!")
        self.x_list = x_list
        self.y_list = y_list
        intervals = zip(x_list, x_list[1:], y_list, y_list[1:])
        self.slopes = [(y2 - y1) / (x2 - x1) for x1, x2, y1, y2 in intervals]

    def __call__(self, x):
        if not (self.x_list[0] <= x <= self.x_list[-1]):
            raise ValueError("x out of bounds!")
        if x == self.x_list[-1]:
            return self.y_list[-1]
        i = bisect_right(self.x_list, x) - 1
        return self.y_list[i] + self.slopes[i] * (x - self.x_list[i])
#
#
