# 
# Copyright (c) 2019-2023 steelpy
#
# Python stdlib imports
from dataclasses import dataclass
from typing import NamedTuple #, ClassVar, List
#import sys
import math


# package imports


class Sigma(NamedTuple):
    """
    Cauchy stress tensor
    https://en.wikipedia.org/wiki/Stress_(mechanics)
    """
    sigma_11:float
    sigma_21:float
    sigma_31:float
    #
    sigma_12:float
    sigma_22:float
    sigma_32:float
    #
    sigma_13:float
    sigma_23:float 
    sigma_33:float
    

# TODO: class duplicated
#@dataclass
class Stress:
    """
    General stress class
    """
    __slots__ = ('sigma_x', 'sigma_y', 'sigma_z', 
                 'tau_x', 'tau_y', 'tau_z','_units')    
    
    def __init__(self, sigma_x, sigma_y, sigma_z,
                 tau_x, tau_y, tau_z):
        """
        """ 
        #print('-->')
        self.sigma_x: list = sigma_x
        self.sigma_y: list = sigma_y
        self.sigma_z: list = sigma_z
        self.tau_x:   list = tau_x
        self.tau_y:   list = tau_y
        self.tau_z:   list = tau_z
    
    def von_mises(self) -> list:
        """
        Returns Von-Mises stress
        """
        items = []
        for x in range(len(self.sigma_x)):
            items.append((0.50 * ((self.sigma_x[x] - self.sigma_y[x])**2 +
                                   (self.sigma_y[x] - self.sigma_z[x])**2 +
                                   (self.sigma_z[x] - self.sigma_x[x])**2)
                           + 3 * (self.tau_x[x]**2 + self.tau_y[x]**2
                                  + self.tau_z[x]**2))**0.50)
        return items
    #
    def sigma(self) -> list:
        """
        Cauchy stress tensor
        """
        sigma_items = []
        for x in range(len(self.sigma_x)):
            sigma_items.append(Sigma(self.sigma_x[x], self.tau_x[x], self.tau_y[x],
                                      self.tau_x[x], self.sigma_y[x], self.tau_z[x],
                                      self.tau_y[x], self.tau_z[x], self.sigma_z[x]))
        return sigma_items
    #
    def analyze_stress_state(self, index) -> None:
        """
        """
        import numpy as np

        _sigma_items = self.sigma()
        _items = _sigma_items[index]
        # load the stresses into our matrix and compute the 
        # deviatoric and isotropic stress matricies
        sigma = np.array([[_items.sigma_11.value, _items.sigma_21.value, _items.sigma_31.value],
                          [_items.sigma_12.value, _items.sigma_22.value, _items.sigma_32.value],
                          [_items.sigma_13.value, _items.sigma_23.value, _items.sigma_33.value]])
    
        self.isotropic_stress = 1.0/3.0*np.trace(sigma)*np.eye(3)
        self.deviatoric_stress = sigma - self.isotropic_stress
        # compute principal stresses
        eigvals = list(np.linalg.eigvalsh(sigma))
        eigvals.sort()
        eigvals.reverse()
        # compute max shear stress
        self.maximun_shear = (max(eigvals) - min(eigvals)) / 2.0
        # compute the stress invariants
        I1 = np.trace(sigma)
        J2 = (1.0 / 2.0 * np.trace(np.dot(self.deviatoric_stress,
                                          self.deviatoric_stress)))
        J3 = (1.0 / 3.0 * np.trace(np.dot(self.deviatoric_stress, 
                                          np.dot(self.deviatoric_stress,
                                                 self.deviatoric_stress))))
        # compute other common stress measures
        self.mean_stress = 1.0/3.0*I1
        self.equivalent_stress  = np.sqrt(3.0*J2)
        # compute lode coordinates
        self.lode_r = np.sqrt(2.0*J2)
        self.lode_z = I1/np.sqrt(3.0)

        dum = (3.0 * np.sqrt(6.0)
               * np.linalg.det(self.deviatoric_stress / self.lode_r))
        self.lode_theta = 1.0 / 3.0 * np.arcsin(dum)
        # compute the stress triaxiality
        self.triaxiality = self.mean_stress / self.equivalent_stress
        # Print out what we've found
        headerprint(" Stress State Analysis ")
        matprint("Input Stress",sigma)
        headerprint(" Component Matricies ")
        matprint("Isotropic Stress",self.isotropic_stress)
        matprint("Deviatoric Stress",self.deviatoric_stress)
        headerprint(" Scalar Values ")
        valprint("P1",eigvals[0])
        valprint("P2",eigvals[1])
        valprint("P3",eigvals[2])
        valprint("Max Shear", self.maximun_shear)
        valprint("Mean Stress",self.mean_stress)
        valprint("Equivalent Stress", self.equivalent_stress)
        valprint("I1",I1)
        valprint("J2",J2)
        valprint("J3",J3)
        valprint("Lode z",self.lode_z)
        valprint("Lode r",self.lode_r)
        valprint("Lode theta (rad)",self.lode_theta)
        valprint("Lode theta (deg)",np.degrees(self.lode_theta))
        valprint("Triaxiality",self.triaxiality)
        headerprint(" End Output ")
    #
    #
#
#
def headerprint(string):
    """ Prints a centered string to divide output sections. """
    mywidth = 64
    mychar = "="
    numspaces = mywidth - len(string)
    before = int(math.ceil(float(mywidth-len(string))/2))
    after  = int(math.floor(float(mywidth-len(string))/2))
    print("\n"+before*mychar+string+after*mychar+"\n")

def valprint(string, value):
    """ Ensure uniform formatting of scalar value outputs. """
    print("{0:>30}: {1: .10e}".format(string, value))

def matprint(string, value):
    """ Ensure uniform formatting of matrix value outputs. """
    print("{0}:".format(string))
    print(value)
  
#
#
@dataclass()
class BeamStress:
    """
    beam element stress
    """
    #__slots__ = ['sigma_x', 'sigma_y', 'sigma_z',
    #             'tau_x', 'tau_y', 'tau_z','x']
    
    #def __init__(self, sigma_x, sigma_y, sigma_z,
    #             tau_x, tau_y, tau_z, x):
    """
    """
    sigma_x : list
    sigma_y : list
    sigma_z : list
    tau_x   : list
    tau_y   : list
    tau_z   : list
    stress_point : list|tuple
    
    #def von_mises(self) -> list:
    #    """
    #    Returns Von-Mises stress
    #    """
    #    _items = []
    #    for x in range(len(self.sigma_x)):
    #        _items.append(((self.sigma_x[x] + self.sigma_y[x] + self.sigma_z[x])**2 +
    #                       3 * (self.tau_x[x]**2 + self.tau_y[x]**2 + self.tau_z[x]**2))**0.50)
    #    return _items
#
#
class PlateStress:
    """
    plate element stress
    """
    __slots__ = ['sigma_x', 'sigma_y', 'sigma_z', 
                 'tau_x', 'tau_y', 'tau_z','_units']    
    
    def __init__(self, sigma_x, sigma_y, sigma_z,
                 tau_x, tau_y, tau_z):
        """
        """ 
        #print('-->')
        self.sigma_x: list = sigma_x
        self.sigma_y: list = sigma_y
        self.sigma_z: list = sigma_z
        self.tau_x:   list = tau_x
        self.tau_y:   list = tau_y
        self.tau_z:   list = tau_z
    
    def von_mises(self) -> list:
        """
        Returns Von-Mises stress
        """
        _items = []
        for x in range(len(self.sigma_x)):
            _items.append(((self.sigma_y[x]**2 + self.sigma_z[x]**2 - self.sigma_y[x] * self.sigma_z[x]) +
                           3 * (self.tau_y[x]**2 + self.tau_z[x]**2))**0.50)
        return _items
    #
    def factor_by(self, other:float)->None:
        """ """
        for x in range(len(self.sigma_x)):
            self.sigma_x[x] *= other
            self.sigma_y[x] *= other
            self.sigma_z[x] *= other
            self.tau_x[x] *= other
            self.tau_y[x] *= other
            self.tau_z[x] *= other
    #
    def principal_stresses(self, index:int):
        """ """
        import numpy as np
        #
        _sigma_items = self.sigma()
        _items = _sigma_items[index]
        # load the stresses into our matrix and compute the
        # deviatoric and isotropic stress matricies
        sigma = np.array([[_items.sigma_11.convert('pascal').value,
                           _items.sigma_21.convert('pascal').value,
                           _items.sigma_31.convert('pascal').value],
                          [_items.sigma_12.convert('pascal').value,
                           _items.sigma_22.convert('pascal').value,
                           _items.sigma_32.convert('pascal').value],
                          [_items.sigma_13.convert('pascal').value,
                           _items.sigma_23.convert('pascal').value,
                           _items.sigma_33.convert('pascal').value]])
        # compute principal stresses
        eigvals = list(np.linalg.eigvalsh(sigma))
        eigvals.sort()
        eigvals.reverse()
        # Get principal stresses
        P1 = eigvals[0]
        P2 = eigvals[1]
        P3 = eigvals[2]
        # compute max shear stress
        tau_max = (max(eigvals) - min(eigvals)) / 2.0
        #
        return [P1, P2, P3, tau_max]
    #
    def sigma(self) -> list:
        """
        Cauchy stress tensor
        """
        _sigma_items = []
        for x in range(len(self.sigma_x)):
            _sigma_items.append(Sigma(self.sigma_x[x], self.tau_x[x], self.tau_y[x],
                                      self.tau_x[x], self.sigma_y[x], self.tau_z[x],
                                      self.tau_y[x], self.tau_z[x], self.sigma_z[x]))
        return _sigma_items