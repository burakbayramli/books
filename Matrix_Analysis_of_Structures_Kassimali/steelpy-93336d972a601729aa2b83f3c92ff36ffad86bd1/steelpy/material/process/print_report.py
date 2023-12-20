# 
# Copyright (c) 2018-2023 steelpy


# Python stdlib imports
from __future__ import annotations
#

# package imports

def print_material_header(self):
    """ """
    print("_______________________________________________________________________________________\n")
    print(" "+"\n")
    print("                                  MATERIAL PROPERTIES\n")
    print(" "+"\n")
    print("Name           Fy [N/mm2]  Fu [N/mm2]  E  [N/mm2]  Poisson     Rho[kg/m3]    G  [N/mm2]\n")
    print(".......................................................................................\n")
    print("\n")    

def print_isomat(self):
    """
    """
    output = ("{:1.4E} {:1.4E} {:1.4E} {:1.4E} {:1.4E} {:1.4E}\n"
              .format(self.Fy.convert('megapascal').value, 
                      self.Fu.convert('megapascal').value, 
                      self.E.convert('megapascal').value, 
                      self.poisson, 
                      self.density.convert('kilogram/metre^3').value,
                      self.G.convert('megapascal').value))
    return output