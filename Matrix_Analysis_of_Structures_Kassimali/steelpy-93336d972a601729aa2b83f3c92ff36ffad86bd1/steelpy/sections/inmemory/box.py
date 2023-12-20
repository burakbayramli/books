# 
# Copyright (c) 2019-2023 steelpy
#
#
# Python stdlib imports
from __future__ import annotations
from array import array
from collections import namedtuple
from dataclasses import dataclass
import math

#

# package imports
from steelpy.sections.process.operations import ShapeProperty
from .operations import SectionBasic, ShapeBasic
from ..process.stress import BeamStress

#
#
points = namedtuple('Points', ['y', 'z'])
#
#
# ----------------------------------------
#      Standard Sections Profiles
# ----------------------------------------
#
#
class Box(SectionBasic):
    __slots__ = ['_labels', '_number', '_title', '_d', '_tw', '_b', '_tb']
    
    def __init__(self):
        """ """
        super().__init__()
        self._d: array = array('f', [])
        self._tw: array = array('f', [])
        self._b: array = array('f', [])
        self._tb: array = array('f', [])
    #
    def __setitem__(self, shape_name: int|str, parameters: list) -> None:
        """
        parameters = [node1, node2, material, section, roll_angle]
        """
        try:
            self._labels.index(shape_name)
            raise Exception('element {:} already exist'.format(shape_name))
        except ValueError:
            self._labels.append(shape_name)
            self._title.append('NULL')
            mnumber = next(self.get_number())
            self._number.append(mnumber)
            #
            self._d.append(parameters[0])
            self._tw.append(parameters[1])
            #
            self._b.append(parameters[2])
            self._tb.append(parameters[3])
    #
    def __getitem__(self, shape_name: str | int):
        """
        """
        try:
            index = self._labels.index(shape_name)
        except ValueError:
            raise Exception(f" section name {shape_name} not found")
        #
        return BoxBasic(name=self._labels[index], 
                        d=self._d[index], tw=self._tw[index],
                        b=self._b[index], tb=self._tb[index])
#
#
@dataclass
class BoxBasic(ShapeBasic):
    """
    Calculate the section properties of a box section

    +   +------+
        | +--+ |
        | |  | |    
    d   | |  | |  
        | |  | |   Z
        | +--+ |   ^
    +   +------+   + > Y
        *  b   *

    Parameters
    ----------
    d  : Section Heigh
    tw : Web thickness
    b  : Base
    tb : Flange thickness

    Returns
    ----------
    area: Section area
    Zc  : Elastic neutral centre
    Yc  : Elastic neutral centre
    Iy  : Second moment of area about mayor axis
    Zey : Elastic modulus about mayor axis
    Zpy : Plastic modulus about mayor axis
    SFy : Shape factor mayor axis
    ry  : Radius of gyration about mayor Axis
    Iz  : Second moment of area about minor axis
    Zez : Elastic modulus about minor axis
    Zpz : Plastic modulus about minor axis
    SFz : Shape factor minor axis
    rz  : Radius of gyration about minor Axis
    SC  : Shear centre
    Cw  : Warping constant

    Notes
    ----------
    Uses formulas from:
    1.- Formulas for stress, strain and strucutral matrices [W.D. Pilkey]
    2.- Roark's formulas for stress and strain [7th Edition]
    3.- Wikipedia

    Examples
    ----------

    """
    #name:str | int
    d: float
    tw: float
    b: float
    tb: float
    type:str = 'box'
    #
    #
    def _stress(self, actions, stress=None, stress_type: str='average'):
        """
        """
        # get section's coordinates
        coord =  self.section_coordinates()
        prop = self.properties()
        #
        # ----------------------------------------------
        # Torsion
        ta = (self.tw + self.tb) * 0.50
        tau_x = [actions.Mx / (2 * ta * (self.b - self.tw) * (self.d - self.tb))
                 for item in coord.y]
        #
        # In Plane
        tau_y = [actions.Fy / prop.area
                 for item in coord.y]
        # Out Plane
        tau_z = [actions.Fz / prop.area
                 for item in coord.z]
        #
        # get bending stress
        sigma_x = [actions.Fx / prop.area for item in coord.y]
        sigma_y = [actions.My * item / prop.Iy for item in coord.z]
        sigma_z = [actions.Mz * item / prop.Iz for item in coord.y]
        #
        stress_out = BeamStress(sigma_x, sigma_y, sigma_z, 
                                tau_x, tau_y, tau_z, coord)
        #
        if stress:
            stress_out = self.add_stress(stress=stress, other=stress_out)
        #
        return stress_out        
    #
    #
    def _properties(self):
        """ """
        # self.units_in = _units_output

        # self.d *= factors[0]
        # self.tw *= factors[0]

        # self.a *= factors[0]
        # self.ta *= factors[0]

        # self.b *= factors[0]
        # self.tb *= factors[0]
        # -------------------------------------------------
        #
        _hi = self.d - 2 * self.tb
        _bi = self.b - 2 * self.tw
        # -------------------------------------------------
        #   Cross-Sectional Area
        area = self.b * self.d - _bi * _hi
        # -------------------------------------------------
        #   Elastic Neutral Centre 
        Zc = self.b / 2.0
        Yc = 0
        # -------------------------------------------------
        #   Shear Centre 
        SCz = 0
        SCy = 0
        # -------------------------------------------------
        #   Warping Constant Cw
        Cw = 0
        # -------------------------------------------------
        #               Section Properties
        # -------------------------------------------------
        #   Second Moment of Area about Mayor Axis
        Iy = ((self.b * self.d ** 3 - _bi * _hi ** 3) / 12.0)
        #   Elastic Modulus about Mayor Axis
        Zey = ((self.b * self.d ** 3 - _bi * _hi ** 3) / (6.0 * self.d))
        #   Plastic Modulus about Mayor Axis
        Zpy = ((self.b * self.d ** 2 - _bi * _hi ** 2) / 4.0)
        #   Shape Factor
        SFy = (1.50 * (self.d * (self.b * self.d ** 2 - _bi * _hi ** 2)) /
               (self.b * self.d ** 3 - _bi * _hi ** 3))
        #   Radius of gyration about Mayor Axis
        ry = math.sqrt(Iy / area)
        # -------------------------------------------------
        #   Second Moment of Area about Minor Axis
        Iz = ((self.d * self.b ** 3 - _hi * _bi ** 3) / 12.0)
        #   Elastic Modulus about Minor Axis
        Zez = ((self.d * self.b ** 3 - _hi * _bi ** 3) / (6.0 * self.b))
        #   Plastic Modulus about Minor Axis
        Zpz = ((self.d * self.b ** 2 - _hi * _bi ** 2) / 4.0)
        #   Shape Factor
        SFz = (1.50 * (self.b * (self.d * self.b ** 2 - _hi * _bi ** 2)) /
               (self.d * self.b ** 3 - _hi * _bi ** 3))
        #   Radius of gyration about Minor Axis 
        rz = math.sqrt(Iz / area)
        # -------------------------------------------------
        #   Torsional Constant
        # Mean corner radious
        _Rc = 1.50 * (self.tw + self.tb) / 2.0
        # Mid countour length
        _p = (2 * ((self.d - self.tb) + (self.b - self.tw))
              - 2 * _Rc ** 2 * (4 - math.pi))
        # Enclosed Area
        _Ap = ((self.d - self.tb) * (self.b - self.tw)
               - _Rc ** 2 * (4 - math.pi))
        # for thin walled sections b/t >= 10
        J = ((4 * _Ap ** 2 * ((self.tw + self.tb) / 2.0)) / _p)
        # -------------------------------------------------
        #   Product of inertia
        _Iyz = 0.0
        Jx = Iy + Iz
        rp = math.sqrt(Jx / area)
        #
        return ShapeProperty(area=area, Zc=Zc, Yc=Yc,
                           Iy=Iy, Zey=Zey, Zpy=Zpy, ry=ry,
                           Iz=Iz, Zez=Zez, Zpz=Zpz, rz=rz,
                           J=J, Cw=Cw)

    #
    def curved(self, R):
        """
        ---------
        R = Radio
        """
        _b = self.b
        _b1 = 2 * self.tw
        _t = self.tb
        _d = self.d

        # shear area
        _warea = self.area

        # extreme fibre distances c
        _c = _D / 2.0

        _c1 = _d - _c

        # centroidal radius
        _R = R
        # _R = orad - _c1

        # Shift of neutral axis from neutral axis
        _e = (_c * ((_R / _c) - ((2.0 * (_t / _c + (1 - _t / _c) * (_b1 / _b)))
                                 / ((math.log(((_R / _c) ** 2 + (_R / _c + 1) * (_t / _c) - 1.0)
                                              / ((_R / _c) ** 2 - (_R / _c - 1.0) * (_t / _c) - 1.0)))
                                    + ((_b1 / _b) * math.log((_R / _c - _t / _c + 1.0)
                                                             / (_R / _c + _t / _c - 1.0)))))))

        # where
        _Ic = self.Iy

        # stress factors Ki
        self.ki = ((_Ic / (_warea * _c ** 2 * (_R / _c - 1.0)))
                   * ((1.0 - _e / _c) / (_e / _c)))

        # stress factors Ko
        self.ko = ((_Ic / (_warea * _c ** 2 * (_R / _c + 1.0)))
                   * ((1.0 + _e / _c) / (_e / _c)))

        # Modulus of rigidity factor (section 8.10)
        _nai = _c - _e  # neautral axis inner fiber
        _nao = _c1 + _e  # neautral axis outer fiber

        _D1 = _nai - _Tfb
        _D2 = _nai
        _t1 = 2 * _Tw
        _t2 = _Bfb
        _r = _rip

        self.F = ((1 + (((3 * (_D2 ** 2 - _D1 ** 2) * _D1) / (2.0 * _D2 ** 3)) * (_t2 / _t1 - 1.0)))
                  * (4 * _D2 ** 2 / (10 * _r ** 2)))
        #
        # Shear factor (section 8.1 equ 8.1-13)

    #
    def _dimension(self) -> str:
        """ Print section dimensions"""
        out = "{:<32s}{:1.4e} {:1.4e} {:1.4e}\n" \
            .format(self.type, self.d, self.b, self.b)
        out += "{:<48s}{:1.4e} {:1.4e} {:1.4e}\n" \
            .format("", self.tw, self.tb, self.tb)
        return out
    #
    #
    def section_coordinates(self):
        """
        1 2  3   4 5
        +-+--+---+-+
        |    :     |       ^ z
      6 +    :     + 7     |
        |    :     |       |
      8 +    :     + 9     +--> y
        |    :     |
     10 +    :     + 11
        |    :     | 
        +-+--+---+-+      
       12 13 14 15 16
        """
        # horizontal
        coord_y = [-1 * (self.b - self.tw * 0.50), #1
                   -1 * (self.b - self.tw), 0,     #2, 3
                   (self.b - self.tw),            #4
                   (self.b - self.tw * 0.50),     #5
                   -1 * (self.b - self.tw * 0.50), #6
                   (self.b - self.tw * 0.50),     #7
                   -1 * (self.b - self.tw * 0.50), #8
                   (self.b - self.tw * 0.50),     #9
                   -1 * (self.b - self.tw * 0.50), #10
                   (self.b - self.tw * 0.50),     #11
                   -1 * (self.b - self.tw * 0.50), #12
                   -1 * (self.b - self.tw), 0,     #13, 14
                   (self.b - self.tw), #15
                   (self.b - self.tw * 0.50)] #16
        # vertical
        top1 = (self.d - self.tb * 0.50)
        top2 = (self.d - self.tb)
        coord_z = [top1, top1, top1, top1, top1, #1-5
                   top2, top2,  #6,7
                   0, 0,        # 8,9
                   -1 * top2, -1 * top2, # 10,11
                   -1 * top1, -1 * top1, -1 * top1,
                   -1 * top1, -1 * top1]
        
        return points(coord_y, coord_z)
    #