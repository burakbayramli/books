# 
# Copyright (c) 2019-2023 steelpy
#
#
# Python stdlib imports
from __future__ import annotations
from array import array
from dataclasses import dataclass
from collections import namedtuple
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
class Channel(SectionBasic):
    __slots__ = ['_labels', '_number', '_title', 
                 '_d', '_tw', '_b', '_tb']
    
    def __init__(self):
        """ """
        super().__init__()
        self._d: array = array('f', [])
        self._tw: array = array('f', [])        
        self._b: array = array('f', [])
        self._tb: array = array('f', [])  
    #
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
        return ChannelBasic(name=self._labels[index], 
                            d=self._d[index], tw=self._tw[index],
                            b=self._b[index], tb=self._tb[index])
#
#
@dataclass
class ChannelBasic(ShapeBasic):
    """
    Calculate the section properties of a channel section

    +   +-----+
        |         
    D   |         Z
        |         ^
    +   +-----+   + > Y
        *  B  *

    Parameters
    ----------
    D  : Section Heigh
    Tw : Web thickness
    B  : Base
    Tf : Flange thickness

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
    #
    d: float
    tw: float
    b: float
    tb: float
    type:str = 'Channel'
    #
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
        # FIXME: torsion
        tau_x = [actions.Mx * 0
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
    def _properties(self):
        """ """
        # self.a *= factors[0]
        # self.ta *= factors[0]
        _b = self.b - 0.50 * self.tw
        _C = self.b - self.tw
        _h = self.d - self.tb
        _D2 = self.d - 2 * self.tb
        # -------------------------------------------------
        #   Cross-Sectional Area
        area = (_h * self.tw) + (2 * _b * self.tb)
        # -------------------------------------------------
        #   Elastic Neutral Centre 
        Zc = 0.50 * self.d
        Yc = ((2 * self.b ** 2 * self.tb + _D2 * self.tw ** 2) /
              (2 * self.b * self.d - 2 * _D2 * _C))
        # -------------------------------------------------
        #   Shear Centre 
        SCz = 0.50 * self.d
        SCy = ((3 * self.tb * self.b ** 2)
               / (6 * _b * self.tb + _h * self.tw))
        # -------------------------------------------------
        #   Warping Constant Cw
        Cw = ((_b ** 3 * _h ** 2 * self.tb / 12.0) *
              ((2 * _h * self.tw + 3 * _b * self.tb) /
               (_h * self.tw + 6 * _b * self.tb)))
        # -------------------------------------------------
        #               Section Properties
        # -------------------------------------------------
        #   Second Moment of Area about Mayor Axis
        Iy = (self.b * self.d ** 3 - _C * _D2 ** 3) / 12.0
        #   Elastic Modulus about Mayor Axis
        Zey = 2 * Iy / self.d
        #   Plastic Modulus about Mayor Axis
        Zpy = (((self.d ** 2 * self.tw) / 4.0) +
               (self.tb * _C * (self.d - self.tb)))
        #   Shape Factor
        SFy = Zpy * self.d / (2 * Iy)
        #   Radius of gyration about Mayor Axis
        ry = math.sqrt(Iy / area)
        # -------------------------------------------------
        #   Second Moment of Area about Minor Axis
        Iz = ((self.d * self.b ** 3 / 3.0) - (_C ** 3 * _D2 / 3.0)
              - (area * (self.b - Yc) ** 2))
        #   Elastic Modulus about Minor Axis
        Zez = Iz / (self.b - Yc)
        #   Plastic Modulus about Minor Axis
        if (2 * self.tb * _C) > (self.d * self.tw):
            Zpz = ((_C ** 2 * self.tb / 2.0)
                   - (self.d ** 2 * self.tw ** 2 / (8 * self.tb)) +
                   (self.d * self.tw * self.b / 2.0))
        else:
            Zpz = (((self.tw ** 2 * self.d) / 4.0) +
                   (self.tb * _C * (self.b - (self.tb * _C / self.d))))
        #   Shape Factor
        SFz = (Zpz * (self.b - Yc) / Iz)
        #   Radius of gyration about Minor Axis 
        rz = math.sqrt(Iz / area)
        # -------------------------------------------------
        #   Torsional Constant
        J = (2 * _b * self.tb ** 3 + _h * self.tw ** 3) / 3.0
        #   Product of inertia
        _Iyz = 0.0
        Jx = Iy + Iz
        rp = math.sqrt(Jx / area)
        #
        #
        return ShapeProperty(area=area, Zc=Zc, Yc=Yc,
                           Iy=Iy, Zey=Zey, Zpy=Zpy, ry=ry,
                           Iz=Iz, Zez=Zez, Zpz=Zpz, rz=rz,
                           J=J, Cw=Cw)

    #
    @property
    def CoG(self):
        """ """
        _C = self.b - self.tw
        _D2 = self.d - 2 * self.tb        
        # -------------------------------------------------
        #   Elastic Neutral Centre 
        Zc = 0
        Yc = ((2 * self.b ** 2 * self.tb + _D2 * self.tw ** 2) /
              (2 * self.b * self.d - 2 * _D2 * _C))
        #
        return Yc, Zc
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
        _c = (_d * (((_b1 / _b) + (1.0 - (_b1 / _b)) * (_t / _d) ** 2) /
                    (2.0 * ((_b1 / _b) + (1.0 - (_b1 / _b)) * (_t / _d)))))

        _c1 = _c * ((_d / _c) - 1.0)

        # centroidal radius
        _R = R
        # _R = orad - _c

        # Shift of neutral axis from neutral axis
        _e = (_c * ((_R / _c) - (((_d / _c) * (_b1 / _b + (1.0 - _b1 / _b) * (_t / _d))) /
                                 (((_b1 / _b) * math.log((_d / _c + _R / _c - 1.0) /
                                                         ((_d / _c) * (_t / _d) + _R / _c - 1.0))) +
                                  math.log(((_d / _c) * (_t / _d) + _R / _c - 1.0) /
                                           (_R / _c - 1.0))))))

        # where
        _Ic = ((_warea * _c ** 2) * (((((_d / _c) ** 2 * ((_b1 / _b + (1.0 - _b1 / _b) * (_t / _d) ** 3)
                                                          / (_b1 / _b + (1.0 - _b1 / _b) * (_t / _d)))))
                                      / 3.0) - 1.0))

        # stress factors Ki
        self.ki = ((_Ic / (_warea * _c ** 2 * (_R / _c - 1.0)))
                   * ((1.0 - _e / _c) / (_e / _c)))

        # stress factors Ko
        self.ko = ((_Ic / (_warea * _c ** 2 * (_e / _c)))
                   * ((_d / _c + _e / _c - 1.0) / (_R / _c + _d / _c - 1.0))
                   * (1.0 / (_d / _c - 1.0)))

        # Modulus of rigidity factor (section 8.10)
        _F = 1.0

        # Shear factor (section 8.1 equ 8.1-13)
        #

    #
    #
    #
    def section_coordinates(self):
        """
        1      2 3 4
        +------+-+-+
                   |       ^ z
                   + 5     |
                   |       |
                   + 6     +--> y
                   |
                   + 7
                   | 
        +------+-+-+      
        8     9 10 11
        """
        # horizontal
        
        CoG = self.CoG
        #
        hzt1 = (self.b - CoG[0])
        hzt2 = 0
        hzt3 = CoG[0] - self.tw
        hzt4 = CoG[0] - self.tw * 0.50
        #
        coord_y = [-1 * hzt1, hzt2, hzt3, hzt4, # 1,2,3,4
                   hzt4, hzt4, hzt4, # 5,6,7
                   -1 * hzt1, hzt2, hzt3, hzt4] # 8,9,10,11
        # vertical
        top1 = (self.d * 0.50 - self.tb * 0.50)
        top2 = (self.d * 0.50 - self.tb)
        coord_z = [top1, top1, top1, top1, #1-4
                   top2, 0,  # 5,6
                   -1 * top2, # 10,11
                   -1 * top1, -1 * top1, -1 * top1, -1 * top1]
        
        return points(coord_y, coord_z)    
    #
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
#
