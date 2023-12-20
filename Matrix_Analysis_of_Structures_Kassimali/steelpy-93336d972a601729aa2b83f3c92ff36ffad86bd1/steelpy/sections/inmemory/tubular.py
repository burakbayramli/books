# 
# Copyright (c) 2019-2023 steelpy
#

# Python stdlib imports
from __future__ import annotations
from array import array
from dataclasses import dataclass
from collections import namedtuple
import math
#import re
#

# package imports
from steelpy.utils.io_module.text import search_line
from ..process.operations import ShapeProperty #, get_sect_properties
from ..process.stress import BeamStress
from .operations import SectionBasic, ShapeBasic
from steelpy.utils.dataframe.main import DBframework

#import pandas as pd
#import numpy as np
#
#
#
def find_tubular_dimensions(line_in: str) -> str:
    """
    """
    _key = {"diameter": r"\b(d(iamet(ro|er|re))?(y)?)\b",
            "thickness": r"\b((w(all)?(\_)?)?t(hickness|hk)?)\b"}

    keyWord, line_out, _match = search_line(line_in, _key)
    return keyWord


#
def get_dimension(self, dim: str, value: float):
    """
    """
    # Section Definition
    if dim == 'diameter':
        self.diameter = value
    elif dim == 'thickness':
        self.thickness = value
    else:
        raise IOError('   *** error Tubular geometry {:} not found'.format(dim))


#
def get_compactness(diameter: float, thickness: float) -> str:
    """
    """
    dt = diameter / thickness
    if dt > 80.0:
        compactness = 'slender'
        
    elif dt > 60.0:
        compactness = 'noncompact'
        
    else:
        compactness = 'compact'
        
    return compactness


#
points = namedtuple('Points', ['y', 'z'])


#
# ----------------------------------------
#      Standard Sections Profiles
# ----------------------------------------
#
#
class TubularIM(SectionBasic):
    __slots__ = ['_labels', '_number', '_title', '_d', '_tw']
    
    def __init__(self):
        """ """
        super().__init__()
        self._d: array = array('f', [])
        self._tw: array = array('f', [])        
        #self._process = TubularBasic()
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
            self._type.append('tubular')
    #
    def __getitem__(self, shape_name: str | int):
        """
        """
        try:
            index = self._labels.index(shape_name)
        except ValueError:
            raise Exception(f" section name {shape_name} not found")
        #
        return TubularBasic(name=self._labels[index], diameter=self._d[index], thickness=self._tw[index])
    #
    @property
    def df(self):
        """ """
        df = DBframework()
        #stype = ['tubular' for _ in self._labels]
        data = {"Number": self._number,
                "Title": self._title,
                "Name": self._labels,
                "type": self._type,
                "d" : self._d,
                "tw" : self._tw}
        #print('-->')
        return df.DataFrame(data)
    
    @df.setter
    def df(self, df):
        """ """
        for item in df.name:
            mnumber =  next(self.get_number())
            self._number.append(mnumber)
        #mnumber = [next(self.get_number()) for _ in df.name]
        #self._number.extend(mnumber)
        self._title.extend(['NULL' for _ in df.name])
        self._labels.extend(df.name.tolist())
        self._type.extend(df.type.tolist())
        try:
            self._d.extend(df.diameter.tolist())
        except TypeError:
            dtype = df['diameter'].apply(lambda x: x.convert('metre').value)
            self._d.extend(dtype.tolist())
        try:
            self._tw.extend(df.wall_thickness.tolist())
        except TypeError:
            dtype = df['wall_thickness'].apply(lambda x: x.convert('metre').value)
            self._tw.extend(dtype.tolist())
        #print('-->')
    #
    #
    #
    #

#
#
#
@dataclass
class TubularBasic(ShapeBasic):
    #name:str | int
    diameter:float
    thickness:float
    type:str = 'tubular'
    #
    def _properties(self):
        """
        """
        # get geometry
        diameter, thickness = self.get_geometry()
        #
        # -------------------------------------------------
        #   Cross-Sectional Area
        area = (math.pi / 4.
                * (diameter ** 2 - (diameter - 2 * thickness) ** 2))
        # Centroid
        Zc = diameter / 2.0
        Yc = diameter / 2.0
        # Shear centre
        SCz = diameter * 0
        SCy = diameter * 0
        # -------------------------------------------------
        #               Section Properties
        # -------------------------------------------------
        #   Second Moment of Area about Mayor Axis
        #   --------------------------------------
        Iy = (math.pi / 64.0
              * (diameter ** 4 - (diameter - 2 * thickness) ** 4))
        Iz = Iy
        #   Elastic Modulus about Mayor Axis
        #   --------------------------------------
        Zey = (2 * Iy / diameter)
        Zez = Zey
        # -------------------------------------------------
        #   Plastic Modulus about Mayor Axis
        Zpy = ((diameter ** 3
                - (diameter - 2 * thickness) ** 3) / 6.0)
        Zpz = Zpy
        # -------------------------------------------------
        #   Radius of gyration about Mayor Axis
        ry = (diameter ** 2 + (diameter - 2 * thickness) ** 2) ** 0.50 / 4.0
        rz = ry
        # -------------------------------------------------
        # Shear Factor
        SFy = Zpy / Zey
        SFz = SFy
        # -------------------------------------------------
        #   Warping Constant Cw
        Cw = 0 * Iy
        # -------------------------------------------------
        #   Torsional Constant
        J = 2 * Iy
        # -------------------------------------------------
        #   Polar Moment of Inertia
        Ip = (math.pi / 32.0
              * (diameter ** 4 - (diameter - 2 * thickness) ** 4))
        #   Product of inertia
        _Iyz = 0
        Jx = 2 * Iy
        rp = (Jx / area) ** 0.50
        #
        # -------------------------------------------------
        #self.section_coordinates()
        #
        # return _Area, _Zc, _Yc, _Iy, _Zey, _Zpy, _ry, _Iz, _Zez, _Zpz, _rz
        return ShapeProperty(area=area, Zc=Zc, Yc=Yc,
                             Iy=Iy, Zey=Zey, Zpy=Zpy, ry=ry,
                             Iz=Iz, Zez=Zez, Zpz=Zpz, rz=rz,
                             J=J, Cw=Cw)

    #
    def shear_stress(self, Vy, Vz,
                     stress_type:str ='average',
                     alpha: float = 2.0):
        """
        alpha: Shape factor (section 8.10 roakrs 7ed) 
        """
        # -------------------------------------------------
        #            Shear Stress Calculation
        prop = self.properties()
        coord =  self.section_coordinates()
        #
        tau_z = [Vz / prop.area for item in coord.z]
        #
        tau_y = [Vy / prop.area for item in coord.y]
        #
        if stress_type != 'average':
            # Shape factor (section 8.10 roakrs 7ed)        
            tau_z = tau_z * alpha
            tau_y = tau_y * alpha
        
        return tau_y, tau_z
    #
    def curved(self, R: float):
        """
        ---------
        R = Radio
        """
        # shear area
        warea = self.area
        D = self.diameter
        Tw = self.thickness

        # extreme fibre distances c
        c = D / 2.0

        c1 = c - Tw

        # centroidal radius
        # _R = R
        # _R = orad - c1

        # Shift of neutral axis from neutral axis
        e = (c * ((2.0 * R / c) - math.sqrt((R / c) ** 2 - 1.0) -
                  math.sqrt((R / c) ** 2 - (c1 / c) ** 2)) / 2.0)

        # where
        _Ic = self.Iy

        # stress factors Ki
        ki = ((1.0 / (4.0 * e / c))
              * ((1.0 - (e / c)) / ((R / c) - 1.0))
              * (1.0 + (c1 / c) ** 2))

        # stress factors Ko
        k0 = ((1.0 / (4.0 * e / c))
              * ((1.0 + (e / c)) / ((R / c) + 1.0))
              * (1.0 + (c1 / c) ** 2))

        # Modulus of rigidity factor (section 8.10)
        F = 2.0
        # Shear factor (section 8.1 equ 8.1-13)
        return ki, k0

    #
    def __str__(self, units: str = "si") -> str:
        """ """
        unit_sec = " m"
        unit_mas = "kg/m"
        space = " "
        output = "\n"
        # output += "\n"
        output += "{:}\n".format(80 * "_")
        output += "\n"
        output += f"{30 * space}SECTION PROPERTIES [{unit_sec}]\n"
        output += "\n"
        output += "Member ID      Type      Diametre   Thickness\n"
        output += "{:}\n".format("." * 80)
        output += "{:<14s} ".format(str(self.name))
        output += self._dimension()
        output += "\n"
        output += "{:}\n".format(80 * "_")
        output += "\n"
        output += (f"{15 * space}Area[{unit_sec}^2] Ixx [{unit_sec}^4] Iyy [{unit_sec}^4]"
                   f" Yp    [{unit_sec}] rx    [{unit_sec}] J   [{unit_sec}^4]\n")
        output += (f"{26 * space}Sxx [{unit_sec}^3] Syy [{unit_sec}^3] SCeny [{unit_sec}]"
                   f" ry    [{unit_sec}] Cw  [{unit_sec}^6]\n")
        output += f"{26 * space}Zxx [{unit_sec}^3] Zyy [{unit_sec}^3] SCenx [{unit_sec}] Mass[{unit_mas}]\n"
        output += "{:}\n".format(80 * ".")
        # output += "\n"
        output += "{:<14s} ".format("")
        output += self.properties.__str__()
        return output

    #
    def _stress(self, actions, stress=None, stress_type: str='average'):
        """
        """
        D, t = self.get_geometry()
        r = D * 0.50
        prop = self.properties()
        #
        coord =  self.section_coordinates()
        #coord_y = self.section_coordinates.y  # lateral
        #coord_z = self.section_coordinates.z  # vertical
        #
        # ---------------------------------
        #spoints = [x for x, item in enumerate(coord_y)]
        #
        #actions['sigma_y'] = actions.apply(lambda row: row.Fy/prop.Zey, axis=1)
        #test = actions[['load_title', 'node_end', 'Fy']].copy()
        #test['sigma_y'] = test['Fy']/prop.Zey
        #sigma_yy = [[actions.node_end.iloc[xx], item, step * item / prop.Zey]
        #            for item in coord.z
        #            for xx, step in enumerate(actions.My)]
        #
        # ----------------------------------------------
        # shear/torsion stress
        #
        # FIXME: what is Ip?
        # tau_x = 0 # actions.Mx * D / (2 * prop.Ip)
        tau_x = [actions.Mx / (2 * math.pi * r**2 * t)
                 for item in coord.y]
        #
        #1 / 0
        # In Plane
        # tau_y = 2*actions.Fy / prop.area
        #tau_y = [2 * actions.Fy / prop.area
        #         for item in coord.y]
        # Out Plane
        # tau_z = 2*actions.Fy / prop.area
        #tau_z = [2 * actions.Fz / prop.area
        #         for item in coord.z]
        tau_y, tau_z = self.shear_stress(Vz=actions.Fz, Vy=actions.Fy, 
                                         stress_type=stress_type)        
        #
        # ----------------------------------------------
        # torsional/bending stress
        #
        # sigma_x = actions.Fx / prop.area
        # sigma_z = actions.My/ prop.Zey
        # sigma_y = actions.Mz / prop.Zez
        #
        # get bending stress
        sigma_x = [actions.Fx / prop.area
                   for item in coord.y]
        #
        sigma_y = [actions.My * item / prop.Zey
                   for item in coord.z]
        #
        sigma_z = [actions.Mz * item / prop.Zez
                   for item in coord.y]
        #
        stress_out = BeamStress(sigma_x, sigma_y, sigma_z, 
                                tau_x, tau_y, tau_z, coord)        
        #
        if stress:
            stress_out = self.add_stress(stress=stress, other=stress_out)
        #
        return stress_out

    #
    def section_coordinates(self, theta: float = 90, steps: int = 6):
        """
        theta : Arch internal angle
        steps : arch division
        :return:
        arch coordinates: list[y, z]
        """
        diameter, thickness = self.get_geometry()
        radius = diameter * 0.50
        arc_length = radius * math.tau * theta / 360
        sinc = arc_length / steps
        r_theta = 360 * sinc / (radius * math.tau)
        coord_1 = []
        #coord_2 = []
        for i in range(steps + 1):
            rad = math.radians(i * r_theta)
            _x, _z = self._circunference_line(x=rad, r=radius)
            coord_1.append(_x)
            #coord_2.append(_z)
        #
        coordx = list(reversed(coord_1))
        coordx.extend([-item for item in coord_1[1:]])
        coordx.extend(list(reversed(coordx[1:-1])))
        #
        coordz = coord_1.copy()
        coordz.extend(list(reversed(coord_1[:steps])))
        coordz.extend([-item for item in coordz[1:-1]])
        #coordz.extend([-item for item in coord_1[1:]])
        #coord_1.reverse()
        #coord = coord_1 + [-item for item in coord_1[1:]]
        # return [coord, coord]
        #self.section_coordinates = points(coordx, coordz)
        return points(coordx, coordz)

    #
    def _circunference_line(self, x: float, r: float, xp1: float = 0, yp1: float = 0):
        """
        Calculating the coordinates of a point on a circles
        circumference from the radius, an origin and the
        arc between the points
        """
        xp2 = xp1 + r * math.sin(x)
        yp2 = yp1 - r * (1 - math.cos(x))
        # try:
        #    xp2 = xp1 + r * math.cos(math.tau/x)
        #    yp2 = yp1 + r * math.sin(math.tau/x)
        # except ZeroDivisionError:
        #    xp2 = x
        #    yp2 = yp1
        return xp2, yp2

    #   
    #
    def _dimension(self) -> str:
        """ Print section dimensions"""
        return "{:<9s} {:1.4e} {:1.4e}\n" \
            .format(self.type, self.d, self.t)

    #
    #
    def get_geometry(self):
        """ """
        return self.diameter, self.thickness
    #
    #def __getattr__(self, attr):
    #    """
    #    Getter for myattr
    #    :param attr:
    #    :return:
    #    """
    #    # if attr in self.__slots__:
    #    #    return self[attr]
    #    if re.search(r"\bd\b", attr, re.IGNORECASE):
    #        return self.diameter
    #    elif re.search(r"\bt(w)?\b", attr, re.IGNORECASE):
    #        return self.thickness
    #    else:
    #        try:
    #            return self.__dict__[attr]
    #        except KeyError:
    #            raise AttributeError(f"Variable {attr} not found")
    #
    ##
    #def __setattr__(self, attr, value):
    #    """
    #    Setter for myattr
    #    :param attr:
    #    :return:
    #    """
    #
    #    if re.search(r"\bd\b", attr, re.IGNORECASE):
    #        value = get_sect_properties([value])
    #        self.diameter = value[0]
    #    elif re.search(r"\bt(w)?\b", attr, re.IGNORECASE):
    #        value = get_sect_properties([value])
    #        self.thickness = value[0]
    #    else:
    #        try:
    #            # super().__setattr__(attr, value)
    #            self.__dict__[attr] = value
    #        except KeyError:
    #            raise AttributeError(f"Variable {attr} not found")
    #
    #
    @property
    def d(self):
        """
        d : Diametre
        """
        return self.diameter
    @d.setter
    def d(self, value):
        """
        d : Diametre
        """
        self.diameter = value
    #
    @property
    def t(self):
        """
        t : wall thickness
        """
        return self.thickness
    @t.setter
    def t(self, value):
        """
        t : wall thickness
        """
        self.thickness = value
    #
    @property
    def tw(self):
        """
        tw : wall thickness
        """
        return self.thickness
    @t.setter
    def tw(self, value):
        """
        tw : wall thickness
        """
        self.thickness = value    
    #     
    #
    #
    #
    def _data_df(self):
        """ """
        return {'type': self.type,
                'diameter': self.d,
                'wall_thickness': self.t}
#
