# 
# Copyright (c) 2019-2021 steelpy
#

# Python stdlib imports
from typing import NamedTuple, Tuple, ClassVar, Union, List

# package imports
#from steelpy.material.material import Materials
#from steelpy.sections.shape import Sections
#from steelpy.utils.units.main import Units
from steelpy.utils.io_module.text import match_line


#
#
def find_stress_item(word_in):
    """
    """
    _key = {"sigma_x": r"\b((sigma|stress)(\_)?(x))\b",
            "sigma_y": r"\b((sigma|stress)(\_)?(y))\b",
            "sigma_z": r"\b((sigma|stress)(\_)?(z))\b",
            #
            "tau_x": r"\b((tau)(\_)?(x))\b",
            "tau_y": r"\b((tau)(\_)?(y))\b",
            "tau_z": r"\b((tau)(\_)?(z))\b"}
    
    _match = match_line(word_in, _key)
    if not _match:
        raise IOError('  ** item {:} not recognized'.format(word_in))
    return _match
#
def assign_stress_item(self, mat_items):
    """
    Assign stress 
    
    """
    #if not self.stress:
    #    self.stress = Stress(0, 0, 0, 0, 0, 0)
    
    for key, value in mat_items.items():
        _item = find_stress_item(key)
        #
        if _item == 'sigma_x':
            self.stress.sigma_x += value
        elif _item == 'sigma_y':
            self.stress.sigma_y += value
        elif _item == 'sigma_z':
            self.stress.sigma_z += value
        elif _item == 'tau_x':
            self.stress.tau_x += value
        elif _item == 'tau_y':
            self.stress.tau_y += value
        elif _item == 'tau_z':
            self.stress.tau_z += value
        else:
            raise IOError('error stress item : {:} not recognized'
                          .format(_item))
    #
    #self.stress = Stress(_sigma_x, _sigma_y, _sigma_z, _tau_x, _tau_y, _tau_z)
    #print('ok')
#
#
#
class CodeResults(NamedTuple):
    """
    """
    axial:Tuple
    shear:Tuple
    bending:Tuple
    combined:Tuple
    report:List[str]
    #
    @property
    def total(self):
        """"""
        total_UR = self.combined.UR
        UR_flag = self.combined.UR_flag
        shear_res = max(self.shear[:2])
        if shear_res > total_UR:
            total_UR = shear_res
            UR_flag = self.shear.UR_flag
        return SummaryResults(total_UR, UR_flag)
#
#
class ChapterResults(NamedTuple):
    """
    """
    URy : float
    URz : float
    UR_flag : str
    allowable_y:float
    allowable_z:float
    #
    @property
    def status(self):
        if max(self.URy, self.URz) > 1.0:
            return "fail"
        return "pass"
#
#class AxialResults(NamedTuple):
#    """
#    """
#    UR : float
#    UR_flag : str
#    allowable:float
#    warning:str
#
class SummaryResults(NamedTuple):
    """
    """
    UR : float
    UR_flag : str
    
    @property
    def status(self):
        if self.UR > 1.0:
            return "fail"
        return "pass"
#
#
#
#
#
class BeamDesignParameters:
    """Beam Desing Patameters """
    __slots__ = ["g", "C", "theta",
                 "_L", "_Lb", "_K", "_Cm"]
    
    def __init__(self):
        """
        """
        #self.units = Units()
        #
        self.g = 9.810 #* self.units.m / self.units.second**2
        self.C = 0.30
        #
        self._K = [1.0, 1.0]
        #self.Kz = 1.0
        #
        # Moment Modifiers
        self._Cm = [0.85, 0.85]
        #self.Cmz = 0.85
        # id this the section rotation?
        self.theta = 0 #* self.units.degrees
    #-------------------------------------------------
    # Design data
    #
    @property
    def Cm(self):
        """ 
        length : Length of member
        """
        return self._Cm
    
    @Cm.setter
    def Cm(self, Cm):
        """ 
        length : Length of member
        """
        self._Cm = Cm   
    #
    #
    @property
    def K(self):
        """
        K : Effective length factor [Ky, Kz]
        """
        return self._K
    @K.setter
    def K(self, K: List[float]):
        """
        K : Effective length factor [Ky, Kz]
        """
        self._K = K   
    #
    @property
    def Lb(self):
        """
        Lb : Length between points that are either braced against lateral
             displacement of compression flange or braced against twist of
             the cross section [Ly, Lz]
        """
        return self._Lb
    @Lb.setter
    def Lb(self, Lb: List[float]):
        """
        Lb : Length between points that are either braced against lateral
             displacement of compression flange or braced against twist of
             the cross section [Ly, Lz]
        """
        self._Lb = Lb
    #
    def _properties(self):
        """ """
        section = self._cls.section
        d = section.diameter
        t = section.t
        L = self._cls.L
        name = self._cls.beam_name
        component = self._cls.component
        #
        output = "_______________________________________________________________________________________\n"
        output += "\n"
        output += "                                     GEOMETRY DATA                                 [mm]\n"
        output += "\n"
        output += "Member ID    Component    Diametre   Thickness  D/t        Ly         Lz\n"
        output += "\n"
        output += ".......................................................................................\n"
        output += "\n"
        output += "{:<12s} {:^12s} {:1.4E} {:1.4E} {:1.4E} {:1.4E} {:1.4E}\n"\
            .format(name, component, d, t, d/t, L, L)
        #
        mat = self._cls.material
        mass = mat.density.value
        output += "_______________________________________________________________________________________\n"
        output += "\n"
        output += "                                  MATERIAL PROPERTIES                            \n"
        output += "\n"
        output += "Member ID      Fy [N/mm2]  Fu [N/mm2]  E  [N/mm2]  G  [N/mm2]  Poisson     Rho[kg/m3]\n"
        output += "\n"
        output += ".......................................................................................\n"
        output += "\n"
        output += "{:<14s} {:1.4E} {:1.4E} {:1.4E} {:1.4E} {:1.4E} {:1.4E}\n"\
            .format(name, mat.Fy.value, mat.Fu.value, mat.E.value,
                    mat.G.value, mat.poisson, mass)
        #
        prop = section.properties
        output += "\n"
        output += "_______________________________________________________________________________________\n"
        output += "\n"
        output += "                              SECTION DERIVED PROPERTIES\n"
        output += "\n"
        output += "Member ID      Area[mm^2]  I   [mm^4]  S   [mm^3]  Z   [mm^3]  ShapeFctor  r    [mm]\n"
        #outpu += ("Number        Awx  [mm^2]  Iyy [mm^4]  Syy [mm^3]  Zyy [mm^3]  SCeny [mm]  ry   [mm]\n"
        output += "               Mass[kg/m]  Ip  [mm^4]  J   [mm^4]\n"
        output += ".......................................................................................\n"
        output += "\n"
        output += "{:<14s} {:1.4E} {:1.4E} {:1.4E} {:1.4E} {:1.4E} {:1.4E}\n"\
            .format(name, prop.area, prop.Iy, prop.Zey, prop.Zpy, prop.Zey/prop.Zpy, prop.ry)
        output += "{:<14s} {:1.4E} {:1.4E} {:1.4E}\n"\
            .format(" "*14, mass * prop.area, prop.Iy, prop.J)        
        #
        return output
    #
    #
    def __str__(self) -> str:
        """ """
        print('----')