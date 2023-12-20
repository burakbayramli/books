#
# Copyright (c) 2009-2023 fem2ufo
# 

# Python stdlib imports
from __future__ import annotations
from collections.abc import Mapping
from collections import defaultdict
from operator import sub, add
from dataclasses import dataclass
from typing import NamedTuple


# package imports
#
from steelpy.utils.units.buckingham import Number
from steelpy.utils.math.operations import trnsload
from steelpy.utils.math.operations import linspace
# steelpy.
from ....f2uModel.load.process.operations import(check_list_units,
                                                 check_beam_dic,
                                                 check_point_dic,
                                                 get_beam_node_load,
                                                 get_beam_udl_load)
#
# steelpy.
from steelpy.trave.beam.pilkey.main import BeamTorsion, BeamAxial, BeamBending
#
import numpy as np
#
# ---------------------------------
#
class LineBeam(NamedTuple):
    """
    """
    qx0: float
    qy0: float
    qz0: float
    #
    qx1: float
    qy1: float
    qz1: float
    #
    L0: float
    L1: float
    #
    name: str | int
    title: str
    load_name: str | int
    system: int
    load_complex: int
    load_type: str = "Line Load"
    #
    @property
    def coordinate_system(self):
        """
        Local  : 1
        Global : 0
        """
        if self.system != 0:
            return "local"
        return "global"
    #
    def local_system(self):
        """
        return q load in local beam system

        retunr:
        [qx0, qy0, qz0, qx1, qy1, qz1]
        """
        # local nodal loading
        #nload = [self.qx0, self.qy0, self.qz0, 0, 0, 0,
        #         self.qx1, self.qy1, self.qz1, 0, 0, 0,]        
        #
        try:  # local system
            1 / self.system
        except TypeError:
            if self.system.lower() == 'global':
                raise IOError('line line should be in beam local system')
        except ZeroDivisionError:
            raise IOError('line line should be in beam local system')
            # global system
            #nload = trns_3Dv(nload, R)
        #return [*nload[:3], *nload[6:9]]
    #
    #
    def __str__(self, units:str="si") -> str:
        """ """
        output = (f"{str(self.name):12s} "
                  f"{self.L0: 1.3e} {self.qx0: 1.3e} "
                  f"{self.qy0: 1.3e} {self.qz0: 1.3e} "
                  f"{self.coordinate_system.upper():6s} "
                  f"{self.load_complex}\n")
        
        if (load_title:= self.title) == "NULL":
            load_title = ""        
        step = self.load_type
        output += (f"{step[:12]:12s} {self.L1: 1.3e} {self.qx1: 1.3e} "
                   f"{self.qy1: 1.3e} {self.qz1: 1.3e} "
                   f"{str(load_title)}\n")
        return output
    #
    # -------------------------
    # Load Process Formulas
    # ------------------------
    #
    def Fx(self, x:float|list, L:float,
           E:float, G: float,
           Iy:float, Iz:float,
           J: float, Cw: float, Area: float):
        """
        Beam load local system

        x : distance from end 1
        L : beam length
        E : Elastic module
        Iy : In plane
        Iz : Out plane
        R :

        return:
        [load_name, load_title, load_type, load_system,
        beam_name, x, Fx, Fy, Fz]
        Fx, Fy, Fz --> [V, M, theta, w] Note positive load is upwards
        """
        # convert load to beam local
        self.local_system()
        #
        # Axial [P, blank, blank, u]
        Fx =  BeamAxial(L=L, E=E, A=Area)
        # Torsion [T, B, psi, phi, Tw]
        Mx_blank = [0, 0, 0, 0, 0]
        #Mx = BeamTorsion(E=E, L=L, G=G)
        #Mx.torque(T=self.mx, L1=self.L0)        
        #
        # In plane [V, M, theta, w]
        in_plane =  BeamBending(L=L, E=E, I=Iy)
        # Out plane [V, M, theta, w]
        out_plane = BeamBending(L=L, E=E, I=Iz)
        #
        # [Fx, Fy, Fz, Mx, My, Mz]
        if isinstance(x, (list, tuple)):
            Fx_out = []
            for xstep in x:
                Fx_out.append([self.load_name, self.title,
                               'basic', self.coordinate_system,
                               self.name, xstep,
                               np.array(Fx.axial(x=xstep,
                                        P=self.qx0, L1=self.L0,
                                        P2=self.qx1, L2=self.L1)),
                               np.array(Mx_blank), 
                               np.array(in_plane.line(x=xstep,
                                             q1=self.qy0, q2=self.qy1,
                                             L1=self.L0, L2=self.L1)),
                               np.array(out_plane.line(x=xstep,
                                              q1=self.qz0, q2=self.qz1,
                                              L1=self.L0, L2=self.L1))])
        
        else:
            Fx_out = [self.load_name, self.title,
                      'basic', self.coordinate_system,
                      self.name, x,
                      np.array(Fx.axial(x=x,
                               P=self.qx0, L1=self.L0,
                               P2=self.qx1, L2=self.L1)),
                      np.array(Mx_blank),
                      np.array(in_plane.line(x=x,
                                    q1=self.qy0, q2=self.qy1,
                                    L1=self.L0, L2=self.L1)),
                      np.array(out_plane.line(x=x,
                                     q1=self.qz0, q2=self.qz1,
                                     L1=self.L0, L2=self.L1))]
        #
        # [load_name, load_title, load_type, load_system, 
        # beam_number, x, Fx, Fy, Fz]
        return Fx_out
    #
    #
    #
    def fer_beam(self, L):
        """fix end reaction [fer] - Line beam load local system
        
        Return:
        fer : [load_name, system, beam_load_id, beam_name, end_1, end_2]
        end : [fx, fy, fz, mx, my, mz]"""
        #
        # convert load to beam local
        self.local_system()        
        #[Fa, Fb]
        # Axial
        #qx_av = (self.qx0 +  self.qx1) / 2
        #qx_av = (qload[0] +  qload[3]) / 2
        #Fa =  distributed_axial(w=qx_av,
        #                        L=L, l1=self.L0, l2=self.L1)
        #
        Fa = self._Fa(L=L)
        #
        Mx = [0, 0]
        #
        #[Fa, Ma, Fb, Mb]
        # In plane
        Finp = trapezoidal_load(w1=self.qy0, w2=self.qy1,
                                L=L, l1=self.L0, l2=self.L1)
        # Out plane
        Foutp = trapezoidal_load(w1=self.qz0, w2=self.qz1,
                                 L=L, l1=self.L0, l2=self.L1)
        #
        #
        # [fx, fy, fz, mx, my, mz]
        end1 = [Fa[0], Finp[0], Foutp[0], Mx[0], Foutp[1], Finp[1]]
        end2 = [Fa[1], Finp[2], Foutp[2], Mx[1], Foutp[3], Finp[3]]        
        #
        Fx_out = [self.load_name, self.system, self.title, 
                  self.name, end1, end2]
        #1 / 0
        return Fx_out
    #
    #
    #
    def _Fa(self, L: float):
        """Beam Axial load"""
        # Calculate area
        h = L - self.L0 - self.L1
        m = (self.qx0 + self.qx1) / 2.0
        area = m * h
        try:
            if self.qx0 > self.qx1:
                Lcog = (self.qx0 + 2*self.qx1)/(self.qx0 + self.qx1) * h / 3.0
            else:
                Lcog = h - (2*self.qx0 + self.qx1)/(self.qx0 + self.qx1) * h / 3.0
        except ZeroDivisionError:
            return 0, 0
        #
        h = area / m
        qload = area / h
        L1 = max(self.L0 + Lcog - h / 2.0, 0)
        L2 = max(L - (self.L0 + Lcog + h / 2.0), 0)
        #
        # Axial
        Fa = distributed_axial(w=qload, L=L,
                               l1=L1, l2=L2)
        #       
        return Fa    
    #
    #
#
#
class PointBeam(NamedTuple):
    """
    """
    fx: float
    fy: float
    fz: float
    mx: float
    my: float
    mz: float
    #
    L0: float
    #
    name: str | int
    title: str
    load_name: str | int
    system: int
    load_complex: int
    load_type: str = "Point Load"
    #
    @property
    def coordinate_system(self):
        if self.system != 0:
            return "local"
        return "global"    
    #    
    @property
    def distance(self):
        """ """
        return self.L0
    #
    def local_system(self):
        """ """
        # local nodal loading
        #pload = [self.fx, self.fy, self.fz, self.mx, self.my, self.mz]       
        #
        try:  # local system
            1 / self.system
        except ZeroDivisionError:
            # global system
            #pload = trns_3Dv(pload, R)
            raise IOError('line line should be in beam local system')
        #return pload[:6]
    #
    #
    def __str__(self, units:str="si") -> str:
        """ """
        output  = (f"{str(self.name):12s} "
                   f"{self.L0: 1.3e} {self.fx: 1.3e} "
                   f"{self.fy: 1.3e} {self.fz: 1.3e} "
                   f"{self.coordinate_system.upper():6s} "
                   f"{self.load_complex}\n")
        
        if (load_title:= self.title) == "NULL":
            load_title = ""        
        step = self.load_type
        output += (f"{step[:12]:12s} {10*' '} {self.mx: 1.3e} "
                   f"{self.my: 1.3e} {self.mz: 1.3e} "
                   f"{str(load_title)}\n")
        return output
    #
    #
    # -------------------------
    # Load Process Formulas
    # ------------------------    
    #
    #
    #def Fa(self, L: float):
    #    """Beam Axial load"""
    #    # Axial 
    #    Fa = axial_load(W=self.fx, L=L, l1=self.L0)
    #    return Fa
    #
    #
    def Fx(self, x:float|list, L: float,
           E:float, G: float, 
           Iy:float, Iz:float,
           J: float, Cw: float, Area: float) -> list:
        """
        Beam load local system

        x : distance from end 1
        L : beam length
        E : Elastic module
        Iy : In plane
        Iz : Out plane
        R :

        Return:
        Fx : [load_name, load_title, load_type, load_system,
              beam_name, L_step,
              Axial, Torsion, Bending_inplane, Bending_outplane]
        
        Axial, Bending_inplane, Bending_outplane = [V, M, w, theta]
        Torsion = [T, B, psi, phi, Tw]
        """
        # convert load to beam local
        self.local_system()
        #
        # Axial [P, blank, blank, u]
        Fx =  BeamAxial(L=L, E=E, A=Area)
        # Torsion [T, B, psi, phi, Tw]
        Mx = BeamTorsion(E=E, L=L, G=G, J=J, Cw=Cw)
        # In plane [V, M, theta, w]
        Finplane =  BeamBending(L=L, E=E, I=Iy)
        # Out plane [V, M, theta, w]
        F_outplane = BeamBending(L=L, E=E, I=Iz)
        #
        # [Fx, Fy, Fz, Mx, My, Mz]
        if isinstance(x, (list,tuple)):
            Fx_out = []
            for xstep in x:
                Axial =  Fx.axial(x=xstep, P=self.fx, L1=self.L0)
                #Axial = Fx.loading_function(x=xstep)
                Torsion =  Mx.torque(x=xstep, T=self.mx, L1=self.L0)
                #Torsion = Mx.loading_function(x=xstep, J=J, Cw=Cw)
                Finp = Finplane.point(x=xstep, P=self.fy, M=self.mz, L1=self.L0)
                #Finp = list(map(sum, zip(F_inplane(xstep, E, Iy), M_outplane(xstep, E, Iy))))
                Foutp = F_outplane.point(x=xstep, P=self.fz, M=self.my, L1=self.L0)
                #Foutp = list(map(sum, zip(F_outplane(xstep, E, Iz), M_inplane(xstep, E, Iz))))
                #
                Fx_out.append([self.load_name, self.title, 'basic', self.coordinate_system,
                               self.name, xstep, np.array(Axial), np.array(Torsion), np.array(Finp), np.array(Foutp)])
        else:
            Axial =  Fx.axial(x=x, P=self.fx, L1=self.L0)
            #Axial = Fx.loading_function(x=x)
            # torsion = [FT, FB, Fpsi, Fphi]
            #Torsion = Mx.loading_function(x=x, J=J, Cw=Cw)
            Torsion =  Mx.torque(x=x, T=self.mx, L1=self.L0)
            #
            Finp = Finplane.point(x=x, P=self.fy, M=self.mz, L1=self.L0)
            #Finp = list(map(sum, zip(F_inplane(x, E, Iy), M_outplane(x, E, Iy))))
            Foutp = F_outplane.point(x=x, P=self.fz, M=self.my, L1=self.L0)
            #Foutp = list(map(sum, zip(F_outplane(x, E, Iz), M_inplane(x, E, Iz))))
            #
            Fx_out = [self.load_name, self.title, 'basic', self.coordinate_system, 
                      self.name, x, np.array(Axial), np.array(Torsion), np.array(Finp), np.array(Foutp)]
        # [axial, torsion, bending_inplane, bending_outplane]
        return Fx_out
    #
    #
    def fer_beam(self, L: float) -> list:
        """fix end reaction [fer] - Point beam load local system 
        
        Return:
        fer : [load_name, system, beam_load_id, beam_name, end_1, end_2]
        end : [fx, fy, fz, mx, my, mz]"""
        # convert load to beam local
        self.local_system()
        #
        #[Fa, Fb]
        # Axial
        Fa = axial_load(W=self.fx, L=L, l1=self.L0)
        Mx = torsion_load(Mt=self.mx, L=L, l1=self.L0)
        #
        #
        #[Fa, Ma, Fb, Mb]
        # In plane        
        F_inplace = point_load(W=self.fy, L=L, l1=self.L0)
        M_inplace = point_moment(M=self.mz, L=L, l1=self.L0)
        # Out plane
        F_outplace = point_load(W=self.fz, L=L, l1=self.L0)
        M_outplace = point_moment(M=self.my, L=L, l1=self.L0)
        #
        Finp = list(map(sum, zip(F_inplace, M_inplace)))
        #Finp.insert(index=0, Fa[0])
        #
        Foutp = list(map(sum, zip(F_outplace, M_outplace)))
        #
        # [fx, fy, fz, mx, my, mz]
        end1 = [Fa[0], Finp[0], Foutp[0], Mx[0], Foutp[1], Finp[1]]
        end2 = [Fa[1], Finp[2], Foutp[2], Mx[1], Foutp[3], Finp[3]]
        Fx_out = [self.load_name, self.system, self.title,
                  self.name, end1, end2]
        #1 / 0
        return Fx_out
    #
#
#
#
# ---------------------------------
#
#
class BeamLoadItem(Mapping):
    __slots__ = ['_labels','_name',  '_load', 
                 '_node_eq']

    def __init__(self) -> None:
        """
        """
        self._labels = []
    #
    #
    def __contains__(self, value) -> bool:
        return value in self._labels

    def __len__(self) -> int:
        return len(self._labels)

    def __iter__(self):
        """
        """
        items = list(dict.fromkeys(self._labels))
        return iter(items)

    #
    def __str__(self, units: str = "si") -> str:
        """ """
        output = ""
        output += self._load.__str__()
        return output
    #
    
#
#
#
@dataclass
class BeamLoad:
    #__slots__ = ['_beam']

    def __init__(self): #, beams
        """
        """
        self._system_flag = 0  # Global system default
    #
    #def __call__(self, beam_name: int | str):
    #    """ """
    #    self._beam_id = beam_name
    #    return self
    #
    # ------------------
    #
    @property
    def line(self):
        """
        Linear Varying Load (lvl) - Non Uniformly Distributed Load
        
        value : [qx1, qy1, qz1, qx2, qy2, qz2, L1, L2]
    
                        |
             q0         | q1
        o------|        |----------o
        |                          |
        +  L0  +        +    L1    +

        """
        beam_name = self._beam.name
        #1 / 0
        #beam_name = self._beam_id
        return self._line[beam_name]

    @line.setter
    def line(self, values: list):
        """
        Linear Varying Load (lvl) - Non Uniformly Distributed Load
                value : [qx1, qy1, qz1, qx2, qy2, qz2, L1, L2]
    
                        |
             q0         | q1
        o------|        |----------o
        |                          |
        +  L0  +        +    L1    +
        """
        beam_name = self._beam.name
        #beam_name = self._beam_id
        #
        if isinstance(values, dict):
            load = self._get_line(values)
            load.insert(0, 'load')
            self._line[beam_name] = load

        elif isinstance(values[0], (list, tuple)):
            for item in values:
                load =  self._get_line(item)
                load.insert(0, 'load')
                self._line[beam_name] = load
        else:
            load =  self._get_line(values)
            load.insert(0, 'load')
            self._line[beam_name] = load
    #
    def _get_line(self, line_load: list|dict):
        """ get line load in beam local system"""
        #
        # update inputs
        if isinstance(line_load, dict):
            udl = check_beam_dic(line_load)
            title = udl.pop()
            
        elif isinstance(line_load[-1], str):
            title = line_load.pop()
            if isinstance(line_load[0], Number):
                udl = check_list_units(line_load)
            else:
                udl = get_beam_udl_load(line_load)
        else:
            title ='NULL'
            udl = get_beam_udl_load(line_load)
        #
        # get system local = 1
        try:
            1 / self._system_flag
            return [*udl, 1, title]
        except ZeroDivisionError:
            # local nodal loading
            nload = [*udl[:3], 0, 0, 0,
                     *udl[3:6], 0, 0, 0,]
            nload = trnsload(nload, self._beam.T3D())
            nload = [*nload[:3], *nload[6:9]] 
            return [*nload, *udl[6:], 1, title]
    #
    # ------------------
    #
    @property
    def point(self):
        """ Concentrated force """
        beam_name = self._beam.name
        #beam_name = self._beam_id
        #1 / 0
        return self._point[beam_name]

    @point.setter
    def point(self, values: list):
        """
        Concentrated force
        """
        beam_name = self._beam.name
        #beam_name = self._beam_id
        #
        if isinstance(values, dict):
            load = self._get_point(values)
            load.insert(0, 'force')
            self._point[beam_name] = load

        elif isinstance(values[0], (list, tuple)):
            for item in values:
                #value.insert(0, self._Lbeam)
                #load = get_beam_point_load(load=values)
                load = self._get_point(item)
                load.insert(0, 'force')
                self._point[beam_name] = load
        else:
            #values.insert(0, self._Lbeam)
            #load = get_beam_point_load(load=values)
            load =  self._get_point(values)
            load.insert(0, 'force')
            self._point[beam_name] = load
    #
    def _get_point(self, point_load: list|dict):
        """ get point load in beam local system"""
        # update inputs
        if isinstance(point_load, dict):
            point = check_point_dic(point_load)
            title = point.pop()
        
        elif isinstance(point_load[-1], str):
            title = point_load.pop()
            if isinstance(point_load[0], Number):
                point = check_list_units(point_load)
            else:
                point = get_beam_node_load(point_load)
        
        else:
            title = 'NULL'
            point = get_beam_node_load(point_load)
        #
        # get system local = 1
        try: # Local system
            1 / self._system_flag
            return [*point, 1, title]
        except ZeroDivisionError: # global to local system
            pload = [*point[:6], 0, 0, 0, 0, 0, 0]
            pload = trnsload(pload, self._beam.T3D())
            return [*pload[:6], point[6], 1, title]
    #
    # ------------------
    #
    def local_system(self):
        """set load beam local system"""
        self._system_flag = 1
        self._line.coordinate_system = self._system_flag
        self._point.coordinate_system = self._system_flag
        return "local"

    # @property
    def global_system(self):
        """set load beam global system"""
        self._system_flag = 0
        self._line.coordinate_system = self._system_flag
        self._point.coordinate_system = self._system_flag
        return "global"
    #
    # ------------------
    #
    def __str__(self, units: str = "si") -> str:
        """ """
        #unit_lenght = " m"
        #unit_force = "  N"
        #unit_bm = "N*m"
        #unit_fl = "N/m"
        output = ""
        #if header:
        #    output += "\n"
        #    output += f"--- Beam \n"
        #    output += f"Element Name{6 * ' '}L1[{unit_lenght}] qx1[{unit_fl}] qy1[{unit_fl}] qz1[{unit_fl}] System Complex\n"
        #    output += f"Line Load{9 * ' '}L2[{unit_lenght}] qx2[{unit_fl}] qy2[{unit_fl}] qz2[{unit_fl}] Comment\n"
        #    output += "\n"
        #    output += f"--- Beam \n"
        #    output += f"Element Name{6 * ' '}L1[{unit_lenght}] fx [{unit_force}] fy [{unit_force}] fz [{unit_force}] System Complex\n"
        #    output += f"Point Load{15 * ' '}mx [{unit_bm}] my [{unit_bm}] mz [{unit_bm}] Comment\n"
        #    output += "\n"
        #    output += "{:}\n".format(80 * ".")
        #    output += "\n"
        # 1/0
        # output += "--- Beam Line Load\n"
        output += self._line.__str__()
        # output += "--- Beam Point Load\n"
        output += self._point.__str__()
        # output += self._nodal_displacement.__str__()
        return output
    #
    # ------------------
    # Load Process
    # ------------------
    #
    def reactions(self):
        """Calculate bean reacition according to boundaries"""
        beam = self._beam
        r1 = 8 * [0]
        r2 = 8 * [0]
        b2n = []
        # line loadreactions
        for key, item in self._line.items():
            #print(f'line load {key}')
            for bload in item:
                res = beam.reaction(load=bload)
                r1 = list(map(add, r1, [*res[0][0], *res[0][1]]))
                r2 = list(map(add, r2, [*res[1][0], *res[1][1]]))
                #b2n.append([bload, item.name, item.title])
        # point load
        for key, item in self._point.items():
            #print(f'point load {key}')
            for bload in item:
                res = beam.reaction(load=bload)
                r1 = list(map(add, r1, [*res[0][0], *res[0][1]]))
                r2 = list(map(add, r2, [*res[1][0], *res[1][1]]))
        return [r1[:4], r1[4:]], [r2[:4], r2[4:]]
    #
    def nodal_equivalent(self):
        """
        Convert beam load [udl and point] to beam's end nodal load local system
        """
        res = self.reactions()
        return self._load2reaction(F0=res[0], F1=res[1])
        #
        #return array('d', nload)
        #return nload
    #
    def _load2reaction(self, F0:list, F1:list):
        """ Change load signs to reacctions"""
        #
        R0y, R0z = F0
        R1y, R1z = F1
        #
        lnload = [0] * 12
        #lnload = array('d', lnload)
        # ---------------------------
        # End 1
        # lnload[ 0 ] # axial
        lnload[1] =  1 * R0y[0]  # y
        lnload[2] =  1 * R0z[0]  # z
        # lnload[3]  # torsion Mx
        lnload[4] = 1 * R0z[1]  # my
        lnload[5] = -1 * R0y[1]  # mz
        # ---------------------------
        # End 2
        # lnload[ 0 ] # axial
        lnload[7] = -1 * R1y[0]  # y
        lnload[8] = -1 * R1z[0]  # z
        # lnload[9]  # torsion Mx
        lnload[10] = -1 * R1z[1]  # my
        lnload[11] = 1 * R1y[1]  # mz
        #
        return lnload
    #
    def beam_function(self, beams, steps:int = 10):
        """ """
        #
        #beamfun = defaultdict(list)
        loadfun = []
        # line load
        for key, items in self._line.items():
            beam = beams[key]
            mat = beam.material
            sec = beam.section.properties()
            Lsteps = linspace(start=0, stop=beam.L, num=steps+1, endpoint=True)
            for bitem in items:
                lout = bitem.Fx(x=Lsteps, L=beam.L,
                                E=mat.E, G=mat.G, 
                                Iy=sec.Iy, Iz=sec.Iz,
                                J=sec.J, Cw=sec.Cw, Area=sec.area)
                #beamfun[key].extend(lout)
                #beamfun[key].append(lout)
                #
                loadfun.extend(lout)
                #loadfun.append([key, bitem.name, lout])
                #loadfun.extend([[bitem.name, 'local', key, *step]
                #                for step in lout])
                #for step in lout:
                #    # load_name, load_title, [Fx, Fy, Fz, Mx, My, Mz]
                #    loadfun.append([bitem.name, 'local', key, *step])
        # point load
        for key, items in self._point.items():
            beam = beams[key]
            mat = beam.material
            sec = beam.section.properties()
            Lsteps = linspace(start=0, stop=beam.L, num=steps+1, endpoint=True)
            for bitem in items:
                lout = bitem.Fx(x=Lsteps, L=beam.L,
                                E=mat.E, G=mat.G, 
                                Iy=sec.Iy, Iz=sec.Iz,
                                J=sec.J, Cw=sec.Cw, Area=sec.area)
                #beamfun[key].append([bitem.name, lout])
                #beamfun[key].extend(lout)
                #beamfun[key].append(lout)
                #
                loadfun.extend(lout)
                #loadfun.append([key, bitem.name, lout])
                #loadfun.extend([[bitem.name, 'local', key, *step]
                #                for step in lout])                
                #for step in lout:
                #    loadfun.append([bitem.name, 'local', key, *step])
        #print('---')
        return loadfun # beamfun # 
    #
    def fer(self, beams):
        """ """
        """Beam reacition global system according to boundaries
        """
        b2n = []
        #beam = self._beam
        global_system = 0
        # line loadreactions
        for key, item in self._line.items():
            beam = beams[key]
            node1, node2 = beam.nodes
            #print(f'line load {key}')
            for bload in item:
                res = bload.fer_beam(L=beam.L)
                # local to global system
                gnload = [*res[4], *res[5]]
                lnload = trnsload(gnload, beam.T3D())
                b2n.append([bload.load_name, bload.title, global_system, 
                            beam.number, node1.number, lnload[:6], node2.number, lnload[6:]])
        # point load
        for key, item in self._point.items():
            beam = beams[key]
            node1, node2 = beam.nodes
            #print(f'point load {key}')
            for bload in item:
                res = bload.fer_beam(L=beam.L)
                gnload = [*res[4], *res[5]]
                lnload = trnsload(gnload, beam.T3D())
                #b2n.append([bload, item.name, item.title])
                #b2n.append(res)
                b2n.append([bload.load_name, bload.title, global_system, 
                            beam.number, node1.number, lnload[:6], node2.number, lnload[6:]])
        #
        #1 / 0
        #return [r1[:4], r1[4:]], [r2[:4], r2[4:]]
        return b2n
    #
    # ------------------
    #
    #def df(self, data):
    #    """ """
    #    print('====')
    #    1/0
    #
    #
#
#
# ---------------------------------
#
# Beam fixed end formulas
#
# ---------------------------------
#
def point_load(W:float, L:float, l1:float) -> list[float]:
    """
    Case 1 from Matrix Analysis of Framed Structures [Aslam Kassimali]
    """
    l2 = L - l1
    Fa = W*l2**2/L**3 * (3*l1 + l2)
    Ma = W*l1*l2**2/L**2
    Fb = W*l1**2/L**3 * (l1 + 3*l2)
    Mb = -W*l1**2 * l2/L**2
    return [Fa, Ma, Fb, Mb]
#
#
def point_moment(M:float, L:float, l1:float) -> list[float]:
    """
    Case 2 from Matrix Analysis of Framed Structures [Aslam Kassimali]
    """
    l2 = L - l1
    Fa = -6 * M * l1*l2 / L**3
    Ma = M*l2/L**2 * (l2-2*l1)
    Fb = 6 * M * l1*l2 / L**3
    Mb = M*l1/L**2 * (l1-2*l2)
    return [Fa, Ma, Fb, Mb]
# 
#
def trapezoidal_load(w1:float, w2:float,
                     L:float, l1:float, l2:float) -> list[float]:
    """
    Case 4 from Matrix Analysis of Framed Structures [Aslam Kassimali]
    """
    Fa = (w1*(L - l1)**3/(20*L**3) * ((7*L + 8*l1) - l2*(3*L + 2*l1)/(L - l1)
                                      * (1 + l2/(L-l1) + l2**2/(L-l1)**2)
                                      + 2*l2**4/(L-l1)**3)
          + w2*(L-l1)**3/(20*L**3) * ((3*L + 2*l1) * (1 + l2/(L-l1) + l2**2/(L-l1)**2)
                                      - l2**3/(L-l1)**2 * (2 + (15*L - 8*l2)/(L-l1))))
    #
    Ma = (w1*(L - l1)**3/(60*L**2) * (3*(L + 4*l1) - l2*(2*L + 3*l1)/(L - l1)
                                      * (1 + l2/(L-l1) + l2**2/(L-l1)**2)
                                      + 3*l2**4/(L-l1)**3)
          + w2*(L-l1)**3/(60*L**2) * ((2*L + 3*l1) * (1 + l2/(L-l1) + l2**2/(L-l1)**2)
                                      - 3*l2**3/(L-l1)**2 * (1 + (5*L - 4*l2)/(L-l1))))
    #
    Fb = (w1+w2)/2.0 * (L-l1-l2) - Fa
    Mb = ((L-l1-l2)/6.0 * (w1*(-2*L + 2*l1 - l2)
                           - w2*(L-l1+2*l2)) + Fa*L - Ma)
    return [Fa, Ma, Fb, Mb]
#
#
def axial_load(W:float, L:float, l1:float) -> list[float]:
    """
    Case 5 from Matrix Analysis of Framed Structures [Aslam Kassimali]
    """
    l2 = L - l1
    Fa = W*l2/L
    Fb = W*l1/L
    return [Fa, Fb]
#
#
def distributed_axial(w:float, L:float,
                      l1:float, l2:float) -> list[float]:
    """
    Case 6 from Matrix Analysis of Framed Structures [Aslam Kassimali]
    """
    Fa = w/(2*L) * (L-l1-l2) * (L-l1+l2)
    Fb = w/(2*L) * (L-l1-l2) * (L+l1-l2)
    return [Fa, Fb]
#
#
def torsion_load(Mt:float, L:float, l1:float) -> list[float]:
    """
    Case 7 from Matrix Analysis of Framed Structures [Aslam Kassimali]
    """
    l2 = L - l1
    Fa = Mt*l2/L
    Fb = Mt*l1/L
    return [Fa, Fb]
#
#