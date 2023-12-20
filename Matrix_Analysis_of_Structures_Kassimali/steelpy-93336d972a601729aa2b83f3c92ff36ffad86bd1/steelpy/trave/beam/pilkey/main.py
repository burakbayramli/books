# 
# Copyright (c) 2019-2023 steelpy
# 

# Python stdlib imports
from __future__ import annotations
from collections import defaultdict
from dataclasses import dataclass
#from collections.abc import Mapping
#from typing import NamedTuple, Dict, List, Tuple, Union



# package imports
# Bending 
#
from steelpy.trave.beam.pilkey.chapter11.table2C import BeamBendingSupports
from steelpy.trave.beam.pilkey.chapter11.table2 import BendingGE
#
from steelpy.trave.beam.pilkey.chapter11.table2B import (Trapezoidal,
                                                       Point, Moment)
# Torsion
from steelpy.trave.beam.pilkey.chapter14.table_C import BTOpenSupports
from steelpy.trave.beam.pilkey.chapter14.table_A import TorsionOpenGE
from steelpy.trave.beam.pilkey.chapter14.table_B import (TOpenConcentrated,
                                                       TOpenDistributed)
from steelpy.trave.beam.pilkey.chapter12.table_2t import (TBarConcentrated,
                                                        TBarDistributed,
                                                        BTBarSupports,
                                                        TorsionBarGE)
# Axial
from steelpy.trave.beam.pilkey.chapter12.table_2axial import (PBarConcentrated,
                                                            PBarDistributed,
                                                            PBarSupports,
                                                            AxialBarGE)
#
from steelpy.utils.math.operations import linspace
from steelpy.utils.dataframe.main import DBframework
#
#
#
#
#
@dataclass
class BeamBasic:
    __slots__ = ['L','area', 'Iy', 'Iz', 'J', 'E', 'G', 'Cw', 
                 '_response', '_support0', '_support1',
                 '_torsion', '_axial', '_bminplane', '_bmoutplane']

    def __init__(self, L: float, area: float,
                 Iy:float, Iz:float, J:float,
                 E:float, G:float, Cw:float) -> None:
        """
        L : beam length
        E : Elastic modulus
        G : Shear modulus
        J : Torsial constant [m^4]
        Iy,z : moment of inertia [m^4]
        Cw : Warping constant [m^6]
        """
        self.L = L
        self.area = area
        self.Iy = Iy
        self.Iz = Iz
        self.J = J
        self.E = E
        self.G = G
        self.Cw = Cw
        #
        self._torsion = BeamTorsion(L=L, E=E, G=G,
                                    J=self.J, Cw=self.Cw)
        self._axial = BeamAxial(L=L, E=E, A=area)
        self._bminplane = BeamBending(L=L, E=E, I=self.Iy)
        self._bmoutplane = BeamBending(L=L, E=E, I=self.Iz)
    #
    # ----------------------------------------------
    #
    def supports(self, end1: str|list, end2: str|list,
                 torsion1: str|list|None  = None, torsion2: str|list|None  = None,
                 k1: float | None = None, k2: float | None = None):
        """
        end1 : beam end 1 [fix, free, pinned, guide, spring]
        end2 : beam end 2 [fix, free, pinned, guide, spring]
        """
        # FIXME supports one plane
        self._support0 = get_support(end1)
        self._support1 = get_support(end2)
        #
        if not torsion1:
            torsion1 = end1
            
        if not torsion2:
            torsion2 = end2
        
        self._torsion.supports(end1=torsion1, end2=torsion2)
        #
        self._axial.supports(end1=self._support0[0],
                             end2=self._support1[0])
        #
        self._bminplane.supports(end1=self._support0[0],
                                 end2=self._support1[0])
        #
        self._bmoutplane.supports(end1=self._support0[1],
                                  end2=self._support1[1])        
    #
    # ----------------------------------------------
    # Beam Formulas
    #
    def _reaction(self, load):
        """beam end reactions in local system
        
        Return:
        R0 [Fa, T, Ry, Rz]
        R1 [Fa, T, Ry, Rz]
        """
        # get initial condition beam local system
        #
        #Fx = [load_name, load_title, load_type, load_system,
        #      beam_name, L_step,
        #      Axial, Torsion, BM_inplane, BM_outplane]
        #
        #Axial, BM_inplane, BM_outplane = [V, M, w, theta]
        #Torsion = [T, B, psi, phi, Tw]
        #
        (lname, ltitle, ltype, lsystem, bnumber, x,
         Fx_bar, Mx_bar, Fy_bar, Fz_bar) = load.Fx(x=self.L, L=self.L,
                                                   E=self.E, G=self.G, 
                                                   Iy=self.Iy, Iz=self.Iz,
                                                   J=self.J, Cw=self.Cw,
                                                   Area=self.area)
        #
        # [Axial, Torsion, BM_inplane, BM_outplane]
        R0 = self.R0(load=load)
        #
        # Axial [P, blank, blank, u]
        Fa = self._axial.response(x=self.L, R0=R0[0], Fx=Fx_bar)
        # Torsion [T, B, Psi, Phi, Tw]
        T1x = self._torsion.response(x=self.L, R0=R0[1], Fx=Mx_bar)
        # Bending in plane [V, M, theta, w]
        R1y = self._bminplane.response(x=self.L, R0=R0[2], Fx=Fy_bar)
        # Bending out plane [V, M, theta, w]
        R1z = self._bmoutplane.response(x=self.L, R0=R0[3], Fx=Fz_bar)
        #
        R1 = [Fa, T1x, R1y, R1z]
        #
        return R0, R1

    #
    def R0(self, load):
        """Reaction end 0 local system
        
        Return: 
        R0 [Fa, T, Ry, Rz]
        """
        #
        #Fx = [load_name, load_title, load_type, load_system,
        #      beam_name, L_step,
        #      Axial, Torsion, BM_inplane, BM_outplane]
        #
        #Axial, BM_inplane, BM_outplane = [V, M, w, theta]
        #Torsion = [T, B, psi, phi, Tw]        
        #
        (lname, ltitle, ltype, lsystem, bnumber, x,
         Fx_bar, Mx_bar, Fy_bar, Fz_bar) = load.Fx(x=self.L, L=self.L,
                                                   E=self.E, G=self.G, 
                                                   Iy=self.Iy, Iz=self.Iz,
                                                   J=self.J, Cw=self.Cw,
                                                   Area=self.area)
        #
        # Axial [P, blank, blank, u]
        suppx = self._axial.R0(Fbar=Fx_bar)
        # Torsion [T, B, Psi, Phi, Tw]
        suppt = self._torsion.R0(Fbar=Mx_bar)
        # Bending in plane [V, M, theta, w]
        suppy = self._bminplane.R0(Fbar=Fy_bar)
        # Bending out plane [V, M, theta, w]
        suppz =  self._bmoutplane.R0(Fbar=Fz_bar)
        #
        return suppx, suppt, suppy, suppz

    #
    def response(self, x: float, R0:list, Fx:list):
        """
        x : Distance from end 1
        I : Moment of inertia [m^4]
        load_list : list of load to be included in calculation (default use all)

        Return:
        Fx = [Axial, Torsion, BM_inplane, BM_outplane]
        
        ----------------------------- 
        Axial, BM_inplane, BM_outplane = [V, M, w, theta]
        V : Shear force
        M : Bending moment
        theta : Slope
        w : Deflection
        
        -----------------------------
        Torsion = [T, B, psi, phi, Tw]
        T : Twisting moment
        B : Bimoment
        psi : Rate of angle of twist
        phi: Angle of twist
        Tw : Warping torque
        """
        # calculate load at x
        Fax, Ftx, Fyx, Fzx = Fx
        R0x, R0t, R0y, R0z = R0
        #
        # Axial [P, blank, blank, u]
        R1x = self._axial.response(x=x, R0=R0x, Fx=Fax)
        # Torsion [T, B, Psi, Phi, Tw]
        R1t = self._torsion.response(x=x, R0=R0t, Fx=Ftx)       
        # Bending in plane [V, M, theta, w]
        R1y = self._bminplane.response(x=x, R0=R0y, Fx=Fyx)
        # Bending out plane [V, M, theta, w]
        R1z = self._bmoutplane.response(x=x, R0=R0z, Fx=Fzx)
        #
        return R1x, R1t, R1y, R1z
    #
    # ----------------------------------------------
    # Beam Calculations
    #
    def reactions(self, bloads) -> dict:
        """Calculate reactions
        
        Return : 
        load_name : [load_title, load_type,
                     load_system, beam_name,
                     R0[Fa, Tx, Ry, Rz],
                     R1[Fa, Tx, Ry, Rz]]
        """
        #
        # -----------------------------------------------------
        # loop basic load
        #
        reac = defaultdict(list)
        for item in bloads: 
            reac[item.load_name].append([item.title, 'basic',
                                         item.coordinate_system,
                                         item.name, 
                                         *self._reaction(load=item)])
        #
        return reac
    #
    def solve(self, bloads, steps:int=10) -> list[list]:
        """
        
        Return:
        Beam Forces 
        [load_title, load_type, load_system,
         beam_name, L_steps,
         Axial, Torsion, BM_inplane, BM_outplane]
        
        ----------------------------- 
        Axial, BM_inplane, BM_outplane = [V, M, w, theta]
        V : Shear force
        M : Bending moment
        theta : Slope
        w : Deflection
        
        -----------------------------
        Torsion = [T, B, psi, phi, Tw]
        T : Twisting moment
        B : Bimoment
        psi : Rate of angle of twist
        phi: Angle of twist
        Tw : Warping torque
        """
        # 
        header =  ['load_name', 'load_title','load_type',
                   'load_system', 'element_name'] 
        #
        #        
        # -----------------------------------------------------
        #
        # {load_name : [load_title, load_type,
        #               load_system, beam_name,
        #               R0[Fa, Tx, Ry, Rz],
        #               R1[Fa, Tx, Ry, Rz]] }     
        #
        reactions = self.reactions(bloads)
        #reac_df = self.support
        #grpsupp = reac_df.groupby(header)
        #
        #
        #Load function 
        # df [load_name, load_title, load_type, load_system,
        #     beam_name, x, Fx, Mx, Fy, Fz]        
        #
        bfunc_df = self.load_function(bloads, steps)
        beamfun = bfunc_df.groupby(header)        
        #
        # -----------------------------------------------------
        #
        #Fblank = [0, 0, 0, 0]
        lbforce = []
        for key, items in reactions.items():
            for Rb in items:
                title = (key, *Rb[:4])
                mnload = beamfun.get_group(title)
                mnload = mnload[['x','Fx','Mx','Fy','Fz']]
                #mnload.set_index('x', inplace=True)
                for bstep in mnload.itertuples():
                    lbforce.append([*title, bstep.x,
                                    *self.response(x=bstep.x, R0=[*Rb[4]],
                                                   Fx=[bstep.Fx, bstep.Mx,
                                                       bstep.Fy, bstep.Fz])])
                #
        #
        return lbforce
    #
    def forces(self, bloads, steps:int=10):
        """
        Return: 
        Beam force  
        Dataframe [load_name, load_title, load_type, load_system,
                   element_name, node_end,
                   Fx, Fy, Fz, Mx, My, Mz,
                   delta_x, delta_y, delta_z,
                   phi_x, theta_y, theta_z,
                   B(bimoment), Tw(Warping torque)]
        """
        # solve = [load_title, load_type, load_system,
        #          beam_name, L_steps,
        #          Axial, Torsion, BM_inplane, BM_outplane]
        #
        # Axial, BM_inplane, BM_outplane = [V, M, w, theta]
        # Torsion = [T, B, psi, phi, Tw]
        lbforce = self.solve(bloads, steps)
        lbforce =[[*lbf[:6], *lbf[6], *lbf[7], *lbf[8], *lbf[9]]
                  for lbf in lbforce]
        #
        # Member
        #
        header = ['load_name', 'load_title', 'load_type', 'load_system',
                  'element_name', 'node_end',
                  'Fx', 'blank1', 'blank2', 'delta_x',
                  'Mx', 'B', 'psi_t', 'phi_x', 'Tw', 
                  'Fy', 'Mz', 'theta_z', 'delta_y',
                  'Fz', 'My', 'theta_y', 'delta_z']
        #
        db = DBframework()
        df_mload = db.DataFrame(data=lbforce, columns=header, index=None)
        df_mload = df_mload[['load_name', 'load_title', 'load_type', 'load_system',
                             'element_name', 'node_end',
                             'Fx', 'Fy', 'Fz', 'Mx', 'My', 'Mz',
                             'delta_x', 'delta_y', 'delta_z',
                             'phi_x', 'theta_y', 'theta_z', 'B', 'Tw']]
        return df_mload
    #
    def load_function(self, bloads, steps:int=10):
        """
        Return:
        Load function 
        Dataframe [load_name, load_title, load_type, load_system,
                   element_name, x, Fx, Mx, Fy, Fz]
        """
        Lsteps = linspace(start=0, stop=self.L, num=steps+1, endpoint=True)        
        #
        # -----------------------------------------------------
        #
        #Fx = [load_name, load_title, load_type, load_system,
        #      beam_name, L_step,
        #      Axial, Torsion, BM_inplane, BM_outplane]
        #
        #Axial, BM_inplane, BM_outplane = [V, M, w, theta]
        #Torsion = [T, B, psi, phi, Tw]        
        #
        beamfun = []
        for idx, item in enumerate(bloads):
            lout = item.Fx(x=Lsteps, L=self.L,
                           E=self.E, G=self.G, 
                           Iy=self.Iy, Iz=self.Iz,
                           J=self.J, Cw=self.Cw, Area=self.area)
            #beamfun[item.load_name].extend(lout)
            beamfun.extend(lout)
        #
        header = ['load_name', 'load_title', 'load_type', 'load_system',
                  'element_name', 'x', 'Fx', 'Mx', 'Fy', 'Fz']
        db = DBframework()
        df_func = db.DataFrame(data=beamfun, columns=header, index=None)
        return df_func
        #return beamfun
#
#
@dataclass
class BeamBending:
    __slots__ = [ 'E', 'L', 'I', 
                  '_support', '_gen']
    
    def __init__(self, L: float, E:float, I: float) -> None:    
        """
        E : Elastic modulus
        """
        self.E = E
        self.L = L
        self.I = I
    #
    def supports(self, end1: str|list, end2: str|list,
                 k1: float|None = None, k2: float|None = None):
        """
        end1 : beam end 1 [fix, free, pinned, guide, spring]
        end2 : beam end 2 [fix, free, pinned, guide, spring]
        """
        self._support = [end1, end2, k1, k2]
    #
    def R0(self, Fbar:list):
        """Reaction end 1"""
        Mb = BeamBendingSupports(L=self.L, I=self.I, E=self.E)
        Mb.supports(*self._support)
        return Mb(F_bar=Fbar)
    #
    #
    def line(self, x: float|list,
             q1: float, q2: float,
             L1: float, L2: float):
        """line loading"""
        udl = Trapezoidal(q1=q1, q2=q2,
                          L=self.L, L1=L1, L2=L2)
        return udl(x=x, E=self.E, I=self.I)
    #
    def point(self, x: float|list,
              P: float, M: float, L1: float,):
        """ point loading"""
        #
        Fp = Point(P=P, L=self.L, L1=L1)
        Fm = Moment(M=M, L=self.L, L1=L1)
        return list(map(sum, zip(Fp(x, self.E, self.I),
                                 Fm(x, self.E, self.I))))

    #
    #def loading_function(self, x: float):
    #    """ Arbitrary Loading functions"""       
    #    return self._gen(x=x, E=self.E, I=self.I)
    #
    def response(self, x: float, R0: list, Fx:list):
        """ General reponse """
        res= BendingGE(E = self.E, I=self.I)
        res.R0(*R0)
        res.load(*Fx)
        return res.response(x=x)
#
#
@dataclass
class BeamTorsion:
    __slots__ = [ 'E', 'L', 'G', 'J', 'Cw', 
                  '_support', '_bar', '_open']
                # 'T1',  'L1', 'T2', 'L2']    
    
    def __init__(self, L: float, E:float, G:float,
                 J: float, Cw: float) -> None:    
        """
        L : beam Lenght 
        E : Elastic modulus
        G : Shear modulus
        """
        self.E = E
        self.G = G
        self.L = L
        self.J = J
        self.Cw = Cw
    #
    #
    def supports(self, end1: str|list, end2: str|list,
                 k1: float | None = None, k2: float | None = None):
        """
        end1 : beam end 1 [fix, free, pinned, guide, spring]
        end2 : beam end 2 [fix, free, pinned, guide, spring]
        """
        self._support = [end1, end2, k1, k2]
    #  
    #
    def R0(self, Fbar:list):
        """Reaction end 1"""
        #1 / 0
        try:
            # open section
            1 / self.Cw
            supp = BTOpenSupports(L=self.L,
                                  J=self.J, Cw=self.Cw,
                                  E=self.E, G=self.G)
        except ZeroDivisionError:
            # bar section
            supp = BTBarSupports(L=self.L, J=self.J,
                                 E=self.E, G=self.G)
        #
        supp.supports(*self._support)
        return supp(F_bar=Fbar)
    #
    #
    def torque(self, x: float|list,
               T: float, L1: float,
               T2: float|None = None, L2: float|None = None):
        """
        Torque
        """
        #
        if T2:
            # distributed
            barsec = TBarDistributed(T=T, T2=T2, L=self.L, L1=L1, L2=L2)
            opensec = TOpenDistributed(T=T, T2=T2, L=self.L, L1=L1, L2=L2)
        else:
            # concentrated
            barsec = TBarConcentrated(T=T, L=self.L, L1=L1)
            opensec = TOpenConcentrated(T=T, L=self.L, L1=L1)
        #
        try:
            # open section
            1 / self.Cw
            return opensec(x=x, E=self.E, G=self.G,
                              J=self.J, Cw=self.Cw)
        except ZeroDivisionError:
            # bar section
            return barsec(x=x, E=self.E, G=self.G, J=self.J)        
    #
    #
    #def loading_function(self, x: float):
    #    """ Arbitrary Loading functions"""
    #    try:
    #        # open section
    #        1 / self.Cw
    #        return self._open(x=x, E=self.E, G=self.G,
    #                          J=self.J, Cw=self.Cw)
    #    except ZeroDivisionError:
    #        # bar section
    #        return self._bar(x=x, E=self.E, G=self.G, J=self.J)
    #
    #
    def response(self, x: float, R0: list, Fx:list):
        """ General reponse
        
        [T, B, Psi, Phi, Tw]
        """
        try:
            # open section
            1 / self.Cw
            res = TorsionOpenGE(E=self.E, G=self.G,
                                J=self.J, Cw=self.Cw)
        except ZeroDivisionError:
            # bar section        
            res = TorsionBarGE(E=self.E, G=self.G, J=self.J)
        #
        res.R0(*R0)
        res.load(*Fx)
        return res.response(x=x)
#
#
@dataclass
class BeamAxial:
    __slots__ = [ 'E', 'L', 'A','_support', '_bar']
    
    def __init__(self, L: float, E:float, A: float) -> None:    
        """
        E : Elastic modulus
        """
        self.E = E
        self.L = L
        self.A = A
    #
    def supports(self, end1: str|list, end2: str|list,
                 k1: float|None = None, k2: float|None = None):
        """
        end1 : beam end 1 [fix, free, pinned, guide, spring]
        end2 : beam end 2 [fix, free, pinned, guide, spring]
        """
        self._support = [end1, end2, k1, k2]
    #
    #
    def R0(self, Fbar:list):
        """Reaction end 1"""
        supp = PBarSupports(L=self.L, E=self.E, A=self.A)
        supp.supports(*self._support)
        return supp(F_bar=Fbar)
    #
    def axial(self, x: float|list,
              P: float, L1: float,
              P2: float|None = None, L2: float|None = None):
        """
        x: 
        """
        #self.P1 = P
        #self.L1 = L1
        if P2:
            bar = PBarDistributed(P=P, P2=P2, L=self.L, L1=L1, L2=L2)
        else:
            bar = PBarConcentrated(P=P, L=self.L, L1=L1)
        if isinstance(x, (list,tuple)):
            1 / 0
        else:
            return bar(x=x, E=self.E, A=self.A)
    #
    #def loading_function(self, x: float):
    #    """ Arbitrary Loading functions"""
    #    return self._bar(x=x, E=self.E, A=self.A)
    #
    def response(self, x: float, R0: list, Fx:list):
        """ General reponse
        
        Results:
        [P, blank, blank, u]
        """
        res =  AxialBarGE(E = self.E, A=self.A)
        res.R0(*R0)
        res.load(*Fx)
        return res.response(x=x)
#
#
#
def list2str(axial: int, vertical: int, moment: int):
    """
    vertical [1:fixed, 0:free]
    moment
    """
    try:
        1 / int(vertical)
        if int(moment) == 1:
            return "fixed"
        else:
            return "pinned"
    except ZeroDivisionError:
        if int(moment) == 1:
            if int(axial) == 1:
                return "guided"
            else:
                return "free"
        else:
            return "free"
#
def get_support(support:list|str):
    """ """
    #
    if isinstance(support, (list, tuple)):
        #suppx = list2str(vertical=support[0], moment=support[3])
        suppy = list2str(axial=support[0], vertical=support[1], moment=support[5])
        suppz = list2str(axial=support[0], vertical=support[2], moment=support[4])
    elif isinstance(support, str):
        suppy = suppz = support
    else:
        raise IOError(f"fixity {support} not valid")
    #
    return suppy, suppz
#
#