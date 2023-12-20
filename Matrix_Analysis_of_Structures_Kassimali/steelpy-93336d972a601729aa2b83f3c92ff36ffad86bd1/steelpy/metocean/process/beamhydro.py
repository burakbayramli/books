#
# Copyright (c) 2009-2023 fem2ufo
#
from __future__ import annotations
# Python stdlib imports
#import math
from typing import NamedTuple
from dataclasses import dataclass
#
# package imports
#
import numpy as np
from numpy.matlib import repmat
#import matplotlib.pyplot as plt
#
from steelpy.utils.dataframe.main import DBframework
#
#
#
#
#
@dataclass
class BeamMorisonWave:
    __slots__ = ['_beam', 'surface', 'rho', 
                 '_data', '_type']
    def __init__(self, beam, rho: float):
        """
        rho : : Sea water density (1025)
        """
        self._beam = beam
        self.rho = rho
    #
    def elevations(self, nelev:int):
        """ Elevation Range"""
        n1, n2 = self._beam.nodes
        zmax = np.maximum(n1.y, n2.y)
        zmin = np.minimum(n1.y, n2.y)
        return np.linspace(zmin, zmax, nelev)
    #
    def dz(self, nelev:int):
        """ """
        elev = self.elevations(nelev=nelev)
        return np.diff(elev)
    #
    def Z(self, nelev:int):
        """ """
        elev = self.elevations(nelev=nelev)
        dz = np.diff(elev)
        # locating the middle point of each element
        Z = elev[:-1] + dz
        return Z

    #
    def Dh(self, mg):
        """Diamtre hydrodynamic"""
        section = self._beam.section
        D = section.diameter
        #mg = self.MG(Z)
        Dh = D + 2 * mg * 0
        At = np.pi * np.power(Dh, 2) / 4
        return Dh, At
    #
    def local_kin(self, kin, Vc):
        """ Kinematics local to the beam member

        beam : beam element
        kin : kinematics
        Vc : current
        """
        #
        uvector = np.array(self._beam.unit_vector)
        # uvector = [0.447, 0.525, 0.724]
        # print(f'Unit Vector [{uvector[0]}, {uvector[1]}, {uvector[2]}]')
        # print('')
        #
        # Components of velocity local to the member
        #
        Un = Vc + kin['ux'] - uvector[0] * (uvector[0] * (Vc + kin['ux']) + uvector[1] * kin['uz'])
        Vn = kin['uz'] - uvector[1] * (uvector[0] * (Vc + kin['ux']) + uvector[1] * kin['uz'])
        Wn = - uvector[0] * (uvector[0] * (Vc + kin['ux']) + uvector[1] * kin['uz'])
        #
        # print('')
        # print('========================================')
        # print('Components of velocity local to the member [N/m]')
        # print(f'Un ={np.max(Un): 1.4e}, Vn={np.max(Vn): 1.4e}, Wn={np.max(Wn): 1.4e}')
        # print(f'Un ={np.min(Un): 1.4e}, Vn={np.min(Vn): 1.4e}, Wn={np.min(Wn): 1.4e}')
        #
        # Water velocity normal to the cylinder axis
        #
        vn = Vc + np.sqrt(np.power(kin['ux'], 2) + np.power(kin['uz'], 2)
                          - np.power(uvector[0] * kin['ux'] + uvector[1] * kin['uz'], 2))
        #
        # print('')
        # print('Water velocity normal to the cilinder axis [N/m]')
        # print(f'vnmax ={np.max(vn): 1.4e}, vnmin={np.min(vn): 1.4e}')
        #
        # Components of acceleration local to the member
        #
        Anx = kin['ax'] - uvector[0] * (uvector[0] * kin['ax'] + uvector[1] * kin['az'])
        Anz = kin['az'] - uvector[1] * (uvector[0] * kin['ax'] + uvector[1] * kin['az'])
        Any = - uvector[0] * (uvector[0] * kin['ax'] + uvector[1] * kin['az'])
        #
        #
        # print('')
        # print('Components of acceleration local to the member [N/m]')
        # print(f'Anx ={np.max(Anx): 1.4e}, Any={np.max(Anz): 1.4e}, Anz={np.max(Any): 1.4e}')
        # print(f'Anx ={np.min(Anx): 1.4e}, Any={np.min(Anz): 1.4e}, Anz={np.min(Any): 1.4e}')
        # print('========================================')
        #
        return KinVel(Un, Vn, Wn, vn, self.rho), KinAcc(Anx, Anz, Any, self.rho)
    #
    def vn(self, kin, Vc):
        """ Absolute Water velocity normal to the cylinder axis
        
        Vc : current velocity
        """
        # Absolute Water velocity normal to the cylinder axis
        uvector = np.array(self._beam.unit_vector)
        #
        vn = Vc + np.sqrt(np.power(kin['ux'], 2) + np.power(kin['uz'], 2)
                          - np.power(uvector[0] * kin['ux'] + uvector[1] * kin['uz'], 2))
        return vn
    #
    def Un(self, kin, Vc):
        """ Instantaneous undisturbed velocity resolved normal to the member including both wave and current
        
        kin :
        Vc : current velocity
        """
        # Components of velocity local to the member
        uvector = np.array(self._beam.unit_vector)
        #
        Un = Vc + kin['ux'] - uvector[0] * (uvector[0] * (Vc + kin['ux']) + uvector[1] * kin['uz'])
        Vn = kin['uz'] - uvector[1] * (uvector[0] * (Vc + kin['ux']) + uvector[1] * kin['uz'])
        Wn = - uvector[0] * (uvector[0] * (Vc + kin['ux']) + uvector[1] * kin['uz'])
        #
        vn = self.vn(kin, Vc)
        #
        return KinVel(Un, Vn, Wn, vn, self.rho)
    #
    def An(self, kin):
        """ Instantaneous undisturbed acceleration resolved normal to the member
        
        kin : 
        """
        # Components of acceleration local to the member
        uvector = np.array(self._beam.unit_vector)
        #
        Anx = kin['ax'] - uvector[0] * (uvector[0] * kin['ax'] + uvector[1] * kin['az'])
        Anz = kin['az'] - uvector[1] * (uvector[0] * kin['ax'] + uvector[1] * kin['az'])
        Any = - uvector[0] * (uvector[0] * kin['ax'] + uvector[1] * kin['az'])
        #        
        return KinAcc(Anx, Anz, Any, self.rho)
    #
    def dF(self, Dh, At, Cd, Cm,
           kinvel, kinacc,
           Elev, dz):
        """Components of the force per unit of cilinder length acting in
        the x, y and z dir are given by the generalized Morisson equation
        
        dF = Fn + Ft
        
        Dh : Hydrodynamic diametre
        At : Cross sectional Area
        Cd : Drag coefficient
        Cm : Mass coefficient
        
        """
        #
        #
        dmx, dmy, dmz = kinacc.FIn(At, Cm)
        # dmx = self.mass(At, cm, kinacc.Anx)
        # dmy = self.mass(At, cm, kinacc.Any)
        # dmz = self.mass(At, cm, kinacc.Anz)
        # print('')
        # print('Components of mass per unit on cilinder lenght [N/m]')
        # print(f'dmx ={np.max(dmx): 1.4e}, dmy={np.max(dmy): 1.4e}, dmz={np.max(dmz): 1.4e}')
        # print(f'dmx ={np.min(dmx): 1.4e}, dmy={np.min(dmy): 1.4e}, dmz={np.min(dmz): 1.4e}')
        #
        ddx, ddy, ddz = kinvel.FDn(Dh, Cd)
        # ddx = self.drag(Dh, cd, kinvel.Un, vn)
        # ddy = self.drag(Dh, cd, kinvel.Vn, vn)
        # ddz = self.drag(Dh, cd, kinvel.Wn, vn)
        # print('')
        # print('Components of drag per unit on cilinder lenght [N/m]')
        # print(f'ddx ={np.max(ddx): 1.4e}, ddy={np.max(ddy): 1.4e}, ddz={np.max(ddz): 1.4e}')
        # print(f'ddx ={np.min(ddx): 1.4e}, ddy={np.min(ddy): 1.4e}, ddz={np.min(ddz): 1.4e}')
        #
        #
        fx = dmx + ddx
        fy = dmy + ddy
        fz = dmz + ddz
        #
        #
        # print('')
        # print('Components of the force per unit on cilinder lenght [N/m]')
        # print(f'qx ={np.max(fx): 1.4e}, qy={np.max(fy): 1.4e}, qz={np.max(fz): 1.4e}')
        # print(f'qx ={np.min(fx): 1.4e}, qy={np.min(fy): 1.4e}, qz={np.min(fz): 1.4e}')
        # print('========================================')
        #
        return BeamUnitForce(fx, fy, fz, dz, Elev, self._beam.name)
    #
    #def ft(self):
    #    """ Component along the axis of the cylinder (a tangential component)"""
    #    return None
    #
    def Fwave(self, Vc, MG, Cd, Cm,
              kinematics, nelev:int=10):
        """
        Wave force on a slender cilindrical element
        
        Vc : Current velocity
        MG : Marine Growth
        Cd : Drag Coefficient
        Cm : Inertia Coefficient
        kinematics : Kinematic class
        nelev : number of elevations
        """
        Elev = self.elevations(nelev=nelev)
        Dh, At = self.Dh(mg=MG)
        dz = self.dz(nelev=nelev)
        #
        shape = kinematics['ax'].shape
        Vc = permute1(Vc, order=shape[0])
        #
        kinacc = self.An(kinematics)
        kinvel = self.Un(kinematics, Vc)
        #kinvel, kinacc = self.local_kin(kinematics, Vc)
        #
        return self.dF(Dh, At, Cd, Cm,
                       kinvel, kinacc,
                       Elev, dz)
        #return udl
    #

#
#
class BeamUnitForce(NamedTuple):
    """Components of the force per unit of cilinder lenght"""
    qx: list
    qy: list
    qz: list
    dz: list
    elevation: list
    beam_name: str | int
    #
    #
    @property
    def df(self):
        """Dataframe of beam's partial linearly variable load
        
        [load_title, 'beam', beam_name, 'line',  qx0,qy0,qz0, qx1,qy1,qz1, L0,L1, comment(optional)]"""
        #
        coords = self.qx.coords
        rows = coords['x'].values
        cols = coords['z'].values
        wlength = coords['length'].values
        #
        #
        Fx, Fy, OTM = self.span_loading()
        #
        #
        # FIXME: wave system to beam local system (is this fixed already?)
        #
        qitem = self.qx.to_dataframe(name='qx').reset_index()
        qy = self._get_line(qname='qx', qitem=qitem)
        qitem = self.qy.to_dataframe(name='qy').reset_index()
        qz = self._get_line(qname='qy', qitem=qitem)
        qitem = self.qz.to_dataframe(name='qz').reset_index()
        qx = self._get_line(qname='qz', qitem=qitem)
        #
        # TODO: L1 and L2 should not be zero for all cases
        dftemp = []
        for x, row in enumerate(rows):
            for idx, wstep in enumerate(wlength):
                for hstep, col in enumerate(cols):
                    ldata = list(zip(qx[idx][hstep], qy[idx][hstep], qz[idx][hstep]))
                    dftemp.append(['beam', self.beam_name, 'line',
                                   *ldata[0], *ldata[1], 0, 0,
                                   float(Fx[x, hstep, idx].values),
                                   float(OTM[x, hstep, idx].values), 
                                   row, wstep, col])
        # setup df's columns
        header = ['element_type', 'element_name', 'load_type',
                  'qx0', 'qy0', 'qz0', 'qx1', 'qy1', 'qz1',
                  'L0', 'L1', 'BS', 'OTM', 
                  'x', 'y', 'z']
        #
        df = DBframework()
        dfload = df.DataFrame(data=dftemp, columns=header, index=None)
        # print('--->')
        # 1 / 0
        return dfload

    #
    #
    def _get_line2(self, qname: str, qitem):
        """ """
        elev = self.elevation
        coords = qitem.coords
        rows = coords['x'].values
        cols = coords['col'].values
        wlength = coords['length'].values
        #
        1 / 0
        grpx = qitem.groupby('x')
        for keyx, item in grpx:
            grp2 = item.groupby('length')
            for keyw, wstep in grp2:
                keyw, wstep
                if -93.75 in wstep.coords['col']:
                    print('--')

        for row in rows:
            for wstep in wlength:
                for el in elev:
                    try:
                        cols = qitem.sel(x=row, col=el, length=wstep)
                        print('--')
                    except TypeError:
                        step = 0
        #
        1 / 0
        print('--')

    #
    #
    def _get_line(self, qname: str, qitem):
        """ """
        elev = self.elevation
        # data = qx.groupby(['x'])[name].agg(lambda x : x.tolist())
        qgrp = qitem.groupby(['x', 'length'])[['z', qname]]
        # TODO : optimize
        load_1 = []
        for key, item in qgrp:
            load_2 = []
            step = None
            for el in elev:
                idx = item.index[item['z'] == el].tolist()
                try:
                    qn = np.round(item[qname][idx[0]], decimals=3)
                    load_2.append([step, qn])
                    step = qn
                except IndexError:
                    step = 0
            # print(key, item)
            load_1.append(load_2)
        #
        return load_1

    #
    def span_loading(self):
        """ """
        qx = self.qx
        qz = self.qz
        dz = permute2(self.dz, (qx.shape[0], qx.shape[2]), 1)
        Fx = qx.cumsum(dim='z') * dz
        Fz = qz.cumsum(dim='z') * dz
        #
        Z = self.elevation[:-1] + self.dz
        Z = permute2(Z, (qx.shape[0], qx.shape[2]), 1)
        OTM = Fx * Z
        return Fx, Fz, OTM
#
#
#
class KinVel(NamedTuple):
    """

    Un : Kinematic components of velocity
    Vn : Kinematic components of velocity
    Wn : Kinematic components of velocity
    vn : Fluid velocity normal to the cylinder axis
    rho : Sea water density (1025)
    """
    Un: list
    Vn: list
    Wn: list
    vn : list
    rho: float
    #
    def fdn(self, D, cd, UX, vn):
        """
        D  : Member diametre
        cd : Drag coefficient
        Ux : Instantaneus velocity resolved normal to the member
        Vn : Fluid velocity normal to the cylinder axis
        """
        # drag load per unit length
        Fdn = 0.5 * self.rho * cd * D * UX * vn
        #return pdrag * dz
        #bdrag = np.sum(ddrag, axis=2)
        #bdrag = ddrag.sum(dim='z')
        #return ddrag
        return Fdn
    #
    def FDn(self, Dt:float, Cd:float):
        """
        Component of drag force per unit of cylinder length

        Dt : Diametre tubular
        Cd : Drag coefficient
        
        Return:
        FDn [x,y,z]
        """
        Dh = permute2(Dt, (self.Un.shape[0], self.Un.shape[2]), 1)
        cd = permute2(Cd, (self.Un.shape[0], self.Un.shape[2]), 1)
        #
        FDnx = self.fdn(Dh, cd, self.Un, self.vn)
        FDny = self.fdn(Dh, cd, self.Vn, self.vn)
        FDnz = self.fdn(Dh, cd, self.Wn, self.vn)
        #
        return FDnx, FDny, FDnz
#
#
class KinAcc(NamedTuple):
    """
    Anx : Kinematic components of velocity x
    Any : Kinematic components of velocity y
    Anz : Kinematic components of velocity z
    rho : Sea water density (1025)
    """
    Anx: list
    Any: list
    Anz: list
    rho: float
    #
    def fin(self, at, cm, An):
        """
        at : Cross sectional area
        cm : Inertia coeffient
        An : Instantaneus acceleration resolved normal to the member
        
        Retuns:
        Fin : inertia load per unit length
        """
        # inertia load per unit length
        Fin = self.rho * cm * at * An
        # figure(4)
        # hold all
        # plot(pinertia(:),Z(:),'.-','LineWidth',1)

        # figure(5)
        # hold all
        # plot(pdrag(:),Z(:),'.-','LineWidth',1)

        # dinertia = pinertia * dz
        # binertia = dinertia.sum(dim='z')
        # return dinertia
        return Fin
    #
    def FIn(self, At:float, Cm:float):
        """
        Component of inertia force per unit of cylinder length normal to the member

        At : Area tubular
        Cm : Mass coefficient
        
        Returns
        FIn [x,y,z]
        """
        at = permute2(At, (self.Anx.shape[0], self.Anx.shape[2]), 1)
        cm = permute2(Cm, (self.Anx.shape[0], self.Anx.shape[2]), 1)
        #
        FInx = self.fin(at, cm, self.Anx)
        FIny = self.fin(at, cm, self.Any)
        FInz = self.fin(at, cm, self.Anz)
        return FInx, FIny, FInz
#
#
#
#
def permute2(A, order, axis:int=1):
    """ """
    A1 = repmat(A, order[1], axis)
    A1 = np.transpose(A1)
    A1 = np.expand_dims(A1, axis=0)
    A1 = np.transpose(A1)
    A1 = np.tile(A1, order[0])
    return np.transpose(A1)
#
def permute1(A, order):
    """ """
    A1 = np.transpose(A)
    A1 = np.expand_dims(A1, axis=0)
    return np.tile(A1, order)
#
#