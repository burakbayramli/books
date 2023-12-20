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
#
from steelpy.utils.dataframe.main import DBframework
#


#
#
class KinVel(NamedTuple):
    """ Kinematic components of velocity"""
    Un: list
    Vn: list
    Wn: list
#
#
class KinAcc(NamedTuple):
    """ Kinematic components of velocity"""
    Anx: list
    Any: list
    Anz: list
#
#
class BeamUnitForce(NamedTuple):
    """Components of the force per unit of cilinder lenght"""
    qx: list
    qy: list
    qz: list
    elevation: list
    beam_name: str | int
    #
    def line(self):
        """get line loading
        [load_title, 'beam', beam_name, 'line',  qx0,qy0,qz0, qx1,qy1,qz1, L0,L1, comment(optional)]"""
        #qx = self.qx.isel(x=0)
        #row, col, length = self.qx.coords[['x','z','length']]
        coords = self.qx.coords
        rows = coords['x'].values
        cols = coords['z'].values
        wlength = coords['length'].values
        #
        #qx = self._get_line2(qname ='qx', qitem=self.qx)
        #
        qitem = self.qx.to_dataframe(name='qx').reset_index()
        qx = self._get_line(qname ='qx', qitem=qitem)
        qitem = self.qy.to_dataframe(name='qy').reset_index()
        qy = self._get_line(qname='qy', qitem=qitem)
        qitem = self.qz.to_dataframe(name='qz').reset_index()
        qz = self._get_line(qname='qz', qitem=qitem)
        #
        lforce = []
        for x, row in enumerate(rows):
            #lforce = []
            for idx, wstep in enumerate(wlength):
                for hstep, col in enumerate(cols):
                    #coord = f'{0}_{0}_{self.elevation[hstep]}'
                    ldata = list(zip(qx[idx][hstep], qy[idx][hstep], qz[idx][hstep]))
                    lforce.append(['beam', self.beam_name, 'line',
                                   *ldata[0], *ldata[1], 0, 0,
                                   row, wstep, col])
            #lforce1.append(lforce)
        #
        header =  ['element_type', 'element_name', 'load_type',
                   'qx0','qy0','qz0', 'qx1','qy1','qz1',
                   'L0','L1', 'x', 'y', 'z']
        #
        df = DBframework()
        dfload = df.DataFrame(data=lforce, columns=header, index=None)        
        #
        #print('--->')
        #1 / 0
        return dfload
    #
    #
    def _get_line2(self, qname:str, qitem):
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
                for el in  elev:
                    try:
                        cols =  qitem.sel(x=row, col=el, length=wstep)
                        print('--')
                    except TypeError:
                        step = 0
        #
        1 / 0
        print('--')
    #
    #
    def _get_line(self, qname:str, qitem):
        """ """
        elev = self.elevation
        #data = qx.groupby(['x'])[name].agg(lambda x : x.tolist())
        qgrp = qitem.groupby(['x', 'length'])[['z', qname]]
        #
        load_1 = []
        for key, item in qgrp:
            load_2 = []
            step = None
            for el in  elev:
                idx = item.index[item['z'] == el].tolist()
                try:
                    qn = np.round(item[qname][idx[0]], decimals=3)
                    load_2.append([step, qn])
                    step = qn
                except IndexError:
                    step = 0
            #print(key, item)
            load_1.append(load_2)
        #
        return load_1
#
#
@dataclass
class BSOTM:
    """
    Calculate the base shear and overturning moment of single pile

    Parameters
    ----------
    ux : horizontal wave particle velocity
    ax : horizontal wave particle acceleration
    z : z-coordinate
    d : Water depth
    D : diamere of cylinder
    condtion :
        1.Linear wave
        2.Non-linear wave
    rho : Sea water density 
    
    Returns
    -------
    bs  : Base shear
    otm : Overturning moment
    """
    kinematics: list
    condition: int = 1
    rho: float = 1025 # 
    #
    #
    def CdCm(self, Z, HTs: float):
        """ """
        cm = np.zeros((Z.shape))
        cm += 1.2
        cm[Z > HTs] = 1.6
        #cm[Z <= 2] = 1.2
        #
        cd = np.zeros((Z.shape))
        # switch condition
        if self.condition == 1:
            cd += 1.15
        else:
            #elif condition == 2:
            cd += 1.05
            cd[Z > HTs] = 0.65
            #cd[Z <= 2] = 1.05
        #
        return cd, cm
    #
    #
    def MGX(self, Z):
        """ MArine growth"""
        mgr = np.zeros((Z.shape))
        mgr[:, :, :] = 0.90 # m
        mgr[Z<-25.0] = 0.50 # m
        mgr[Z>2] = 0.0      # m
        return mgr
    #
    #
    def MG(self, Z):
        """ MArine growth"""
        mgr = np.zeros((Z.shape))
        mgr[:] = 0.90 # m
        mgr[Z < -25.0] = 0.50 # m
        mgr[Z > 2] = 0.0      # m
        return mgr
    #    
    #
    def Dh(self, D: float, Z):
        """Diamtre hydrodynamic"""
        mg = self.MG(Z)
        Dh = D + 2 * mg * 0
        At = np.pi * np.power(Dh, 2) / 4
        return Dh, At
    #
    #
    def Vc2(self, Vct: float, d: float, Z):
        """Current Velocity"""
        vc =  np.zeros((Z.shape))
        vc[:, :, :] = Vct
        vcr = np.power(np.abs(Vct * (Z + d), 1.0/7.0))
        vidx = Z < 0
        vc[vidx] = vcr[vidx]
        return vc
    #
    def Vc(self, Vct: float, d: float, z: list, eta: list):
        """Current Velocity"""
        zd = (z + d) / d
        vc =  np.zeros((eta.size, zd.size))
        vc[:, :] = Vct
        #vcr1 = np.abs(Vct *  d)
        #
        ze = np.zeros((eta.size, zd.size))
        ze[:, :] = z
        zebool = np.transpose(ze[:, :].T < eta)
        #
        vcr = np.zeros((eta.size, zd.size))
        vcr[:, :] = np.power(np.abs(Vct *  zd), 1.0/7.0)
        #
        vc[zebool] = vcr[zebool]
        return vc    
    #
    #
    def mass(self, At, cm, AX):
        """ """
        # inertia load per unit length
        pinertia = self.rho * cm * At * AX  
        # figure(4)
        # hold all
        # plot(pinertia(:),Z(:),'.-','LineWidth',1)
    
        # figure(5)
        # hold all
        # plot(pdrag(:),Z(:),'.-','LineWidth',1)
    
        #dinertia = pinertia * dz
        #binertia = dinertia.sum(dim='z')
        #return dinertia
        return pinertia
    #
    def drag(self, D: float, cd, UX, vn):
        """ """
        # drag load per unit length
        pdrag = 0.5 * self.rho * cd * D * UX * vn
        #return pdrag * dz
        #bdrag = np.sum(ddrag, axis=2)
        #bdrag = ddrag.sum(dim='z')
        #return ddrag
        return pdrag
    #
    #
    #def base_shear(self, dinertia, ddrag):
    #    """ """
    #    #bdrag = ddrag.sum(dim='z')
    #    #binertia = dinertia.sum(dim='z')
    #    bs = dinertia + ddrag
    #    return bs
    #
    def OTM(self, dinertia, ddrag, Z, d):
        """ """
        oinertia = dinertia * (Z + d)
        #oinertia = oinertia.sum(dim='z')
        #
        odrag = ddrag * (Z + d)
        #odrag = odrag.sum(dim='z')
        otm = oinertia + odrag
        #
        otm = otm.to_dataframe(name='OTM').reset_index()
        #otm.drop('row', axis=1, inplace=True)
        #otm['z'] = Z.flatten()
        return otm
    #
    def BS(self, dinertia, ddrag, Z):
        """ Base Shear"""
        bs = dinertia + ddrag
        #bs = self.base_shear(dinertia, ddrag)
        #
        bs = bs.to_dataframe(name='BS').reset_index()
        #bs.drop('row', axis=1, inplace=True)
        #
        #bs['z'] = Z.flatten()
        return bs
    #
    def solveBSOTM(self, D: float, L: float):
        """
        D : Pile diametre
        L : Pile length
        """
        d = self.kinematics.d
        z =  self.kinematics.z
        #ux =  self.kinematics.ux
        #ax =  self.kinematics.ax
        #
        eta = self.kinematics.surface.eta
        #
        crestmax = np.max(eta)        
        #
        # [x, z, time] = value --> irregular
        # [x, z, lenght] = value --> regular
        #
        #UX = ux[:, :-1, :] + ux.diff('z') * 0.50
        #AX = ax[:, :-1, :] + ax.diff('z') * 0.50
        #
        # sea water density
        #rho = self.rho  
        #
        dz = np.diff(z)
        # locating the midle point of each element
        Z = z[:-1] + dz * 0.50
        #
        # -----------------------------------------
        # Kinematis
        #
        kin = self.kinematics.get_kin(Z)
        #
        UX = kin['ux']
        AX = kin['ax']
        #
        # -----------------------------------------
        #
        Dh, At = self.Dh(D, Z)
        #
        # -----------------------------------------
        #
        # get Cd & Cm
        cd, cm = self.CdCm(Z, crestmax)
        #
        # -----------------------------------------
        #
        #eta = self.kinematics.surface.eta
        Vct=1.54
        #zd = (z + d) / d
        Vc = self.Vc(Vct, d, z, eta)
        #
        #        
        # -----------------------------------------
        #
        #
        Z = permute2(Z, (UX.shape[0],UX.shape[2]), 1)
        #
        dz = permute2(dz, (UX.shape[0],UX.shape[2]), 1)
        #
        At = permute2(At, (UX.shape[0],UX.shape[2]), 1)
        #
        cm = permute2(cm, (UX.shape[0],UX.shape[2]), 1)
        #
        cd = permute2(cd, (UX.shape[0],UX.shape[2]), 1)        
        #
        Dh = permute2(Dh, (UX.shape[0],UX.shape[2]), 1)
        #
        # -----------------------------------------
        #        
        dmass = self.mass(At, cm, AX)
        #
        ddrag = self.drag(Dh, cd, UX, np.abs(UX))
        #
        bs = self.BS(dmass, ddrag, Z)
        #
        otm = self.OTM(dmass, ddrag, Z, d)
        #
        return bs, otm
    #
    # -----------------------------------------------
    #
    def span_loading(self, fx, fz, dz):
        """ """
        #Fx = np.cumsum(fx, axis=1) * Lm / (nelev - 1)
        #Fz = np.cumsum(fz, axis=1) * Lm / (nelev - 1)
        Fx = fx.cumsum(dim='z') * dz
        Fz = fz.cumsum(dim='z') * dz        
        #Fx = fx * Lm / (nelev - 1)
        #Fz = fz * Lm / (nelev - 1)        
        # x, range, wave lenght
        return Fx, Fz
    #
    def local_kin(self, beam, kin, Vc):
        """ Kinematics local to the member
        
        beam : beam element
        kin : kinematics
        Vc : current
        """
        #
        uvector = np.array(beam.unit_vector)
        #uvector = [0.447, 0.525, 0.724]
        print(f'Unit Vector [{uvector[0]}, {uvector[1]}, {uvector[2]}]')
        print('')
        #
        # Components of velocity local to the member
        #
        Un = Vc + kin['ux'] - uvector[0] * (uvector[0] * (Vc + kin['ux']) + uvector[1] * kin['uz'])
        Vn = kin['uz'] - uvector[1] * (uvector[0] * (Vc + kin['ux']) + uvector[1] * kin['uz'])
        Wn = - uvector[0] * (uvector[0] * (Vc + kin['ux']) + uvector[1] * kin['uz'])
        #
        print('')
        print('========================================')
        print('Components of velocity local to the member [N/m]')
        print(f'Un ={np.max(Un): 1.4e}, Vn={np.max(Vn): 1.4e}, Wn={np.max(Wn): 1.4e}')
        print(f'Un ={np.min(Un): 1.4e}, Vn={np.min(Vn): 1.4e}, Wn={np.min(Wn): 1.4e}')        
        #
        # Water velocity normal to the cilinder axis
        #
        vn = Vc + np.sqrt(np.power(kin['ux'], 2) + np.power(kin['uz'], 2)
                          - np.power(uvector[0] * kin['ux'] + uvector[1] * kin['uz'], 2))
        #
        print('')
        print('Water velocity normal to the cilinder axis [N/m]')
        print(f'vnmax ={np.max(vn): 1.4e}, vnmin={np.min(vn): 1.4e}')       
        #
        # Components of acceleration local to the member
        #
        Anx = kin['ax'] - uvector[0] * (uvector[0] *  kin['ax'] + uvector[1] * kin['az'])
        Anz = kin['az'] - uvector[1] * (uvector[0] * kin['ax'] + uvector[1] * kin['az'])
        Any = - uvector[0] * (uvector[0] *  kin['ax'] + uvector[1] * kin['az'])
        #
        #
        print('')
        print('Components of acceleration local to the member [N/m]')
        print(f'Anx ={np.max(Anx): 1.4e}, Any={np.max(Anz): 1.4e}, Anz={np.max(Any): 1.4e}')
        print(f'Anx ={np.min(Anx): 1.4e}, Any={np.min(Anz): 1.4e}, Anz={np.min(Any): 1.4e}')
        print('========================================')
        #
        return KinVel(Un, Vn, Wn), KinAcc(Anx, Anz, Any),  vn
    #
    def local_uforce(self, Dh, At, cd, cm,
                     kinvel, kinacc, vn,
                     Elev, beam):
        """Components of the force per unit of cilinder length acting in 
        the x, y and z dir are given by the generalized Morisson equation
        
        """
        dmx = self.mass(At, cm, kinacc.Anx)
        dmy = self.mass(At, cm, kinacc.Any)
        dmz = self.mass(At, cm, kinacc.Anz)
        print('')
        print('Components of mass per unit on cilinder lenght [N/m]')
        print(f'dmx ={np.max(dmx): 1.4e}, dmy={np.max(dmy): 1.4e}, dmz={np.max(dmz): 1.4e}')
        print(f'dmx ={np.min(dmx): 1.4e}, dmy={np.min(dmy): 1.4e}, dmz={np.min(dmz): 1.4e}')
        #
        ddx = self.drag(Dh, cd, kinvel.Un, vn)
        ddy = self.drag(Dh, cd, kinvel.Vn, vn)
        ddz = self.drag(Dh, cd, kinvel.Wn, vn)
        print('')
        print('Components of drag per unit on cilinder lenght [N/m]')
        print(f'ddx ={np.max(ddx): 1.4e}, ddy={np.max(ddy): 1.4e}, ddz={np.max(ddz): 1.4e}')
        print(f'ddx ={np.min(ddx): 1.4e}, ddy={np.min(ddy): 1.4e}, ddz={np.min(ddz): 1.4e}')
        #
        #
        fx = dmx + ddx
        fy = dmy + ddy
        fz = dmz + ddz
        #
        #
        print('')
        print('Components of the force per unit on cilinder lenght [N/m]')
        print(f'qx ={np.max(fx): 1.4e}, qy={np.max(fy): 1.4e}, qz={np.max(fz): 1.4e}')
        print(f'qx ={np.min(fx): 1.4e}, qy={np.min(fy): 1.4e}, qz={np.min(fz): 1.4e}')
        print('========================================')
        #
        return BeamUnitForce(fx, fy, fz, Elev, beam.name)
    #
    def wave_force(self, mesh, nelev=5):
        """Calculation of wave forces on beam elements"""
        beams = mesh.elements().beams()
        #
        # process
        #
        beam = beams[12]
        #uvector = np.array(beam.unit_vector)
        #print(f'Unit Vector [{uvector[0]}, {uvector[1]}, {uvector[2]}]')
        #print('')
        #Duv = uvector / 100
        #Lm = beam.L
        #
        n1, n2 = beam.nodes
        #x = n1.x - Duv[0] * 0.5
        #y = n1.y - Duv[1] * 0.5
        #z = n1.z - Duv[2] * 0.5
        #
        # -----------------------------------------
        #
        d = self.kinematics.d
        z =  self.kinematics.z
        #ux =  self.kinematics.ux
        #uz =  self.kinematics.uz
        #print(f'Max horizontal wave particle velocity {max(np.max(ux), np.min(ux), key=abs): 1.4e}')
        #print(f'Max vertical wave particle velocity {max(np.max(uz), np.min(uz), key=abs): 1.4e}')        
        #
        #ax =  self.kinematics.ax
        #az =  self.kinematics.az
        #print('')
        #print(f'Max horizontal wave particle acceleration {max(np.max(ax), np.min(ax), key=abs): 1.4e}')
        #print(f'Max vertical wave particle acceleration {max(np.max(az), np.min(az), key=abs): 1.4e}')        
        #
        eta = self.kinematics.surface.eta
        #
        crestmax = np.max(eta)
        #
        # -----------------------------------------
        #
        section = beam.section
        D = section.diameter
        #
        zmax = np.maximum(n1.y, n2.y)
        zmin = np.minimum(n1.y, n2.y)
        Elev = np.linspace(zmin, zmax, nelev)
        #
        #b_range = np.abs(n2.y - n1.y)
        #
        #bzidx = (z > zmin) & (z < zmax)
        #
        #
        # -----------------------------------------
        #
        dz = np.diff(Elev)
        # locating the midle point of each element
        Z = Elev[:-1] + dz        
        #
        # -----------------------------------------
        # Hydro diametre & area
        Dh, At = self.Dh(D, Z)
        #
        # -----------------------------------------
        # Cd & Cm
        cd, cm = self.CdCm(Z, crestmax)
        #
        # -----------------------------------------
        # Current
        Vct=1.54
        Vc = self.Vc(Vct, d, z, eta)
        Vc = 0
        #
        # -----------------------------------------
        # Kinematis
        #
        kin = self.kinematics.get_kin(Z)
        #
        print('')
        print('--> local Member')
        print(f"Max horizontal wave particle velocity {max(np.max(kin['ux']), np.min(kin['ux']), key=abs): 1.4e} m/s")
        print(f"Max vertical wave particle velocity {max(np.max(kin['uz']), np.min(kin['uz']), key=abs): 1.4e} m/s")
        print('')
        print(f"Max horizontal wave particle acceleration {max(np.max(kin['ax']), np.min(kin['ax']), key=abs): 1.4e} m/s2")
        print(f"Max vertical wave particle acceleration {max(np.max(kin['az']), np.min(kin['az']), key=abs): 1.4e} m/s2")
        print('--> local Member')
        print('')
        #
        #
        # ---------------------------------------
        #
        kinvel, kinacc, vn  = self.local_kin(beam, kin, Vc)
        #
        # ---------------------------------------
        #
        At = permute2(At, (vn.shape[0], vn.shape[2]), 1)
        #
        cm = permute2(cm, (vn.shape[0], vn.shape[2]), 1)
        #
        cd = permute2(cd, (vn.shape[0], vn.shape[2]), 1)        
        #
        Dh = permute2(Dh, (vn.shape[0], vn.shape[2]), 1)        
        #
        dz = permute2(dz, (vn.shape[0], vn.shape[2]), 1)
        #
        Z = permute2(Z, (vn.shape[0], vn.shape[2]), 1)
        #
        # ---------------------------------------
        #
        # Components of the force per unit of cilinder length acting in 
        # the x, y and z dir are given by the generalized Morisson equation
        #        
        udl = self.local_uforce(Dh, At, cd, cm,
                                kinvel, kinacc, vn,
                                Elev, beam)
        #
        lineload = udl.line()
        #
        # ---------------------------------------
        # Calculate wave loading on exposed span
        #
        Fx, Fy = self.span_loading(udl.qx, udl.qy, dz)
        #
        print('')
        print('Total combined force [kN]')
        print(f'Fx ={np.max(Fx) / 1000: 1.3e}, Fy={np.max(Fy) / 1000: 1.3e}')
        print('---')
        #
        #
        return lineload
    #
    #
#
def bsotm_reg(kinematics,
              D: float,
              condition: int=1,
              rho:float = 1025):
    """
    Calculate the base shear and overturning moment of single pile

    Parameters
    ----------
    ux : horizontal wave particle velocity
    ax : horizontal wave particle acceleration
    z : z-coordinate
    d : Water depth
    D : diamatre of cylinder
    condtion :
        1.Linear wave
        2.Non-linear wave

    Returns
    -------
    bs   : base shear
    ovtm : overturning moment
    """
    # [Wave surface x coord, time,  Water depth]
    ux =  kinematics.ux
    ax =  kinematics.ax
    z =  kinematics.z
    d = kinematics.d
    #
    # sea water density
    rho = rho  
    #dz = z[1:] - z[:-1]
    dz = np.diff(z)
    # locating the midle point of each element
    Z = z[:-1] + dz * 0.50
    #
    #UX = (ux[:, :, 1:] - ux[:, :, :-1]) / 2 + ux[:, :, :-1]
    #AX = (ax[:, :, 1:] - ax[:, :, :-1]) / 2 + ax[:, :, :-1]
    #
    # [x, z, time] = value --> irregular
    # [dummy, z, x] = value --> regular
    #
    UX = ux[:, :-1, :] + ux.diff('z') * 0.50
    AX = ax[:, :-1, :] + ax.diff('z') * 0.50
    #
    Z = permute2(Z, (UX.shape[0],UX.shape[2]), 1)
    #
    dz = permute2(dz, (UX.shape[0],UX.shape[2]), 1)
    #dz = permute(dz, [1, 1, 1])
    #dz = np.tile(dz, [UX.shape[0], UX.shape[1], 1])
    # dz=repmat(dz, [UX.shape[0],UX.shape[1],1])

    cm = np.zeros((dz.shape))
    cd = np.zeros((dz.shape))
    cm[Z > 2] = 1.6
    cm[Z <= 2] = 1.2

    # switch condition
    if condition == 1:
        cd = cd + 1.15
    elif condition == 2:
        cd[Z > 2] = 0.65
        cd[Z <= 2] = 1.05
    # end
    pinertia = rho * np.pi * D**2 / 4 * cm * AX  # inertia load per unit length
    pdrag = 0.5 * rho * cd * D * UX * np.abs(UX)  # drag load per unit length

    # figure(4)
    # hold all
    # plot(pinertia(:),Z(:),'.-','LineWidth',1)

    # figure(5)
    # hold all
    # plot(pdrag(:),Z(:),'.-','LineWidth',1)

    dinertia = pinertia * dz
    binertia = dinertia.sum(dim='z')
    #binertia = np.sum(dinertia, axis=2)
    # binertia=summ(dinertia)
    ddrag = pdrag * dz
    #bdrag = np.sum(ddrag, axis=2)
    bdrag = ddrag.sum(dim='z')
    bs = binertia + bdrag
    #
    oinertia = dinertia * (Z + d)
    oinertia = oinertia.sum(dim='z')
    #oinertia = np.sum((dinertia * (Z + d)), axis=2)
    #
    odrag = ddrag * (Z + d)
    odrag = odrag.sum(dim='z')
    #odrag = np.sum((ddrag * (Z + d)), axis=2)
    ovtm = oinertia + odrag
    #
    return (bs.to_dataframe(name='BS').reset_index(),
            ovtm.to_dataframe(name='OTM').reset_index())
#
#
def permute(A, order):
    """ """
    return np.tile(np.expand_dims ( A, axis=(0, 1) ), order)
#
def repmat2(A, n, axis:int=1):
    """
    """
    A1 = repmat(A, 1, 1)
    A1 = np.transpose(A1)
    A1 = np.expand_dims(A1, 0)
    A1 = np.transpose(A1)
    A1 = np.tile(A1, n)
    return A1
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
def permute1(A, order, axis:int=1):
    """ """
    return np.transpose(repmat(A, order, axis))