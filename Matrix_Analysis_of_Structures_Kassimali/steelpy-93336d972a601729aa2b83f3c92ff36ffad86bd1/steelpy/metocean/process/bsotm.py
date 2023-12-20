#
# Copyright (c) 2009-2023 fem2ufo
#
from __future__ import annotations
# Python stdlib imports
#import math
#from typing import NamedTuple
from dataclasses import dataclass
#
# package imports
#
import numpy as np
#from numpy.matlib import repmat
#import matplotlib.pyplot as plt
#
#from steelpy.process.dataframe.main import DBframework
from steelpy.metocean.process.beamhydro import BeamMorisonWave
#


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
    __slots__ = ['kinematics', 'current', 'hydro',
                 'condition', 'rho_w']
    def __init__(self, kinematics:list,
                 current:tuple, hydro:tuple,
                 rho_w:float,
                 condition:int=1): # 
        """
        """
        self.kinematics = kinematics
        self.current = current
        self.hydro = hydro
        self.condition = condition
        self.rho_w = rho_w
    #
    #
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
    def BS(self, dinertia, ddrag):
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
        z = self.kinematics.z
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
        bs = self.BS(dmass, ddrag)
        #
        otm = self.OTM(dmass, ddrag, Z, d)
        #
        return bs, otm
    #
    # -----------------------------------------------
    #
    #
    def Fwave(self, beam, nelev=5):
        """Calculation of wave forces on beam elements"""
        #beams = mesh.elements().beams()
        current = self.current.current
        #
        # process
        Bwave = BeamMorisonWave(beam=beam, rho=self.rho_w)
        #
        #beam = beams[12]
        #uvector = np.array(beam.unit_vector)
        #print(f'Unit Vector [{uvector[0]}, {uvector[1]}, {uvector[2]}]')
        #print('')
        #Duv = uvector / 100
        #Lm = beam.L
        #
        #n1, n2 = beam.nodes
        #x = n1.x - Duv[0] * 0.5
        #y = n1.y - Duv[1] * 0.5
        #z = n1.z - Duv[2] * 0.5
        #
        # -----------------------------------------
        #
        d = self.kinematics.d
        z = self.kinematics.z
        eta = self.kinematics.surface.eta
        #
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
        #
        crestmax = np.max(eta)
        #
        # -----------------------------------------
        #
        #section = beam.section
        #D = section.diameter
        #
        #zmax = np.maximum(n1.y, n2.y)
        #zmin = np.minimum(n1.y, n2.y)
        #Elev = np.linspace(zmin, zmax, nelev)
        #Elev = beamhydro.elevations(nelev=nelev)
        #
        #b_range = np.abs(n2.y - n1.y)
        #
        #bzidx = (z > zmin) & (z < zmax)
        #
        #
        # -----------------------------------------
        #
        #dz = np.diff(Elev)
        # locating the middle point of each element
        #Z = Elev[:-1] + dz
        Z = Bwave.Z(nelev=nelev)
        #
        # -----------------------------------------
        # Hydro diametre & area
        marine_growth = self.hydro.marine_growth
        mg = marine_growth.MG(Z)
        #Dh, At = self.Dh(D, Z)
        #Dh, At = beamhydro.Dh(mg=mg)
        #
        # -----------------------------------------
        # Cd & Cm
        cdcm = self.hydro.CdCm
        cd, cm = cdcm.getCdCm(Z, crestmax, condition=self.condition)
        #
        # -----------------------------------------
        # Current
        current.seastate(d=d, z=z, eta=eta)
        Vc = current.Vc(d, eta, Z)
        #Vc = 0
        #
        # -----------------------------------------
        # Kinematis
        #
        kin = self.kinematics.get_kin(Z)
        #
        #print('')
        #print('--> local Member')
        #print(f"Max horizontal wave particle velocity {max(np.max(kin['ux']), np.min(kin['ux']), key=abs): 1.4e} m/s")
        #print(f"Max vertical wave particle velocity {max(np.max(kin['uz']), np.min(kin['uz']), key=abs): 1.4e} m/s")
        #print('')
        #print(f"Max horizontal wave particle acceleration {max(np.max(kin['ax']), np.min(kin['ax']), key=abs): 1.4e} m/s2")
        #print(f"Max vertical wave particle acceleration {max(np.max(kin['az']), np.min(kin['az']), key=abs): 1.4e} m/s2")
        #print('--> local Member')
        #print('')
        #
        #
        # ---------------------------------------
        #
        #kinvel, kinacc  = self.local_kin(beam, kin, Vc)
        #kinvel, kinacc = beamhydro.local_kin(kin, Vc)
        #
        # ---------------------------------------
        #
        #At = permute2(At, (vn.shape[0], vn.shape[2]), 1)
        #
        #cm = permute2(cm, (vn.shape[0], vn.shape[2]), 1)
        #
        #cd = permute2(cd, (vn.shape[0], vn.shape[2]), 1)
        #
        #Dh = permute2(Dh, (vn.shape[0], vn.shape[2]), 1)
        #
        #dz = permute2(dz, (vn.shape[0], vn.shape[2]), 1)
        #
        #Z = permute2(Z, (vn.shape[0], vn.shape[2]), 1)
        #
        # ---------------------------------------
        #
        # Components of the force per unit of cylinder length acting in
        # the x, y and z dir are given by the generalized Morison equation
        #        
        #udl = beamhydro.local_uforce(Dh, At, cd, cm,
        #                             kinvel, kinacc,
        #                             Elev, dz)
        #
        #udl = Bwave.Fwave(Vc=Vc, MG=mg, Cd=cd, Cm=cm,
        #                  kinematics=kin, nelev=nelev)
        #lineload = udl.line()
        #
        # ---------------------------------------
        # Calculate wave loading on exposed span
        #
        #Fx, Fy = self.span_loading(udl.qx, udl.qy, dz)
        #Fx, Fy, OTM = udl.span_loading()
        #indmax = Fx.argmax(dim='length').values
        #vmax = Fx.idxmax(dim='length').values
        #
        #print('')
        #print('Total combined force [kN-m]')
        #print(f'Fx ={np.max(Fx) / 1000: 1.3e}, Fy={np.max(Fy) / 1000: 1.3e}, OTM={np.max(OTM)/1000: 1.3e}')
        #print('---')
        #
        #Fx.sel(x=0).plot.line(hue='z')
        #plt.show()
        #
        #return lineload
        #return udl
        return Bwave.Fwave(Vc=Vc, MG=mg, Cd=cd, Cm=cm,
                           kinematics=kin, nelev=nelev)        
    #
    #
#
#
def permute(A, order):
    """ """
    return np.tile(np.expand_dims(A, axis=(0, 1)), order)
#
#def repmat2(A, n, axis:int=1):
#    """
#    """
#    A1 = repmat(A, 1, 1)
#    A1 = np.transpose(A1)
#    A1 = np.expand_dims(A1, 0)
#    A1 = np.transpose(A1)
#    A1 = np.tile(A1, n)
#    return A1
#
#def permute2(A, order, axis:int=1):
#    """ """
#    A1 = repmat(A, order[1], axis)
#    A1 = np.transpose(A1)
#    A1 = np.expand_dims(A1, axis=0)
#    A1 = np.transpose(A1)
#    A1 = np.tile(A1, order[0])
#    return np.transpose(A1)   
#
#def permute1(A, order, axis:int=1):
#    """ """
#    return np.transpose(repmat(A, order, axis))
#
#