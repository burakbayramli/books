# Copyright (c) 2009-2022 steelpy


# Python stdlib imports
from array import array
from dataclasses import dataclass
import math
from typing import Dict, List, Tuple, NamedTuple

# package imports
from steelpy.utils.units.main import Units
#from steelpy.utils.math.vector import Vector
from steelpy.utils.dataframe.main import DBframework #, SeriesItem

#
# ************************************************
#                     PROGRAM RINGS
#              STRESSES IN CIRCULAR RINGS
#                R.J.ROARK & W.C.YOUNG
# ************************************************
#
#
#-------------------------------------------------
#
def stress2(smac, area, ceni, ceno, fki, fko, F, rforces):
            #xloc, xm, xh, xs):
    '''
    Calculate stress for curved beams
    F : shear factor
    '''
    # Circunferencial normal stress due tu pure bending M:
    bstri = rforces['M'] * ( fki * ceni / smac) # Stresses Bending Inner
    bstro = rforces['M'] * (-fko * ceno / smac) # Stresses Bending Outer
    #
    # Circunferencial normal stress due to hoop tension N:
    hstr =  rforces['N'] / area # Hoop Stresses
    #
    # Shear stress due to the radial shear force V:
    # Tau_r_theta = V(R-e)/(trAer^2) (RAr - Qr)
    sstr =  rforces['V'] * F / area  # Shear Stresses
    #
    return RadialStress({'x':rforces['x'], 
                         'bs_ri':bstri, 'bs_ro':bstro, 
                         'axial':hstr, 'tau':sstr})
#
def polar(xx, yy):
    """returns r, theta(degrees)
    """
    r = [(x**2 + y**2)**0.5 for x,y in zip(xx, yy)]
    theta = [math.degrees(math.atan2(y,x)) for x,y in zip(xx, yy)]
    return r, theta
#
def rect(rad, Theta):
    """theta in degrees

    returns tuple; (float, float); (x,y)
    """
    x = [r * math.cos(math.radians(theta)) for r, theta in zip(rad, Theta)]
    y = [r * math.sin(math.radians(theta)) for r, theta in zip(rad, Theta)]
    return x,y
#
class RadialForces():

    def __init__(self, data:Dict):
        """
        data {x, M, N, V}
        """
        #x:List[float]
        #M:array
        #N:array
        #V:array
        #super().__init__(data)
        self._data = data

    #
    @property
    def Mmax(self) -> float:
        """ """
        #mmax = min(self._data['M'])
        #if max(self._data['M']) > abs(mmax):
        #    mmax = max(self._data['M'])
        #return mmax  # --> N
        return self._data['M'].maxabs()
    #
    @property
    def Nmax(self) -> float:
        """ """
        #mmax = min(self._data['N'])
        #if max(self._data['N']) > abs(mmax):
        #    mmax = max(self._data['N'])
        #return mmax  # --> N
        return self._data['N'].maxabs()
    #
    @property
    def Vmax(self) -> float:
        """ """
        #mmax = min(self._data['V'])
        #if max(self._data['V']) > abs(mmax):
        #    mmax = max(self._data['V'])
        #return mmax  # --> N
        return self._data['V'].maxabs()
    #
    def printout(self):
        """ """
        #
        #print(self.__str__())
        #out = ""
        print('LOCATION    MOMENT      THRUST      SHEAR')
        print('             N.m          N           N')
        for i, x in  enumerate(self._data['x']):
            print("{:>5.1f}    {: 1.4e} {: 1.4e} {: 1.4e}"
                  .format(x, self._data['M'][i], self._data['N'][i], self._data['V'][i]))
    #
    def plot_radial(self):
        """ Polar Plot"""
        import matplotlib.pyplot as plt
        import numpy as np
        #r = []
        steps = len(self._data['x'])
        # Normalise
        mm = self._data['M']
        nn = self._data['N']
        vv = self._data['V']
        M = mm / self.Mmax
        M = [1. + item for item in M]
        N = nn / self.Nmax
        N = [1. + item for item in N]
        V = vv / self.Vmax
        V = [2 + item for item in V]
        #
        plt, ax = plot_view(2, steps)
        #
        rtheta = np.arange(0, 1, 1/steps)
        r = [1 for x in range(steps)]
        theta = 2 * np.pi * rtheta
        plt.plot(theta, M, color = 'blue', linewidth=1)
        #
        title:str="Forces"
        #close_view(ax, title)
        ax.set_rmax(3)
        ax.set_rticks([1, 1.5, 2, 3])  # Less radial ticks
        ax.grid(True, linestyle='--')
        plt.show()
        print('--')
    #
    def plot_force(self, column:str):
        """ Polar Plot"""
        #import numpy as np
        import matplotlib.pyplot as plt
        #
        data = self._data[column]
        dataMax = max(data)
        dataMin = min(data)
        Mmax = dataMax
        if Mmax < abs(dataMin):
            Mmax = dataMin
        mm = [item for item in data]
        mm.append(mm[0])
        #M = [1+(item / Mmax ) for item in mm]
        M = [item for item in mm]
        #
        x = [item for item in self._data['x']]
        x.append(x[0])        
        theta = [math.radians(item) for item in x]
        rad = 0
        #if dataMax == 0:
        #    fix = max([item for item in M if not math.isclose(abs(item), 0, abs_tol=0.05)])
        #    M = [abs(dataMin) + item for item in mm]
        rad = [rad for item in x]
        plt.polar(theta, rad, color='black', lw=2)
        plt.polar(theta, M, lw=2)
        #
        z1 = [item < M[i] for i, item in enumerate(rad)]
        plt.fill_between(theta, rad, M, where=(z1),
                         alpha=0.25, color='red', interpolate=True,
                         label=f'max {dataMax: 1.3e}')
        #
        z2 = [item >= M[i] for i, item in enumerate(rad)]
        plt.fill_between(theta, rad, M, where=(z2),
                         alpha=0.25, color='green', interpolate=True,
                         label=f'min {dataMin: 1.3e}')        
        #
        #get current axes
        ax = plt.gca()
        #hide x-axis
        #ax.get_xaxis().set_visible(False)
        #hide y-axis
        #ax.get_yaxis().set_visible(False)
        #
        plt.grid(True, linestyle='--')
        #plt.axis('off')
        #
        plt.legend()
        plt.show()
        #print('---')
#
#
class RadialStress():
    
    def __init__(self, data:Dict):
        """
        sigma_x : Circumferential stress on cross section of curved bars 
                  or normal stress or fiber stres.
        sigma_z : Radial stress.
        sigma_h : Hoop stress
        tau : Shear stress.
        """
        #x:List[float]
        #bs_ri:array
        #bs_ro:array
        #axial:array
        #tau: array
        #super().__init__(data) 
        #self.ki = ki
        #self.ko = ko
    #
    #@property
    #def sigma_i(self):
    #    """sigma_i: The normal (circumferential) stresses at the inside fiber."""
    #    return (self.sigma_z  + self.sigma_h) * self.ki 
    ##
    #@property
    #def sigma_o(self):
    #    """sigma_o: The normal (circumferential) stresses at the outer fiber."""
    #    return (self.sigma_z + self.sigma_h) * self.ko
    #
    @property
    def tau_max(self):
        """Maximum shear stress """
        shmax = min(self._data['tau'])
        if max(self._data['tau']) > abs(shmax):
            shmax = max(self._data['tau'])
        return shmax / 1000**2 # N/m2 --> N/mm2
    #
    @property
    def sigma_axial(self):
        """Maximum shear stress """
        shmax = min(self._data['axial'])
        if max(self._data['axial']) > abs(shmax):
            shmax = max(self._data['axial'])
        return shmax / 1000**2 # N/m2 --> N/mm2
    #
    @property
    def sigma_bending(self):
        """ Maximum Section Combined Stress"""
        cstri = self._data['bs_ri']
        cstro = self._data['bs_ro']
        #
        #xloc = []
        istep = len(self._data['x'])
        #step = 360.0 / istep
        cstmax = 0
        for i in range(istep):
            #xloc.append(i * step)
            x = abs(cstri[i])
            y = abs(cstro[i])
            #
            temp = cstro[i]
            if x > y:
                temp = cstri[i]
            #
            if abs(cstmax) <= abs(temp):
                cstmax = temp
        #
        return cstmax / 1000**2 # N/m2 --> N/mm2
    #
    #@property
    #def sigma_hmax(self):
    #    """ Maximum Section Hoop Stress"""
    #    shmax = min(self.tau)
    #    if max(self.tau) > abs(shmax):
    #        shmax = max(self.tau)
    #    return shmax
    ##
    @property
    def sigma_comb(self):
        """ Maximum Section Combined Stress"""
        cstri = self._data['bs_ri'] + self._data['axial']  # * fki  # Stresses Bending Inner + Hoop
        cstro = self._data['bs_ro'] + self._data['axial']  # * -fko # Stresses Bending Outer + Hoop
        scomb1 = max(max(cstro), max(cstri))
        scomb2 = min(min(cstro), min(cstri))
        #
        shmax = scomb1
        if shmax < abs(scomb2):
            shmax = scomb2
        return shmax / 1000**2 # N/m2 --> N/mm2
        
    ##
    #def sigma_bending(self):
    #    """ Maximum Section Axial Stress"""
    #    pass    
    ##
    def printout(self):
        """ """
        print("Maximum Stress")
        print(f"f axial    : {self.sigma_axial: 1.2f} MPa")
        print(f"f Bending  : {self.sigma_bending: 1.2f} MPa")
        print(f"Comb Fa+Fb : {self.sigma_comb: 1.2f} MPa")
        print(f"f Shear    : {self.tau_max: 1.2f} MPa")
        #print ('Combined stress    max. shear stress')
        #print ('      N/mm^2                N/mm^2')
        #print ("   {: 1.3f}            {: 1.3f}"
        #       .format(self.sigma_tmax, self.sigma_hmax))
        print("")
#
#
def plot_view(rad:float, steps:int = 360):
    """ """
    import matplotlib.pyplot as plt
    import numpy as np
    #
    rtheta = np.arange(0, 1, 1/steps)
    r = [rad for x in range(steps)]
    theta = 2 * np.pi * rtheta
    #steps = 1/len(r)
    #theta = [math.tau*item for item in range(0, 1, )]
    fig, ax = plt.subplots(subplot_kw={'projection': 'polar'})
    ax.plot(theta, r, color = 'black', linewidth=3)
    #
    #
    #theta4_angle = (self.theta - math.pi*1.51)
    #r4 = [ 1, 1.5]
    #theta4 = [theta3_angle, theta4_angle]
    #ax.plot(theta4, r4)
    #
    #ax.set_rmax(2)
    #ax.set_rticks([0.5, 1, 1.5, 2])  # Less radial ticks
    ##ax.set_rlabel_position(-22.5)  # Move radial labels away from plotted line
    #ax.grid(True)
    #ax.set_title(title, va='bottom')
    ##plt.show()
    return plt, ax
#
def plot_theta(plt, theta, phase):
    """ """
    theta2_angle = (math.pi*0.50 - theta) -phase
    r2 = [0, 0.50, 1]
    theta2 = [0, theta2_angle, theta2_angle]
    #theta2 = [0, self.theta  ]
    plt.plot(theta2, r2, color = 'blue', linewidth=1)
    #
    theta3_angle = (theta - math.pi*1.5) -phase
    r3 = [0, 0.5, 1]
    theta3 = [0, theta3_angle, theta3_angle]
    plt.plot(theta3, r3, color = 'blue', linewidth=1)
    
#
def close_view(ax, title):
    """ """
    ax.set_rmax(2)
    ax.set_rticks([0.5, 1, 1.5, 2])  # Less radial ticks
    #ax.set_rlabel_position(-22.5)  # Move radial labels away from plotted line
    ax.grid(True, linestyle='--')
    #title:str="A line plot on a polar axis"
    ax.set_title(title, va='bottom')
    ax.axes.xaxis.set_visible(False)
    ax.axes.yaxis.set_visible(False)
    ax.set_frame_on(False)    
#
#-------------------------------------------------
#                 Roark Cases
#-------------------------------------------------
#
@dataclass
class RingBase:
    apld:float
    phase:float
    theta:float
    phi:float = 0 # rad
    rho:float = 1000 # kg/m^3
    istep:int = 360
    Kt:str = 0
    alpha:str = 0
    beta:str = 0
    title:str = "Roark's Ring"
    #
    def solution(self, amom:float, ahop:float, ashr:float, 
                 tm:float, tt:float, tv, 
                 istep:int, ist:int, crad:float, 
                 sx:float, cx:float, j:int, 
                 xmom:List[float], xhop:List[float], xshr:List[float]):
        '''
        General formulas for moment, hoop load and radial shear:
            M = MA - NAR(1-u) + VARz + LTM
            N = NAu + VAz + LTM
            V = -NAz + VAu + LTv
            
        Where LTM, LTN and LTv are loads terms given below for
        several types of loads.
        
        This funtion calculates the forces around the ring.
        only n/2 + 1 points are calculated around the ring
        for symmetric load cases 
        '''
        if abs(1000000.0 / math.pi - tv) < 0.0001:
            xmom[j] = amom
            xhop[j] = ahop
            xshr[j] = ashr
        else:
            xmom[j] = amom - ahop * crad * (1 - cx) + ashr * crad * sx + tm
            xhop[j] = ahop * cx + ashr * sx + tt
            xshr[j] = -ahop * sx + ashr * cx + tv
            
            if istep != ist:
                if j != 0 and j != ist - 1:
                    k = istep - j 
                    xmom[k] = xmom[j]
                    xhop[k] = xhop[j]
                    xshr[k] = -xshr[j]
        #return xmom, xhop, xshr
    #    
    def rotadd(self, phase:float, 
               xmom:List[float], xhop:List[float], xshr:List[float]):
        """
        phase : the rotation angle of the axis of symmetry of the loading pattern.
        """
        #phase = self.phase
        istep = len(xmom)
        xloc = [0 for i in range(istep)]
        xm   = [0 for i in range(istep)]
        xh   = [0 for i in range(istep)]
        xs   = [0 for i in range(istep)]
        #
        k = int(math.degrees(phase)) * istep // 360
        #k = int(phase) * istep // 360
        #print('====> k :',k, phase, istep)
        step = 360 / istep
        for i in range(istep):
            l = k - i 
            j = istep - l
            if l <= 0:
                j = abs(l) 
            #
            xloc[i] = i * step
            xm[i] += xmom[j]
            xh[i] += xhop[j]
            xs[i] += xshr[j]
        return RadialForces({'x':xloc, 'M':xm, 'N':xh, 'V':xs})
    #    
#
#
# Diametrical opposed forces
@dataclass
class ring_1(RingBase):
    '''
    Circular Ring Case 1 : 
    Diametrical Opposed Forces
    
    Parameters
    ----------
    W = Load (force)
    
    Returns
    ----------
    M
    N
    V
    
    Notes
    ----------
    Formulas for stress and strain, 7th edition
    R.J.Roark & W.C.Young 
    
    Examples
    ----------
    '''
    title:str= "Diametrical Opposed Forces"
    #
    def get_load(self, crad, k1, k2):
        """ """
        apld = self.apld
        istep = self.istep
        phase = self.phase
        xmom = [0 for i in range(istep)]
        xhop = [0 for i in range(istep)]
        xshr = [0 for i in range(istep)]
        #dDh = [0 for i in range(istep)]
        #dDv = [0 for i in range(istep)]
        #
        print (' ')
        print ('roark case 1 Diametrical Opposed Forces')
        print ('')
        print(f'R: {crad: 1.3e} m')
        print(f'Applied load W: {apld: 1.3e}')
        print(f'Phase: {math.degrees(phase): 1.3f} deg')
        print ('')
        #
        pi = math.pi
        tau = math.tau # 2*pi
        amom = k2 * apld * crad / pi
        ahop = 0
        ashr = 0
        ist = (istep // 2) + 1
        sin = math.sin
        cos = math.cos
        for j in range(ist):
            #i = j
            d = j / istep
            x = tau * d 
            sx = sin(x)
            cx = cos(x)
            Ltm = -apld * crad * sx / 2.0
            Ltn = -apld * sx / 2.0
            Ltv = -apld * cx / 2.0
            #
            self.solution(amom, ahop, ashr, Ltm, Ltn, Ltv, 
                          istep, ist, crad, sx, cx, 
                          j, xmom, xhop, xshr)
        #
        return self.rotadd(phase, xmom, xhop, xshr)
    #
    def view(self):
        """ """
        plt = plot_view()
        #
        #theta4_angle = (self.theta - math.pi*1.51)
        #r4 = [ 1, 1.5]
        #theta3_angle = (self.theta - math.pi*1.5)
        #theta4 = [theta3_angle, theta4_angle]
        #ax.plot(theta4, r4)
        #
        plt.show()     
#
# Off centre opposed force
@dataclass
class ring_2(RingBase):
    '''
    Circular Ring Case 2 
    Off Centre Opposed Forces
    
    Parameters
    ----------
    W = Load (force)
    
    Returns
    ----------
    M
    N
    V
    
    Notes
    ----------
    Formulas for stress and strain, 7th edition
    R.J.Roark & W.C.Young 
    
    Examples
    ----------
    '''
    title:str = "Off Centre Opposed Forces"
    #
    def get_load(self, crad, k1, k2):
        """
        k1, k2 : Hoop correction factor
        """
        apld = self.apld
        istep = self.istep
        xmom = [0 for i in range(istep)]
        xhop = [0 for i in range(istep)]
        xshr = [0 for i in range(istep)]
        print (' ')
        print ('roark case 2 Off Centre Opposed Forces')
        print ('')
        print(f'R: {crad: 1.3e} m')
        print(f'Applied load W: {apld: 1.3e}')
        print(f'Phase: {math.degrees(self.phase): 1.3f} deg')
        print(f'Theta: {math.degrees(self.theta): 1.3f} deg')
        print ('')
        pi = math.pi
        tau = math.tau
        a = self.theta
        sth = math.sin(a)
        cth = math.cos(a)
        amom = (-apld * crad / pi 
                * ((pi - a) * (1 - cth) -  sth * ( k2 - cth)))
        ahop = -apld / pi * (pi - a + sth * cth)
        ashr = 0
        ist = (istep // 2) + 1
        sin = math.sin
        cos = math.cos
        for j in range(ist):
            i = j
            d = i / istep
            x = tau * d 
            sx = sin(x)
            cx = cos(x)
            
            if a > x:
                Ltm = 0
                Ltn = 0
                Ltv = 0
            else:
                Ltm = -apld * crad * (cth - cx)
                Ltn =  apld * cx
                Ltv = -apld * sx
            #
            self.solution(amom, ahop, ashr, Ltm, Ltn, Ltv, 
                          istep, ist, crad, sx, cx, 
                          j, xmom, xhop, xshr)
        #
        return self.rotadd(self.phase, xmom, xhop, xshr)
    #
    def get_displacement(self, crad, k1, k2, EI):
        """ """
        apld = self.apld
        pi = math.pi
        a = self.theta
        sth = math.sin(a)
        cth = math.cos(a)        
        deltaLH = (apld * crad**3/ EI * pi 
                   * ((pi - a)*2*a*cth**2 
                      - k1*(pi*sth*cth + sth**2 * cth**2 - 2*a*sth*cth
                            - pi*a + a**2)
                      - 2*k2*sth*cth*(pi-2*a) - 2*k2**2*sth**2))
    #
    def view(self):
        """ """
        plt, ax = plot_view(1)
        plot_theta(ax, self.theta, self.phase)
        #
        r2 = 1.5
        theta_angle = (self.theta - math.pi*1.5)
        a = math.sin(theta_angle)
        phi = 0.50*math.pi - a/r2
        theta1_angle = (phi - math.pi*1.5) - self.phase
        #
        plt.annotate(text='', xy=(theta_angle - self.phase, 1), 
                     xytext=(theta1_angle, r2), 
                     arrowprops=dict(arrowstyle='->', lw=2.5, color="red"))
        #
        #
        theta2_angle = (math.pi*0.50 - self.theta) 
        a = math.sin(theta2_angle)
        phi = 0.50*math.pi - a/r2
        theta1_angle = (math.pi*0.50 - phi) - self.phase
        #
        plt.annotate(text='', xy=(theta2_angle - self.phase, 1), 
                     xytext=(theta1_angle, r2), 
                     arrowprops=dict(arrowstyle='->', lw=2.5, color="red"))        
        #
        # ----------------------------------------------------------
        #
        title:str="Ring 2"
        #
        close_view(ax, title)
        plt.show()
        # -------------------------------------------------------
        print('-->')
#
#
# Opposite moments
@dataclass
class ring_3(RingBase):
    '''
    Circular Ring Case 3 
    Opposing Moments
    
    Parameters
    ----------
    W = Load (force)
    
    Returns
    ----------
    M
    N
    V
    
    Notes
    ----------
    Formulas for stress and strain, 7th edition
    R.J.Roark & W.C.Young 
    
    Examples
    ---------- 
    '''
    title:str = "Opposing Moments"
    #
    def get_load(self, crad, k1, k2):
        """ """
        apld = self.apld
        istep = self.istep    
        xmom = [0 for i in range(istep)]
        xhop = [0 for i in range(istep)]
        xshr = [0 for i in range(istep)]
        
        print (' ')
        print ('roark case 3 Opposing Moments')
        print ("")
        print(f'Applied load W: {apld: 1.3e}')
        print(f'Phase: {math.degrees(self.phase): 1.3f} deg')
        print(f'Theta: {math.degrees(self.theta): 1.3f} deg')
        print ("")
        pi = math.pi
        tau = math.tau # 2*pi
        a = self.theta #* pi / 180.0
        sth = math.sin(a)
        cth = math.cos(a)
        #b = (2 * sth / (math.pi * fk1))
        #amom = -apld * (1 - (a / math.pi) - b)
        amom = (-apld / pi * (pi - a - (2 * sth * k2 / k1)))
        #ahop = (apld / crad) * b
        ahop = (apld / (crad * pi) * (2 * sth * k2 / k1))
        ashr = 0
        ist = (istep // 2) + 1
        sin = math.sin
        cos = math.cos
        for j in range(ist):
            i = j
            d = i / istep
            x = tau * d
            sx = sin(x)
            cx = cos(x)
            
            if a > x:
                Ltm = 0
                Ltn = 0
                Ltv = 0
            else:
                Ltm = apld
                Ltn = 0
                Ltv = 0
            #
            self.solution(amom, ahop, ashr, Ltm, Ltn, Ltv, istep, ist, 
                          crad, sx, cx, j, xmom, xhop, xshr)        
        #
        return self.rotadd(self.phase, xmom, xhop, xshr)
    #
    def view(self):
        """ """
        plt = plot_view(self)
        #
        #theta4_angle = (self.theta - math.pi*1.51)
        #r4 = [ 1, 1.5]
        #theta3_angle = (self.theta - math.pi*1.5)
        #theta4 = [theta3_angle, theta4_angle]
        #ax.plot(theta4, r4)
        #
        plt.show()     
#
# Two parallel forces with reaction
class ring_4(RingBase):
    '''
    Circular Ring Case 4 
    Two Parallel Forces with Reaction
    
    Parameters
    ----------
    W = Load (force)
    
    Returns
    ----------
    M
    N
    V
    
    Notes
    ----------
    Formulas for stress and strain, 7th edition
    R.J.Roark & W.C.Young 
    
    Examples
    ---------- 
    '''
    title:str = "Two Parallel Forces with Reaction"
    #
    def get_load(self, crad, k1, k2):
        """ """
        apld = self.apld
        istep = self.istep    
        #global xmom, xhop, xshr
        xmom = [0 for i in range(istep)]
        xhop = [0 for i in range(istep)]
        xshr = [0 for i in range(istep)]
        
        print (' ')
        print ('roark case 4 Two Parallel Forces with Reaction')
        print ("")
        print(f'Applied load W: {apld: 1.3e}')
        print(f'Phase: {math.degrees(self.phase): 1.3f} deg')
        print(f'Theta: {math.degrees(self.theta): 1.3f} deg')
        print ("")
        pi = math.pi
        tau = math.tau # 2*pi    
        a = self.theta #* math.pi / 180.0
        sth = math.sin(a)
        cth = math.cos(a)
        #b = a / math.pi
        #c = sth * sth * fk4
        #p = 1.0 / math.pi
        #amom = -apld * crad * (p * (1 + cth + c) - sth * (1 - b))
        amom = (-apld * crad / pi 
                * (sth * (sth - pi + a) + k2 * (1 + cth)))
        #ahop = -apld * (c / math.pi)
        ahop = -apld * sth**2 / pi
        ashr = 0
        ist = (istep // 2) + 1
        sin = math.sin
        cos = math.cos   
        for j in range(ist):
            i = j 
            d = i / istep
            x = tau * d
            sx = sin(x)
            cx = cos(x)
            ax = x - 0.009
            if a > ax:
                Ltm = 0
                Ltn = 0
                Ltv = 0
            else:
                Ltm = apld * crad * (sx - sth)
                Ltn = apld * sx
                Ltv = apld * cx
            #
            self.solution(amom, ahop, ashr, Ltm, Ltn, Ltv, istep, ist, 
                          crad, sx, cx, j, xmom, xhop, xshr)        
        #
        return self.rotadd(self.phase, xmom, xhop, xshr)
    #
    def view(self):
        """ """
        plt = plot_view(self)
        #
        #theta4_angle = (self.theta - math.pi*1.51)
        #r4 = [ 1, 1.5]
        #theta3_angle = (self.theta - math.pi*1.5)
        #theta4 = [theta3_angle, theta4_angle]
        #ax.plot(theta4, r4)
        #
        plt.show()     
#
# Two radial forces with reaction
@dataclass
class ring_5(RingBase):
    '''
    Circular Ring Case 5 
    Two Radial Forces with Reactions
    
    Parameters
    ----------
    W = Load (force)
    
    Returns
    ----------
    M
    N
    V
    
    Notes
    ----------
    Formulas for stress and strain, 7th edition
    R.J.Roark & W.C.Young 
    
    Examples
    ---------- 
    '''
    title:str = "Two Radial Forces with Reactions"
    #
    def get_load(self, crad, k1, k2):
        """ """
        apld = self.apld
        istep = self.istep    
        xmom = [0 for i in range(istep)]
        xhop = [0 for i in range(istep)]
        xshr = [0 for i in range(istep)]
        
        print (" ")
        print ('roark case 5 Two Radial Forces with Reactions')
        print (" ")
        print(f'Applied load W: {apld: 1.3e}')
        print(f'Phase: {math.degrees(self.phase): 1.3f} deg')
        print(f'Theta: {math.degrees(self.theta): 1.3f} deg')
        print (" ")
        pi = math.pi
        tau = math.tau # 2*pi     
        a = self.theta #* math.pi / 180.0
        sth = math.sin(a)
        cth = math.cos(a)
        #b = a / math.pi
        #p = 1.0 / math.pi
        #amom = -apld * crad * (sth * (1 - b) - p * (1 + cth))
        amom = (-apld * crad / pi 
                * (sth * (pi - a) - k2 * (1 + cth)))
        #ahop = -apld * (sth * (1 - b))
        ahop = (-apld / pi * sth * (pi - a))
        ashr = 0
        ist = (istep // 2) + 1
        sin = math.sin
        cos = math.cos 
        for j in range(ist):
            i = j
            d = i / istep
            x = tau * d 
            sx = sin(x)
            cx = cos(x)
            #ax = x - 0.009
            if a > x:
                Ltm = 0
                Ltn = 0
                Ltv = 0
            else:
                Ltm = -apld * crad * sin(x - a)
                Ltn = -apld * sin(x - a)
                Ltv = -apld * cos(x - a)
            #
            self.solution(amom, ahop, ashr, Ltm, Ltn, Ltv, istep, ist, 
                          crad, sx, cx, j, xmom, xhop, xshr)        
        #
        return self.rotadd(self.phase, xmom, xhop, xshr)
    #
    def view(self):
        """ """
        plt = plot_view(self)
        #
        theta4_angle = (self.theta - math.pi*1.51)
        r4 = [ 1, 1.5]
        theta3_angle = (self.theta - math.pi*1.5)
        theta4 = [theta3_angle, theta3_angle]
        plt.plot(theta4, r4)
        #
        plt.show()     
#
# Two tangential forces with their reactions
@dataclass
class ring_6(RingBase):
    '''
    Circular Ring Case 6 
    Two Tangential Forces with their Reactions
    
    Parameters
    ----------
    W = Load (force)
    
    Returns
    ----------
    M
    N
    V
    
    Notes
    ----------
    Formulas for stress and strain, 7th edition
    R.J.Roark & W.C.Young 
    
    Examples
    '''
    title:str = "Two Tangential Forces with their Reactions"
    #
    def get_load(self, crad, k1, k2):
        """ """
        apld = self.apld
        istep = self.istep    
        xmom = [0 for i in range(istep)]
        xhop = [0 for i in range(istep)]
        xshr = [0 for i in range(istep)]
        
        print (' ')
        print ('roark case 6 Two Tangential Forces with their Reactions')
        print (" ")
        print(f'Applied load W: {apld: 1.3e}')
        print(f'Phase: {math.degrees(self.phase): 1.3f} deg')
        print(f'Theta: {math.degrees(self.theta): 1.3f} deg')
        print (" ")
        pi = math.pi
        tau = math.tau # 2*pi      
        a = self.theta #* math.pi / 180.0
        sth = math.sin(a)
        cth = math.cos(a)
        b =   a / pi
        c = sth / pi
        #amom = -apld * crad * (c * (1 + fk4) - (1 - b) * (1 - cth))
        amom = (-apld * crad / pi 
                * (sth * (1 + k2) - (pi - a) * (1 - cth)))
        #ahop = -apld * (c * fk4 + (1 - b) * cth)
        ahop = -apld / pi * (sth + (pi - a) * cth)
        ashr = 0
        ist = (istep // 2) + 1
        sin = math.sin
        cos = math.cos
        for j in range(ist):
            i = j
            d = float(i) / float(istep)
            x = tau * d
            sx = sin(x)
            cx = cos(x)
            #ax = x - 0.009
            if a > x :
                Ltm = 0
                Ltn = 0
                Ltv = 0
            else:
                Ltm = -apld * crad * (1 - cos(x - a))
                Ltn =  apld * cos(x - a)
                Ltv = -apld * sin(x - a)
            #
            self.solution(amom, ahop, ashr, Ltm, Ltn, Ltv, istep, ist, 
                          crad, sx, cx, j, xmom, xhop, xshr)        
        #
        return self.rotadd(self.phase, xmom, xhop, xshr)
    #
    def view(self):
        """ """
        plt = plot_view(self)
        #
        #theta4_angle = (self.theta - math.pi*1.51)
        #r4 = [ 1, 1.5]
        #theta3_angle = (self.theta - math.pi*1.5)
        #theta4 = [theta3_angle, theta4_angle]
        #ax.plot(theta4, r4)
        #
        plt.show()     
#
# Equal radial loads equally spaced
@dataclass
class ring_7(RingBase):
    """ """
    title:str = "Equal Radial Loads Equally Spaced"
    #raise NotImplemented('***ERROR*** ROARK CASE 7 IS NOT IMPLEMENTED')
    def get_load(self, crad, k1, k2):
        """ """
        apld = self.apld
        istep = self.istep
        #
        xmom = [0 for i in range(istep)]
        xhop = [0 for i in range(istep)]
        xshr = [0 for i in range(istep)]
        
        print (" ")
        print ('Roark case 7 Equal Radial Loads Equally Spaced')
        print ("")
        print(f'R: {crad: 1.3e} m')
        print(f'Applied load W: {apld: 1.3e}')
        print(f'Phase: {math.degrees(self.phase): 1.3f} deg')
        print(f'Theta: {math.degrees(self.theta): 1.3f} deg')
        print ("")
        #
        pi = math.pi
        tau = math.tau
        a = self.theta 
        sth = math.sin(a)
        cth = math.cos(a)
        #
        amom = (apld * crad *(1/sth - k2/a))/2
        ahop = 0
        ashr = 0
        ist = (istep // 2) + 1
        sin = math.sin
        cos = math.cos
        for j in range(ist):
            i = j
            d = i / istep
            x = tau * d 
            sx = sin(x)
            cx = cos(x)
            
            #if a > x:
            #    Ltm = 0
            #    Ltn = 0
            #    Ltv = 0
            #else:
            Ltm = (apld * crad *(cx/sth - k2/a))/2
            Ltn = apld * cx/ (2*sth)
            Ltv = -apld * sx/ (2*sth)
            #
            self.solution(amom, ahop, ashr, Ltm, Ltn, Ltv, istep, ist, 
                          crad, sx, cx, j, xmom, xhop, xshr)        
        #
        return self.rotadd(self.phase, xmom, xhop, xshr)        
#
# Uniform unit load (at the base theta > pi/2)
@dataclass
class ring_8(RingBase):
    '''
    Circular Ring Case 8 
    Uniform Unit Load Applied at Base theta > pi/2
    
    Parameters
    ----------
    W = Load (force)
    
    Returns
    ----------
    M
    N
    V
    
    Notes
    ----------
    Formulas for stress and strain, 7th edition
    R.J.Roark & W.C.Young 
    
    Examples
    '''
    title:str = "Uniform Unit Load Applied at Base theta > pi/2"
    #
    def get_load(self, crad, k1, k2):
        """ """
        apld = self.apld
        istep = self.istep
        if self.theta < math.pi * 0.50:
            raise IOError(f"Theta [{theta}] < pi/2")
        w = apld / (2*crad*math.sin(self.theta))
        xmom = [0 for i in range(istep)]
        xhop = [0 for i in range(istep)]
        xshr = [0 for i in range(istep)]
        
        print (" ")
        print ('Roark case 8 Uniform Unit Load Applied at Base theta > pi/2')
        print ("")
        print(f'R: {crad: 1.3e} m')
        print(f'Applied load W: {apld: 1.3e}')
        print(f'Applied load w: {w: 1.3e}')
        print(f'Phase: {math.degrees(self.phase): 1.3f} deg')
        print(f'Theta: {math.degrees(self.theta): 1.3f} deg')
        print ("")
        pi = math.pi
        tau = math.tau # 2*pi
        a = self.theta #* pi / 180.0
        sth = math.sin(a)
        cth = math.cos(a)
        #
        amom = (w * crad**2 / tau 
                * (pi * (sth**2 - 0.50) - (sth * cth - a) / 2. 
                   - sth**2 * (a + 2. * sth / 3.) 
                   - k2 * (2 * sth + sth * cth - pi + a)))
        #
        ahop = -w * crad * sth**3 / (3.0 * pi)
        ashr = 0
        ist = (istep // 2) + 1
        sin = math.sin
        cos = math.cos  
        for j in range(ist):
            i = j
            d = i / istep
            x = tau * d 
            sx = sin(x)
            cx = cos(x)
            
            if a > x:
                Ltm = 0
                Ltn = 0
                Ltv = 0
            else:
                Ltm = -w * crad**2 / 2.0 * (sx - sth)** 2
                Ltn = -w * crad * sx * (sx - sth)
                Ltv = -w * crad * cx * (sx - sth)
            # 
            self.solution(amom, ahop, ashr, Ltm, Ltn, Ltv, istep, ist, 
                          crad, sx, cx, j, xmom, xhop, xshr)        
        #xloc, xm, xh, xs = rotadd(istep, phase, xmom, xhop, xshr)
        #return xloc, xm, xh, xs
        return self.rotadd(self.phase, xmom, xhop, xshr)
    #
    def view(self):
        """ """
        plt = plot_view(self)
        #
        #theta4_angle = (self.theta - math.pi*1.51)
        #r4 = [ 1, 1.5]
        #theta3_angle = (self.theta - math.pi*1.5)
        #theta4 = [theta3_angle, theta4_angle]
        #ax.plot(theta4, r4)
        #
        plt.show()     
#
# Linear varying unit load (at the base theta > pi/2)
@dataclass
class ring_9(RingBase):
    '''
    Circular Ring Case 9 
    Linear varying unit load at Base theta > pi/2
    
    Parameters
    ----------
    W = Load (force)
    
    Returns
    ----------
    M
    N
    V
    
    Notes
    ----------
    Formulas for stress and strain, 7th edition
    R.J.Roark & W.C.Young 
    
    Examples
    '''
    title:str = "Linear varying unit load at Base theta > pi/2"
    #
    def get_load(self, crad, k1, k2):
        """ """
        apld = self.apld
        istep = self.istep
        w = apld / (crad*math.sin(self.theta))
        xmom = [0 for i in range(istep)]
        xhop = [0 for i in range(istep)]
        xshr = [0 for i in range(istep)]
        
        print ("")
        print ('roark case 9 Linear varying unit load at Base theta > pi/2')
        print ("")
        print(f'Applied load C: {apld: 1.3e}')
        print(f'Applied load w: {w: 1.3e}')
        print(f'Phase: {math.degrees(self.phase): 1.3f} deg')
        print(f'Theta: {math.degrees(self.theta): 1.3f} deg')
        print ("")
        pi = math.pi
        tau = math.tau # 2*pi     
        a = self.theta # * math.pi / 180.0
        sth = math.sin(a)
        cth = math.cos(a)
        #c = sth * sth * sth
        #d1 = apld * crad * crad
        #e = a / math.pi
        #p = 1 / math.pi
        #f = p * (11 * sth * cth / 36.0 + (1 + cth) / (9.0 * sth) + 0.5 * sth)
        #g = c * k2 / (12.0 * math.pi)
        #amom = d1 * ((1 - e) * (0.25 + (sth * sth) / 6.0) - f - g)
        amom = (w * crad**2 / (36. * math.pi * sth)
                * ((pi - a) * (6. * sth**3 - 9. * sth) - (3 * sth**4)
                   + 8 + (8 * cth) - (5 * sth**2 * cth) 
                   - (6 * k2 * (3 * sth * (sth - pi + a)
                                 + (sth**2 * cth) + 2 + (2 * cth)))))
        #ahop = -apld * crad * g
        ahop = -w * crad * sth**3 / (12. * pi)
        ashr = 0
        ist = (istep // 2) + 1
        sin = math.sin
        cos = math.cos
        for j in range(ist):
            i = j
            d = i / istep
            x = tau * d
            sx = sin(x)
            cx = cos(x)
            if a > x:
                Ltm = 0
                Ltn = 0
                Ltv = 0
            else:
                #tm = (d1 / 6) * ((sx * sx * sx) / sth + 3 * sx * sth - sth * sth - 3 * sx * sx)
                Ltm = (w * crad**2 / (6. * sth)) * (sx - sth)** 3
                #tt = ((apld * crad * sx) / sth) * ((sx * sx) / sth + sth - 2 * sx)
                Ltn = ((w * crad * sx) / sth**2) * (sx - sth)** 2
                #tv = ((apld * crad * cx) / 2) * ((sx * sx) / sth + sth - 2 * sx)
                Ltv = ((w * crad * cx) / (2. * sth)) * (sx - sth)** 2
            #
            self.solution(amom, ahop, ashr, Ltm, Ltn, Ltv, istep, ist, 
                          crad, sx, cx, j, xmom, xhop, xshr)        
        #
        return self.rotadd(self.phase, xmom, xhop, xshr)
    #
    def view(self):
        """ """
        plt = plot_view(self)
        #
        #theta4_angle = (self.theta - math.pi*1.51)
        #r4 = [ 1, 1.5]
        #theta3_angle = (self.theta - math.pi*1.5)
        #theta4 = [theta3_angle, theta4_angle]
        #ax.plot(theta4, r4)
        #
        plt.show()     
#
# Uniform compressive unit load (from 180 to theta)
@dataclass
class ring_10(RingBase):
    '''
    Circular Ring Case 10 
    Uniform compressive unit load from 180 to theta
    
    Parameters
    ----------
    W = Load (force)
    
    Returns
    ----------
    M
    N
    V
    
    Notes
    ----------
    Formulas for stress and strain, 7th edition
    R.J.Roark & W.C.Young 
    
    Examples
    '''
    title:str = "Uniform Compressive Unit Load"
    #
    def get_load(self, crad, k1, k2):
        """ """
        apld = self.apld
        istep = self.istep    
        xmom = [0 for i in range(istep)]
        xhop = [0 for i in range(istep)]
        xshr = [0 for i in range(istep)]
        
        print (" ")
        print ('roark case 10 Uniform Compressive Unit Load from 180 to theta')
        print ("")
        print(f'Applied load w: {apld: 1.3e}')
        print(f'Phase: {math.degrees(self.phase): 1.3f} deg')
        print(f'Theta: {math.degrees(self.theta): 1.3f} deg')
        print ("")
        pi = math.pi
        tau = math.tau # 2*pi     
        a = self.theta # * math.pi / 180.0
        sth = math.sin(a)
        cth = math.cos(a)
        #b = (sth**3) * k2 / 3.0
        #c = sth * sth / 2.0
        #d = a * cth
        #e = apld * crad
        #p = 1.0 / math.pi
        #amom = -e * crad * (cth + c - 0.75 + p * (sth - d + 0.75 * (a - sth * cth) - a * c - b))
        amom = (-apld * crad**2 / (4. * pi)
                * ((pi - a)*(4 * cth + 2 * sth**2 - 1.0)
                   + sth * (4 - (4 * sth**2 / 3.0) - cth)
                   - 2 * k2 * (pi - a + sth * cth)))
        #ahop = -e * (cth + p * (sth - d - b))
        ahop = (-apld * crad / pi 
                * (pi * cth + sth - a * cth - sth**3 / 3.0))
        ashr = 0
        ist = (istep // 2) + 1
        sin = math.sin
        cos = math.cos
        for j in range(ist):
            i = j
            d = i / istep
            x = tau * d
            sx = sin(x)
            cx = cos(x)
            if a > x:
                Ltm = 0
                Ltn = 0
                Ltv = 0
            else:
                #tm = (-e * crad / 2.0) * (1 + cx * cx - sth * sth - 2 * cx * cth)
                Ltm = (-apld * crad**2 / 2.0) * (cth - cx)**2
                #tt = e * cx * (cth - cx)
                Ltn = (apld * crad * cx) * (cth - cx)
                #tv = -e * sx * (cth - cx)
                Ltv = (-apld * crad * sx) * (cth - cx)
            #
            self.solution(amom, ahop, ashr, Ltm, Ltn, Ltv, istep, ist, 
                          crad, sx, cx, j, xmom, xhop, xshr)
        #
        return self.rotadd(self.phase, xmom, xhop, xshr)
    #
    def view(self):
        """ """
        plt = plot_view(self)
        #
        #theta4_angle = (self.theta - math.pi*1.51)
        #r4 = [ 1, 1.5]
        #theta3_angle = (self.theta - math.pi*1.5)
        #theta4 = [theta3_angle, theta4_angle]
        #ax.plot(theta4, r4)
        #
        plt.show()     
#
# Linear varying unit load (from 180 to theta)
@dataclass
class ring_11(RingBase):
    '''
    Circular Ring Case 11 
    TDL from 180 to theta
    
    Parameters
    ----------
    W = Load (force)
    
    Returns
    ----------
    M
    N
    V
    
    Notes
    ----------
    Formulas for stress and strain, 7th edition
    R.J.Roark & W.C.Young 
    
    Examples
    '''
    title:str = "Linear Varying Unit Load"
    #
    def get_load(self, crad, k1, k2):
        """ """
        apld = self.apld
        istep = self.istep    
        xmom = [0 for i in range(istep)]
        xhop = [0 for i in range(istep)]
        xshr = [0 for i in range(istep)]
        
        print (' ')
        print ('roark case 11 Linear Varying Unit Load')
        print ("")
        print(f'Applied load w: {apld: 1.3e}')
        print(f'Phase: {math.degrees(self.phase): 1.3f} deg')
        print(f'Theta: {math.degrees(self.theta): 1.3f} deg')
        print ("")
        pi = math.pi
        tau = math.tau # 2*pi
        a = self.theta # * math.pi / 180.0
        sth = math.sin(a)
        cth = math.cos(a)
        #b = a / math.pi
        #s2 = sth * sth
        #s3 = sth * sth * sth
        #c2 = cth * cth
        #c3 = cth * cth * cth
        #f = apld * crad * crad / (1 + cth)
        #g = 1 - b
        #h = (0.75 * cth - 0.5 * c2 - c3 / 3.0 - s2 * cth / 2.0 - 0.25)
        #q = (1.0 / math.pi) * (sth / 9.0 - (3 * sth * cth) / 4.0 + (11 * sth * c2) / 36.0)
        #r = k2 / (8 * math.pi)
        #r1 = r * (math.pi - a + sth * cth + (2 * s3 * cth) / 3.0)
        #amom = f * (g * h + q + r1)
        amom = (-apld * crad**2 / (pi * (1 + cth))
                * ((pi - a) * (((3 + 12 * cth**2 + 2 *cth + 4 * cth * sth**2) / 24.0)
                                    - ((3 * sth**3 * cth - 3 * sth - 5 * sth**3) / 36.0)
                                    + (5 * sth * cth / 8.0)
                                    - k2 * (pi * cth / 2.0 - a * cth / 2.0 
                                             + sth**3 / 3.0 + sth * cth**2 / 2.0))))
        #w = (apld * crad) / (1 + cth)
        #p = 1 / math.pi
        #ahop = w * (p * (a / 4.0 + a * c2 / 2.0 - 3 * sth * cth / 4.0) - 0.25 - c2 / 2.0 + r1)
        ahop = (-apld * crad / (pi * (1 + cth))
                * ((pi - a) * (((1 + 4 * cth**2) / 8.0) + (5 * sth * cth / 8.0)
                               - (sth**3 * cth / 12.0))))
        ashr = 0
        ist = (istep // 2) + 1
        sin = math.sin
        cos = math.cos
        for j in range(ist):
            i = j
            d = i / istep
            x = tau * d
            sx = sin(x)
            cx = cos(x)
            if a > x:
                Ltm = 0
                Ltn = 0
                Ltv = 0
            else:
                #tm = f * ((cx**3) / 6.0 - c3 / 6.0 + cx * c2 / 2.0 - cx * cx * cth / 2.0)
                Ltm = (-apld * crad**2 / (6.0 * (1 + cth))) * (cth - cx)**3
                #tt = ((apld * crad * cx) / (2.0 * (1 + cth))) * ((cth - cx)**2)
                Ltn = (apld * crad * cx / (2.0 * (1 + cth))) * (cth - cx)**2
                #tv = ((apld * crad * sx) / (2.0 * (1 + cth))) * ((cth - cx)**2)
                Ltv = (-apld * crad * sx / (2.0 * (1 + cth))) * (cth - cx)**2
            #
            self.solution(amom, ahop, ashr, Ltm, Ltn, Ltv, istep, ist, 
                          crad, sx, cx, j, xmom, xhop, xshr)        
        #
        return self.rotadd(self.phase, xmom, xhop, xshr)
    #
    def view(self):
        """ """
        plt = plot_view(self)
        #
        #theta4_angle = (self.theta - math.pi*1.51)
        #r4 = [ 1, 1.5]
        #theta3_angle = (self.theta - math.pi*1.5)
        #theta4 = [theta3_angle, theta4_angle]
        #ax.plot(theta4, r4)
        #
        plt.show()     
#
# Uniform radial unit load (from 180 to theta)
@dataclass
class ring_12(RingBase):
    '''
    Circular Ring Case 12
    Uniform radial unit load Base to theta
    
    Parameters
    ----------
    W = Load (force)
    
    Returns
    ----------
    M
    N
    V
    
    Notes
    ----------
    Formulas for stress and strain, 7th edition
    R.J.Roark & W.C.Young 
    
    Examples 
    '''
    title:int = "Uniform Radial Unit Load"
    #
    def get_load(self, crad, k1, k2):
        """ """
        apld = self.apld
        istep = self.istep    
        w = apld / (2*crad*math.sin(self.theta))
        xmom = [0 for i in range(istep)]
        xhop = [0 for i in range(istep)]
        xshr = [0 for i in range(istep)]
        print ('')
        print ('Roark case 12 Uniform Radial Unit Load')
        print ('')
        print(f'R: {crad: 1.3e} m')
        print(f'Applied load W: {apld: 1.3e}')
        print(f'Applied load w: {w: 1.3e}')
        print(f'Phase: {math.degrees(self.phase): 1.3f} deg')
        print(f'Theta: {math.degrees(self.theta): 1.3f} deg')
        print ('')
        pi = math.pi
        tau = math.tau # 2*pi
        ist = (istep // 2) + 1
        a = self.theta # * pi / 180.0
        sth = math.sin(a)
        cth = math.cos(a)
        #
        print('Applied load w : ',w)
        #p = 1.0 / math.pi
        amom = (-w * crad**2 / pi
                * (sth + pi * cth - a * cth - k2 * (pi - a - sth)))
        ahop = -w * crad / pi * (sth + pi * cth - a * cth)
        ashr = 0
        sin = math.sin
        cos = math.cos    
        for j in range(ist):
            i = j
            d = i / istep
            x = tau * d 
            sx = sin(x)
            cx = cos(x)
            
            if a > x:
                Ltm = 0
                Ltn = 0
                Ltv = 0
            else:
                Ltm = -w * crad**2 * (1 - math.cos(x - a))
                Ltn = -w * crad * (1 - math.cos(x - a))
                Ltv = -w * crad * math.sin(x - a)
            #
            self.solution(amom, ahop, ashr, Ltm, Ltn, Ltv, istep, ist, 
                          crad, sx, cx, j, xmom, xhop, xshr)
        #
        return self.rotadd(self.phase, xmom, xhop, xshr)
    #
    def view(self):
        """ """
        plt = plot_view(self)
        #
        #theta4_angle = (self.theta - math.pi*1.51)
        #r4 = [ 1, 1.5]
        #theta3_angle = (self.theta - math.pi*1.5)
        #theta4 = [theta3_angle, theta4_angle]
        #ax.plot(theta4, r4)
        #
        plt.show()     
#
#
# Lineary varying radial unit load (from 180 to theta)
@dataclass
class ring_13(RingBase):
    '''
    Circular Ring Case 13
    Lineary varying radial unit load (from 180 to theta)
    
    Parameters
    ----------
    W = Load (force)
    
    Returns
    ----------
    M
    N
    V
    
    Notes
    ----------
    Formulas for stress and strain, 7th edition
    R.J.Roark & W.C.Young 
    
    Examples 
    '''
    title:int = "Lineary Varying Radial Unit Load"
    #
    def get_load(self, crad, k1, k2):
        """ """
        apld = self.apld
        istep = self.istep
        xmom = [0 for i in range(istep)]
        xhop = [0 for i in range(istep)]
        xshr = [0 for i in range(istep)]
        
        print ('')
        print ('roark case 13 Lineary Varying Radial Unit Load')
        print ('')
        print(f'Applied load C: {apld: 1.3e}')
        print(f'Phase: {math.degrees(self.phase): 1.3f} deg')
        print(f'Theta: {math.degrees(self.theta): 1.3f} deg')
        print ('')
        pi = math.pi
        tau = math.tau # 2*pi
        a = self.theta # * math.pi / 180.0
        ist = (istep // 2) + 1
        sth = math.sin(a)
        cth = math.cos(a)
        w = (apld * (pi - self.theta) 
             / (2*crad*(1+cth)))
        print('Applied load w : ',w)
        #
        amom = (-w * crad**2 / (pi * (pi - a))
                * (2 + 2 * cth - sth * (pi - a) 
                   + k2 * (1 + cth - (pi - a)**2 / 2.0)))
        
        ahop = (-w * crad / (pi * (pi - a))
                * (2 + 2 * cth - sth * (pi - a)))
        
        ashr = 0
        sin = math.sin
        cos = math.cos    
        for j in range(ist):
            i = j
            d = i / istep
            x = tau * d
            sx = sin(x)
            cx = cos(x)
            if a > x:
                Ltm = 0
                Ltn = 0
                Ltv = 0
            else:
                Ltm = (-w * crad **2 / (pi - a) 
                      * (x - a - sx * cth + cx * sth))
                Ltn = (-w * crad / (pi - a) 
                      * (x - a - sx * cth + cx * sth))
                Ltv = (-w * crad / (pi - a) 
                      * (1 - cx * cth - sx * sth))
            #
            self.solution(amom, ahop, ashr, Ltm, Ltn, Ltv, istep, ist, 
                          crad, sx, cx, j, xmom, xhop, xshr)
        #
        return self.rotadd(self.phase, xmom, xhop, xshr)
    #
    def view(self):
        """ """
        plt = plot_view(self)
        #
        #theta4_angle = (self.theta - math.pi*1.51)
        #r4 = [ 1, 1.5]
        #theta3_angle = (self.theta - math.pi*1.5)
        #theta4 = [theta3_angle, theta4_angle]
        #ax.plot(theta4, r4)
        #
        plt.show()     
#
# Radial quadratic unit load (from 180 to theta)
@dataclass
class ring_14(RingBase):
    '''
    Circular Ring Case 14
    Radial quadratic unit load (from 180 to theta)
    
    Parameters
    ----------
    W = Load (force)
    
    Returns
    ----------
    M
    N
    V
    
    Notes
    ----------
    Formulas for stress and strain, 7th edition
    R.J.Roark & W.C.Young 
    
    Examples 
    '''
    title:int = "Radial Quadratic Unit Load"
    #
    def get_load(self, crad, k1, k2):
        """ """
        apld = self.apld
        istep = self.istep    
        xmom = [0 for i in range(istep)]
        xhop = [0 for i in range(istep)]
        xshr = [0 for i in range(istep)]
        
        print ('')
        print ('roark case 14 Radial Quadratic Unit Load')
        print ('')
        print(f'Applied load C: {apld: 1.3e}')
        print(f'Phase: {math.degrees(self.phase): 1.3f} deg')
        print(f'Theta: {math.degrees(self.theta): 1.3f} deg')
        print ('')
        pi = math.pi
        tau = math.tau # 2*pi
        a = self.theta # * math.pi / 180.0
        ist = (istep // 2) + 1
        sth = math.sin(a)
        cth = math.cos(a)
        #
        w = (apld * (pi - self.theta)**2
             / (4*crad*(pi - self.theta - sth)))
        print('Applied load w : ',w)
        #
        amom = ((-w * crad**2 / (pi * (pi - a)**2))
                * (2 * (pi - a) * (2 - cth) - 6 * sth
                   + k2 * (2 * (pi - a - sth) 
                            - (pi - a)**3 / 3.0)))
        
        ahop = (-w * crad / (pi * (pi - a)**2)
                * (2 * (pi - a) * (2 - cth) - 6 * sth))
        
        ashr = 0
        sin = math.sin
        cos = math.cos
        for j in range(ist):
            i = j
            d = i / istep
            x = tau * d
            sx = sin(x)
            cx = cos(x)
            if a > x:
                Ltm = 0
                Ltn = 0
                Ltv = 0
            else:
                Ltm = (-w * crad **2 / (pi - a)**2 
                      * ((x - a)**2 - 2 + 2 * cx *cth 
                         + 2 * sx * sth))
                Ltn = (-w * crad / (pi - a)**2 
                      * ((x - a)**2 - 2 + 2 * cx *cth
                         + 2 * sx * sth))
                Ltv = (-2 * w * crad / (pi - a)**2 
                      * (x - a - sx * cth + cx * sth))
            #
            self.solution(amom, ahop, ashr, Ltm, Ltn, Ltv, istep, ist, 
                          crad, sx, cx, j, xmom, xhop, xshr)
        #
        return self.rotadd(self.phase, xmom, xhop, xshr)
    #
    def view(self):
        """ """
        plt = plot_view(self)
        #
        #theta4_angle = (self.theta - math.pi*1.51)
        #r4 = [ 1, 1.5]
        #theta3_angle = (self.theta - math.pi*1.5)
        #theta4 = [theta3_angle, theta4_angle]
        #ax.plot(theta4, r4)
        #
        plt.show()     
#
# Ring supported at the base and loaded by own weight
@dataclass
class ring_15(RingBase):
    '''
    Circular Ring Case 15
    Ring supported at the base and loaded by own weight
    
    Parameters
    ----------
    W = Load (force)
    
    Returns
    ----------
    M
    N
    V
    
    Notes
    ----------
    Formulas for stress and strain, 7th edition
    R.J.Roark & W.C.Young 
    
    Examples 
    '''
    title:str = "Ring Supported at the Base and Loaded by Own Weight"
    #
    #def properties(self, beta, Area, E, I):
    #    """ """
    #    self.beta = beta
    #    self.area = Area
    #    self.I = I
    #
    def get_load(self, crad, k1, k2):
        """ """
        apld = self.apld
        istep = self.istep    
        xmom = [0 for i in range(istep)]
        xhop = [0 for i in range(istep)]
        xshr = [0 for i in range(istep)]
        
        print (' ')
        print ('roark case 15 Ring Supported at the Base and Loaded by Own Weight')
        print ("")
        print(f'Applied load W: {apld: 1.3e}')
        print(f'Phase: {math.degrees(self.phase): 1.3f} deg')
        print(f'Theta: {math.degrees(self.theta): 1.3f} deg')
        print ("")
        #pi = math.pi
        tau = math.tau # 2*pi    
        #a = theta # * math.pi / 180.0
        #sth = math.sin(a)
        #cth = math.cos(a)
        #amom = apld * (crad * crad) * k2 / 2.0
        Kt = 1 + (I / (Area * crad**2))
        amom = (apld * crad**2 * (k2 - 0.50 - ((Kt - 1) * beta / k1)))
        #ahop = apld * crad * k2 / 2.0
        ahop = apld * crad * (0.50 + ((Kt - 1) * k2 / k1))
        ashr = 0
        ist = (istep // 2) + 1
        sin = math.sin
        cos = math.cos
        for j in range(ist):
            i = j
            d = i / istep
            x = tau * d
            sx = sin(x)
            cx = cos(x)
            #tm = -apld * (crad * crad) * (x * sx + cx - 1)
            Ltm = -apld * crad**2 * (x * sx + Kt * (cx - 1))
            Ltn = -apld * crad * x * sx
            Ltv = -apld * crad * x * cx
            #
            self.solution(amom, ahop, ashr, Ltm, Ltn, Ltv, istep, ist, 
                          crad, sx, cx, j, xmom, xhop, xshr)
        #
        return self.rotadd(phase, xmom, xhop, xshr)
#
# Unit axial segment of pipe filled with liquid
@dataclass
class ring_16(RingBase):
    '''
    Circular Ring Case 16
    Unit axial segment of pipe filled with liquid
    
    Parameters
    ----------
    W = Load (force)
    
    Returns
    ----------
    M
    N
    V
    
    Notes
    ----------
    Formulas for stress and strain, 7th edition
    R.J.Roark & W.C.Young 
    
    Examples 
    '''
    #rho:float
    title:str = "Unit Axial Segment of Pipe Filled with Liquid"
    #
    def get_load(self, crad, k1, k2):
        """ """
        apld = self.apld
        istep = self.istep
        rho = self.rho
        xmom = [0 for i in range(istep)]
        xhop = [0 for i in range(istep)]
        xshr = [0 for i in range(istep)]
        
        print (' ')
        print ('roark case 16 Unit Axial Segment of Pipe Filled with Liquid')
        print ("")
        print(f'Rho  : {rho: 1.3e} kg/m^3')
        print(f'Phase: {math.degrees(self.phase): 1.3f} deg')
        print(f'Theta: {math.degrees(self.theta): 1.3f} deg')
        print ("")
        #pi = math.pi
        tau = math.tau # 2*pi     
        #a = theta # * math.pi / 180.0
        #sth = math.sin(a)
        #cth = math.cos(a)
        amom = (rho * crad**3 * (0.75 - k2 / 2.0))
        ahop = rho * crad**2 * 0.75
        ashr = 0
        ist = (istep // 2) + 1
        sin = math.sin
        cos = math.cos
        for j in range(ist):
            i = j
            d = i / istep
            x = tau * d
            sx = sin(x)
            cx = cos(x)
            
            Ltm = rho * crad**3 * (1 - cx - x * sx / 2.0)
            Ltn = rho * crad**2 * (1 - cx - x * sx / 2.0)
            Ltv = rho * crad**2 * (sx / 2.0 - x * cx / 2.0)
            #
            self.solution(amom, ahop, ashr, Ltm, Ltn, Ltv, istep, ist, 
                          crad, sx, cx, j, xmom, xhop, xshr)
        #
        return self.rotadd(self.phase, xmom, xhop, xshr)
    #
    def view(self):
        """ """
        plt = plot_view(self)
        #
        #theta4_angle = (self.theta - math.pi*1.51)
        #r4 = [ 1, 1.5]
        #theta3_angle = (self.theta - math.pi*1.5)
        #theta4 = [theta3_angle, theta4_angle]
        #ax.plot(theta4, r4)
        #
        plt.show()     
#
# Unit axial segment of pipe partly filled with liquid
@dataclass
class ring_17(RingBase):
    '''
    Circular Ring Case 17
    Unit axial segment of pipe partly filled with liquid
    
    Parameters
    ----------
    W = Load (force)
    
    Returns
    ----------
    M
    N
    V
    
    Notes
    ----------
    Formulas for stress and strain, 7th edition
    R.J.Roark & W.C.Young 
    
    Examples 
    '''
    #rho:float
    title:str = "Unit Axial Segment of Pipe Partly Filled with Liquid"
    #
    def get_load(self, crad, k1, k2):
        """ """
        apld = self.apld
        istep = self.istep
        rho = self.rho
        xmom = [0 for i in range(istep)]
        xhop = [0 for i in range(istep)]
        xshr = [0 for i in range(istep)]
        
        print (' ')
        print ('roark case 17 Unit Axial Segment of Pipe Partly Filled with Liquid')
        print ("")
        print(f'Rho  : {rho: 1.3e} kg/m^3')
        print(f'Phase: {math.degrees(self.phase): 1.3f} deg')
        print(f'Theta: {math.degrees(self.theta): 1.3f} deg')
        print ("")
        pi = math.pi
        tau = math.tau # 2*pi
        a = self.theta # * math.pi / 180.0
        sth = math.sin(a)
        cth = math.cos(a)
        amom = (rho * crad**3 / (4. * pi) 
                * (2 * a * sth**2 + 3 * sth * cth - 3 * a + pi
                   + 2 * pi * cth**2
                   + 2 * k2 * (sth * cth - 2 * sth 
                                + (pi - a) * (1 - 2 * cth))))
        ahop = (rho * crad**2 / (4. * pi) 
                * (3 * sth * cth + (pi - a) * (1 + 2 * cth**2)))
        ashr = 0
        ist = (istep // 2) + 1
        sin = math.sin
        cos = math.cos    
        for j in range(ist):
            i = j
            d = i / istep
            x = tau * d
            sx = sin(x)
            cx = cos(x)
            
            if a > x:
                Ltm = 0
                Ltn = 0
                Ltv = 0
            else:
                Ltm = (rho * crad**3 / 2.0 
                       * (2 * cth - sx * (x - a + sth * cth)
                          - cx * (1 + cth**2)))
                Ltn = (rho * crad**2 / 2.0 
                       * (2 * cth - sx * (x - a + sth * cth)
                          - cx * (1 + cth**2)))
                Ltv = (rho * crad**2 / 2.0 
                       * (sx * cth**2 - cx * (x - a + sth * cth)))
            #
            self.solution(amom, ahop, ashr, Ltm, Ltn, Ltv, istep, ist, 
                          crad, sx, cx, j, xmom, xhop, xshr)
        #
        return self.rotadd(self.phase, xmom, xhop, xshr)
    #
    def view(self):
        """ """
        plt = plot_view(self)
        #
        #theta4_angle = (self.theta - math.pi*1.51)
        #r4 = [ 1, 1.5]
        #theta3_angle = (self.theta - math.pi*1.5)
        #theta4 = [theta3_angle, theta4_angle]
        #ax.plot(theta4, r4)
        #
        plt.show()     
#
# Two antiparallel forces offset by angle phi + Tangential Shear
@dataclass
class ring_18(RingBase):
    '''
    Circular Ring Case 18 
    Two Antiparallel Forces Offset by Angle phi + Shear
    
    Parameters
    ----------
    W = Load (force)
    
    Returns
    ----------
    M
    N
    V
    
    Notes
    ----------
    Formulas for stress and strain, 7th edition
    R.J.Roark & W.C.Young 
    
    Examples 
    '''
    #phi:float
    title:str = "Two Antiparallel Forces Offset by Angle phi + Tangential Shear"
    #
    def get_load(self, crad, k1, k2):
        """ """
        apld = self.apld
        istep = self.istep    
        xmom = [0 for i in range(istep)]
        xhop = [0 for i in range(istep)]
        xshr = [0 for i in range(istep)]
        
        print (' ')
        print ('roark case 18 Two Antiparallel Forces Offset by Angle phi + Tangential Shear')
        print ("")
        pi = math.pi
        tau = math.tau # 2*pi    
        phi = self.phi # * math.pi / 180.0
        a = self.theta # * math.pi / 180.0
        sth = math.sin(a)
        cth = math.cos(a)
        sthi = math.sin(phi)
        cthi = math.cos(phi)
        #
        v = apld / (tau * crad**2)
        print(f'Applied load W: {apld: 1.3e} N-m')
        print(f'Applied load v: {v: 1.3e} N')
        print(f'Phase: {math.degrees(self.phase): 1.3f} deg')
        print(f'Theta: {math.degrees(self.theta): 1.3f} deg')
        print(f'Phi  : {math.degrees(phi): 1.3f} deg')
        print ("")        
        #
        #b = crad / (2.0 * math.pi)
        #c = sth * sth
        #d = sthi * sthi
        #amom = (-apld * b * (cth - cthi - (math.pi - a) *
        #                     sth + (math.pi - phi) * sthi + k2 * (c - d)))
        amom = (apld * crad / tau 
                * (sthi**2 - sth**2 - (pi - phi) * sthi 
                   + (pi - a) * sth - k2 * (cth - cthi)))
        #        
        #ahop = ((-apld / (2.0 * math.pi)) * (k2 * (c - d)))
        ahop = apld / tau * (sthi**2 - sth**2)
        #
        #ashr = ((apld / (2.0 * math.pi)) * 
        #        (a - phi + sth - sthi + k2 * 
        #         (sth * cth - sthi * cthi)))
        ashr = (apld / tau * (a - phi + sth - sthi + sth * cth - sthi * cthi))
        
        ist = (istep // 2) + 1
        sin = math.sin
        cos = math.cos    
        for j in range(istep):
            i = j
            d = i / istep
            x = tau * d
            sx = sin(x)
            cx = cos(x)
            Z = 1
            z1 = 1
            ax = x - 0.009
            
            if phi > ax : 
                Z = 0
            
            if a > ax : 
                z1 = 0
            
            Ltm = ((-apld * crad / tau * (sthi - sth) * (x - sx))
                   + apld * crad * (sx - sth) * z1 
                   - apld * crad * (sx - sthi) * Z)
            
            Ltn = ((apld / tau * (sthi - sth) * sx) 
                   + apld * sx * z1 
                   - apld * sx * Z)
            
            Ltv = ((-apld / tau * (sthi - sth) * (1 - cx))
                   + apld * cx * z1 
                   - apld * cx * Z)
            #
            self.solution(amom, ahop, ashr, Ltm, Ltn, Ltv, istep, ist, 
                          crad, sx, cx, j, xmom, xhop, xshr)
        #
        return self.rotadd(self.phase, xmom, xhop, xshr)
    #
    def view(self):
        """ """
        plt = plot_view(self)
        #
        #theta4_angle = (self.theta - math.pi*1.51)
        #r4 = [ 1, 1.5]
        #theta3_angle = (self.theta - math.pi*1.5)
        #theta4 = [theta3_angle, theta4_angle]
        #ax.plot(theta4, r4)
        #
        plt.show()     
#
#
# Moment on the rim + Tangential Shear
@dataclass
class ring_19(RingBase):
    '''
    Circular Ring Case 19 
    Ring Shear + Moment
    
    Parameters
    ----------
    W = Load (force)
    
    Returns
    ----------
    M
    N
    V
    
    Notes
    ----------
    Formulas for stress and strain, 7th edition
    R.J.Roark & W.C.Young 
    
    Examples 
    '''
    title:str = "Moment on the Rim + Tangential Shear"
    #
    def get_load(self, crad, k1, k2):
        """
        apld : Moment [N-m]
        """
        apld = self.apld
        istep = self.istep    
        xmom = [0 for i in range(istep)]
        xhop = [0 for i in range(istep)]
        xshr = [0 for i in range(istep)]
        
        print (' ')
        print ('roark case 19 Moment on the Rim + Tangential Shear')
        print ("")
        pi = math.pi
        tau = math.tau # 2*pi     
        a = self.theta # * math.pi / 180.0
        sth = math.sin(a)
        cth = math.cos(a)
        #
        v = apld / (tau * crad**2)
        print(f'Applied load W: {apld: 1.3e} N-m')
        print(f'Applied load v: {v: 1.3e} N')
        print(f'Phase: {math.degrees(self.phase): 1.3f} deg')
        print(f'Theta: {math.degrees(self.theta): 1.3f} deg')
        print ("")      
        #
        #p = 1.0 / math.pi
        #amom = (-apld * p / 2.0) * (math.pi - a - 2 * k2 * sth)
        amom = -apld / tau * (pi - a - (2 * k2 * sth / k1))
        #ahop = (apld * p / crad) * k2 * sth
        ahop = apld / (crad * pi) * (k2 * sth / k1)
        #ashr = (-apld * p / (2.0 * crad)) * (1 + 2 * k2 * cth)
        ashr = -apld / (tau * crad) * (1 + (2 * k2 * cth / k1))
        
        ist = (istep // 2) + 1
        sin = math.sin
        cos = math.cos
        for j in range(istep):
            i = j
            d = i / istep
            x = tau * d
            sx = sin(x)
            cx = cos(x)
            z1 = 1
            if a > x :
                z1 = 0
            #
            #tm = (-apld * p / 2.0) * (x - sx) + apld * z1
            Ltm = (-apld / tau * (x - sx)) + apld * z1
            #tt = apld * sx * p / (2.0 * crad)
            Ltn = apld * sx / (tau * crad)
            #tv = (-apld * p / (2.0 * crad)) * (1 - cx)
            Ltv = -apld / (tau * crad) * (1 - cx)
            #
            self.solution(amom, ahop, ashr, Ltm, Ltn, Ltv, istep, ist, 
                          crad, sx, cx, j, xmom, xhop, xshr)
        #
        return self.rotadd(self.phase, xmom, xhop, xshr)
    #
    def view(self):
        """ """
        plt = plot_view(self)
        #
        #theta4_angle = (self.theta - math.pi*1.51)
        #r4 = [ 1, 1.5]
        #theta3_angle = (self.theta - math.pi*1.5)
        #theta4 = [theta3_angle, theta4_angle]
        #ax.plot(theta4, r4)
        #
        plt.show()     
#
# Bulkhead or supporting ring in pipe + Tangential Shear
@dataclass
class ring_20(RingBase):
    '''
    Circular Ring Case 18 
    Point Support at Base + Tangential Shear
    
    Parameters
    ----------
    W = Load (force)
    
    Returns
    ----------
    M
    N
    V
    
    Notes
    ----------
    Formulas for stress and strain, 7th edition
    R.J.Roark & W.C.Young 
    
    Examples 
    '''
    title:str = "Bulkhead or Supporting Ring in Pipe + Tangential Shear"
    #
    def get_load(self, crad, k1, k2):
        """ """
        apld = self.apld
        istep = self.istep
        xmom = [0 for i in range(istep)]
        xhop = [0 for i in range(istep)]
        xshr = [0 for i in range(istep)]
        
        print (' ')
        print ('roark case 20 Bulkhead or Supporting Ring in Pipe + Tangential Shear')
        print ("")
        pi = math.pi
        tau = math.tau # 2*pi
        a = self.theta # * math.pi / 180
        sth = math.sin(a)
        cth = math.cos(a)
        #p = 1 / pi        
        #
        v = sum([apld * math.sin(math.radians(x)) / (pi * crad)
                 for x in range(180)])
        print(f'Applied load W: {apld: 1.3e} N')
        print(f'Applied load v: {v: 1.3e} N/m')
        print(f'Phase: {math.degrees(self.phase): 1.3f} deg')
        print(f'Theta: {math.degrees(self.theta): 1.3f} deg')
        print ("")        
        #
        #amom = (apld * crad * p / 2.0) * (k2 - 0.5)
        amom = apld * crad / tau * (k2 - 0.5)
        #ahop = (apld * p / 2.0) * (k2 + 0.5)
        ahop = 0.75 * apld / pi
        ashr = 0
        ist = (istep // 2) + 1
        sin = math.sin
        cos = math.cos    
        for j in range(ist):
            i = j
            d = i / istep
            x = tau * d
            sx = sin(x)
            cx = cos(x)
            #tm = (apld * crad * p) * (1 - cx - x * sx * 0.5)
            Ltm = apld * crad / pi * (1 - cx - (x * sx / 2.0))
            #tt = (-apld * p / 2.0) * x * sx
            Ltn = -apld / tau * x * sx
            #tv = (apld * p / 2.0) * (sx - x * cx)
            Ltv = apld / tau * (sx - x * cx)
            #
            self.solution(amom, ahop, ashr, Ltm, Ltn, Ltv, istep, ist, 
                          crad, sx, cx, j, xmom, xhop, xshr)
        #
        return self.rotadd(self.phase, xmom, xhop, xshr)
    #
    def view(self):
        """ """
        plt = plot_view(self)
        #
        #theta4_angle = (self.theta - math.pi*1.51)
        #r4 = [ 1, 1.5]
        #theta3_angle = (self.theta - math.pi*1.5)
        #theta4 = [theta3_angle, theta4_angle]
        #ax.plot(theta4, r4)
        #
        plt.show()     
#
# --------------------------------
#
def circular_ring(ring_case:int, ring_load:float, 
                  theta:float, phi:float, 
                  rho:float, phase:float, istep:int):
    """
    [case, load, theta, phi, rho, phase, istep]
    """
    #print ('==> roark case number: ',ring_case)
    # Updated to the 7th edition
    # Case 1
    if ring_case == 1:
        return ring_1(apld=ring_load, phase=phase, theta=theta, istep=istep)
    
    # Case 2
    elif ring_case == 2:
        return ring_2(apld=ring_load, phase=phase, theta=theta, istep=istep)
    
    # Case 3
    elif ring_case == 3:
        return ring_3(apld=ring_load, phase=phase, theta=theta, istep=istep)
    
    # Case 4
    elif ring_case == 4:
        return ring_4(apld=ring_load, phase=phase, theta=theta, istep=istep)
    
    # Case 5
    elif ring_case == 5:
        return ring_5(apld=ring_load, phase=phase, theta=theta, istep=istep)
    
    # Case 6
    elif ring_case == 6:
        return ring_6(apld=ring_load, phase=phase, theta=theta, istep=istep)
    
    # Case 7
    elif ring_case == 7:
        return ring_7(apld=ring_load, phase=phase, theta=theta, istep=istep)
    
    # Case 8
    elif ring_case == 8:
        return ring_8(apld=ring_load, phase=phase, theta=theta, istep=istep)
    
    # Case 9
    elif ring_case == 9:
        return ring_9(apld=ring_load, phase=phase, theta=theta, istep=istep)
    
    # Case 10
    elif ring_case == 10:
        return ring_10(apld=ring_load, phase=phase, theta=theta, istep=istep)
    
    # Case 11
    elif ring_case == 11:
        return ring_11(apld=ring_load, phase=phase, theta=theta, istep=istep)
    
    # Case 12
    elif ring_case == 12:
        return ring_12(apld=ring_load, phase=phase, theta=theta, istep=istep)
    
    # check it
    # Case 13
    elif ring_case == 13:
        return ring_13(apld=ring_load, phase=phase, theta=theta, istep=istep)
    
    # check it
    # Case 14
    elif ring_case == 14:
        return ring_14(apld=ring_load, phase=phase, theta=theta, istep=istep)
    #
    # Case 15
    elif ring_case == 15:
        return ring_15(apld=ring_load, phase=phase, theta=theta, istep=istep)
     
    # check it beta & gamma factors to be fixed
    # Case 16
    elif ring_case == 16:
        return ring_16(apld=ring_load, phase=phase, theta=theta, rho=rho, istep=istep)
    # check it beta & gamma factors to be fixed
    # Case 17
    elif ring_case == 17:
        return ring_17(apld=ring_load, phase=phase, theta=theta, rho=rho, istep=istep)
    #
    # Case 18
    elif ring_case == 18:
        return ring_18(apld=ring_load, phase=phase, theta=theta, phi=phi, istep=istep)
    
    # Case 19
    elif ring_case == 19:
        return ring_19(apld=ring_load, phase=phase, theta=theta, istep=istep)
    
    # Case 20
    elif ring_case == 20:
        return ring_20(apld=ring_load, phase=phase, theta=theta, istep=istep)
    
    #
    else:
        print('***ERROR***THIS ROARK CASE IS NOT IMPLEMENTED')
        print('This case will not be included in calculations')
    #
    #
    #
    #print ('LOCATION    MOMENT      THRUST      SHEAR')
    #print ('             N.mm         N           N')
    
    #for i in range (len(_xloc)):
    #    print("{:>5.1f}    {: 1.4e} {: 1.4e} {: 1.4e}"
    #          .format(_xloc[i], _xm[i], _xh[i], _xs[i]))
    #
    #
    print (' ')
    # end of main subroutineme loop
    #return _xloc, _xm, _xh, _xs
    #
#
