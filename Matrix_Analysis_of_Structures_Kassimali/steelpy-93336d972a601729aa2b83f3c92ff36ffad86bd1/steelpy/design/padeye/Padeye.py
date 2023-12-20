#
import math
import SectProp as sp


def sectionProperty(self):
    """
    """
    # main axis
    D = (self.Lbw_1 + self.Lbw_2) / 2.0
    Dp = self.Dph
    _sec = [sp.rectangle(D, self.hmp)]
    
    a = self.hmp
    b = self.Dmp
    h = self.Lch - D

    c1 = self.hch - self.Dmp/2.0
    c = a - c1 - self.Dmp
    _sec.append(sp.trapeziod(a, b, h, c))
    
    
    _sec.append(sp.semicircle(Dp))
    
    # Major axis
    _z = [D/2.0, D + h/2.0 + _sec[1][1], self.Lch - _sec[2][1]]
    # Sum area
    _Area = _sec[0][0] + _sec[1][0] - _sec[2][0]
    _Az = _sec[0][0] * _z[0] + _sec[1][0] * _z[1] - _sec[2][0] * _z[2]
    _zz = _Az / _Area
    _Azz = (_sec[0][0] * (_zz - _z[0])**2
            + _sec[1][0] * (_zz - _z[1])**2
            - _sec[2][0] * (_zz - _z[2])**2)
    # second moment of area
    _Iy = _sec[0][3] + _sec[1][3] - _sec[2][3] + _Azz
    _Zy = _Iy / max(_zz, self.Lch - _zz)
    print('{:1.4e}  {:1.4e} {:}'.format(_Iy, _Zy, max(_zz, self.Lch - _zz)))
    
    # weak axis
    _y = [self.hmp/2.0, 
          (self.hch + _sec[1][2]), 
          self.hch]
    
    _Iz = _sec[0][7] + _sec[1][7] - _sec[2][7]
    
    _Ay = _sec[0][0] * _y[0] + _sec[1][0] * _y[1] - _sec[2][0] * _y[2]
    _yy = _Ay / _Area
    
    _Ayy = (_sec[0][0] * (_yy - _y[0])**2
            + _sec[1][0] * (_yy - _y[1])**2
            - _sec[2][0] * (_yy - _y[2])**2)    
    # second moment of area
    _Iz = _sec[0][7] + _sec[1][7] - _sec[2][7] + _Ayy
    _Zz = _Iz / max(_yy, self.hmp - _yy)
    print('{:1.4e}  {:1.4e} {:}'.format(_Iz, _Zz, max(_yy, self.hmp - _yy)))
    
    _secA = sp.rectangle(self.Lch, self.tmp)
    _Zx = _secA[4]
    print('{:1.4e}  {:1.4e} {:}'.format(_secA[3], _secA[4], self.Lch/2.0 + _secA[1]))
    
    return _Zy, _Zz, _Zx
    
    

def weldDisign():
    pass


def eyeDimensioning(Dpin):
    """
    Dpin : pin diametre
    """
    
    # main plate thickness
    tmp = 0.25 - 0.40 * Dpin
    # main plate radius
    rmp = 1.75 * Dpin
    
    # cheek plate radius
    rcp = 1.5 * Dpin
    # cheek plate thickness
    tcp = 0.15 - 0.30 * Dpin
    
    # pin hole diametre
    HDpin = 1.04 * Dpin
    
    return tmp, rmp, rcp, tcp, HDpin


def loadVector(P, alpha, transP):
    """
    P     : load
    alpha : angle
    transP : transversal force
    """
    Pv = P * math.sin(math.radians(alpha))
    Ph = P * math.cos(math.radians(alpha))
    Pt = P * transP
    _P = [P, Pv, Ph, Pt]
    
    return _P


def cheeckPlateWelds(self, P, factor = 2):
    """
    """
    # load per cheeck plate
    _FF = (P[0] * self.tcp / (self.tmp + 2.0*self.tcp))
    
    # design load
    _FD = factor * _FF
    
    # Allowable weld stress
    _Fw = 0.30 * self.Fyw
    
    # weld lenght
    _Wl = self.Dcp * math.pi
    # weld area
    _Aw = _Wl * self.Wcp / math.sqrt(2)    
    # weld strenght
    _Ws = _FD /_Wl
    print('Weld strenght = {:} N/mm'.format(_Ws))
    
    # wel size required
    _Wreq = _Ws/_Fw
    print('Weld required = {:} mm'.format(_Wreq))    
    
    print('Actual Weld = {:} mm, Fy = {:}'.format(self.Wcp, self.Fyw)) 
    # shear stress at weld cheek-main plate
    _Fs2 = _FD / _Aw
    print('Fs = {:} N/mm2'.format(_Fs2))    
    
    

class padeyeDesign():
    """
    """
    def __init__(self):
        pass
    
    def units(self):
        """
        """
    
    def cheeckPlate(self, t, Dc, Wc = 0, Fy = 0):
        """
        tc : Cheek plate thickness
        Dc : Cheeck plate diameter
        Wc : Weld size
        """
        self.tcp = t
        self.Dcp = Dc
        self.Wcp = Wc
        self.Fyw = Fy
    
    def mainPlate(self, Fy, t, D, Dph, Lch, Lbw_1, Lbw_2, h, hch=0):
        """
        Fy : Yield strength
        t  : Main plate thickness
        D  : Main plate radius
        Dh : Pin hole diameter
        Lch : Centre hole distance (Z)
        Lbw_1 : Bottom width end 1
        Lbw_2 : Bottom width end 2
        h  : Main plate heigth
        hch : Centre hole distance (y)
        
        """
        self.Fymp = Fy
        self.tmp = t
        self.Dmp = D
        self.Dph = Dph
        self.Lch = Lch
        self.Lbw_1 = Lbw_1
        self.Lbw_2 = Lbw_2
        self.hmp = h
        self.hch = hch
        
    
    def material(self):
        """
        Fy : Yield streght
        E  : Elastic modulus
        """
    
    def load(self, P, alpha, Pt = 0.05):
        """
        P : Force (force unit)
        alpha : Force angle (degrees)
        Pt : Tranverse load (%)
        """
        self.P = P
        self.alpha = alpha
        self.Pt = Pt
    
    def stiffenerPlate(self):
        """
        tsp : Stiffener plate thickness
        Ln  : Distance from Padeye hole centre
        """
    
    def shackle(self):
        """
        a: Bow diametre
        b: Pin diametre
        e: Inside width
        f: Inside length
        g: Width of bow
        w: Weigth
        """
    
    def printResults(self):
        
        if self.hch == 0:
            self.hch = self.hmp / 2.0
        
        # check if distance to centre of the pin hole > main plate diam
        if self.hch - self.Dmp/2.0 < 0:
            self.hch = self.Dmp/2.0        
        
        _Lmp = self.Lch + self.Dmp/2.0 # main plate total heigth
        
        # Padeye section properties
        _Ze = sectionProperty(self)
        
        P = self.P
        alpha = self.alpha
        transP = self.Pt
        _P = loadVector(P, alpha, transP)        
        # check this
        _Mi = (_P[1] * self.Lch - _P[2] *((self.hmp/2.0 - self.Dmp/2.0 ) 
                                          - abs(2/3.0 * self.Lch - self.hmp/2.0)))
        print('Mi = {:1.4e} N/mm2'.format(_Mi))
        
        # bearing stress at contact area
        _Fp = _P[0] / (self.Dph * (self.tmp + 2 * self.tcp))
        print('Fp = {:} N/mm2'.format(_Fp))
        
        # shear stress at section alpha-alpha
        _Fs = (1.10 * _P[0] 
               / (2 * (self.tmp * self.Dmp/2.0 
                       + 2 * self.tcp * self.Dcp / 2.0)))
        print('Fs = {:} N/mm2'.format(_Fs))
        
        # Tension stress at section beta-beta
        _Ft = (_P[0] 
               / (self.tmp * (self.Dmp - self.Dph) 
                  + 2 * self.tcp * (self.Dcp - self.Dph)))
        print('Ft = {:} N/mm2'.format(_Ft))
        
        # tear out stress at section
        #_uno = self.tmp * math.pi * self.Dcp / 8.0
        #_dos = self.tmp * math.sqrt()
        #_Fs1 = _P[0] / ()
        
        cheeckPlateWelds(self, _P)
        
        # combined stress
        _Faa = _P[2] / (self.tcp * self.hmp)
        _Fba = _Mi / _Ze[2]
        _Fc = 0
        
        print('ok')
    