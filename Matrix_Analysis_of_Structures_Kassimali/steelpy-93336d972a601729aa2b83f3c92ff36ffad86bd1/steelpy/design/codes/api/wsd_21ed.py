# *****************************************
#                 Log History
#
#
#
# *****************************************
#
from WaveStokes5 import *
#
import math
import datetime
#
#
#
#
def UnitFinder(UnitConv):
    #
    if UnitConv == 'SI':
        SectionFinal = 'SI'
    #
    elif UnitConv == 'METRIC':
        SectionFinal = 'SI'
    #
    elif UnitConv == 'METRIC SYSTEM':
        SectionFinal = 'SI'
    #
    elif UnitConv == 'I':
        SectionFinal = 'SI'
    #
    #
    else:
        print ('Unit Not Recognised')
        exit()
    #
    #
    return SectionFinal   
#
#
def UnitLengthConv(LUnit, UnitOutput):
#
    if UnitOutput == "MM":
        #
        # Metric system
        if LUnit == 'KM':
            FactorL = 1000000.0
        #
        elif LUnit == 'M':
            FactorL = 1000.0
        #
        elif LUnit == 'CM':
            FactorL = 10.0
        #
        elif LUnit == 'MM':
            FactorL = 1.0
        #
        # Alien system
        elif LUnit == 'IN':
            FactorL = 25.40
        #
        elif LUnit == 'FT':
            FactorL = 304.8
        #
        elif LUnit == 'YARD':
            FactorL = 914.4
        #
        elif LUnit == 'MILE':
            FactorL = 1609344.0
        #
        else:
            print ('Unit Not Recognised')
            exit()
            #
    #
    elif UnitOutput == "IN":
        #
        # Metric system
        if LUnit == 'KM':
            FactorL = 39370.07874
        #
        elif LUnit == 'M':
            FactorL = 39.37007874
        #
        elif LUnit == 'CM':
            FactorL = 0.3937007874
        #
        elif LUnit == 'MM':
            FactorL = 0.03937007874
        #
        # Alien system
        elif LUnit == 'IN':
            FactorL = 1.0
        #
        elif LUnit == 'FT':
            FactorL = 12.0
        #
        elif LUnit == 'YARD':
            FactorL = 36.0
        #
        elif LUnit == 'MILE':
            FactorL = 63360.0
        #
        else:
            print ('Unit Not Recognised')
            exit()
            #
    #
    return FactorL
#
#
def UnitForceConv(FUnit, UnitOutput):
    #
    #
    if UnitOutput == "N":
        #
        # Metric system
        if FUnit == 'MN':
            FactorF=1000000.0
        #
        elif FUnit == 'KN':
            FactorF=1000.0
        #
        elif FUnit == 'N':
            FactorF=1.0
        #
        # Alien system
        elif FUnit == 'POUNDS':
            FactorF = 4.4482
        #
        elif FUnit == 'KIPS':
            FactorF = 4448.2
        #
        else:
            print ('Unit Not Recognised')
            exit()
            #
    #
    # 
    elif UnitOutput == 'POUNDS':
        #
        # Metric system
        if FUnit == 'MN':
            FactorF = 224810.0
        #
        if FUnit == 'KN':
            FactorF = 224.81
        #
        if FUnit == 'N':
            FactorF = 0.22481
        #
        # Alien system
        elif FUnit == 'POUNDS':
            FactorF = 1.0
        #
        elif FUnit == 'KIPS':
            FactorF = 1000.0
        #
        else:
            print ('Unit Not Recognised')
            exit()
            #
    #
    return FactorF
#
#
def UnitMassConv(FUnit, UnitOutput):
    #
    #
    if UnitOutput == "KG":
        #
        # Metric system
        if FUnit == 'TE':
            FactorF = 0.001
        #
        else:
            print ('Unit Not Recognised')
            exit()
    #
    return FactorF
#
#
class APIwsd21ed:
    #
    #
    def __init__(self):
        pass
    #
    #
    def SectionProperties(self):
        #
        #
        #     
        #-------------------------------------------------
        #   Cross-Sectional Area
        #    def A(self):
        #
        self.A = ((math.pi / 4.)*
                  (self.D**2 -(self.D - 2*self.t)**2))
        #print ('circular area:',self.SecArea)
        #
        #
        #
        #
        #-------------------------------------------------
        #               Section Properties
        #-------------------------------------------------
        #
        #
        #   Second Moment of Area about Mayor Axis
        #   --------------------------------------
        #
        self.I = ((math.pi / 64.0)*
                  (self.D**4 - (self.D-2*self.t)**4))
        # print ('circular Second Moment',self.Iip)
        #
        #
        #       
        #   Elastic Modulus about Mayor Axis
        #   --------------------------------------
        #
        self.S = ((math.pi/64.0) * 
                   (self.D**4 - (self.D - 2 * self.t)**4) /
                   (self.D / 2.0))
        # print ('circular Elastic Modulus',self.S)
        #
        #
        #
        #-------------------------------------------------
        #   Plastic Modulus about Mayor Axis
        #    def Zx(self):
        #
        self.Z = ((self.D**3 - 
                    (self.D - 2 * self.t)**3) / 6.0 )
        # print ('circular Plastic Modulus',self.Z)
        #
        #
        #-------------------------------------------------
        #   Radius of gyration about Mayor Axis
        #
        self.r = (math.sqrt(self.I / self.A))
        # print ('circular Radius of gyration about Mayor Axis',self.rip)
        # 
        #self.rop = self.rip
        #
        #-------------------------------------------------
        #   Torsional Constant
        #
        self.J = (2 * self.I)
        # print ('Torsional Constant',self.J)
        # 
        #
        #-------------------------------------------------
        #   Polar Moment of Inertia
        self.Ip = ((math.pi / 32.0) *
                   (self.D**4 - 
                    (self.D - 2*self.t)**4))
        # print ('Polar Moment of Inertia Ip:',self.Ip)
        #
        #
        #-------------------------------------------------
        #  Mass
        self.mass = (self.A * self.Rhos)/1000**2
        #
    #
    #-------------------------------------------------
    #                   Main Section
    #-------------------------------------------------
    #
    #
    #    3.2 ALLOWABLE STRESSES FOR CYLINDRICAL MEMBERS
    #
    #    3.2.1 Axial Tension
    #
    def AxialTension(self):
        # 
        # The allowable tensile stress, Ft, for cylindrical members
        # subjected to axial tensile loads should be determined from:
        #
        #  (3.2.1-1)
        self.Ft = 0.60 * self.Fy
        print ('Ft Allowable', self.Ft)
        #        
        # The utilization of a member Um
        self.UmTension = self.fa / self.Ft
        self.UmTension_Flag = '(3.2.1-1)'
        print ('IR Tension = ',self.UmTension)
        print (' ')
    #
    #
    #   3.2.2 Axial Compression
    #
    def AxialCompression(self):
        #      
        #  3.2.2.b Local Buckling
        #
        # Unstiffened cylindrical members fabricated from structural
        # steels specified in Section 8.1 should be investigated for local
        # buckling due to axial compression when the D/t ratio is
        # greater than 60. When the D/t ratio is greater than 60 and less
        # than 300, with wall thickness t > 0.25 in. (6 mm), both the
        # elastic (Fxe) and inelastic local buckling stress (Fxc) due to
        # axial compression should be determined from Eq. 3.2.2-3 and
        # Eq. 3.2.2-4. Overall column buckling should be determined
        # by substituting the critical local buckling stress (Fxe or Fxc,
        # whichever is smaller) for Fy in Eq. 3.2.2-1 and in the equation
        # for Cc.
        #
        # 1. Elastic Local Buckling Stress.
        # The elastic local buckling stress, Fxe, should be determined
        # from:
        # (3.2.2-3)
        #  where
        # C = critical elastic buckling coefficient,
        # D = outside diameter, in. (m),
        # t = wall thickness, in. (m).
        #
        # The theoretical value of C is 0.6. However, a reduced value
        # of C = 0.3 is recommended for use in Eq. 3.2.2-3 to account
        # for the effect of initial geometric imperfections within API
        # Spec 2B tolerance limits.
        # self.C = 0.30
        #
        #
        print('D/t =', self.D/self.t)
        #
        self.Fxe = 2*self.C*self.E*self.t / self.D
        print('Fxe = ', self.Fxe)
        #
        #
        # 2. Inelastic Local Buckling Stress.
        # The inelastic local buckling stress, Fxc, should be determined
        # from:
        # (3.2.2-4)
        self.Fxc = min((self.Fy * 
                        (1.64 - 0.23 * math.pow(self.D/self.t, 1/4))),
                       self.Fxe)
        #
        if (self.D/self.t) <= 60:
            self.Fxc = self.Fy
        #
        print('Fxc =', self.Fxc)
        #
        _Fymod = self.Fy
        #
        if (self.D/self.t) > 60:
            _Fymod = min( self.Fxe, self.Fxc)
        #
        #
        #  3.2.2.a Column Buckling
        # 
        # E = Youngs Modulus of elasticity, ksi (MPa)
        # K = effective length factor, Section 3.3.1d
        # l = unbraced length, in. (m)
        # r = radius of gyration, in. (m)
        #
        self.KLr = max((self.Kx*self.Lx/self.r), 
                       (self.Ky*self.Ly/self.r) )
        #
        print('KLr =', self.KLr)
        #
        self.Cc = math.sqrt((2 * self.E * math.pi**2) / _Fymod)
        print('Cc = ', self.Cc)
        # 
        # The allowable axial compressive stress, Fa, should be
        # determined from the following AISC formulas for members
        # with a D/t ratio equal to or less than 60:
        #
        if self.KLr < self.Cc :
            #
            self.Fa = (((1 - (self.KLr**2) / (2*self.Cc**2)) * _Fymod) / 
                       ((5.0/3.0) + (3*self.KLr/(8*self.Cc)) -
                        (self.KLr**3/(8*self.Cc**3))))
            #
            self.Fa_flag = "(3.2.2-1)"
            #print('Fa =', self.Fa, self.Fa_flag)
            #
        #
        else:
        #
            self.Fa =  (12 * self.E * math.pi**2) / (23 * self.KLr **2)
            self.Fa_flag = "(3.2.2-2)"
            #print('Fa =', self.Fa, self.Fa_flag)
            #
        #
        #
        print ('Fa Allowable', self.Fa)
        #        
        # 
        self.UmComp = self.fa / self.Fa 
        self.UmComp_flag = self.Fa_flag 
        print ('IR Compression =', self.UmComp, self.UmComp_flag)
        print (' ')
    # 
    #
    #   3.2.3 Bending
    #
    def Bending(self):
        #
        # The allowable bending stress, Fb, should be determined
        # from:
        #
        #
        if self.Units == 'SI':
            #
            #
            if  self.D/self.t > 300:
                print ("For D/t ratios greater than 300, refer to API Bulletin 2U.")
            #
            elif self.D/self.t <= 10340/self.Fy:
                self.Fb = 0.75*self.Fy
                self.Fb_flag = "(3.2.3-1a)"
            #
            else:
                if self.D/self.t > 20680/self.Fy:
                    self.Fb = (0.72 - 0.58 * (self.Fy*self.D / (self.E * self.t))) * self.Fy
                    self.Fb_flag = "(3.2.3-1c)"
                    #
                else:
                    self.Fb = (0.84 - 1.74 * (self.Fy*self.D / (self.E * self.t))) * self.Fy
                    self.Fb_flag = "(3.2.3-1b)"
            #
        #
        else:
            #
            if  self.D/self.t > 300:
                print ("For D/t ratios greater than 300, refer to API Bulletin 2U.")
            #
            elif self.D/self.t <= 1500/self.Fy:
                self.Fb = 0.75*self.Fy
                self.Fb_flag = "(3.2.3-1a)"
            #
            else:
                if self.D/self.t > 3000/self.Fy:
                    self.Fb = (0.72 - 0.58 * (self.Fy*self.D / (self.E * self.t))) * self.Fy
                    self.Fb_flag = "(3.2.3-1c)"
                    #
                else:
                    self.Fb = (0.84 - 1.74 * (self.Fy*self.D / (self.E * self.t))) * self.Fy
                    self.Fb_flag = "(3.2.3-1b)"
        #
        #
        #
        #
        print ('Fb Allowable', self.Fb)
        # 
        self.UmBendingIP = self.fbx / self.Fb
        self.UmBendingOP = self.fby / self.Fb
        self.UmBending_Flag = self.Fb_flag
        #
        print ('IR Bending In Plane =', self.UmBendingIP)
        print ('IR Bending Out Plane =', self.UmBendingOP)
        print (' ')
        #
    #
    #
    #   3.2.4 Shear
    #
    def Shear(self):
        #
        #   3.2.4.a Beam Shear
        #
        #  where
        # fv = the maximum shear stress, ksi (MPa),
        # V = the transverse shear force, kips (MN),
        # A = the cross sectional area, in.2 (m2).
        #
        # In Plane
        self.fvy = abs(self.Vip / (0.50*self.A))
        # Out Plane
        self.fvx = abs(self.Vop / (0.50*self.A))
        #
        # The allowable beam shear stress, Fv, should be determined
        # from:
        #
        self.Fv = 0.40 * self.Fy
        #
        #
        print ('Fv Allowable', self.Fv)
        #
        # (13.2-17)
        # In Plane
        self.UmShear_IP = self.fvy /  self.Fv 
        # Out Plane
        self.UmShear_OP = self.fvx /  self.Fv 
        #
        self.UmShear = (math.sqrt(self.fvy**2 + self.fvx**2) /  self.Fv )
        #
        self.UmShear_flag = "(3.2.4-2)"
        #
        print ('IR Shear In Plane =', self.UmShear_IP)
        print ('IR Shear Out Plane =', self.UmShear_OP)
        print ('IR Shear =', self.UmShear)
        print (' ')
    #
    #
    #   3.2.4.b Torsional Shear
    #
    def TorsionalShear(self):
        #
        # The maximum torsional shear stress, Fv, for cylindrical
        # members caused by torsion is:
        # (3.2.4-3)
        # where
        # fvt = maximum torsional shear stress, ksi (MPa),
        # Mt = torsional moment, kips-in. (MN-m),
        # Ip = polar moment of inertia, in.4 (m4),
        #
        self.fvt = abs((self.Mvt * self.D)/(2 * self.Ip))
        #
        #
        # and the allowable torsional shear stress, Fvt, should be determined
        # from:
        # (3.2.4-4)
        self.Fvt = 0.40*self.Fy
        #
        print ('Fvt Allowable', self.Fvt)
        #
        self.UmTorsion = self.fvt / self.Fvt
        #
        self.UmTorsion_flag = "(3.2.4-4)"
        #
        print ('IR Torsion =', self.UmTorsion)
        print (' ')
        #
    #
    #
    #   3.2.5 Hydrostatic Pressure (Stiffened and
    #            Unstiffened Cylinders)
    #
    #   3.2.5.a Design Hydrostatic Head
    def HydrostaticPressure(self):
        # 
        # The hydrostatic pressure to be used should be
        # determined from the design head, Hz, defined as follows:
        # (3.2.5-3)
        # where
        # z = depth below still water surface including tide, ft(m). 
        #     z is positive measured downward from the still
        #     water surface. For installation, z should be the 
        #     maximum submergence during the launch or
        #     differential head during the upending sequence, 
        #     plus a reasonable increase in head to account for 
        #     structural weight tolerances and for deviations 
        #     from the planned installation sequence.
        # Hw = wave height, ft(m),
        # k = with L equal to wave length, ft (m),
        # d = still water depth, ft. (m),
        # Rhow = seawater density, 64 lbs/ft3 (0.01005 MN/m3).
        # 
        self.Hz = (self.z + (self.Hw/2)*
                   ((math.cosh(self.k * (self.d - self.z)))/
                    (math.cosh(self.k * self.d))))
        #
        print ('Hz =', self.Hz)
        #
        # (13.2-20)
        self.p = ( self.Rhow  * self.Hz)
        #
        print ('p =',self.p)
        # 
    #
    #
    #  3.2.5.b Hoop Buckling Stress
    def HoopBuckling(self):
        #
        # For tubular platform members satisfying API Spec 2B outof-
        # roundness tolerances, the acting membrane stress, fh, in ksi
        # (MPa), should not exceed the critical hoop buckling stress,
        # Fhc, divided by the appropriate safety factor:
        # where
        # fh = hoop stress due to hydrostatic pressure, ksi (MPa),
        # p = hydrostatic pressure, ksi (MPa),
        # SFh = safety factor against hydrostatic collapse (see
        #          Section 3.3.5).
        #
        # (3.2.5-2)
        self.fh = ((self.p * self.D) / (2*self.t))
        #
        print ('fh =', self.fh)
        #
        #
        # The elastic hoop buckling stress, Fhe, and the critical hoop
        # buckling stress, Fhc, are determined from the following formulas.
        #
        print ('D/t =',(self.D/self.t))
        #
        # 
        # The geometric parameter, M, is defined as:
        # (3.2.5-5)
        # where
        # Lr = length of cylinder between stiffening rings, diaphragms,
        # or end connections, in. (m).
        #
        #
        _LrOptimum = 1.6 * math.sqrt(self.D**3/(2*self.t))
        print ('Lr =',self.Lr, ' <> ', _LrOptimum)
        #
        self.M = ((self.Lr / self.D)*
                   (math.sqrt(2 * self.D / self.t)))
        #
        # Note: For M > 1.6D/t, the elastic buckling stress is approximately
        # equal to that of a long unstiffened cylinder. Thus, stiffening rings, if
        # required, should be spaced such that M < 1.6D/t in order to be beneficial.
        _Mu_min = 1.6 * (self.D / self.t)
        #
        print ('M =', self.M, ' <> ', _Mu_min)
        #
        # The critical hoop buckling coefficient Ch includes the
        # effect of initial geometric imperfections within 
        #  API Spec 2B tolerance limits.
        #
        # 
        if self.M < 1.5:
            self.Ch = 0.80
            print ('Ch =', self.Ch,)
        #
        else:
            # 
            if self.M >= 1.60*(self.D / self.t):
                #
                self.Ch = 0.44 * (self.t/self.D)
                print ('Ch =', self.Ch)
            #
            else:
                # 
                if self.M < 1.5:
                    #
                    self.Ch = 0.80
                    print ('Ch =', self.Ch)
                    #
                # 
                elif self.M < 3.5:
                    #
                    self.Ch = (0.755 * (self.M - 0.559) )
                    print ('Ch =', self.Ch)
                    #
                #
                elif self.M < 0.825*(self.D / self.t):
                    #
                    self.Ch = (0.736 * (self.M - 0.636) )
                    print ('Ch =', self.Ch)
                    #
                #
                else:
                    #
                    self.Ch = (0.44 * (self.t / self.D) +
                               (0.21 * (self.D / self.t)**3 / (self.M**4)))
                    #print (((0.21 * (self.D / self.t)**3 )))
                    print ('Ch =', self.Ch)
        #
        #
        # 1. Elastic Hoop Buckling Stress. The elastic hoop buckling
        #    stress determination is based on a linear stress-strain 
        #    relationship from:
        # (3.2.5-4)
        self.Fhe = (2 * self.Ch * self.E * self.t / self.D)
        #
        print ('fhe =', self.Fhe)
        #
        #
        # 2. Critical Hoop Buckling Stress. The material yield
        #    strength relative to the elastic hoop buckling stress 
        #    determines whether elastic or inelastic hoop buckling 
        #    occurs and the critical hoop buckling stress, Fhc, 
        #    in ksi (MPa) is defined by the appropriate formula.
        #
        # (3.2.5-6)
        #
        # Elastic Buckling
        if self.Fhe <= 0.55 * self.Fy:
            self.Fhc = self.Fhe
        #
        # Inelastic Buckling:
        else:
            # 
            if self.Fhe > 6.2 * self.Fy:
                self.Fhc = self.Fy
            #
            elif self.Fhe > 1.6 * self.Fy:
                self.Fhc = ((1.31 * self.Fy)/(1.15 + (self.Fy/self.Fhe)))
            #
            else:
                self.Fhc =  0.45*self.Fy + 0.18*self.Fhe
        #
        print ('Fhc =', self.Fhc)
        #
        # (13.2-31)
        self.UmHP = self.SFh*self.fh / self.Fhc
        self.UmHP_flag = '(3.2.5-1)'
        #
        print('IR hp =', self.UmHP, self.UmHP_flag)
        print(' ')
        #
    #
    #def RingStiffnerDesign(self):
    #    print ('ok')
    #
    #
    # 3.3 COMBINED STRESSES FOR CYLINDRICAL MEMBERS
    #
    #
    def Combination(self):
        #
        # Sections 3.3.1 and 3.3.2 apply to overall member behavior
        # while Sections 3.3.3 and 3.3.4 apply to local buckling
        #
        #
        # 3.3.2 Combined Axial Tension and Bending
        if self.FaxialFlag == 'TENSION':
            #
            # Cylindrical members subjected to combined tension and
            # bending should be proportioned to satisfy Eq. 3.3.1-2 at all
            # points along their length, where fbx and fby are the computed
            # bending tensile stresses
            # (3.3.1-2)
            self.IRt = (self.fa/(0.60*self.Fy) + (math.sqrt(self.fbx**2 +
                                            self.fby**2))/self.Fb)
            #
            self.IRt_flag = '(3.3.1-2)'
            #
        #
        # 3.3.1 Combined Axial Compression and Bending
        else:
            #
            #3.3.1.a Cylindrical Members
            #
            # When fa/Fa <= 0.15 the following formula may be used 
            # in lieu of the foregoing two formulas
            if self.fa/self.Fa <= 0.15:
                self.IRc = (self.fa / self.Fa + 
                            (math.sqrt(self.fbx**2 +
                                       self.fby**2))/self.Fb)
                #
                self.IRc_flag = '(3.3.1-3)'
            #
            #
            # Cylindrical members subjected to combined compression
            # and flexure should be proportioned to satisfy both the 
            # following requirements at all points along their length
            else:
                #
                self.Fex =  ((12 * self.E * math.pi**2) /
                             (23 * (self.Kx*self.Lx/self.r)**2))
                #
                self.Fey =  ((12 * self.E * math.pi**2) /
                             (23 * (self.Ky*self.Ly/self.r)**2))
                #
                # (3.3.1-1)
                # (3.3.1-4)
                # Eq. 3.3.1-1 assumes that the same values of Cm and Fe' are
                # appropriate for fbx and fby. If different values are applicable,
                # the following formula or other rational analysis should be
                # used instead of Eq. 3.3.1-1:
                _IR1 = (self.fa/self.Fa + 
                        (math.sqrt((self.Cmx * self.fbx / (1 - (self.fa / self.Fex)))**2 + 
                                   (self.Cmx * self.fby / (1 - (self.fa / self.Fey)))**2) /
                         (self.Fb)))
                #
                #
                # (3.3.1-2)
                _IR2 = (self.fa/(0.60*self.Fy) + 
                        (math.sqrt(self.fbx**2 +
                                   self.fby**2))/self.Fb)
                #
                #
                self.IRc = max(_IR1, _IR2)
                #
                if self.IRc == _IR1:
                    self.IRc_flag = '(3.3.1-4)'
                #
                else:
                    self.IRc_flag = '(3.3.1-2)'
                #
                print ('IR =', self.IRc, self.IRc_flag)
                #
                # where the undefined terms used are as defined by the AISC
                # Specification for the Design, Fabrication, and Erection of
                # Structural Steel for Buildings
                #
    #
    #
    #   3.3.3 Axial Tension and Hydrostatic Pressure
    #
    def CombHydro(self):
        #
        # 
        # the term 'A' should reflect the maximum tensile
        # stress combination
        self.A = (((self.fa + math.sqrt( self.fbx**2 + self.fby**2) -
                    (0.50* self.fh)) / self.Fy) * self.SFxt)
        #
        self.B = (self.fh/self.Fhc)*self.SFh
        #
        _AB = self.A**2 + self.B**2 + 2 * self.Poisson * abs(self.A)*self.B
        #
        #
        # 3.3.3 Axial Tension and Hydrostatic Pressure
        if self.FaxialFlag == 'TENSION':
            # When member longitudinal tensile stresses and hoop 
            # compressive stresses (collapse) occur simultaneously, 
            # the following interaction equation should be satisfied:
            #
            self.IRth = _AB
            self.IRth_flag = '(3.3.3-1)'
            print (' ')
            print('IRch =', self.IRth, self.IRth_flag )
            #
        #
        # 3.3.4 Axial Compression and Hydrostatic Pressure
        else:
            # When longitudinal compressive stresses and hoop 
            # compressive stresses occur simultaneously, the 
            # following equations should be satisfied:
            #
            # fx should reflect the maximum
            # compressive stress combination
            _fx = (self.fa + math.sqrt( self.fbx**2 + self.fby**2) +
                   (0.50* self.fh))
            #
            #
            # (3.3.4-1)
            # Eq. 3.3.4-1 should reflect the maximum compressive stress
            # combination.
            _IRch1 = (self.SFxc * ((self.fa + (0.5*self.fh))/self.Fxc) +
                      (self.SFb * math.sqrt( self.fbx**2 + self.fby**2) / self.Fy))
            #
            # (3.3.4-2)
            _IRch2 = self.SFh * self.fh / self.Fhc
            #
            # The following equation should also be satisfied when
            # fx > 0.5Fha
            _IRch3 = 0
            #
            _Faa = self.Fxe/self.SFxc
            #
            _Fha = self.Fhe / self.SFh
            #
            # (3.3.4-3)
            if _fx > 0.5 * _Fha :
                #
                _IRch3 = (((_fx - 0.50 * _Fha) / 
                           (_Faa - 0.50 * _Fha)) + 
                          (self.fh / _Fha)**2)
            #
            # Note: If fb > fa + 0.5 fh, both Eq. 3.3.3-1 and 
            # Eq. 3.3.4-1 must be satisfied.
            _IRch4 = 0
            if math.sqrt( self.fbx**2 + self.fby**2) > self.fa :
                _IRch4 = _AB
            #
            #
            self.IRch = max(_IRch1, _IRch2, _IRch3, _IRch4)
            #
            #
            if self.IRch == _IRch1:
                self.IRch_flag = '(3.3.4-1)'
            #
            elif self.IRch == _IRch2:
                self.IRch_flag = '(3.3.4-2)'
            #
            elif self.IRch == _IRch3:
                self.IRch_flag = '(3.3.4-3)'
            #
            else:
                self.IRch_flag = '(3.3.3-1)'
            #
            #
            print (' ')
            print('IRch =', self.IRch, self.IRch_flag )
            #
    #
    #
    #
    #
    #-------------------------------------------------
    #                   Print Section
    #-------------------------------------------------
    #
    #
    #      
    def PrintSectionProperties(self):
        #  
        OutputFile = open(self.FileOut,'a+')
        # 
        OutputFile.write(" "+"\n")
        OutputFile.write("_______________________________________________________________________________________"+"\n")
        OutputFile.write(" "+"\n")
        OutputFile.write(("Hydrostatic Pressure: "+ "%-3s" +45*" "+" CALCULATION: " +"% 3.0f" +"\n")
                         % (self.HydroCheck, self.Header))
        #
        OutputFile.write("_______________________________________________________________________________________"+"\n")
        OutputFile.write(" "+"\n")
        OutputFile.write("                                     GEOMETRY DATA                                 [mm]"+"\n")
        OutputFile.write(" "+"\n")
        OutputFile.write("Member ID     Component    Diameter    Thickness   D/t         Ly          Lz     "+"\n")
        OutputFile.write(" "+"\n")
        OutputFile.write("......................................................................................."+"\n")
        OutputFile.write(" "+"\n")        
        #
        if self.t < 6.0: _t_flag = 't<6mm FAIL'
        else: _t_flag = 't > 6mm OK'
        #
        if self.Dt > 120.0 : _Dt_flag = 'D/t>120 FAIL'
        else: _Dt_flag = 'D/t<120 OK'
        #
        OutputFile.write(("%-14s" +" "+"%-10s" +"  "+"%-1.4E"+"  "+"%-1.4E" +"  "+"%-1.4E"+"  "+"%-1.4E" +
                          "  "+"%-1.4E" +"\n") % 
                         (self.MemberName, self.Component, self.D, self.t, self.Dt, self.Lx, self.Ly))
        OutputFile.write((39*" " + "%-10s" + "  " + "%-10s" + "\n")%
                         ( _t_flag, _Dt_flag ))
        #
        #
        OutputFile.write("_______________________________________________________________________________________"+"\n")
        OutputFile.write(" "+"\n")
        OutputFile.write("                                  MATERIAL PROPERTIES                            "+"\n")
        OutputFile.write(" "+"\n")
        OutputFile.write("Member ID      Fy [N/mm2]  Fu [N/mm2]  E  [N/mm2]  G  [N/mm2]  Poisson     Rho[kg/m3] "+"\n")
        OutputFile.write(" "+"\n")
        OutputFile.write("......................................................................................."+"\n")
        OutputFile.write(" "+"\n")        
        #
        if self.Fy > 500.0 : _Fy_flag = '> 500 FAIL'
        else: _Fy_flag = '< 500   OK'
        #
        if self.Fu > self.Fy/0.90 : _Fu_flag = '>Fy/0.9 FAIL'
        else: _Fu_flag = '<Fy/0.9 OK'
        #
        OutputFile.write(("%-14s" +" "+ "%-1.4E" +"  "+"%-1.4E"+"  "+"%-1.4E" +"  "+"%-1.4E" +"  "+
                          "%-1.4E" +"  "+"%-1.4E" +"\n") % 
                         (self.MemberName, self.Fy, self.Fu, self.E, self.G, self.Poisson, self.Rhos))
        OutputFile.write((15*" "+"%-10s" +"  "+"%-10s" +"\n")%( _Fy_flag, _Fu_flag ))
        #
        OutputFile.write(" "+"\n")
        OutputFile.write("_______________________________________________________________________________________"+"\n")
        OutputFile.write(" "+"\n")
        OutputFile.write("                              SECTION DERIVED PROPERTIES"+"\n")
        OutputFile.write(" "+"\n")
        OutputFile.write("Member ID      Area[mm^2]  I   [mm^4]  S   [mm^3]  Z   [mm^3]  ShapeFctor  r    [mm]"+"\n")
        #OutputFile.write("Number        Awx  [mm^2]  Iyy [mm^4]  Syy [mm^3]  Zyy [mm^3]  SCeny [mm]  ry   [mm]"+"\n")
        OutputFile.write("               Mass[kg/m]  Ip  [mm^4]  J   [mm^4]"+"\n")
        OutputFile.write("......................................................................................."+"\n")
        OutputFile.write(" "+"\n")
        OutputFile.write(("%-14s" +" "+"%-1.4E"+"  "+"%-1.4E"+"  "+"%-1.4E"+"  "+"%-1.4E"+"  "+"%-1.4E"+"  "+"%-1.4E"+"\n")%
                    (self.MemberName, self.A, self.I, self.S, self.Z , (self.Z/self.S), self.r))
        #OutputFile.write(("               "+"%-1.4E"+"  "+"%-1.4E"+"  "+"%-1.4E"+"  "+"%-1.4E"+"  "+"%-1.4E"+"  "+"%-1.4E"+"\n")%
        #            (self.Awy, self.I, self.S, self.Z, self.r,self.r))
        OutputFile.write((15*" "+"%-1.4E"+ "  " +"%-1.4E"+"  "+"%-1.4E"+"\n")%
                    ( self.mass, self.Ip, self.J))
    #
    #
    def PrintHydroPressureCalc(self):
        OutputFile = open(self.FileOut,'a+')
        #
        if self.M < (1.6*self.D/self.t):
            _MuLimitFlag = '< M'
            _Ring_flag = 'No Required'
        #
        else:
            _MuLimitFlag = '> M'
            _Ring_flag = 'Required'
        #
        #
        _LrLimitit = (1.6 * math.sqrt(self.D**3 / (2*self.t)))
        #
        if self.Lr < _LrLimitit :
            _LrLimititFlag = '< LrM'
            _RingSpacing = 'Lr < 1.6[D^3/2t]^0.5  OK'
        #
        else:
            _LrLimititFlag = '> LrM'
            _RingSpacing = 'Lr > Limit  -->Reduce Lr'
        #
        if self.UmHP > 1.0:
            _UmHP_flag = 'FAIL'
        #
        else:
            _UmHP_flag = 'PASS'
            _Ring_flag = 'No Required'
            _RingSpacing = 'No Required'
        #
        #
        OutputFile.write(" "+"\n")
        OutputFile.write("_______________________________________________________________________________________"+"\n")
        OutputFile.write(" "+"\n")
        OutputFile.write("                           HYDROSTATIC PRESSURE CALCULATIONS"+"\n")
        OutputFile.write(" "+"\n")
        OutputFile.write("Member ID      IR  hp  fh [N/mm2]  Hw    [mm]  p  [N/mm2]  "+"\n")
        OutputFile.write("               Equ IR  Fhc[N/mm2]  T    [sec]  z     [mm]  M  [N/mm2]  [1.6 D/t]"+"\n")
        OutputFile.write("Result                 SFh         d     [mm]  Hz    [mm]  Ch [N/mm2]  Lr   [mm]"+"\n")
        OutputFile.write("RingStiffener?         fh/Fhc/SFh  Lwave [mm]  k           Fhe[N/mm2]  LrM  [mm]"+"\n")
        OutputFile.write("......................................................................................."+"\n")
        OutputFile.write(" "+"\n")
        #
        OutputFile.write(("%-12s" +"  "+"%3.4f" +"   "+ "%-1.4E"+"  "+"%-1.4E"+"  "+"%-1.4E" + "\n")%
                    (self.MemberName, self.UmHP, self.fh, self.Hw, self.p ))
        #
        OutputFile.write((12*" "+ "%6s" +"  "+"%-1.4E"+"  "+"%-1.4E"+"  "+"%-1.4E"+"  "+"%1.4E" + "  " +
                          "%-1.4E"+" "+ "%-14s" +"\n")%
                    ( self.UmHP_flag, self.Fhc,  self.T, self.z, self.M, (1.6*self.D/self.t) , _MuLimitFlag))
        #
        OutputFile.write(("%-12s" + 11*" "+ "%-1.4E"+"  "+"%-1.4E"+"  "+"%-1.4E"+"  "+"%-1.4E"+"  "+"%-1.4E"+ " " + 
                          "%-2s" + "\n")%
                    (_UmHP_flag, self.SFh,  self.d, self.Hz, self.Ch, self.Lr,
                     _LrLimititFlag))
        #
        OutputFile.write(("%-12s" + 11*" "+ "%-1.4E"+"  "+"%-1.4E"+"  "+"%-1.4E"+"  "+"%-1.4E"+ "  " +"%1.4E" +"\n")%
                    ( _Ring_flag, self.UmHP, self.WaveLength, self.k, self.Fhe, _LrLimitit))
        #
    #
    #
    def PrintShear(self):
        OutputFile = open(self.FileOut,'a+')
        OutputFile.write(" "+"\n")
        OutputFile.write("_______________________________________________________________________________________"+"\n")
        OutputFile.write(" "+"\n")
        OutputFile.write("                                 SHEAR CHECK RESULTS"+"\n")
        OutputFile.write(" "+"\n")
        OutputFile.write("Member ID     IR max   Vy     [N]   Vx     [N]   Mt  [N/mm]  "+"\n")
        OutputFile.write("                       fvy[N/mm2]   fvx[N/mm2]   fvt[N/mm2]  "+"\n")
        OutputFile.write("             Equ  Fv   Fv [N/mm2]   Fv [N/mm2]   Fvt[N/mm2]  "+"\n")
        OutputFile.write("Result       Equ Fvt   fvy/Fv       fvx/Fv       fvt/Fvt      "+"\n")
        OutputFile.write("......................................................................................."+"\n")
        OutputFile.write(" "+"\n")
        #
        _UmResult = max(self.UmShear, self.UmTorsion)
        if _UmResult > 1.0:
            _UmResult_flag = 'FAIL'
        else:
            _UmResult_flag = 'PASS'
        #
        OutputFile.write(("%-12s" +"  "+"%3.4f"+"   "+"%1.4E"+"   "+"%1.4E"+"   "+"%1.4E"+ "\n")%
                    (self.MemberName, _UmResult, self.Vip, self.Vop, self.Mvt))
        #
        OutputFile.write((23*" " +"%1.4E"+"   "+"%1.4E"+"   "+"%1.4E"+ "\n")%
                    (  self.fvy, self.fvx, self.fvt))
        #
        OutputFile.write((12*" " + "%6s" +"  "+"%1.4E"+"   "+"%1.4E"+"   "+"%1.4E"+ "\n")%
                    ( self.UmShear_flag, self.Fv, self.Fv, self.Fvt))
        #
        OutputFile.write(("%-11s" + " " + "%6s"+"  "+"%-1.4E"+"   "+"%-1.4E"+"   "+"%-1.4E"+ "\n")%
                    ( _UmResult_flag, self.UmTorsion_flag, self.UmShear_IP, self.UmShear_OP, self.UmTorsion))
    #
    #
    def PrintAxialBendingNoHP(self):
        OutputFile = open(self.FileOut,'a+')
        #
        if self.FaxialFlag == 'COMPRESSION': 
            #
            OutputFile.write(" "+"\n")
            OutputFile.write("_______________________________________________________________________________________"+"\n")
            OutputFile.write(" "+"\n")
            OutputFile.write("                      AXIAL COMPRESSION AND BENDING CHECK RESULTS"+"\n")
            OutputFile.write(" "+"\n")
            OutputFile.write("Member ID     IR Comb  Faxial [N]  Mx   [Nmm]  My   [Nmm]  C           Lx    [mm]  Kx"+"\n")
            OutputFile.write("              Equ IR   fa [N/mm2]  fbx[N/mm2]  fby[N/mm2]  Cc          Ly    [mm]  Ky"+"\n")
            OutputFile.write("              Equ Fa   Fa [N/mm2]  Fb [N/mm2]  Fb [N/mm2]  Fxe[N/mm2]  Fex[N/mm2]  Cmx"+"\n")
            #OutputFile.write("              Equ Umc  GammaRc     GammaRb     GammaRb     fe [N/mm2]  fez[N/mm2]  Cmz"+"\n")
            OutputFile.write("Result        Equ Fb   fa/Fa       fbx/Fb      fby/Fb      Fxc[N/mm2]  Fey[N/mm2]  Cmy"+"\n")                
            OutputFile.write("......................................................................................."+"\n")
            OutputFile.write(" "+"\n")
            #                
            OutputFile.write(("%-12s" +"  "+"%3.4f" +"   "+ "%1.4E" +"  "+ "%1.4E" +"  "+
                         "%1.4E" +"  "+"%1.4E" +"  "+"%1.4E"+"  "+ "%1.2f" +"\n")%
                        (self.MemberName, self.IRc, abs(self.Faxial), abs(self.Mip), abs(self.Mop),
                         self.C, self.Lx, self.Kx))
            #                
            OutputFile.write((12*" "+ "%6s" +"  "+ "%1.4E" +"  "+ "%1.4E" +"  "+
                         "%1.4E" +"  "+"%1.4E" +"  "+"%1.4E" +"  "+ "%1.2f" +"\n")%
                        (self.IRc_flag, abs(self.fa), self.fbx, self.fby,
                         self.Cc, self.Ly, self.Ky))
            #
            #
            if self.IRc > 1.0:
                _UmResult_flag = 'FAIL'
            else:
                _UmResult_flag = 'PASS'
            #
            OutputFile.write((12*" "+ "%6s" +"  "+ "%1.4E" +"  "+ "%1.4E" +"  "+
                         "%1.4E" +"  "+"%1.4E" +"  "+ "%1.4E"  +"  "+ "%1.2f" +"\n")%
                        (self.UmComp_flag, self.Fa, self.Fb, 
                         self.Fb, self.Fxe, self.Fex, self.Cmx))
            #
            #OutputFile.write((13*" "+ "%6s" +"  "+"%1.4E" +"  "+ "%1.4E" +"  "+
            #             "%1.4E" +"  "+"%1.4E" +"  "+"%1.4E"+"  "+"%1.2f" +"\n")%
            #            (self.UmComp_flag, self.GammaRc, self.GammaRb, self.GammaRb,
            #             self.fe, self.fez, self.Cmy))
            #
            OutputFile.write(("%-10s" +" "+ "%6s" +"  "+"%1.4E" +"  "+ "%1.4E" +"  "+
                         "%1.4E" +"  "+"%1.4E" +"  "+ "%1.4E"  +"  "+ "%1.2f" +"\n")%
                        (_UmResult_flag ,self.UmBending_Flag, self.UmComp, self.UmBendingIP, self.UmBendingOP,
                         self.Fxc, self.Fey, self.Cmy))
            #                
            OutputFile.write(" "+"\n")
            #OutputFile.write("_______________________________________________________________________________________"+"\n")
            OutputFile.write(" "+"\n")
            #
        #
        else:
            #
            OutputFile.write(" "+"\n")
            OutputFile.write("_______________________________________________________________________________________"+"\n")
            OutputFile.write(" "+"\n")
            OutputFile.write("                        AXIAL TENSION AND BENDING CHECK RESULTS"+"\n")
            OutputFile.write(" "+"\n")
            OutputFile.write("Member ID    IR Comb   Faxial [N]   Mx   [Nmm]   My   [Nmm]   D/t"+"\n")
            OutputFile.write("             Equ IR    fa [N/mm2]   fbx[N/mm2]   fby[N/mm2]   Lx    [mm]"+"\n")
            OutputFile.write("             Equ Ft    Ft [N/mm2]   Fb [N/mm2]   Fb [N/mm2]   Ly    [mm]"+"\n")
            #OutputFile.write("              Equ Umt  GammaRt     GammaRb     GammaRb                             "+"\n")
            OutputFile.write("Result       Equ Fb    fa/Ft        fbx/Fb       fby/Fb                           "+"\n")                
            OutputFile.write("......................................................................................."+"\n")
            OutputFile.write(" "+"\n")
            #
            OutputFile.write(("%-12s" +"  "+"%3.4f" +"   "+ "%1.4E" +"   "+ "%1.4E" +"   "+
                         "%1.4E" +"   "+ "%1.4E" +"\n")%
                        (self.MemberName, self.IRt, abs(self.Faxial), abs(self.Mip), abs(self.Mop),
                         (self.D/self.t)))
            #                
            OutputFile.write((12*" "+ "%6s" +"  "+ "%1.4E" +"   "+ "%1.4E" +"   "+ "%1.4E" +
                              "   "+ "%1.4E"+"\n")%
                        (  self.IRt_flag, abs(self.fa), self.fbx, self.fby, self.Lx))
            #
            if self.IRt > 1.0:
                _UmResult_flag = 'FAIL'
            #
            else:
                _UmResult_flag = 'PASS'
            #
            OutputFile.write((12*" "+ "%6s" +"  "+ "%1.4E" +"   "+ "%1.4E" +"   "+ "%1.4E" +
                              "   "+ "%1.4E"+"\n")%
                        (self.UmTension_Flag , self.Ft, self.Fb, self.Fb, self.Ly))
            #
            #OutputFile.write((13*" "+ "%6s" +"  "+"%1.4E" +"  "+ "%1.4E" +"  "+
            #             "%1.4E"  +"\n")%
            #            (self.UmTension_Flag, self.GammaRt, self.GammaRb, self.GammaRb))
            #                
            OutputFile.write(("%-10s" +" "+ "%6s" +"  "+"%1.4E" +"   "+ "%1.4E" +"   "+ "%1.4E" +"\n")%
                        (_UmResult_flag , self.UmBending_Flag, self.UmTension, self.UmBendingIP, self.UmBendingOP))
            #                
            OutputFile.write(" "+"\n")
            #OutputFile.write("_______________________________________________________________________________________"+"\n")
            OutputFile.write(" "+"\n")
            #
            #
    #
    #
    #
    def PrintAxialBendingAndHP(self):
        OutputFile = open(self.FileOut,'a+')
        #
        #
        if self.FaxialFlag == 'COMPRESSION': 
            OutputFile.write(" "+"\n")
            OutputFile.write("_______________________________________________________________________________________"+"\n")
            OutputFile.write(" "+"\n")
            OutputFile.write("         AXIAL COMPRESSION AND BENDING WITH HYDROSTATIC PRESSURE CHECK RESULTS"+"\n")
            OutputFile.write(" "+"\n")
            OutputFile.write("Member ID     IR Comb  Faxial [N]  Mx   [Nmm]  My   [Nmm]  C           Lx    [mm]  Kx"+"\n")
            OutputFile.write("              Equ IR   fa [N/mm2]  fbx[N/mm2]  fby[N/mm2]  Cc          Ly    [mm]  Ky"+"\n")
            OutputFile.write("              Equ Fa   Fa [N/mm2]  Fb [N/mm2]  Fb [N/mm2]  Fxe[N/mm2]  SFx         Cmx"+"\n")
            #OutputFile.write("              Equ Umc  GammaRc     GammaRb     GammaRb     fe [N/mm2]  fch[N/mm2]  Cmy"+"\n")
            OutputFile.write("Result        Equ Fb   fa/Fa       fbx/Fb      fby/Fb      Fxc[N/mm2]  SFb         Cmy"+"\n") 
            OutputFile.write("......................................................................................."+"\n")
            OutputFile.write(" "+"\n")
            #                
            OutputFile.write(("%-12s" +"  "+"%3.4f" +"   "+ "%1.4E" +"  "+ "%1.4E" +"  "+
                         "%1.4E" +"  "+"%1.4E" +"  "+"%1.4E"+"  "+ "%1.2f" +"\n")%
                        (self.MemberName, self.IRch, abs(self.Faxial), abs(self.Mip), abs(self.Mop),
                         self.C, self.Lx, self.Kx))
            #                
            OutputFile.write((12*" "+ "%6s" +"  "+ "%1.4E" +"  "+ "%1.4E" +"  "+
                         "%1.4E" +"  "+"%1.4E" +"  "+"%1.4E" +"  "+ "%1.2f" +"\n")%
                        ( self.IRch_flag, abs(self.fa), self.fbx, self.fby,
                         self.Cc, self.Ly, self.Ky))
            #
            #
            if self.IRch > 1.0:
                _UmResult_flag = 'FAIL'
            else:
                _UmResult_flag = 'PASS'
            #
            OutputFile.write(( 12*" "+ "%6s" +"  "+ "%1.4E" +"  "+ "%1.4E" +"  "+
                         "%1.4E" +"  "+"%1.4E" +"  "+"%1.4E" +"  "+ "%1.2f" +"\n")%
                        (self.UmComp_flag, self.Fa, self.Fb, self.Fb, self.Fxe, self.SFxc, self.Cmx))
            #                
            #OutputFile.write((13*" "+ "%6s" +"  "+"%1.4E" +"  "+ "%1.4E" +"  "+
            #             "%1.4E" +"  "+"%1.4E" +"  "+"%1.4E"+"  "+ "%1.2f" +"\n")%
            #            (self.UmComp_flag, self.GammaRc, self.GammaRb, self.GammaRb,
            #             self.fe, self.fch, self.Cmx))
            #
            #                
            OutputFile.write(("%-10s"  + " "+ "%6s" +"  "+"%1.4E" +"  "+ "%1.4E" +"  "+
                         "%1.4E" +"  "+"%1.4E" +"  "+"%1.4E"+"  "+ "%1.2f" +"\n")%
                        (_UmResult_flag, self.UmBending_Flag, self.UmComp, self.UmBendingIP, self.UmBendingOP,
                         self.Fxc, self.SFb, self.Cmy))
            #                
            OutputFile.write(" "+"\n")
            #OutputFile.write("_______________________________________________________________________________________"+"\n")
            OutputFile.write(" "+"\n")
            #
        #
        else:
            #
            OutputFile.write(" "+"\n")
            OutputFile.write("_______________________________________________________________________________________"+"\n")
            OutputFile.write(" "+"\n")
            OutputFile.write("          AXIAL TENSION AND BENDING WITH HYDROSTATIC PRESSURE CHECK RESULTS"+"\n")
            OutputFile.write(" "+"\n")
            OutputFile.write("Member ID     IR Comb  Faxial [N]  My   [Nmm]  Mz   [Nmm]  A           Lx    [mm]"+"\n")
            OutputFile.write("              Equ IR   fa [N/mm2]  fbx[N/mm2]  fby[N/mm2]  B           Ly    [mm]"+"\n")
            OutputFile.write("              Equ Ft   Ft [N/mm2]  Fb [N/mm2]  Fb [N/mm2]  SFx"+"\n")
            #OutputFile.write("              Equ IRt  GammaRt     GammaRb     GammaRb                 fbh[N/mm2]"+"\n")
            OutputFile.write("Result        Equ Fb   fa/Ft       fbx/Fb      fby/Fb      Fhc[N/mm2]"+"\n")                
            OutputFile.write("......................................................................................."+"\n")
            OutputFile.write(" "+"\n")
            #
            OutputFile.write(("%-12s" +"  "+"%3.4f" +"   "+ "%1.4E" +"  "+ "%1.4E" +"  "+
                         "%1.4E" +"  "+"%1.4E" +"  "+"%1.4E" +"\n")%
                        (self.MemberName, self.IRth, abs(self.Faxial), abs(self.Mip), abs(self.Mop),
                         self.A, self.Lx))
            #                
            OutputFile.write((12*" "+ "%6s" +"  "+ "%1.4E" +"  "+ "%1.4E" +"  "+
                         "%1.4E" +"  "+"%1.4E" +"  "+"%1.4E" +"\n")%
                        ( self.IRth_flag, abs(self.fa), self.fbx, self.fby,
                         self.B, self.Ly))
            #
            if self.IRth > 1.0:
                _UmResult_flag = 'FAIL'
            #
            else:
                _UmResult_flag = 'PASS'
            #
            OutputFile.write((12*" "+ "%6s" +"  "+ "%1.4E" +"  "+ "%1.4E" +"  "+
                         "%1.4E" + "  " +"%1.4E" +"\n")%
                        (self.UmTension_Flag, self.Ft, self.Fb, self.Fb, self.SFxt))
            #
            #OutputFile.write((13*" "+ "%6s" +"  "+"%1.4E" +"  "+ "%1.4E" +"  "+
            #             "%1.4E" +14*" "+"%1.4E" +"\n")%
            #            (self.UmTension_Flag, self.GammaRt, self.GammaRb, self.GammaRb,
            #             self.fbh))
            #
            OutputFile.write(("%-10s" +" "+ "%6s" +"  "+"%1.4E" +"  "+ "%1.4E" +"  "+
                         "%1.4E" + "  " +"%1.4E" +"\n")%
                        (_UmResult_flag,  self.UmBending_Flag, self.UmTension, self.UmBendingIP, self.UmBendingOP,
                         self.Fhc))
            #                
            OutputFile.write(" "+"\n")
            #OutputFile.write("_______________________________________________________________________________________"+"\n")
            OutputFile.write(" "+"\n")
            #
            #
    #
    #
#
#
#
#
class APIwsdCodeCheck(APIwsd21ed):
    #
    def __init__(self):
        #
        self.Header = 1
        #
        self.Lx = 0
        self.Ly = 0
        #
        self.Units = 'SI'
        self.UnitsFlag = "OFF"
        #
        # Default Material
        #
        #       [N/mm2]
        self.Fy = 248.0
        #        [N/mm2]
        self.E = 205000.0
        #        [N/mm2]
        self.G = 77200.0
        #        [kg/m3]
        self.Rhos = 7850
        #        (MN/m3)
        self.Rhow = 0.00001005
        #         (ksi)[N/mm2]
        self.Fu = self.Fy/0.75
        # poisson
        self.Poisson = 0.30
        #      [m/s^2]
        self.g = 9.810
        #
        #
        #
        self.C = 0.30
        self.Kx = 1.0
        self.Ky = 1.0    
        #
        # Default output
        self.MemberName = 'N/A'
        self.FileOut = 'APIwsdCodeCheck.out'
        self.MemberNumber = 0
        #
        # Moment Modifiers
        self.Cmx = 1.0
        self.Cmy = 1.0
        #
        # Axial        
        self.Faxial = 0
        # Shear        
        self.Vip = 0
        self.Vop = 0
        # Bending
        self.Mvt = 0
        self.Mip = 0
        self.Mop = 0
        #
        #
        #      Factors
        self.FaxialFlag = 'COMPRESSION'
        #
        #
        #
        # Hydro Check
        #
        self.DesignCondition = 'OPERATING'
        #
        self.HydroCheck = 'OFF'
        #
        #
    #
    #
    def UnitsInput(self, units = 'SI', Length = 'M', Force = 'N', Mass = 'KILOGRAM'):
        #
        self.Units = UnitFinder(units.upper())
        print ('Units = ', self.Units)
        self.Length_Unit = Length.upper()
        self.Force_Unit = Force.upper()
        self.Mass_Unit = Mass.upper()
        #
        self.UnitsFlag = "ON"
        #
        #  ----- Material -----
        if self.Units == 'SI':
            #       [N/mm2]
            self.Fy = 248.0
            #        [N/mm2]
            self.E = 205000.0
            #        [N/mm2]
            self.G = 77200.0
            #        [kg/m3]
            self.Rhos = 7850
            #        (MN/m3)
            self.Rhow = 0.01005
            #         [kg/mm^3]
            #self.Rhow = 0.0000010250
        #
        else:
            #        [ksi]
            self.Fy = 36.0
            #          [ksi]
            self.E = 29000.0
            #        [ksi]
            self.G = 11200.0
            #         [lb/in3]
            self.Rhos = 0.2836
            #         [lb/ft3]
            self.Rhow = 64.0
            #
    #
    #
    def GeneralData(self, MemberID , DesingCond = 'OPERATING', component = 'TUBULAR', NameOut = 'APIwsdCodeCheck.out'):
        #
        #
        self.MemberName = str(MemberID)
        self.DesignCondition = DesingCond.upper()
        self.Component = str(component)
        self.FileOut = str(MemberID) +'_APIwsd.out'
        #
        #
    #
    #
    def Material(self, Fy, Young = 0, fu = 0, Nu = 0.30, G = 0):
# 
        self.Fy = float(Fy)
        self.E = float(Young)
        self.Fu = float(fu)
        self.Poisson = float (Nu)
        self.G = float(G)
        #
    #
    #        
    def SectionData(self, D, Tw):
        #        
        self.D = float(D)
        self.t = float(Tw)
        #
        #
    #
    #
    def EffectiveLength(self, Lx, Ly = 0):
        #       
        self.Lx = float(Lx)  # Beam Efective Length Majoy Axis (Lx)
        self.Ly = float(Ly)  # Beam Efective Length Minor Axis (Ly)
        #  
        #
        if self.Ly == 0:
            self.Ly = self.Lx
        #
        self.Lr = max(self.Lx, self.Ly)
        #
    #
    #
    def StabilityFactors(self, Ky, Kz = 0):
        #
        self.Kx = float(Ky)
        self.Ky = float(Kz)
        #
        if self.Ky == 0:
            self.Ky = self.Kx
    #
    #
    def AppliedLoads(self, Px, Vy, Vz, BMx, BMy, BMz):
        #        
        # Axial        
        self.Faxial = float(Px)
        # Shear        
        self.Vip = float(abs(Vy))
        self.Vop = float(abs(Vz))
        # Bending
        self.Mvt = float(BMx)
        self.Mip = float(BMy)
        self.Mop = float(BMz)   
        #        
        #
    #
    #
    def MomentModifiers(self, CmInPlane, CmOutPlane = 0):
        self.Cmx = CmInPlane
        self.Cmy = CmOutPlane
        #
        if self.Cmy == 0:
            self.Cmy = self.Cmx
    #
    #
    def HydrostaticPressureData(self, Hw, T, d, z, WaveLength = 0, Rhow = 0.00001005, g = 9.810):
        #
        self.Hw = Hw
        self.T = T
        self.d = d
        self.z = z
        #
        self.WaveLength = WaveLength
        self.Rhow = Rhow
        self.g = g
        #
        self.HydroCheck = 'ON'
        #
    #
    #
    def SafetyFactors(self, SFxt = 1.0, SFb = 1.0, SFxc = 1.0, SFh = 1.0):
        #
        #
        self.SFxt = SFxt
        self.SFb = SFb
        self.SFxc = SFxc
        self.SFh = SFh
        self.DesignCondition = 'USER'
        #          
        #
        #
    #
    #
    def PrintResults(self):
        #
        # Fu check
        if self.Fu == 0 :
            self.Fu = self.Fy/0.90
        #
        #
        # Calc Section Properties
        APIwsd21ed.SectionProperties(self)
        self.Dt = self.D/self.t
        #
        #  Flag Tension or Compression
        self.FaxialFlag = 'TENSION'
        #  
        if self.Faxial != 0:
            #
            if (self.Faxial/abs(self.Faxial)) == -1.0:
                #
                self.FaxialFlag = 'COMPRESSION'
                #
        #
        #
        self.fa = abs(self.Faxial/self.A)
        # In Plane
        self.fvy = abs(2*self.Vip/self.A)
        # Out Plane
        self.fvx = abs(2*self.Vop/self.A)
        #
        self.fvt = abs((self.Mvt * self.D)/(2 * self.Ip))
        self.fbx = abs(self.Mip/ self.S)
        self.fby = abs(self.Mop/ self.S)
        #
        #
        if self.Header == 1:
            today = datetime.date.today()
            OutputFile = open(self.FileOut,'w')
            #
            #
            print (' ')
            print ("++++++++++++++++++++++++++++++")
            print ('APIwsd21ed Code Check Results')
            print ("Member : ",self.MemberName)
            print ("Output file: ",self.FileOut)
            #print ("++++++++++++++++++++++++++++++")
            #
            OutputFile.write(" "+"\n")
            OutputFile.write("***************************************************************************************"+"\n")
            OutputFile.write("*                                  CODE CHECK TOOL                                    *"+"\n")
            OutputFile.write("*                            Strength Of Tubular Members                              *"+"\n")
            OutputFile.write("*                              API RP2A-WSD-ED21-Sup2                                 *"+"\n")
            OutputFile.write("*                                  ALPHA Version                             12/08/10 *"+"\n")            
            OutputFile.write("***************************************************************************************"+"\n")
            OutputFile.write(("DATE: "+ "%-8s" + 59*" " + "UNITS [N-mm]"+"\n")%(today))
            #
            OutputFile = open(self.FileOut,'a+')
            #APIwsd21ed.PrintSectionProperties(self)
        #
        # No Hydro Check
        if self.HydroCheck == 'OFF':
            #
            print ("+++++++++++++++++++++++++++++")
            print ("              Calculation: ",self.Header)
            #
            APIwsd21ed.AxialTension(self)
            APIwsd21ed.AxialCompression(self)
            APIwsd21ed.Bending(self)
            APIwsd21ed.Shear(self)
            APIwsd21ed.TorsionalShear(self)
            APIwsd21ed.Combination(self)
            #
            APIwsd21ed.PrintSectionProperties(self)
            APIwsd21ed.PrintShear(self)
            APIwsd21ed.PrintAxialBendingNoHP(self)
            #
            self.Header = self.Header + 1
            #
        #
        # Hydro Check
        else:
            #
            if self.WaveLength == 0:
                Wave = Stoke5()
                Wave.Data(self.Hw/1000., self.T, self.d/1000.)
                #print ('Wave Length =',Wave.WaveLength)
                self.WaveLength = Wave.WaveLength * 1000.0
                print ('Wave Length =',self.WaveLength)
            #
            self.k = (2*math.pi)/self.WaveLength
            #
            print ("+++++++++++++++++++++++++++++")
            print ("              Calculation: ",self.Header)
            #
            APIwsd21ed.AxialTension(self)
            APIwsd21ed.AxialCompression(self)
            APIwsd21ed.Bending(self)
            APIwsd21ed.Shear(self)
            APIwsd21ed.TorsionalShear(self)
            #
            # Determine Safety Factors
            if self.DesignCondition == 'STORM':
                #
                self.SFxt = 1.25
                self.SFb = self.Fy/(1.33*self.Fb)
                self.SFxc = 1.50
                self.SFh = 1.50
            #
            if self.DesignCondition == 'USER':
                pass
            #
            # Operating
            else:
                self.SFxt = 1.67
                self.SFb = self.Fy/self.Fb
                self.SFxc = 2.00
                self.SFh = 2.00    
            #
            # Calc Hydro Pressure
            APIwsd21ed.HydrostaticPressure(self)
            APIwsd21ed.HoopBuckling(self)
            APIwsd21ed.CombHydro(self)
            # Print out Results
            APIwsd21ed.PrintSectionProperties(self)
            APIwsd21ed.PrintShear(self)
            APIwsd21ed.PrintHydroPressureCalc(self)
            APIwsd21ed.PrintAxialBendingAndHP(self) 
            #
            self.Header = self.Header + 1
            #
        #
        #
    #
    #