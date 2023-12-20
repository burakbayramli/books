# Copyright (c) 2021 steelpy
#
# *****************************************
#                 Log History
#
# Bug fixed on equ 13.2-5 & 13.2-6 13/09/10
#
#
# *****************************************
#
#from steelpy.metocean.wave.Stokes5 import Stoke5 
#
import math
import datetime
#
#
class ISO19902:
    #
    def __init__(self):
        pass
    #
    #
    def SectionProperties(self):
        #
        print (' Section Properties ')
        print ('D =', self.D)
        print ('Tw =',self.t)
        print (' ')
        print (' Material Properties ')
        print (' ')
        print ('Fy =',self.Fy)
        print ('Fu =',self.Fu)
        print ('E =',self.E)
        print ('G =',self.G)
        print (' ')
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
        self.Ze = ((math.pi/64.0) * 
                   (self.D**4 - (self.D - 2 * self.t)**4) /
                   (self.D / 2.0))
        # print ('circular Elastic Modulus',self.Zeip)
        #
        #self.Zeop = self.Zeip
        #
        #
        #-------------------------------------------------
        #   Plastic Modulus about Mayor Axis
        #    def Zx(self):
        #
        self.Zp = ((self.D**3 - 
                    (self.D - 2 * self.t)**3) / 6.0 )
        # print ('circular Plastic Modulus',self.Zpip)
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
    #   13.2.2 Axial tension
    def AxialTension(self):
        # 
        # Sigmat is the axial tensile stress due to forces from factored actions
        # ft is the representative axial tensile strength, ft = fy
        # Fy is the representative yield strength, in stress units
        # GammaRt is the partial resistance factor for axial tensile strength, R,t = 1,05
        #
        #self.Sigmat = abs(self.Faxial/self.A)
        self.ft = self.Fy
        #        
        # (13.2-2) The utilization of a member Um
        self.UmTension = self.Sigmat/(self.ft / self.GammaRt)
        self.UmTension_Flag = '(13.2-2)'
        print ('Tension Um = ',self.UmTension)
    #
    #
    #   13.2.3 Axial compression
    def AxialCompression(self):
        #       
        #
        # self.Sigmac = abs(self.Faxial/self.A)
        #
        # (13.2-3) 
        # sigmacAllowable = (self.fc / self.GammaRc)
        # 
        #
        # (13.3-5)
        self.fey = (math.pi**2 * self.E)/(self.Ky*self.Ly/self.r)**2
        #
        # (13.3-6)
        self.fez = (math.pi**2 * self.E)/(self.Kz*self.Lz/self.r)**2
        #
        #
        # 13.2.3.3 Local buckling
        #
        # where
        # fy is the representative yield strength, in stress units;
        # fxe is the representative elastic local buckling strength, in stress units;
        # Cx is the elastic critical buckling coefficient, see below;
        # E is Youngs modulus of elasticity;
        # D is the outside diameter of the member;
        # t is the wall thickness of the member.
        #
        # (13.2-10)
        self.fxe = 2*self.Cx*self.E*(self.t/self.D)
        #
        # (13.2-8)
        if (self.Fy/self.fxe) <= 0.170 :
            self.fyc = self.Fy
        #       
        # (13.2-9)
        else:
            self.fyc = (1.047 - 0.274*(self.Fy/self.fxe))*self.Fy
        #
        #
        # 13.2.3.2 Column Buckling
        #
        # where :
        # fc is the representative axial compressive strength, in stress units;
        # fyc is the representative local buckling strength, 
        #     in stress units, see 13.2.3.3;
        # Lambda is the column slenderness parameter;
        # fe is the smaller of the Euler buckling strengths in the 
        # y- and z-directions, in stress units, see 13.3.3;
        # E is Youngs modulus of elasticity;
        # K is the effective length factor in the y- or z-direction selected so
        #   that K L is the larger of the values in
        #   the y- and z-directions, see 13.5;
        # L is the unbraced length in y- or z-direction;
        # r is the radius of gyration, Gamma = I / A ;
        # I is the line moment of inertia of the cross-section;
        # A is the cross-sectional area.   
        #
        # (13.2-7)
        _feIP = ((self.Ky*self.Ly)/
                    (math.pi*self.r))
        #
        _feOP = ((self.Kz*self.Lz)/
                    (math.pi*self.r))
        #
        self.fe = max(_feIP, _feOP)
        #
        self.Lambda = self.fe * math.sqrt(self.fyc/self.E)
        #
        # (13.2-5)
        if self.Lambda <= 1.34:
            self.fc = (1.0 - 0.278*self.Lambda**2)*self.fyc
        #            
        # (13.2-6)
        else:
            self.fc = (0.90/(self.Lambda**2))*self.fyc
        #        
        # (13.2-4)
        self.UmComp = self.GammaRc * self.Sigmac / self.fc 
        self.UmComp_flag = '(13.2-4)'
        print ('UmCompression =', self.UmComp)
    # 
    #
    #   13.2.4 Bending
    def Bending(self):
        #
        self.My = self.Fy*self.Ze
        #print ('-----> My?', self.My )
        #
        #
        # (13.2-11)
        self.Sigmaby = abs(self.Mip/ self.Ze)
        self.Sigmabz = abs(self.Mop/ self.Ze)
        #        
        #
        # (13.2-13)
        if ((self.Fy*self.D)/(self.E*self.t)) <= 0.0517 :       
            self.fb = (self.Zp/self.Ze)*self.Fy
        #
        else:
            #
            # (13.2-14)
            if ((self.Fy*self.D)/(self.E*self.t)) <= 0.1034 :
                self.fb = ((1.130 - 
                            2.58*((self.Fy * self.D) / 
                                  (self.E*self.t))) * 
                           (self.Zp / self.Ze) * self.Fy)
            #
            # (13.2-15)
            else:
                self.fb = ((0.94 - 
                            0.76*((self.Fy * self.D) / 
                                  (self.E*self.t))) * 
                           (self.Zp / self.Ze) * self.Fy)
        #
        #       
        # (13.2-12)
        self.UmBendingIP = self.Sigmaby/(self.fb/self.GammaRb)
        self.UmBendingOP = self.Sigmabz/(self.fb/self.GammaRb)
        self.UmBending_Flag = '(13.2-12)'
        #
        print ('UmBending In Plane =', self.UmBendingIP)
        print ('UmBending Out Plane =', self.UmBendingOP)
        #
    #
    #
    #   13.2.5 Shear
    def Shear(self):
        #
        #   13.2.5.1 Beam shear
        #
        #
        #
        self.fv = self.Fy/math.sqrt(3)
        #
        # (13.2-16)
        # In Plane
        self.Taub_IP = abs(2*self.Vip/self.A)
        # Out Plane
        self.Taub_OP = abs(2*self.Vop/self.A)
        #
        # (13.2-17)
        # In Plane
        self.UmShear_IP = self.Taub_IP/(self.fv/self.GammaRv)
        # Out Plane
        self.UmShear_OP = self.Taub_OP/(self.fv/self.GammaRv)
        #
        self.UmShear = (math.sqrt(self.Taub_IP**2 + self.Taub_OP**2) / 
                        (self.fv/self.GammaRv))
        #
        self.UmShear_flag = '(13.2-17)'
        #
        print ('UmShear In Plane =', self.UmShear_IP, (self.fv/self.GammaRv))
        print ('UmShear Out Plane =', self.UmShear_OP, (self.fv/self.GammaRv))
        print ('UmShear =', self.UmShear)
    #
    #
    #   13.2.5.2 Torsional shear
    def TorsionalShear(self):
        #
        #
        # (13.2-18)
        self.Taut = abs((self.Mvt * self.D)/(2 * self.Ip))
        #
        # (13.2-19)
        self.UmTorsion = self.Taut/(self.fv/self.GammaRv)
        #
        self.UmTorsion_flag = '(13.2-19)'
        #
        print ('UmTorsion =', self.UmTorsion, (self.fv/self.GammaRv))
        #
    #
    #
    #   13.2.6 Hydrostatic Pressure
    def HydrostaticPressure(self):
        # print ('ok')
        #
        #
        # (13.2-21)
        self.Hz = (-self.z + (self.Hw/2)*
                   ((math.cosh(self.k * (self.d + self.z)))/
                    (math.cosh(self.k * self.d))))
        #
        print ('Hz =', self.Hz)
        #
        # (13.2-20)
        self.p = (self.GammafG1 * self.Rhow * self.g * self.Hz)
        #
        print ('p =',self.p)
        # 
    #
    #
    def HoopBuckling(self):
        #
        _LrOptimum = 1.6 * math.sqrt(self.D**3/(2*self.t))
        print ('Lr =',self.Lr, ' <> ', _LrOptimum)
        #print ()
        print ('D/t =',(self.D/self.t))
        #
        #
        self.Sigmah = ((self.p * self.D) / (2*self.t))
        #
        print ('Sigmah =', self.Sigmah)
        #
        #
        _Mu_min = 1.6 * (self.D / self.t)
        #
        self.Mu = ((self.Lr / self.D)*
                   (math.sqrt(2 * self.D / self.t)))
        #
        print ('Mu =', self.Mu, ' <> ', _Mu_min)
        #
        # (13.2-30)
        if self.Mu < 1.5:
            self.Ch = 0.80
            print ('Ch =', self.Ch,'(13.2-30)')
        #
        else:
            # (13.2-27)
            if self.Mu >= 1.60*(self.D / self.t):
                #
                self.Ch = 0.44 * (self.t/self.D)
                print ('Ch =', self.Ch,'(13.2-27)')
            #
            else:
                # (13.2-29)
                if self.Mu < 0.825*(self.D / self.t):
                    #
                    self.Ch = (0.737 * (self.Mu - 0.579) )
                    print ('Ch =', self.Ch,'(13.2-29)')
                    #
                #
                # (13.2-28)
                else:
                    #
                    self.Ch = (0.44 * (self.t / self.D) +
                               (0.21 * (self.D / self.t)**3 / (self.Mu**4)))
                    #print (((0.21 * (self.D / self.t)**3 )))
                    print ('Ch =', self.Ch,'(13.2-28)')
        #
        #print ('Ch =', _Ch)
        #
        # (13.2-26)
        self.fhe = (2 * self.Ch * self.E * self.t / self.D)
        #
        print ('fhe =', self.fhe)
        #
        # (13.2-23)
        if self.fhe > 2.44*self.Fy:
            self.fh = self.Fy
        #
        else:
            # (13.2-24)
            if self.fhe <= 0.55*self.Fy:
                self.fh = self.fhe
            #
            # (13.2-25)
            else:
                self.fh = min((self.Fy * 0.70 * (self.fhe / self.Fy)**0.40), self.Fy)
        #
        print ('fh =', self.fh)
        #
        # (13.2-31)
        self.UmHP = self.Sigmah / (self.fh / self.GammaRh)
        self.UmHP_flag = '(13.2-31)'
        #
        print ('UmHP =', self.UmHP, self.UmHP_flag)
        #
    #
    #def RingStiffnerDesign(self):
    #    print ('ok')
    #
    #
    #   13.3 Tubular members subjected to combined forces without hydrostatic pressure
    def Combination(self):
        #
        #   13.3.1 General
        #   The following gives requirements for members subjected to combined forces,
        #   which give rise to global and local interactions between axial forces and
        #   bending moments, without hydrostatic pressure. Generally, the secondary 
        #   moments from factored global actions and the associated bending stresses
        #   (P Delta effects) do not need to be considered. However, when the axial 
        #   member force is substantial, or when the component on which the axial force
        #   acts is very flexible, the secondary moments due to P Delta effects from
        #   factored global actions should be taken into account
        #
        #
        # 13.3.2 Axial tension and bending
        if self.FaxialFlag == 'TENSION':
            #
            self.UmT = (self.UmTension +
                   (self.GammaRb *math.sqrt(self.Sigmaby**2 +
                                            self.Sigmabz**2))/self.fb)
            #
            self.UmT_flag = '(13.3.2)'
            #
        #
        # 13.3.3 Axial compression and bending
        else:
            #
            #
            # (13.3-7)
            _Um1 = (self.UmComp + 
                    (self.GammaRb/self.fb)*
                    math.sqrt(((self.Cmy * self.Sigmaby / (1 - (self.Sigmac / self.fey)))**2)+
                              ((self.Cmz * self.Sigmabz / (1 - (self.Sigmac / self.fez)))**2)))
            #
            #
            # (13.3-8)
            _Um2 = (self.UmComp +
                    (self.GammaRb/self.fb)*
                    math.sqrt(self.Sigmaby**2 + self.Sigmabz**2))
            #
            #
            self.UmC = max (_Um1, _Um2)
            #
            if self.UmC == _Um1:
                self.UmC_flag = '(13.3-7)'
            #
            else:
                self.UmC_flag = '(13.3-8)'
            #
            print ('Um =', self.UmC)
            #
    #
    #
    #   13.4 Tubular members subjected to combined forces with hydrostatic pressure
    def HydroCheck(self):
        #
        # (13.4-4)
        self.Sigmaq = 0.50*self.Sigmah
        print ('Sigmaq =' , self.Sigmaq, '(13.4-4)')
        #        
        #
        #
        # For analyses using factored actions that do not include the capped-end actions:
        if self.CappedEnd  == 'OFF':
            #
            # Tension
            if self.FaxialFlag == 'TENSION':
                #
                # (13.4-1)
                if self.Sigmat >= self.Sigmaq:
                    #
                    self.Sigmatc = self.Sigmat - self.Sigmaq
                    #
                    self.SigmaHydroFlag = 'TENSION'
                    #
                    print ('Sigmatc =' , self.Sigmatc, '(13.4-1)' )
                #
                # (13.4-2)
                else:
                    #
                    self.Sigmacc = self.Sigmaq - self.Sigmat
                    #
                    self.SigmaHydroFlag = 'COMPRESSION'
                    #
                    print ('Sigmacc =' , self.Sigmacc, '(13.4-2)' )
                #
            #
            # Compression
            else:
                # (13.4-3)
                self.Sigmacc = self.Sigmac + self.Sigmaq
                #
                self.SigmaHydroFlag = 'COMPRESSION'
                    #
                print ('Sigmacc =' , self.Sigmacc, '(13.4-3)' )
                #
        #
        #  Capped End On
        else:
            #
            if self.FaxialFlag == 'TENSION':
                #
                self.Sigmatc = self.Sigmat
                self.SigmaHydroFlag = 'TENSION'
                #
                # (13.4-5)
                if self.Sigmat < self.Sigmaq:
                    #
                    self.Sigmac = self.Sigmaq - self.Sigmat
                    #
                    self.FaxialFlag = 'COMPRESSION'
                    #
                    print ('Updated Sigmac =' , self.Sigmac, '(13.4-5)' )
                #
            #
            else:
                #
                #print ('sigma c ===>', self.Sigmac)
                self.Sigmacc = self.Sigmac 
                self.SigmaHydroFlag = 'COMPRESSION'
                #
                # (13.4-6)
                if self.Sigmacc > self.Sigmaq:
                    self.Sigmac = self.Sigmacc - self.Sigmaq 
                #print ('---> Capped on', self.Sigmac)
                #
        #
        #   
    #
    #
    def CombHydro(self):
        #
        # A tubular member below the water line is subjected to hydrostatic pressure unless it has been flooded.
        # Flooding is normally only used for a structure's legs in order to assist in upending and placement and for pile
        # installation. Even where members are flooded in the in-place condition, they can be subjected to hydrostatic
        # pressure during launch and installation. The effects of hydrostatic pressure shall be taken into account when
        # conducting member checks, including the axial components of such pressure (i.e. capped-end actions). When
        # conducting an analysis of the axial components of hydrostatic pressure, such action effects can be taken
        # directly into account during the analysis or can be included subsequently. The formulations presented in 13.4
        # allow either approach for accounting for the axial effects of hydrostatic pressure to be used.
        # When checking tubular members subjected to hydrostatic pressure, four checks are required: 
        #
        #
       # print ('Sigmac =>' , self.Sigmac ,'Sigmacc =>' ,self.Sigmacc , self.SigmaHydroFlag) 
        #
        # (13.4-10)
        self.B = min(((self.GammaRh*self.Sigmah)/self.fh),1.0)
        print('B =', self.B, '(13.4-10)' )
        #
        # (13.4-11)
        self.Eta = 5 - 4*(self.fh / self.Fy)
        print('Eta =', self.Eta, '(13.4-11)' )
        #
        # (13.4-9)
        self.fbh = self.fb * (math.sqrt(1.0 + 0.90 * self.B**2 - math.pow( self.B, 2 * self.Eta)) - 0.30 * self.B )
        print('fbh  =', self.fbh , '(13.4-9)' )
        #
        #
        # 13.4.2 Axial tension, bending and hydrostatic pressure
        if self.SigmaHydroFlag == 'TENSION':
            #
            # (13.4-8)
            self.fth = self.Fy * (math.sqrt(1.0 + 0.09 * self.B**2 - 
                                            self.B**(2 * self.Eta)) - 0.30 * self.B )
            #
            # (13.4-12)
            self.UmT_HP = ((self.GammaRt * self.Sigmatc / self.fth) +
                           (self.GammaRb * math.sqrt(self.Sigmaby**2 +
                                                     self.Sigmabz**2)) / self.fbh)
            #
            self.UmT_HP_flag = '(13.4-12)'
            #
            print ('UmT =', self.UmT_HP, self.UmT_HP_flag )
        #
        # 13.4.3 Axial compression, bending and hydrostatic pressure
        else:
            #
            # (13.4-15)
            if self.Lambda <= (1.340 * math.sqrt((1.0 - (2* self.Sigmaq / self.fyc)))):
                #
                self.fch = ((1 / 2.0) * self.fyc *
                            ((1.0 - 0.278 * self.Lambda**2) -
                             (2 * self.Sigmaq / self.fyc) +
                             math.sqrt((1.0 - 0.278 * self.Lambda**2) +
                                       1.12 * self.Lambda**2 * (self.Sigmaq / self.fyc))))
                #
                print ('fch =', self.fch, '(13.4-15)')
                #
            #
            # (13.4-16)
            else:
                #
                self.fch = (0.90/self.Lambda**2)*self.fyc
                #
                print ('fch =', self.fch, '(13.4-16)')
            #
            #
            #
            self.Sigmax = max(self.Sigmaby, self.Sigmabz) + self.Sigmacc
            #
            if (self.Sigmax > 0.50*(self.fhe / self.GammaRh)) and ((self.fxe / self.GammaRc) > 0.50*(self.fhe / self.GammaRh)):
                # 
                # (13.4-21)
                _UmCH3 = ((self.Sigmax - 0.50*(self.fhe / self.GammaRh)) /
                          ((self.fxe / self.GammaRc)- 0.50*(self.fhe / self.GammaRh)))
                #
            #
            #
            else:
                _UmCH3 = 0.0
                #
            #
            print('Um =', _UmCH3, '(13.4-21)')
            #
            # (13.4-19)
            _UmCH1 = ((self.GammaRc * self.Sigmacc / self.fyc) +
                      (self.GammaRb * math.sqrt(self.Sigmaby**2 + self.Sigmabz**2) / self.fbh))
            #
            print('Um =', _UmCH1, '(13.4-19)')
            #
            # (13.4-20)
            if self.Sigmacc > self.Sigmaq:
                #
                _UmCH2 = ((self.GammaRc * self.Sigmac / self.fch) +
                          (self.GammaRb/self.fbh)*
                          math.sqrt(((self.Cmy * self.Sigmaby / (1-(self.Sigmac / self.fey)))**2) +
                                    ((self.Cmz * self.Sigmabz / (1-(self.Sigmac / self.fez)))**2)))
            #
            else:
                _UmCH2 = 0.0
                #
            #
            print('Um =', _UmCH2, '(13.4-20)')
            #
            self.UmC_HP = max(_UmCH1, _UmCH2, _UmCH3)
            #
            if self.UmC_HP == _UmCH1:
                self.UmC_HP_flag ='(13.4-19)'
            #
            elif self.UmC_HP  == _UmCH2:
                self.UmC_HP_flag ='(13.4-20)'
            #
            else:
                self.UmC_HP_flag ='(13.4-21)'
                #
            #
            print('UmC =', self.UmC_HP, self.UmC_HP_flag )
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
        OutputFile.write("Name          Component    Diameter    Thickness   D/t         Ly          Lz     "+"\n")
        OutputFile.write("Number                                                                            "+"\n")
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
                         (self.MemberName, self.Component, self.D, self.t, self.Dt, self.Ly, self.Lz))
        OutputFile.write(("%-14s"+ 25*" " + "%-10s" + "  " + "%-10s" + "\n")%
                         (self.MemberNumber, _t_flag, _Dt_flag ))
        #
        #
        OutputFile.write("_______________________________________________________________________________________"+"\n")
        OutputFile.write(" "+"\n")
        OutputFile.write("                                  MATERIAL PROPERTIES                            "+"\n")
        OutputFile.write(" "+"\n")
        OutputFile.write("Name           Fy [N/mm2]  Fu [N/mm2]  E  [N/mm2]  G  [N/mm2]  Poisson     Rho[kg/m3] "+"\n")
        OutputFile.write("Number                                                                   "+"\n")
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
        OutputFile.write(("%-14s" +" "+"%-10s" +"  "+"%-10s" +"\n")%(self.MemberNumber, _Fy_flag, _Fu_flag ))
        #
        OutputFile.write(" "+"\n")
        OutputFile.write("_______________________________________________________________________________________"+"\n")
        OutputFile.write(" "+"\n")
        OutputFile.write("                              SECTION DERIVED PROPERTIES"+"\n")
        OutputFile.write(" "+"\n")
        OutputFile.write("Name           Area[mm^2]  I   [mm^4]  Ze  [mm^3]  Zp  [mm^3]  ShapeFctor  r    [mm]"+"\n")
        #OutputFile.write("Number        Awx  [mm^2]  Iyy [mm^4]  Syy [mm^3]  Zyy [mm^3]  SCeny [mm]  ry   [mm]"+"\n")
        OutputFile.write("Number         Mass[kg/m]  Ip  [mm^4]  J   [mm^4]"+"\n")
        OutputFile.write("......................................................................................."+"\n")
        OutputFile.write(" "+"\n")
        OutputFile.write(("%-14s" +" "+"%-1.4E"+"  "+"%-1.4E"+"  "+"%-1.4E"+"  "+"%-1.4E"+"  "+"%-1.4E"+"  "+"%-1.4E"+"\n")%
                    (self.MemberName, self.A, self.I, self.Ze, self.Zp , (self.Zp/self.Ze), self.r))
        #OutputFile.write(("               "+"%-1.4E"+"  "+"%-1.4E"+"  "+"%-1.4E"+"  "+"%-1.4E"+"  "+"%-1.4E"+"  "+"%-1.4E"+"\n")%
        #            (self.Awy, self.I, self.Ze, self.Zp, self.r,self.r))
        OutputFile.write(("%-14s" +" "+"%-1.4E"+ "  " +"%-1.4E"+"  "+"%-1.4E"+"\n")%
                    ( self.MemberNumber, self.mass, self.Ip, self.J))
    #
    #
    def PrintHydroPressureCalc(self):
        OutputFile = open(self.FileOut,'a+')
        #
        if self.Mu < (1.6*self.D/self.t):
            _MuLimitFlag = ' > Mu'
            _Ring_flag = 'Ring Stiff No Reduired'
        #
        else:
            _MuLimitFlag = ' < Mu'
            _Ring_flag = 'Ring Stiffener Reduired'
        #
        #
        _LrLimitit = (1.6 * math.sqrt(self.D**3 / (2*self.t)))
        #
        if self.Lr < _LrLimitit :
            _LrLimititFlag = ' < '
            _RingSpacing = 'Lr < 1.6[D^3/2t]^0.5  OK'
        #
        else:
            _LrLimititFlag = ' > '
            _RingSpacing = 'Lr > Limit  -->Reduce Lr'
        #
        if self.UmHP > 1.0:
            _UmHP_flag = 'FAIL'
        #
        else:
            _UmHP_flag = 'PASS'
        #
        #
        OutputFile.write(" "+"\n")
        OutputFile.write("_______________________________________________________________________________________"+"\n")
        OutputFile.write(" "+"\n")
        OutputFile.write("                           HYDROSTATIC PRESSURE CALCULATIONS"+"\n")
        OutputFile.write(" "+"\n")
        OutputFile.write("Name           Sh [N/mm2]  Mu [N/mm2]  Hw    [mm]  GammafG1    [1.6 D/t]  Vs Mu"+"\n")
        OutputFile.write("Number         fh [N/mm2]  Ch [N/mm2]  T    [sec]  z     [mm]  Ring Stiffener?"+"\n")
        OutputFile.write("Result         GammaRh     fhe[N/mm2]  d     [mm]  Hz    [mm]  Lr   [mm]  Vs Limit"+"\n")
        OutputFile.write("               UmHP        p  [N/mm2]  Lambda[mm]  k           Ring Stiff Spaced?"+"\n")
        OutputFile.write("......................................................................................."+"\n")
        OutputFile.write(" "+"\n")
        #
        OutputFile.write(("%-14s" +" "+"%-1.4E"+"  "+"%-1.4E"+"  "+"%-1.4E"+"  "+"%-1.4E"+"  "+"%-1.4E"+ " "+ "%-14s" +"\n")%
                    (self.MemberName, self.Sigmah, self.Mu, self.Hw, self.GammafG1 , (1.6*self.D/self.t) , _MuLimitFlag))
        #
        OutputFile.write(("%-14s" +" "+"%-1.4E"+"  "+"%-1.4E"+"  "+"%-1.4E"+" "+"%1.4E"+ "  " + "%-14s" +"\n")%
                    (self.MemberNumber, self.fh, self.Ch, self.T, self.z, _Ring_flag))
        #
        OutputFile.write(("%-14s" +" "+"%-1.4E"+"  "+"%-1.4E"+"  "+"%-1.4E"+"  "+"%-1.4E"+"  "+"%-1.4E"+ " " + 
                          "%-2s" +"%1.4E" + "\n")%
                    (_UmHP_flag, self.GammaRh, self.fhe, self.d, self.Hz, self.Lr,
                     _LrLimititFlag, _LrLimitit))
        #
        OutputFile.write((15*" "+"%-1.4E"+"  "+"%-1.4E"+"  "+"%-1.4E"+"  "+"%-1.4E"+ "  " + "%-14s"+"\n")%
                    ( self.UmHP, self.p, self.WaveLength, self.k, _RingSpacing))
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
        OutputFile.write("Name          Um max   Vy (IP) [N]  Vz (OP) [N]  Mvt  [N/mm]  fv [N/mm2]"+"\n")
        OutputFile.write("Number       Equ UmV   Taub[N/mm2]  Taub[N/mm2]  Taut[N/mm2]  GammaRv"+"\n")
        #OutputFile.write("Result       Equ UmV   GammaRv      GammaRv      GammaRv       "+"\n")
        OutputFile.write("Result       Equ Umt   UmVy         UmVz         Umt                   "+"\n")
        OutputFile.write("......................................................................................."+"\n")
        OutputFile.write(" "+"\n")
        #
        _UmResult = max(self.UmShear, self.UmTorsion)
        if _UmResult > 1.0:
            _UmResult_flag = 'FAIL'
        else:
            _UmResult_flag = 'PASS'
        #
        OutputFile.write(("%-12s" +"  "+"%3.4f"+"   "+"%1.4E"+"   "+"%1.4E"+"   "+"%1.4E"+"   "+"%1.4E"+ "\n")%
                    (self.MemberName, _UmResult, self.Vip, self.Vop, self.Mvt, self.fv))
        #
        OutputFile.write(("%-11s" +  " " + "%6s" +"  "+"%1.4E"+"   "+"%1.4E"+"   "+"%1.4E"+"   "+"%1.4E"+ "\n")%
                    (self.MemberNumber, self.UmShear_flag, self.Taub_IP, self.Taub_OP, self.Taut, self.GammaRv))
        #
        #OutputFile.write(("%-11s" + " " + "%6s" +"  "+"%1.4E" +"   "+ "%1.4E" +"   "+
        #                 "%1.4E" +"\n")%
        #                (_UmResult_flag, self.UmShear_flag, self.GammaRv, self.GammaRv, self.GammaRv))
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
            OutputFile.write("Name          Um Comb  Faxial [N]  BMy  [Nmm]  BMz  [Nmm]  Cx          Ly    [mm]  Ky"+"\n")
            OutputFile.write("Number        Equ UmC  Sc [N/mm2]  Sby[N/mm2]  Sbz[N/mm2]  Fxe[N/mm2]  Lz    [mm]  Kz"+"\n")
            OutputFile.write("Result                 fc [N/mm2]  fb [N/mm2]  fb [N/mm2]  Fyc[N/mm2]  fey[N/mm2]  Cmy"+"\n")
            OutputFile.write("              Equ Umc  GammaRc     GammaRb     GammaRb     fe [N/mm2]  fez[N/mm2]  Cmz"+"\n")
            OutputFile.write("              Equ Umb  Umc         Umby        Umbz        Lambda                     "+"\n")                
            OutputFile.write("......................................................................................."+"\n")
            OutputFile.write(" "+"\n")
            #                
            OutputFile.write(("%-12s" +"  "+"%3.4f" +"   "+ "%1.4E" +"  "+ "%1.4E" +"  "+
                         "%1.4E" +"  "+"%1.4E" +"  "+"%1.4E"+"  "+ "%1.2f" +"\n")%
                        (self.MemberName, self.UmC, abs(self.Faxial), abs(self.Mip), abs(self.Mop),
                         self.Cx, self.Ly, self.Ky))
            #                
            OutputFile.write(("%-12s" +" "+ "%6s" +"  "+ "%1.4E" +"  "+ "%1.4E" +"  "+
                         "%1.4E" +"  "+"%1.4E" +"  "+"%1.4E" +"  "+ "%1.2f" +"\n")%
                        (self.MemberNumber, self.UmC_flag, abs(self.Sigmac), self.Sigmaby, self.Sigmabz,
                         self.fxe, self.Lz, self.Kz))
            #
            #
            if self.UmC > 1.0:
                _UmResult_flag = 'FAIL'
            else:
                _UmResult_flag = 'PASS'
            #
            OutputFile.write(("%-12s" + 11*" "+ "%1.4E" +"  "+ "%1.4E" +"  "+
                         "%1.4E" +"  "+"%1.4E" +"  "+ "%1.4E"  +"  "+ "%1.2f" "\n")%
                        (_UmResult_flag , self.fc, self.fb, 
                         self.fb, self.fyc, self.fey, self.Cmy))
            #
            OutputFile.write((13*" "+ "%6s" +"  "+"%1.4E" +"  "+ "%1.4E" +"  "+
                         "%1.4E" +"  "+"%1.4E" +"  "+"%1.4E"+"  "+"%1.2f" +"\n")%
                        (self.UmComp_flag, self.GammaRc, self.GammaRb, self.GammaRb,
                         self.fe, self.fez, self.Cmz))
            #
            OutputFile.write((12*" "+ "%6s" +"  "+"%1.4E" +"  "+ "%1.4E" +"  "+
                         "%1.4E" +"  "+"%1.4E" +"\n")%
                        (self.UmBending_Flag, self.UmComp, self.UmBendingIP, self.UmBendingOP,
                         self.Lambda))
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
            OutputFile.write("Name          Um Comb  Faxial [N]  BMy  [Nmm]  BMz  [Nmm]  My   [Nmm]              "+"\n")
            OutputFile.write("Number        Equ UmC  St [N/mm2]  Sby[N/mm2]  Sbz[N/mm2]                          "+"\n")
            OutputFile.write("Result                 ft [N/mm2]  fb [N/mm2]  fb [N/mm2]                          "+"\n")
            OutputFile.write("              Equ Umt  GammaRt     GammaRb     GammaRb                             "+"\n")
            OutputFile.write("              Equ Umb  St/ft/GRt   Sb/fb/GRb   Sb/fb/GRb                           "+"\n")                
            OutputFile.write("......................................................................................."+"\n")
            OutputFile.write(" "+"\n")
            #
            OutputFile.write(("%-12s" +"  "+"%3.4f" +"   "+ "%1.4E" +"  "+ "%1.4E" +"  "+
                         "%1.4E" +"  "+"%1.4E" +"\n")%
                        (self.MemberName, self.UmT, abs(self.Faxial), abs(self.Mip), abs(self.Mop),
                         self.My))
            #                
            OutputFile.write(("%-12s" +" "+ "%6s" +"  "+ "%1.4E" +"  "+ "%1.4E" +"  "+ "%1.4E" +"\n")%
                        (self.MemberNumber, self.UmT_flag, abs(self.Sigmat), self.Sigmaby, self.Sigmabz))
            #
            if self.UmT > 1.0:
                _UmResult_flag = 'FAIL'
            #
            else:
                _UmResult_flag = 'PASS'
            #
            OutputFile.write(("%-12s" + 11*" "+ "%1.4E" +"  "+ "%1.4E" +"  "+ "%1.4E" +"\n")%
                        (_UmResult_flag , self.ft, self.fb, self.fb))
            #
            OutputFile.write((13*" "+ "%6s" +"  "+"%1.4E" +"  "+ "%1.4E" +"  "+
                         "%1.4E"  +"\n")%
                        (self.UmTension_Flag, self.GammaRt, self.GammaRb, self.GammaRb))
            #                
            OutputFile.write((12*" "+ "%6s" +"  "+"%1.4E" +"  "+ "%1.4E" +"  "+ "%1.4E" +"\n")%
                        (self.UmBending_Flag, self.UmTension, self.UmBendingIP, self.UmBendingOP))
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
        if self.CappedEnd == 'OFF':
            _CappedEnd_flag ='No Included'
        #
        else:
            _CappedEnd_flag ='Included'
        #
        if self.SigmaHydroFlag == 'COMPRESSION': 
            OutputFile.write(" "+"\n")
            OutputFile.write("_______________________________________________________________________________________"+"\n")
            OutputFile.write(" "+"\n")
            OutputFile.write("         AXIAL COMPRESSION AND BENDING WITH HYDROSTATIC PRESSURE CHECK RESULTS"+"\n")
            OutputFile.write(" "+"\n")
            OutputFile.write("Name          Um Comb  Faxial [N]  BMy  [Nmm]  BMz  [Nmm]  Cx          Scc[N/mm2]  Ky"+"\n")
            OutputFile.write("Number        Equ UmC  Sc [N/mm2]  Sby[N/mm2]  Sbz[N/mm2]  Fxe[N/mm2]  Sq [N/mm2]  Kz"+"\n")
            OutputFile.write("Result                 fc [N/mm2]  fb [N/mm2]  fb [N/mm2]  Fyc[N/mm2]  Sx [N/mm2]    "+"\n")
            OutputFile.write("              Equ Umc  GammaRc     GammaRb     GammaRb     fe [N/mm2]  fch[N/mm2]  Cmy"+"\n")
            OutputFile.write("Capped-End    Equ Umb  Umc         Umby        Umbz        Lambda      fbh[N/mm2]  Cmz"+"\n") 
            OutputFile.write("......................................................................................."+"\n")
            OutputFile.write(" "+"\n")
            #                
            OutputFile.write(("%-12s" +"  "+"%3.4f" +"   "+ "%1.4E" +"  "+ "%1.4E" +"  "+
                         "%1.4E" +"  "+"%1.4E" +"  "+"%1.4E"+"  "+ "%1.2f" +"\n")%
                        (self.MemberName, self.UmC_HP, abs(self.Faxial), abs(self.Mip), abs(self.Mop),
                         self.Cx, self.Sigmacc, self.Ky))
            #                
            OutputFile.write(("%-11s" +" "+ "%6s" +"  "+ "%1.4E" +"  "+ "%1.4E" +"  "+
                         "%1.4E" +"  "+"%1.4E" +"  "+"%1.4E" +"  "+ "%1.2f" +"\n")%
                        (self.MemberNumber, self.UmC_HP_flag, abs(self.Sigmac), self.Sigmaby, self.Sigmabz,
                         self.fxe, self.Sigmaq, self.Kz))
            #
            #
            if self.UmC_HP > 1.0:
                _UmResult_flag = 'FAIL'
            else:
                _UmResult_flag = 'PASS'
            #
            OutputFile.write(("%-12s" + 11*" "+ "%1.4E" +"  "+ "%1.4E" +"  "+
                         "%1.4E" +"  "+"%1.4E" +"  "+"%1.4E" +"\n")%
                        (_UmResult_flag , self.fc, self.fb, 
                         self.fb, self.fyc, self.Sigmax))
            #                
            OutputFile.write((13*" "+ "%6s" +"  "+"%1.4E" +"  "+ "%1.4E" +"  "+
                         "%1.4E" +"  "+"%1.4E" +"  "+"%1.4E"+"  "+ "%1.2f" +"\n")%
                        (self.UmComp_flag, self.GammaRc, self.GammaRb, self.GammaRb,
                         self.fe, self.fch, self.Cmy))
            #
            #                
            OutputFile.write(("%-11s"  + " "+ "%6s" +"  "+"%1.4E" +"  "+ "%1.4E" +"  "+
                         "%1.4E" +"  "+"%1.4E" +"  "+"%1.4E"+"  "+ "%1.2f" +"\n")%
                        (_CappedEnd_flag, self.UmBending_Flag, self.UmComp, self.UmBendingIP, self.UmBendingOP,
                         self.Lambda, self.fbh, self.Cmz))
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
            OutputFile.write("Name          Um Comb  Faxial [N]  BMy  [Nmm]  BMz  [Nmm]  B           Stc[N/mm2]"+"\n")
            OutputFile.write("Number        Equ UmC  St [N/mm2]  Sby[N/mm2]  Sbz[N/mm2]  Eta         Sq [N/mm2]"+"\n")
            OutputFile.write("Result                 ft [N/mm2]  fb [N/mm2]  fb [N/mm2]              fth[N/mm2]"+"\n")
            OutputFile.write("              Equ Umt  GammaRt     GammaRb     GammaRb                 fbh[N/mm2]"+"\n")
            OutputFile.write("Capped-End    Equ Umb  Umt         Umby        Umbz                              "+"\n")                
            OutputFile.write("......................................................................................."+"\n")
            OutputFile.write(" "+"\n")
            #
            OutputFile.write(("%-12s" +"  "+"%3.4f" +"   "+ "%1.4E" +"  "+ "%1.4E" +"  "+
                         "%1.4E" +"  "+"%1.4E" +"  "+"%1.4E" +"\n")%
                        (self.MemberName, self.UmT_HP, abs(self.Faxial), abs(self.Mip), abs(self.Mop),
                         self.B, self.Sigmatc))
            #                
            OutputFile.write(("%-11s" +" "+ "%6s" +"  "+ "%1.4E" +"  "+ "%1.4E" +"  "+
                         "%1.4E" +"  "+"%1.4E" +"  "+"%1.4E" +"\n")%
                        (self.MemberNumber, self.UmT_HP_flag, abs(self.Sigmat), self.Sigmaby, self.Sigmabz,
                         self.Eta, self.Sigmaq))
            #
            if self.UmT_HP > 1.0:
                _UmResult_flag = 'FAIL'
            #
            else:
                _UmResult_flag = 'PASS'
            #
            OutputFile.write(("%-12s" + 11*" "+ "%1.4E" +"  "+ "%1.4E" +"  "+
                         "%1.4E" + 14*" "+"%1.4E" +"\n")%
                        (_UmResult_flag ,  self.ft, self.fb, 
                         self.fb, self.fth))
            #
            OutputFile.write((13*" "+ "%6s" +"  "+"%1.4E" +"  "+ "%1.4E" +"  "+
                         "%1.4E" +14*" "+"%1.4E" +"\n")%
                        (self.UmTension_Flag, self.GammaRt, self.GammaRb, self.GammaRb,
                         self.fbh))
            #
            OutputFile.write(("%-11s"  +" "+ "%6s" +"  "+"%1.4E" +"  "+ "%1.4E" +"  "+
                         "%1.4E" +14*" "+"%1.4E" +"\n")%
                        (_CappedEnd_flag, self.UmBending_Flag, self.UmTension, self.UmBendingIP, self.UmBendingOP,
                         self.My))
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
class ISOCodeCheck(ISO19902):
    #
    def __init__(self, component = 'TUBULAR'):
        #
        #
        self.Component = str(component)
        self.Header = 1
        #
        self.Ly = 0
        self.Lz = 0
        #
        # Default Material
        #        [N/mm2]
        self.Fu = 0
        #        [N/mm2]
        self.E = 205000.0
        #        [N/mm2]
        self.G = 77200.0
        # poisson
        self.Poisson = 0.30
        #
        #        (kg/m3)
        self.Rhos = 7850
        # 
        self.Ky = 1.0
        self.Kz = 1.0    
        #
        # Default output
        self.MemberName = 'N/A'
        self.FileOut = 'ISOCodeCheck.out'
        self.MemberNumber = 0
        #
        # Moment Modifiers
        self.Cmy = 1.0
        self.Cmz = 1.0
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
        self.FaxialFlag = 'COMPRESSION'
        #
        #      Factors
        # Tension
        self.GammaRt = 1.05
        # Compression
        self.GammaRc = 1.18
        self.Cx = 0.30
        # Bending
        self.GammaRb = 1.05
        # Shear
        self.GammaRv = 1.05
        #
        #
        # Hydro Check
        #
        # Varies, see Table 9.10-1
        self.GammafG1 = 1.30
        self.GammafG2 = 1.30
        #
        self.GammafQ1 = 1.50
        self.GammafQ2 = 1.50
        #
        self.GammafEo = 0
        self.GammafEo = 0
        #
        # Hoop buckling
        self.GammaRh = 1.25
        #
        self.HydroCheck = 'OFF'
        #
        self.CappedEnd = 'OFF'
        #self.CappedEnd = 'ON'
        #
        #         [kg/mm^3]
        self.Rhow = 0.0000010250
        #
        #     [m/s^2]
        self.g = 9.810
        #
    #
    #
    def GeneralData(self, MemberID , CappedEndActions = 'OFF', NameOut = 'ISOCodeCheck.out'):
        #
        self.MemberName = str(MemberID)
        self.CappedEnd = CappedEndActions.upper()
        self.MemberNumber = 0
        self.FileOut = str(MemberID) +'_ISO.out'
    #
    #
    def Material(self, Fy, Young = 205000.0, fu = 0, Nu = 0.30, G = 77200):
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
    #
    def EffectiveLength(self, Ly, Lz = 0):
        #       
        self.Ly = float(Ly)  # Beam Efective Length Majoy Axis (Lx)
        self.Lz = float(Lz)  # Beam Efective Length Minor Axis (Ly)
        #  
        #
        if self.Lz == 0:
            self.Lz = self.Lz
        #
        self.Lr = max(self.Ly, self.Lz)
        #
    #
    #
    #
    def StabilityFactors(self, Ky, Kz = 0):
        #
        self.Ky = float(Ky)
        self.Kz = float(Kz)
        #
        if self.Kz == 0:
            self.Kz = self.Ky
    #
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
        self.Cmy = CmInPlane
        self.Cmz = CmOutPlane
        #
        if self.Cmz == 0:
            self.Cmz = self.Cmy
    #
    #
    def HydrostaticPressureData(self, Hw, T, d, z, WaveLength = 0, Rhow = 0.0000010250, g = 9.810):
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
    def PartialActionFactors(self, GfG1 = 1.30, GfG2 = 1.30, GfQ1 = 1.50, GfQ2 = 1.50, GfEo = 0.0, GfEe = 0.0):
        self.GammafG1 = GfG1
        self.GammafG2 = GfG2
        #
        self.GammafQ1 = GfQ1
        self.GammafQ2 = GfQ2
        #
        self.GammafEo = GfEo
        self.GammafEo = GfEe
    #
    #
    def PartialResistanceFactors(self, GRt= 1.05, GRc = 1.18, GRb = 1.05, GRv = 1.05):
        #      Factors
        # Tension
        self.GammaRt = GRt
        # Compression
        self.GammaRc = GRc
        # Bending
        self.GammaRb = GRb
        # Shear
        self.GammaRv = GRv
        #
    #
    #
    def print_results(self):
        #
        # Fu check
        if self.Fu == 0 :
            self.Fu = self.Fy/0.90
        #
        #
        # Calc Section Properties
        ISO19902.SectionProperties(self)
        self.Dt = self.D/self.t
        #
        #  Flag Tension or Compression     
        if self.Faxial != 0:
            #
            if (self.Faxial/abs(self.Faxial)) == -1.0:
                #
                self.FaxialFlag = 'COMPRESSION'
                #
            #
            else:
                #
                self.FaxialFlag = 'TENSION'
                #
        #        
        else:
            self.FAxial = 'TENSION'
            #
        #
        self.Sigmac = abs(self.Faxial/self.A)
        self.Sigmat = abs(self.Faxial/self.A)
        # In Plane
        self.Taub_IP = abs(2*self.Vip/self.A)
        # Out Plane
        self.Taub_OP = abs(2*self.Vop/self.A)
        #
        self.Taut = abs((self.Mvt * self.D)/(2 * self.Ip))
        self.Sigmaby = abs(self.Mip/ self.Ze)
        self.Sigmabz = abs(self.Mop/ self.Ze)
        #
        #
        if self.Header == 1:
            today = datetime.date.today()
            OutputFile = open(self.FileOut,'w')
            #self.Header = self.Header + 1
            #
            #
            print (' ')
            print ("+++++++++++++++++++++++++++++")
            print (' ISO19902 Code Check Results')
            print ("Member : ",self.MemberName)
            print ("Output file: ",self.FileOut)
            #print ("+++++++++++++++++++++++++++++")
            #
            OutputFile.write(" "+"\n")
            OutputFile.write("***************************************************************************************"+"\n")
            OutputFile.write("*                                  CODE CHECK TOOL                                    *"+"\n")
            OutputFile.write("*                            Strength Of Tubular Members                              *"+"\n")
            OutputFile.write("*                                     ISO90902                                        *"+"\n")
            OutputFile.write("*                                  ALPHA Version                             23/07/10 *"+"\n")            
            OutputFile.write("***************************************************************************************"+"\n")
            OutputFile.write(("DATE: "+ "%-8s" + 59*" " + "UNITS [N-mm]"+"\n")%(today))
            #
            OutputFile = open(self.FileOut,'a+')
            #ISO19902.PrintSectionProperties(self)
        #
        # No Hydro Check
        if self.HydroCheck == 'OFF':
            #
            print ("+++++++++++++++++++++++++++++")
            print ("              Calculation: ",self.Header)
            #
            ISO19902.AxialTension(self)
            ISO19902.AxialCompression(self)
            ISO19902.Bending(self)
            ISO19902.Shear(self)
            ISO19902.TorsionalShear(self)
            ISO19902.Combination(self)
            #
            ISO19902.PrintSectionProperties(self)
            ISO19902.PrintShear(self)
            ISO19902.PrintAxialBendingNoHP(self)
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
            ISO19902.HydrostaticPressure(self)
            ISO19902.HoopBuckling(self)
            ISO19902.HydroCheck(self)
            ISO19902.AxialTension(self)
            ISO19902.AxialCompression(self)
            ISO19902.Bending(self)
            ISO19902.Shear(self)
            ISO19902.TorsionalShear(self)
            ISO19902.CombHydro(self)
            #
            ISO19902.PrintSectionProperties(self)
            ISO19902.PrintHydroPressureCalc(self)
            ISO19902.PrintShear(self)
            ISO19902.PrintAxialBendingAndHP(self) 
            #
            self.Header = self.Header + 1
            #
        #
        #
    #
    #
