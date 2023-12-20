# API-LRFD Code Check
#
#
#from __future__ import print_function
#import sys
import math
import datetime
#
#-------------------------------------------------
#               Chord Forces
#-------------------------------------------------
#
#
#        
#        
#-------------------------------------------------
#               Code Check LRFD
#-------------------------------------------------
#               +++ Geometry +++
#
#     Chord Data
# Fy - The yield strength of the chord member
# Chord Diameter
# Chord thicness
#
#     Brace Data
# Fyb - The yield strength of the brace member
# Brace Diameter
# Brace thicness
#
# Theta Angle
# Gap
#              +++ Joint Type +++
#
#
#-------------------------------------------------
#                    PROCESS
#-------------------------------------------------
#
def checkNumber(number):
    #
    #
    if number%2==0:
        centralnumber = 0
        halfnumber = (number /2)
        #print ("Even Number", centralnumber )
        #
    #
    else:
        centralnumber =  1+ (number -1)/2
        halfnumber =  centralnumber - 1
       # print ("Odd Number", centralnumber)
       #
    #print ( 'halfnumber ', halfnumber )
    #
    return halfnumber, centralnumber
#
#
class Rings:
    #
    def __init__(self, name, number, hw, tw, bf, tf, Ln, Fyr, WP = 0, V = 1.05, tfratio = 15.0):
        #
        self.name = name
        self.number = number
        self.hw = hw
        self.tw = tw
        self.bf = bf
        self.tf = tf
        self.Ln = Ln
        self.WP = WP
        self.tfratio = tfratio
        #
        #
        self.Fyr = Fyr
        #
        self.V = V
        #
        if self.bf > self.tw:
            self.FyrFactored = self.Fyr
        #
        else:
            self.FyrFactored = (self.Fyr*
                                (min(1.0, (115.0 / math.sqrt(self.Fyr))/
                                     (self.hw/self.tw))))
        #
        self.OffsetAtRing = 0.0
        #print ('Ln', Ln)
    #
    #
    def Chord(self, chordname, D, T, Fy):
        #
        self.ChordName = chordname
        self.D = D
        self.T = T
        self.Fy = Fy
    #
    #
    def Brace(self, bracename, d, t, angle, Fyb):
        #
        self.BraceName = bracename
        self.d = d
        self.t = t
        self.theta = angle
        self.Fyb = Fyb
    #
    #
    def EffectiveWidth(self, RingNumber, Lnp):
        #
        # print 'ok'
        self.RingNumber = RingNumber
        self.Lnp = Lnp
        # self.tfratio = 15.0
        #
        _rew1 = (1.10*math.sqrt(self.D*self.T) + self.tw)
        _rew2 = (self.tfratio*self.T)+(self.Ln - self.Lnp)*0.50
        #
        self.RingEffectiveWidth = min( _rew1, _rew2)
        #         
        #
        #
        #print ('RingEffectiveWidth', self.RingEffectiveWidth )        
        #
        #
        #
    #
    #
    def Properties(self):
        #print 'ok'
        #
        _centreLineDiameter = self.d - self.t
        #
        #
        _centreLineIntercept = (((0.50*self.D)/(math.tan(self.theta)))+
                                self.WP)
        #
        #
        _footPrintLength = (_centreLineDiameter/(math.tan(self.theta)))
        #
        #
        _firstIntercept = ((_centreLineIntercept - 0.50*_footPrintLength)+
                           self.WP)
        #
        #
        _lastIntercept = ((_centreLineIntercept + 0.50*_footPrintLength)+
                          self.WP)
        #
        _angle = math.radians(self.theta)
        #
        self.OffsetAtRing = (((self.Ln * math.cos(_angle) -
                               math.sqrt(self.Ln**2 +
                                         (0.50*self.D)**2 -
                                         (0.50*self.d - 0.50*self.t)**2))/
                              (-math.sin(_angle)))+self.WP)
        #
        self.WidthAtRing = math.sqrt(((self.D**2)/4.0) - self.OffsetAtRing**2)
        #
        #
        #print (' ')
        #print ('Geometry' )
        #print ('Ln', self.Ln)
        #print ('RingWidth', self.RingWidth)
        #
        #
        self.RingArea = ((self.RingEffectiveWidth*self.T)+
                     (self.hw*self.tw)+
                     (self.bf*self.tf))
        #
        #print ('RingArea', self.RingArea)
        #
        self.RingCentroidalAxis = (((0.50*self.bf*self.tf**2)+
                                    (self.hw*self.tw)*(self.tf + 0.50*self.hw)+
                                    (self.RingEffectiveWidth*self.T*
                                     (self.tf+self.hw+0.50*self.T)))/self.RingArea)
        #
        #print ('RingCentroidalAxis', self.RingCentroidalAxis)
        #
        self.RingCentroidRadius = (0.50*self.D - self.hw 
                                    - self.tf - self.T 
                                    + self.RingCentroidalAxis)
        #
        #print ('RingCentroidRadius', self.RingCentroidRadius)
        #
        #print (((0.50*self.D-self.T)))
        #print (((0.50*self.D-self.T-self.hw)))
        #print (((0.50*self.D-self.T)/(0.50*self.D-self.T-self.hw)))
        #
        self.RingNeutralAxis = (self.RingArea/
                                ((self.bf*math.log((0.50*self.D-self.T-self.hw)/(0.50*self.D-self.T-self.hw-self.tf)))+
                                (self.tf*math.log((0.50*self.D-self.T)/(0.50*self.D-self.T-self.hw)))+
                                (self.RingEffectiveWidth*math.log(0.50*self.D/(0.50*self.D-self.T)))))
        #
        #print ('RingNeutralAxis', self.RingNeutralAxis)
        #
        self.RingOffset = (self.RingCentroidRadius - self.RingNeutralAxis)
        #
        #print ('RingOffset', self.RingOffset)
        #
        #print ((((self.RingEffectiveWidth*self.T**3)+
        #                (self.tw*self.hw**3) + (self.bf*self.tf**3))/12.0))
        #print ((self.RingEffectiveWidth*self.T*(self.tf+self.hw + 0.50*self.T - self.RingCentroidalAxis)**2))
        #print ((self.hw*self.tw*(0.50*self.hw + self.tf - self.RingCentroidalAxis)**2))
        #print ((self.bf*self.tf*(0.50*self.tf-self.RingCentroidalAxis)**2))
        #
        self.RingIip = (((self.RingEffectiveWidth*self.T**3)+
                        (self.tw*self.hw**3) + (self.bf*self.tf**3))/12.0 +
                        (self.RingEffectiveWidth*self.T*(self.tf+self.hw + 0.50*self.T - self.RingCentroidalAxis)**2)+
                        (self.hw*self.tw*(0.50*self.hw + self.tf - self.RingCentroidalAxis)**2)+
                        (self.bf*self.tf*(0.50*self.tf-self.RingCentroidalAxis)**2))
        #
        #print ('RingIip', self.RingIip)
        #
        self.RingIop = (((self.T*self.RingEffectiveWidth**3)+
                        (self.hw*self.tw**3)+(self.tf*self.bf**3))/12.0)
        #
        #print ('RingIop', self.RingIop)
        #
        self.RingShearArea = (self.hw+self.tf+self.T)*self.tw
        #
        #print ('RingShearArea', self.RingShearArea)
        #
        _ringRadiusChord = 0.50*self.D - self.T
        #
        #print ('ringRadiusChord' , _ringRadiusChord )
        #
        _ringRadiusFlange = _ringRadiusChord - self.hw - self.tf
        #
        #
        self.RingRadiusGyration = math.sqrt(self.RingIip/self.RingArea)
        #
        #print ('RingRadiusGyration', self.RingRadiusGyration)
        #
        self.RingShapeFactor = ((1 + ((3*((0.50*self.D - self.RingNeutralAxis)**2 -
                                          (_ringRadiusChord - self.RingNeutralAxis)**2)*
                                       ((_ringRadiusChord - self.RingNeutralAxis)*(self.T/self.tw - 1.0)))/
                                      (2*(0.50*self.D-self.RingNeutralAxis)**3)))*
                                (4.0*((0.50*self.D-self.RingNeutralAxis)**2)/
                                 (10.0*self.RingRadiusGyration**2)))
        #
        #
        #
        #print ('RingShapeFactor', self.RingShapeFactor)
        #
        #
        #
        #
    #
    #
    def RingShearCapacity(self, LnAverage, RingWidthSum):
        #
        self.LnAverage = LnAverage
        self.RingWidthSum = RingWidthSum
        #
        #
        _shearCapacityPoint = ((self.FyrFactored/math.sqrt(3.0))*(self.hw + self.tf)*
                               (self.tw/self.V))
        #
        #print ('_shearCapacityPoint',_shearCapacityPoint)
        #
        self.RingAxialCapacity = self.RingNumber*2*_shearCapacityPoint
        #
        #print ('RingAxialCapacity', self.RingAxialCapacity)
        #
        self.RingIPBcapacity = (abs(self.LnAverage - self.Ln)*_shearCapacityPoint)
        #
        self.RingOPBcapacity = self.RingWidthSum * self.RingAxialCapacity
        #print ('RingOPBcapacity', self.RingOPBcapacity)
        #
        #
        #print ('----> RingIPBcapacity', self.RingIPBcapacity)
        return self.RingIPBcapacity
        #
    #
    #
    def PrintRingGeometry(self, NameOut):
        #
        self.NameOut = NameOut
        #
        OutputFile = open(self.NameOut,'a+')
        #
        if self.number == 1 :
            OutputFile.write("_______________________________________________________________________________________"+"\n")
            OutputFile.write(" "+"\n")
            OutputFile.write("                                  RING GEOMETRY DATA"+"\n")
            OutputFile.write(" "+"\n") 
            OutputFile.write("Name         hw    [mm]  bf    [mm]  Fy [N/mm2]  tf    [mm]  WP    [mm]  Offset@Ring"+"\n")
            OutputFile.write("Number       tw    [mm]  tf    [mm]  Fu [N/mm2]  Y           L     [mm]  Width@Ring"+"\n")
            #OutputFile.write("Brace                                                  "+"\n")
        #
        OutputFile.write("......................................................................................."+"\n")         
        OutputFile.write(" "+"\n")
        OutputFile.write(("%-12s" +" "+ "%1.4E" + 2*" "+ "%1.4E" + 2*" " + "%1.4E" + 2*" " + "%1.4E" + 2*" " +"%1.4E" + 2*" " +"%1.4E" +"\n")%
                    (self.name,  self.hw, self.bf, self.Fyr, self.tf, self.WP, self.OffsetAtRing))
        #
        OutputFile.write(("%-12s" +" "+ "%1.4E" + 2*" "+ "%1.4E" + 2*" " + "%1.4E" + 2*" " + "%1.4E"  + " " +"% 1.4E" + 2*" " +"%1.4E" + "\n")%
                    (self.number,  self.tw, self.tf, self.FyrFactored, self.V, self.Ln, self.WidthAtRing )) 
        #
        #OutputFile.write(("%-12s" + "\n")%
        #            (self.BraceName ))         
        #
        #OutputFile.write((13*" "+ "%1.4E" +"  "+ "%1.4E" +"  "+ "%-10s" + "  "+"%-10s" +"  "+ "%1.4E" + "\n")%
        #            (self.Fu, self.Fub, self.JointType, self.GeometryCheck, (self.K + self.YT + self.X)*100))         
        #         
    #
    #
    def PrintRingProperties(self, NameOut):
        #
        self.NameOut = NameOut
        #
        OutputFile = open(self.NameOut,'a+')
        #
        if self.number == 1 :
            OutputFile.write(" "+"\n")
            OutputFile.write("_______________________________________________________________________________________"+"\n")
            OutputFile.write(" "+"\n")
            OutputFile.write("                            RING SECTION DERIVED PROPERTIES"+"\n")
            OutputFile.write(" "+"\n")
            OutputFile.write("Name         EffWth[mm]  CAxis [mm]  NAxis [mm]  Iyy  [mm4]  ShrA [mm2]  ShapeFactor"+"\n")
            OutputFile.write("Number       Area [mm2]  CRad  [mm]  h     [mm]  Izz  [mm4]  ry    [mm] "+"\n")
            #OutputFile.write("               Cw  [mm^6]"+"\n")
        #
        OutputFile.write("......................................................................................."+"\n")
        OutputFile.write(" "+"\n")
        OutputFile.write(("%-12s" +" "+"%-1.4E"+"  "+"%-1.4E" +"  "+"%-1.4E"+ "  "+"%-1.4E" +"  "+ "%-1.4E"+"  "+ "%-1.4E"+"\n")%
                    (self.name, self.RingEffectiveWidth, self.RingCentroidalAxis, self.RingNeutralAxis, self.RingIip, self.RingShearArea, self.RingShapeFactor))
        OutputFile.write(("%-12s" +" "+"%-1.4E"+"  "+"%-1.4E" +"  "+ "%-1.4E" +"  "+ "%-1.4E"+ "  "+ "%-1.4E"+"\n")%
                    (self.number, self.RingArea, self.RingCentroidRadius, self.RingOffset, self.RingIop, self.RingRadiusGyration ))
        #OutputFile.write(("               "+"%-1.4E" +"  "+"%-1.4E" +"  "+"%-1.4E"+"\n")%
        #            ( self.RingShapeFactor))
    #
    #
    #
    def PrintRingShearCapacity(self, NameOut, Pd, Mdipb, Mdopb):
        #
        self.NameOut = NameOut
        self.Pd = Pd
        self.Mdipb = Mdipb
        self.Mdopb = Mdopb
        #
        #
        OutputFile = open(self.NameOut,'a+')
        #
#
        self.UjR = (abs(self.Pd/self.RingAxialCapacity) +
                    abs(self.Mdipb/self.RingIPBcapacity) + 
                    abs(self.Mdopb/self.RingOPBcapacity))
        #
        if self.UjR > 1.0:
            _flagUjR ='FAIL'
        else: _flagUjR = 'OK'
        #
        #
        OutputFile.write("_______________________________________________________________________________________"+"\n")
        OutputFile.write(" "+"\n")
        OutputFile.write("                                 RING SHEAR CAPACITY"+"\n")
        OutputFile.write(" "+"\n")        
        OutputFile.write("Joint        Uj          R PB        MB ipb      MB opb      "+"\n")
        OutputFile.write("Chord                    Pd          Md ipb      Md opb      "+"\n")
        OutputFile.write("Brace        Result      PB/Pd       MB/Mdi      MB/Mdo      "+"\n")
        #
        OutputFile.write("......................................................................................."+"\n")
        OutputFile.write(" "+"\n")        
        #        
        OutputFile.write(("%-12s" +" "+ "%3.4f" +"      "+ "%1.4E" +"  "+ "%1.4E" + "  "+"%1.4E" +"  " + "\n")%
                    (self.name, abs(self.UjR), abs(self.RingAxialCapacity), abs(self.RingIPBcapacity), abs(self.RingOPBcapacity)))
        #
        #
        OutputFile.write(("%-12s" +" "+ 10*" " +"  "+ "%1.4E" +"  "+ "%1.4E" + "  "+"%1.4E" + "\n")%
                    (self.ChordName, abs(self.Pd), abs(self.Mdipb), abs(self.Mdopb))) 
        #
        OutputFile.write(("%-12s" +" "+ "%-10s" +"  "+ "%3.4f" +6*" "+ "%3.4f" + 6*" "+"%3.4f" + "\n")%
                    (self.BraceName, _flagUjR, abs(self.Pd/self.RingAxialCapacity), abs(self.Mdipb/self.RingIPBcapacity), abs(self.Mdopb/self.RingOPBcapacity)))
        #
        OutputFile.write("_______________________________________________________________________________________"+"\n")
#
#     
#
#  Joint Geometry Parameters
class API_LRFD:  
    #
    def __init__(self):
        pass
    #
    def cal(self):
#       Geometrical Parameters for
#       Simple tubular connections
        tau = self.t/self.T
        beta = self.d/self.D
        gamma = self.D/(2*self.T)
#
        print ('Joint Parameters')
        print ('tau :',tau )
        print ('beta :',beta)
        print ('gamma :',gamma)
#
#             +++ Section E.1 +++
#
# Minimum Strength Requirement
#E3_1=
#
#             +++ Section E.1 +++
#
#             +++ Section E.2 +++
#
#             +++ Section E.3 +++
#
#   --> E.3-1.1 Strength Check
#
#       Table E.3-1
#
#       Table E.3-2 Values for Qu
#       
#       Qbeta
        if beta<=0.6:
            Qbeta=1.0
            Qbeta_flag = 'beta <= 0.6'
        else:
            Qbeta=0.3/(beta*(1-0.833*beta))
            Qbeta_flag = 'beta > 0.6'
#            
        print ('Qbeta :',Qbeta, Qbeta_flag)
#
#       Gap Factor Qg
        if gamma <= 20:
            Qg=1.8-0.1*(self.gap/self.T)
            if Qg < 1.0 : Qg =1
            Qg_flag ='gamma <= 20'
        else:
            Qg=1.8-4*(self.gap/self.D)
            if Qg < 1.0 : Qg =1
            Qg_flag ='gamma > 20'
#
        print ('Qg :',Qg, Qg_flag)
#
#       Qu
        Qu_tension =((self.K*(3.4+19*beta)*Qg/100)+
                  (self.YT*(3.4+19*beta)/100)+
                  (self.X**(3.4+19*beta)/100))
#
        print ('Qu Tension :',Qu_tension)
#
        Qu_comp = ((self.K*(3.4+19*beta)*Qg/100))
#
#
#        
#
#
#
class  ISO19902:
    #
    def __init__(self):
        pass
        #
        #
    #
    #
    # 14.2.5 Detailing practice
    #
    # -----------------------------------
    # 14.3 Simple circular tubular joints
    # -----------------------------------
    #
    #
    # ---------------------------------
    # 14.3.1 General
    # ---------------------------------
    def GeometryCheck(self):        
        # 
        #        
        # ---------------------------------
        # 14.2.4 Joint classification
        # ---------------------------------
        #
        self.K = self.K / 100.0
        self.YT = self.YT / 100.0
        self.X = self.X / 1000.0
        #
        #
        # 14.2 Design considerations
        #
        # Geometrical parameters for simple tubular joints
        self.beta = self.d/self.D
        self.gamma = self.D/(2*self.T)
        self.tau = self.t/self.T
        self.gapD = self.gap/self.D
        self.gapT = self.gap/self.T
        #
        print ('')
        print ('Joint Parameters')
        print ('beta :',self.beta)
        print ('gamma :',self.gamma)  
        print ('tau :',self.tau)
        # 
        # ---------------------------------
        # 14.2.1 Materials
        # ---------------------------------
        # Chord
        self.Fy = min(self.Fy, 0.80*self.Fu)
        # Brace
        self.Fyb = min(self.Fyb, 0.80*self.Fub)
        #
        # 14.2.2 Design forces and joint flexibility
        #
        # 14.2.3 Minimum strength
        #
        self.gammaRj = 1.05        
        #
        #
        self.GeometryCheck = 'PASS'
        self.GeometryCheckFlag = ' '
        #
        if self.beta < 0.2 or self.beta > 1.0:
            self.GeometryCheck = 'FAIL'
            self.GeometryCheckFlag = (self.GeometryCheckFlag +
                                      '0.2<Beta<1.0')
        #
        #
        if self.gamma < 10.0 or self.beta > 50.0:
            self.GeometryCheck = 'FAIL'
            self.GeometryCheckFlag = (self.GeometryCheckFlag + 
                                      '0.2<Beta<1.0')  
        #
        #
        if self.Fy > 500.0:
            self.GeometryCheck = 'FAIL'
            self.GeometryCheckFlag = (self.GeometryCheckFlag + 
                                      'Fy>500N/mm2')      
        #
        #
        if self.K != 0:
            if self.gap*self.T < -1.0 or self.gap*self.T < 2*self.gamma:
                self.GeometryCheck = 'FAIL'
                self.GeometryCheckFlag = (self.GeometryCheckFlag + 
                                          'K:gT<-1,2G') 
        #
        #
        print ('')
        print ('Geometry Check', self.GeometryCheck)
        print (self.GeometryCheckFlag)
    #
    #
    # ---------------------------------
    # 14.3.3 Strength factor, Qu
    # ---------------------------------
    def StrengthFactor(self):
        #
        print (' ')
        print ('14.3.3 Strength factor, Qu')
        #
        # Qbeta is a geometrical factor defined by
        # (14.3-5)
        if self.beta > 0.60:
            _Qbeta = 0.30/(self.beta*(1-0.833*self.beta))
        # (14.3-6)
        else:
            _Qbeta = 1.0
        #
        # Qg is a gap factor defined by
        # (14.3-7)
        #
        self.phi = (self.t*self.Fyb)/(self.T*self.Fy)
        #
        # 
        try:
            _Qg_143_7 = ((1.90 - 0.70  * (self.gapT**0.5)) / (self.gamma)**0.5)
            _Qg_143_7 = max(1.0, _Qg_143_7)
        #
        except:
            _Qg_143_7 = 1.0
        #
        _Qg_143_8 = 0.13 + 0.65 * self.phi * self.gamma**0.5
        #
        # (14.3-8)
        if self.gapT <= -2.0:
            _Qg = _Qg_143_8
        # (14.3-7)
        else:
            if self.gapT > 2.0:
                _Qg = _Qg_143_7
            # for -2.0 < g/T < +2.0
            else:
                _Qg = (_Qg_143_8 + 
                           (self.gapT + 2.0)*(_Qg_143_7 - _Qg_143_8)/4.0)
                #
            #
        #
        #
        try:
            _PCsign = self.PC/abs(self.PC)
        #
        except:
            _PCsign = 1.0
        #
        # Table 14.3-1 Values for Qu
        # Compression
        if _PCsign == -1.0:
            #
            # K joint Axial Tension & Compression
            _QuAxialK = ((1.9 + 19.0*self.beta)*_Qg*_Qbeta**0.5)*self.K
            #
            # Y joint Axial Compression
            _QuAxialYT = ((1.9 + 19.0*self.beta)*_Qbeta**0.5)*self.YT
            # 
            # X joint Axial Compression
            _QuAxialX = ((2.8 + (12.0 + 0.10*self.gamma)*self.beta)*_Qbeta)* self.X
            #
            #
        # Tension
        else:
            # K joint Axial Tension & Compression
            _QuAxialK = ((1.9 + 19.0*self.beta)*_Qg*_Qbeta**0.5)*self.K            
            #
            # Y joint Axial Tension
            _QuAxialYT = (30.0*self.beta * self.YT)  
            #
            # X joint Axial Tension
            if self.beta > 0.90:
                _QuAxialX = (20.7 + (self.beta-0.90)*(17.0*self.gamma - 220.0))* self.X
            else:
                _QuAxialX = (23.0*self.beta * self.X)
            #
        #
        #
        # K Joint In-Plane Bending
        _QuIPB_K = (4.5*self.beta*self.gamma**0.5) *self.K
        #
        # K Joint Out-of-Plane Bending
        _QuOPB_K = (3.2*self.gamma**(0.5*self.beta**2.0)) *self.K
        #
        #
        # Y Joint In-Plane Bending
        _QuIPB_YT = (4.5*self.beta*self.gamma**0.5) *self.YT
        #
        # Y Joint Out-of-Plane Bending
        _QuOPB_YT = (3.2*self.gamma**(0.5*self.beta**2.0)) *self.YT
        #    
        #
        # X Joint In-Plane Bending
        _QuIPB_X = (4.5*self.beta*self.gamma**0.5) *self.X
        #
        # X Joint Out-of-Plane Bending
        _QuOPB_X = (3.2*self.gamma**(0.5*self.beta**2.0)) *self.X
        #    
        #
        self.Quaxial = _QuAxialK + _QuAxialYT + _QuAxialX
        #
        self.Quipb = _QuIPB_K + _QuIPB_YT + _QuIPB_X
        #
        self.Quopb = _QuOPB_K + _QuOPB_YT + _QuOPB_X
        #
        #
        print ('Qu Axial =', self.Quaxial)
        print ('Qu ipb =', self.Quipb)
        print ('Qu opb =', self.Quopb)
        #
    #
    #
    # ---------------------------------
    # 14.3.4 Chord force factor, Qf
    # ---------------------------------
    def ChordForceFactor(self):        
        #
        print ('')
        print ('14.3.4 Chord force factor')
        # gammaRq is the partial resistance factor for yield strength
        self.gammaRq = 1.05
        #
        # A is the cross-sectional area of the chord or chord can 
        #   at the brace intersection
        _A = (math.pi/4.0)*(self.D**2 - (self.D - 2*self.T)**2)
        print ('Area =',_A)
        #
        # Py is the representative axial strength due to yielding 
        #    of the chord member not taking account of buckling, 
        #    in force units
        _Py = self.Fy * _A
        print ('Py =',_Py/1000.)
        #
        # Mp is the representative plastic moment strength of 
        #    the chord member
        _Mp = self.Fy * (self.D**3 - (self.D - 2*self.T)**3)/6.0
        print ('MP =',_Mp/1000000.)
        #
        #
        # Where alpha is a factor dependent on force pattern
        _alpha_Axial = 0.030
        _alpha_ipb = 0.045
        _alpha_opb = 0.021
        #
        # C1, C2 are the coefficients given in table 14.3-2
        #
        # Y joints for calculating strength against brace axial forces
        _C1_axial_YT = 25.0
        _C2_axial_YT = 11.0
        #
        # X joints for calculating strength against brace axial forces
        _C1_axial_X = 20.0
        _C2_axial_X = 22.0
        #
        # K joints for calculating strength against balanced brace axial forces
        _C1_axial_K = 14.0
        _C2_axial_K = 43.0
        #
        #
        # C1 & C2 Summary
        _C1_axial = ((_C1_axial_YT * self.YT) + 
                     (_C1_axial_X * self.X) + 
                     (_C1_axial_K * self.K))
        print ('C1 axial =',_C1_axial)
        #
        _C2_axial = ((_C2_axial_YT * self.YT) + 
                     (_C2_axial_X * self.X) + 
                     (_C2_axial_K * self.K))
        print ('C2 axial =',_C2_axial)
        #  
        #
        # All joints for calculating strength against brace moments
        _C1_BM = ((self.YT + self.X + self.K) * 25.0)
        _C2_BM = ((self.YT + self.X + self.K) * 43.0)
        #
        #
        # The parameter qA is defined as follows:
        # (14.3-10)
        #
        _qA_axial = (math.sqrt((_C1_axial*(self.PC/_Py)**2)+
                               (_C2_axial*(self.MCipb/_Mp)**2)+
                               (_C2_axial*(self.MCopb/_Mp)**2)))*self.gammaRq
        #
        #
        _qA_ipb = (math.sqrt((_C1_BM*(self.PC/_Py)**2)+
                             (_C2_BM*(self.MCipb/_Mp)**2)+
                             (_C2_BM*(self.MCopb/_Mp)**2)))*self.gammaRq
        #
        #
        _qA_opb = (math.sqrt((_C1_BM*(self.PC/_Py)**2)+
                             (_C2_BM*(self.MCipb/_Mp)**2)+
                             (_C2_BM*(self.MCopb/_Mp)**2)))*self.gammaRq
        #
        #
        # The chord force factor Qf is a factor that accounts for
        # the presence of forces from factored actions in the chord.
        # (14.3-9)
        #
        self.Qfaxial = 1.0 - _alpha_Axial * _qA_axial**2
        #
        self.Qfipb = 1.0 - _alpha_ipb * _qA_ipb**2
        #
        self.Qfopb = 1.0 - _alpha_opb * _qA_opb**2
        #
        #
        print ('Qf axial =',self.Qfaxial)
        print ('Qf ipb =',self.Qfipb)
        print ('Qf opb =',self.Qfopb)
        #
    #
    #
    # ---------------------------------
    # 14.3.2 Basic joint strength
    # ---------------------------------
    def BasicJointStrength(self):        
        #
        print ('')
        print ('14.3.2 Basic joint strength')
        #print 'angle',self.theta, math.radians(self.theta)
        #print 'sin theta', (math.sin(self.theta)), (math.sin(math.radians(self.theta)))
        #
        # Puj is the representative joint axial strength, 
        # in force units
        _Puj = (((self.Fy*self.T**2)/(math.sin(math.radians(self.theta))))*
                self.Quaxial*self.Qfaxial)
        #
        # Muj is the representative joint bending moment strength, 
        # in moment units
        _Mujipb = ((self.d*(self.Fy*self.T**2)/(math.sin(math.radians(self.theta))))*
                   self.Quipb*self.Qfipb)
        #
        _Mujopb = ((self.d*(self.Fy*self.T**2)/(math.sin(math.radians(self.theta))))*
                   self.Quopb*self.Qfopb)
        #
        self.Pd = _Puj / self.gammaRj
        #
        self.Mdipb = _Mujipb / self.gammaRj
        #
        self.Mdopb = _Mujopb / self.gammaRj
        #
        print ('Pd =', self.Pd/1000.0)
        print ('Md ipb =', self.Mdipb/1000000.0)
        print ('Md opb =', self.Mdopb/1000000.0)
    #
    #
    # ---------------------------------
    # 14.3.6 Strength check
    # ---------------------------------
    def StrengthCheck(self): 
        #
        print (' ')
        print ('14.3.6 Strength check')
        print ('PB =',self.PB/1000.0)
        print ('MB ipb =',self.MBipb/1000000.0)
        print ('MB opb =',self.MBopb/1000000.0)
        #
        # Each brace in a joint that is subjected either 
        # to an axial force or a bending moment alone, 
        # or to an axial force combined with bending moments,
        # shall be designed to satisfy the following conditions:
        # (14.3-12)
        self.Uj = (abs(self.PB/self.Pd) + 
                   (self.MBipb/self.Mdipb)**2 + 
                   abs(self.MBopb/self.Mdopb))
        #
        self.UjFlag = '(14.3-12)'
        print ('UR =',self.Uj)
        print (' ')
        #
        # for all joints except those identified as non-critical
        # (14.3-13)
        #
    #
    #
    # ---------------------------------
    # 14.6 Ring stiffened circular 
    #      tubular joints
    # ---------------------------------
    def RingStiffened(self):
        #
        # Primary joints along launch frames are often 
        # strengthened by ring stiffening. Ring stiffening 
        # is also used in some structures to address fatigue
        # requirements or to avoid very thick chord cans. 
        # A.14.6 outlines the salient features of several 
        # common approaches available to design ring-stiffened
        # joints.
        #
        # A.14.6 Ring stiffened circular tubular joints
        #
        # A.14.6 - C
        # The joint forces are assumed to be resisted by a summation
        # of simple joint strength and ultimate strength
        # behaviour of the rings. The residual ultimate ring strength
        # may simply be calculated as being the shear
        # strength of two cross-sections of the ring. Partial resistance
        # factors are applied to both the simple joint
        # and ring strengths.
        #
        # Ring strength
        self.URingAxial = abs(self.PB/self.RingAxialCapacity)
        self.URingIPB = abs(self.MBipb/self.RingBMipCapacity)
        self.URingOPB = abs(self.MBopb/self.RingBMopCapacity)
        self.URing = (abs(self.PB/self.RingAxialCapacity) +
                      (self.MBipb/self.RingBMipCapacity)**2 + 
                      abs(self.MBopb/self.RingBMopCapacity))
        #
        #self.URing_flag = '(14.6)'
        #
        # Cobined strengths (Joint + Ring)
        self.UjRingAxial = abs(self.PB/(self.Pd + self.RingAxialCapacity))
        self.UjRingIPB = abs(self.MBipb/(self.Mdipb + self.RingBMipCapacity))
        self.UjRingOPB = abs(self.MBopb/(self.Mdopb + self.RingBMopCapacity))
        self.UjRing = self.UjRingAxial + self.UjRingIPB + self.UjRingOPB
        #
        #self.UjRing_flag = '(A.14.6-c)'
        #
        #
        # ---------------------------------------
        # Remaining Load on Ring Stiffener Method
        #self.PB_Residual = 0
        #self.MBipb_Residual = 0
        #self.MBopb_Residual = 0
        #
        self.UjResidual = (abs(self.Uj) - 1.0)/abs(self.Uj)
        #print ('====>',self.UjResidual)
        #self.UjResidual = round(self.UjResidual,1)
        #print (self.UjResidual)
        self.PB_Residual = self.UjResidual * self.PB
        self.MBipb_Residual = self.UjResidual * self.MBipb
        self.MBopb_Residual = self.UjResidual * self.MBopb 
        #
        self.URingAxialDiff = abs(self.PB_Residual / self.RingAxialCapacity)
        self.URingIPBDiff = abs(self.MBipb_Residual / self.RingBMipCapacity)
        self.URingOPBDiff = abs(self.MBopb_Residual / self.RingBMopCapacity)
        self.URingDiff = self.URingAxialDiff + self.URingIPBDiff**2 + self.URingOPBDiff 
        #  
        #
        self.UjAxialDiff = self.PB - self.PB_Residual
        self.UjIPBDiff = self.MBipb - self.MBipb_Residual
        self.UjOPBDiff = self.MBopb - self.MBopb_Residual
        self.UjDiff = abs(self.UjAxialDiff/self.Pd) + (self.UjIPBDiff/self.Mdipb)**2 + abs(self.UjOPBDiff/self.Mdopb)
        #
        #
        # ------------------------------------------
        # Section Forces (Perpendicular to Chord)
        _angle = math.radians(self.theta)
        print (' ')
        print ('-----------------')
        print ('angle', math.sin(_angle))
        #
        print ('Additional Axial',self.PB_Residual  )
        self.PerpendicularAxial = self.PB_Residual  * math.sin(_angle) * -1
        print ('PerpendicularAxial', self.PerpendicularAxial)
        self.PerpendicularBMop = self.MBopb_Residual  * math.sin(_angle)
        self.AxialperPoint = self.PerpendicularAxial /(2 * self.RingNumber)
        print ( 'AxialperPoint = ', self.AxialperPoint, self.PerpendicularAxial )
        #
        _halfNumber, _centralNumber = checkNumber(len(self.ring))
        #
        _OutPlanePerRingPoint = []
        _RingPoint =[]
        _InPlanePerRingPoint =[]
        self.LoadAtPoint =[]
        #
        _twMin = self.ring[1].tw
        #
        self.FyrFactoredMin = self.ring[1].FyrFactored
        #
        for i in range(1,len(self.ring)+1):
            #
            try:
                _RingPoint.append( self.ring[i+1].Ln - self.ring[i].Ln)
            #
            #except ValueError:
            #    _RingPoint.append( self.ring[i].Ln)
            #
            except:
                _RingPoint.append( self.ring[i].Ln - self.ring[i-1].Ln)
            #
            #print ( 'L = ',self.ring[i].Ln, _RingPoint[i-1] )
            #
            if self.ring[i].tw < _twMin :
                _twMin  = self.ring[i].tw 
            #
            if self.ring[i].FyrFactored < self.FyrFactoredMin :
                _FryMin  = self.ring[i].FyrFactored
            #
            _InPlanePerRingPoint.append( self.MBipb_Residual / (2*_RingPoint[i-1]))
            #
            _OutPlanePerRingPoint.append( self.PerpendicularBMop*self.ring[i].WidthAtRing / (2*self.RingWidthSum2))
            #
            #print (' ----->',i,  _halfNumber, _centralNumber)
            if i <= _halfNumber:
                #print (' < _halfNumber')
                self.LoadAtPoint.append(self.AxialperPoint + _InPlanePerRingPoint[i-1] - _OutPlanePerRingPoint[i-1])
                self.LoadAtPoint.append(self.AxialperPoint + _InPlanePerRingPoint[i-1] + _OutPlanePerRingPoint[i-1])
                #
            else:
                #
                if i == _centralNumber :
                    #print (' == _centralNumber')
                    self.LoadAtPoint.append(self.AxialperPoint - _OutPlanePerRingPoint[i-1])
                    self.LoadAtPoint.append(self.AxialperPoint + _OutPlanePerRingPoint[i-1])
                #
                else:
                    #print (' > _halfNumber')
                    self.LoadAtPoint.append(self.AxialperPoint - _InPlanePerRingPoint[i-1] - _OutPlanePerRingPoint[i-1])
                    self.LoadAtPoint.append(self.AxialperPoint - _InPlanePerRingPoint[i-1] + _OutPlanePerRingPoint[i-1])
            #
            #
            #print ('InPlanePerRingPoint', _InPlanePerRingPoint[i-1] )
            #print ('OutPlanePerRingPoint', _OutPlanePerRingPoint[i-1] )
            #
        #
        #
        self.MaximumLoadOnRing = []
        _PerpendicularLoad = 0
        print (' ')
        for i in range(0, len(self.LoadAtPoint), 2):
            self.MaximumLoadOnRing.append(max(abs(self.LoadAtPoint[i]),  abs(self.LoadAtPoint[i+1])))
            #print ('self.LoadAtPoint', self.LoadAtPoint[i]/1000.0,  self.LoadAtPoint[i+1]/1000.0)
            _PerpendicularLoad  =_PerpendicularLoad  + self.LoadAtPoint[i] +  self.LoadAtPoint[i+1]
            #
        self.MaxLoad = max(self.MaximumLoadOnRing)
       # print ('MaxLoad',self.MaxLoad)
        #
        for i in range(len(self.MaximumLoadOnRing)):
            print ('MaximumLoadOnRing', i+1, self.MaximumLoadOnRing[i]/1000)
        # print ('WidthAtRing',self.ring[i].WidthAtRing)
        #
        print (' ')
        print ('tw min', _twMin)
        # Brace
        self.BearingWidthOnBrace = _twMin + 2*self.T
        print ('BearingWidthOnBrace', self.BearingWidthOnBrace)
        self.BearingAreaOnBrace = self.BearingWidthOnBrace * self.t
        print ('BearingAreaOnBrace', self.BearingAreaOnBrace)
        self.BearingStressOnBrace = self.MaxLoad / self.BearingAreaOnBrace
        print ('BearingStressOnBrace ', self.BearingStressOnBrace )
        # Web
        self.BearingWidthOnWeb = self.t + 2*self.T
        print ('BearingWidthOnWeb ', self.BearingWidthOnWeb )
        self.BearingAreaOnWeb = self.t * self.BearingWidthOnWeb
        print ('BearingAreaOnWeb ', self.BearingAreaOnWeb )
        self.BearingStressOnWeb = self.MaxLoad / self.BearingAreaOnWeb 
        print ('BearingStressOnWeb', self.BearingStressOnWeb)
        self.UCsf = (self.BearingStressOnBrace + self.BearingStressOnWeb)/self.FyrFactoredMin
        print ('Unity Check against Yield',  self.UCsf )
        #
        print (' ')
        print ('Cross Check on Loads')
        print ('PerpendicularLoad', _PerpendicularLoad)
        _CheckAxialLoad = _PerpendicularLoad /math.sin(_angle)
        print ('CheckAxialLoad', _CheckAxialLoad)
        #
    #
    #
    def PrintJointGeometry(self):
        OutputFile = open(self.NameOut,'a+')
        #
        OutputFile.write("_______________________________________________________________________________________"+"\n")
        OutputFile.write(" "+"\n")
        OutputFile.write("                                  JOINT GEOMETRY DATA"+"\n")
        OutputFile.write(" "+"\n") 
        OutputFile.write("GeomCheck    D     [mm]  d     [mm]  Gap   [mm]   Beta        K  factor  Joint ID"+"\n")
        OutputFile.write("             T     [mm]  t     [mm]  Theta[deg]   Gamma       YT factor  Chord ID"+"\n")
        OutputFile.write("FactorChk    Fy [N/mm2]  Fyb[N/mm2]  CrtcalJnt?   Tau         X  factor  Brace ID"+"\n")
        OutputFile.write("             Fu [N/mm2]  Fub[N/mm2]  JointType                SumFactor  "+"\n")
        OutputFile.write("......................................................................................."+"\n")         
        OutputFile.write(" "+"\n")
        OutputFile.write(("%-12s" +" "+ "%1.4E" +"  "+ "%1.4E" +" "+ "% 1.4E" + "  "+"%1.4E" +"  "+ "%1.4E" +
                     "  "+ "%-12s" +"\n")%
                    (self.GeometryCheck,  self.D, self.d, self.gap, self.beta, self.K*100, self.JointName, ))
        #
        OutputFile.write(("%-12s" +" "+ "%1.4E" +"  "+ "%1.4E" +"  "+ "%3.4f" + 5*" "+"%1.4E" +"  "+ "%1.4E" +
                     "  "+ "%-12s"  "\n")%
                    (self.GeometryCheckFlag,  self.T, self.t, self.theta, self.gamma, self.YT*100, self.ChordName,)) 
        #
        _sumFactor_check = "OK"
        _sumFactor_flag = " "
        #
        if (self.K + self.YT + self.X) > 1.0:
            _sumFactor_check = "FAIL"
            _sumFactor_flag = "> 100%"
        #
        elif (self.K + self.YT + self.X) < 1.0:
            _sumFactor_check = "FAIL"
            _sumFactor_flag = "< 100%"
        #
        OutputFile.write(("%-12s" +" "+  "%1.4E" +"  "+ "%1.4E" +"  "+ "%-10s" + "  "+"%1.4E" +"  "+ "%1.4E" +
                     "  "+ "%-12s"  "\n")%
                    (_sumFactor_check, self.Fy, self.Fyb, self.CriticalJoint, self.tau, self.X*100,
                     self.BraceName,))         
        #
        OutputFile.write(("%-12s" +" "+ "%1.4E" +"  "+ "%1.4E" +"  "+ "%-10s" + 14*" "+ "%1.4E" + "\n")%
                    (_sumFactor_flag, self.Fu, self.Fub, self.JointType, (self.K + self.YT + self.X)*100))         
        #         
    #
    #
    def PrintResultsSimple(self):
        OutputFile = open(self.NameOut,'a+')
        #
        #
        OutputFile.write("_______________________________________________________________________________________"+"\n")
        OutputFile.write(" "+"\n")
        OutputFile.write(" "+"\n")
        OutputFile.write("                                 JOINT CHECK RESULTS"+"\n")
        OutputFile.write(" "+"\n")
        OutputFile.write(("Joint: "+"%-12s" +" Chord: "+ "%-12s" +" Brace: "+ "%-12s" +"\n")% 
                    (self.JointName, self.ChordName, self.BraceName))     
        OutputFile.write("......................................................................................."+"\n")        
        OutputFile.write("                                                                           Simple Joint"+"\n")
        OutputFile.write(" "+"\n")        
        OutputFile.write("StregthChck  Uj          PB     [N]  MBipb[Nmm]  MBopb[Nmm]  Qf ax       Qu ax"+"\n")
        OutputFile.write("             Equ         Pd     [N]  Mdipb[Nmm]  Mdopb[Nmm]  Qf ipb      Qu ipb"+"\n")
        OutputFile.write("                         PB/Pd       MB/Mdi      MB/Mdo      Qf opb      Qu opb"+"\n")
        #OutputFile.write("JntType      Result      PB/Pd       MB/Mdi      MB/Mdo      Qf opb      Qu opb "+"\n")
        OutputFile.write("......................................................................................."+"\n")
        OutputFile.write(" "+"\n")        
        #  
        _flagUj = 'PASS'
        if abs(self.Uj) > 1 : _flagUj = 'FAIL'
        #
        OutputFile.write(("%-12s" +" "+ "%3.4f" +"      "+ "%1.4E" +"  "+ "%1.4E" + "  "+"%1.4E" +"  "+ "%1.4E" +"  "+ "%1.4E" + "\n")%
                    (_flagUj, abs(self.Uj), abs(self.PB), abs(self.MBipb), abs(self.MBopb), self.Qfaxial,
                     self.Quaxial))
        #
        OutputFile.write((13*" "+ "%-10s" +"  "+ "%1.4E" +"  "+ "%1.4E" + "  "+"%1.4E" +"  "+ "%1.4E" +"  "+ "%1.4E" + "\n")%
                    ( self.UjFlag , abs(self.Pd), abs(self.Mdipb), abs(self.Mdopb), self.Qfipb,
                     self.Quipb)) 
        #
        OutputFile.write(( 25*" "+ "%3.4f" +6*" "+ "%3.4f" + 6*" "+"%3.4f" + 6*" "+ "%1.4E" +"  "+ "%1.4E" + "\n")%
                    ( abs(self.PB/self.Pd), abs(self.MBipb/self.Mdipb), abs(self.MBopb/self.Mdopb), self.Qfipb,
                     self.Quipb))
        #
        OutputFile.write("_______________________________________________________________________________________"+"\n")
    #
    #
    #
    def PrintResultsRing(self):
        OutputFile = open(self.NameOut,'a+')
        #
        #
        #OutputFile.write("_______________________________________________________________________________________"+"\n")
        OutputFile.write(" "+"\n")
        OutputFile.write(" "+"\n")
        OutputFile.write("                      RING STIFFENED CIRCULAR JOINT CHECK RESULTS"+"\n")
        OutputFile.write(" "+"\n")
        OutputFile.write(("Joint: "+"%-12s" +" Chord: "+ "%-12s" +" Brace: "+ "%-12s" +"\n")% 
                    (self.JointName, self.ChordName, self.BraceName)) 
        OutputFile.write("......................................................................................."+"\n")
        OutputFile.write("           Summation of simple joint strength and ultimate strength of the rings Method"+"\n")
        OutputFile.write(" "+"\n")                
        OutputFile.write("StregthChck  Ujoint      Pd     [N]  Mdipb[Nmm]  Mdopb[Nmm]   PB/Pd    MB/MdIP  MB/MdOP"+"\n")        
        OutputFile.write("(A.14.6-a)   Uring       Pr     [N]  Mripb[Nmm]  Mropb[Nmm]   PB/Pr    MB/MrIP  MB/MrOP"+"\n")
        OutputFile.write("                         ----------  ----------  ----------   ------   -------  -------"+"\n")
        OutputFile.write("(A.14.6-c)   Ujrsum      Pc = Pd+Pr  McIP=Md+Mr  McOP=Md+Mr   PB/Pc    MB/McIP  MB/McOP"+"\n")
        #
        OutputFile.write("......................................................................................."+"\n")
        OutputFile.write(" "+"\n")        
        #        
        #OutputFile.write(("%-12s" +" "+  "%-10s" +"  "+ "%1.4E" +"  "+ "%1.4E" + "  "+"%1.4E" +"  "+ "%1.4E" +"  "+ "%1.4E" + "\n")%
        #            (self.JointName, _FlagUt , abs(self.PB), abs(self.MBipb), abs(self.MBopb), self.Qfaxial,
        #             self.Quaxial))
        #
        _flagUj = 'PASS'
        if abs(self.Uj) > 1 : _flagUj = 'FAIL'
        #
        _flagUr ='PASS'
        if self.URing > 1.0: _flagUr ='FAIL'
        #
        _flagUjR ='PASS'
        if self.UjRing > 1.0: _flagUjR ='FAIL'
        #
        #OutputFile.write(("%-12s" +" "+ "%3.4f"  +6*" "+ "%1.4E" +"  "+ "%1.4E" + "  "+"%1.4E" +"  "+ "%1.4E" +"  "+ "%1.4E" + "\n")%
        #            (self.UjFlag, abs(self.Uj) , abs(self.Pd), abs(self.Mdipb), abs(self.Mdopb), self.Qfipb,
        #             self.Quipb)) 
        #        
        OutputFile.write(("%-12s" +" "+  "%3.4f"  + 6*" "+ "%1.4E" +"  "+ "%1.4E" + "  "+"%1.4E" +
                     3*" "+ "%3.4f" + 3*" "+ "%3.4f" + 3*" "+"%3.4f"  + "\n")%
                    ( _flagUj, abs(self.Uj), abs(self.Pd), abs(self.Mdipb), abs(self.Mdopb),
                     abs(self.PB/self.Pd), abs(self.MBipb/self.Mdipb), abs(self.MBopb/self.Mdopb)))
        #                      
        #
        OutputFile.write(("%-12s" +" "+ "%3.4f" + 5*" "+ "% 1.4E" +" "+ "% 1.4E" + " "+"% 1.4E" +
                     3*" "+ "%3.4f" + 3*" "+ "%3.4f" + 3*" "+"%3.4f"  + "\n")%
                    (_flagUr, self.URing, abs(self.RingAxialCapacity), abs(self.RingBMipCapacity), 
                     abs(self.RingBMopCapacity), self.URingAxial, self.URingIPB, self.URingOPB))
        #
        #
        OutputFile.write("                         ----------  ----------  ----------   ------   ------   ------"+"\n")  
        #
        #
        OutputFile.write(("%-12s" +" "+ "%3.4f" + 5*" "+ "% 1.4E" +" "+ "% 1.4E" + " "+"% 1.4E" +
                     3*" "+ "%3.4f" + 3*" "+ "%3.4f" + 3*" "+"%3.4f"  + "\n")%
                    (_flagUjR, self.UjRing, (self.Pd + self.RingAxialCapacity), (self.Mdipb + self.RingBMipCapacity),
                     (self.Mdopb + self.RingBMopCapacity), self.UjRingAxial, self.UjRingIPB, self.UjRingOPB))
        #
        #
        #
        #
        #
        OutputFile.write(" "+"\n")
        OutputFile.write("......................................................................................."+"\n")
        OutputFile.write("                                                Remaining Load on Ring Stiffener Method"+"\n")
        OutputFile.write(" "+"\n")                
        OutputFile.write("ShearCheck   Uj          PB     [N]  MBipb[Nmm]  MBopb[Nmm]   PB/Pd   MB/MdIP  MB/MdOP"+"\n")
        OutputFile.write("                         ----------  ----------  ----------   ------  -------  -------"+"\n")
        OutputFile.write("Joint        Uj          PB updated  MBu ipb     MBu opb      PBu/Pd  MBu/MdIP MBu/MdOP"+"\n")
        OutputFile.write("Ring         Ur          P remainig  Mr  ipb     Mr  opb      Pr/Pr   Mr/MrIP  Mr/MrOP"+"\n")
        #OutputFile.write("Method       Ut          PB/(Pr)     MB/(Md+Mr)  MB/(Md+Mr)   "+"\n")
        #
        OutputFile.write("......................................................................................."+"\n")
        OutputFile.write(" "+"\n")        
        #
        #        
        OutputFile.write(("%-12s" +" "+ "%3.4f"  + 5*" "+ "% 1.4E" +" "+ "% 1.4E" + " "+"% 1.4E" +
                     3*" "+ "%3.4f" + 3*" "+ "%3.4f" + 3*" "+"%3.4f"  + "\n")%
                    ( _flagUj, abs(self.Uj), abs(self.PB), abs(self.MBipb), abs(self.MBopb),
                     abs(self.PB/self.Pd), abs(self.MBipb/self.Mdipb), abs(self.MBopb/self.Mdopb)))
        #
        _flagUju = 'PASS'
        if abs(self.UjDiff) > 1 : _flagUju = 'FAIL'
        #
        _flagUr ='PASS'
        if self.URingDiff > 1.0: _flagUr ='FAIL'
        #
        #OutputFile.write(("%-12s" +" "+ "%3.4f"  +6*" "+ "%1.4E" +"  "+ "%1.4E" + "  "+"%1.4E"  + "\n")%
        #            (self.UjFlag, abs(self.Uj) , abs(self.Pd), abs(self.Mdipb), abs(self.Mdopb))) 
        #
        #
        if abs(self.Uj) > 1:
            #
            #
            OutputFile.write("                         ----------  ----------  ----------   ------   ------   ------"+"\n")
            #
            #
            #
            #
            OutputFile.write(("%-12s" +" "+ "%3.4f" + 5*" "+ "% 1.4E" +" "+ "% 1.4E" + " "+"% 1.4E" +
                         3*" "+ "%3.4f" + 3*" "+ "%3.4f" + 3*" "+"%3.4f"  + "\n")%
                        (_flagUju, self.UjDiff, self.UjAxialDiff, self.UjIPBDiff, self.UjOPBDiff,
                         abs(self.UjAxialDiff/self.Pd) , abs(self.UjIPBDiff/self.Mdipb), 
                         abs(self.UjOPBDiff/self.Mdopb)))
            #
            #
            OutputFile.write(("%-12s" +" "+ "%3.4f" + 5*" "+ "% 1.4E" +" "+ "% 1.4E" + " " +"% 1.4E" +
                         3*" "+ "%3.4f" +3*" "+ "%3.4f" + 3*" "+"%3.4f" + "\n")%
                        (_flagUr, self.URingDiff, self.PB_Residual, self.MBipb_Residual, 
                         self.MBopb_Residual, self.URingAxialDiff, self.URingIPBDiff, self.URingOPBDiff))
            #
            #
        #
        #
        OutputFile.write(" "+"\n")
        OutputFile.write("......................................................................................."+"\n")
        OutputFile.write("                                                Section Forces (Perpendicular to Chord)"+"\n")
        OutputFile.write(" "+"\n")
        OutputFile.write("BearingChck  UcYield     BWb   [mm]  BWw   [mm]  PAxial [N]  TotalLoadOn:  Load at :"+"\n")
        OutputFile.write("                         BAb  [mm2]  BAw  [mm2]  ApPoint[N]  1  Ring  [N]  A  Point [N]"+"\n")
        OutputFile.write("Brace        Sb/Fyb      Fyb[N/mm2]  Fyw[N/mm2]  PBMop[Nmm]  n  Ring  [N]  Z  Point [N]"+"\n")
        OutputFile.write("Web          Sw/Fyw      Sb [N/mm2]  Sw [N/mm2]              ------------ "+"\n")
        OutputFile.write("                                                             Maximun Load "+"\n")
        OutputFile.write("......................................................................................."+"\n")
        OutputFile.write(" "+"\n")
        #
        _flagUCsf ='PASS'
        if self.UCsf > 1.0: _flagUCsf ='FAIL'
        #
        _n = 0
        _ntotal = len(self.MaximumLoadOnRing)
        #print ("---> ntotal",_ntotal)
        #
        OutputFile.write(("%-12s" +" "+ "%3.4f"  + 5*" "+ "% 1.4E" +" "+ "% 1.4E" + " "+"% 1.4E" + 
                     "  No " + "Ring Load"  + "  Pt " + "  Load"  + "\n")%
                    ( _flagUCsf, abs(self.UCsf), abs(self.BearingWidthOnBrace), abs(self.BearingWidthOnWeb), 
                      self.PerpendicularAxial))
        #
        OutputFile.write((24*" "+ "% 1.4E" +" "+ "% 1.4E" + " "+"% 1.4E" +2*" "+ 
                     "%-2.0f" +" " + "%1.3E"  +"  A  " + "%1.3E" + "\n")%
                    ( abs(self.BearingAreaOnBrace), abs(self.BearingAreaOnWeb), 
                      self.AxialperPoint, _n+1 , self.MaximumLoadOnRing[ _n ], self.LoadAtPoint[ _n ]))
        #
        _UCbsBrace = abs(self.BearingStressOnBrace/self.FyrFactoredMin)
        _UCbsBrace_flag = 'PASS'
        if _UCbsBrace > 1.0:
            _UCbsBrace_flag = 'FAIL'
        #
        OutputFile.write(("%-12s" +" "+ "%3.4f"  + 5*" "+ "% 1.4E" +" "+ "% 1.4E" + " "+"% 1.4E" + 2*" ")%
                     #"%3.4f" + 3*" "+ "%3.4f" + 3*" "+"%3.4f"  + "\n")%
                    ( _UCbsBrace_flag, _UCbsBrace, self.FyrFactoredMin, self.FyrFactoredMin, 
                      self.PerpendicularBMop))
        _n = _n + 1
        if _n <= _ntotal:
            OutputFile.write(("%-2.0f" +" " + "%1.3E"  +"  B  " + "%1.3E" + "\n")%
                        (_n+1 , self.MaximumLoadOnRing[ _n ], self.LoadAtPoint[ _n ]))
            #
        #
        else:
            OutputFile.write(("------------  B  " + "%1.3E" + "\n")%
                        (self.LoadAtPoint[_n-1]))
        #
        _UCbsWeb = abs(self.BearingStressOnWeb/self.FyrFactoredMin)
        _UCbsWeb_flag = 'PASS'
        if _UCbsWeb > 1.0:
            _UCbsWeb_flag = 'FAIL'
        #
        OutputFile.write(("%-12s" +" "+ "%3.4f"  + 5*" "+ "% 1.4E" +" "+ "% 1.4E" + 14*" ")%
                     #"%3.4f" + 3*" "+ "%3.4f" + 3*" "+"%3.4f"  + "\n")%
                    ( _UCbsWeb_flag, _UCbsWeb, self.BearingStressOnBrace, self.BearingStressOnWeb))
        #
        _n = _n + 1
        if _n < _ntotal:
            OutputFile.write(("%-2.0f" +" " + "%1.3E"  +"  C  " + "%1.3E" + "\n")%
                        (_n+1 , self.MaximumLoadOnRing[ _n ], self.LoadAtPoint[ _n ]))
            #
        elif _ntotal == _n - 1:
            OutputFile.write(("%1.3E"  +" " + "%1.3E" +  "\n")%
                        (self.MaxLoad, self.LoadAtPoint[ _n ]))
            #
        #
        else:
            OutputFile.write(("------------  C  " + "%1.3E" + "\n")%
                        (self.LoadAtPoint[ _n ]))
        #
        _n = _n + 1
        for i in range(_n, _ntotal+2):
            #
            #print (chr( 65 + i))
            #print (_ntotal ,'==', i, _n)
            if i < _ntotal:
                OutputFile.write((61*" "+"%-2.0f" +" " + "%1.3E"  +"  " +"%-1s" +"  "+ "%1.3E" + "\n")%
                            (i+1 , self.MaximumLoadOnRing[i], (chr( 65 + i)), self.LoadAtPoint[i]))
            #
            elif _ntotal == i - 1:
                OutputFile.write((64*" "+"%1.3E" + "  " +"%-1s"+ "  "+ "%1.3E" +  "\n")%
                            (self.MaxLoad, (chr( 65 + i)), self.LoadAtPoint[i]))
                #
                for j in range (i+1, _ntotal*2):
                    OutputFile.write((75*" " + "%-1s"+ "  " + "%1.3E" + "\n")%
                                ((chr( 65 + j)), self.LoadAtPoint[j]))
                #
            #
            else:
                OutputFile.write((61*" " + "------------  "+"%-1s"+ "  " + "%1.3E" + "\n")%
                            ((chr( 65 + i)), self.LoadAtPoint[i]))
        #
        OutputFile.write(" "+"\n") 
        OutputFile.write("_______________________________________________________________________________________"+"\n")
        #
        #
#
#
#
class JointCodeCheck(ISO19902, Rings):
    #
    #-------------------------------------------------
    #                  Joint Data
    #-------------------------------------------------
    #
    def __init__(self, DesignCode, DesignMethod = 'WSD'):
        #
        self.Code = str(DesignCode.upper())
        # print ('code = ',self.Code)
        self.DesignMethod = str(DesignMethod.upper())
        #        self.setup()
        # 
        self.K = 0
        self.YT = 0
        self.X = 0     
        self.Diaph = 'W/O'
        # Default output name
        self.NameOut = 'JointCodeCheck_res.out'
        #
        # ring section
        self.ring = {}
        self.number = 0
        #
        #
    #
    #
    def GeneralData(self, JointID = 'N/A', Jnumber = 0, NameOut = 'CodeCheck_res.out'):
        #
        self.JointName =str(JointID)
        self.JointNumber = Jnumber
        self.NameOut = str(JointID)+'_Joint.out'
    #
    #
    #-------------------------------------------------
    #               Joint Clasification
    #-------------------------------------------------
    #
    def Classification(self, K = 0, YT = 0, X = 0, Diaph = 'W/O'):
        #
        self.K = K
        self.YT = YT
        self.X = X
        self.Diaph = Diaph.upper()
        #
        #
        #print 'X =',self.X,' Diaphrams =', self.Diaph 
    #
    #
    #-------------------------------------------------
    #               Joint Geometry
    #-------------------------------------------------
    #
    #
    def Geometry(self, JointType = 'SIMPLE', gap = 0, Angle = 0 ,CriticalJoint = 'YES'):
        self.JointType = JointType.upper()
        self.gap = gap
        self.theta = Angle
        self.CriticalJoint = CriticalJoint.upper()
        #
        #print ' '
        #print 'Joint Geometry '
        #print ' '
        #print 'Joint configuration =',self.JointType
        #print 'Gap =',self.gap
        #print 'theta =',self.theta 
    #       print 'E =',self.E
    #
    #
    #
    #-------------------------------------------------
    #                Chord Geometry
    #-------------------------------------------------
    #
    #
    def ChordID(self, chordname = 'N/A', chordnumber = 0):
        # 
        self.ChordName = chordname
        self.ChordNumber = chordnumber
        #        
    #
    #
    def ChordMaterial(self, Fy, Fu = 0):
        # 
        self.Fy = Fy
        #        self.E = E
        #
        if Fu == 0 :
            self.Fu = Fy/0.75
        else: self.Fu = Fu
        #
        #print ' '
        #print 'Chord Material Properties '
        #print ' '
        #print 'Fy =',self.Fy
        #print 'Fu =',self.Fu
    #        print 'E =',self.E
    #
    #
    def ChordGeometry(self, D, T):
        #
        self.D = D
        self.T = T
        #print ' '
        #rint 'Chord Geometry '
        #print ' '
        #print 'D =',self.D
        #print 'T =',self.T
    #
    #
    #
    def ChordForces(self, PCaxial = 0, MCinPlane = 0, MCoutPlane = 0 ):
    #        
    #       Axial        
        self.PC = float(PCaxial)
    #       Bending
        self.MCipb = float(MCinPlane)
        self.MCopb = float(MCoutPlane)   
    #
    #
    #
    #
    #-------------------------------------------------
    #               Brace Geometry
    #-------------------------------------------------
    #
    #
    def BraceID(self, bracename = 'N/A', bracenumber = 0):
        # 
        self.BraceName = bracename
        self.BraceNumber = bracenumber
        # 
    #
    #
    def BraceGeometry(self, d, t):
        self.d = d
        self.t = t
        #print ' '
        #print 'Brace Geometry '
        #print ' '
        #print 'd =',self.d
        #print 't =',self.t
    #
    #
    def BraceMaterial(self, Fyb, Fub = 0):
        # 
        self.Fyb = Fyb
        #        self.E = E
        #
        if Fub == 0 :
            self.Fub = Fyb/0.75
        else: self.Fub = Fub
        #
        #print ' '
        #print 'Brace Material Properties '
        #print ' '
        #print 'Fyb =',self.Fyb
        #print 'Fu =',self.Fub
        #        print 'E =',self.E
    #
    #
    def BraceForces(self, PBaxial = 0, MBinPlane = 0, MBoutPlane = 0 ):
    #        
    #       Axial        
        self.PB = float(PBaxial)
    #       Bending
        self.MBipb = float(MBinPlane)
        self.MBopb = float(MBoutPlane)   
    #
    #
    #
    #
    #-------------------------------------------------
    #               Ring Stiffeners
    #-------------------------------------------------
    #
    def RingData(self, name, hw, tw, bf, tf, Ln, Fyr, WP = 0, V = 1.05, tfratio = 15.0):
        self.ringname = name
        self.hw = hw
        self.tw = tw
        self.bf = bf
        self.tf = tf
        self.Ln = Ln
        self.Fyr = Fyr
        self.WP = WP 
        self.V = V
        self.tfratio = tfratio  
        #
        self.number = self.number + 1
        #print ('ring number', self.number)
        #
        #RingID = 'ring'+str(self.number)
        #self.ring[self.number]= Rings.Geometry(self, hw, tw, bf, tf, Ln, WP)
        self.ring[self.number] = Rings(self.ringname, self.number,
                                       self.hw, self.tw,
                                       self.bf, self.tf,
                                       self.Ln, self.Fyr,
                                       self.WP, self.V,
                                       self.tfratio)
        #self.ring[self.number].Geometry()
        #self.ring[self.number].Geometry(hw, tw, bf, tf, Ln, WP)
        #
    #
    #
    #
    #-------------------------------------------------
    #               Print Results
    #-------------------------------------------------
    #
    #
    def PrintResults(self):
        print (' ')
        print ("+++++++++++++++++++++++++++++")
        print ('  Joint Code Check Results')
        print ('Code Check to: ',self.Code)
        print ("Joint : ",self.JointName)
        print ("Output file: ",self.NameOut)
        print ("+++++++++++++++++++++++++++++")
        #
        #if self.Header == 1:
        today = datetime.date.today()
        OutputFile = open(self.NameOut,'w')
        #        
        OutputFile.write(" "+"\n")
        OutputFile.write("***************************************************************************************"+"\n")
        OutputFile.write("*                                   CODE CHECK TOOL                                   *"+"\n")
        OutputFile.write("*                                 Joint Check Module                                  *"+"\n")
        OutputFile.write("*                                   ISO/FDIS 19902                                    *"+"\n")
        OutputFile.write("*                                    BETA Version                            11/08/10 *"+"\n")            
        OutputFile.write("***************************************************************************************"+"\n")
        OutputFile.write(("DATE: "+ "%-8s" +"\n")%(today))
        #
        if self.Code == 'ISO':
            OutputFile = open(self.NameOut,'a+')
            #
            #
            if self.JointType == 'SIMPLE':
                ISO19902.GeometryCheck(self)
                ISO19902.StrengthFactor(self)
                ISO19902.ChordForceFactor(self)
                ISO19902.BasicJointStrength(self)
                ISO19902.StrengthCheck(self)
                ISO19902.PrintJointGeometry(self)
                ISO19902.PrintResultsSimple(self)
                #
                #
                #
            #
            elif self.JointType == 'RING':
                #
                #
                print (' ')
                print ('Ring')
                ISO19902.GeometryCheck(self)
                ISO19902.StrengthFactor(self)
                ISO19902.ChordForceFactor(self)
                ISO19902.BasicJointStrength(self)
                ISO19902.StrengthCheck(self)
                ISO19902.PrintJointGeometry(self)
                #
                if len(self.ring)< 2:
                    print ("Minimum 2 Rings are required")
                    OutputFile.write(" "+"\n")
                    ISO19902.PrintResultsSimple(self)
                    OutputFile.write("**WARNING**"+"\n")
                    OutputFile.write("Minimum 2 Rings Are Required "+"\n")
                    OutputFile.write("===> Number of Rings Given : "+ str(len(self.ring))+"\n")
                    OutputFile.write("RING CHECK TERMINATED" +"\n")
                    exit()
                #
                #
                self.RingNumber = len(self.ring)
                self.LnAverage = 0
                self.RingAxialCapacity = 0
                self.RingBMipCapacity = 0
                self.RingBMopCapacity = 0
                self.RingWidthSum = 0
                self.RingWidthSum2 = 0
                #
                for i in range(1,len(self.ring)+1):
                    #
                    self.LnAverage = self.LnAverage + self.ring[i].Ln
                    #print (' ')
                    #print ('Ring Number:', i, self.ring[i].Ln)
                    self.ring[i].Chord(self.ChordName, self.D, self.T, self.Fy)
                    self.ring[i].Brace(self.BraceName, self.d, self.t, self.theta, self.Fyb)
                    try:
                        #
                        self.ring[i].EffectiveWidth(self.RingNumber, self.ring[i-1].Ln)
                    #
                    except:
                        self.ring[i].EffectiveWidth(self.RingNumber, 0.0)
                    #
                    self.ring[i].Properties()
                    self.RingWidthSum = self.RingWidthSum + self.ring[i].WidthAtRing
                    self.RingWidthSum2 = self.RingWidthSum2 + self.ring[i].WidthAtRing**2
                    self.ring[i].PrintRingGeometry(self.NameOut)
                #
                #
                self.LnAverage = self.LnAverage/self.RingNumber
                #if len(self.ring) == 1:
                #    self.LnAverage = 0
                #
                print ('---------------------------')
                #print ('LnAverage', self.LnAverage)
                #print ('RingWidthSum', self.RingWidthSum)
                #
                for i in range(1,len(self.ring)+1):
                    #print ('====> Ring Number:',i)
                    _ringIPBMadd = self.ring[i].RingShearCapacity(self.LnAverage, self.RingWidthSum)
                    self.RingBMipCapacity = self.RingBMipCapacity + _ringIPBMadd
                    self.RingBMopCapacity = self.ring[i].RingOPBcapacity 
                    self.RingAxialCapacity = self.ring[i].RingAxialCapacity
                    self.ring[i].PrintRingProperties(self.NameOut)
                    #
                #
                print ('RingAxialCapacity ', self.RingAxialCapacity)
                print ('RingBMipCapacity', self.RingBMipCapacity)
                print ('RingBMopCapacity', self.RingBMopCapacity)
                print ('End writiing output file')
                #
                #
                ISO19902.PrintResultsSimple(self)
                ISO19902.RingStiffened(self)
                ISO19902.PrintResultsRing(self)
                #self.ring[1].PrintRingShearCapacity(self.NameOut, self.PB, self.MBipb, self.MBopb)
                #
            #    
            else:
                print ('No yet implemented')
                exit()
            #
#        
#
#-------------------------------------------------
#                    END
#-------------------------------------------------
#