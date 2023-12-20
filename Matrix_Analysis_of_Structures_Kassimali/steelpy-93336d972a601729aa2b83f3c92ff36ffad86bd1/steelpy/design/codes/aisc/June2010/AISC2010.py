# *******************************************
#                 Bug History
#
# Bug fixed on section (E4-3b) 18/07/10 - SVO
# Bug fixed on section (F4)    21/07/10 - SVO
# Mayor program upgrade to
# allow re-design of members   30/07/10 - SVO
#
#
# *******************************************
#
#
# from __future__ import print_function
import math
import datetime
#
#
def SecFinder(section):
    #
    if section == 'I':
        SectionFinal = 'I'
    #
    elif section == 'H':
        SectionFinal = 'I'
    #
    elif section == 'W':
        SectionFinal = 'I'
    #
    elif section == 'M':
        SectionFinal = 'I'
    #
    elif section == 'S':
        SectionFinal = 'I'
    #
    elif section == 'HP':
        SectionFinal = 'I'
     #
    if section == 'UB':
        SectionFinal = 'I'
     #
    if section == 'UC':
        SectionFinal = 'I'
     #
    if section == 'UBP':
        SectionFinal = 'I'
    #
    elif section == 'C':
        SectionFinal = 'CHANNEL'
    #
    elif section == 'MC':
        SectionFinal = 'CHANNEL'
    #
    elif section == 'L':
        SectionFinal = 'ANGLE'
    #
    elif section == 'WT':
        SectionFinal = 'TEE'
    #
    elif section == 'MT':
        SectionFinal = 'TEE'
    #
    elif section == 'ST':
        SectionFinal = 'TEE'
    #
    elif section == 'HSS-ROUND':
        SectionFinal = 'TUBULAR'
    #
    elif section == 'PIPE':
        SectionFinal = 'TUBULAR'
    #
    elif section == 'HSS-RECT':
        SectionFinal = 'BOX'
    #
    elif section == '2L':
        SectionFinal = 'ANGLE?'
    #
    return SectionFinal   
#
#
class Shape:
    # 
    """
    ------------------------------------------------
    A       = Cross-Sectional Area
    Yc      = Elastic Neutral Centre 
    SC      = Shear Centre
    J       = Torsional Constant
    Cw      = Warping Constant
    ------------------------------------------------
    Mayor Axis
    ------------------------------------------------
    Ix      = Second Moment of Area In Plane
    Sx      = Elastic Modulus In Plane
    Zx      = Plastic Modulus In Plane
    rx      = Radius of gyration In Plane
    ------------------------------------------------
    Minor Axis
    ------------------------------------------------
    Iy      = Second Moment of Area Out of Plane
    Sy      = Elastic Modulus Out of Plane
    Zy      = Plastic Modulus Out of Plane
    ry      = Radius of gyration Out of Plane
    ------------------------------------------------
    """
    #    
    def __init__(self):
        pass
    #
    #
    def Angle(self, Higth = 0, Tw = 0, Bfc = 0, Tfc = 0, Kk = 0, Rr = 0 ):
        print ('Angle')
        #  Beam Section Definition
        self.d = float(Higth)
        self.tw = float(Tw)
        self.bfc = float(Bfc)
        self.tfc = float(Tfc)
        self.WebOrientation = float(Kk)
        self.RadiusCurvature = float(Rr)
        #
        #-------------------------------------------------
        #   Cross-Sectional Area
        self.Area = (self.d * self.tw) + (self.bfc * self.tfc)
        #
        #-------------------------------------------------
        #   Elastic Neutral Centre 
    #
    #
    def BarSquare(self,D = 0, Bfc=0, Bft=0 ):
        print ('Bar Sqare')
        self.d = float(D)
        self.bfc = float(Bfc)
        self.bft = float(Bft)
        #
    #
    #
    def Box(self, H = 0, B = 0, TT = 0, TB = 0, TL = 0, TR = 0):
        print ('box')
        self.H = float(H)
        self.B = float(B)
        self.TT = float(TT)
        self.TB = float(TB)
        self.TL = float(TL)
        self.TR = float(TL)
    #
    #
    def Channel(self, H = 0, Tw=0, Bfc=0, Tfc=0, K = 0, R = 0):
        print ('Channel')
        self.d = float(D)
        self.tw = float(Tw)
        self.bfc = float(Bfc)        
        self.tfc = float(Tfc)
        self.tft = float(Tfc)
        self.WebOrientation = float(K)
        self.Radius = float(R)
    #
    #
    def DoubleBottom(self, D=0, Tw=0, By=0, Tfc=0, Tft=0):
        print ('Double Bottom')
        self.d = float(D)
        self.tw = float(Tw)
        self.EffectiveWidthPlate = float(By)        
        self.tfc = float(Tfc)
        self.tft = float(Tft)
    #
    #
    def General(self, area=0, Ix=0, Iy=0, Iz=0, Iyz=0, WXmin = 0, WYmin = 0, WZmin = 0, SharY =0, SharZ =0, SHcenY =0, SHcenZ =0):
        print ('General Beam')
        self.Area = float(area)
        self.Ix = float(Ix)
        self.Iy = float(Iy)
        #self.tft = float(Tft)
        #self.bfc = float(Bfc)
    #
    #        
    def I(self,D=0,Tw=0,Bfc=0,Tfc=0,Bft=0,Tft=0):
        #        
        # Beam Section Definition
        self.d = float(D)
        self.tw = float(Tw)
        self.bfc = float(Bfc)
        self.tfc = float(Tfc)
        self.bft = float(Bft)
        self.tft = float(Tft)
        #        print ' '
        #        print 'Section Properties '
        #        print ' '
        #        print 'D =',self.d
        #        print 'Tw =',self.tw
        #        print 'Bfc=',self.bfc
        #        print 'Tfc =',self.tfc
        #        print 'Bft =',self.bft
        #        print 'Tft=',self.tft
        #
        #-------------------------------------------------             
        #
        #    def SecSymmetry(self):
        if self.bfc == self.bft and self.tfc == self.tft :
            self.SecSym = 'DOUBLY'
            #
        #
        else: self.SecSym = 'SINGLY'
        #
        # print ('Geometry Symmetry :',self.SecSym)
        #
        #
        #-------------------------------------------------   
        #    def d(self):
        #
        self.hw = (self.d-self.tfc-self.tft)
        self.ho = (self.d-0.5*self.tfc-0.5*self.tft)
        #        print 'd =',self.hw
        #        print 'ho = ',self.ho
        #        return self.hw
        #     
        #-------------------------------------------------
        #   Cross-Sectional Area
        #    def A(self):
        #
        self.A = (self.bfc*self.tfc+
                  self.bft*self.tft+
                  self.hw*self.tw)
        #      print 'Section Area =', self.A
        #        return self.A
        #
        #-------------------------------------------------
        #   Elastic Neutral Centre to from bfc
        #    def y(self):      
        #
        self.Yp = ((self.bfc * self.tfc**2 / 2 +
                    self.bft * self.tft * 
                    (self.hw + self.tfc + self.tft/2) +
                    self.hw * self.tw * (self.hw / 2 + self.tfc))/
                   (self.bfc * self.tfc + self.hw * self.tw +
                    self.bft * self.tft))
        #        print 'Yc =', self.Yp
        #        return self.Yp
        #-------------------------------------------------
        #   Torsional Constant
        #    def J(self):
        #
        if self.bfc == self.bft and self.tfc == self.tft :
            self.J = ((2*self.bfc*self.tfc**3/3)+(self.d*self.tw**3/3))
            #
        #
        else:
            self.J = ((self.bfc*self.tfc**3+
                       self.bft*self.tft**3+
                       (self.d-self.tfc/2-self.tft /2)*self.tw**3)/3)
        #
        #        print 'J ',self.J
        #        return self.J
        #            
        #-------------------------------------------------
        #   Shear Centre Y Axis
        #    def SC(self):
        #
        self.SCy = (((self.d-self.tfc/2-self.tft/2)*self.tfc*self.bfc**3)/
                    (self.tfc*self.bfc**3+self.tft*self.bft**3))
        #        print 'Shear Centre',self.SCy
        #
        #   Shear Centre X Axis
        #
        self.SCx = 0
        #
        #if self.bfc == self.bft and self.tfc == self.tft :
        #    self.SCx = 0
        #
        #else:
        #    print ('fix this')
        #
        #-------------------------------------------------
        #               Section Properties
        #-------------------------------------------------
        #
        #
        #   Second Moment of Area about Mayor Axis
        #   --------------------------------------
        #
        #    def Ix(self):
        #
        self.Ix = (self.bfc*self.tfc**3/12+
                   self.bfc*self.tfc*(self.Yp-self.tfc/2)**2+
                   self.bft*self.tft**3/12+
                   self.bft*self.tft*(self.hw+self.tft/2+self.tfc-self.Yp)**2+
                   self.tw*self.hw**3/12+
                   self.tw*self.hw*(self.hw/2+self.tfc-self.Yp)**2)
        #        print 'Ixx',self.Ix
        #        return self.Ix
        #
        #-------------------------------------------------        
        #   Elastic Modulus about Mayor Axis
        #    def Sx(self):
        #
        if self.Yp >= (self.d - self.Yp):
            self.Sx = self.Ix/self.Yp
        else:
            self.Sx = self.Ix/(self.d - self.Yp)
        #
        #        print 'Elastic Modulus Sx',self.Sx
        #        return self.Sx
        #
        #-------------------------------------------------
        #   Plastic Modulus about Mayor Axis
        #    def Zx(self):
        #
        self.Zx = ((self.tw*self.hw**2/4)+
                   (self.bfc*self.tfc*(self.Yp-self.tfc/2))+
                   (self.bft*self.tft*(self.d-self.Yp-self.tft/2)))
        #
        #        print 'Plastic Modulus Zx', self.Zx
        #        return self.Zx
        #
        #-------------------------------------------------
        #   Radius of gyration about Mayor Axis
        #    def rx(self):
        #
        self.rx = math.sqrt(self.Ix/self.A)
        # 
        #        print 'rxx',self.rx
        #        return self.rx
        #       
        #
        #
        #   Second Moment of Area about Minor Axis
        #   --------------------------------------
        #
        #    def Iy(self):
        #
        self.Iy = (self.bfc**3*self.tfc/12+
                   self.bft**3*self.tft/12+
                   self.tw**3*self.hw/12)
        # 
        #        print 'Iyy',self.Iy
        #        return self.Iy
        #
        #-------------------------------------------------
        #   Elastic Modulus about Minor Axis
        #    def Sy(self):
        #
        if self.bfc >= self.bft:
            self.Sy = 2*self.Iy/self.bfc
        else:
            self.Sy = 2*self.Iy/self.bft
        # 
        #        print 'Elastic Modulus Sy',self.Sy
        #        return self.Sy
        #
        #-------------------------------------------------
        #   Plastic Modulus about Minor Axis  
        #    def Zy(self):
        #
        self.Zy = ((self.tfc*self.bfc**2+self.tft*self.bft**2+
                    self.hw*self.tw**2)/4)
        # 
        #        print 'Plastic Modulus Zy',self.Zy
        #        return Zyy
        #
        #-------------------------------------------------
        #   Radius of gyration about Mayor Axis  
        #    def ry(self):
        #
        self.ry = math.sqrt(self.Iy/self.A)
        # 
        #        print 'rxx',self.ry
        #        return self.ry
        #
        #
        #   Warping Constant Cw
        self.Cw = (self.Iy*self.ho**2)/4
        #        print 'Warping Constant Cw need to be verify', self.Cw 
        #
        #
        #-------------------------------------------------        
        #            Shear Stress Calculation
        # 
        #       Average (Shear Stress)
        #    if self.ShearStress == 'AVERAGE':
        #        
        #    Area of Web
        #       The overall depth times the web thickness
        self.Aw = self.d*self.tw        
        #
        #    Area of Flange
        self.Af = ((self.bfc*self.tfc)+(self.bft*self.tft))
        #                    -(self.tw*self.tfc)-(self.tw*self.tft))
        #
        #    True (Shear Stress)
        try:
            #
            _Yten = (self.d - self.Yp)
            _npt = 9
            #       Define X Coordinates        
            _CoorX = [(-self.bfc/2.0), 0, (self.bfc/2.0), 0, 0,
                     0, (-self.bft/2.0), 0, (self.bft/2.0)]
            #        
            _CoorY = [self.Yp, self.Yp, self.Yp, (self.Yp - self.tfc), 0,
                     (-_Yten + self.tft), -_Yten, -_Yten, -_Yten]
            #
            _q1 = ((abs(self.Vy)/self.Ix)*
                   (self.Yp - self.tfc/2)*self.tfc*self.bfc/2)
            #
            _q2 = ((abs(self.Vy)/self.Ix)*
                   (self.Yp - self.tfc/2)*self.tfc*self.bfc)
            #
            _q3 = ((abs(self.Vy)/self.Ix)*
                   (self.Yp - self.tfc)/2*self.tw*
                   (self.Yp - self.tfc/2))
            #
            _q4 = ((abs(self.Vy)/self.Ix)*
                   (_Yten-self.tft)/2*self.tw*(_Yten-self.tft/2))
            #
            _q5 = ((abs(self.Vy)/self.Ix)*
                   (_Yten-self.tft/2)*self.tft*self.bft)
            #
            _q6 = ((abs(self.Vy)/self.Ix)*
                   (_Yten-self.tft/2)*self.tft*self.bft/2)
            #
            _q = [0, _q1, 0, _q2, min((_q2+_q3),(_q4+_q5)), _q5, 0, _q6, 0]
            #
            _tau_y =[]
            #
            for i in range(_npt):
                _tau_y.append(abs((self.Vy/(2.0*self.Ix))*             
                        ((self.d**2)/4-(_CoorY[i]**2))))
                
            #
            _tau_y[1] = _q[1]/self.bfc
            _tau_y[3] = _q[3]/self.tw
            _tau_y[4] = _q[4]/self.tw
            _tau_y[5] = _q[5]/self.tw
            _tau_y[7] = _q[7]/self.bft
            #        
            #
            _VxComp = abs((self.Vx * self.Yp)/self.d)
            _VxTen = abs((self.Vx * _Yten )/self.d)
            #
            _tauComp = (3*_VxComp /(2*self.bfc*self.tfc))
            _tauTen = (3*_VxTen /(2*self.bft*self.tft))
            _tau_x =[0, _tauComp , 0, _tauComp, 0, _tauTen, 0, _tauTen, 0]
        #        
        #        print (' X    Y')
        #        for i in range(_npt):
        #            print (_tau_x[i],_tau_y[i])
        #
            self.tau_y = max(_tau_y)
            self.tau_x = max(_tau_x)
        #
        except:
            self.tau_y = (0.40*self.Fy*self.Aw)
            self.tau_x = (0.40*self.Fy*self.Af)
        #
        # print ('++++++++++++++++++++++')
        # print ('Max tauY  tauX')
        # print ((self.tau_y),self.tau_x)
        # print ((self.tau_y*self.Aw),(self.tau_x*self.Af))
        # print (' ')
        #
        #-------------------------------------------------             
        # Mass
        #
        self.mass = self.A * self.Rhos
        #
    #
    #
    #     
    def Tubular(self, Diameter, WallThickness):
        self.OD = float(Diameter)
        self.WT = float(WallThickness)
#     
#-------------------------------------------------
#   Cross-Sectional Area
#    def A(self):
#
        self.A=((math.pi / 4.)*
                      (self.OD**2 -(self.OD - 2*self.WT)**2))
        #print ('circular area:',self.A)
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
        self.Ix = ((math.pi / 64.0)*
                    (self.OD**4 - (self.OD-2*self.WT)**4))
        # print ('circular Second Moment',self.Ix)
#
#
#       
#   Elastic Modulus about Mayor Axis
#   --------------------------------------
#
        self.Sx = ((self.OD**3 -
                      (self.OD - 2*self.WT)**3) / 6.0)
        # print ('circular Elastic Modulus',self.Sx)
#
#
#
#-------------------------------------------------
#   Plastic Modulus about Mayor Axis
#    def Zx(self):
#
        self.Zx = (2*self.Ix/self.OD)
        # print ('circular Plastic Modulus',self.Zx)
#
#
#-------------------------------------------------
#   Radius of gyration about Mayor Axis
#
        self.rx = ((self.Ix/self.A)**0.5)
        # print ('circular Radius of gyration about Mayor Axis',self.rx)
# 
#
#-------------------------------------------------
#   Torsional Constant
#
        self.J = (2*self.Ix)
        # print ('Torsional Constant',self.J)
# 
#
#-------------------------------------------------
#   Polar Moment of Inertia
        self.Ip = ((math.pi/32.0)*
                   (self.OD**4 - 
                    (self.OD - 2*self.WT)**4))
        # print ('Polar Moment of Inertia Ip:',self.Ip)
    #
    #
    def TonPlate(self, hz=0, ty=0, bt=0, tt=0, bp=0, tp=0):
        print ('T on Plate')
        self.deight = float(hz)
        self.tw = float(Tw)
        self.EffectiveWidthPlate = float(By)        
        self.tfc = float(Tfc)
        self.tft = float(Tft)
    #
    #
    def IUnsynetric(self, hz=0, ty=0, bt=0, b1=0, tt=0, bb=0, b2=0, tb=0):
        print ('Unsymetrical I beam')
        self.deight = float(hz)
        self.tw = float(Tw)
        self.EffectiveWidthPlate = float(By)        
        self.tfc = float(Tfc)
        self.tft = float(Tft)
    #
    #
    def Plate(self, th = 0):
        print ('Plate')
        self.Tck = float(th)
    #
    #
    #
    def PrintProperty(self):
        #  
        OutputFile=open(self.FileOut,'a+')
        # 
        OutputFile.write(" "+"\n")
        OutputFile.write("_______________________________________________________________________________________"+"\n")
        OutputFile.write(" "+"\n")
        OutputFile.write("                                  SECTION PROPERTIES"+"\n")
        OutputFile.write("_______________________________________________________________________________________"+"\n")
        OutputFile.write("                                           |"+"\n")
        OutputFile.write(("          Section Geometry "+ "%-16s"+ "|                   ^  Y "+"\n")%(self.SectionType))
        OutputFile.write("                                           |                   ."+"\n")
        OutputFile.write(("Overal Section Depth       (D) = "+"% 6.0f"+" mm |"+"           +      bfc     +"+"\n")%(self.d))
        OutputFile.write(("Web Thickness             (Tw) = "+"% 6.0f"+" mm |"+"   +.......+-------.------+ ..+"+"\n")%(self.tw))
        OutputFile.write(("Width Flange Compression (Bfc) = "+"% 6.0f"+" mm |"+"   .       +------+.+-----+ ..+ tfc"+"\n")%(self.bfc))
        OutputFile.write(("Thickness Flange Comp    (Tfc) = "+"% 6.0f"+" mm |"+"   .              |.|"+"\n")%(self.tfc))
        OutputFile.write(("Width Flange Tension     (Bft) = "+"% 6.0f"+" mm |"+"   .              |.|"+"\n")%(self.bft))
        OutputFile.write(("Thickness Flange Tension (Tft) = "+"% 6.0f"+" mm |"+"   d     + .......|.|........> X"+"\n")%(self.tft))
        OutputFile.write(("Clear Dist Btwn Flangles  (hw) = "+"% 6.0f"+" mm |"+"   .     .        |.|"+"\n")%(self.hw))
        OutputFile.write(("Dist Btwn Flangles Cntr   (ho) = "+"% 6.0f"+" mm |"+"   .    Yc        |.|"+"\n")%(self.ho))
        OutputFile.write(("Elastic Neutral Centre    (Yc) = "+"% 6.0f"+" mm |"+"   .     .   +----+.+----+ ...+"+"\n")%(self.Yp))
        OutputFile.write(("Shear Centre              (SC) = "+"% 6.0f"+" mm |"+"   +.....+...+-----.-----+ ...+ tft"+"\n")%(self.SCy))
        OutputFile.write("                                           |"+"             +    bft    +"+"\n")
        OutputFile.write(("-------------------------------------------|"+
                     "-------------------------------------------"+"\n"))
        OutputFile.write("                                           |"+"\n")
        OutputFile.write(("Overal Section Depth       (D) = "+"% 6.0f"+" mm |"+"  Geometry Symmetry      = "+ "%-16s"+"\n")%(self.d,self.SecSym))
        OutputFile.write(("Web Thickness             (Tw) = "+"% 6.0f"+" mm |"+"  Cross-Sec Area     (A) = "+ "%-1.4E"+ " mm2"+"\n")%(self.tw,self.A))
        OutputFile.write(("Width Flange Compression (Bfc) = "+"% 6.0f"+" mm |"+"  Torsional Constant (J) = "+ "%-1.4E"+ " mm4"+"\n")%(self.bfc,self.J))
        OutputFile.write(("Thickness Flange Comp    (Tfc) = "+"% 6.0f"+" mm |"+"  Warping Constant  (Cw) = "+ "%-1.4E"+ " mm6"+"\n")%(self.tfc,self.Cw))        
        OutputFile.write("                                           |"+"\n")
        OutputFile.write(("-------------------------------------------|"+
                     "-------------------------------------------"+"\n"))
        OutputFile.write("                                           |"+"\n")
        OutputFile.write(("          Major Axis (In-Plane)            |"
                     +"            Minor Axis (Out-Plane)         "+"\n"))
        OutputFile.write("                                           |"+"\n")
        OutputFile.write(("Moment of Inertia  (Ix) = "+ "%-1.4E"+ " mm4   |"+
                     "  Moment of Inertia  (Iy) = "+"%-1.4E"+ " mm4"+"\n")%(self.Ix,self.Iy))
        OutputFile.write(("Elastic Modulus    (Sx) = "+ "%-1.4E"+ " mm3   |"+
                     "  Elastic Modulus    (Sy) = "+"%-1.4E"+ " mm4"+"\n")%(self.Sx,self.Sy))
        OutputFile.write(("Plastic Modulus    (Zx) = "+ "%-1.4E"+ " mm3   |"+
                     "  Plastic Modulus    (Zy) = "+"%-1.4E"+ " mm4"+"\n")%(self.Zx,self.Zy))
        OutputFile.write(("Radious Gyration   (rx) = "+ "%-1.4E"+ " mm    |"+
                     "  Radious Gyration   (ry) = "+"%-1.4E"+ " mm4"+"\n")%(self.rx,self.ry))
        OutputFile.write("                                           |"+"\n")
        OutputFile.write("-------------------------------------------|-------------------------------------------"+"\n")
        #OutputFile.write("                                           |"+"\n")
        #
        # 
        #
    #
    #      
    def PrintPropertyShort(self):
        #  
        OutputFile = open(self.FileOut,'a+')
        # 
        OutputFile.write(" "+"\n")
        OutputFile.write("_______________________________________________________________________________________"+"\n")
        OutputFile.write(" "+"\n")
        OutputFile.write(("Design Method : "+ "%-12s" +42*" "+" CALCULATION: " +"% 3.0f" +"\n")
                         % (self.DesignMethod , self.Header))
        #
        #OutputFile.write(" "+"\n")
        OutputFile.write("_______________________________________________________________________________________"+"\n")
        OutputFile.write(" "+"\n")
        OutputFile.write("                               SECTION PROPERTIES REPORT                     UNITS [mm]"+"\n")
        OutputFile.write(" "+"\n")
        OutputFile.write("Member ID      Type    Diametre   Thkness    DepthWeb   WebThk     WidthFC    FlgThkC"+"\n")
        OutputFile.write("                                                                   WidthFT    FlgThkT"+"\n")
        OutputFile.write("......................................................................................."+"\n")
        OutputFile.write(" "+"\n")        
        #        OutputFile.write(("xxxxxxxxxxxx   "+"%-6s" +"  "+"%-8.2f"+"   "+"%-8.2f"+"\n")%(self.SectionType,self.d,self.tw))
        OutputFile.write(("%-12s" +"   "+"%-6s" +"                        "+"%-8.2f"+"   "+"%-8.2f"+"   "+"%-8.2f"+
                     "   "+"%-8.2f"+"\n")%(self.BeamID,self.SectionType,self.d,self.tw,self.bfc,self.tfc))
        OutputFile.write(("                                                                   "+"%-8.2f"+"   "+"%-8.2f"+
                     "\n")%(self.bft,self.tft))
        #
        OutputFile.write(" "+"\n")
        OutputFile.write("_______________________________________________________________________________________"+"\n")
        OutputFile.write(" "+"\n")
        OutputFile.write("                               SECTION DERIVED PROPERTIES"+"\n")
        OutputFile.write(" "+"\n")
        OutputFile.write("Member ID     Area [mm^2]  Ixx [mm^4]  Iyy [mm^4]  Yp    [mm]  rx    [mm]  J   [mm^4]"+"\n")
        OutputFile.write("                           Sxx [mm^3]  Syy [mm^3]  SCeny [mm]  ry    [mm]  Cw  [mm^6]"+"\n")
        OutputFile.write("              Mass [kg/m]  Zxx [mm^3]  Zyy [mm^3]  SCenx [mm]  "+"\n")
        OutputFile.write("......................................................................................."+"\n")
        OutputFile.write(" "+"\n")
        OutputFile.write(("%-12s" +"   "+"%-1.4E"+"  "+"%-1.4E"+"  "+"%-1.4E"+"  "+"%-1.4E"+"  "+"%-1.4E" +
                          "  "+"%-1.4E"+"\n")%
                    (self.BeamID, self.A, self.Ix, self.Iy, self.Yp, self.rx, self.J))
        OutputFile.write((27*" " + "%-1.4E" + "  " + "%-1.4E" + "  " + "%-1.4E"+"  " +
                          "%-1.4E"+"  "+"%-1.4E"+"\n")%
                    (self.Sx, self.Sy, self.SCy, self.ry, self.Cw))
        OutputFile.write(("               "+"%-1.4E"+"  "+"%-1.4E"+"  "+"%-1.4E"+ "  "+"%-1.4E"+ "\n")%
                    (self.mass/1000**2, self.Zx, self.Zy, self.SCx))
        # 
        #
#
# 
class AISC2010:        
    #            
    def __init__(self):
        #       
        pass
    #-------------------------------------------------
    #
    #             ++++++ Chapter B  ++++++ 
    #
    #        
    def ChapterB(self):
        #       
        #
        #
        #    def AISC_B(self):
        #       B4.3 Gross and Net Area Determination
        #
        #       B4.3a Gross Area
        self.Ag = self.A
        #
        #       B4.3b Net Area
        self.An =self.A
        #        print 'Ag :', self.Ag
        #
        #   ++++++ Flange Compacness ++++++
        #
        #
        _bt_c = 0.5*self.bfc/self.tfc
        _bt_t = 0.5*self.bft/self.tft
        #        
        if _bt_c > _bt_t:
            self.bt = _bt_c
            self.t = self.tfc
        #
        else:
            self.bt = _bt_t
            self.t = self.tft            
        #       
        self.Kc = 4.0/(self.hw/self.tw)**0.5
        #
        if self.Kc < 0.35 : self.Kc = 0.35
        #
        elif self.Kc > 0.76 : self.Kc = 0.76
        #        print 'Kc =',self.Kc
        #
        #
        #   ++++++++ Web Compacness ++++++++
        #
        self.htw = self.hw / self.tw
        #
        # ---------------------------------------------
        #                Table B4.1b
        #
        self.bt_F = 0.5*self.bfc/self.tfc
        #
        #
        self.FL = 0.7*self.Fy
        #   
        #
        # ---------------------------------------------
        #
        self.Mpx = self.Fy*self.Zx
        #
        self.Mym = self.Mpx
        # print (' --------------------------')
        # print ('My needs to be corrected', self.Mym)
        #
        # Elastic Centroid to the Compression Flange
        self.hc = 2*(self.Yp-self.tfc)
        #
        self.hctw = self.hc / self.tw
        #
        #       Plastic Neutral Axis to the Compression Flange
        self.hp = self.hc
        #        print ('hp needs to be corrected', self.hp)
        #        print (' --------------------------')
        #
        #
        # 
        # +++++++++++++++++++++++++++++++++++++++++++++
        #    ----- Table B4.1a &  Table B4.1b -----
        # +++++++++++++++++++++++++++++++++++++++++++++
        #   
        #   
        if self.SectionType =='I':
            #
            # Rolled & Symetric I-shaped sections
            if self.SecSym == 'DOUBLY' and self.Build == 'ROLLED':
                #
                # Table B4.1a
                #
                # CASE 1 <-----------------
                # Flanges of rolled I-shaped sections
                _lambda_B4_1 = 0.56*(self.E/self.Fy)**0.5
                #
                self.lambda_r_fE = _lambda_B4_1
                self.lambda_r_fE_flag = 'B4.1'                
                #
                #
                # CASE 5 <-----------------
                # Webs of doubly symmetric I-shaped sections
                _lambda_B4_5 = 1.49*(self.E/self.Fy)**0.5        
                #
                self.lambda_r_wE = _lambda_B4_5
                self.lambda_r_wE_flag = 'B4.5'                
                #
                #
                # Table B4.1b
                #
                #
                # CASE 10 <-----------------
                # Flanges of rolled I-shaped sections
                # Main Axis
                #
                # Compact/NonCompact
                _lambda_B4_10 = 0.38*(self.E/self.Fy)**0.5
                #
                # NonCompact/Slender
                _lambda_B4_10_r = 1.0*(self.E/self.Fy)**0.5
                #
                #
                # CASE 13 <-----------------
                # Flanges of all I-shaped sections
                # Weak Axis
                #
                # Compact/NonCompact
                _lambda_B4_13 = 0.38*(self.E/self.Fy)**0.5
                # NonCompact/Slender
                _lambda_B4_13_r = 1.0*(self.E/self.Fy)**0.5        
                #     
                #
                # Flange
                self.lambda_p_fF = _lambda_B4_10
                self.lambda_r_fF = _lambda_B4_10_r                 
                #
                #    WEBS
                #
                # CASE 15 <-----------------
                # Webs of doubly symmetric I-shaped sections
                #
                # Compact/NonCompact
                _lambda_B4_15 = 3.76*(self.E/self.Fy)**0.5
                #
                # NonCompact/Slender
                _lambda_B4_15_r = 5.70*(self.E/self.Fy)**0.5
                #                
                #               
                # Web
                self.lambda_p_wF = _lambda_B4_15
                self.lambda_r_wF = _lambda_B4_15_r
                #
            #
            #
            # Singly Symetric or buil up I-shaped sections
            else:
                #
                # Table B4.1a
                #
                # CASE 2 <-----------------
                # Flanges of build-up I-shaped sections
                _lambda_B4_2 = 0.64*(self.Kc*self.E/self.Fy)**0.5
                #
                self.lambda_r_fE = _lambda_B4_2
                self.lambda_r_fE_flag = 'B4.2'                 
                #
                #
                # CASE 5 <-----------------
                # Webs of doubly symmetric I-shaped sections
                _lambda_B4_5 = 1.49*(self.E/self.Fy)**0.5        
                #
                self.lambda_r_wE = _lambda_B4_5
                self.lambda_r_wE_flag = 'B4.5'                
                #
                #               
                #
                #
                # Table B4.1b
                #
                # CASE 11 <-----------------
                # Flanges of doubly and singly symmetric I-shaped
                # build-up sections
                #
                # Compact/NonCompact
                _lambda_B4_11 = 0.38*(self.E/self.Fy)**0.5
                #
                # NonCompact/Slender
                _lambda_B4_11_r = 0.95*(self.Kc*self.E/self.FL)**0.5
                #
                #
                # CASE 13 <-----------------
                # Flanges of all I-shaped sections
                # Weak Axis
                #
                # Compact/NonCompact
                _lambda_B4_13 = 0.38*(self.E/self.Fy)**0.5
                # NonCompact/Slender
                _lambda_B4_13_r = 1.0*(self.E/self.Fy)**0.5        
                #
                #
                # Flange
                self.lambda_p_fF = _lambda_B4_11
                self.lambda_r_fF = _lambda_B4_11_r                  
                #                
                #
                #
                # Doubly symmetric I-shaped sections
                if self.SecSym == 'DOUBLY':
                    #
                    # CASE 15 <-----------------
                    # Webs of doubly symmetric I-shaped sections
                    #
                    # Compact/NonCompact
                    _lambda_B4_15 = 3.76*(self.E/self.Fy)**0.5
                    #
                    # NonCompact/Slender
                    _lambda_B4_15_r = 5.70*(self.E/self.Fy)**0.5
                    #
                    # Web
                    self.lambda_p_wF = _lambda_B4_15
                    self.lambda_r_wF = _lambda_B4_15_r                    
                    #
                #
                # Syngly symmetric I-shaped sections
                else:
                    #
                    # CASE 16 <-----------------
                    # Webs of single symmetric I-shaped sections
                    # (My is the yield moment determined with minimum
                    #  elastic modulus)
                    #
                    _lambda_B4_16 = (((self.hc/self.hp)*(self.E/self.Fy)**0.5)/
                                    (0.54*(self.Mpx/self.Mym)-0.09)**2)
                    #
                    #       Slenderness
                    _lambda_B4_16_r = 5.70*(self.E/self.Fy)**0.5
                    #
                    if _lambda_B4_16 > _lambda_B4_16_r:
                        _lambda_B4_16 = _lambda_B4_16_r
                    #  
                    # 
                    # Web
                    self.lambda_p_wF = _lambda_B4_16
                    self.lambda_r_wF = _lambda_B4_16_r                    
                    #                    
                #
                #                    
                #
                #
                #
                #                
                #
            #
            #
            #            
            #
        #
        #
        elif self.SectionType =='ANGLE':
            #
            # Table B4.1a 
            #
            # CASE 3 <-----------------
            # Legs of single angles
            #
            _lambda_B4_3 = 0.45*(self.E/self.Fy)**0.5
            #
            self.lambda_r_fE = _lambda_B4_3
            self.lambda_r_fE_flag = 'B4.3'
            #
            #
            # Table B4.1b
            #
            #
            # CASE 12 <-----------------
            # Leg of single angles
            #
            # Compact/NonCompact
            _lambda_B4_12 = 0.54*(self.E/self.Fy)**0.5
            #
            # NonCompact/Slender
            _lambda_B4_12_r = 0.91*(self.E/self.Fy)**0.5       
            #  
            #
            # Flange
            self.lambda_p_fF = lambda_B4_12
            self.lambda_r_fF = lambda_B4_12_r             
            #
            #
            #
            #
            #
        #
        #
        # Channel Sections
        elif self.SectionType =='CHANNEL':
            #
            # Table B4.1a 
            #
            # CASE 1 <-----------------
            # Flanges of Channels
            _lambda_B4_1 = 0.56*(self.E/self.Fy)**0.5
            #
            self.lambda_r_fE = _lambda_B4_1
            self.lambda_r_fE_flag = 'B4.1'
            #
            #
            # CASE 5 <-----------------
            # Webs of channels
            _lambda_B4_5 = 1.49*(self.E/self.Fy)**0.5        
            #        
            self.lambda_r_wE = _lambda_B4_5
            self.lambda_r_wE_flag = 'B4.5'
            #
            #
            # Table B4.1b
            #
            # CASE 10 <-----------------
            #
            # Flanges of Channels Main Axis
            #
            # Compact/NonCompact
            _lambda_B4_10 = 0.38*(self.E/self.Fy)**0.5
            #
            # NonCompact/Slender
            _lambda_B4_10_r = 1.0*(self.E/self.Fy)**0.5 
            #
            #
            # CASE 13 <-----------------
            # Flanges of channels about the weak axis
            # 
            # Compact/NonCompact
            _lambda_B4_13 = 0.38*(self.E/self.Fy)**0.5
            #
            # NonCompact/Slender
            _lambda_B4_13_r = 1.0*(self.E/self.Fy)**0.5        
            #    
            #
            # Flange
            self.lambda_p_fF = _lambda_B4_10
            self.lambda_r_fF = _lambda_B4_10_r             
            #
            #
            # CASE 15 <-----------------
            # Webs of channels
            # 
            # Compact/NonCompact
            _lambda_B4_15 = 3.76*(self.E/self.Fy)**0.5
            #
            # NonCompact/Slender
            _lambda_B4_15_r = 5.70*(self.E/self.Fy)**0.5
            #
            # Web
            self.lambda_p_wF = _lambda_B4_15
            self.lambda_r_wF = _lambda_B4_15_r            
            #
            # 
            #
        #
        #
        # Tee Sections
        elif self.SectionType =='TEE':
            #
            # Table B4.1a 
            #
            # CASE 1 <-----------------
            # Flanges of tees
            _lambda_B4_1 = 0.56*(self.E/self.Fy)**0.5
            #
            self.lambda_r_fE = _lambda_B4_1
            self.lambda_r_fE_flag = 'B4.1'            
            #
            # CASE 4 <-----------------
            # Stems of tees
            _lambda_B4_4 = 0.75*(self.E/self.Fy)**0.5
            #
            self.lambda_r_wE = _lambda_B4_4
            self.lambda_r_wE_flag = 'B4.4'
            #
            #
            # Table B4.1b
            #
            # CASE 10 <-----------------
            # Flanges of Tees
            #
            # Compact/NonCompact
            _lambda_B4_10 = 0.38*(self.E/self.Fy)**0.5
            #
            # NonCompact/Slender
            _lambda_B4_10_r = 1.0*(self.E/self.Fy)**0.5 
            #
            # Flange
            self.lambda_p_fF = _lambda_B4_10
            self.lambda_r_fF = _lambda_B4_10_r  
            #
            #
            # CASE 14 <-----------------
            # Steems of tees
            #
            # Compact/NonCompact
            _lambda_B4_14 = 0.84*(self.E/self.Fy)**0.5
            #
            # NonCompact/Slender
            _lambda_B4_14_r = 1.03*(self.E/self.Fy)**0.5  
            #
            # Web
            self.lambda_p_wF = lambda_B4_14
            self.lambda_r_wF = lambda_B4_14_r              
            #
        #            
        #
        # Rectangular HSS and boxes of
        # uniform thickness        
        elif self.SectionType =='BOX':
            #
            # Table B4.1a 
            # CASE 6 <-----------------
            # Walls of rectangular HSS and boxes of
            # uniform thickness             
            #
            _lambda_B4_6 = 1.40*(self.E/self.Fy)**0.5
            #
            self.lambda_r_fE = _lambda_B4_6
            self.lambda_r_fE_flag = 'B4.6' 
            #
            # Table B4.1b
            #
            # CASE 17 <----------------- 
            # Flanges of rectangular HSS and boxes of
            # uniform thickness
            #
            # Compact/NonCompact
            _lambda_B4_17 = 1.12*(self.E/self.Fy)**0.5
            # NonCompact/Slender
            _lambda_B4_17_r = 1.40*(self.E/self.Fy)**0.5               
            #
            # Flange
            self.lambda_p_fF = lambda_B4_17
            self.lambda_r_fF = lambda_B4_17_r             
            #
            #
            # CASE 19 <----------------- 
            # Webs of rectangular HSS and boxes of
            # uniform thickness
            #
            # Compact/NonCompact
            _lambda_B4_19 = 2.42*(self.E/self.Fy)**0.5
            # NonCompact/Slender
            _lambda_B4_19_r = 5.70*(self.E/self.Fy)**0.5               
            #    
            # Web
            self.lambda_p_wF = lambda_B4_19
            self.lambda_r_wF = lambda_B4_19_r              
            #
        #
        #
        # Flange cover plates and diaphram plates
        # between lines of fasterners or welds
        elif self.SectionType =='FLANGE_COVER_PLATE':
            #
            # Table B4.1a 
            # CASE 7 <-----------------
            #
            _lambda_B4_7 = 1.40*(self.E/self.Fy)**0.5 
            #
            self.lambda_r_fE = _lambda_B4_7
            self.lambda_r_fE_flag = 'B4.7'
            #
            #
            # Table B4.1b
            # CASE 18 <----------------- 
            #
            # Compact/NonCompact
            _lambda_B4_18 = 1.12*(self.E/self.Fy)**0.5
            #
            # NonCompact/Slender
            _lambda_B4_18_r = 1.40*(self.E/self.Fy)**0.5               
            #   
            # Flange
            self.lambda_p_fF = lambda_B4_18
            self.lambda_r_fF = lambda_B4_18_r 
            #
        #
        #
        # Circular & HSS
        elif self.SectionType =='TUBULAR':
            #
            #
            self.bt = self.Diameter/self.WallThickness
            self.htw = self.Diameter/self.WallThickness
            #
            # Table B4.1a 
            # CASE 9 <-----------------
            #
            _lambda_B4_9 = 0.11*(self.E/self.Fy)
            #
            self.lambda_r_fE = _lambda_B4_9
            self.lambda_r_fE_flag = 'B4.9' 
            #
            #
            # Table B4.1b
            # CASE 20 <----------------- 
            #
            # Compact/NonCompact
            _lambda_B4_20 = 0.07*(self.E/self.Fy)
            #
            # NonCompact/Slender
            _lambda_B4_20_r = 0.31*(self.E/self.Fy)
            #
            # Flange
            self.lambda_p_fF = lambda_B4_20
            self.lambda_r_fF = lambda_B4_20_r             
        #             
        #
        # All other elements
        else:
            #
            # Table B4.1a 
            #
            # CASE 3 <-----------------
            # all other unstiffened elements
            _lambda_B4_3 = 0.45*(self.E/self.Fy)**0.5
            #
            self.lambda_r_fE = _lambda_B4_3
            self.lambda_r_fE_flag = 'B4.3' 
            #
            #
            # CASE 8 <-----------------
            # All other stiffened elements
            _lambda_B4_8 = 1.49*(self.E/self.Fy)**0.5 
            #
            self.lambda_r_wE = _lambda_B4_8
            self.lambda_r_wE_flag = 'B4.8'            
            #
            #          
        #
        #
        # ---------------------------------------------
        #                 Select Class
        # ---------------------------------------------
        #
        #    ----- Table B4.1a  -----
        #
        #  Check Flange Slenderness Limit
        #
        if  self.bt <= self.lambda_r_fE :
            self.ClassFlangeE = 'NONSLENDER'
        #        
        else: 
            self.ClassFlangeE = 'SLENDER'
        #
        #
        #  Check Web Slenderness Limit
        #
        if  self.htw  <= self.lambda_r_wE : 
            self.ClassWebE = 'NONSLENDER'
        #        
        else: 
            self.ClassWebE = 'SLENDER'        
        #
        #
        #    ----- Table B4.1b  -----
        #
        #
        # Check if Flange compact
        if  self.bt_F <= self.lambda_p_fF : 
            self.ClassFlangeF = 'COMPACT'
        #        
        else: 
            self.ClassFlangeF = 'NONCOMPACT'
        #
        # Check if Flange slender
        if self.bt_F > self.lambda_r_fF:
            self.ClassFlangeF = 'SLENDER'
        #
        #
        # htw = self.hw / self.tw         
        #                       
        #  Check web if compact
        if self.htw <= self.lambda_p_wF: 
            self.ClassWebF = 'COMPACT'
        #        
        else: 
            self.ClassWebF = 'NONCOMPACT'
        #
        # Check web if slender
        if self.htw > self.lambda_r_wF : 
            self.ClassWebF = 'SLENDER'
        #
        #print ('ClassWebF',self.ClassWebF)
        #
        #   ++++++++ Section Compacness ++++++++
        #
        #    def AISC_SecCompacness(self):
        if self.ClassFlangeF == 'COMPACT' and self.ClassWebF =='COMPACT':
            self.SecComp ='COMPACT'
            #
        #
        elif self.ClassFlangeF == 'NONCOMPACT' or self.ClassWebF =='NONCOMPACT':
            self.SecComp = 'NONCOMPACT'
            #
        #
        elif self.ClassFlangeF == 'SLENDER' or self.ClassWebF =='SLENDER':
            self.SecComp = 'SLENDER'
        #
        else: self.SecComp = 'N/A'
        #
        #        print (' ')
        #        print ('Section Compacness :',self.SecComp)
    #    
    # 
    #-------------------------------------------------
    #
    #            ++++++ Chapter D  ++++++ 
    #
    def ChapterD(self):
        print (' ')
        print ('-----------------------------')
        print ('     Chapter D - Tension')
        print (' ')
        # self.L=L
        #
        #if self.Fu == 0:
        #    self.Fu = self.Fy/0.75        
        #
        # D1 Slender Limitation
        _L_rx = self.Lx / self.rx
        _L_ry = self.Ly / self.ry
        #
        self.L_r = max(_L_rx, _L_ry)
        #        
        if self.L_r < 300 : print (("L/r ("+"%6.2f"+" )< 300")%(self.L_r))
        #        
        else: print ('FAIL')
        #        
        # D3 Effective Net Area
        _U_D3 = 1.0
        _Ae_D3_1 = self.An * _U_D3
        #        print 'Ae_D3_1 =',Ae_D3_1
        #
        # D2 Tensile Strength
        # 
        if self.DesignMethod == 'ASD':
            # a)
            _ASD_D2a = 1.67
            _LRFD_D2a = 1.0
            # b)
            _ASD_D2b = 2.0
            _LRFD_D2b = 1.0
            #
        #
        elif self.DesignMethod == 'USER_DEFINED':
            # a)
            _ASD_D2a = self.UserFactorT
            _LRFD_D2a = 1.0
            # b)
            _ASD_D2b = self.UserFactorT
            _LRFD_D2b = 1.0 
            #
        #
        else:
            # a)
            _ASD_D2a = 1.0
            _LRFD_D2a = 0.9
            # b)
            _ASD_D2b = 1.0
            _LRFD_D2b = 0.75
            #
        #
        # a) For tensile yielding i the gross section
        _Pn_D2a = (self.Fy * self.A)
        _Pn_D2a_Flag ='(D2-1)'
        #print 'a)',Pn_D2a, Pn_D2a_Flag 
        #
        #
        # b) For tensile rupture in the net section       
        _Pn_D2b = (self.Fu * _Ae_D3_1)
        _Pn_D2b_Flag ='(D2-2)'
        # print 'b)',Pn_D2b, Pn_D2b_Flag
        #
        # Take the lower value
        if (_Pn_D2a*(_LRFD_D2a / _ASD_D2a)) < (_Pn_D2b *(_LRFD_D2b / _ASD_D2b)) :
            #
            self.Pn_D = _Pn_D2a
            self.OmegaT = _ASD_D2a
            self.PhiT = _LRFD_D2a 
            self.Pn_D_Flag = _Pn_D2a_Flag
        #
        else:
            self.Pn_D = _Pn_D2b
            self.OmegaT = _ASD_D2b
            self.PhiT = _LRFD_D2b
            self.Pn_D_Flag = _Pn_D2b_Flag
            #
        # print ('LRFD :',LRFD, '  ASD:',ASD)
        print (("Pn ="+ "%8.2f"+" kN "+ "%-6s" )%(self.Pn_D/1000, self.Pn_D_Flag))
        print (' ')
    #        return self.Pn_D
    #      
    #
    #-------------------------------------------------
    #
    #             ++++++ Chapter E  ++++++ 
    #
    def ChapterE(self):
        print (' ')        
        print ('-----------------------------')
        print ('   Chapter E - Compression')
        print (' ')
        #
        # Stability Factors
        # Kx = Kx
        # Ky = Ky       
        # Kz = Kz
        print (("Kx = "+"%2.3f")%(self.Kx))
        print (("Ky = "+"%2.3f")%(self.Ky))
        print (("Kz = "+"%2.3f")%(self.Kz))
        #
        # self.StiffnedElement = 'NO'
        #        
        #_Fcr_E = 0.0
        #_Fcr_E_flag = 'N/A'
        #
        # E1 General Previsions
        if self.DesignMethod == 'ASD' :
            self.OmegaC = 1.67
            self.PhiC = 1.0
        #
        elif self.DesignMethod == 'USER_DEFINED' :
            self.OmegaC = self.UserFactorC
            self.PhiC = 1.0
        #
        else:
            self.OmegaC=1.0
            self.PhiC = 0.90
        #
        #       E2 Efective Length
        #
        _kLrx = self.Kx*self.Lx/self.rx
        _kLry = self.Ky*self.Ly/self.ry
        #
        if _kLrx > _kLry: self.KLr = _kLrx
        #        
        else: self.KLr = _kLry
        #
        if self.KLr > 200: print (("WARNING KLr ("+"%2.3f"+") > 200")%(self.KLr))
        #        
        else: print (("KLr ("+"%2.3f"+") < 200")%(self.KLr))
        #
        #       --------------------------------------------------------
        #                               CONSTANTS
        #       --------------------------------------------------------
        #            
        # Coordinate of Shear Centre with respect to
        # the centroid (_xo & yo)
        _xo = self.SCx
        _yo = self.SCy
        #
        # Polar radius of gyration about the shear centre
        _ro2 = (_xo**2 + _yo**2 + ((self.Ix + self.Iy)/self.Ag))
        #
        # (E4-10) Flexural Constant H
        _H_E4_10 = 1.0 - ((_xo**2 + _yo**2)/_ro2)
        #
        # (E4-7) Elastic Flexural Buckling Stress Major Axis
        self.Fex_E4_7 = (self.E*math.pi**2)/(_kLrx)**2
        #
        # (E4-8) Elastic Flexural Buckling Stress Minor Axis
        self.Fey_E4_8 = (self.E*math.pi**2)/(_kLry)**2
        #        
        # (E4-9) Elastic Torsional Buckling
        self.Fez_E4_9 = ((((self.Cw*self.E*math.pi**2)/(self.Kz*self.Lzt)**2)+
                     (self.G*self.J))/(self.Ag*_ro2))
        #       Start Q
        self.Q =1.0
        #
        # Elastic Buckling Stress Fe
        # --------------------------        
        #
        # (E3-4)
        #  Fe determined according to Ecu E3-4, as especified
        #  in Appendix 7.2.3(b), or through an elastic buckling
        #  analysis as applicable.
        _Fe_E3_4 = (self.E*math.pi**2)/(self.KLr)**2        
        #
        # E-4 the critical stres Fcr is determined as follows:
        #        
        # a) For double angle and tee-shaped compression members
        # (E4-2) No yet implemented
        # (E4-3) No yet implemented
        #        
        # b) For all other cases, Fcr shal be determined according
        #    to Ecu E3-2 or E3-3, using the torsional or flexural-
        #    torsional elastic buckling stress Fe as follows:
        #        
        # (i) For doubly symmetric members
        # (E4_4)
        _Fe_E4_4 = ((((self.Cw*self.E*math.pi**2)/(self.Kz*self.Lzt)**2)+
                    (self.G*self.J))*(1.0/(self.Ix + self.Iy)))        
        #
        # (ii) For singly symmetric members where y is the axis of
        #      symmetry
        # (E4_5)
        _Fe_E4_5 = (((self.Fey_E4_8 + self.Fez_E4_9)/(2*_H_E4_10))*
                 (1.0-(1-((4.0*self.Fey_E4_8*self.Fez_E4_9*_H_E4_10)/
                          (self.Fey_E4_8 + self.Fez_E4_9)**2))**0.5))
        #
        #
        # --------------------------------------------------------
        #                    END CONSTANTS
        # --------------------------------------------------------
        #
        #
        # E3 & E4 Members Without Slender Elements        
        if self.ClassFlangeE != 'SLENDER' and self.ClassWebE != 'SLENDER':
            #
            # E5 Single Angle Compression Members
            if self.SectionType =='Single-Angle':
                print ('Channel Members not implemented yet')
                # 
            #
            #       E6 Built-Up Members
            elif self.SectionType =='Built-Up':
                print ('Built-Up Members not implemented yet')
                #
            # E3 & E4 Chapters
            else:
                # E3 Flexural Buckling of Members Without slender
                #    Elements and Lz < Lb
                if self.Lzt <= self.Lb:
                    #                
                    # This section applies to compression members without
                    # slender elements as defined in Sec B4.1
                    #                
                    # USER NOTE : When the torsional unbraced length is
                    #             larger than the lateral unbraced length, 
                    #             Section E4 may control the design of wide 
                    #             flange and similarly shaped columns.
                    #
                    # The Critical Stress,Fcr is determined as follows:
                    #                
                    # b) (E3-3)
                    if self.KLr > 4.71*(self.E/self.Fy) and (self.Fy/_Fe_E3_4) > 2.25:
                        _Fcr_E3 = 0.877*_Fe_E3_4
                        _Fcr_E3_flag = '(E3-3)'
                        self.Fe = _Fe_E3_4
                        self.Fe_flag ='(E3-4)'
                        #    
                    #
                    # a) (E3-2)
                    else:
                        _Fcr_E3 = (0.658**(self.Fy/_Fe_E3_4))*self.Fy
                        _Fcr_E3_flag = '(E3-2)'
                        self.Fe = _Fe_E3_4
                        self.Fe_flag = '(E3-4)'                        
                        #
                    #
                    #(E3-4) Elastic Buckling Stress Fe               
                    # _Fe_E3_4_flag = '(E3-4)'
                    # print 'Fe :',_Fe_E3_4, ' ',_Fe_E3_4_flag
                    #
                    # print 'Fcr :',_Fcr_E3,' ',_Fcr_E3_flag
                    #
                    # The Nominal Compressive Strength, Pn shal be determined
                    # based on the limit state of flexural buckling
                    # E3-1  
                    self.Fcr_E = _Fcr_E3
                    self.Fcr_E_flag = _Fcr_E3_flag
                    self.Pn_E = _Fcr_E3*self.Ag
                    self.Pn_E_flag = '(E3)'
                    # print 'Pn Max=',self.Pn_E, self.Pn_E_flag
                #
                # E4 Torsional and Flexural-torsional Buckling of
                #    Members without Slender Elements and Lz > Lb            
                else:
                    # This section applies to singly symmetric and
                    # unsymmeric members and certain doubly symmetric
                    # members, such as cruciform or buil-up columns
                    # without slender elements.
                    # 
                    # The critical stress, Fcr is determined as follows:
                    #                
                    # (E4-3a) For double Angle and Tee-Shaped Members
                    if self.SectionType =='Double-Angle' or self.SectionType =='T':
                        print ('Double-Anle & T Members not implemented yet')
                        #
                    #
                    # (E4-3b) For All Other Cases
                    else:
                        # Fcr shall be determined according to Ecu
                        # E3-2 or E3-3, using the torsional or 
                        # flexural-torsional elastic buckling stress
                        # Fe determine as follows:    
                        #                 
                        # i) For Doubly Symmetric Members
                        if self.SecSym == 'DOUBLY':
                            #                        
                            _Fe_E4 = _Fe_E4_4
                            #_Fe_E4_flag = '(E4-4)'
                            self.Fe = _Fe_E4_4
                            self.Fe_flag ='(E4-4)'                                     
                            #
                        #
                        # ii) For Syngle Symmetric Members where y is
                        #     the axis of symmetry
                        elif self.SecSym == 'SINGLY':
                            #                        
                            _Fe_E4 = _Fe_E4_5
                            #_Fe_E4_flag = '(E4-5)'
                            self.Fe = _Fe_E4_5
                            self.Fe_flag ='(E4-5)'    
                            #
                        #
                        # iii) For Unsymmetric Members, Fc is the lowest 
                        #      root of the cubic equation 
                        else: print ('Unsymmetric MembersNo Yet Defined')
                        #
                        #  The Critical Stress,Fcr is determined as follows:
                        #                
                        #  b) (E3-3)
                        if self.KLr > 4.71*(self.E/self.Fy) and (self.Fy/_Fe_E4) > 2.25:
                            _Fcr_E4 = 0.877*_Fe_E4
                            _Fcr_E4_flag = '(E3-3)'
                        #                    
                        #  a) (E3-2)
                        else:
                            _Fcr_E4 = (0.658**(self.Fy/_Fe_E4))*self.Fy
                            _Fcr_E4_flag = '(E3-2)'       
                            #
                    #-------
                    # The Nominal Compressive Strength, Pn shall be
                    # determined based on the limit states of flexural
                    # torsional and torsional buckling.
                    # E4-1    
                    self.Fcr_E = _Fcr_E4
                    self.Fcr_E_flag = _Fcr_E4_flag    
                    self.Pn_E = _Fcr_E4*self.Ag
                    self.Pn_E_flag = '(E4-1)'
                    # print 'Pn max=',self.Pn_E, self.Pn_E_flag
        #                
        # E7 Members with Slender Elements
        else:
            # This section applies to compression members with 
            # slender elements, as defined in Sec B4.1.
            #
            _Qa = 1.0
            _Qs = 1.0
            #            
            _bt = (max((self.bfc/self.tfc),(self.bft/self.tft)))
            # print 'b/t =',_bt,'<=',0.56*(self.E/self.Fy)**0.5,(1.03*(self.E/self.Fy)**0.5)
            #
            # (E7.1) Slender Unstiffened Elements _Qs            
            # if Self.Stiffned == 'NO':
            if self.StiffnedElement == 'NO':  
                # The reduction fator _Qs for slender unstiffened elements
                # is defined as follows:
                # 
                # (E7.1c) For Single Angles
                if self.SectionType =='ANGLE':
                    #                    
                    # i)
                    if _bt <= 0.45*(self.E/self.Fy)**0.5:
                        _Qs = 1.0
                        _Qs_Flag = '(E7-10)'
                    #
                    # ii)
                    elif _bt > (0.45*(self.E/self.Fy)**0.5) and _bt < (0.91*(self.E/self.Fy)**0.5):
                        _Qs = (1.34-(0.76*_bt*(self.Fy/self.E)**0.5))
                        _Qs_Flag = '(E7-11)'
                    #
                    # iii )
                    else:
                        _Qs = (0.53*self.E)/(self.Fy*_bt**2)
                        _Qs_Flag = '(E7-12)'
                #
                # (E7.1d) For Stems of Tees
                elif self.SectionType =='T':
                    #
                    # i)
                    if _bt <= 0.75*(self.E/self.Fy)**0.5:
                        _Qs = 1.0
                        _Qs_Flag = '(E7-13)'
                    #
                    # ii)
                    elif _bt > (0.75*(self.E/self.Fy)**0.5) and _bt < (1.03*(self.E/self.Fy)**0.5):
                        _Qs = (1.908-(1.22*(self.d/self.tw)*(self.Fy/self.E)**0.5))
                        _Qs_Flag = '(E7-14)'
                    #
                    # iii )
                    else:
                        _Qs = (0.69*self.E)/(self.Fy*(self.d/self.tw)**2)
                        _Qs_Flag = '(E7-12)'
                #                
                # (E7.1a) & (E7.1b) For other compression members
                else :
                    # (E7.1a) For flanges, angles and Plates projecting from 
                    # rolled coulms or other compresion members                    
                    if self.Build == 'ROLLED':
                        #                    
                        # i)
                        if _bt <= 0.56*(self.E/self.Fy)**0.5:
                            _Qs = 1.0
                            _Qs_Flag = '(E7-4)'
                        #
                        # ii)
                        elif _bt > (0.56*(self.E/self.Fy)**0.5) and _bt < (1.03*(self.E/self.Fy)**0.5):
                            # print 'here',_bt,self.Fy,self.E,(self.Fy/self.E)**0.5
                            _Qs = (1.415-(0.74*_bt*(self.Fy/self.E)**0.5))
                            _Qs_Flag = '(E7-5)'
                        #
                        # iii )
                        else:
                            _Qs = (0.69*self.E)/(self.Fy*_bt**2)
                            _Qs_Flag = '(E7-6)'
                            #
                    #                        
                    # (E7.1b) For flanges, angles and plates projecting from build-up
                    # columns or other compression memebers (welded)
                    else:
                        #
                        # i)   (E7-7)
                        if _bt <= 0.64*(self.Kc*self.E/self.Fy)**0.5:
                            _Qs = 1.0
                            _Qs_Flag = '(E7-7)'
                        #                        
                        # ii)  (E7-8)
                        elif _bt > (0.64*(self.Kc*self.E/self.Fy)**0.5) and _bt < (1.17*(self.Kc*self.E/self.Fy)**0.5):
                            _Qs = (1.415-(0.65*_bt*(self.Fy/(self.E*self.Kc))**0.5))
                            _Qs_Flag = '(E7-8)'
                        #                        
                        # iii ) (E7-9)
                        else:
                            _Qs = (0.90*self.E*self.Kc)/(self.Fy*_bt**2)
                            _Qs_Flag = '(E7-9)'
            #-----------
            # 
            # (E7.2) Slender Stiffened Elements _Qa
            else:
                # The reduction factor _Qa, for slender stiffened
                # elements is defined as follows:                
                # 
                # Where:  
                # f = Pn/Ae                    
                _f = self.Fy                
                #                
                # (E7.2b) For flanges of square and rectangular sections
                #         of uniform thickness
                if self.SectionType =='BOX':                    
                    # 
                    # USER NOTE: In lieu of calculating f = Pn/Ae, which
                    # requires iteration, f may be taken as Fy, this will 
                    # result slightly conservative estimate of column
                    # capacity
                    #                    
                    # Where:  
                    # f = Pn/Ae                    
                    #_f = self.Fy
                    _tw = min(self.TL, self.TR)
                    _tf = min(self.TT, self.TB)
                    #  
                    if (self.d/_tw) > 1.40*(self.E/_f) and (self.B/_tf) > 1.40*(self.E/_f):
                        # (E7-18)
                        # Webs
                        _be_tw = ((1.92*_tw*(self.E/_f)**0.5)*
                                  (1-(0.38/(self.d/_tw))*(self.E/_f)**0.5))
                        #
                        _be_H = min(_be_tw, self.d)
                        #
                        # Flanges
                        _be_tf = ((1.92*_tf*(self.E/_f)**0.5)*
                                  (1-(0.38/(self.B/_tf))*(self.E/_f)**0.5))
                        #
                        _be_B = min(_be_tf, (self.B-self.TL-self.TR))
                        #
                        # (1-(0.38/_bt)*(self.E/f)**0.5))
                        # be_E7 = min (be_E7,self.bfc)
                        #print ('Box section no defined yet')
                        _Ae_E7 = 2*(_be_H*_tw)+2*(_be_B*_tf)
                        #_Ae_flag = '(E7-18)'
                    #
                    else:
                        print ('*******************************')
                        print ('(E7-18)' )
                        if (self.d/_tw) < 1.40*(self.E/_f):
                            print ('ERROR H-b/t (',(self.d/_tw),') < 1.40*E/f (',1.49*(self.E/f),')')
                            #
                        else:
                            print ('ERROR B-b/t (',(self.B/_tf),') < 1.40*E/f (',1.49*(self.E/f),')')
                            #
                        print ('*******************************')                        
                #---
                #
                # (E7.2c) For axial loaded circular section
                elif self.SectionType =='CIRCULAR':
                    #
                    if 0.11*(self.E/self.Fy) < (self.od/self.wt) and (self.od/self.wt) < 0.45*(self.E/self.Fy):
                        #
                        _Q_circular = ((0.38*self.E)/(self.Fy*(self.od/self.wt))) + (2.0/3.0)
                        _Ae_E7 = self.A
                        #_Ae_flag = '(E7-19)'
                        #
                    #
                    else:
                        print ('*******************************')
                        print ('(E7-19)' )
                        if (self.od/self.wt) < 0.45*(self.E/self.Fy):
                            print ('WARNING D/t (',(self.od/self.wt),') > 0.45*E/Fy (',0.45*(self.E/self.Fy),')')
                        else:
                            print ('WARNING D/t (',(self.od/self.wt),') < 0.11*E/Fy (',0.11*(self.E/self.Fy),')')
                        print ('*******************************')                        
                #---
                #
                # (E7.2a) For uniformly compressed slender elements except 
                # flanges of square and rectangular sections of 
                # uniform thickness
                else:
                    #
                    #
                    # f is taken as Fcr with Fcr based on Q = 1.0    
                    #                    
                    # The critical stress, Fcr shall be determined as follows:
                    # b)
                    if self.KLr > 4.71*(self.E/(self.Fy))**0.5 and (self.Fy/_Fe_E7) > 2.25:
                        _f = 0.877*_Fe_E7
                    #
                    # a)
                    else: _f = (0.658**(self.Fy/_Fe_E7))*self.Fy
                    # 
                    #
                    #
                    if _bt >= 1.49*(self.E/_f):
                        #
                        # Compression flange
                        _bt = (self.bfc/self.tfc)                    
                        _be_bfc = ((1.92*self.tfc*(self.E/_f)**0.5)*
                                 (1-(0.34/_bt)*(self.E/_f)**0.5))
                        _be_bfc = min (_be_bfc,self.bfc)
                        #
                        # Tension flange
                        _bt = (self.bft/self.tft)
                        _be_bft = ((1.92*self.tft*(self.E/_f)**0.5)*
                                 (1-(0.34/_bt)*(self.E/_f)**0.5))
                        _be_bft = min (_be_bft,self.bft)
                        # 
                        # Web
                        _bt = (self.hw/self.tw)
                        _be_hw = ((1.92*self.tw*(self.E/_f)**0.5)*
                                 (1-(0.34/_bt)*(self.E/_f)**0.5))
                        _be_hw = min (_be_hw,self.hw)
                        # 
                        _Ae_E7 = (_be_bfc*self.tfc)+(_be_bft*self.tft)+(_be_hw*self.tw)
                        #_Ae_flag = '(E7-17)'              
                    #
                    else:
                        print ('*******************************')
                        print ('(E7-17)' )
                        print ('ERROR _bt (',_bt,') < 1.49*E/f (',1.49*(self.E/_f),')')
                        print ('*******************************')
                #
                #                    
                # _Qa --> Reduction Factor
                _Qa = _Ae_E7/self.Ag
            # print '_Qa = ',_Qa
            #   
            #
            if self.SectionType == 'CIRCULAR':
                self.Q  = _Q_circular
                #
            # Net Reduction Factor Accounting for all Slender compression elements                
            else:
                self.Q = _Qs*_Qa
                #
            #
            # Q=0.89
            if self.SectionType =='CIRCULAR':
                #
                print ("Circular Section")
                print (("Q =  "+"%2.3f"+"%8s")%(self.Q, _Qs_Flag))
                #
            #
            else:
                print (("_Qa = "+"%2.3f")%(_Qa))
                print (("_Qs = "+"%2.3f")%(_Qs))
                print (("Q =  "+"%2.3f"+"%8s")%(self.Q, _Qs_Flag))
            #print ("------------------")
            #print ("self.SecSym", self.SecSym)
            #print ( self.Lzt," <= ",self.Lb )
            #print (_Fe_E3_4, _Fe_E4_4, _Fe_E4_5 )
            #
            # Select elastic buckling stress Fe using Equations:
            # Ecu E3-4 and E4-4 for doubly symmetric members with Lz < Lb      
            if self.SecSym == 'DOUBLY' and self.Lzt <= self.Lb:
                _Fe_E7 = _Fe_E3_4
                _Fe_E7_flag = '(E3-4)'
                self.Fe = _Fe_E3_4
                self.Fe_flag ='(E3-4)'                
            #
            # Ecu E3-4, E4-5 for Singly and E4-6 for unsimmetric members
            else:
                # 
                # i) For Doubly Symmetric Members
                #    elastic buckling stress, calculated using Equations 
                #    E3-4 and E4-4 for doubly symmetric members                  
                if self.SecSym == 'DOUBLY':         
                    #
                    _Fe_E7 = min(_Fe_E3_4, _Fe_E4_4)
                    #
                    if _Fe_E7 == _Fe_E3_4:
                        _Fe_E7_flag= '(E3-4)'
                    #
                    else:
                        _Fe_E7_flag = '(E4-4)'
                    #
                    self.Fe = min(_Fe_E3_4,_Fe_E4_4)
                    #
                    if self.Fe == _Fe_E3_4:
                        self.Fe_flag = '(E3-4)'
                    #
                    else:
                        self.Fe_flag ='(E4-4)'  
                        #
                #---
                # ii) For Syngle Symmetric Members where y is the axis of symmetry
                #     Equations E3-4 and E4-5 for singly symmetric members
                elif self.SecSym == 'SINGLY':
                    #
                    _Fe_E7 = min(_Fe_E3_4, _Fe_E4_5)
                    if _Fe_E7 == _Fe_E3_4:
                        _Fe_E7_flag = '(E3-4)'
                    #
                    else:
                        _Fe_E7_flag = '(E4-5)'
                    #
                    self.Fe = min(_Fe_E3_4, _Fe_E4_5)
                    if self.Fe == _Fe_E3_4:
                        self.Fe_flag ='(E3-4)'
                    #
                    else:
                        self.Fe_flag ='(E4-5)'
                        #
                # 
                # iii) For Unsymmetric Members, Fc is the lowest root of the
                #      cubic equation 
                else: print ('Unsymmetric Members No Yet Defined')
                #
            #
            # The critical stress, Fcr shall be determined as follows:
            # b)
            if self.KLr > 4.71*(self.E/(self.Q*self.Fy))**0.5 and (self.Q*self.Fy/_Fe_E7) > 2.25:
                _Fcr_E7 = 0.877*_Fe_E7
                _Fe_E7_flag = '(E7-3)'
                #                print '_Fcr_E7 ',_Fcr_E7,' Ecu', _Fe_E7_flag 
            #
            # a)
            else:
                _Fcr_E7 = self.Q*(0.658**(self.Q*self.Fy/_Fe_E7))*self.Fy
                _Fe_E7_flag = '(E7-2)'
                #                print '_Fcr_E7 ',_Fcr_E7,' Ecu', _Fe_E7_flag 
            #
            # The Nominal Compressive Strength,Pn shall be the lowest
            # value based on the applicable limit states of flexural
            # buckling (E-3), torsional and flexural-torsional buckling (E-4)
            #     
            self.Fcr_E = _Fcr_E7
            self.Fcr_E_flag = _Fe_E7_flag
            self.Pn_E = _Fcr_E7*self.Ag
            self.Pn_E_flag = '(E-7)'
            # print 'Pn =', Pn_E,_Fe_E7_flag
            # print 'Fcr =',_Fcr_E7,_Fe_E7_flag
            # print 'Pn max =',self.Pn_E
        #
        #
        #
        #self.Pn_E = self.Pn_E*(self.PhiC/self.OmegaC)
        print (("Fe  = "+"%-6.0f"+" N/mm2 "+"%-8s")%(self.Fe, self.Fe_flag))
        print (("Fcr = "+"%-6.0f"+" N/mm2 "+"%-8s")%(self.Fcr_E,self.Fcr_E_flag))
        # print ('Pn max=',self.Pn_E, self.Pn_E_flag)
        print (("Pn  = "+"%-8.3f"+"  kN "+"%-8s")%((self.Pn_E/1000), self.Pn_E_flag))
        print (" ")
    #
    #   
    #
    #-------------------------------------------------
    #
    #             ++++++ Chapter F  ++++++ 
    #
    def ChapterF(self):
        print (" ")        
        print ("-----------------------------")
        print ("     Chapter F - Flexion ")
        print (" ") 
        #
        # ho=(self.d-0.5*self.tfc-0.5*self.tft)
        # print 'ho = ',ho
        #
        # F1 General Provisions
        #
        # The design flexural strength and the allowable
        # flexural strength shal be determined as follows:
        #
        # (1) For all provisions in this chapter
        if self.DesignMethod == 'ASD':
            self.OmegaB=1.67
            self.PhiB = 1.0
        #
        elif self.DesignMethod == 'USER_DEFINED':
            self.OmegaB = self.UserFactorB
            self.PhiB = 1.0 
        #
        else:
            self.OmegaB=1.0
            self.PhiB = 0.9
        # 
        # (2) The provision in this chapter are based on
        # the assumtion that points of support for
        # beams and girders are restrained agains
        # rotation about their longitudinal axis
        # 
        # (3) For singly symmetric members in single
        #     curvature and all doubly symmetric members  
        #            
        # Cb = Lateral-Torsional Buckling Factor for 
        #      nonuniform moment diagrams when both ends
        #      are braced    
        print (("Cb = "+"%-4.3f")%(self.Cb))
        #
        #
        # F2 to F11 Symmetric Shapes 
        if self.SecSym != 'UNSYMMETRIC':
            # 
            if self.SectionType =='I' or self.SectionType =='CHANNEL':   
                #
                # F2 Doubly symmetric compact channel members bent
                #    about their major axis 
                if self.SectionType == 'CHANNEL': 
                    print ('Channel Section On')                     
                    #
                    # This section applies to doubly symmetric I-shaped
                    # and channel bent aout their major axis, having
                    # compact flanges as defined in Sec B4.1 for flexure
                    #                               
                    # (F2-5) Limiting Laterally Unbraced Length Lp
                    #        for Full Plastic Flexural Stregth
                    self.Lp = 1.76*self.ry*(self.E/self.Fy)**0.5
                    self.Lp_flag ='(F2-5)'
                    #                
                    # (F2-7) Efective Radius of Gyration rts for
                    #        Mayor Axis Bending of Doubly 
                    #        Simetric Compact I Beam
                    _rts1=(self.bfc/
                         (12*(1+(1/6)*(self.hw*self.tw/
                                       (self.bfc*self.tfc))))**0.5)
                    #
                    _rts2=(((self.Cw*self.Iy)**0.5)/self.Sx)**0.5
                    #
                    _rts = min(_rts1,_rts2)
                    # print 'rts =',_rts
                    #
                    #               
                    # (F2-8) Coefficient c is determined:
                    #                
                    # b) For Channels
                    _c = (ho/2)*(self.Iy/Cw)
                    # print 'c =',_c
                    #
                    # F2.2 Lateral-Torsional Buckling
                    #                
                    # (F2-6) Limiting Laterally Unbraced Length 
                    #        Lr for Inelastic Lateral-Torsional
                    #        Buckling
                    self.Lr=(1.95*_rts*(self.E/(0.7*self.Fy))*
                        ((self.J*_c/(self.Sx*self.ho))+
                         ((self.J*_c/(self.Sx*self.ho))**2+
                          (6.76*(0.7*self.Fy/self.E)**2))**0.5)**0.5)
                    #                
                    self.Lr_flag = '(F2-6)'
                    #
                    # Lc -> Maximum Unbraced Length of the compression flange
                    #       at which the allowable bending stress is 0.66Fy
                    Lc=min(self.Lr,self.Lp)
                    #
                    # Checking Length of the Beam
                    #
                    if self.Lx == 0: self.Lx = Lc
                    #
                    if self.Ly == 0: self.Ly = self.Lx 
                    #
                    if self.Lb == 0: self.Lb = max(self.Lx, self.Ly)
                    #  
                    #
                    # (F2-4)
                    # Critical Stress Fcr
                    _Fcrx = (((self.Cb*self.E*math.pi**2)/(self.Lb/_rts)**2)*
                            (1+0.078*(self.J*_c/(self.Sx*self.ho))*
                             (self.Lb/_rts)**2)**0.5)
                    # print 'Fcr =',_Fcrx
                    # 
                    #
                    # F2.1 Yielding
                    _Mn_F2_1 = self.Fy*self.Zx
                    _Mn_F2_1_flag = '(F2-1)'
                    #  
                    # F2.2 Lateral-Torsional Buckling
                    #
                    # a) When Lb < Lp the limit state of lateral-torsional
                    #    buckling does not apply
                    if self.Lb <= self.Lp:
                        # F2.1 Yielding
                        _Mn_F2_2 = _Mn_F2_1
                        _Mn_F2_2_flag = '(F2.2a)'
                    #
                    # b) When Lp < Lb < = Lr
                    elif  self.Lp < self.Lb and self.Lb <= self.Lr:
                        _Mn_F2_2 = (self.Cb*(self.Mpx-(self.Mpx-0.7*self.Fy*self.Sx)*
                                       ((self.Lb-self.Lp)/(self.Lr-self.Lp))))
                        _Mn_F2_2_flag = '(F2.2b)'
                    #                
                    # c) When Lb > Lr
                    else:
                        _Mn_F2_2 = (_Fcrx*self.Sx)
                        _Mn_F2_2_flag = '(F2.2b)'
                    #
                    #
                    # F2 The nominal  flexural strength, Mn shall be the
                    #    lower value obtained according to the limit states
                    #    of yielding (platic moment) and lateral torsional
                    #    buckling
                    #
                    self.Mnx = min(_Mn_F2_1,_Mn_F2_2)
                    # flag selection                
                    if self.Mnx == _Mn_F2_1: self.Mnx_flag = _Mn_F2_1_flag
                    #                
                    else:self.Mnx_flag = _Mn_F2_2_flag
                #
                #
                # F2 to F5 I-shaped members bent about their major axis                                        
                else:
                    #                    
                    # F2 & F3 Doubly symmetric I-shaped members bent about 
                    #         their major axis 
                    if self.SecSym == 'DOUBLY' and self.ClassWebF == 'COMPACT' :        
                        #
                        # This section applies to F2 & F3
                        #                        
                        # (F2-5) Limiting Laterally Unbraced Length Lp
                        #        for Full Plastic Flexural Stregth
                        self.Lp = 1.76 * self.ry * math.sqrt(self.E/self.Fy)
                        #
                        self.Lp_flag = '(F2-5)'
                        #                
                        # (F2-7) Efective Radius of Gyration rts for
                        #        Mayor Axis Bending of Doubly 
                        #        Simetric Compact I Beam
                        _rts1 = (self.bfc/
                                 math.sqrt(12*(1+(1/6)*(self.hw*self.tw/
                                               (self.bfc*self.tfc)))))
                        #
                        _rts2 = math.sqrt((math.sqrt(self.Cw * self.Iy)) / self.Sx)
                        #
                        _rts = min(_rts1, _rts2)
                        #
                        _rts_flag = '(F2-7)'
                        #print 'rts =',_rts, rts_flag
                        #      
                        # (F2-8) Coefficient c is determined:
                        #                
                        # a) For doubly symmetric I-Shapes
                        _c = 1
                        #
                        _c_flag ='(F2-8a)'
                        # print 'c =',_c,c_flag
                        #                
                        # (F2-6) Limiting Laterally Unbraced Length 
                        #        Lr for Inelastic Lateral-Torsional
                        #        Buckling
                        self.Lr = (1.95 * _rts * (self.E / (0.7 * self.Fy)) *
                                   math.sqrt((self.J * _c / (self.Sx * self.ho)) +
                                             math.sqrt((self.J * _c / (self.Sx * self.ho))**2 +
                                                       (6.76 * (0.7 * self.Fy / self.E)**2))))
                        #                
                        self.Lr_flag = '(F2-6)'
                        #
                        # Lc -> Maximum Unbraced Length of the compression flange
                        #       at which the allowable bending stress is 0.66Fy
                        #Lc = max(Lr,Lp)
                        #
                        # Checking Length of the Beam
                        #
                        #if self.Lx == 0: self.Lx = Lc
                        #                            
                        #if self.Ly == 0: self.Ly = Lc
                        #                            
                        #if self.Lzt == 0: self.Lzt = Lc  
                        #                            
                        #if self.Lb == 0: self.Lb = Lc 
                        #
                        #print ('Lb =====>', self.Lb)
                        #               
                        # (F2-4) Critical Stress Fcr
                        self.Fcrx = (((self.Cb * self.E * math.pi**2) / (self.Lb / _rts)**2) *
                                     math.sqrt(1 + 0.078 * (self.J * _c / (self.Sx * self.ho)) *
                                               (self.Lb / _rts)**2))
                        #
                        self.Fcrx_flag ='(F2-4)'
                        # print 'Fcr =',_Fcrx,Fcrx_flag
                        #
                        # F2.1 Yielding
                        _Mpx = self.Fy * self.Zx
                        _Mn_F2_1 = _Mpx
                        _Mn_F2_1_flag = '(F2-1)'
                        #  
                        # F2.2 Lateral-Torsional Buckling
                        #
                        # a) When Lb < Lp the limit state of lateral-torsional
                        #    buckling does not apply
                        if self.Lb <= self.Lp:
                            # F2.1 Yielding
                            _Mn_F2_2 = _Mn_F2_1
                            _Mn_F2_2_flag = '(F2.2a)'
                            #
                        #
                        else:
                            #
                            # c) When Lb > Lr
                            if self.Lb > self.Lr:
                                _Mn_F2_2 = (self.Fcrx * self.Sx)
                                #
                                _Mn_F2_2 = min(_Mn_F2_2, _Mpx)
                                #
                                _Mn_F2_2_flag = '(F2.2b)'
                                #
                            #
                            # b) When Lp < Lb < = Lr
                            else:
                                _Mn_F2_2 = (self.Cb * (_Mpx - (_Mpx - 0.7 * self.Fy * self.Sx) *
                                                       ((self.Lb - self.Lp) / (self.Lr - self.Lp))))
                                #
                                _Mn_F2_2 = min(_Mn_F2_2, _Mpx)
                                #
                                _Mn_F2_2_flag = '(F2.2b)'
                                #                
                        #-------
                        #
                        #
                        # F2 Doubly symmetric compact I-shaped members bent 
                        #    about their major axis                             
                        if  self.ClassFlangeF == 'COMPACT':
                            # This section applies to doubly symmetric I-shaped
                            # and channel bent aout their major axis, having
                            # compact flanges as defined in Sec B4.1 for flexure
                            #        
                            # The nominal  flexural strength, Mn shall be the
                            # lower value obtained according to the limit states
                            # of yielding (platic moment) and lateral torsional
                            # buckling
                            #
                            self.Mnx = min(_Mn_F2_1, _Mn_F2_2)
                            # flag selection                
                            if self.Mnx == _Mn_F2_1: 
                                self.Mnx_flag = _Mn_F2_1_flag
                            #
                            else: 
                                self.Mnx_flag = _Mn_F2_2_flag
                            #                
                        #
                        # F3 Doubly Symmetric I_Shaped Members with Compact Webs
                        #    and NonCompact or Slenders Flanges Bent about their 
                        #    Major Axis
                        else:
                            # This section applies to doubly symmetric I-shaped
                            # members bent about their major axis having compact
                            # webs and noncompact or slender flanges as defined
                            # in Sec B4.1 for flexure
                            #
                            # -------------------------------
                            # 1.- Lateral-Torsional Buckling
                            # F3.1 
                            _Mn_F3_1 = min(_Mn_F2_1, _Mn_F2_2)
                            _Mn_F3_1_flag = '(F3.1)'
                            # print 'Mnx Section F.2: ',Mn_F2/1000000
                            #
                            # 2.- Compression Flange Local Buckling
                            # F3.2 
                            # Compression Flange
                            _lambda_F3_2 = self.bfc / (2 * self.tfc)
                            #
                            # Flange Limiting Slenderness
                            _lambda_pf = self.lambda_p_fF
                            _lambda_rf = self.lambda_r_fF
                            #
                            # a) For Sections With NonCompact Flanges
                            #
                            if self.ClassFlangeF == 'NONCOMPACT':
                                #                        
                                _Mn_F3_2 = (_Mpx - (_Mpx - 0.7 * self.Fy * self.Sx) *
                                            ((_lambda_F3_2 - _lambda_pf) /
                                             (_lambda_rf - _lambda_pf)))
                                #                        
                                _Mn_F3_2_flag = '(F3.2a)'
                                #
                            #
                            # b) For Sections With Slender Flanges
                            #
                            else:
                                #                        
                                _Mn_F3_2 = (0.9 * self.E * self.Kc * self.Sx / _lambda_F3_2**2)
                                #
                                _Mn_F3_2_flag = '(F3.2b)'
                                #
                            #
                            # The nominal flexural strength, Mn shall be the
                            # lower value obtained accordingly to the limit
                            # states of lateral-torsional buckling and 
                            # compression flange local buckling
                            #
                            self.Mnx = min(_Mn_F3_1, _Mn_F3_2)
                            # Select flag
                            if self.Mnx == _Mn_F3_1: 
                                self.Mnx_flag = _Mn_F3_1_flag
                            #
                            else: 
                                self.Mnx_flag = _Mn_F3_2_flag
                            #
                    #
                    # F4 & F5 Doubly and singly I-shaped members with
                    # compact, noncompact and slender webs bent
                    # about their major axis.
                    else :
                        #
                        # F5 Dobly and singly symmetric I-Shaped Members 
                        # With slender Webs Bent About Their Major Axis
                        #
                        # hc --> twice the distance from the centroid to the following:
                        #         --The inside face of the compression flange less the 
                        #           fil1 let or corner radius, 
                        #         --For rolled shapes; the nearest line of fasteners at 
                        #           the compression flange or the inside faces of the 
                        #           compression flange when welds are used, for 
                        #           built-up sections, in. (mm)
                        #  
                        _hc = 2*(self.Yp - self.tfc)
                        #
                        # aw --> Radio of two times the web area in compression due 
                        #        to application of major axis bending moment alone 
                        #        to the area of the compression flange components
                        # (4-11)
                        _aw = ((_hc*self.tw)/(self.bfc*self.tfc))
                        # print ('aw :',aw)
                        #
                        #
                        # (F4-10)
                        # rt --> The effective radious of gyration for lateral-torsional 
                        #        buckling
                        #
                        # i) For I-shapes with a rectangular compression flange
                        _rt_1 = (self.bfc/math.sqrt(12*((self.ho / self.d) +
                                                        ((_aw/6.0)*
                                                         (self.hw**2/(self.ho*self.d))))))
                        #
                        # ii) For I-shapes with a channel cap or a cover plate attached
                        #     to the compression flange
                        _rt_2 = (self.bfc / (math.sqrt(12*(1 + (_aw / 6.0)))))
                        #
                        _rt = min(_rt_1, _rt_2)
                        #
                        # flag
                        if _rt == _rt_1:
                            _rt_flag='(F4-10i)'
                        #
                        else:
                            _rt_flag='(F4-10ii)'
                        #             
                        #print ('rt :', _rt, _rt_flag)
                        #
                        #                
                        # (F4-7)
                        # Lp --> The limiting laterally unbraced length for the 
                        # limit stated of yielding, Lp, is determined as:
                        self.Lp = 1.1 * _rt * math.sqrt(self.E/self.Fy)
                        self.Lp_flag ='(F4-7)'
                        # print 'Lp :',Lp
                        #
                        #
                        # Elastic Section Modulus
                        _Sxc = self.Ix /  self.Yp
                        _Sxt = self.Ix / (self.d - self.Yp)
                        #print ('Sx needs to be checked ',_Sxc, _Sxt)
                        #
                        #
                        #
                        if self.ClassWebF == 'SLENDER' :
                        #if self.ClassWebF == 'NONCOMPACT' :
                            #
                            # This section applies to doubly and single symmetric 
                            # I-shaped members with slender webs attached to the
                            # mid-width of the flanges and bent about their major
                            # axis as defined in Sec B4.1 for flexure
                            #
                            # aw is defined by Ecu F4-11 but shall not exceed 10
                            #
                            # hc --> Elastic Centroid to the Compression Flange
                            # hc =2*(self.Yp-self.tfc)        
                            #
                            # aw --> Radio of two times the web area in compression 
                            #        due to application of major axis bending moment alone 
                            #        to the area of the compression flange components
                            _aw = min(10.0, _aw)
                            #print ( 'aw ---->:', _aw)
                            #               
                            # rt is the effective radious of gyration for lateral
                            # buckling as defined in section F4
                            #
                             #
                            # Limiting Slenderness of Flange
                            #
                            _lambda_F5 = (self.bfc/(2*self.tfc))
                            _lambda_pf = self.lambda_p_fF
                            _lambda_rf = self.lambda_r_fF
                            #   
                            #
                            # (F5-6) Rpg is the bending strength reduction factor
                            #
                            _Rpg1 = (1 - (_aw/(1200.0+300*_aw))*
                                     ((_hc / self.tw) - 5.70*math.sqrt(self.E / self.Fy)))
                            #
                            _Rpg = min(1.0, _Rpg1)
                            #
                            #
                            # -------------------------------------
                            # 1. Compression Flange Yielding
                            # F5-1 
                            #
                            _Mn_F5_1 = _Rpg * self.Fy * _Sxc
                            _Mn_F5_1_flag = '(F5-1)'
                            #
                            # (F4-10) The effective radious of gyration,rt for lateral
                            #         torsional buckling
                            #
                            #        
                            # Lp is defined by Equ F4-7  
                            #
                            # (F5-5)
                            self.Lr = math.pi * _rt * math.sqrt(self.E / (0.70*self.Fy))
                            self.Lr_flag = '(F5-5)'
                            #
                            #
                            # ------------------------------------
                            # 2. Lateral-Torsional Buckling
                            # F5-2
                            # a) The limit state of lateral-torsional buckling
                            #    does not apply
                            if self.Lb <= self.Lp :
                                _Fcrx_5_2 = slef.Fy 
                                _Fcrx_flag_5_2 = '(F5-1)'
                                #
                            #
                            # (F5-3 b) & (F5-4 c) 
                            else:
                                # (F5-4 c) 
                                if self.Lb > self.Lr:
                                    #
                                    _Fcrx1 = ((self.Cb*self.E*math.pi**2)/(self.Lb / _rt)**2)
                                    #
                                    _Fcrx_5_2 = min(self.Fy, _Fcrx1)
                                    #
                                    _Fcrx_flag_5_2 = '(F5-4)'
                                    #
                                # (F5-3 b)
                                else:
                                    #
                                    _Fcrx1 = (self.Cb*(self.Fy - (0.30*self.Fy) * 
                                                       ((self.Lb - self.Lp) / (self.Lr - self.Lp))))
                                    #
                                    _Fcrx_5_2 = min(self.Fy, _Fcrx1)
                                    #
                                    _Fcrx_flag_5_2 = '(F5-3)'
                                    #
                            #     
                            # F5-2
                            _Mn_F5_2 = _Rpg * _Fcrx_5_2 * _Sxc
                            _Mn_F5_2_flag = '(F5-2)'
                            #
                            #
                            # -----------------------------------------------
                            # 3. Compression Flange Local Buckling
                            # F5-3 
                            #
                            # a) For sections with compact flanges, the limit
                            #    state of compresion flange local buckling
                            #    does not apply
                            if self.ClassFlangeF == 'COMPACT':
                                _Fcrx_5_3 = self.Fy
                                #
                                _Fcrx_flag_5_3 = '(5-1)'
                                #
                            #
                            # b) For section with noncompact flanges
                            elif self.ClassFlangeF == 'NONCOMPACT':
                                _Fcrx_5_3 = (self.Fy-(0.30*self.Fy)*
                                             ((_lambda_F5 - _lambda_pf) /
                                              (_lambda_rf - lambda_pf)))
                                #
                                _Fcrx_flag_5_3 = '(F5-8)'
                                #
                            #
                            # c) For section with slender flanges
                            else:
                                _Fcrx_5_3 = ((0.90 * self.E * self.Kc) / 
                                         (self.bfc / (2*self.tfc))**2)
                                #
                                _Fcrx_flag_5_3 = '(F5-9)'
                                #
                            #
                            # F5-3                 
                            _Mn_F5_3 = _Rpg * _Fcrx_5_3 * _Sxc
                            _Mn_F5_3_flag = '(F5-7)'
                            #
                            #
                            # -------------------------------
                            # 4.- Tension Flange yielding
                            # (F5-4)
                            if _Sxt >= _Sxc:
                                #
                                _Fcrx_5_4 = self.Fy
                                #
                                _Fcrx_flag_5_4 = '(F5-1)'
                                #
                            #
                            # (F5-4b)
                            else:
                                #
                                _Fcrx_5_4 = self.Fy
                                #
                                _Fcrx_flag_5_4 = '(F5-10b)'
                                #
                            # 
                            # (F5-4)
                            _Mn_F5_4 = _Fcrx_5_4 * _Sxt
                            _Mn_F5_4_flag = '(F5-10)'        
                            #
                            #
                            # The nominal flexural strength, Mn shall be the
                            # lowest value obtained according to the limit 
                            # states of compression flange yielding, lateral
                            # torsional buckling, compression flange local
                            # buckling and tension flange yielding
                            #
                            self.Mnx = min(_Mn_F5_1, _Mn_F5_2, _Mn_F5_3, _Mn_F5_4)
                            #                            
                            if self.Mnx == _Mn_F5_1: 
                                self.Mnx_flag = _Mn_F5_1_flag
                                self.Fcrx = self.Fy
                                self.Fcrx_flag = '(F5-1)'
                            #                        
                            elif self.Mnx == _Mn_F5_2: 
                                self.Mnx_flag = _Mn_F5_2_flag
                                self.Fcrx = _Fcrx_5_2
                                self.Fcrx_flag = _Fcrx_flag_5_2
                            #
                            elif self.Mnx == _Mn_F5_3: 
                                self.Mnx_flag = _Mn_F5_3_flag
                                self.Fcrx = _Fcrx_5_3
                                self.Fcrx_flag = _Fcrx_flag_5_3
                            #                        
                            else: 
                                self.Mnx_flag = _Mn_F5_4_flag
                                self.Fcrx = _Fcrx_5_4
                                self.Fcrx_flag = _Fcrx_flag_5_4
                            #
                            #
                        #----
                        #
                        # F4 Other I-Shaped members with compact or noncompact
                        #    webs bent about their major axis
                        else:
                            # This section applies to:
                            # a) Doubly symmetric I-shaped memebers bent about
                            #    their major axis with noncompact webs.
                            # b) Singly symmetric I-shaped memebrs with webs
                            #    attached to the mid-width of the flanges, bent
                            #    about their major axis, with compact or noncompact
                            #    webs, as defined in Sec B4.1 for flexure.
                            #
                            #
                            #
                            # Moment of inertia of the compression flange
                            # about main axis x
                            _Ixc = (self.bfc*self.tfc**3)/12.0
                            # about weak axis y
                            _Iyc = (self.tfc*self.bfc**3)/12.0
                            # print 'Iyc =',Iyc
                            # print 'Ixc =',Ixc
                            #
                            #
                            # Slenderness Parameter
                            _lambda_F4_9 = _hc/self.tw
                            #
                            # Limiting Slenderness of Compression Web
                            _lambda_pw = self.lambda_p_wF
                            _lambda_rw = self.lambda_r_wF
                            #
                            #
                            # FL --> Nominal Flexural Strength
                            #
                            #
                            # Myc = Yield moment in the compression flange
                            # (F4-4)
                            _Myc = self.Fy*_Sxc
                            #print ('Myc Needs to be corrected:',_Myc)
                            #
                            # Plastic Moment
                            _Mpx = min(self.Fy*self.Zx, 1.6*_Sxc*self.Fy)
                            #print ('Mp :',_Mpx)
                            #
                            #
                            # The stress, FL, is determined as follows:
                            #
                            # (F4-6b)(ii)
                            if (_Sxt/_Sxc) < 0.70:
                                _FL = max(self.Fy*(_Sxt/_Sxc),0.5*self.Fy)
                                _FL_flag ='(F4-6b-ii)'
                            #                    
                            # (F4-6a)(i)
                            else:
                                _FL = 0.7*self.Fy
                                _FL_flag ='(F4-6b-i)'
                            #                    
                            #print ('FL :', _FL, _FL_flag)
                            #
                            #                         
                            # The web plastification factor Rpc, shall be
                            # determined as follows:
                            # J shall be taken as zero if:
                            if (_Iyc/self.Iy) <= 0.23:
                                _J = 0
                                # Rpc --> The web plastification factor
                                _Rpc = 1.0
                                _Rpc_flag = '(Iyc/Iy <= 0.23)'
                            #
                            else:
                                #
                                _J = self.J
                                #
                                # Rpc --> The web plastification factor
                                #         (F4-9b)(ii)
                                if (_hc/self.tw) > _lambda_pw:
                                    #
                                    _Rpc = ((_Mpx/_Myc)-
                                           ((_Mpx/_Myc)-1)*
                                           ((_lambda_F4_9 - _lambda_pw )/
                                            (_lambda_rw - _lambda_pw )))
                                    #
                                    _Rpc = min(_Rpc, (_Mpx/_Myc))
                                    _Rpc_flag = '(F4-9b-ii)'
                                #                    
                                # (F4-9a)(i)
                                else:
                                    #
                                    _Rpc = (_Mpx/_Myc)
                                    _Rpc_flag = '(F4-9b-i)'
                                #
                            #
                            #print ('Rpc =', _Rpc, _Rpc_flag)
                            #
                            #
                            # (F4-8)
                            # rts -> The limiting unbraced lenght for the limit
                            #         state of inelastic lateral-torsional buckling
                            self.Lr = (1.95*_rt*(self.E / _FL) *
                                       math.sqrt((_J / (_Sxc * self.ho)) +
                                                 math.sqrt((self.J / (_Sxc * self.ho))**2 +
                                                           (6.76*(_FL / self.E)**2))))
                            #
                            self.Lr_flag ='(F4-8)'
                            #
                            print ('Lr = ', self.Lr, self.Lr_flag)
                            #
                            # Lc -> Maximum Unbraced Length of the compression flange
                            #       at which the allowable bending stress is 0.66Fy
                            Lc = min(self.Lr, self.Lp)
                            #
                            #
                            # Checking Length of the Beam
                            #
                            #if self.Lx == 0: self.Lx = self.Lr
                            #                            
                            #if self.Ly == 0: self.Ly = self.Lr
                            #                            
                            #if self.Lzt == 0: self.Lzt = self.Lr
                            #                            
                            #if self.Lb == 0: self.Lb = self.Lr 
                            #
                            #
                            # (F4-5) Critical Stress Fcr
                            self.Fcrx = (((self.Cb*self.E*math.pi**2)/(self.Lb/_rt)**2)*
                                         math.sqrt(1 + 0.078*(_J/(_Sxc*self.ho))*
                                                   (self.Lb/_rt)**2))
                            #                
                            self.Fcrx_flag = '(F4-5)'
                            #print ('Fcr =',Fcrx)
                            # 
                            #
                            # 1.- Compression Flange Yielding 
                            # -------------------------------
                            # (F4-1)
                            _Mn_F4_1 = _Rpc * _Myc
                            _Mn_F4_1_flag = '(F4-1)'
                            #print ('Mn_F4_1',_Mn_F4_1)
                            #
                            #
                            # 2.- Lateral-Torsional Buckling
                            # ------------------------------
                            #
                            # (F4-2a)
                            if self.Lb <= self.Lp:
                                _Mn_F4_2 = _Mn_F4_1
                                _Mn_F4_2_flag = '(F4-2a)'
                            #
                            # (F4-2c & 2b)
                            else:
                                #
                                # (F4-2c)
                                if self.Lb > self.Lr:
                                    #
                                    _Mn_F4_2 = min((self.Fcrx * _Sxc),(_Rpc * _Myc))
                                    _Mn_F4_2_flag = '(F4-3)'
                                #
                                # (F4-2b)
                                else:
                                    #
                                    _Mn = self.Cb*(_Rpc * _Myc -
                                                  (_Rpc * _Myc - _FL * _Sxc) *
                                                  ((self.Lb - self.Lp)/(self.Lr - self.Lp)))
                                    #
                                    _Mn_F4_2 = min(_Mn,(_Rpc * _Myc))
                                    _Mn_F4_2_flag = '(F4-2)'
                                    #
                            #
                            #
                            # 3.- Compression Flange Local Buckling
                            # -------------------------------------
                            #
                            _lambda_F4_12 = self.bfc/(2 * self.tfc)
                            #
                            if self.ClassFlangeF == 'NONCOMPACT':
                                #
                                _lambda_pf = self.lambda_p_fF
                                _lambda_rf = self.lambda_r_fF
                                #
                                _Mn_F4_12 = (_Rpc * _Myc - 
                                            ((_Rpc * _Myc - _FL * _Sxc) * 
                                             ((_lambda_F4_12 - _lambda_pf)/(_lambda_rf - _lambda_pf))))
                                #
                                _Mn_F4_12_flag = '(F4-12)'
                                #
                            #
                            #
                            elif self.ClassFlangeF == 'SLENDER':
                                #
                                _Kc = 4/math.sqrt(self.d / self.tw)
                                #
                                _Mn_F4_12 = (0.90 * self.E * _Kc * _Sxc)/ _lambda_F4_12**2
                                _Mn_F4_12_flag = '(F4-13)'
                                #
                            #
                            #
                            else:
                                _Mn_F4_12 = _Mn_F4_1
                                _Mn_F4_12_flag = _Mn_F4_1_flag
                            #
                            #
                            # 4.- Tension Flange Yielding
                            #
                            # b) When
                            if _Sxt < _Sxc:
                                #
                                _Myt = self.Fy * _Sxt
                                _lambda_F4_15 = _hc / self.tw
                                #
                                # (F4-15b)
                                if (_hc/self.tw) > _lambda_pw :
                                    #
                                    _Rpt1 = ((_Mpx/_Myt) - 
                                             ((_Mpx/_Myt)-1) * 
                                             ((_lambda_F4_15 - _lambda_pw)/(_lambda_rw - _lambda_pw)))
                                    #
                                    _Rpt = min(_Rpt1, (_Mpx/_Myt))
                                    #
                                    _Rpt_flag = '(F4-15b)'
                                    #
                                #
                                # (F4-15a)
                                else:
                                    #
                                    _Rpt = _Mpx / _Myt
                                    #
                                    _Rpt_flag = '(F4-15a)'
                                    #
                                #
                                _Mn_F4_15 = _Rpt * _Myt
                                _Mn_F4_15_flag = '(F4-14)'
                            #
                            else:
                                _Mn_F4_15 = _Mn_F4_1
                                _Mn_F4_15_flag = _Mn_F4_1_flag
                            #
                            #
                            # -------------------------------------
                            #              SUMMARY
                            # -------------------------------------
                            #
                            self.Mnx = min(_Mn_F4_1,_Mn_F4_2, _Mn_F4_12, _Mn_F4_15)
                            #
                            #print ('min(_Mn_F4_1,_Mn_F4_2)', _Mn_F4_1, _Mn_F4_2)
                            #               flag   
                            if self.Mnx == _Mn_F4_1:
                                self.Mnx_flag = _Mn_F4_1_flag
                            #
                            elif self.Mnx == _Mn_F4_2:
                                self.Mnx_flag = _Mn_F4_2_flag
                            #
                            elif self.Mnx == _Mn_F4_12:
                                self.Mnx_flag = _Mn_F4_12_flag
                            #
                            else:
                                self.Mnx_flag = _Mn_F4_15_flag
                            #
                            #
                            #print ('self.Mnx' , self.Mnx, self.Mnx_flag)
                            #
                #-----------
                #                              
                #
                #
                # F6 I-Shaped Members and Channels Bent About their
                #    Minor Axis
                #            
                #    This section applies to I-Shaped members and
                #    channel bent about their minor axis
                #
                #    The nominal flexural strength, Mn shall be the
                #    lower value obtained according to the limit
                #    states of yielding (plastic moment) and flange
                #    local buckling
                #
                # -------------
                # 1.- Yielding
                # F6.1 
                _Mpy = min( self.Fy * self.Zy, 1.6 * self.Fy * self.Sy)
                _Mn_F6_1 = _Mpy
                _Mn_F6_1_flag = '(F6-1)'
                #
                # --------------------------
                # 2.- Flange Local Buckling
                # F6.2 
                #
                # Limiting Slenderness of Flange
                #
                _lambda_F6 = (self.bfc / (2.0*self.tfc))
                _lambda_pf = self.lambda_p_fF
                _lambda_rf = self.lambda_r_fF
                #
                # 
                # (F6-4)
                _tf = min(self.tfc, self.tft)
                #
                self.Fcry = (0.69 * self.E) / (self.bfc / _tf)**2
                #
                self.Fcry_flag = '(F6-4)'
                #
                #
                # (F6.2a) For Sections with Compact Flanges
                if self.ClassFlangeF == 'COMPACT':
                    #
                    _Mn_F6_2 = _Mn_F6_1
                    _Mn_F6_2_flag = '(F6.1)'
                    #
                #
                # F6.2b For Sections with Non Compact Flanges 
                elif self.ClassFlangeF == 'NONCOMPACT':
                    #            
                    _Mn_F6_2 = (_Mpy - (_Mpy - 0.70 * self.Fy * self.Sy) *
                                ((_lambda_F6 - _lambda_pf) /
                                 (_lambda_rf - _lambda_pf)))
                    #
                    _Mn_F6_2_flag = '(F6.2)'            
                    #
                #
                # F6.2c For Sections with Slender Flanges
                else:
                    #            
                    _Mn_F6_2 = self.Fcry * self.Sy
                    _Mn_F6_2_flag = '(F6.3)'
                    #
                #           
                # Summary Weak Axis
                self.Mny = min(_Mn_F6_1, _Mn_F6_2)
                # Flag            
                if self.Mny == _Mn_F6_1:
                    self.Mny_flag = _Mn_F6_1_flag
                #
                else:
                    self.Mny_flag = _Mn_F6_2_flag
            #               
            #
            # F7 Square and rectangular HSS and box-shaped members       
            if self.SectionType =='BOX':
                #
                # This section applies to square and rectangular HSS, and doubly
                # symmetric box-shaped members bent about either axis, having
                # compact or noncompact webs and compact, noncompact or slender
                # flanges as defined in Section B4 for flexure.
                #
                #
                # The nominal flexural strength, Mn, shall be the lowest value obtained
                # according to the limit states of yielding (plastic moment), flange local
                # buckling and web local buckling under pure flexure.
                #
                # F7.1. Yielding
                #
                _Mpy = self.Fy*self.Zx
                _Mn_F7_1 = Mpy
                _Mn_F7_1_flag = '(F7-1)'
                #
                # (F7.2a) For Sections with Compact Flanges
                if self.ClassFlangeF == 'COMPACT': 
                    #
                    _Mn_F7_2 = _Mn_F7_1
                    _Mn_F7_2_flag = '(F7.2a)'
                    # 
                # F7.2b For Sections with Non Compact Flanges 
                elif self.ClassFlangeF == 'NONCOMPACT':
                    #
                    _Mn_F7_2 = _Mn_F7_1
                #
            #
            # F8 Round HSS       
            elif self.SectionType =='HSS':
                print ('HSS (F8) No defined yet')
            #
            # F9 Tees and double angles loaded in the plane of     
            elif self.SectionType =='Te':
                print ('TEE (F9) No defined yet')
            #
            # F10 Single angles    
            elif self.SectionType =='ANGLE':
                print ('Angle (F10) No defined yet')
            #
            # F11 Rectangular bars and rounds   
            elif self.SectionType =='BAR' or self.SectionType =='Round':
                print ('Bar & Round (F11) No defined yet')
            #
            # F13 Proportions of beam and girders  
            elif self.SectionType =='HOLE':
                print ('F13 No defined yet')
        #
        # F12 Unsymmetrical shapes
        else:
            print ('F12 Unsymmetrical No defined yet')
        #
        #
        #               Print Major Axis Summary
        #
        print (("Lx = "+"%-4.3f"+" m")%(self.Lx/1000.0))
        print (("Ly = "+"%-4.3f"+" m")%(self.Ly/1000.0))
        print (("Lz = "+"%-4.3f"+" m")%(self.Lzt/1000.0))
        print (("Lp = "+"%-4.3f"+" m "+"%-8s")%(self.Lp/1000.0, self.Lp_flag ))
        print (("Lr = "+"%-4.3f"+" m "+"%-8s")%(self.Lr/1000.0, self.Lr_flag ))
        print (("Lb = "+"%-4.3f"+" m")%(self.Lb/1000.0))
        #
        print (" ")
        print ("      Major Axis ")
        try:
            print (("rts = "+"%-4.3f"+" "+"%-8s")%(_rts,rts_flag))
            print (("c   = "+"%-4.4f"+" "+"%-8s")%(c,c_flag))
        #
        except:
            pass
        #
        print (("Fcr = "+"% 6.0f"+" N/mm2 "+"%-8s")%(self.Fcrx, self.Fcrx_flag))
        #self.MnxMax = self.Mnx
        # print ('Mnx Max =', (self.MnxMax/1000000.0))
        #self.Mcx = self.Mnx*(self.PhiB/self.OmegaB)
        #print ('self.Mnx*(LRFD/ASD)',self.Mnx, self.PhiB, self.OmegaB, (self.PhiB/self.OmegaB))
        print (("Mn  = "+"% 6.0f"+" N/mm2 "+"%-8s")%((self.Mnx/1000000.0),self.Mnx_flag))    
        #
        #
        # Print Minor Axis Summary
        #
        print (" ")
        print ("      Minor Axis ")
        print (("Fcr  = "+"% 6.0f"+" N/mm2 "+"%-8s")%(self.Fcry, self.Fcry_flag))
        #self.MnyMax = self.Mny
        # print ('Mny Max =', (self.MnyMax/1000000))
        #self.Mcy = self.Mny*(self.PhiB/self.OmegaB)
        # following line changed so that it prints Mny and Mny_flag, previously it reprinted Mnx and Mnx_flag
        print (("Mn   = "+"% 6.0f"+" N/mm2 "+"%-8s")%((self.Mny/1000000.0),self.Mny_flag))  
    #
    #
    #
    #-------------------------------------------------
    #
    #            ++++++ Chapter G  ++++++ 
    #
    def ChapterG(self):
        #
        print (" ")        
        print ("-----------------------------")
        print ("     Chapter G - Shear ")
        print (" ")
        print ("Shear Stress Calc :", self.ShearStress)
        print (" ")
        print ("        Main Axis ")        
        #
        # (1) For all provisions in this chapter
        if self.DesignMethod == 'ASD' :
            # if self.ShearStress == 'MAXIMUM' or self.ShearStress == 'MAX':
            #  ASD=1.50
            #  else: ASD=1.67
            #
            self.OmegaV = 1.67
            self.PhiV = 1.0
        # 
        elif self.DesignMethod == 'USER_DEFINED' :
            self.OmegaV = self.UserFactorV
            self.PhiV = 1.0 
        #            
        else:
            self.OmegaV = 1.0
            self.PhiV = 0.9
        #            
        # ShearFv = 1000.0
        #  print 'ShearFv =',self.Vy 
        #
        #
        # This is temporary will need to be fixed   
        #if self.ShearStress == 'MAXIMUM':
        self.VryMax = (self.tau_y*self.Aw)
        self.VrxMax = (self.tau_x*self.Af)
        #        self.Aw=self.d*self.tw
        #        
        #       Rolled sections
        if self.Build == 'ROLLED':
            _h = self.hw
        #       Tee Sections
        elif self.Build == 'T':
            print ('TEE No yet defined')
        #       Welded + rest
        else :
            _h = self.hw
        #
        #        print 'h :',_h
        #
        self.htw_Gx = _h/self.tw
        print (("h/tw = "+"%-3.4f")%(self.htw_Gx))
        #
        #       Clear distance between transverse stiffeners
        self.aMaxLength = round(_h*min( 3.0,(260/(_h/self.tw))**2 ))
        #
        #
        #
        #       G4 Single Angles
        #        
        if self.SectionType =='ANGLE':
            print ('Angle Section No implemented yet')
        #
        #
        #       G5 Rectangular HSS and Box Members
        #
        elif self.SectionType =='BOX':
            print ('Rectangular HSS / BOX Section No implemented yet')
        #
        #
        #       G6 Round HSS
        #
        elif self.SectionType =='HSS' or self.SectionType =='BOX':
            print ('Circular HSS Section No implemented yet')
        #
        #
        # G2 Members with Unstiffened or Stiffened Webs
        #
        else:
            #
            # G2.2 Transverse Stiffeners
            #      Transverse stiffeners are not required where
            #      h/tw <= 2.46(E/Fy)^0.5 , or where the required
            #      shear strength is less than or equal to the
            #      available shear stregth in accordance with 
            #      Section G2.1 for Kv = 5.0
            #
            self.Kv = 5.0
            #            
            # a) For webs of rolled I-Shaped members with:
            #    h/tw <= 2.24(E/Fy)^0.5            
            if (self.SectionType =='I' and self.SecSym == 'DOUBLY' and
                self.Build == 'ROLLED' and self.htw_Gx <= (2.24*(self.E/self.Fy)**0.5)):          
            #
                #
                if self.DesignMethod == 'ASD':
                    self.OmegaV=1.50
                    self.PhiV = 1.0
                #
                elif self.DesignMethod == 'USER_DEFINED':
                    self.OmegaV = self.UserFactorV
                    self.PhiV = 1.0
                #
                else:
                    self.OmegaV=1.0
                    self.PhiV = 1.0   
                #
                self.Cv = 1.0
            #
            #
            #  b) For webs of all other shapes, except round HSS.
            else:
                #          
                # i)
                if self.htw_Gx <= (1.10*(self.Kv*self.E/self.Fy)**0.5):
                    #
                    self.Cv = 1.0
                #
                elif (self.htw_Gx > (1.10*(self.Kv*self.E/self.Fy)**0.5) and
                      self.htw_Gx <= (1.37*(self.Kv*self.E/self.Fy)**0.5)):
                    #
                    self.Cv = ((1.10*(self.Kv*self.E/self.Fy)**0.5)/(self.htw_Gx))
                #
                else:
                    #
                    self.Cv = ((1.51*self.E*self.Kv)/(self.Fy*self.htw_Gx**2))
            #
            # Shear Strength
            self.Vny = (0.6*self.Fy*self.Aw*self.Cv)
            #
            #
            # Check if transverse stifenners are required   
            # print 'h/tw (',self.htw_Gx, ') < 2.24',(2.24*(self.E/self.Fy)**0.5)
            # print 'h/tw (',self.htw_Gx, ') < 2.46',(2.46*(self.E/self.Fy)**0.5)
            # 
            if self.htw_Gx > (2.46*(self.E/self.Fy)**0.5) or self.Vy > self.Vny:
                self.TransvStiffeners= 'Required'               
                print ("Transverse Stiffeners : Requiered")
            #            
            else:
                print ("Transverse Stiffeners : No Requiered")
                self.TransvStiffeners= 'NoRequired'
            #
            print ("Tension Field Action : ",self.TensionFieldAction )
            #
            #
            # G2.1. Shear strength
            #       This section applies to webs of singly or doubly
            #       symmertric members and channels subject to shear
            #       in the plane of the web
            #
            #       The web plate shear buckling coefficient, Kv, is
            #       determined as follows:
            #
            #  i) For unstiffened webs:
            if self.a == 0.0:
                #                
                # with h/tw < 260
                if self.htw_Gx < 260.0:
                    self.Kv = 5.0
                    self.Kv_flag = 'h/tw < 260'
                #                    
                # rise an error
                else:
                    print ('***error h/tw > 260')
            #
            # ii) For stiffened webs
            else:
                #                
                #
                # G2 Members with Unstiffened Webs
                #    The moment of inertia Ist, of a transverse
                #    stiffeners used to develop the available web shear
                #    strength, as provided in Sec G2.1, about an axis
                #    in the web center for stiffener pairs or about the
                #    face in contact with the web plate for single 
                #    stiffeners, shall meet the following requirement:
                #                  
                #    Clear distance between transverse stiffeners
                #    print 'a =',self.a
                #    self.aMaxLength = round(h*min( 3.0,(260/(h/self.tw))**2 ))
                print (("a = "+"%-4.3f"+" m")%(self.a/1000.0))
                print (("a Max Length = "+"%-4.3f"+" m")%(self.aMaxLength/1000.0))
                #      
                # G2-8
                _j_G2 = max((2.5/(self.a/_h)**2)-2,0.5)
                #                
                # b is the smaller of the dimensions a and h
                _b_G2 = min(self.a, _h)
                #
                # G2-7
                self.Ist1 = _b_G2*_j_G2*self.tw**3
                #                
                # print (("j = "+"%-2.3f"+" (G2-8)")%(_j_G2))
                # print (("b = "+"%-4.3f"+" mm")%(_b_G2))
                # print (("Ist "+"%-1.4E"+" mm^4 (G2-7)")%(self.Ist1))
                #
                #
                # with a/h > 3.0
                if self.htw_Gx > 3.0 :
                    self.Kv = 5.0
                    self.Kv_flag = 'h/tw > 3.0'
                #
                #               or a/h > (260/(h/tw))^2 
                elif (a/h) > (260.0/self.htw_Gx)**2:
                    self.Kv = 5.0
                    self.Kv_flag = 'a/h > (260/(h/tw))^2'
                #
                # (G2-6)
                else:
                    self.Kv = 5.0 + (5.0/(a/h)**2)
                    self.Kv_flag = '(G2-6)'
                    #                
            #
            # print Kv
            print (("Kv = "+"%-2.3f"+ " (" +"%-8s"+")")%(self.Kv, self.Kv_flag))
            #
            #        
            # a) For webs of rolled I-Shaped members with:
            # h/tw <= 2.24(E/Fy)^0.5            
            if (self.SectionType =='I' and self.SecSym == 'DOUBLY' and
                self.Build == 'ROLLED' and self.htw_Gx <= (2.24*(self.E/self.Fy)**0.5)):          
                #
                self.Cv = 1.0
                self.Cv_flag = '(G2-2)'
                #                print 'a)' 
                #                print 'Cv :',self.Cv, self.Cv_flag
                #
            #
            # b) For webs of all other doubly symmetric shapes and
            #    channels, except round HSS, the web shear coefficient
            #    Cv, is determined as follows:
            else:
                # print 'b)'
                #                
                # (i) When h/tw <= 1.10*(Kv*E/Fy)^0.5
                if self.htw_Gx <= (1.10*(self.Kv*self.E/self.Fy)**0.5):
                    #                    
                    self.Cv = 1.0
                    self.Cv_flag = '(G2-3)'
                #
                # (ii) When 
                elif ((1.10*(self.Kv*self.E/self.Fy)**0.5) < self.htw_Gx <=
                      (1.37*(self.Kv*self.E/self.Fy)**0.5)):
                    #
                    self.Cv = ((1.10*(self.Kv*self.E/self.Fy)**0.5)/(self.htw_Gx))
                    self.Cv_flag = '(G2-4)'
                #
                # (iii) When h/tw > 1.37*(Kv*E/Fy)^0.5
                else:
                    #
                    self.Cv = ((1.51*self.E*self.Kv)/(self.Fy*self.htw_Gx**2))
                    self.Cv_flag = '(G2-5)'
                    #
            #-------
            # G2.1 Shear Strength
            # The nominal shear strength,Vn, of unstifened or stiffened
            # webs according to the limit states of shear yielding and
            # shear buckling:
            #                    
            self.Vny = (0.6*self.Fy*self.Aw*self.Cv)
            self.Vny_Flag = '(G2-1)'
            # print 'Vnx1 :',self.Vny , self.Vny_Flag
            self.Vc1 = self.Vny
            # print 'TensionFieldAction :', self.TensionFieldAction
            print (("Cv = "+"%-2.3f"+" "+"%-8s")%(self.Cv, self.Cv_flag))
            print (("Vn = "+"%-6.0f"+ " kN " +"%-8s")%(self.Vny/1000.0 , self.Vny_Flag ))
            #
            #
            #
            # G3 Tension Field Actions
            #
            # print 'xxxxx', self.Kv,self.Cv
            if self.TensionFieldAction != 'NO' and self.a != 0.0:
                #            
                # G3.1 Limits on the Use of Tension Field Actions
                # Consideration of tension field action is permited
                # for flanged mebers when the web plate is supported
                # on all four sides by flanges or stiffeners. 
                # Consideration of thension field actions is not permited:
                # print (" ")
                print (" -- Tension Field Action On --")
                #
                # Fyst - Specified minimum yield stress of the
                #        stiffener material        
                if self.Fyst==0: self.Fyst = self.Fy
                # print 'Fy stiffener :',self.Fyst
                #        
                # tst - Specified Thickness of the stiffener
                if self.tst == 0.0: self.tst = self.tw
                # print 'tst stiffener :',self.tst
                #            
                #            
                # Aw = (self.hw*self.tw)
                _Afc = (self.bfc*self.tfc)
                _Aft = (self.bft*self.tft)
                #
                #            
                # (a) for end panels in all members with transverse
                #     stiffeners;
                #     ??????
                #
                # (b) when a/h exceeds 3.0 or [260/(h/tw)]^2
                if self.a/_h > min( 3.0,(260/(_h/self.tw))**2 ):
                    # print (self.a/h) ,'>', min( 3.0,(260/(h/self.tw))**2 )
                    # print 'a = ' ,self.a
                    print (("a ("+"%-2.3f"+" m) should be < " +"%-2.3f" +" m")%
                           (self.a/1000.0,round(_h*min( 3.0,(260/(_h/self.tw))**2 ))/1000.0))
                    _G3_1b_flag = '(b) --> Reduce a - clear distance between transverse stiffnefer'
                    _Limit_G3_1 = 'FAIL'
                #
                # (c) when 2Aw/(Afc + Aft) > 2.5       
                elif (2*self.Aw/(_Afc + _Aft)) > 2.5 :
                    _G3_1b_flag = '(c) --> 2Aw/(Afc + Aft) > 2.5 '
                    _Limit_G3_1 = 'FAIL'
                #
                # (d) When h/bfc or h/bft > 2.5
                elif (h/self.bfc) > 6.0 or (h/self.bft) > 6.0:
                    _G3_1b_flag = '(d) --> h/bfc or h/bft > 2.5'
                    _Limit_G3_1 = 'FAIL'
                #
                else:
                    _G3_1b_flag = ' '
                    _Limit_G3_1 = 'PASS'
                    #
                #
                # G3.2 Shear Strength with Tension field Action
                #
                if _Limit_G3_1 == 'PASS' :
                    # When tension field action is permitted accordingly to
                    # Sec G3.1, the nominal shear strength, Vn, with tension
                    # field action, accordingly to the limit state of tension
                    # field yielding, shall be:
                    #                
                    # print '(G3_1)',_Limit_G3_1
                    # print '(G3_3)',Limit_G3_3                
                    #                
                    # (b) When h/tw > 1.10*(Kv*E/Fy)^0.5
                    if (_h/self.tw) > 1.10*(self.Kv*self.E/self.Fy)**0.5:
                        #                    
                        self.VnG3 = ((0.6*self.Fy*self.Aw*
                                    (self.Cv+((1-self.Cv)/
                                              (1.15*(1+(self.a/_h)**2)**0.5))))*
                                     (self.PhiV/self.OmegaV))
                        self.VnG3_Flag = '(G3-2)'
                        #
                    # (a) When h/tw <= 1.10*(Kv*E/Fy)^0.5
                    else:
                        #                    
                        self.VnG3 = 0.6*self.Fy*self.Aw*(self.PhiV/self.OmegaV)
                        self.VnG3_Flag = '(G3-1)'
                        #
                    # Vn2
                    self.Vc2 = self.VnG3
                    # print 'Vnx2 G3:',self.VnG3 , self.VnG3_Flag
                    # print (("Cv  = "+"%-2.3f"+"%-8s")%(self.Cv, self.Cv_flag))
                    print (("Vn  = "+"%-6.0f"+ " kN " +"%-8s")%(self.VnG3/1000.0 , self.VnG3_Flag ))    
                    #
                    # 
                    # G3.3. Transversal Stiffeners
                    # Transverse stiffeners subject to tension field
                    # action shall meet the requirements of Sec G2.2
                    # and the following limitations:   
                    #
                    # print (' ')
                    # print ('Stiffener Section')
                    # Stiffener Section
                    _h_st = self.hw
                    # print ('h stiffener :',_h_st)
                    #            
                    self.b_st = (0.5*self.bfc) - (0.5*self.tw)
                    # print ('b stiffener :',self.b_st)
                    #            
                    # t_st = self.tw
                    # print ('t stiffener :',self.tst)
                    #            
                    #            
                    # Stiffener Properties
                    # Fyw = self.Fy
                    # print ('Fyw =',self.Fyst)
                    self.Ist = (self.b_st*_h_st**3)/12.0
                    # print ('Ist =', self.Ist)
                    # print ('Ist1 =',self.Ist1)
                    _pst = max((self.Fy/self.Fyst),1.0)
                    self.Ist2 = (((_h**4*float(_pst)**1.3)/40.0)*(self.Fyst/self.E)**1.50)
                    # print ('Ist2 =',self.Ist2)
                    # print ('Vr =', self.Vy)
                    # print ('Vc1 =', self.Vc1)
                    # print ('Vc2 =', self.Vc2)
                    #   
                    _Limit_G3_31 = 'PASS'
                    _Limit_G3_32 = 'PASS'
                    #                
                    # (1) (b/t)st <= 0.56*(E/Fyst)
                    if (self.b_st/self.tst) > 0.56*(self.E/self.Fyst):
                        _G3_3_flag = '(G3-3) --> (b/t)st > 0.56*(E/Fyst)'
                        _Limit_G3_31 = 'FAIL'
                    #
                    # (2) Ist >= Ist1 + (Ist2-Ist1)[(Vr-Vc1)/(Vc2-Vc1)]
                    try:
                        if self.Ist < (self.Ist1 + (self.Ist2-self.Ist1)[(abs(self.Vy)-self.Vc1)/
                                                      (self.Vc2-self.Vc1)]):
                            _G3_3_flag = '(G3-4) --> Ist < Ist1 + (Ist2-Ist1)[(Vr-Vc1)/(Vc2-Vc1)]'
                            _Limit_G3_32 = 'FAIL'                
                    #
                    # except ZeroDivisionError:
                    except :                        
                        # print 'zero division'                
                        if self.Ist < self.Ist1 :
                            _G3_3_flag = "(G3-4) --> Ist < Ist1 + (Ist2-Ist1)[(Vr-Vc1)/(Vc2-Vc1)]"
                            _Limit_G3_32 = 'FAIL'                 
                    #
                    if _Limit_G3_31 == 'PASS' and _Limit_G3_32 == 'PASS':
                        self.Limit_G3 = 'PASS'
                        self.TensionFieldActionsflag='Permitted'
                    #                        
                    else : 
                        # Limit_G3_3 = 'FAIL'
                        self.TensionFieldActionsflag='Not Permitted'
                        self.Limit_G3 = str(_G3_3_flag)
                        #                
                        # print self.Limit_G3,self.TensionFieldActionsflag
                    #
                    #
                    #
                    if self.TensionFieldActionsflag =='Permitted':
                        #                        
                        # self.TensionFieldActionsflag='Permitted'
                        print ("Tension Field Actions : Permitted")
                        # print 'Vn G3 = ',self.VnG3, self.VnG3_Flag
                        #
                        if self.ShearStress == 'MAXIMUM': self._Vr_TA = abs(self.VryMax)
                        #                
                        else: self._Vr_TA = abs(self.Vy)
                
                        self.URvG3 = abs(self._Vr_TA/self.VnG3)
                        #
                        if self.URvG3 > 1.0:
                            self.URvG3Status = 'FAIL'
                        #            
                        else:
                            self.URvG3Status = 'PASS'
                            #
                    #                    
                    else:
                        # self.TensionFieldActionsflag='Not Permitted'
                        print ("Tension Field Actions : Not Permitted")              
                        #  print '(G3_3)',Limit_G3_3    
                #-------           
                # 
                else:
                    self.TensionFieldActionsflag ='Not Permitted'
                    #  print (self.TensionFieldActionsflag)
                    print ("Tension Field Actions : Not Permitted") 
                    self.Limit_G3 = 'G3.1.'+str(_G3_1b_flag)
                    print (self.Limit_G3,_Limit_G3_1)
                    #
        #-----------
        #
        #
        #       G7 Weak Axis Shear in Singly and Doubly Symmetric Shapes
        #
        if self.SecSym != 'UNSYMMETRIC':
            #            
            # For singly and doubly symmetric shapes loaded in the
            # weak axis without torsion, the nominal shear strength  
            # Vn, for shear resisting element shall be determined
            # using Equ G2-1 and Sec G2.1(b) with Aw = bf*tf,
            # h/tw = bf/tf and Kv = 1.2
            #
            print (" ")
            print ("        Weak Axis ")
            #            print ('Aw  =',self.Af)
            self.htw_Gy = max(self.bfc/self.tfc,self.bft/self.tft)
            # print ('h/tw Weak Axis=',self.htw_Gy)
            self.Kvx = 1.20
            # print ('Kv Weak Axis =',self.Kvx)
            #            
            # G2.1(b)
            #                
            # (i) When h/tw <= 1.10*(Kv*E/Fy)^0.5
            if self.htw_Gy <= (1.10*(self.Kvx*self.E/self.Fy)**0.5):
                #                    
                self.Cvx = 1.0
                self.Cvx_flag = '(G2-3)'
                #
            # (ii) When 
            elif ((1.10*(self.Kvx*self.E/self.Fy)**0.5) < self.htw_Gy <=
                  (1.37*(self.Kvx*self.E/self.Fy)**0.5)):
                #
                self.Cvx = ((1.10*(self.Kvx*self.E/self.Fy)**0.5)/(self.htw_Gy))
                self.Cvx_flag = '(G2-4)'
                #
            # (iii) When h/tw > 1.37*(Kv*E/Fy)^0.5
            else:
                #
                self.Cvx = ((1.51*self.E*self.Kvx)/(self.Fy*self.htw_Gy**2))
                self.Cvx_flag = '(G2-5)'
                #
            #
            # G2.1 Shear Strength
            #                    
            self.Vnx = (0.6*self.Fy*self.Af*self.Cvx)
            self.Vnx_Flag  = '(G2-7)'
            # print ('Factor ASD-LRFD', self.OmegaV, self.PhiV,(0.6*self.Fy*self.Af*self.Cvx))
            print (("Cv = "+"%-2.3f"+" "+"%-8s" )%(self.Cvx, self.Cvx_flag))
            print (("Vn = "+"%-6.0f"+ " kN " +"%-8s")%(self.Vnx/1000.0 , self.Vnx_Flag ))
            print (" ")
            #
        #---
        #
        # Unsymmetric  Sections
        else:
            print ('UNSYMMETRIC Sections no implemented yet')
            #
        #
        #
        # Determine Max Shear UR
        #
        if self.ShearStress == 'MAXIMUM':
        #            
            self.URvy = abs(self.VryMax/(self.Vny*(self.PhiV/self.OmegaV)))
            self.URvx = abs(self.VrxMax/(self.Vnx*(self.PhiV/self.OmegaV)))
            #            
            #if  self.URvy > self.URvx:
            #    self.URv = self.URvy
            #    self.URv_flag = self.Vny_Flag
            #            
            #else:
            #    self.URv = self.URvx
            #    self.URv_flag = self.Vnx_Flag   
            #                
        # 
        else:
            #   
            self.URvy = abs(self.Vy/(self.Vny*(self.PhiV/self.OmegaV)))
            self.URvx = abs(self.Vx/(self.Vnx*(self.PhiV/self.OmegaV)))
            # 
        #
        if  self.URvy > self.URvx :
            self.URv = self.URvy
            self.URv_flag = self.Vny_Flag
        #            
        else:
            self.URv = self.URvx 
            self.URv_flag = self.Vnx_Flag             
        #
        #
        if self.URv > 1.0:
            self.URvStatus = 'FAIL'
        #            
        else:
            self.URvStatus = 'PASS'
        #
        #        print ("ASD =",self.OmegaV," LRFD =",self.PhiV)
        try:
            print (( "UR Main Axis (TFA) = "+"%-2.3f")%(self.URvG3))
        #
        except:
            print (( "UR Main Axis = "+"%-2.3f")%(self.URvy))
        #            
        print (( "UR Weak Axis = "+"%-2.3f")%(self.URvx))            
    #
    #
    #
    #                
    #-------------------------------------------------
    #
    #             ++++++ Torsion  ++++++             
    #
    def Torsion(self):
        # (a)
        _Fna = self.Fy
        #
        #(b)
        _Fnb = 0.6*self.Fy
        #
        # (c)
        _Fnc = 1
        #
    #
    #
    #
    #-------------------------------------------------
    #
    #            ++++++ Chapter H  ++++++ 
    #
    def ChapterH(self):
        #        
        print (" ")        
        print ("-----------------------------")
        print (" Chapter H - Combined Forces")
        print (" ")
        #
        # self.P = Pr
        # Pr = Pr
        # self.Mx =abs(Mrx)
        # self.My =abs(Mry)
        #      
        # Flange Moment of Inertia            
        _Iyc = (self.tfc * self.bfc**3) / 12.0
        # Ixc = (self.tfc*self.bfc**3)/12.0
        # print 'Iyc =',Iyc
        # print 'Ixc =',Ixc  
        _Icy_Iy = _Iyc / self.Iy
        # print 'Icy/Iy',Icy_Iy
        #
        # H1 Doubly and singly symmetric members subject
        #    to flexure and axial force
        if self.SecSym != 'UNSYMMETRIC' and _Icy_Iy >= 0.1 and _Icy_Iy <= 0.9:
            #      
            self.Mcx = self.Mnx * (self.PhiB/self.OmegaB)
            self.Mcy = self.Mny * (self.PhiB/self.OmegaB)
            #
            # H1-1 Doubly and Syngly Symmetric Members
            #      in Flexure and Compression
            if self.FAxial == 'COMPRESSION':
                self.Pc = self.Pn_E * (self.PhiC/self.OmegaC)
            #
            # if (Mrx/Mcx) < 0.05 or (Mcy/Mcy) < 0.05:
            # print '?'
            #                                   
            #
            # H1-2 Doubly and Syngly Symmetric Members
            #      n Flexure and Tension           
            else:
                self.Pc = self.Pn_D * (self.PhiT/self.OmegaT)
            #
            # (H1-1b)
            if (abs(self.P) / self.Pc) < 0.2:
                #
                _UR = ((abs(self.P) / (2 * self.Pc)) +
                       (abs(self.Mx) / self.Mcx) + 
                       (abs(self.My)/self.Mcy))
                #
                _UR_flag = '(H1-1b)'
            #
            # (H1-1a)
            else:
                #
                _UR = ((abs(self.P) / self.Pc) +
                       (8.0 / 9.0) * ((abs(self.Mx) / self.Mcx) + 
                                      (abs(self.My) / self.Mcy)))
                #
                _UR_flag = '(H1-1a)'   
                #
        #
        # self.Pc = _Pc
        self.UR = _UR
        self.UR_flag = _UR_flag
        #        
        if self.UR > 1.0:
            self.URStatus = 'FAIL'
        #
        else:
            self.URStatus = 'PASS'
        #            
        # print (' ')
        print (("UR = "+"%-2.3f"+" "+"%-8s")%(self.UR, self.UR_flag))
    #        print self.FAxial
    #
    #     
    #
    #
    def PrintSummary(self):
        #          
        OutputFile = open(self.FileOut,'a+')
        #
        if self.SectionType != 'I':
            OutputFile.write(" "+"\n")
            OutputFile.write("_______________________________________________________________________________________"+"\n")
            OutputFile.write(" "+"\n")
            OutputFile.write("                                YIELD Check Results"+"\n")
            OutputFile.write(" "+"\n")
            OutputFile.write("Name          URComb   UR Ax   Pr      Mrx       Mry   UR Vx    Vrx      Vry    Tr"+"\n")
            OutputFile.write("              Equ      UR Mx   Pc      Mcx       Mcy   UR Vy    Vcx      Vcy    Tc"+"\n")
            OutputFile.write("              Result   UR My                           UR T                       "+"\n")
            OutputFile.write(" "+"\n")
            OutputFile.write("_______________________________________________________________________________________"+"\n")
            OutputFile.write(" "+"\n")
            #
            # OutputFile.write(" "+"\n")
            # OutputFile.write("_______________________________________________________________________________________"+"\n")
            OutputFile.write(" "+"\n")
            OutputFile.write("                              STABILITY Check Results"+"\n")
            OutputFile.write(" "+"\n")
            OutputFile.write("Name          URComb   UR Ax     Pr        Mrx       Mry       Fex       Kx        Lx"+"\n")
            OutputFile.write("              Equ      UR Mx     Pc        Mcx       Mcy       Fey       Ky        Ly"+"\n")
            OutputFile.write("              Result   UR My               Cmx       Cmx       Cb        Kz        Lb"+"\n")
            OutputFile.write("_______________________________________________________________________________________"+"\n")
            OutputFile.write(" "+"\n")
            #
        #
        else:
            #
            # Memeber Compacness Section
            #
            OutputFile.write(" "+"\n")
            OutputFile.write("_______________________________________________________________________________________"+"\n")
            OutputFile.write(" "+"\n")
            OutputFile.write("                                  SECTION COMPACTNESS"+"\n")
            OutputFile.write(" "+"\n")
            OutputFile.write("Member ID     Type     T B4.1a    T B4.1a    T B4.1b    T B4.1b    b    [mm]  Kc "+"\n")
            OutputFile.write("              Build    Flg b/t    Web h/tw   Flg b/t    Web h/tw   t    [mm]  FL[N/mm2]"+"\n")
            OutputFile.write("              Symm     Alpha r    Alpha r    Alpha r    Alpha r    tw   [mm]  Ag  [mm2]"+"\n")
            OutputFile.write("                                             Alpha p    Alpha p    hc   [mm]  An  [mm2]"+"\n")            
            OutputFile.write("                       Class      Class      Class      Class      hp   [mm]  My [N.mm]"+"\n")
            OutputFile.write("......................................................................................."+"\n")
            OutputFile.write(" "+"\n") 
            #            
            OutputFile.write(("%-12s" +'  '+"%-6s" +"   FLANGE     WEB        FLANGE     WEB"+ 8*" "+ "%-1.3E" +
                              2*" "+ "%-1.3E" +"\n")% (self.BeamID, self.SectionType, max(0.5*self.bft,0.5*self.bfc),self.Kc))
            #            
            OutputFile.write((14*" "+ "%-6s" +3*" "+ "%-1.3E" +2*" "+ "%-1.3E" +2*" "+
                              "%-1.3E" +2*" "+ "%-1.3E" +2*" "+ "%-1.3E" +2*" "+ "%-1.3E" +"\n")%
                             (self.Build, self.bt, self.htw, self.bt_F, self.htw, self.t,
                              self.FL))
            #
            OutputFile.write((14*" "+ "%-6s" +3*" "+ "%-1.3E" +2*" "+ "%-1.3E" + 2*" "+
                              "%-1.3E" +2*" "+ "%-1.3E" +2*" "+ "%-1.3E" +2*" "+ "%-1.3E" +"\n")%
                             (self.SecSym, self.lambda_r_fE, self.lambda_r_wE, self.lambda_r_fF, 
                              self.lambda_r_wF, self.tw, self.Ag))
            #
            OutputFile.write((45*" "+ "%-1.3E" +2*" "+ "%-1.3E" +2*" "+ "%-1.3E" +2*" "+ "%-1.3E" +"\n")%
                             ( self.lambda_p_fF, self.lambda_p_wF,
                               self.hc, self.An))
            #
            OutputFile.write((23*" "+ "%-10s" +" "+ "%-10s"  +" "+ "%-10s" + " "+
                              "%-10s"+" "+ "%-1.3E" +2*" "+ "%-1.3E" +"\n")%
                             ( self.ClassFlangeE, self.ClassWebE, self.ClassFlangeF, self.ClassWebF,
                               self.hp, self.Mym))
            #
            #
            #
            #
            # Shear and Torsion Section
            #
            #                
            OutputFile.write(" "+"\n")
            OutputFile.write("_______________________________________________________________________________________"+"\n")
            OutputFile.write(" "+"\n")
            OutputFile.write("                                  SHEAR CHECK RESULTS"+"\n")
            OutputFile.write(" "+"\n")
            OutputFile.write("Member ID     UR  Max  Vry    [N]  Vrx    [N]  h/tw_W  h/tw_F   TransvStiff  ShrStress"+"\n")
            OutputFile.write("Design to     Equ Max  Vny    [N]  Vnx    [N]  Kv_Web  Kv_Flg   a      [mm]  Maximum[N]"+"\n")
            #
            if self.DesignMethod == 'ASD' or self.DesignMethod == 'USER_DEFINED':
                _shear_factor = self.OmegaV
                #
                OutputFile.write("Result        Equ Vny  OmegaV web  OmegaV flg  Cv_Web  Cv_Flg   aMax   [mm]  Average[N]"+"\n")           
                OutputFile.write("              Equ Vnx  OmV*Vr/Vn   OmV*Vr/Vn                    Comments"+"\n")
                #
            #
            else:
                _shear_factor = self.PhiV
                #
                OutputFile.write("Result        Equ Vny  PhiV        Vn Flng[N]  Cv_Web  Cv_Flg   aMax   [mm]  Average"+"\n")           
                OutputFile.write("              Equ Vnx  Vr/Vn*PhiV  Vr/Vn*PhiV                   Comments"+"\n")
                # 
            #
            OutputFile.write("......................................................................................."+"\n")
            OutputFile.write(" "+"\n")
            #  
            if self.ShearStress == 'MAXIMUM':
                #
                _shear_stress_flag_1 = 'MAXIMUM'
            #                
            else:
                #_shear_stress_flag_2 = ' '
                _shear_stress_flag_1 = 'AVERAGE'
                #
            #
            OutputFile.write(("%-12s" +"  "+ "%3.4f" +"   "+ "%1.4E" +"  "+ "%1.4E" +"  "+
                              "%3.3f" +"  "+"%3.3f"+"   "+ "%-12s" +2*" "+ "%-9s" +"\n")%
                             (self.BeamID, abs(self.URv), abs(self.Vy), abs(self.Vx), self.htw_Gx, 
                              self.htw_Gy, self.TransvStiffeners,_shear_stress_flag_1))
            #
            # if a was selected  
            if self.a != 0.0:                
                #
                if float(self.a) > float(self.aMaxLength): _a_comm = "Fail a>aMax"
                #
                else: _a_comm = "Ok  a<aMax"               
                #
                #
                OutputFile.write(("%-12s" +"  "+ "%6s" +3*" "+ "%1.4E" +"  "+ "%1.4E" +"  "+
                                  "%1.4f" +"  "+"%1.4f"+"   "+ "%1.4E"  + 3*" "+ "%1.4E" +"\n")%
                                 (self.DesignMethod, self.URv_flag, self.Vny, self.Vnx, self.Kv, 
                                  self.Kvx, self.a, self.VryMax))
                #                    
                OutputFile.write(("%-12s" +"  "+ "%6s" +3*" "+ "%1.4E" +"  " + "%1.4E" +"  "+
                                  "%1.4f" +"  "+"%1.4f"+"   "+ "%1.4E" + 3*" "+ "%1.4E" +"\n")%
                                 (self.URvStatus, self.Vny_Flag, _shear_factor, _shear_factor, 
                                  self.Cv, self.Cvx, self.aMaxLength, abs(self.Vy)))
                #
                OutputFile.write((14*" "+ "%-6s" +"   "+ "%3.6f" +4*" "+ "%3.6f" +21*" "+ "%6s" + "\n")%
                                 (self.Vnx_Flag, self.URvy, self.URvx, _a_comm))
                #                
            #
            #
            else:
                #
                # Check if Transversal Stiffeners are Required                
                if self.TransvStiffeners == 'NoRequired' :
                    #                
                    #
                    OutputFile.write(("%-12s" +"  "+ "%6s" +3*" "+ "%1.4E" +"  "+ "%1.4E" +"  "+
                                      "%1.4f" +"  "+"%1.4f"+ 16*" "+ "%1.4E" + "\n")%
                                     (self.DesignMethod, self.URv_flag, self.Vny, self.Vnx, self.Kv, 
                                      self.Kvx, self.VryMax))
                    #                    
                    OutputFile.write(("%-12s" +"  "+ "%6s" +3*" "+ "%1.4E" +"  " + "%1.4E" +"  "+ 
                                      "%1.4f" +"  "+"%1.4f" + 16*" "+ "%1.4E" +"\n")%
                                     (self.URvStatus, self.Vny_Flag, _shear_factor, _shear_factor, 
                                      self.Cv, self.Cvx, abs(self.Vy)))
                    #
                    OutputFile.write((14*" "+ "%-6s" +"   "+ "%3.6f" +4*" "+ "%3.6f" + "\n")%
                                     (self.Vnx_Flag, self.URvx, self.URvy))               
                    # 
                #
                else :
                    #                    
                    _a_comm = 'Provide a'                
                    #
                    OutputFile.write((14*" "+ "%6s" +3*" "+ "%1.4E" +"  "+ "%1.4E" +"  "+
                                 "%1.4f" +"  "+"%1.4f"+"   "+ "%1.4E"  +4*" "+ "%1.4E" +"\n")%
                                (self.URv_flag, self.VryMax, self.VrxMax, self.Kv, self.Kvx,
                                 self.a, self.VryMax))
                    #                    
                    OutputFile.write((14*" "+ "%-6s" +"   "+ "%1.4E" +"  "+ "%1.4E" +"  "+
                                 "%1.4f" +"  "+"%1.4f"+"   "+ "%1.4E" +"\n")%
                                (self.URvStatus, _shear_factor, _shear_factor, self.Cv, self.Cvx,
                                 self.aMaxLength))
                    #
                    OutputFile.write((14*" "+ "%-6s" +"   "+ "%3.4f" +6*" "+ "%3.4f" +
                                 23*" "+ "%6s" + "\n")%
                                ( self.DesignMethod.upper(), self.URvx, self.URvy, _a_comm))      
                    #
            #
            # Check if Tension Action is selected                    
            if (self.TensionFieldAction != 'NO') :
                #  
                if self.a == 0.0: self.TensionFieldActionsflag = 'MISSING DATA'
                #                
                OutputFile.write(" "+"\n")
                OutputFile.write("_______________________________________________________________________________________"+"\n")
                # OutputFile.write("......................................................................................."+"\n")
                OutputFile.write(" "+"\n")
                OutputFile.write("                          TENSION FIELD ACTIONS : ")
                OutputFile.write(str(self.TensionFieldActionsflag)+"\n")
                #                
                if self.TensionFieldActionsflag == 'Permitted' and self.URvy > 1.0:
                    #                    
                    OutputFile.write(" "+"\n")
                    OutputFile.write("Member ID     URComb   Vr Web[N]   h_st  [mm]  Fyw[N/mm2]  Ist  [mm4]      Comments"+"\n")
                    OutputFile.write("              Equ      Vn Web[N]   b_st  [mm]  Vc1    [N]  Ist1 [mm4]  "+"\n")
                    OutputFile.write("Result                 Vr/Vn Web   t_st  [mm]  Vc2    [N]  Ist2 [mm4]  "+"\n")
                    # OutputFile.write(" "+"\n")
                    OutputFile.write("......................................................................................."+"\n")
                    OutputFile.write(" "+"\n")
                    #    
                    OutputFile.write(("%-12s" +"  "+ "%3.4f" +"   "+ "%1.4E" +"  "+ "%1.4E" +"  "+
                                 "%1.4E" +"  "+ "%1.4E")%
                                (self.BeamID, self.URvG3, self._Vr_TA, self.hw, self.Fyst, self.Ist))
                    #                    
                    if self.Ist < self.Ist1 or self.Ist < self.Ist2:
                        OutputFile.write("  **Fail"+"\n")
                    #
                    else:
                        OutputFile.write("   < Ist1 & < Ist2"+"\n")
                    #                
                    OutputFile.write(("              "+  "%6s"  +"   "+ "%1.4E" +"  "+ "%1.4E" +"  "+
                                 "%1.4E" +"  "+ "%1.4E")%
                                (self.VnG3_Flag, self.VnG3, self.b_st, self.Vc1, self.Ist1))
                    #                    
                    if self.Ist < self.Ist1 :
                        OutputFile.write("  > Ist "+"\n")
                    #
                    else:
                        OutputFile.write("         ok "+"\n")                    
                    #                
                    OutputFile.write(("%-12s" +11*" "+ "%3.4f" +"      "+ "%1.4E" +"  "+
                                 "%1.4E" +"  "+ "%1.4E")%
                                (self.URvG3Status, abs(self._Vr_TA/self.VnG3), self.tst, self.Vc2, self.Ist2))
                    if self.Ist < self.Ist2 :
                        OutputFile.write("  > Ist "+"\n")
                    #
                    else:
                        OutputFile.write("         ok "+"\n")                      
                #                    
                elif self.a == 0.0:
                    OutputFile.write(" "+"\n")                     
                    OutputFile.write("  **Fail : Provide the Clear Distance (a) Between Transverse Stiffeners"+"\n")
                    OutputFile.write("_______________________________________________________________________________________"+"\n")
                    OutputFile.write(" "+"\n")       
                #
                elif self.URvy < 1.0:
                    OutputFile.write(" "+"\n")                     
                    OutputFile.write(("   But Web Shear Utilisation Vrx/Vnx ("+ "%2.4f" +") < 1.0 Therefore not required "+" "+"\n")%(self.URvy ))
                    OutputFile.write("_______________________________________________________________________________________"+"\n")
                    OutputFile.write(" "+"\n")                         
                #                    
                else:
                    OutputFile.write(" "+"\n")                     
                    OutputFile.write("  **Fail : "+str(self.Limit_G3)+" "+"\n")
                    OutputFile.write("_______________________________________________________________________________________"+"\n")
                    OutputFile.write(" "+"\n")                    
                    #
            #
            # Axial and Bending Moment Section
            #                
            if self.FAxial == 'COMPRESSION':
                #
                OutputFile.write(" "+"\n")
                OutputFile.write("_______________________________________________________________________________________"+"\n")
                OutputFile.write(" "+"\n")
                OutputFile.write("                    FLEXURE AND AXIAL COMPRESSION FORCE CHECK RESULTS"+"\n")
                OutputFile.write(" "+"\n")
                OutputFile.write("Member ID     URComb   Pr     [N]  Mrx [N/mm]  Mry [N/mm]  Cb    Q     Lx   [mm]  Kx"+"\n")
                OutputFile.write("Design to     Equ UR   Pn     [N]  Mnx [N/mm]  Mny [N/mm]  Fex[N/mm2]  Ly   [mm]  Ky"+"\n")
                #
                if self.DesignMethod == 'ASD' or self.DesignMethod == 'USER_DEFINED':
                    _Axial_factor = self.OmegaC
                    _BM_factor = self.OmegaB
                    #
                    OutputFile.write("Result        Equ Pn   OmegaC      OmegaBx     OmegaBy     Fey[N/mm2]  Lz   [mm]  Kz"+"\n")
                    OutputFile.write("              Equ Mn   Om*Pr/Pn    Om*Mrx/Mnx  Om*Mry/Mny  Fez[N/mm2]  Lb   [mm]  KL/r"+"\n")                
                else:
                    _Axial_factor = self.PhiC
                    _BM_factor = self.PhiB
                    #
                    OutputFile.write("Result        Equ Pn   PhiC        PhiCx       PhiCy       Fey[N/mm2]  Lz   [mm]  Kz"+"\n")
                    OutputFile.write("              Equ Mn   Pr/Pn*Phi   Mrx/Mn*Phi  Mry/Mn*Phi  Fez[N/mm2]  Lb   [mm]  KL/r"+"\n")
                #
                OutputFile.write("......................................................................................."+"\n")
                OutputFile.write(" "+"\n")
                #                
                OutputFile.write(("%-12s" +"  "+ "%3.4f" +"   "+ "%1.4E" +"  "+ "%1.4E" +"  "+
                             "%1.4E" +"  "+"%1.2f" +"  "+"%1.2f"+"  "+"%1.3E"+"  "+ "%1.2f" +"\n")%
                            (self.BeamID, self.UR, abs(self.P), abs(self.Mx), abs(self.My), self.Cb,
                              self.Q, self.Lx, self.Kx))
                #                
                OutputFile.write(("%-12s" +"  "+ "%6s" +"  "+ "%1.4E" +"  "+ "%1.4E" +"  "+
                             "%1.4E" +"  "+"%1.4E" +"  "+"%1.3E" +"  "+ "%1.2f" +"\n")%
                            (self.DesignMethod, self.UR_flag, self.Pn_E, self.Mnx, self.Mny, self.Fex_E4_7, self.Ly, self.Ky))
                #                
                OutputFile.write(("%-12s" +"  "+ "%-6s" +"   "+ "%1.4E" +"  "+ "%1.4E"  + "  "+ "%1.4E"  +
                              "  "+ "%1.4E" +"  "+"%1.3E" +"  "+ "%1.2f" +"\n")%
                            (self.URStatus, self.Pn_E_flag, _Axial_factor, _BM_factor, _BM_factor, 
                             self.Fey_E4_8, self.Lzt, self.Kz))
                #                
                OutputFile.write((14*" "+ "%-6s" +"   "+ "%3.6f" + 4*" "+ "%3.6f"  + 4*" "+ "%3.6f"  +
                              4*" " + "%1.4E" +"  "+"%1.3E" +"  "+ "%3.1f" +"\n")%
                            (self.Mnx_flag, abs(self.P/self.Pc ), abs(self.Mx/self.Mcx), 
                             abs(self.My/self.Mcy), self.Fez_E4_9, self.Lb, self.KLr))
                #                
                OutputFile.write(" "+"\n")
                #OutputFile.write("_______________________________________________________________________________________"+"\n")
                OutputFile.write(" "+"\n")
                #
            #
            else:
                OutputFile.write(" "+"\n")
                OutputFile.write("_______________________________________________________________________________________"+"\n")
                OutputFile.write(" "+"\n")
                OutputFile.write("                     FLEXURE AND AXIAL TENSION FORCE CHECK RESULTS"+"\n")
                OutputFile.write(" "+"\n")
                OutputFile.write("Member ID     URComb   Pr     [N]  Mrx [N/mm]  Mry [N/mm]  Lx/rx       Lx    [mm]"+"\n")
                OutputFile.write("Design to     Equ UR   Pn     [N]  Mnx [N/mm]  Mny [N/mm]  Ly/ry       Ly    [mm]"+"\n")
                #
                if self.DesignMethod == 'ASD' or self.DesignMethod == 'USER_DEFINED':
                    _Axial_factor = self.OmegaT
                    _BM_factor = self.OmegaB
                    #
                    OutputFile.write("Result        Equ Pn   OmegaT      OmegaBx     OmegaBy                 Lz    [mm]"+"\n")
                    OutputFile.write("              Equ Mn   Om*Pr/Pn    Om*Mrx/Mnx  Om*Mry/Mny              Lb    [mm]"+"\n")
                #
                else:
                    _Axial_factor = self.PhiT
                    _BM_factor = self.PhiB
                    #
                    OutputFile.write("Result        Equ Pn   PhiT        OmegaBx     OmegaBy                 Lz    [mm]"+"\n")
                    OutputFile.write("              Equ Mn   Pr/Pn*Phi   Mrx/Mn*Phi  Mry/Mn*Phi              Lb    [mm]"+"\n")
                #
                OutputFile.write("......................................................................................."+"\n")
                OutputFile.write(" "+"\n")
                #   
                if self.L_r > 300 :
                    _Lr_flag_1 = "L/r > 300"
                    _Lr_flag_2 = "**FAIL"
                #
                else:
                    _Lr_flag_1 = "L/r < 300"
                    _Lr_flag_2 = "OK"
                #                
                OutputFile.write(("%-12s" +"  "+ "%3.4f" +"   "+ "%1.4E" +"  "+ "%1.4E" +"  "+
                             "%1.4E" +"  "+"%1.4E" +"  "+"%1.4E"+"  "+"\n")%
                            (self.BeamID, self.UR, abs(self.P), abs(self.Mx), abs(self.My), 
                             (self.Lx/self.rx), self.Lx))
                #                
                OutputFile.write(("%-12s" +"  "+ "%-6s" +"  "+ "%1.4E" +"  "+ "%1.4E" +"  "+
                             "%1.4E" +"  "+"%1.4E" +"  "+"%1.4E" +"  "+"\n")%
                            (self.DesignMethod, self.UR_flag, self.Pn_D, self.Mnx, self.Mny, (self.Ly/self.ry), self.Ly))
                #                
                OutputFile.write(("%-12s" +"  "+ "%-6s" +"   "+ "%1.4E" + "  "+ "%1.4E"  +"  "+ "%1.4E"  +
                              "  "+ "%-6s" + 3*" " +"%1.4E" +"\n")%
                            (self.URStatus, self.Pn_D_Flag, _Axial_factor, _BM_factor, _BM_factor, 
                            _Lr_flag_1, self.Lzt))   
                #                
                OutputFile.write((14*" "+ "%-6s" +"   "+ "%3.6f" + 4*" "+ "%3.6f"  + 4*" "+ "%3.6f" +
                                  4*" " +"%-6s"+ 6*" " +"%1.4E"  +"\n")%
                                 (self.Mnx_flag, abs(self.P/self.Pc ), abs(self.Mx/self.Mcx),
                                  abs(self.My/self.Mcy), _Lr_flag_2,self.Lb))
                #                
                OutputFile.write(" "+"\n")
                #OutputFile.write("_______________________________________________________________________________________"+"\n")
                OutputFile.write(" "+"\n")             
                #
        #
        #
        OutputFile.close()
        #
        print (" ")
        print ("End Writing Out File")
    #
    #    
    #
    #
#
#
#
#
class CodeCheck(Shape, AISC2010):
    #
    def __init__(self, DesignMethod = 'ASD', units = 'Nmm'):
        #
        # ----- Ini -----
        #self.Code = str(DesignCode.upper())
        self.DesignMethod = str(DesignMethod.upper())
        # Units
        self.Units = str(units)
        #
        # ----- General Data -----
        self.BeamID = 'N/A'
        # Section Type 
        self.SectionType ='I'
        # Build [WELDED / ROLLED]
        self.Build = 'WELDED'
        # Stiffned Element [YES/NO]
        self.StiffnedElement = 'NO'
        # Output file
        self.FileOut ='AISC2010_res.out'
        # Shear Stress [MAXIMUM / AVERAGE]
        self.ShearStress = 'AVERAGE'
        #
        # ----- Beam Efective Length ----- 
        #  Majoy Axis Lx
        self.Lx = 0
        # Minor Axis Ly
        self.Ly = 0
        #
        # ----- Unbraced Length -----
        # Lateral Unbraced Length
        self.Lb = 0    
        # Torsional Unbraced Length
        self.Lzt = 0
        #
        #  ----- Material -----
        if self.Units == 'Nmm':
            #       [N/mm2]
            self.Fy = 248.0
            #        [N/mm2]
            self.E = 205000.0
            #        [N/mm2]
            self.G = 77200.0
            #        [kg/m3]
            self.Rhos = 7850
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
        #
        #         (ksi)[N/mm2]
        self.Fu = self.Fy/0.75
        # poisson
        self.Poisson = 0.30
        #
        # ----- Stiffener Spacing ----- 
        self.a = 0
        self.TensionFieldAction = 'NO'
        self.tst = 0
        self.Fyst  = 0
        #
        # ----- StabilityFactors ----- 
        self.Kx = 1.0
        self.Ky = 1.0
        self.Kz = 1.0
        #
        # ----- Moment Modifiers -----
        self.Cb = 1.0
        #
        #
        #
        #  ----- Forces -----
        # Axial        
        self.P = 0.0
        # Bending        
        self.Mx = 0.0
        self.My = 0.0 
        self.Mt = 0.0
        # Shear        
        self.Vy = 0.0
        self.Vx = 0.0
        # Flag Axial Force (worst case)
        self.FAxial = 'COMPRESSION'
        #  
        # ----- Headers -----
        self.Header = 1
        #
        self.UserFactorT = 1.0
        self.UserFactorC = 1.0
        self.UserFactorB = 1.0
        self.UserFactorV = 1.0
    #
    #        
    #-------------------------------------------------
    #                   General Data
    #-------------------------------------------------
    #    
    #    
    def GeneralData(self,BeamID, Type, Build = 'WELDED', ShearStress ='AVERAGE', StiffnedElement = 'NO', NameOut ='N/A'):
        #
        self.BeamID =str(BeamID)
        self.SectionType = str(Type.upper())
        self.Build = str(Build.upper())
        self.ShearStress = str(ShearStress.upper())
        self.StiffnedElement = str(StiffnedElement.upper())
        #
        if NameOut == 'N/A':
            self.FileOut = str(BeamID)+'.out'
        #
        else:
            self.FileOut = str(NameOut)+'.out'
        #
    #
    #        
    #-------------------------------------------------
    #                  Section Data
    #-------------------------------------------------
    #  
    #        
    def SectionData(self, D, Tw, Bfc, Tfc, Bft, Tft):
        # 
        #
        self.d = float(D)
        self.tw = float(Tw)
        self.bfc = float(Bfc)
        self.tfc = float(Tfc)
        self.bft = float(Bft)
        self.tft = float(Tft)
        #
        #
    #
    #
    #-------------------------------------------------
    #              Material Properties
    #-------------------------------------------------
    #
    #   
    def Material(self, Fy , E = 0, Fu = 0, Nu = 0, G = 0, Rhos = 0):
        # 
        # Minimum Yield Stress
        self.Fy = float(Fy)
        # Youngs Modulus
        self.E = float(E)
        # Poisson
        self.Poisson = float (Nu)
        # Shear Modulus
        self.G = float(G)
        # Material Density
        self.Rhos = float(Rhos)
        # Minimum Tensile Strenth
        if Fu == 0 :
            self.Fu = Fy/0.75
        #
        else: self.Fu = float(Fu)
        #
    #
    #
    #-------------------------------------------------
    #                Length of Beam
    #-------------------------------------------------
    #
    def EffectiveLength(self, Lx, Ly = 0):
        #
        # Beam Efective Length Majoy Axis
        self.Lx = float(Lx)
        #
        # Beam Efective Length Minor Axis
        self.Ly = float(Ly)
        #         
    #
    #
    #
    def UnbracedLength(self, Lb, Lzt = 0):
        #
        # Lateral Unbraced Length
        self.Lb = float(Lb)
        #        
        # Torsional Unbraced Length
        self.Lzt = float(Lzt) 
        #
    #
    #
    def MomentModifiers(self, Cb = 1.0):
        #
        self.Cb = float (Cb)
        #
    #
    #
    def StiffenerSpacing(self, a = 0, TensionFieldAction = 'NO', tst = 0, Fyst = 0):
        self.a = a
        self.TensionFieldAction = str(TensionFieldAction.upper())
        self.tst  = tst
        self.Fyst  = Fyst 
        #
    #
    #
    def StabilityFactors(self, Kx = 1.0, Ky = 1.0, Kz = 1.0):
        # Main Axis
        self.Kx = float(Kx)
        # Weak Axis
        self.Ky = float(Ky)
        # Torsion
        self.Kz = float(Kz) 
        #
    #
    #
    #-------------------------------------------------
    #                 Acting Forces
    #-------------------------------------------------
    #
    def MemberForces(self, P = 0, Vy = 0, Vx = 0, Mt = 0, Mx = 0, My = 0 ):
        #        
        # Axial        
        self.P = float(P)
        # Shear        
        self.Vy = float(abs(Vy))
        self.Vx = float(abs(Vx))
        # Bending Moment
        self.Mt = float(Mt)
        self.Mx = float(Mx)
        self.My = float(My)   
        #
    # 
    #
    #-------------------------------------------------
    #                 User Defined
    #-------------------------------------------------
    #
    def UserDefinedFactors(self, FactorT = 1.0, FactorC = 1.0, FactorB = 1.0, FactorV = 1.0):
        #
        self.UserFactorT = abs(FactorT)
        self.UserFactorC = abs(FactorC)
        self.UserFactorB = abs(FactorB)
        self.UserFactorV = abs(FactorV)
        #
        self.DesignMethod = 'USER_DEFINED'
    #
    #-------------------------------------------------
    #                 Print Results
    #-------------------------------------------------
    #
    #
    def PrintResults(self):
        #       
        # change to upper cases all flag variable
        self.DesignMethod = str(self.DesignMethod.upper())
        self.Build = str(self.Build.upper())
        self.ShearStress = str(self.ShearStress.upper())
        self.StiffnedElement = str(self.StiffnedElement.upper())
        self.TensionFieldAction = str(self.TensionFieldAction.upper())
        #
        #
        # Flag Axial Force (COMPRESSION/TENSION)
        if self.P != 0:
            #            
            if (self.P/abs(self.P)) == -1.0:
                self.FAxial = 'COMPRESSION'
            #
            else:
                self.FAxial = 'TENSION'
            # 
        #
        else:
            self.FAxial = 'TENSION'            
            #
        #  
        #
        # Check Beam Efective Length Minor Axis
        if self.Ly == 0:
            self.Ly = self.Lx      
        #
        # Check Torsional Unbraced Length
        if self.Lzt == 0:
            self.Lzt = max(self.Lx, self.Ly)
        #
        # Check Lateral Unbraced Length
        if self.Lb == 0:
            self.Lb = max(self.Lx, self.Ly)            
        #
        #
        #
        # Check Material 
        if self.Units == 'Nmm':
            #
            #        [N/mm2]
            if self.E == 0 : self.E = 205000.0
            #        [N/mm2]
            if self.G == 0 : self.G = 77200.0
            #        [kg/m3]
            if self.Rhos == 0: self.Rhos = 7850
        #
        else:
            #
            #          [ksi]
            if self.E == 0 : self.E = 29000.0
            #        [ksi]
            if self.G == 0 : self.G = 11200.0
            #         [lb/in3]
            if self.Rhos == 0: self.Rhos = 0.2836
        #
        #  (ksi)[N/mm2]
        if self.Fu == 0: self.Fu = self.Fy/0.75
        #
        #
        self.SectionType = SecFinder(self.SectionType)
        print ('SectionFinal --->', self.SectionType)
        # Select Section
        # I Sections        
        if self.SectionType == 'I':
            #         
            Shape.I(self, self.d, self.tw, self.bfc, self.tfc, self.bft, self.tft)
            # 
        #
        elif self.SectionType == 'TUBULAR':
            Shape.Tubular(Diam ,tchk)
            #    
        #
        elif self.SectionType == 'CHANNEL':
            Shape.CH(self,self.d,self.tw,self.Bfc,self.Tfc,self.Bft,self.Tft) 
            #
        #
        #
        #
        if self.Header == 1:
            OutputFile = open(self.FileOut,'w')
            today = datetime.date.today()
            #        
            OutputFile.write(" "+"\n")
            OutputFile.write("***************************************************************************************"+"\n")
            OutputFile.write("*                                  CODE CHECK TOOL                                    *"+"\n")
            OutputFile.write("*                                     AISC 2010                                       *"+"\n")
            OutputFile.write("*                     Specification for Structural Steel Buildings                    *"+"\n")
            OutputFile.write("*                                   BETA Version                             30/07/10 *"+"\n")            
            OutputFile.write("***************************************************************************************"+"\n")
            OutputFile.write(("DATE: "+ "%-8s" + 59*" " + "UNITS ["+"%-4s"+"]" + "\n")%(today, self.Units))
            # 
            OutputFile=open(self.FileOut,'a+')
            #
        #            
        #
        print (" ")
        print (" ")
        print ("+++++++++++++++++++++++++++++")
        print ("       AISC 2010 ",self.DesignMethod)
        print ("Member : ",self.BeamID)
        print ("Results: ",self.FileOut)
        print ("+++++++++++++++++++++++++++++")
        print ("              Calculation: ",self.Header)
        #        
        #
        # in here just AISC is been used
        #
        AISC2010.ChapterB(self)
        AISC2010.ChapterF(self)        
        AISC2010.ChapterD(self)
        AISC2010.ChapterE(self)        
        AISC2010.ChapterG(self)        
        AISC2010.ChapterH(self)
        #
        # Print Results
        #AISC.PrintProperty(self)
        #Shape.PrintProperty(self)
        Shape.PrintPropertyShort(self)
        AISC2010.PrintSummary(self)
        self.Header = self.Header + 1
        #
        #
#   
#
#
#
if __name__=="__main__": 
    #
    #
    #
    D = raw_input('Overall Depth of Section:')
    # self.d =float(D)
    Tw = raw_input('Web Thickness:')
    # self.tw =float(Tw)
    #                
    Bfc = raw_input('Compression Flange Width:')
    # self.bfc =float(Bfc) 
    Tfc = raw_input('Compression Flange Thickness:')
    # self.tfc =float(Tfc)   
    #
    Bft = raw_input('Tension Flange Width:')
    # self.bft =float(Bft) 
    Tft = raw_input('Tension Flange Thickness:')
    # self.tft =float(Tft)  
    #
    SectionProperty = Section()
    SectionProperty.I(D,Tw,Bfc,Tfc,Bft,Tft)
    #
    #
    #Section.PrintPropertyShort(self)            
    #PrintSummary(self)    
    #
    #