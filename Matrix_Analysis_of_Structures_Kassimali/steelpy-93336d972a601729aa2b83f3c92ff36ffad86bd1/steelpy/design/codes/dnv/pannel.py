# Copyright (c) 2009-2023 steelpy


# Python stdlib imports
from dataclasses import dataclass
from typing import Dict, List, Tuple, Union, NamedTuple

# package imports
from steelpy.material.main import Materials
from steelpy.sections.main import Sections
from steelpy.utils.units.main import Units
from steelpy.design.codes.dnv.DVNRPC201 import DNVRPC201

#
#
#     Stiffener is at top/bottom of panel as defined by ILRFLG below
#
#              6                5                  4
#           x  |=================|=================|  x
#           :  |                 |                 |  :
#           :  |                 |                 |  :
#    Plate  :  |=================|=================|  x
#    Length :  |                 |                 |  : Stiffener Spacing
#   Vertical:  |                 |                 |  :       (Ls)
#     (LG)  :  |=================|=================|  x
#           : Y|                 |                 |  :
#           : ^|                 |                 |  :
#           x  |=================|=================|  x
#              1  > X            2                  3
#              x-----------------x-----------------x 
#              :      (L1)       :      (L2)       : Girder Spacing
#              :                 :                 :
#              x-----------------x                 :
#              :       (L)       : Stiffener Length
#              :                 :                 :
#              x-----------------------------------x Panel Length Htal
#                              (Lp)
#
#     Note:
#           b1,b2   : Sub-panel widths for panels adjoining stiffener
#           tf1, tf2: Panel thickness for panels adjoining stiffener
#           PANEL1  : Flags if panels present on each side of stiffener
#           PANEL2
#
#
class Stiffener:
    __slots__ =  ['section', 'material', 'units',
                  'L', 'Lt', 's', 's2']
    
    def __init__(self):
        """ 
        section: Sections
        material : Materials
        L: Stiffener Length
        Lt: Stiffener torsional buckling length
        s: Stiffener Spacing
        s2: width of flange
        """
        self.units = Units()

#
class Girder:
    __slots__ =  ['section', 'material', 'units',
                  'L', 'Lk', 'Lt']
    
    def __init__(self):
        """
        section: Sections
        material : Materials
        L : Girder Length
        Lk: Girder Overal Buckling Length
        Lt: Girder Torsional Buckling Length (Length between tripping brackets)
        """
        self.units = Units()
#
#
class StressDefinition(NamedTuple):
    """ """
    Sd1: float
    Sd2: float
    L: float
    L1: float
    #
    # self.sigmay_2Sd, self.SigmaySd, self.SigmayType
    #return Sigma2Sd, sigmaSd, sigmaType 
    #
    @property
    def Sd(self):
        """ """
        if self.Sd1 == self.Sd2:
            return self.Sd1
        else:
            return max((self.Sd2 + (self.Sd1 - self.Sd2) 
                        * (self.L - self.L1) / self.L ) ,
                       0.75 * self.Sd1 )
    #
    @property
    def type(self):
        """ """
        if self.Sd1 == self.Sd2:
            return "uniform"
        else:
            return "varying"
#
#
#
def LoadCase(LoadDescription):
    """ """
    _LoadDescDummy = ''
    #
    if 'LONGITUDINAL' in LoadDescription:
        _LoadDescDummy = 'LONGITUDINAL ' + str(_LoadDescDummy)
    #
    #
    if 'TRANSVERSE' in LoadDescription:
        _LoadDescDummy = 'TRANSVERSE ' + str(_LoadDescDummy)
    #
    #
    if 'SHEAR' in LoadDescription:
        _LoadDescDummy = 'SHEAR ' + str(_LoadDescDummy)
    #
    #
    if 'LATERAL-PRESSURE' in LoadDescription:
        _LoadDescDummy = 'LATERAL-PRESSURE ' + str(_LoadDescDummy)
    #
    return _LoadDescDummy
#
#
def PlateCase(PlateDescription):
    """ """
    plateDescDummy = ''
    if 'PLATE' in PlateDescription:
        if 'STIFFENER' in PlateDescription:
            if 'GIRDER' in PlateDescription:
                plateDescDummy  = 'GIRDER STIFFENED PLATE'
            else:
                plateDescDummy = 'STIFFENED PLATE'
        else:
            plateDescDummy = 'UNSTIFFENED PLATE'
    else:
        self.msg = self.msg + '\n' + 'not defined yet'
    #
    return plateDescDummy
#
#
# DNVRPC201
class CodeCheckPanel:
    """
    This programm checks stiffener buckling strength of stiffened panels 
    according to DNV recommended practice "Buckling Strength of Plated 
    Structures", DNV-RP-C201, October 2002.with amendments in October 2008
    Note: compressive stresses are positive,  
    tensile stresses are negtive.
    """

    def __init__(self):
        """ """
        self.DesignCode = 'DNV'
        #
        self._stiffener = Stiffener()
        self._girder = Girder()
        #
        self.stress = 'TENSION'
        self.plate_description = 'N/A'
        self.load_description = 'N/A'
        self.buckling_check = 'YES'
        #
        # Material Factor
        self.GammaM = 1.150
        #
        ## Longitudinal stress
        #self.sigmax_1Sd = 0
        #self.sigmax_2Sd = 0
        ##
        ## Tranverse stress 
        #self.sigmay_1Sd = 0
        #self.sigmay_2Sd = 0
        ##
        # Shear stress 
        self.tau_Sd = 0
        #
        # Lateral Pressure 
        self.PSd = 0
        #
        # lenght to reference point
        #
        self.L = 0
        self.L1 = 0
        self.S = 0
        self.S1 = 0
        self.LG = 0
        # 
        # for simplification Z = 0
        self.Z = 0
        #
        self.ur = 0.0
        self.msg = ''
    #
    #
    def general_data(self, Stiffener = 'C', Girder = 'C', EPLSy = 'N', GM = 1.15):
        """ """
        # Stiffener can be :
        # C for continuos stiffener
        # S for simply supported stiffener (with sniped ends)
        # 
        self.stiffener = Stiffener.upper()
        #
        # Girder can be :
        # C for continous girder 
        # S for simply supported girder (with sniped ends)
        self.girder = Girder.upper()
        #
        # Y if assuming that the stiffened plate is effective
        #   against transverse compression stress Sigmay
        # N if otherwise
        # (refer section 8.4.2 & 8.4.3)
        self.effectivePL_sigmay = EPLSy.upper()
        #
        # Material Factor
        self.GammaM = GM
    #
    #
    # Plate Pannel Section
    #
    def plate_geometry(self, thk:Units, LP:Units, LG:Units):
        """ """
        # Plate thickness
        self.t = thk.value
        # Plate Length horizontal
        self.Lp = LP.value
        # Plate Length Vertical
        self.LG = LG.value
        #
        self.plate_description = 'PLATE + ' + str(self.plate_description)
    #
    #
    def plate_material(self, Fyp = 265.0, E = 210000, Nu = 0.3):
        """ """
        # Young's Modulus
        self.E = E
        # Yield Strength
        self.fyp = Fyp
        # Poisson's ratio
        self.poisson = Nu
    #
    #
    # Stiffener Section
    @property
    def stiffener(self):
        """ """
        return self._stiffener
    #
    #def stiffener_cross_section(self, StifType, hw, b, tw, tf, S1, S2 = 0):
    #    """Type for stiffener can be :
    #       I for I-Section
    #       L for angle
    #       T for T-Section
    #       F for flat bar
    #       B for Box-Section 
    #       """
    #    #
    #    self.stiffener_type = StifType.upper()
    #    #
    #    # Stiffener Section Detail
    #    #
    #    # height of stiffener web
    #    self.hw = hw
    #    # web thickness
    #    self.tw = tw
    #    #
    #    # width of flange (top)
    #    self.bf = b
    #    # flange thickness
    #    self.tf = tf
    #    # self.ef = self.tw /2
    #    #
    #    # Stiffener Spacing (mm)
    #    # width of flange (bottom)
    #    self.s1 = S1
    #    self.s2 = S2
    #    #
    #    #
    #    self.plate_description = 'STIFFENER + ' + str(self.plate_description)
    #
    #
    #def stiffener_buckling_length(self, LS, LT):
    #    """ """
    #    # Stiffener Length
    #    self.L = LS
    #    # Stiffener torsional buckling length
    #    self.Lt = LT
    #
    #
    #def stiffener_material(self, FyS = 265.0, ES = 210000, Nu = 0.3):
    #    """ """
    #    # Yield Strength (N/mm2)
    #    self.fyS = FyS
    #    # Young's Modulus
    #    self.ES = ES
    #    # Poisson's ratio
    #    self.poissonS = Nu
    #
    #
    # Girder Section
    #
    @property
    def girder(self):
        """ """
        return self._girder
    #
    #def girder_cross_section(self, GType , hwG, twG, bG, tfG, L1 = 0, L2 = 0):
    #    """ """
    #    # Type for stiffener can be :
    #    # I for I-Section
    #    # L for angle
    #    # T for T-Section
    #    # F for flat bar
    #    # B for Box-Section
    #    self.girder_type = GType.upper()
    #    #
    #    # height of stiffener web
    #    self. hwG = hwG
    #    # web thickness
    #    self.twG = twG
    #    # width of flange
    #    self.bG = bG
    #    # flange thickness
    #    self.tfG = tfG
    #    #
    #    # Girder Spacing (mm)
    #    self.L1 = L1
    #    self.L2 = L2
    #    #
    #    self.plate_description = 'GIRDER + ' + str(self.plate_description)
    #
    #
    #def girder_buckling_length(self, LG, LGK, LGT):
    #    """ """
    #    # Girder Length
    #    self.LG = LG
    #    #
    #    # Girder Overal Buckling Length
    #    self.LGk = LGK
    #    # Girder Torsional Buckling Length
    #    # (Length between tripping brackets)
    #    self.LGt = LGT
    #
    #
    #def girder_material(self, FyG = 265.0, EG = 210000, Nu = 0.3):
    #    """ """
    #    # Yield Strength (N/mm2)
    #    self.fyG = FyG
    #    # Young's Modulus
    #    self.EG = EG
    #    # Poisson's ratio
    #    self.poissonG = Nu
    #
    #
    # Web Stiffener Section
    #
    def web_stiffener(self, WSType, Tws, Lws, SWs):
        """ """
        # Web Stiffener Type:
        # TRANVERSE
        # LONGITUDINAL
        #
        self.web_stiffener_type = WSType
        #
        # thickness of web stiffener
        self.tws = Tws
        #
        # Length of tranverse web stiffener
        self.Lws = Lws
        #
        # Distance between transverse web stiffener
        self.Sws = SWs
        #
        self.plate_description = 'WEB_STIFFENER + ' + str(self.plate_description)
    #
    #
    def web_stiffener_material(self, FyWS = 265.0, EWS = 210000, Nu = 0.3):
        """ """
        # Yield Strength (N/mm2)
        self.fyWs = FyWS
        # Young's Modulus
        self.EWs = EWS
        # Poisson's ratio
        self.poissonWs = Nu
    #
    #
    # Bracket Section
    #
    def bracket_geometry(self, BType, Tb, D1, D2):
        """ """
        # Bracket type:
        # FREE-EDGE
        # SINGLE
        # DOUBLE
        #
        self.bracket_type = BType
        #
        # plate thickness of the bracket
        #
        self.tb = Tb
        #
        # see Figure 9-3
        # distance d0 or d1
        self.d1 = D1
        # distance d2 for double brackets
        self.d2 =D2
        #
        self.plate_description = 'BRACKET + ' + str(self.plate_description)
    #
    #
    def bracket_material(self, FyB = 265.0, EB = 210000, Nu = 0.3):
        """ """
        # Yield Strength (N/mm2)
        self.fyB = FyB
        # Young's Modulus
        self.EB = EB
        # Poisson's ratio
        self.poissonB = Nu
    #
    #
    # Stress / Pressure Section
    #
    def Sigmax_Sd(self, Sigmax1_Sd:Units, 
                  Sigmax2_Sd:Union[Units,None] = None, 
                  S1:Union[Units,None] = None):
        """ 
        Sigmax1_Sd : Longitudinal stress larger (compression positive)
        Sigmax2_Sd : Longitudinal stress smaller (compression positive)
        S1 : lenght to reference point
        """
        #self.sigmax_1Sd = Sigmax1_Sd.convert('megapascal').value
        #
        if not Sigmax2_Sd:
            Sigmax2_Sd = Sigmax1_Sd
        #else:
        #    self.sigmax_2Sd = Sigmax1_Sd.convert('megapascal').value
        #
        # lenght to reference point
        if S1:
            self.S1 = S1.value
        else:
            self.S1 = 0
        #
        self._sigmax_sd = StressDefinition(Sigmax1_Sd.convert('megapascal').value,
                                           Sigmax2_Sd.convert('megapascal').value,
                                           self.Lp, self.S1)
        self.load_description = 'LONGITUDINAL + ' + str(self.load_description)
    #
    @property
    def Sigmax(self):
        """ """
        return self._sigmax_sd
    #
    def Sigmay_Sd(self, Sigmay1_Sd, 
                  Sigmay2_Sd:Union[Units,None] = None, 
                  L1:Union[Units,None] = None):
        """ 
        Sigmay1_Sd : Tranverse stress larger (compression positive)
        Sigmay2_Sd : Tranverse stress smaller (compression positive)
        L1 : lenght to reference point
        """
        #self.sigmay_1Sd = Sigmay1_Sd.convert('megapascal').value
        #
        #if Sigmay2_Sd:
        #    self.sigmay_2Sd = Sigmay2_Sd.convert('megapascal').value
        if not Sigmay2_Sd:
            Sigmay2_Sd = Sigmay1_Sd
        #
        if L1:
            self.L1 = L1.value
        else:
            self.L1 = 0
        #
        self._sigmay_sd = StressDefinition(Sigmay1_Sd.convert('megapascal').value,
                                           Sigmay2_Sd.convert('megapascal').value,
                                           self.LG, self.L1)        
        self.load_description = 'TRANSVERSE + ' + str(self.load_description)
    #
    @property
    def Sigmay(self):
        """ """
        return self._sigmay_sd
    #
    def Tau_Sd(self, TauSd:Units):
        """ Shear stress """
        #  
        self.tau_Sd = TauSd.convert('megapascal').value
        #
        self.load_description = 'SHEAR + ' + str(self.load_description)
    #
    def P_Sd(self, PSd:Units):
        """ Lateral Pressure 
            *Note - Lateral pressure should be
            Input as:
            Positive for pressure on Plate Side
            Negative for pressure on stiffener side
            """
        self.PSd = PSd.convert('megapascal').value
        #
        self.load_description = 'LATERAL-PRESSURE + ' + str(self.load_description)
    #
    # Results
    #
    def print_results (self):
        """ """
        # Plate Description Selection
        #
        self.msg = self.msg + '\n' + 'Decription: ' + str(self.plate_description)
        #
        self.plate_description = PlateCase(self.plate_description)
        #
        self.msg = self.msg + '\n' + 'Plate Description: ' + str( self.plate_description)
        #
        #
        # Load Description Selection
        #
        self.msg = self.msg + '\n' + 'Load Description =' + str( self.load_description)
        #
        self.load_description = LoadCase(self.load_description)
        #
        self.msg = self.msg + '\n' + 'Load Description =' + str( self.load_description) + str(len(self.load_description.split()))
        #
        #
        #
        #
        if 'UNSTIFFENED' in self.plate_description:
            # print('Unstiffened plate')
            self.S = self.LG
            self.L = self.Lp
            self.fy = self.fyp
        else:
            #  stiffener
            self.msg = self.msg + '\n' + ''
            self.msg = self.msg + '\n' + 'Stiffened plate'
            #
            # flange excentricity
            self.ef = self.tw / 2.0
            #
            # plate width, stiffener spacing
            self.S = (self.s1 + self.s2) / 2.0
            #
            # define mc factor
            #
            if self.stiffener.upper() == 'S':
                self.mc = 8.90

            elif self.stiffener.upper() == 'C':
                self.mc = 13.30

            else:
                'Stiffener type not recognized'
                exit
            #
            # Girder
            if 'GIRDER' in self.plate_description:
                # Girder excentricity
                self.efG = self.twG / 2.0
            #
            _b = max(self.s1 , self.s2)
            #
            self.Asf, self.As,self.Zpf, self.Is, _Iz, _Wes, _Wep, self.fy = PlateStiffener(_b, self.t,
                                                                                           self.hw, self.tw, 
                                                                                           self.bf, self.tf, 
                                                                                           self.L,
                                                                                           self.fyp, self.fyS, self.fyS,
                                                                                           self.ES)
            #
            #
            self.msg = self.msg + '\n' + 'fy ='+str(self.fy)
            self.msg = self.msg + '\n' + 'Total Cross Sectional Area =' + str( self.Asf)
            self.msg = self.msg + '\n' + 'Cross Sectional Area of stiffener =' + str( self.As)
            self.msg = self.msg + '\n' + 'Neautral Axis Loacation =' + str( self.Zpf)
            self.msg = self.msg + '\n' + 'Moment of Inertia I =' + str( self.Is)
            self.msg = self.msg + '\n' + ''
        #
        # Calculate equivalent design transverse stress
        #
        # Longitudinal stress
        #self.sigmax_2Sd, self.SigmaxSd, self.SigmaxType = StressDefinition(self.sigmax_1Sd, 
        #                                                                  self.sigmax_2Sd, 
        #                                                                  self.S, self.S1)
        ##
        self.msg = self.msg + '\n' + ''
        self.msg = self.msg + '\n' + 'SigmaxSd =' + str( self.SigmaxSd) + ' Load Type: '  + str(self.SigmaxType)
        self.msg = self.msg + '\n' + ' '
        #
        #
        # this need to pass to 6.8
        # Transverse stress
        #
        _L1 = min(0.25*self.L, 0.50*self.S)
        self.msg = self.msg + '\n' + 'Equiv L1 =' + str( _L1)
        #
        self.sigmay_2Sd, self.SigmaySd, self.SigmayType = StressDefinition(self.sigmay_1Sd, 
                                                                          self.sigmay_2Sd, 
                                                                          self.L, _L1)
        #
        self.msg = self.msg + '\n' + 'SigmaySd =' + str( self.SigmaySd) + ' Load Type: ' + str(self.SigmayType)
        self.msg = self.msg + '\n' + ' '
        #
        #
        self.stress = StressSign(self.SigmaxSd, self.SigmaySd)
        #
        self.msg = self.msg + '\n' + 'Stress Sign: ' + str( self.stress)
        self.msg = self.msg + '\n' + ''
        #
        self.epsilon = math.sqrt(235.0 / self.fy)
        #
        self.msg = self.msg + '\n' + 'Epsilon =' + str( self.epsilon)
        self.msg = self.msg + '\n' + ''
        #
        # Shear Modulus
        self.G = self.E / (2.0 * (1 + self.poisson))
        self.msg = self.msg + '\n' + 'Shear Modulus, G =' + str(self.G)
        #
        #
        if self.DesignCode == 'DNV':
            #DNVRPC201.PlateBucklingCheck(self)
            if 'UNSTIFFENED' in self.plate_description:
                if self.S > self.L:
                    self.msg = self.msg + '\n' + 'Fail ==> s > l'
                    exit()
                else:
                    if len(self.load_description.split()) == 1:
                        self.msg = self.msg + '\n' + 'Single'

                        if self.SigmaxSd != 0:
                            self.msg = self.msg + '\n' + 'SigmaxSd'

                            if self.SigmaxType == 'UNIFORM':
                                self.msg = self.msg + '\n' + 'UNIFORM'
                                DNVRPC201.BucklingOfUnstiffenedPlatesLongitudinalUniformCompression(self)
                            else:
                                self.msg = self.msg + '\n' + 'Varying'
                                DNVRPC201.BucklingOfUnstiffenedPlatesVaryingLongStress(self)
                        elif self.SigmaySd != 0:
                            self.msg = self.msg + '\n' + 'SigmaySd '

                            if self.SigmayType == 'UNIFORM':
                                self.msg = self.msg + '\n' + 'UNIFORM'
                                DNVRPC201.BucklingOfUnstiffenedPlatesTransverseCompression(self)
                            else:
                                self.msg = self.msg + '\n' + 'Varying'
                        else:
                            self.msg = self.msg + '\n' + 'Shear'
                            DNVRPC201.BucklingOfUnstiffenedPlatesShear(self)
                    else:
                        self.msg = self.msg + '\n' + 'Multiple'
                        DNVRPC201.BucklingOfUnstiffenedPlatesLongitudinalUniformCompression(self)
                        DNVRPC201.BucklingOfUnstiffenedPlatesTransverseCompression(self)
                        DNVRPC201.BucklingOfUnstiffenedPlatesBiaxiallyLoadedShear(self)
            else:
                self.msg = self.msg + '\n' + 'Buckling check not necessary'
                self.buckling_check = 'NO'
                #
                self.msg = self.msg + '\n' + 'stiffened'
                #
                #self.LG = self.S
                # Chp 5
                DNVRPC201.LateralLoadedPlates(self)
                # Chp 6.3
                DNVRPC201.BucklingOfUnstiffenedPlatesTransverseCompression(self)
                # Chp 7
                DNVRPC201.BucklingOfStiffenedPlates(self)
                #
                if 'GIRDER' in self.plate_description:
                    #
                    self.msg = self.msg + '\n' + 'stiffened + girder'
                    #
                    # need to fix this
                    self.girderSupport = 'C'
                    self.msg = self.msg + '\n' + 'Girder Support Type =' + str(self.girderSupport)
                    #
                    #
                    DNVRPC201.BucklingOfGirders(self)
                    #
                    #
                    if 'WEB_STIFFENER' in self.plate_description:
                        self.msg = self.msg + '\n' + 'WEB_STIFFENER'
                    #
                    if 'BRACKET' in self.plate_description:
                        self.msg = self.msg + '\n' + 'BRACKET'
        #self.ur = DNVRPC201.ur
        #print('DNV ' + str(DNVRPC201.ur))
        #print('self ' + str(self.ur))
