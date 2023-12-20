# Copyright (c) 2015-2023 steelpy

# Python stdlib imports

# package imports

#
#-------------------------------------------------
#                   DNV Section
#-------------------------------------------------
#
class DNV:
    #
    def __init__(self):
        pass
    #
    def temperature(self, Tmax):
        """
        """
        self.Tmax = Tmax
    #
    def material(self, SMYS, SMTS):
        """
        """
        self.SMYS = SMYS
        self.SMTS = SMTS
    #
    # C300 Characteristic material properties
    #
    def characteristic_material_properties(self, design_condition, material_type):
        """
        """
        #
        #if self.MaterialDerate == "DNVF101":
        #print ('ok')
        #
        # 301 Characteristic material properties shall be 
        # used in the resistance calculations. The yield 
        # stress and tensile strength in the limit state 
        # formulations shall be based on the engineering 
        # stress-strain curve.
        #
        # 306 The material factor, AlphaU, depend on
        # Supplementary requirement U as shown in 
        # Table 5-6.
        #
        # Table 5-6 Material Strength factor,
        #
        # Supplementary requirement U
        if 'hydro' in design_condition:
            alpha_U = 1.0
        #
        # Normally
        else:
            alpha_U = 0.96
        #
        # Note: For system pressure test, AlphaU shall
        # be equal to 1.00, which gives an allowable 
        # hoop stress of 96% of SMYS both for materials
        # fulfilling supplementary requirement U and 
        # those not. This is equivalent to the mill 
        # test utilisation.
        #
        # Guidance note:
        # The application of Supplementary requirement
        # U requires documentation after the 
        # manufacture and shall be used with care.
        # Based on production data, it may be used for
        # future upgrade of the pipeline
        # 
        #
        # 303 The different mechanical properties refer
        # to room temperature unless otherwise stated.
        # 
        # 304 The material properties shall be selected
        # with due regard to material type and potential
        # temperature and/or ageing effects and shall 
        # include:
        #         - yield stress
        #         - tensile strength
        #         - Young's modulus
        #         - temperature expansion coefficient
        #
        # For C-Mn steel this shall be considered for 
        # temperatures above 50C, and for 22Cr and 
        # 25Cr for temperatures above 20C
        #
        if material_type == "DSS":
            #
            if self.Tmax < 20:
                _fytemp = 0.0
            #
            elif self.Tmax < 50:
                _grad = ((40.0 - 0.0)/(50. - 20.))
                _fytemp = ((self.Tmax - 20)*_grad + 0.0)
            #
            elif self.Tmax < 100:
                _grad = ((90.0 - 40.0)/(100. - 50.))
                _fytemp = ((self.Tmax - 50)*_grad + 40.0)
            #
            elif self.Tmax < 150:
                _grad = ((120.0 - 90.0)/(150. - 100.))
                _fytemp = ((self.Tmax - 100)*_grad + 90.0)
            #
            elif self.Tmax < 200:
                _grad = ((140.0 - 120.0)/(200. - 150.))
                _fytemp = ((self.Tmax - 150)*_grad + 120.0)
            #
            else:
                print ("Temperature no valid")
                _fytemp = 0.0
        #
        #
        elif material_type == "CMN":
            #
            if self.Tmax < 50:
                _fytemp = 0.0
            #
            elif self.Tmax < 100:
                _grad = ((30.0 - 0.0)/(100. - 50.))
                _fytemp = ((self.Tmax - 50)*_grad + 0.0)
            #
            elif self.Tmax < 150:
                _grad = ((50.0 - 30.0)/(150. - 100.))
                _fytemp = ((self.Tmax - 100)*_grad + 30.0)
            #
            elif self.Tmax < 200:
                _grad = ((70.0 - 50.0)/(200. - 150.))
                _fytemp = ((self.Tmax - 150)*_grad + 50.0)
            #
            else:
                print ("Temperature no valid")
                _fytemp = 0.0
        #
        #
        elif material_type == "25CR":
            #
            if self.Tmax < 20:
                _fytemp = 0.0
            #
            elif self.Tmax < 100:
                _grad = ((80.0 - 0.0)/(100. - 20.))
                _fytemp = ((self.Tmax - 20)*_grad + 0.0)
            #
            elif self.Tmax < 200:
                _grad = ((130.0 - 80.0)/(200. - 100.))
                _fytemp = ((self.Tmax - 100)*_grad + 80.0)
            #
            else:
                print ("Temperature no valid")
                _fytemp = 0.0
        #
        #
        elif material_type == "22CR":
            #
            if self.Tmax < 20:
                _fytemp = 0.0
            #
            elif self.Tmax < 50:
                _grad = ((65.0 - 0.0)/(50. - 20.))
                _fytemp = ((self.Tmax - 20)*_grad + 0.0)
            #
            elif self.Tmax < 100:
                _grad = ((120.0 - 65.0)/(100. - 50.))
                _fytemp = ((self.Tmax - 50)*_grad + 65.0)
            #
            elif self.Tmax < 200:
                _grad = ((170.0 - 120.0)/(200. - 100.))
                _fytemp = ((self.Tmax - 100)*_grad + 120.0)
            #
            else:
                print ("Temperature no valid")
                _fytemp = 0.0
        #
        #
        else:
            print("Material No Available for Derating")
            _grad = 1.0
            _fytemp = 0.0
        #
        # Guidance note:
        # Field joint coating application during 
        # installation may also impose temperatures 
        # in excess of the above and shall be considered
        #
        # Guidance note:
        # If no other information of de-rating effects
        # on the yield stress exist the recommendations
        # for C-Mn steel and Duplex steels Figure
        # 2 below may be used. For 13Cr testing is 
        # normally required.
        #
        # Guidance note:
        # If no other information on de-rating effect
        # of the ultimate stress exists, the de-rating 
        # of the yield stress can be conservatively
        # applied
        #
        # 305 Any difference in the de-rating effect 
        # of temperature for tension and compression
        # shall be accounted for.
        #
        # Guidance note:
        # Difference in de-rating effect for tension and compression has
        # been experienced on 13Cr steel material.
        #
        #
        # 302 The characteristic material strength fy 
        # and fu, values to be used in the limit state
        # criteria are:
        #
        # Where:
        # fy,temp and fu,temp are the de-rating values due 
        # to the temperature of the yield stress and the 
        # tensile strength respectively, see 304.
        # 
        # 
        # AlphaU is the material strength factor, 
        # see Table 5-6.
        #
        #
        Fy = ((self.SMYS - _fytemp) * alpha_U)
        Fu = ((self.SMTS - _fytemp) * alpha_U)
        print ("Derated Material ======>",material_type )
        print ("Derating Factors", alpha_U,_grad,_fytemp)
        print ("fy", Fy)
        print ("fu", Fu)
        #
        return Fy, Fu
        #
    #
    #
    #
#
