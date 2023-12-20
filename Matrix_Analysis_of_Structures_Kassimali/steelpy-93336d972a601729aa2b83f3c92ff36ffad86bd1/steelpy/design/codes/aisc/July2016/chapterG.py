# 
# Copyright (c) 2019 iLift
#
# Python stdlib imports

# package imports
from iLift.codes.process.process import ChapterResults
#

#  
def Chapter_G(self, section, material):   # @hami2230 - widespread changes to this chapter due to changes in the 2016 version of the code.
    """
    DESIGN OF MEMBERS FOR SHEAR\n
    This chapter addresses webs of singly or doubly symmetric members subject to shear in 
    the plane of the web, single angles and HSS sections, and shear in the weak direction
    of singly or doubly symmetric shapes.\n
    The chapter is organized as follows:\n
    G1. General Provisions
    G2. I-Shapped Members and Channels
    G3. Single Angles and Tees
    G4. Rectangular HSS, Box-Shaped Sections and Singly Symmetric Shapes
    G5. Round HSS
    G6. Weak Axis Shear in Doubly Symmetric and Singly Symmetric Shapes
    G7. Beams and Girders with Web Openings
    """
    #
    #print(" ")        
    #print("-----------------------------")
    #print("     Chapter G - Shear ")
    #print(" ")
    #print("Shear Stress Calc :", self.shear_stress)
    #print(" ")
    #print("        Main Axis ")
    #
    # G1 For all provisions in this chapter
    #
    if 'asd' in self.design_method.lower():
        self.Omega_shear = 1.67
        self.Phi_shear = 1.0
    elif 'user_defined' in self.design_method :
        self.Omega_shear = self.user_factor_V
        self.Phi_shear = 1.0 
    else:
        self.Omega_shear = 1.0
        self.Phi_shear = 0.9
    #
    # G2 I-Shaped Members and Channels
    #
    if "i section" in section.type.lower():
        #
        # -----------------------Constants -----------------------------
        #
        _h = section.hw
        if "rolled" in section.build:
            _h = section.ho
        #
        self.htw_Gx = _h / section.tw
        self.Aw = section.d * section.tw
        self.Af_t = section.bft*section.tft
        self.Af_b = section.bfb*section.tfb
        self.Af = self.Af_t + self.Af_b
        self.VryMax = max([abs(_item.value*self.Af) for _item in self.stress.tau_y])
        self.VrzMax = max([abs(_item.value*self.Aw) for _item in self.stress.tau_z])
        #
        # Clear distance between transverse stiffeners
        self.a_max_length = _h * min(3.0, (260 / (_h / section.tw))**2 )
        #
        #print("h/tw = {:1.2f}".format(self.htw_Gx.value))
        #
        # G2 I-Shaped Members and Channels
        #
        # G2.1 Webs without Tension Field Action
        #
        # G2.1.(a) Web of rolled I-shaped members with h/tw <= 2.24 * (material.E / material.Fy)**0.5
        #
        if "rolled" in section.build.lower() and self.htw_Gx <= 2.24 * (material.E / material.Fy)**0.5:
            self.Cv1 = 1
            self.Cv1_flag = '(G2-2)'
            self.Kv = 1
            
            if 'asd' in self.design_method.lower():
                self.Omega_shear = 1.5
                self.Phi_shear = 1.0
            elif 'user_defined' in self.design_method:
                self.Omega_shear = self.user_factor_V
                self.Phi_shear = 1.0 
            else:
                self.Omega_shear = 1.0
                self.Phi_shear = 1
        #
        # G2.1.(b) All other I-shaped members and channels without tension field action
        # 
        else:
            # (2) Calculate web shear buckling coefficient, Kv
            #
            # For for webs without transverse stiffeners
            self.Kv = 5.34
            #
            # *** Hold - working for transverse stiffeners ***                 # @hami2230 - transverse stiffers not fully coded.
            # if not self.a:            
            #    self.Kv = 5.34
            #
            #else:
            #    if self.a / _h > 3:
            #        self.Kv = 5.43
            #    else:
            #        self.Kv = 5 + 5 / (self.a / self.htw_Gx)
            # *** Hold end ***
            #
            # (1) Calculate Cv1 web shear strength coefficient
            #
            # (i)
            if self.htw_Gx <= (1.10 * (self.Kv * material.E / material.Fy)**0.5):
                self.Cv1 = 1.0
                self.Cv1_flag = '(G2-3)'
            # (ii)
            else:
            # (G2-4)
                self.Cv1 = (1.10 * (self.Kv * material.E / material.Fy)**0.5 / self.htw_Gx).value
                self.Cv1_flag = '(G2-4)'
        #    
        # Calculate shear strength without tension field action
        # (G2-1)
        self.Vny_G2_1 = (0.6 * material.Fy * self.Aw * self.Cv1)
        self.Vny_G2_1_flag = '(G2-1)'
        #
        # *** Hold - working for transverse stiffeners ***                     # @hami2230 - transverse stiffers not fully coded.
        # G2.2 Shear Strength of Webs without Tension Field Action
        #
        #self.Vny_G2_2 = self.Vny_G2_1
        #self.Cv2 = self.Cv1
        #
        # Only evalulate section G2.2 and G2.3 for webs with stiffeners
        #
        #if self.a:                                                             
        #    #                
        #    if 'yes' in self.tension_field_action.lower() and self.a/_h <= 3:   
        #        #   
        #        print ("-- Tension Field Action On --")
        #        print ("a/h = {:1.2f}".format(self.a / _h))
        #        #
        #        # Calculate web shear buckling coefficient, Cv2
        #        #
        #        # (i)
        #        if self.htw_Gx <= 1.10 * (self.Kv * material.E / material.Fy)**0.5:
        #            self.Cv2 = 1.0
        #            self.Cv2_flag = '(G2-9)'
        #        # (ii)
        #        elif (1.10 * (self.Kv * material.E / material.Fy)**0.5 < self.htw_Gx 
        #              and self.htw_Gx <= 1.37 * (self.Kv * material.E / material.Fy)**0.5):
        #            self.Cv2 = 1.10 * (self.Kv * material.E / material.Fy)**0.5 / self.htw_Gx
        #            self.Cv2_flag = '(G2-10)'
        #        # (iii)
        #        else:
        #            self.Cv2 = 1.51 * self.Kv * material.E / ((self.htw_Gx)**2 * material.Fy) 
        #            self.Cv2_flag = '(G2-11)'
        #        #
        #        # Calculate nominal shear strength, Vn, with tension field action
        #        #
        #        # (a)
        #        if self.htw_Gx <= 1.10 * (self.Kv * material.E / material.Fy)**0.5:
        #           self.Vny_G2_2 = (0.6 * material.Fy * self.Aw)
        #           self.Vny_G2_2_flag = '(G2-6)'
        #        # (b)
        #        else:
        #            # (1)
        #            if (2 * self.Aw / (self.Af_t + self.Af_b) <= 2.5 
        #                and _h / section.bft <= 6 
        #                and _h / section.bfb <= 6):
        #                self.Vny_G2_2 = (0.6 * material.Fy * self.Aw 
        #                            * (self.Cv2 + (1 - self.Cv2) / (1.15 * (1 + (self.a / _h)**2)**0.5)))
        #                self.Vny_G2_2_flag = '(G2-7)'
        #            # (2)
        #            else:
        #                self.Vny_G2_2 = (0.6 * material.Fy * self.Aw *
        #                            (self.Cv2 + (1 - self.Cv2) / (1.15 * (self.a / _h + (1 +(self.a / _h)**2)**0.5))))
        #                self.Vny_G2_2_flag = '(G2-8)'                    
        #                #            
        #    else:
        #        print ("Not implemented")
        #                
        # self.Vny = max(self.Vny_G2_1, self.Vny_G2_2)
        # if self.Vny == self.Vny_G2_1:
        #    self.Cv = self.Cv1
        # else:
        #    self.Cv = self.Cv2
        # self.Vny_Flag = '(TBC)'
        # self.Cv_Flag = '(TBC)'
        # *** Hold end ***
        #
        self.Vny = (self.Vny_G2_1)
        self.Cvy = self.Cv1
        self.Vny_Flag = '(G2-1)'
        self.Cvy_Flag = self.Cv1_flag
        #
        # Check if transverse stiffeners are required
        if self.htw_Gx > (2.46*(material.E/material.Fy)**0.5) or self.actions.Fy > self.Vny:
            self.TransvStiffeners= 'Required'               
            #print ("Transverse Stiffeners : Required")
        else:
            #print ("Transverse Stiffeners : Not Required")
            self.TransvStiffeners= 'norequired'  
            
            
        #if self.Kv != 1:
        #    print("Kv =",self.Kv)
        #
        #print("Cv = {:2.3f}    {:}".format(self.Cvy, self.Cvy_Flag))
        #print("Vn = {:2.3f} kN {:}".format(self.Vny.convert('kilonewton').value , 
        #                                   self.Vny_Flag ))
        #
        # vertical
        self.Vnz = (self.Vny * self.Phi_shear / self.Omega_shear)
        self.fvz = self.Vnz / self.Aw
        #
        # G6 Weak Axis Shear in Singly and Doubly Symmetric Shapes
        #
        # For singly and doubly symmetric shapes loaded in the
        # weak axis without torsion, the nominal shear strength  
        # Vn, for shear resisting element shall be determined
        #
        #print("")
        #print("        Weak Axis      ")                                       
        #
        if 'asd' in self.design_method.lower():
            self.Omega_shear = 1.67
            self.Phi_shear = 1.0
        elif 'user_defined' in self.design_method :
            self.Omega_shear = self.user_factor_V
            self.Phi_shear = 1.0 
        else:
            self.Omega_shear = 1.0
            self.Phi_shear = 0.9
        #
        self.htw_Gy = max(section.bft/(2*section.tft), section.bfb/(2*section.tfb)) # @hami2230 - error corrected here (* 2 missing on denominator)
        # print ('h/tw Weak Axis=',self.htw_Gy)
        self.Kvx = 1.20
        # 
        # Cv2 from G2.2  
        #    
        #
        # (i) When h/tw <= 1.10*(Kv*E/Fy)^0.5
        if self.htw_Gy <= 1.10 * (self.Kvx * material.E / material.Fy)**0.5:              
            self.Cvx = 1.0
            self.Cvx_flag = '(G2-9)'
        # (ii) When 
        elif (1.10 * (self.Kvx * material.E / material.Fy)**0.5 < self.htw_Gy 
              <= 1.37 * (self.Kvx * material.E / material.Fy)**0.5):
            #
            self.Cvx = (1.10 * (self.Kvx * material.E / material.Fy)**0.5 / self.htw_Gy)
            self.Cvx_flag = '(G2-10)'
        # (iii) When h/tw > 1.37*(Kv*E/Fy)^0.5
        else:
            self.Cvx = 1.51 * material.E * self.Kvx / (material.Fy * self.htw_Gy**2)
            self.Cvx_flag = '(G2-11)'
        #
        # G6.1 Shear Strength
        #
        self.Vnx = 0.6 * material.Fy * self.Af * self.Cvx
        self.Vnx_Flag  = '(G6-1)'
        #
        #print("Cv = {:2.3f}    {:}".format(self.Cvx, self.Cvx_flag))
        #print("Vn = {:2.3f} kN {:}".format(self.Vnx.convert('kilonewton').value , 
        #                                   self.Vnx_Flag ))
        #
        #
        # lateral
        self.Vny = (self.Vnx * self.Phi_shear / self.Omega_shear)
        self.fvy = self.Vny / self.Af
        #    
    elif section.type.lower() == "rectangular bar":
        Cv = 1.0
        self.Cvx = Cv
        self.Cvy = Cv
        self.Vny = 0.60 * material.Fy * section.Ag * Cv
        self.Vny_Flag = '(G4-1)'
        self.Vnx = self.Vny
        self.Vnx_Flag = '(G4-1)'
        #
        # vertical
        self.Vnz = (self.Vny * self.Phi_shear / self.Omega_shear)
        self.fvz = self.Vnz / section.area 
        # lateral
        self.Vny = (self.Vnx * self.Phi_shear / self.Omega_shear)
        self.fvy = self.Vny / section.area
        #
        self.htw_Gx = section.d / section.w
        self.htw_Gy = section.w / section.d
        self.TransvStiffeners = 'norequired'
        self.Kv = 1
        self.Kvx = 1
        self.VryMax = max([abs(_item.value*section.area ) for _item in self.stress.tau_y])
        self.VrzMax = max([abs(_item.value*section.area ) for _item in self.stress.tau_z])
        #
    else:
        raise Exception('section {:} not yet implemented'.format(section.typ))
        
    #
    # print 'TensionFieldAction :', self.tension_field_action
    #
    #print('')
    #print('Vertical')
    #print("Vnz = {:1.2f} kN {:}".format(self.Vnz.convert('kilonewton').value,
    #                                   self.Vny_Flag ))
    #print("fvz = {:1.2f} MPa".format(self.fvz.convert('megapascal').value))
    ur_outz = max([abs(_item.value/self.fvz.value) for _item in self.stress.tau_z])
    #print("URz  = {:1.2}".format(ur_outz))    
    #print('')
    #print('Horizontal')
    #print("Vny = {:1.2f} kN {:}".format(self.Vny.convert('kilonewton').value,
    #                                    self.Vnx_Flag ))
    #print("fvy = {:1.2f} MPa".format(self.fvy.convert('megapascal').value))
    ur_outy = max([abs(_item.value/self.fvy.value) for _item in self.stress.tau_y])
    #print("URy  = {:1.2}".format(ur_outy))
    #
    
#    print("Vnz")
#    print(self.Vnz.convert("kilonewton").value)
#    print("fvz")
#    print(self.fvz.value)
#    print(self.Omega_shear)
#    print(self.Vny_Flag)
    
    if ur_outz > ur_outy:
        self.ChaperG_results = ChapterResults(ur_outy, ur_outz, self.Vny_Flag, self.fvy, self.fvz)
    else:
        self.ChaperG_results = ChapterResults(ur_outy, ur_outz, self.Vnx_Flag, self.fvy, self.fvz)

#