# 
# Copyright (c) 2019 iLift
#
# Python stdlib imports
from math import pi

# package imports
from iLift.codes.process.process import AxialResults
#

#
def Chapter_E(self, section, material):       # @hami2230 - widespread changes made to Chapter E.
    """
    DESIGN OF MEMBERS FOR COMPRESSION\n
    This chapter addresses members subject to axial compression through the centroidal axis.
    The chapter is organized as follows:   
    E1. General Provisions
    E2. Effective Length
    E3. Flexural Buckling of Members without Slender Elements
    E4. Torsional and Flexural-Torsional Buckling of Members without Slender Elements
    E5. Single Angle Compression Members
    E6. Built-Up Members
    E7. Members with Slender Elements
    """
    #print('')        
    #print('-----------------------------')
    #print('   Chapter E - Compression')
    #print('')
    #
    ## Stability Factors
    #print("Kx = {:2.3f}".format(self.Kx))
    #print("Ky = {:2.3f}".format(self.Ky))
    #print("Kz = {:2.3f}".format(self.Kz))
    
    _warning = ""
    # E1 General Provisions
    self.Omega_c = 1.0
    self.Phi_c = 1.0
    
    # ASD
    if 'asd' in self.design_method.lower():
        self.Omega_c = 1.67
    # user defined
    elif 'user_defined' in self.design_method.lower():
        self.Omega_c = self.user_factor_c                                      
    # LRFD
    else:
        self.Phi_c = 0.90
    #
    # E2 Efective Length
    _kLrx = self.Ky * self.L / section.ry  # Major Axis
    _kLry = self.Kz * self.L / section.rz  # Minor Axis
    
    self.KLr = _kLry
    if _kLrx > _kLry: 
        self.KLr = _kLrx
    #
    if self.KLr > 200: 
        #raise Warning("  ** KLr {:1.2f} > 200".format(self.KLr.value))
        _warning = "KLr {:1.2f} > 200".format(self.KLr.value)
        print('  **Warning ' + _warning)
    #else: 
    #    print("KLr {:2.3f} < 200".format(self.KLr.value))
        
    if 'i section' in section.type.lower():
        #
        #       --------------------------------------------------------
        #                               CONSTANTS
        #       --------------------------------------------------------
        #            
        # Coordinate of Shear Centre with respect to
        # the centroid (_xo & yo)
        _xo = section.SCy
        # _yo = section.SCz
        _yo = abs(section.Zc - (section.d - section.SCz - section.tfb/2))      # @hami2230 - 31.01.19 - error corrected.
        #
        # Polar radius of gyration about the shear centre
        _ro2 = (_xo**2 + _yo**2 + ((section.Iy + section.Iz) / section.Ag))
        #
        # (E4-8) Flexural Constant H
        _H_E4_8 = 1.0 - ((_xo**2 + _yo**2) / _ro2)
        #
        # (E4-5) Elastic Flexural Buckling Stress Major Axis
        self.Fex_E4_5 = material.E * pi**2 / _kLrx**2
        #
        # (E4-6) Elastic Flexural Buckling Stress Minor Axis
        self.Fey_E4_6 = material.E * pi**2 / _kLry**2
        #        
        # (E4-7) Elastic Torsional Buckling
        self.Fez_E4_7 = (((section.Cw * material.E * pi**2 / (self.Kz * self.L)**2) 
                          + material.G * section.J) / (section.Ag * _ro2))
        #
        # (E3-4) Elastic Buckling Stress Fe
        _Fe_E3_4 = material.E * pi**2 / self.KLr**2
        #
        # (E4-2) Elastic Buckling Stress Fe
        _Fe_E4_2 = (((section.Cw * material.E * pi**2 / (self.Kz * self.L)**2) 
                     + material.G * section.J) * 1.0 / (section.Iy + section.Iz))
        #
        # (E4_3) Elastic Buckling Stress Fe
        _Fe_E4_3 = (((self.Fey_E4_6 + self.Fez_E4_7) / (2 * _H_E4_8)) 
                    * (1.0 - (1.0 - ((4.0 * self.Fey_E4_6 * self.Fez_E4_7 * _H_E4_8) 
                                     / (self.Fey_E4_6 + self.Fez_E4_7)**2))**0.5))
        #
        #      --------------------------------------------------------
        #                E3 & E4 - Fcr Critical Stress
        #      --------------------------------------------------------
        #
        # E3 Flexural Buckling - Fcr                                           
        #                                 
        # The Critical Stress, Fcr for E3:
        #
        # (b) (E3-3)
        if self.KLr > 4.71*(material.E / material.Fy)**0.5 or (material.Fy / _Fe_E3_4) > 2.25: # @hami2230 - added sqrt to first term
            _Fcr_E3 = 0.877 * _Fe_E3_4
            _Fcr_E3_flag = '(E3-3)'
            _Fe_E3 = _Fe_E3_4
            _Fe_E3_flag ='(E3-4)'
        # (a) (E3-2)
        else:
            _Fcr_E3 = (0.658**(material.Fy.convert('megapascal').value / _Fe_E3_4.convert('megapascal').value)) * material.Fy            
            _Fcr_E3_flag = '(E3-2)'
            _Fe_E3 = _Fe_E3_4
            _Fe_E3_flag = '(E3-4)'
        #  
        # E4 Torsional and Flexural Buckling - Fcr         
        #
        # (a) Doubly Symmetric Members
        if 'symmetrical i section' == section.type.lower():                       
            _Fe_E4 = _Fe_E4_2
            _Fe_E4_flag ='(E4-2)'
        # (b) Singly Symmetric Members
        elif 'asymmetrical' in section.type.lower():
            _Fe_E4 = _Fe_E4_3
            _Fe_E4_flag ='(E4-3)'
        # (c) Other Members
        else:
            raise NotImplemented('Section type not implemented yet')
        #
        # The Critical Stress, Fcr for E4:
        #
        # (b) (E3-3)
        if self.KLr > 4.71 * (material.E / material.Fy)**0.5 or (material.Fy / _Fe_E4) > 2.25:  # @hami2230 - error corrected here (square root was missing)
            _Fcr_E4 = 0.877 * _Fe_E4
            _Fcr_E4_flag = '(E3-3)'
        # (a) (E3-2)
        else:
            _Fcr_E4 = (0.658**(material.Fy.value / _Fe_E4.value)) * material.Fy
            _Fcr_E4_flag = '(E3-2)'
        #      --------------------------------------------------------
        #            E3 & E4 - Pn without slender elements
        #      --------------------------------------------------------
        #
        # Calculate min applicable Fcr
        #
        # E3 - All nonslender-element compression members
        #
        # E4 - This section applies to singly symmetric and unsymmetric 
        # members, certain doubly symmetric members, such as cruciform or 
        # built-up members, and doubly symmetric  members when the 
        # torsional unbraced length exceeds the lateral unbraced length, all
        # without slender elements.
        #
        if 'asymmetrical' in section.type.lower():
            self.Fcr_E = min(_Fcr_E4, _Fcr_E3)         
        elif section.build != 'rolled':
            self.Fcr_E = min(_Fcr_E4, _Fcr_E3)   
        elif section.type.lower() == 'symmetrical i section' and self.Kz*self.L <= self.Lb:
            self.Fcr_E = min(_Fcr_E4, _Fcr_E3)
        else:
            self.Fcr_E = _Fcr_E3
        #    
        # Calculate Pn for Section E3 and E4
        #
        if self.compactness.compression.lower() != 'slender':
            self.Pn_E_n = self.Fcr_E * section.Ag
            #
            if self.Fcr_E == _Fcr_E3:
                self.Pn_E_flag = '(E3-1)'
                self.Fe = _Fe_E3
                self.Fe_flag = _Fe_E3_flag
                self.Fcr_E_flag = _Fcr_E3_flag
            else:
                self.Pn_E_flag = '(E4-1)'  
                self.Fe = _Fe_E4
                self.Fe_flag = _Fe_E4_flag
                self.Fcr_E_flag = _Fcr_E4_flag
        #
        #      --------------------------------------------------------
        #               E7 - Pn with slender elements                          # @hami2230 - section has been re-written to AISC360-2016 
        #      --------------------------------------------------------
        #                                                                                                                          
        #      This section applies to slender-element compression members, 
        #      as defined in Section B4.1 for elements in axial compression.
        #      The nominal compressive strength, Pn, shall be the lowest value 
        #      based on the applicable limit states of flexural buckling, 
        #      torsional buckling, and flexural-torsional buckling in 
        #      interaction with local buckling.
        #
        else:
            #
            #      E7-1 - Slender Element Members Excluding Round HSS
            #
            # Table E7.1 - unstiffened I sections only implemented
            c1 = 0.22
            # (E7-4)
            c2 = (1 - (1 - 4*c1)**0.5) / (2*c1)
            #
            # Critical stress for E7
            _Fcr_E7 = self.Fcr_E
            #
            self._he = section.hw
            self._be_top = section.bft
            self._be_bottom = section.bfb
            #
            # If web is slender, find h_e
            #)
            if self.compactness.web.compression.compactness == "slender":
                # (E7-5)
                F_el_web = ((c2 * self.compactness.web.compression.lambda_r / self.compactness.web.compression.lambda_ratio)**2) * material.Fy
                if self.compactness.web.compression.lambda_ratio <= self.compactness.web.compression.lambda_r * (material.Fy / _Fcr_E7)**0.5:
                    self._he = section.hw
                else:
                    self._he = section.hw * (1 - c1 * (F_el_web / _Fcr_E7)**0.5) * (F_el_web / _Fcr_E7)**0.5
            #
            # If top flange is slender, calculate b_e
            #
            if self.compactness.flange.compression.top == "slender":
                self._ratio_flange_top = 0.5 * section.bft / section.tft
                
                F_el_top_flange = ((c2 * self.compactness.flange.compression.lambda_r / self._ratio_flange_top)**2) * material.Fy
                if self._ratio_flange_top <= self.compactness.flange.compression.lambda_r * (material.Fy / _Fcr_E7)**0.5:
                    self._be_top = section.bft
                else:
                    self._be_top = section.bft * (1 - c1*(F_el_top_flange / _Fcr_E7)**0.5) * (F_el_top_flange/ _Fcr_E7)**0.5
            #
            # If bottom flange is slender, calculate b_e
            #
            if self.compactness.flange.compression.bottom == "slender":
                self._ratio_flange_bottom = 0.5 * section.bfb / section.tfb
                F_el_bottom_flange = ((c2 * self.compactness.flange.compression.lambda_r / self._ratio_flange_bottom)**2) * material.Fy
                if self._ratio_flange_bottom <= self.compactness.flange.compression.lambda_r * (material.Fy / _Fcr_E7)**0.5:
                    self._be_bottom = section.bfb
                else:
                    self._be_bottom = section.bfb * (1 - c1*(F_el_bottom_flange / _Fcr_E7)**0.5) * (F_el_bottom_flange/ _Fcr_E7)**0.5
            # Calculate Ae
            #
            self.A_e = self._be_top * section.tft + self._be_bottom * section.tfb + self._he * section.tw
            #
            # Nominal Compressive Strenght, Pn, with slender elements
            # (E7-1)
            #           
            self.Pn_E_n = _Fcr_E7 * self.A_e
            self.Pn_E_flag = '(E7-1)'
            #
            #           
            if _Fcr_E7 == _Fcr_E3:
                self.Fe = _Fe_E3
                self.Fe_flag = _Fe_E3_flag
                self.Fcr_E_flag = _Fcr_E3_flag
            else:  
                self.Fe = _Fe_E4
                self.Fe_flag = _Fe_E4_flag
                self.Fcr_E_flag = _Fcr_E4_flag
            #
        
        
        self.Pn_E = self.Pn_E_n * self.Phi_c / self.Omega_c
        self.Fa = self.Pn_E / section.Ag
    #
    else:
        if 'bar' in section.type.lower():
            # print('-->')
            # E3 Flexural Buckling - Fcr
            # (E3-4) Elastic Buckling Stress Fe
            _Fe_E3_4 = material.E * pi**2 / self.KLr**2
            #                                 
            # The Critical Stress, Fcr for E3:
            #
            # (b) (E3-3)
            if self.KLr > 4.71*(material.E / material.Fy)**0.5 or (material.Fy / _Fe_E3_4) > 2.25: # @hami2230 - added sqrt to first term
                _Fcr_E3 = 0.877 * _Fe_E3_4
                _Fcr_E3_flag = '(E3-3)'
                _Fe_E3 = _Fe_E3_4
                _Fe_E3_flag ='(E3-4)'
            # (a) (E3-2)
            else:
                _Fcr_E3 = (0.658**(material.Fy.convert('megapascal').value / _Fe_E3_4.convert('megapascal').value)) * material.Fy            
                _Fcr_E3_flag = '(E3-2)'
                _Fe_E3 = _Fe_E3_4
                _Fe_E3_flag = '(E3-4)'
            #
            #
            self.Pn_E_flag = '(E3-1)'
            self.Fe = _Fe_E3
            self.Fe_flag = _Fe_E3_flag
            self.Fcr_E_flag = _Fcr_E3_flag
            #
            self.Fcr_E = _Fcr_E3
            self.Pn_E_n = self.Fcr_E * section.Ag
            self.Pn_E = self.Pn_E_n * self.Phi_c / self.Omega_c
            self.Fa = self.Pn_E / section.Ag            
        else:
            raise NotImplemented('Section type not implemented yet')
    #                                                        
    #print("Fe  = {: 6.0f} N/mm2 {:}"
    #      .format(self.Fe.convert('megapascal').value, self.Fe_flag))
    #print("Fcr = {: 6.0f} N/mm2 {:}"
    #      .format(self.Fcr_E.convert('megapascal').value, self.Fcr_E_flag))
    #print("Pn  = {: 6.0f} kN    {:}"
    #      .format(self.Pn_E.convert('kilonewton').value, self.Pn_E_flag))
    #print("Fa  = {: 6.0f} N/mm2 ".format(self.Fa.convert('megapascal').value))
    ur_out = max([abs(_item.value)/self.Fa.value for _item in self.stress.sigma_x])
    #print("UR  = {:1.3f}".format(ur_out))
    #print(" ")
    self.Axial_results  = AxialResults(ur_out, self.Pn_E_flag, self.Fa, _warning)    
#