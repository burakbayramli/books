# 
# Copyright (c) 2021 steelpy
#
# Python stdlib imports

# package imports
from iLift.codes.process.process import AxialResults
#

#
def Chapter_D(self, section, material):
    """
    DESIGN OF MEMBERS FOR TENSION\n
    This chapter applies to members subject to axial tension caused by static forces acting through the centroidal axis.   
    The chapter is organized as follows:
    D1. Slenderness Limitations
    D2. Tensile Strength
    D3. Effective Net Area
    D4. Built-Up Members
    D5. Pin-Connected Members
    """

    #print('')
    #print('-----------------------------')
    #print('     Chapter D - Tension')
    #print('')
    #
    _warning = ""
    # D1 Slender Limitation
    _L_rx = self.L / section.ry
    _L_ry = self.L / section.rz
    #
    self.L_r = max(_L_rx, _L_ry)
    #        
    if self.L_r > 300 : 
        #pass
        #print ("L/r ({:1.2f}) < 300".format(self.L_r.value))
    #else: 
        _warning = "L/r ({:1.2f}) > 300".format(self.L_r.value)
        print("** warning " + _warning)
        #raise Warning("** warning :  L/r ({:1.2f}) > 300".format(self.L_r.value))
    #        
    
    # D3 Effective Net Area
    #
    # U factor < 1 not applicable for solid continuous beam
    _U_D3 = 1                                                   
    # Assuming I shape --> Table D3.1 - 7
    #_U_D3 = 0.90
    #if 'I section' in section.type:
    #    if section.bf < 2/3.0 * section.d:
    #        _U_D3 = 0.85
    _Ae_D3_1 = section.An * _U_D3

         
    # D2 Tensile Strength
    _LRFD_D2a = 1.0
    _LRFD_D2b = 1.0
    _ASD_D2a = 1.0
    _ASD_D2b = 1.0
    
    if 'asd' in self.design_method.lower():
        _ASD_D2a = 1.67
        _ASD_D2b = 2.0
    elif 'user_defined' in self.design_method.lower():                         
        _ASD_D2a = self.user_factor_t
        _ASD_D2b = self.user_factor_t
    else:
        _LRFD_D2a = 0.9
        _LRFD_D2b = 0.75
    #
    # a) For tensile yielding i the gross section
    _Pn_D2a = material.Fy * section.Ag
    _Pn_D2a_Flag ='(D2-1)'
    #
    # b) For tensile rupture in the net section       
    _Pn_D2b = material.Fu * _Ae_D3_1
    _Pn_D2b_Flag ='(D2-2)'
    # 
    # Take the lower value
    if _Pn_D2a * _LRFD_D2a / _ASD_D2a < _Pn_D2b * _LRFD_D2b / _ASD_D2b :
        self.Pn_D = _Pn_D2a * _LRFD_D2a / _ASD_D2a 
        self.Omega_t = _ASD_D2a
        self.Phi_t = _LRFD_D2a 
        self.Pn_D_Flag = _Pn_D2a_Flag
    else:
        self.Pn_D = _Pn_D2b * _LRFD_D2b / _ASD_D2b 
        self.Omega_t = _ASD_D2b
        self.Phi_t = _LRFD_D2b
        self.Pn_D_Flag = _Pn_D2b_Flag
    #
    self.Pc = self.Pn_D                # @hami2230 removed "* self.Phi_t / self.Omega_t" from this equation. These factors have already been included above.                            
    self.Fa = self.Pc / section.Ag
    #
    #print("Pn = {: 6.0f} kN {:}".format(self.Pc.convert('kilonewton').value, self.Pn_D_Flag))
    #print("Ft = {: 6.0f} N/mm2 ".format(self.Fa.convert('megapascal').value))
    ur_out = max([abs(_item.value)/self.Fa.value for _item in self.stress.sigma_x])
    #print("UR = {:1.3f}".format(ur_out))
    #print(' ')
    self.Axial_results = AxialResults(ur_out, self.Pn_D_Flag, self.Fa, _warning)
    #self.ChapterD_results = ChapterResults(ur_out, ur_out, self.Pn_D_Flag)
# 