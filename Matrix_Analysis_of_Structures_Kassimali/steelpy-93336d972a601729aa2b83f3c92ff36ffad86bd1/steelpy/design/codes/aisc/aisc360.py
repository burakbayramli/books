# 
# Copyright (c) 2019-2021 steelpy
#
#
# Python stdlib imports
#import datetime

#
# package imports
#from iLift.codes.AISC360.July2016.AISC360_16 import *
#from iLift.codes.AISC360.print_report import *
#from iLift.codes.process.process import SummaryResults
#
#
#
#-------------------------------------------------
#
class AISC_360_16:
    #
    def __init__(self, cls_design):
        """
        """
        #
        # ----- General Data -----
        self._units = cls_design._units
        self.beam_name = cls_design.item_name
        #
        #  ----- Material -----
        self._material = cls_design._material
        self._section = cls_design._section
        #
        # ----- Stiffener Spacing ----- 
        self.stiffned_element = cls_design.stiffned_element
        self.a = cls_design._a
        self.tst = cls_design.tst
        self.Fyst = cls_design.Fyst   
        self.tension_field_action = cls_design.tension_field_action      
        #
        # ----- Unbraced Length -----
        self.L = cls_design.L
        self.Lb = cls_design.Lb
        #
        # ----- user factors -----
        self.user_factor_t = cls_design.user_factor_t
        self.user_factor_c = cls_design.user_factor_c
        self.user_factor_B = cls_design.user_factor_B
        self.user_factor_V = cls_design.user_factor_V        
        #
        # ----- StabilityFactors ----- 
        self.Kx = cls_design.Kx
        self.Ky = cls_design.Ky
        self.Kz = cls_design.Kz
        #
        # ----- Moment Modifiers -----
        self.Cb = cls_design.Cb
        #
        #  ----- element Forces -----
        #if cls_design.actions:
        self.actions = cls_design.actions
        #else:
        #    self.actions = Actions(Fx = 0 * self._units.N, 
        #                           Fy = 0 * self._units.N, 
        #                           Fz = 0 * self._units.N, 
        #                           Mx = 0 * self._units.N * self._units.m, 
        #                           My = 0 * self._units.N * self._units.m, 
        #                           Mz = 0 * self._units.N * self._units.m)
        # ----- internal stress -----
        self.stress = cls_design.stress
        #
        self.shear_stress = cls_design.shear_stress
    #
    def get_compacness(self):
        """
        """
        #
        self.compactness = Chapter_B(self, self._section, self._material)
        
        self.file_out.extend(self._section._print_section_properties())
        if not 'bar' in self._section.type:
            comp_out = print_compactness(self, self._section)
            self.file_out.extend(comp_out)        
    #
    def AISC_bending_check(self, design_method:str ='asd'):
        """
        """
        #
        try:
            self.L
        except AttributeError:
            raise IOError("   ** member length must be provided")
        
        self.design_method = design_method
        #print('Design Method {:}'.format(self.design_method.upper()))
        # 
        self.file_out = print_header() # 'AISC_14ed.out'
        self.file_out.extend(self._material._print_material())
        #
        self.get_compacness()
        #
        # 
        # Check Lateral Unbraced Length
        if not self.Lb:
            self.Lb = self.L
        #
        # Bending check
        Chapter_F(self, self._section, self._material)
        #
    #
    def AISC_shear_check(self, design_method:str ='asd'):
        """
        """
        self.design_method = design_method
        #print('Design Method {:}'.format(self.design_method.upper()))
        #         
        self.file_out = print_header() # 'AISC_14ed.out'
        self.file_out.extend(self._material._print_material())
        #
        self.get_compacness()        
        # Shear check 
        Chapter_G(self, self._section, self._material)
        self.file_out.extend(printChaperG(self, self._section, self._material))
    #
    def get_AISC_results(self, design_method:str ='asd'):
        """
        """
        #self.beam_name = self.item_name
        #print("{:_<87}\n".format(""))
        #
        try:
            self.L
        except AttributeError:
            raise IOError("   ** member length must be provided")
        
        self.design_method = design_method
        #print('Design Method {:}'.format(self.design_method.upper()))
        # 
        self.file_out = print_header() # 'AISC_14ed.out'
        self.file_out.extend(self._material._print_material())
        #
        self.get_compacness()
        #
        # 
        # Check Lateral Unbraced Length
        if not self.Lb:
            self.Lb = self.L
        #
        # Bending check
        Chapter_F(self, self._section, self._material)
        #
        # Shear check 
        Chapter_G(self, self._section, self._material)
        self.file_out.extend(printChaperG(self, self._section, self._material))
        #
        #
        fa = max([_item.value for _item in self.stress.sigma_x])
        #
        # Axial check
        if fa < 0.0:
        #if self.actions.Fx.value < 0.0:
            self.FAxial = 'compression'
            Chapter_E(self, self._section, self._material)
            # Combination check
            Chapter_H_stress(self, self._section)
            self.file_out.extend(printChapterE(self, self._section))
        else:
            self.FAxial = 'tension'
            Chapter_D(self, self._section, self._material)
            # Combination check
            Chapter_H_stress(self, self._section)
            self.file_out.extend(printChapterD(self, self._section))
        #
        _shear = max(self.ChaperG_results[:2])
        _total_UR, _URStatus, _UR_flag = self.ChapterH_results
        
        # Summary UR
        if _shear > _total_UR :
            _total_UR = _shear
            _UR_flag = self.ChaperG_results.UR_flag
            if _total_UR > 1.0:
                _URStatus = 'FAIL'
        #print('ok')
        self.UR_results = SummaryResults(_total_UR, _URStatus, _UR_flag)
        print ("UR = {:2.3f}  {:}".format(_total_UR, _UR_flag))
    #
    def print_results(self):
        """
        """
        #self.Header = 0
        #
        #printHeader()
        #
        # Print Results
        #self._section.print_file(self.file_out)
        #print_section_compacness(self.beam_name)
        #AISC.PrintProperty(self)
        #Shape.PrintProperty(self)
        #Shape.PrintPropertyShort(self)
        #PrintSummary(self)
        #
        #self.Header = self.Header + 1
        #
        return self.file_out
        #  
    #    
#   
#
