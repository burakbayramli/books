# 
# Copyright (c) 2021 steelpy
#
# Python stdlib imports
from typing import NamedTuple, Tuple

# package imports
#

#
# self.compacness_compression, self.compacness_flexure
#
class ItemClass(NamedTuple):
    """
    """
    compression:Tuple
    flexure:Tuple
#
class Compactness:
    """
    """
    __slots__ = ['compression', 'flexure', 'flange', 'web']
    
    def __init__(self, compression:str, flexure:str,
                 flange=None, web=None):
        self.compression = compression
        self.flexure = flexure
        self.flange = flange
        self.web = web
#
class FlangeClass(NamedTuple):
    """
    """
    compactness:str
    top:str
    bottom:str
    lambda_ratio:float
    lambda_p:float
    lambda_r:float
#
class WebClass(NamedTuple):
    """
    """
    compactness:str
    lambda_ratio:float
    lambda_p:float
    lambda_r:float
#
#
# @hami2230 - I've modified this section to return more values for use in other sections.
def Chapter_B(self, section, material): 
    '''
    Chapter B
    This chapter addresses general requirements for the analysis
    and design of steel structures applicable to all chapters of
    the specification.
    
    This chapter is organized as follows:
    B4. Member Properties
    '''
    #global SectionType, Build, SecComp
    
    # B4.3 Gross and Net Area Determination
    # B4.3a Gross Area
    # The gross area, Ag, of a member is the total cross-sectional area.
    section.Ag = section.area
    
    # B4.3b Net Area
    # For members without holes, the net area, An, is equal to the
    # gross area, Ag.
    section.An = section.Ag
    #
    if 'i section' in section.type.lower():
        # hc --> twice the distance from the centroid to the following:
        #         --The inside face of the compression flange less the 
        #           fil1 let or corner radius, 
        #         --For rolled shapes; the nearest line of fasteners at 
        #           the compression flange or the inside faces of the 
        #           compression flange when welds are used, for 
        #           built-up sections, in. (mm)
        #  
        self.hc = 2*(section.Zc - section.tft)
        
        #
        #    ----- Table B4.1a &  Table B4.1b -----
        #
        (class_B41a_flange, class_B41a_flange_top, class_B41a_bottom, 
         class_B41a_web, B41a_ratio_flange, B41a_lambda_r_flange_a, B41a_ratio_web, 
         B41_lambda_r_web_a) = table_B41a(self, section, material)
        #
        # print('class Table B4.1a :', class_B41a)
        class_B41a = 'nonslender'
        if 'slender' in [class_B41a_flange,  class_B41a_web ]:
            class_B41a = 'slender'
        
        (class_B41b_flange,  class_B41b_web, B41b_ratio_flange, 
         B41b_lambda_p_flange, B41b_lambda_r_flange_b, B41b_ratio_web, 
         B41b_lambda_p_web, B41b_lambda_r_web_b) = table_B41b(self, section, material)      
        #
        # print('class Table B4.1b  flange:', class_B41b_flange,' web:',class_B41b_web)
        if 'slender' in [class_B41b_flange,  class_B41b_web ]:
            class_B41b = 'slender'
        elif "noncompact" in [class_B41b_flange,  class_B41b_web ]:
            class_B41b = "noncompact"
        else:
            class_B41b = "compact"
        # print (' ')
        # for row in comp_out:
        #    print(row.rstrip())
        #         
        #self check code
        # print("")
        # print(section.type.lower())
        # print("")
        # print("*** Compression ***")
        # print("")
        # print("B41a_ratio_flange :", B41a_ratio_flange) 
        # print("B41a_lambda_r_flange_a :", B41a_lambda_r_flange_a)
        # print("class_B41a_flange :", class_B41a_flange)
        # print("class_B41a_flange_top :", class_B41a_flange_top)
        # print("class_B41a_flange_bottom :", class_B41a_bottom)
        # print("")
        # print("class_B41a_web :", class_B41a_web)
        # print("B41a_ratio_web :", B41a_ratio_web) 
        # print("B41_lambda_r_web_a :", B41_lambda_r_web_a)
        # print("")
        # print("class_B41a :", class_B41a)
        # print("")
        # print("*** Bending ***")
        # print("")
        # print("B41b_ratio_flange :", B41b_ratio_flange) 
        # print("B41b_lambda_p_flange :", B41b_lambda_p_flange)
        # print("B41b_lambda_r_flange_b :", B41b_lambda_r_flange_b)
        # print("class_B41b_flange :", class_B41b_flange)
        # print("")
        # print("B41b_ratio_web :", B41b_ratio_web) 
        # print("B41b_lambda_p_web :", B41b_lambda_p_web)
        # print("B41b_lambda_r_web_b :" ,B41b_lambda_r_web_b)
        # print("class_B41b_web :", class_B41b_web)
        # print("")
        # print("class_B41b :", class_B41b)
        #
        # print(section.build.lower())
        #        
        #return (class_B41a, class_B41b, class_B41a_flange, class_B41a_flange_top, 
        #        class_B41a_bottom, B41a_ratio_flange, B41a_lambda_r_flange_a, 
        #        class_B41a_web, B41a_ratio_web, B41_lambda_r_web_a, 
        #        class_B41b_flange, B41b_ratio_flange, B41b_lambda_p_flange, 
        #        B41b_lambda_r_flange_b, class_B41b_web, B41b_ratio_web, 
        #        B41b_lambda_p_web, B41b_lambda_r_web_b)
        #
        # Flange
        _compression = FlangeClass(compactness= class_B41a_flange,
                                   top= class_B41a_flange_top,
                                   bottom= class_B41a_bottom,
                                   lambda_ratio= B41a_ratio_flange.value,
                                   lambda_p=None,
                                   lambda_r= B41_lambda_r_web_a.value)
        
        _flexure = FlangeClass(compactness= class_B41b_flange,
                               top=None,
                               bottom=None,
                               lambda_ratio= B41b_ratio_flange.value,
                               lambda_p= B41b_lambda_p_flange.value,
                               lambda_r= B41b_lambda_r_flange_b.value)
        
        _flange = ItemClass(compression=_compression,
                            flexure = _flexure)
        #
        # Web
        _compression = WebClass(compactness= class_B41a_web,
                                lambda_ratio= B41a_ratio_web.value,
                                lambda_p=None,
                                lambda_r= B41_lambda_r_web_a.value)
        
        _flexure = WebClass(compactness= class_B41b_web,
                            lambda_ratio= B41b_ratio_web.value,
                            lambda_p= B41b_lambda_p_web.value,
                            lambda_r= B41b_lambda_r_web_b.value)
        
        _web = ItemClass(compression=_compression,
                         flexure = _flexure)
        #
        return Compactness(compression=class_B41a, 
                          flexure=class_B41b,
                          flange= _flange,
                          web= _web)
                #
                #class_B41a_flange, class_B41a_flange_top, 
                #class_B41a_bottom, B41a_ratio_flange, B41a_lambda_r_flange_a, 
                #
                #class_B41a_web, B41a_ratio_web, B41_lambda_r_web_a, 
                #
                #class_B41b_flange, B41b_ratio_flange, B41b_lambda_p_flange, 
                #B41b_lambda_r_flange_b, 
                #
                #class_B41b_web, B41b_ratio_web, 
                #B41b_lambda_p_web, B41b_lambda_r_web_b)        

    else:
        comp_out = []
        #return None, None
        return Compactness(compression=None, flexure=None)
#
def table_B41a(self, section, material):
       
    '''
    TABLE B4.1a
    Width-to-Thickness Ratios: Compression Elements
    Members Subject to Axial Compression
    '''   
    
    # h : Clear distance between flanges less the fillet or corner 
    #     radius for rolled shapes; distance between adjacent lines 
    #     of fasteners or the clear distance between flanges when 
    #     welds are used for built-up shapes.
    if section.build == "rolled":
        hw = section.d - section.tft - section.tfb - 2 * section.r
    else:
        hw = section.d - section.tft - section.tfb

    # CASE 1, 2 & 5 <----------
    if 'i section' in section.type.lower():
        _ratio_flange_top = 0.5 * section.bft / section.tft                    # @hami2230 - modified this to calculate and return slenderness of top and bottom flanges. This is required for Section E.7.
        _ratio_flange_bottom = 0.5 * section.bfb / section.tfb
        _ratio_flange = max (_ratio_flange_top, _ratio_flange_bottom)
               
        _case_ratio = 'b/t'
        _case_flag = 'unstiffend'
        # flanges
        # CASE 1
        if "rolled" in section.build.lower():
            self._lambda_r_flange_a = 0.56 * (material.E / material.Fy)**0.50
            _case = '1'
        # CASE 2
        else:
            # Kc shall not be taken less than 0.35 nor
            # greater than 0.76 for calculation purposes.                    
            self.kc = (4.0 / (hw / section.tw)**0.50).value
            if self.kc < 0.35 : 
                self.kc = 0.35
            elif self.kc > 0.76 : 
                self.kc = 0.76
            #print 'Kc =',Kc
            self._lambda_r_flange_a = 0.64 * (self.kc * material.E / material.Fy)**0.50
            _case = '2'
        #
        # Web
        # CASE 5
        _ratio_web = hw / section.tw
        self._lambda_r_web_a = 1.49 * (material.E / material.Fy)**0.50
        _case_web = '5'
          
        #if _ratio_web  > self._lambda_r_web_a:
        #    _ratio_flange = _ratio_web
        #    self._lambda_r_flange_a = self._lambda_r_web_a
        #    _case = _case_web
        #    _case_ratio = 'h/tw'
        #    _case_flag = 'Stiffened'
    # Case 8 <-----------------
    # All other stiffened elements
    else:
        _ratio_flange = section.bft / section.tft
        self._lambda_r_flange_a = 1.49 * (material.E / material.Fy)**0.50
        _case = '8'
        raise IOError('error section not implemented')
    #
    #_class = 'nonslender'
    #if  _ratio > self._lambda_r_flange_a : 
    #    _class = 'slender'
    #
    # Find top flange compacness
    _class_flange_top = "nonslender"
    if  _ratio_flange_top > self._lambda_r_flange_a :
        _class_flange_top = 'slender'

    # Find bottom flange compacness
    _class_flange_bottom = "nonslender"
    if  _ratio_flange_bottom > self._lambda_r_flange_a :
        _class_flange_bottom = 'slender'        
    
    #
    # Find flange compacness
    _class_flange = "nonslender"
    if  _ratio_flange > self._lambda_r_flange_a :
        _class_flange = 'slender'
        
    
    #
    # find web compacness    
    _class_web = "nonslender"  
    if  _ratio_web > self._lambda_r_web_a :
        _class_web = 'slender'
        
    _lambda_r_flange_a = self._lambda_r_flange_a
    _lambda_r_web_a = self._lambda_r_web_a     
        
    return  (_class_flange, _class_flange_top, _class_flange_bottom, _class_web, 
             _ratio_flange, _lambda_r_flange_a, _ratio_web, _lambda_r_web_a)
#
def table_B41b(self, section, material):
    '''
    TABLE B4.1b
    Width-to-Thickness Ratios: Compression Elements
    Members Subject to Flexure
    '''
    # h : Clear distance between flanges less the fillet or corner 
    #     radius for rolled shapes; distance between adjacent lines 
    #     of fasteners or the clear distance between flanges when 
    #     welds are used for built-up shapes.
       
    if section.build == "rolled":
        hw = section.d - section.tft - section.tfb - 2 * section.r
    else:
        hw = section.d - section.tft - section.tfb
    #
    if 'i section' in section.type.lower():
        # flange
        _ratio_flange = max(0.5 * section.bft / section.tft,
                            0.5 * section.bfb / section.tfb)
        
        _case_flange_ratio = 'b/t'
                
        # symmetry
        sec_symmetry = 'doubly'
        if 'asymmetrical' in str(section.type).lower():
            sec_symmetry = 'singly'
                    
        # but shall not be taken less than 0.35 nor 
        # greater than 0.76 for calculation purposes.        
        self.kc = (4.0 / (hw / section.tw)**0.50).value
        if self.kc < 0.35 : 
            self.kc = 0.35
        
        elif self.kc > 0.76 : 
            self.kc = 0.76
        
        # Sxc, Sxt : Elastic section modulus referred to   
        # compression and tension flanges, respectively
        _Sxc = section.Sxc             # @hami2230 - updated in iBeam to be section modulus about NA of the beam
        _Sxt = section.Sxt                                     
        
        # for major axis bending of compact and noncompact 
        # web built-up I-shaped members with :
        self.FL = 0.7 * material.Fy
        if _Sxt / _Sxc < 0.7:
            self.FL = max(material.Fy * (_Sxt / _Sxc), 
                     0.5 * material.Fy)
        
        # Mp = plastic bending moment
        Mpx = material.Fy * section.Zpy  ##Zx
        
        # My is the moment at yielding of the extreme fiber
        self.Mym = material.Fy * section.Zey      # @hami2230 - equation correct - # FIXME removed
        
        #
        # hc : Elastic Centroid to the Compression Flange
        # Twice the distance from the center of gravity to 
        # the following:
        # The inside face of the compression flange less the 
        # fillet or corner radius.
        # For rolled shapes; the nearest line of fasteners at
        # the compression flange or the inside faces of the 
        # compression flange when welds are used, for built-up
        # sections.
        hc = 2 * (section.Zc - section.tft)                                    
               
        # hp : Twice the distance from the plastic neutral axis 
        # to the nearest line of fasteners at the compression 
        # flange or the inside face of the compression flange
        # when welds are used.
        self.hp = abs(2 * section.Zp)       # @hami2230 - ibeam updated to calculate location of plastic neutral axis
        #    
        # CASE 10 
        if section.build == "rolled":
            self._lambda_p_flange = 0.38 * (material.E / material.Fy)**0.50
            self._lambda_r_flange_b = 1.0 * (material.E / material.Fy)**0.50
            _case_flange = '10'
        # CASE 11 
        else:
            self._lambda_p_flange = 0.38 * (material.E / material.Fy)**0.50
            self._lambda_r_flange_b = 0.95 * (self.kc * material.E / self.FL)**0.50
            _case_flange = '11'
        # CASE 15 
        if sec_symmetry == 'doubly':
            self._lambda_p_web = 3.76 * (material.E / material.Fy)**0.50
            self._lambda_r_web_b = 5.70 * (material.E / material.Fy)**0.50
            _case_web = '15'
        # CASE 16
        else:
            self._lambda_r_web_b = 5.70 * (material.E / material.Fy)**0.50
            _lambda_p = (((hc / self.hp) * (material.E / material.Fy)**0.50)/        
                          (0.54 * (Mpx / self.Mym) - 0.09)**2)                 # @hami2230 - equation updated to include self.hp, self.Mym
            #
            self._lambda_p_web = min(_lambda_p, self._lambda_r_web_b)
            _case_web = '16'
            
        # web
        if _case_web == '15':                                                  # @hami2230 - _ratio_web updated to be case specific
            _ratio_web = hw / section.tw
            _case_web_ratio = 'h/tw'
        elif _case_web == '16':
            _ratio_web = hc / section.tw
            _case_web_ratio = 'hc/tw'
        else:
            raise IOError('error section no implemented')
                
    else:
        raise IOError('error section no implemented')    

    # find web compactness
    _class_web = "noncompact"
    
    if  _ratio_web < self._lambda_p_web :                                      
        _class_web = "compact"
        
    if  _ratio_web > self._lambda_r_web_b :
            _class_web = 'slender'    
    
    #self.lambda_p_fF = self._lambda_p_flange
    #self.lambda_r_fF = self._lambda_r_flange_b
    
    #self.lambda_p_wF = self._lambda_p_web
    #self.lambda_r_wF = self._lambda_r_web_b
    
    # Find flange compactness
    
    _class_flange = "noncompact"
    if  _ratio_flange < self._lambda_p_flange :
        _class_flange = "compact"
        
    if  _ratio_flange > self._lambda_r_flange_b :
        _class_flange = 'slender'
    
    _lambda_p_flange = self._lambda_p_flange 
    _lambda_r_flange_b = self._lambda_r_flange_b
    _lambda_p_web = self._lambda_p_web
    _lambda_r_web_b = self._lambda_r_web_b
                
    return  (_class_flange, _class_web, _ratio_flange, _lambda_p_flange, 
             _lambda_r_flange_b, _ratio_web, _lambda_p_web, _lambda_r_web_b)
#