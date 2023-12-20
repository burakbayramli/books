# 
# Copyright (c) 2019-2021 steelpy
#
# Python stdlib imports
#import math
#import datetime
from typing import Union, ClassVar

# package imports
from steelpy.design.codes.api.wsd_22ed import APIwsd22ed
from steelpy.design.beam.process import BeamDesignParameters, CodeResults
#from steelpy.f2uModel.load.operations.actions import Actions
#from steelpy.f2uModel.sections.process.stress import BeamStress
import steelpy.design.codes.process.print_report as print_report

#
#
class API_design(BeamDesignParameters):
    """
    """
    def __init__(self, cls):
        """
        """
        self._cls = cls
        BeamDesignParameters.__init__(self)
        #self._actions:ClassVar = Actions()
        #
        # Default output
        #self.member_name:Union[str:int] = 'N/A'
        #self.file_out = 'APIwsdCodeCheck.out'
        #self.member_number:int = 0
        #
        #      Factors
        #self.flag_axial = 'COMPRESSION'
        self.design_condition:str = 'OPERATING'
        self.SFxt:float =  1.67
        self.SFb:float = 1.333
        self.SFxc:float = 2.00
        self.SFh:float = 2.00
        #
        self.hydro_check:bool = False
        #
        # internal stress 
        #self.stress = BeamStress(0, 0, 0, 0, 0, 0)
    #
    @property
    def wave(self):
        """
        """
        self.hydro_check = True
        return self._wave
    
    @wave.setter
    def wave(self, value):
        """
        """
        self.hydro_check = True
        self._wave = value
    #
    #
    def reduction_factor(self):
        """
        6.3.2.5
        """
        pass
    #
    def safety_factors(self, SFxt, SFb, SFxc, SFh):
        """
        """
        self.SFxt = SFxt
        self.SFb = SFb
        self.SFxc = SFxc
        self.SFh = SFh
        self.design_condition = 'USER'
    #
    def WSD(self, section, material, stress):
        """
        """
        #
        APIwsd = APIwsd22ed()
        #print ("+++++++++++++++++++++++++++++")
        #print ("              Calculation: ", self.Header)
        #
        APIwsd.axial_tension(section, material, stress)
        APIwsd.axial_compression(Klr=max(self.Klrz, self.Klry), 
                                          section=section, 
                                          material=material,
                                          stress=stress)
        APIwsd.bending(section, material, stress)
        APIwsd.shear(section, material, stress)
        APIwsd.torsional_shear(section, material, stress)
        #
        # Hydro Check
        if self.hydro_check :
            #print ('Wave Length = {:1.3f} m'.format(self._wave.wave_length.value))
            # Calc Hydro Pressure
            APIwsd.safety_factors(self.design_condition, material,
                                  self.SFxt, self.SFb, self.SFxc, self.SFh)            
            APIwsd.hydrostatic_pressure(self.z, self.wave)
            APIwsd.hoop_buckling(self.Lr, section, material)
            APIwsd.combination_hydro(material)
            # Print out Results
            #APIwsd.PrintSectionProperties(self)
            #APIwsd.PrintShear(self)
            #APIwsd.PrintHydroPressureCalc(self)
            #APIwsd.PrintAxialBendingAndHP(self) 
            #
            #self.Header += 1
            comb = APIwsd._hydro_pressure()
            comb += APIwsd._axial_bending_andHP()
        else :
            APIwsd.combination(self.Klrz, self.Klry, 
                               self.Cm[0], self.Cm[1],
                               material, stress)
            #
            ##APIwsd22ed.PrintSectionProperties(self)
            ##APIwsd22ed.PrintShear(self)
            comb = APIwsd._axial_bending_noHP(stress)
        #
        #
        ##self.Header += 1
        #code_detail = ["API RP2A-WSD-Ed22 [Nov 2014]", "Strength Of Tubular Members"]
        #output = print_report.print_header(code_detail)
        output = APIwsd._header()
        output += self._properties()
        ## Calc Section Properties
        output += APIwsd._shear(stress)
        #output.extend(comb)
        print(output)
        #
        return CodeResults(axial= APIwsd.axial_results,
                            shear= APIwsd.shear_results,
                            bending= APIwsd.bending_results,
                            combined= APIwsd.combined_results,
                            report=output)
    #
    def LRFD(self):
        """
        """
        print("to be included")
    #
    def __call__(self, code="wsd"):
        """" """
        # get section properties
        prop = self._cls.section.properties
        # select code
        design = self.WSD
        if code != "wsd":
            design = self.LRFD
        # define beam effective lenght
        self.Klrz = self.K[ 1 ] * self._cls.L / prop.rz
        self.Klry = self.K[ 0 ] * self._cls.L / prop.ry
        #
        print("Klry : {:1.3f}".format(self.Klry))
        print("Klrz : {:1.3f}".format(self.Klrz))
        #
        try:
            self.Lr
        except AttributeError:
            self.Lr = max(self.K[0] * self._cls.L, self.K[1] * self._cls.L)
        #
        res = [ ]
        for stress in self._cls._stress:
            res.append(design(section=self._cls.section,
                              material=self._cls.material,
                              stress=stress))
        return res
    #
    def __str__(self, code="wsd"):
        """" """
        
        print(self._cls.section)
        print(self._cls.material)
        res = self.__call__(code)
        #
        #for line in self.results.report:
        #    print(line.rstrip())
        #
        #output.extend(self.section.print_properties())
        #output.extend(self.material.print_properties())        
        print('-->')
    #
    #