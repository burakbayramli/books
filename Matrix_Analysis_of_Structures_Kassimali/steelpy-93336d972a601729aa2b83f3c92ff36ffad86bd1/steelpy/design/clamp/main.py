# Copyright (c) 2019-2022 steelpy

# Python stdlib imports
import math
import datetime
#from typing import ClassVar # NamedTuple, List, 

# package imports
from steelpy.utils.units.main import Units
from steelpy.design.clamp.ISO19902 import ISO19902
from steelpy.design.clamp.OTH88283 import OTH88283

#from steelpy.metocean.wave.Stokes5 import Stoke5
#from steelpy.metocean.regular_wave.operations.operations import WaveData
from steelpy.design.clamp.process.shell import ClampShell
from steelpy.design.clamp.process.neoprene import Neoprene
from steelpy.design.clamp.process.bolts import Bolts
from steelpy.design.codes.api.main import API_design
from steelpy.sections.main import Sections
from steelpy.material.main import Materials

#
#-------------------------------------------------
#
#-------------------------------------------------
#
# 
#
#
class ClampDesign(ISO19902, OTH88283):
    """
    """
    #__slots__ = ['_units', 'design_method', '_design_condition', 'clamp_application',
    #             'name', 'clamp_type', 'flange_type','root_search',
    #             'file_out', 'header', 'load_data',
    #             '_shell', '_substrate', '_neoprene', '_stiffener', '_stud_bolt', '_wave',
    #             'Lst', 'top_plate_base', 'Po', 'wave_data', 'Hgap',
    #             'gamma_friction', 'gamma_lift', 'fo',
    #             'FwminWA', 'FwminWB',
    #             'As', 'Fy_factor', 'tau_is', 'tau_P'] '_stud_bolt'
    
    def __init__(self, RootSearch:str = "FAST",
                 design_method:str = "ISO"):
        """
        clamp_name :
        clamp_type : G - Grouted
                     L - Lined
                     M - Mechanical
        flange_type : Clamp's Flage Plate Arrangement
        design_condition : Operating/Extreme
        clamp_application : end-to-end/member-addition/tubular-joint
        theta : Angle between the added member and the substrate member (degrees)
        """
        #
        print("-- module : Clamp Design Version 0.20dev")
        print('{:}'.format(52*'-'))        
        #
        self._units = Units()
        # ----- Ini -----
        self.design_method = design_method
        self._design_condition:str = "OPERATING"
        #
        self.root_search = RootSearch.upper()
        self.header = 1
        #
        # Default output
        self.name:str = 'N/A'
        self.clamp_type:str = 'M'
        self.flange_type:str = "DISCONTINUOUS"
        self.clamp_application:str = "END-TO-END"
        #
        self.file_out = 'ClampDesign.out'
        #self.MemberNumber = 0
        #
        db_file = "beam_f2u.db"
        mesh_type = 'inmemory'
        self._section = Sections(mesh_type=mesh_type,
                                 db_file=db_file)
        #
        self._material = Materials(mesh_type=mesh_type,
                           db_file=db_file)
        #
        self._shell = ClampShell(material=self._material,
                                 section=self._section)
        self._substrate = API_design()
        self._section["substrate"] = "tubular"
        self._substrate.section = self._section['substrate']
        self._material['substrate'] = 'elastic'
        self._substrate.material = self._material['substrate']
        #
        self._neoprene = Neoprene(material=self._material)
        #
        self._add_on = API_design()
        self._section["add_on"] = "tubular"
        self._add_on.section = self._section['add_on']
        self._material['add_on'] = 'elastic'
        self._add_on.material = self._material['add_on']
        self._add_on.theta = 90 * self.units.degrees
        self.add_on_flag = False        
        #
        self._wave = {} # WaveData()
        self._stud_bolt = Bolts()
        #
        # Actual Stiffener Spacing
        self.Lst = 0 * self.units.mm
        #
        # URGEN ---> svo???
        #self.Cf = 0 * self.units.mm
        self.Bf = 0 * self.units.mm
        self.top_plate_base = 0 * self.units.mm
        # External Pressure
        #self.Po = 0  * self.units.Pa
        self.wave_data = False
        #
        # DoE
        #
        self.gamma_friction = 2.25
        self.gamma_lift = 2.0
        #self.mu = 0.10
        #
        # Pressure
        self.Po = 0 *self.units.Pa
        # Initial ovalisation of pipe
        # cross section
        self.fo = 0.025 * self.units.mm
        #
        # Welds
        self.FwminWA = 0 * self.units.mm
        self.FwminWB = 0 * self.units.mm
        #
        self.load_data = True
        #self.file_out = str(self.name) +'_ISO.out'
    #
    @property
    def shell(self):
        """
        """
        return self._shell   
    #
    @property
    def units(self):
        """
        """
        return self._units
    #
    @property
    def substrate(self):
        """
        """
        return self._substrate
    
    @property
    def tubular(self):
        """
        """
        return self.substrate  
    #
    @property
    def add_on_member(self):
        """
        """
        self.add_on_flag = True
        return self._add_on
    #
    @property
    def neoprene(self):
        """
        """
        self.clamp_type = "LINED"
        return self._neoprene
    #   
    #
    @property
    def design_condition(self):
        """
        Operating/Extreme
        """
        return self._design_condition
    @design_condition.setter
    def design_condition(self, value):
        """
        Operating/Extreme
        """
        self._design_condition = value.upper()
    #
    @property
    def hydrostatic_pressure_data(self):
        """
        """
        self.wave_data = True
        return self._wave
    #
    @property
    def bolt(self):
        """
        """
        self._stud_bolt.bolt_data = True
        #try:
        #    return self._stud_bolt.bolts
        #except AttributeError:
        return self._stud_bolt
    #
    def HSE_OTH88283(self):
        """
        """
        # OTH
        # Pretension Factor
        if self.bolt.fp == 0:
            #(ref. TransIMarE(TM), Vol.99, Page 23)
            self.bolt.fp = 0.70
        
        # Calculate Factors
        OTH88283.factors_of_safety(self)
        OTH88283.internal_frictional_force(self)
        OTH88283.tension_force_pull_clamp_shell(self)
        OTH88283.number_bolts_required(self)
        OTH88283.clamp_shell_thickness_required(self)
        
        # Grouted
        if self.clamp_type == "GROUTED":
            raise NotImplemented("Grouted not implemented")
        # Lined
        elif self.clamp_type == "LINED":
            print("Lined")
            OTH88283.neoprene_thickness_check(self)
        # Mechanical
        elif self.clamp_type == "MECHANICAL":
            raise NotImplemented("Mechanical not implemented")
            #OTH88283.CheckNeopreneThickness(self)
        # clamp type?
        else:
            raise IOError("Clamp Type No Recognized")
        # ok
        ISO19902.clamp_properties(self)
        
        if self.flange_type.upper() == "DISCONTINUOUS":
            OTH88283.stiffener_plate_height(self)
            OTH88283.flange_plate_check(self)
            OTH88283.stiffener_plate_design(self)
            OTH88283.weld_design(self)
        else:
            print("continuous")
            ISO19902.clamp_properties(self)
            OTH88283.stiffener_plate_height2(self)
            
            if self.wave_data:
                OTH88283.clamp_top_plate_hydro_check(self)
            
            OTH88283.flange_plate_check(self)
            OTH88283.stiffener_plate_design2(self)
            OTH88283.saddle_plate_check(self)
        #
        OTH88283.hydro_collapse_check(self)
        OTH88283.clamp_split_check(self)
        OTH88283.combined_stress(self)        
    #
    def ISO19902_2007(self):
        """
        """
        #iso_check = ISO19902()
        # Pretension Factor
        #if self.shell.bolt.fp == 0:
        #    # No reference of this factor for iso
        #    self.shell.bolt.fp = 0.80
        #
        # Section Properties
        #ISO19902.section_properties(self)
        if self.load_data:
            # ISO Main Chapters
            # End-to-end connection clamps
            if "END-TO-END" in self.clamp_application.upper():
                ISO19902.end_to_end_connection(self)
            # Addtion of member clamps
            elif "MEMBER-ADDITION" in self.clamp_application.upper():
                ISO19902.addition_of_member(self)
                if self.add_on_flag:
                    print('')
                    print('Check brace')
                    if self.wave_data:
                        self._add_on.wave = self.hydrostatic_pressure_data                    
                    self._add_on.print_results()
            # Clamps on Tubular Joints
            elif "TUBULAR-JOINT" in self.clamp_application.upper():
                raise Warning("No yet implemented")
            else:
                raise IOError("Application not recognized")
            #
            ISO19902.prestressed_clamp(self)
            # Calculate Bolt Safety Factors according to JIRRP (OTH88283) guidance
            OTH88283.factors_of_safety(self)            
            ISO19902.maximum_bolt_tension(self)
            ISO19902.bolt_selection(self)
        else:
            print('80% Proof Load from Bolt : {:}'.format(self.shell.bolt.name))
            # The bolt pretension will conservatively
            # taken as the following:
            self.Fpt = self.bolt.bolts.proof_load_80percent
            print("Fpt : {:1.3} kN".format(self.Fpt.convert('kilonewton').value))
            # Note:
            # Bolt pretension after loses
            self.Fptal = self.Fpt / (1.0 + self.bolt.total_Pmax)
            print("Bolt pretension after loses : {:1.3f}".format(self.Fptal))
        #
        # At design stage, the spacing of the studbolts
        # on the bearing plate requires to be adequate
        # to permit installation of the spherical washers
        # and the tensioning tool used to pretension the
        # bolts.
        Lst_geometry = ((self.shell.Lc - 
                         0.50*(self.shell.Lc / self.shell.Nbp))
                        / self.shell.Nbp)
        
        # Actual Stiffener Spacing 
        print('')
        print('Actual Stiffener Spacing')
        if self.Lst.value < Lst_geometry.value:
            if Lst_geometry.value < self.bolt.bolts.tensioner_A.value :
                self.Lc = self.shell.Nbp  * self.bolt.bolts.tensioner_A
                self.Lst = self.bolt.bolts.tensioner_A
                print ("Lc new : {:1.2f} mm"
                       .format(self.Lc.convert('millimetre').value))
            else:
                self.Lst = Lst_geometry
            print ("Lst : {:1.2f} mm"
                   .format(self.Lst.convert('millimetre').value))
        #
        # Check Neoprene if selected
        if self.clamp_type == "LINED":
            ISO19902.neoprene_liner_checks(self)
        # Check grouted
        elif self.clamp_type == "GROUTED":
            raise Warning('No yet defined')
        
        # tc is here estimated
        try:
            self.shell.tc
            ISO19902.clamp_shell_design(self)
        except AttributeError:
            self.get_shell_thickness()
        #else:
        #    ISO19902.clamp_shell_design(self)
        
        if self.flange_type == "DISCONTINUOUS":
            #ISO19902.StiffenerPlateCheck(self)
            ISO19902.clamp_properties(self)
            OTH88283.stiffener_plate_height(self)
            OTH88283.flange_plate_check(self)
            OTH88283.stiffener_plate_design(self)
            OTH88283.weld_design(self)
        else:
            print("")
            print("continuous")
            ISO19902.clamp_properties(self)
            OTH88283.stiffener_plate_height2(self)
            
            if self.wave_data :
                OTH88283.clamp_top_plate_hydro_check(self)
            
            OTH88283.flange_plate_check(self)
            OTH88283.stiffener_plate_design2(self)
            OTH88283.saddle_plate_check(self)
        #
        OTH88283.hydro_collapse_check(self)
        OTH88283.clamp_split_check(self)
        OTH88283.combined_stress(self)
        print('------------------->')
    #
    def wave_calculation(self):
        """
        """
        #if self.wave_length == 0:
        #    Wave = Stoke5()
        #    Wave.Data(self.Hw.value, self.Twave, self.d.value)
        #    #print ('Wave Length =',Wave.WaveLength)
        #    self.wave_length = Wave.WaveLength * self.units.m
        #    print ('Wave Length = {:1.3f} m'.format(self.wave_length.value))
        #
        #self.k = (2*math.pi)/self.wave_length
        print ('Wave Length = {:1.3f} m'.format(self._wave.wave_length.value))
        # Maximum External Pressure
        self.Hz = (-1*self._wave.z + (self._wave.Hw/2)
                   * ((math.cosh(self._wave.k.value * (self._wave.d.value + self._wave.z.value)))
                      / (math.cosh(self._wave.k.value * self._wave.d.value))))
        #
        print ("Hz  = {:2.3f} m".format(self.Hz.value))
        # (13.2-20)
        self.Po = (self._wave.rho * self.g * self.Hz)
        print("Po  = {:2.3f} MPa".format(self.Po.convert('megapascal').value))        
    #
    def get_shell_thickness(self):
        """
        """
        # Initial conditions
        self.shell.tc = 10.0 * self.units.mm
        self.UCc = 2.0
        
        while self.UCc > 1.0:
            self.shell.tc += 5.0 * self.units.mm
            ISO19902.clamp_shell_design(self)
        #
        print('found tc ==> {:1.3f} mm'
              .format(self.shell.tc.convert('millimetre').value))
        #return _tc
    #
    def print_results(self, name_out:str='clamp_design.out'):
        """
        """
        self.file_out = str(self.name) +'_clamp.out'
        #
        #self.solve()
        #
        #-------------------------------------------------
        #
        # Clamp Arc Length
        # Contact area of one clamp shell
        self.As = math.pi * (self.substrate.section.D / 2.0) * self.shell.Lc
        print("Contact Area of One Clamp: {:1.3f} mm^2"
              .format(self.As.convert('millimetre^2').value))
        #
        self.get_loads()
        # self.T_section = Tee(self.stiffener.material, self.units)
        #
        #
        # Find bolt spacing
        #if self.shell.bolt.Bsp.value == 0:
        #    try:
        #        self.shell.bolt.Bsp = self.shell.Lc / self.shell.Nbp
        #    except ZeroDivisionError: 
        #        pass
        #
        # find e
        #if self.stiffener.e.value == 0:
        #    #self.stiffener.e = self.Bsp / 2.0
        #    self.stiffener.e = self.shell.bolt.bolts.d + 2 * self.units.mm
        #
        #
        #  Flag Tension or Compression   
        #self.Faxial_flag = 'TENSION'
        #if self._actions.Fx.value != 0:
        #if self._actions.Fx.value < 1.0:
        #    self.Faxial_flag = 'COMPRESSION'
        #
        if self.header == 100:
            today = datetime.date.today()
            OutputFile = open(self.file_out,'w')
            #
            print (' ')
            print ("+++++++++++++++++++++++++++++")
            print ('        CLAMP DESIGN')
            print ("Member : ",self.clamp_name)
            print ("Output file: ",self.file_out)
            #print ("+++++++++++++++++++++++++++++")
            #
            OutputFile.write(" "+"\n")
            OutputFile.write("***************************************************************************************"+"\n")
            OutputFile.write("*                                  CODE CHECK TOOL                                    *"+"\n")
            OutputFile.write("*                            Strength Of Tubular Members                              *"+"\n")
            OutputFile.write("*                                     ISO90902                                        *"+"\n")
            OutputFile.write("*                                  ALPHA Version                             23/07/10 *"+"\n")            
            OutputFile.write("***************************************************************************************"+"\n")
            OutputFile.write(("DATE: "+ "%-8s" + 59*" " + "UNITS [N-mm]"+"\n")%(today))
            #
            OutputFile = open(self.file_out,'a+')
        #
        # Po is calculated based on hydro pressure if
        # card was activated by user.
        if self.wave_data:
            self.wave_calculation()
        #
        if "OTH" in self.design_method:
            self.HSE_OTH88283()
        #ISO
        elif "ISO" in self.design_method:
            self.ISO19902_2007()
        # Design Method?
        else:
            raise Warning("Design Method No Recognized")
    #
    #
    def solve(self):
        """
        """
        try:
            self.shell.Dci
        except AttributeError:
            self.shell.Dci = self._substrate.section.D
        #
        # Stressed length of the studbolts
        # which can be taken as distance between nuts
        #
        #       
        #
        #
        print("----------------------------------------")
        print("Loads")
        print("----------------------------------------")
        self.get_loads()
        print("----------------------------------------")
        print("A) Calculate required clamp bolt tension")
        print("----------------------------------------")
        # A) Calculate required clamp bolt tension
        self.required_clamp_bolt_tension()
        # B) Check bolt tension
        print("----------------------------------------")
        print("B) Check bolt tension")
        print("----------------------------------------")
        self.bolt_design()
        # check line/grout performance
        if self.clamp_type == "LINED":
            self.neoprene_liner_check()
        print("----------------------------------------")
        print("E) Clamp Shell, Plates and Weld Design")
        print("----------------------------------------")        
        self.get_stiffener_spacing()
        self.desing_clamp_shell()
        print("----------------------------------------")
        print("F) check Clamped Member")
        print("----------------------------------------")          
        self.check_clamped_member()
        print("end")
    #
    #
    def get_loads(self):
        """
        """
        # TODO: Loading may not be correct
        #
        _theta = self._add_on.theta.value
        _add_on_member = self._add_on.actions
        self.substrate.actions.Fx += (_add_on_member.Fx * math.cos(_theta)
                                      + _add_on_member.Fz * math.sin(_theta))
        self.substrate.actions.Fz += (_add_on_member.Fx * math.sin(_theta)
                                      + _add_on_member.Fz * math.cos(_theta))
        # TODO: Fy fixed but should have an inclination angle
        self.substrate.actions.Fy += _add_on_member.Fy
        #
        self.substrate.actions.Mx += (_add_on_member.Mx * math.cos(_theta)
                                      + _add_on_member.Mz * math.sin(_theta))
        self.substrate.actions.Mz += (_add_on_member.Mx * math.sin(_theta)
                                      + _add_on_member.Mz * math.cos(_theta))
        #
        self.substrate.actions.My += _add_on_member.My
        #
        print("Load Summary")
        print("Fx = {:8.2f} kN".format(self.substrate.actions.Fx.convert("kilonewton").value))
        print("Fy = {:8.2f} kN".format(self.substrate.actions.Fy.convert("kilonewton").value))
        print("Fz = {:8.2f} kN".format(self.substrate.actions.Fz.convert("kilonewton").value))
        print("Mx = {:8.2f} kNm".format(self.substrate.actions.Mx.convert("kilonewton * metre").value))
        print("My = {:8.2f} kNm".format(self.substrate.actions.My.convert("kilonewton * metre").value))
        print("Mz = {:8.2f} kNm".format(self.substrate.actions.Mz.convert("kilonewton * metre").value))
    #
    def _bolt_check(self):
        """
        """
        ISO19902.prestressed_clamp(self)
        # Calculate Bolt Safety Factors according to JIRRP (OTH88283) guidance
        OTH88283.factors_of_safety(self)            
        ISO19902.maximum_bolt_tension(self)
        ISO19902.bolt_selection(self)
    #
    def bolt_design(self):
        """
        """
        # FIXME
        try:
            self.bolt.Lsb
        except AttributeError:
            # FIXME: Take diametre of substrate (no conservative)
            self.bolt.Lsb = self.substrate.section.D
        #
        #
        try:
            try:
                self._bolt_check()
            except Warning as note:
                print(note)
                raise RuntimeError("runtime error")
        except AttributeError:
            # probaly bolt data missing
            #bolt = Clamp1.bolt
            bolt_data = ['M20', 'M24', 'M30', 'M36', 'M42']
            for _bolt_name in bolt_data:
                self.bolt.name = _bolt_name
                try:
                    self._bolt_check()
                    print('Bolt found', _bolt_name)
                    break
                except Warning:
                    print('Bolt {:} fail'.format(_bolt_name))
                    print("")
                    continue
        #
        # FIXME : all below
        # Find bolt spacing
        self.shell.stiffener.Bsp = self.shell.Lc / self.shell.Nbp
        #
        # find e
        if self.shell.stiffener.e.value == 0:
            self.shell.stiffener.e = self.bolt.bolts.d + 2 * self.units.mm
        #
        # The bolt pretension will conservatively
        # taken as the following:        
        #self.Fpt = self.bolt.bolts.proof_load_80percent
        #print("-------------------------------------")
        #print("Fpt : {:1.3} kN".format(self.Fpt.convert('kilonewton').value))
        # Bolt pretension after loses
        #self.Fptal = self.Fpt / (1.0 + self.bolt.total_Pmax)
        #print("Bolt pretension after loses : {:1.3f} kN"
        #      .format(self.Fptal.convert('kilonewton').value))
        # Bolt eccentricity default
        try:
            self.stiffener.eBolt
        except AttributeError:
            self.shell.stiffener.eBolt = self.bolt.bolts.spherical_washer_B
    #      
    #
    def shear_force(self):
        """
        Calculate shear force to prevent slip
        """
        try:
            self.Ls
        except AttributeError:
            self.Ls = self.shell.Lc # * 0.90
        #
        if "OTH" in self.design_method:
            pass
        else:
            ISO19902.get_shear_force(self)
    
    #
    def transfer_stress(self):
        """
        """
        self.get_prying_moment(self)
    #
    def get_prying_moment(self):
        """
        """
        if "OTH" in self.design_method:
            pass
        else:
            pass
    #
    def contact_force(self):
        """
        Calculation of contact force to prevent lift off
        """
        # Find bolt spacing
        try:
            self.shell.stiffener.Bsp
        except AttributeError: 
            self.shell.stiffener.Bsp = self.shell.Lc / self.shell.Nbp 
        #
        if "OTH" in self.design_method:
            pass
        else:
            ISO19902.get_contact_force(self)
    #
    def required_clamp_bolt_tension(self):
        """
        Calculation of the required clamp bolt tension
        """
        self.contact_force()
        self.shear_force()
    #
    #
    def neoprene_liner_check(self):
        """
        """
        print("----------------------------------------")
        print("D) Check on Neprene Liner")
        print("----------------------------------------")
        if "OTH" in self.design_method:
            OTH88283.neoprene_thickness_check(self)
        else:        
            ISO19902.neoprene_liner_checks(self)
    #
    def get_stiffener_spacing(self):
        """
        At design stage, the spacing of the studbolts
        on the bearing plate requires to be adequate
        to permit installation of the spherical washers
        and the tensioning tool used to pretension the
        bolts.
        """
        Lst_geometry = ((self.shell.Lc - 
                         0.50*(self.shell.Lc / self.shell.Nbp))
                        / self.shell.Nbp)
        
        # Actual Stiffener Spacing 
        print('Actual Stiffener Spacing')
        if self.Lst.value < Lst_geometry.value:
            if Lst_geometry.value < self.bolt.bolts.tensioner_A.value :
                self.Lc = self.shell.Nbp  * self.bolt.bolts.tensioner_A
                self.Lst = self.bolt.bolts.tensioner_A
                print ("Lc new : {:1.2f} mm"
                       .format(self.Lc.convert('millimetre').value))
            else:
                self.Lst = Lst_geometry
            print ("Lst : {:1.2f} mm"
                   .format(self.Lst.convert('millimetre').value))
    #
    def desing_clamp_shell(self):
        """
        """
        # tc is here estimated
        try:
            self.shell.tc
            ISO19902.clamp_shell_design(self)
        except AttributeError:
            self.get_shell_thickness()
        #else:
        #    ISO19902.clamp_shell_design(self)
        #       
        #
        if self.flange_type == "DISCONTINUOUS":
            #ISO19902.StiffenerPlateCheck(self)
            ISO19902.clamp_properties(self)
            OTH88283.stiffener_plate_height(self)
            OTH88283.flange_plate_check(self)
            OTH88283.stiffener_plate_design(self)
            OTH88283.weld_design(self)
        else:
            print("")
            print("continuous")
            ISO19902.clamp_properties(self)
            #try:
            OTH88283.stiffener_plate_height2(self)
            #except AttributeError:
            #    print('-->')
            if self.wave_data :
                OTH88283.clamp_top_plate_hydro_check(self)
            #
            OTH88283.flange_plate_check(self)
            OTH88283.stiffener_plate_design2(self)
            OTH88283.saddle_plate_check(self)
    #
    def check_clamped_member(self):
        """
        """
        OTH88283.hydro_collapse_check(self)
        OTH88283.clamp_split_check(self)
        OTH88283.combined_stress(self)
    #
    #