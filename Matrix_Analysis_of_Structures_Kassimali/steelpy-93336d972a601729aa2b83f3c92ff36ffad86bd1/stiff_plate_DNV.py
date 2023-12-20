#
from steelpy import Units
from steelpy import CodeCheck
from steelpy import Materials
from steelpy import Sections
#
#
units = Units()
code = CodeCheck()
#
pannel = code.DNV_pannel()
#
pannel.plate_geometry(thk=8*units.mm, 
                      LP=4.6*units.m, 
                      LG=2.3*units.m)

pannel.plate_material(Fyp = 420 *units.MPa, 
                      E = 210000 *units.MPa, Nu = 0.3)
#
# -----------------------------------
# define material
#
material = Materials()
fyp = 420 * units.MPa
material["plate"] = ['elastic', fyp]
#
fyS = 420 * units.MPa
material["stiffener"] = ['elastic', fyS]
material["girder"]    = ['elastic', fyS]
#
# -----------------------------------
# Define sections
section = Sections()
#
d = 150.0 * units.mm
tw = 10.0 * units.mm
b = 75.0 * units.mm
section["stiffener"] = ['angle', d, tw, b]
section["girder"]    = ['angle', d, tw, b]
#
# -----------------------------------
# stiffener
#
pannel.stiffener.section = section["stiffener"]
pannel.stiffener.material = material["stiffener"]
pannel.stiffener.L = 5*units.m
pannel.stiffener.s = 700 * units.mm
#
#
# -----------------------------------
# girder
#
pannel.girder.section = section["girder"]
pannel.girder.material = material["girder"]
pannel.girder.L = 4.6*units.m
pannel.girder.Lt = 2.3 * units.m
pannel.girder.Lk = 2.3 * units.m
#
#
#
# -----------------------------------
#
# Design Loads (ULS)
#
# Pressure Loads
# shear 
pannel.Tau_Sd(30*units.MPa)
# Longitudinal pressure
pannel.Sigmax_Sd(4*units.MPa)
pannel.Sigmay_Sd(4*units.MPa, 2*units.MPa)
# Lateral Pressure 
pannel.P_Sd(4*units.KPa)
#
# Defined Loads
#
#
# -----------------------------------
# results
#
pannel.print_results()
#
print('end')