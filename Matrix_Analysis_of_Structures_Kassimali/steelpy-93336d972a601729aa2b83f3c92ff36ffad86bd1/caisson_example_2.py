from steelpy import f2uModel
from steelpy import Units
from steelpy import Trave3D
#
units = Units()
#
# -----------------------------------
# Start conceptual modelling
# -----------------------------------
f2u_model = f2uModel(component="EA_PDQ_Caisson_C1")
#
#
concept = f2u_model.concept()
#
# -----------------------------------
# define material
material = concept.materials()
material["MAT345"] = ['elastic', 345.0 * units.MPa]
#
# -----------------------------------
# Define sections
section = concept.sections()
# Caisson
section["T1350x40"] = ['Tubular', 1350 * units.mm, 40 * units.mm]
section["T1350x25"] = ['Tubular', 1350 * units.mm, 25 * units.mm]
section["T1350x15"] = ['Tubular', 1350 * units.mm, 15 * units.mm]
# stub member
section["T559x16"] = ['Tubular', 559 * units.mm, 15.9 * units.mm]
section["T610x16"] = ['Tubular', 610 * units.mm, 15.9 * units.mm]
# jacket
section["T800x30"] = ['Tubular', 800 * units.mm, 30 * units.mm]
section["T900x35"] = ['Tubular', 900 * units.mm, 35 * units.mm]
section["T100x25"] = ['Tubular', 100 * units.mm, 25 * units.mm]
#
# -----------------------------------
#
#
# define Elevations
elevation = concept.points()
elevation[1] = [0*units.m, 14.2*units.m, 0*units.m]
elevation[2] = [0*units.m, 8.0*units.m, 0*units.m]
elevation[3] = [0*units.m, -14.0*units.m, 0*units.m]
elevation[4] = [0*units.m, -39.0*units.m, 0*units.m]
elevation[5] = [0*units.m, -64.0*units.m, 0*units.m]
elevation[6] = [0*units.m, -74.0*units.m, 0*units.m]
#
point = concept.points()
point[22] = [0*units.m, 8.0*units.m, -1.5*units.m]
point[33] = [0*units.m, -14.0*units.m, -1.5*units.m]
point[44] = [2.7*units.m, -39.0*units.m, 0*units.m]
point[55] = [2.7*units.m, -64.0*units.m, 0*units.m]
#
# -----------------------------------
#
elements = concept.elements()
#
# Start beam modelling
beam = elements.beams()
# set material & section default
material.default = "MAT345"
#
# Define Caisson from bottom to top
#
section.default = "T1350x15"
beam["bm12"] = elevation[6], elevation[5]
beam["bm12"].step[1].length = 7.0 * units.m
beam["bm12"].step[1].section = section["T1350x25"]
#
section.default = "T1350x25"
beam["bm9"] = elevation[5], elevation[4]
beam["bm9"].step[1].length = 1.5 * units.m
beam["bm9"].step[1].section = section["T1350x15"]
beam["bm9"].step[2].length = 23.5 * units.m
beam["bm9"].step[2].section = section["T1350x25"]
#
beam["bm6"] = elevation[4], elevation[3]
beam["bm6"].step[1].length = 1.5 * units.m
beam["bm6"].step[1].section = section["T1350x15"]
beam["bm6"].step[2].length = 23.5 * units.m
beam["bm6"].step[2].section = section["T1350x25"]
#
beam["bm3"] = elevation[3], elevation[2]
beam["bm3"].step[1].length = 1.5 * units.m
beam["bm3"].step[1].section = section["T1350x15"]
beam["bm3"].step[2].length = 9.0 * units.m
beam["bm3"].step[2].section = section["T1350x25"]
beam["bm3"].step[3].length = 20.5 * units.m
beam["bm3"].step[3].section = section["T1350x40"]
#
section.default = "T1350x40"
beam["bm27"] = elevation[2], elevation[1]
beam["bm27"].step[1].length = 1.5 * units.m
beam["bm27"].step[1].section = section["T1350x25"]
#
# Stub members
section.default = "T610x16"
beam["bm14"] = elevation[2], point[22]
section.default = "T559x16"
beam["bm15"] = elevation[3], point[33]
beam["bm16"] = elevation[4], point[44]
beam["bm17"] = elevation[5], point[55]
#
# -----------------------------------
# Define boundary conditions
#
#concept.boundaries([[point, node1, type, boundary/parameters],
#                     line, node1, node2, type, boundary/parameters])
boundary = concept.boundaries()
supports = boundary.supports()
#
supports['fixed'] = 'fixed'
supports['fixed'].points = elevation[1]
supports['fixed'].points = point[22]
supports['fixed'].points = point[33]
supports['fixed'].points = point[44]
supports['fixed'].points = point[55]
#
#
#
# -----------------------------------
# Start concept loading
#
load = concept.load()
# define basic load
basic = load.basic()
#
# create new basic load
basic[1] = 'wave_loading'
#
#basic[1].point = elevation[2]
#basic[1].point.load["point_22"] = {'fz': 5000 * units.kN}
#
basic[1].beam = beam["bm12"]
#basic[1].beam.local_system
basic[1].beam.line = {'qx': 90 * units.kN / units.m, "name":"wave_1"}
#
basic[1].beam = beam["bm9"]
basic[1].beam.line = {'qx': 90 * units.kN / units.m, "name":"wave_2"}
#
basic[1].beam = beam["bm6"]
basic[1].beam.line = {'qx': 90 * units.kN / units.m, "name":"wave_3"}
#
basic[1].beam = beam["bm3"]
basic[1].beam.line = {'qx': 90 * units.kN / units.m, "name":"wave_4"}
#
basic[1].beam = beam["bm27"]
basic[1].beam.line = {'qx': 90 * units.kN / units.m, "name":"wave_5"}
#
#
basic[1].beam = beam["bm14"]
basic[1].beam.line = {'qx': 90 * units.kN / units.m, "name":"wave_6"}
#
basic[1].beam = beam["bm15"]
basic[1].beam.line = {'qx': 90 * units.kN / units.m, "name":"wave_7"}
#
#
#basic[1].beam.line_load["snow_2"] = {'qy1': 0 * units.kN / units.m, # in plane triangular load
#                                     'qy2':-2 * units.kN / units.m} # from node to node
## trapezoidal out plane load
#basic[1].beam.global_system # reset load coord system global
#basic[1].beam.line_load["snow_3"] = {'qz1': 2 * units.kN / units.m, # start load value
#                                     'qz2': 4 * units.kN / units.m, # end load value
#                                     'd1': 0.5 * units.m, # load start 0.5m from node 1
#                                     'd2': 1.0 * units.m} # load end 1m from node 2
#
#basic[2] = 'point_loading'
##
#basic[2].point = elevation[1]
#basic[2].point.load["point_1"] = {'fz': 1000 * units.kN} # nodal load in plane
##
#basic[2].point = elevation[2]
#basic[2].point.load["point_2"] = {'fz': 1000 * units.kN}
##
#basic[2].point = elevation[3]
#basic[2].point.load["point_3"] = {'fz': 1000 * units.kN}
##
#basic[2].point = elevation[4]
#basic[2].point.load["point_4"] = {'fz': 1000 * units.kN}
##
#basic[2].point = elevation[5]
#basic[2].point.load["point_5"] = {'fz': 1000 * units.kN}
##
##basic[2].point = elevation[6]
##basic[2].point.load["point_6"] = {'fz': 1000 * units.kN}
##
#basic[3] = 'crane load'
#basic[3].beam = beam["bm3"]
#basic[3].beam.point_load["crane_1"] = {'fx':-100 * units.kN,  # beam point axial load
#                                       'd1': 3 * units.m}   # 2.5m from node 1
#
# ----------------------------------------------------
# Plotting
# ----------------------------------------------------
#
# Structure
#
#plot = concept.plot()
#plot.frame()
#plot.material()
#plot.section()
#
# Loading
#
#plotload = load.plot()
#plotload.basic()
#
#
# ----------------------------------------------------
# Meshing
# ----------------------------------------------------
#
#
mesh = concept.mesh()
#
#
#
#
print("Materials")
print(material)
#
print("Sections")
print(section)
#
nodes = mesh.nodes()
print(nodes)
#
bds = mesh.boundaries()
print("boundaries")
print(bds)
#
print("")
elements = mesh.elements()
print(elements)
#
loadm = mesh.load()
print("Load")
print(loadm.basic())
#
#
# ----------------------------------------------------
# Structural Analysis
# ----------------------------------------------------
#
#
frame = Trave3D()
frame.mesh = mesh
frame.static()
results = frame.solve()
print(results)
print('-->')
