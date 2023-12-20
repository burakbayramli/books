from steelpy import f2uModel
from steelpy import Units
from steelpy import Trave3D
#
#
# call units module
units = Units()
#
# -----------------------------------
# Start conceptual modelling
# -----------------------------------
f2u_model = f2uModel(component="caisson_1") # set name component
#
concept = f2u_model.concept() # call conceptual model
#
#
# -----------------------------------
# define material
material = concept.materials()
#
# material[number] = [elastic, Fy, Fu, E, G, Poisson, density, alpha]
material["MAT45"] = ['elastic', 345.0 * units.MPa]
#
print(material["MAT45"].Fy)
#
# -----------------------------------
# Define sections
section = concept.sections()
#
# section[section_name] =  ['Tubular', Diametre, Wall_thickness]
section["TUB500"] = ['Tubular', 500 * units.mm, 25 * units.mm]
section["TUB400"] = ['Tubular', 400 * units.mm, 15 * units.mm]
#
# -----------------------------------
#
#
# -----------------------------------
# define points
point = concept.points()
point[3] = [4*units.m, 3*units.m]
point[4] = {"x":4*units.m, "y":0*units.m}
#print(point[4])
#
# -----------------------------------
elements = concept.elements()
#
# Start beam modelling
beam = elements.beams()
# set material & section default
material.default = "MAT45"
section.default = "TUB500"
#
# define beam via coordinades with list = [x, y, z=0]start, [x, y, z=0]end
beam["bm1"] = [0*units.m, 0*units.m], [0*units.m, 6*units.m]
#
# define beam via coordinades with dict and concept Point = {x, y, z=0}start, point_end
beam["bm2"] = {"x":0*units.m, "y":6*units.m}, point[3]
# segmented beam
#beam["bm2"].step[1] = [1*units.m, 6*units.m]
beam["bm2"].step[1].length = 1.0 * units.m
beam["bm2"].step[1].section = section["TUB400"]
#
beam["bm2"].step[2] = [3*units.m, 6*units.m]
#beam["bm2"].step[2].length = 3.0 * units.m
#
# define beam via concept Points = point_start, point_end
beam["bm3"] = point[3], point[4]
#
# -----------------------------------
# Define boundary conditions
boundary = concept.boundaries()
supports = boundary.supports()
#
supports['fixed'] = 'fixed'
# define boundary via coordinades with list = [x, y, z=0]
supports['fixed'].points = [0*units.m, 0*units.m]
# define beam via concept Points = point
supports['fixed'].points = point[4]
#
#
# -----------------------------------
# Start concept loading
#
load = concept.load()
# define basic load
basic = load.basic()
# create new basic load
basic[1] = 'wind load'
basic[1].point = [0*units.m, 6*units.m]
basic[1].point.load = {'fy': -2 * units.MN, 'name': "wind_1"} # nodal load in plane
basic[1].point.load = {'mz': -3 * units.MN*units.m, 'name': "wind_2"} # bending moment in plane
#
# create new basic load
basic[1].point = point[3]
basic[1].point.load = {'fz': -4 * units.MN, 'name': "wind_3"} # nodal load out plane
#
# create new basic load
basic[2] = 'snow basic'
basic[2].beam = beam["bm2"]
#basic[2].beam.coordinate_system = 'local'
basic[2].beam.local_system() # set load coord system local
basic[2].beam.line = {'qy': -1 * units.kN / units.m, 'name': "snow_1"} # in plane udl from node to node

basic[2].beam.line = {'qy1': 0 * units.kN / units.m, # in plane triangular load
                      'qy2': -2 * units.kN / units.m, # from node to node
                      'name': "snow_2"}
# trapezoidal out plane load
#basic[2].beam.coordinate_system = 'global'
basic[2].beam.global_system() # reset load coord system global
basic[2].beam.line = {'qz1': 2 * units.kN / units.m, # start load value
                      'qz2': 4 * units.kN / units.m, # end load value
                      'd1': 0.5 * units.m, # load start 0.5m from node 1
                      'd2': 1.0 * units.m, # load end 1m from node 2
                      'name': "snow_3"}
#
# create new basic load
basic[3] = 'crane load'
basic[3].beam = beam["bm2"]
basic[3].beam.point = {'fx':-100 * units.kN,  # beam point axial load
                       'd1': 2.55 * units.m, # 2.5m from node 1
                       'name': "crane_1"}
#
# create new basic load
#basic[4] = 'dead load'
#basic[4].selfweight.y = -1
#
#
# ----------------------------------------------------
# Plotting
# ----------------------------------------------------
#
# Structure
#
plot = concept.plot()
plot.frame()
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
print("Materials")
print(material)
#
print("Sections")
print(section)
#
nodes = mesh.nodes()
#print("Nodes")
print(nodes)
#for key, node in nodes.items():
#    print(node)
#
print("")
elements = mesh.elements()
print("Elements")
print(elements)
#for key, element in elements.items():
#    print(element)
#
load = mesh.load()
print("Load")
print(load.basic())
#
#
# ----------------------------------------------------
# Structural Analysis
# ----------------------------------------------------
#
frame = Trave3D()
frame.mesh = mesh
frame.static()
results = frame.solve()
print(results)
print('-->')
