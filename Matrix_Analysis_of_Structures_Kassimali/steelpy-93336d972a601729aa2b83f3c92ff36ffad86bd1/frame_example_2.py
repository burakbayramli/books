# Import steelpy modules
from steelpy import Units
from steelpy import f2uModel
from steelpy import Trave2D, Trave3D
#
#
units = Units()
#
f2umodel = f2uModel(component="Frame2")
# ----------------------------------------------------
# ----------------------------------------------------
# Mesh 
# ----------------------------------------------------
# ----------------------------------------------------
#
mesh = f2umodel.mesh()
#
# ----------------------------------------------------
# Material input
# ----------------------------------------------------
#
# Materials([[name, elastic, Fy, Fu, E, G, Poisson, density, alpha, title(optional)]])
#
# material = f2umodel.materials([[1, 'elastic', 345.0 * units.MPa],
#                                [3, 'elastic', 345.0 * units.MPa, 490.0 * units.MPa,
#                                205000.0 * units.MPa, 77200.0 * units.MPa]])
#
material = mesh.materials()
#
# material.elastic([mat_name, Fy, Fu, E, G, Poisson, density, alpha])
# material.elastic([[1, 345.0 * units.MPa],
#                  [3, 345.0 * units.MPa, 490.0 * units.MPa, 205000.0 * units.MPa, 77200.0 * units.MPa]])
#
#
#material[number] = [elastic, Fy, Fu, E, G, Poisson, density, alpha]
#
#material[1] = ['elastic', 345.0 * units.MPa]
#material[3] = ['elastic', 345.0 * units.MPa, 490.0 * units.MPa,
#               205000.0 * units.MPa, 77200.0 * units.MPa]
#
# FIXME
#matlinear = material.elastic([[1, 345.0 * units.MPa],
#                              [3, 345.0 * units.MPa, 490.0 * units.MPa, 205000.0 * units.MPa, 77200.0 * units.MPa]])
#
matlinear = material.linear()
matlinear[1] = [345.0 * units.MPa]
matlinear[3] = [345.0 * units.MPa, 490.0 * units.MPa, 205000.0 * units.MPa, 77200.0 * units.MPa]
#
print(material)
#
print(matlinear.df)
#
#
# ----------------------------------------------------
# Section Input
# ----------------------------------------------------
#
# sections([[sect_number, 'Rectangle', Height, Width],
#           [sect_number, 'Circular', Diametre],
#           [sect_number, 'Tubular', Diametre, Wall_thickness],
#           [sect_number, 'UB', Height, Web_Thickness, Top_Flange_Width, Top_Flange_Thickness, Bottom_Flange_Width, Bottom_Flange_Thickness, Root_Radious],
#           [sect_number, 'BOX', Height, Web_Thickness, Flange_Width, Flange_Thickness],
#           [sect_number, 'CHANNEL', Height, Web_Thickness, Flange_Width, Flange_Thickness],
#           [sect_number, 'TEE', Height, Web_Thickness, Flange_Width, Flange_Thickness]])
#           
#
#
#section = f2umodel.sections([[1, 'Rectangle', 0.40, 0.35],
#                             [5, 'Circular', 500 * units.mm],
#                             [2, 'Tubular', 500 * units.mm, 25 * units.mm],
#                             [20, 'ub', 700 * units.mm, 12 * units.mm, 264 * units.mm, 15.5 * units.mm],
#                             [30, 'box', 500 * units.mm, 6 * units.mm, 250 * units.mm, 12 * units.mm],
#                             [40, 'channel', 250 * units.mm, 6 * units.mm, 150 * units.mm, 12 * units.mm],
#                             [50, 'tee', 150 * units.mm, 6 * units.mm, 150 * units.mm, 6 * units.mm]])
#
#
section = mesh.sections()
#
# section[number] = [section_type, *parameters]
#
#section[1] = ['Rectangle', 0.40, 0.35]
section[100] = ['ub', 303.4*units.mm, 6*units.mm, 165*units.mm, 10.20*units.mm]
#
section[2] = ['Tubular', 500 * units.mm, 25 * units.mm]
#
section[5] = ['Circular', 500 * units.mm]
section[10] = ['trapeziod', 400 * units.mm, 350 * units.mm, 200 * units.mm] #
section[20] = ['ub', 700 * units.mm, 12 * units.mm, 264 * units.mm, 15.5 * units.mm]
section[30] = ['box', 500 * units.mm, 6 * units.mm, 250 * units.mm, 12 * units.mm]
section[40] = ['channel', 250 * units.mm, 6 * units.mm, 150 * units.mm, 12 * units.mm]
section[50] = ['tee', 150 * units.mm, 6 * units.mm, 150 * units.mm, 6 * units.mm]
section[60] = ['Angle', 150 * units.mm, 6 * units.mm, 150 * units.mm, 6 * units.mm]
#
#
print(section)
#
#print(section.df)
#
#
#
# ----------------------------------------------------
# Node input
# ----------------------------------------------------
#
# nodes corrdinates [x, y, z]
#mesh.nodes([[1, 0, 0, 0],
#            [2, 3, 4, 0],
#            [3, 8, 4, 0]])
#
nodes = mesh.nodes()
#nodes[1] = [0 , 0 , 0 ]
#nodes[2] = [0 , 6 , 0 ]
##nodes[2] = [0 , 3 , 0 ]
#nodes[3] = [4 , 3 , 0 ]
#nodes[4] = [4 , 0 , 0 ]
#
nodes[10] = [0 * units.m, 0 * units.m, 0 * units.m]
nodes[20] = [0 * units.m, 5 * units.m, 0 * units.m]
nodes[30] = [10* units.m, 5 * units.m, 0 * units.m]
nodes[40] = [10* units.m, 0 * units.m, 0 * units.m]
#
print(nodes)
#
#
# ----------------------------------------------------
# boundary Input
# ----------------------------------------------------
#
# mesh.boundaries([[node_number, boundary_type [constrain/support], parameters]])
#
# mesh.boundaries([[1, 'constrain', 'fixed'],
#                  [4, 'constrain', [1,1,1,1,1,1]]])
#
boundary = mesh.boundaries()
#
#boundary.supports([[1, 'fixed'],
#                   [4, [1,1,1,1,1,1]]])
#
supports = boundary.supports()
# supports[node_number] =  boundary/parameters
#supports[1] = 'fixed'
#supports[4] = [1,1,1,1,1,1]
supports[10] = [1,1,1,1,1,1]
supports[40] = [1,1,1,1,1,1]
#
print(boundary)
#
# ----------------------------------------------------
# Element input
# ----------------------------------------------------
#
# Exmaple:
# mesh.elements([[element_number, beam,  material, section, node1, node2, roll_angle]
#                [element_number, plate, material, section, node1, node2, node3, node4]])
#
# mesh.elements([[1, 'beam', 1, 2, 1, 1, 0],
#                [2, 'beam', 2, 3, 1, 1, 0],
#                [3, 'beam', 3, 4, 1, 1, 0]])
#
#       
#
elements = mesh.elements()
# Example:
# Elements[number] = [beam, material, section, node1, node2, roll_angle]
# Elements[number] = [plate, material, section, node1, node2, node3, node4]
#
#elements[1] = ['beam', 1, 2, 1, 1, 0]
#elements[2] = ['beam', 2, 3, 1, 1, 0]
#elements[3] = ['beam', 3, 4, 1, 1, 0]
#
#print(elements)
# 
#
#               [beam_number, material, section, node1, node2, roll_angle]
#elements.beams([[1, 1, 2, 1, 1, 0],
#                [2, 2, 3, 1, 1, 0],
#                [3, 3, 4, 1, 1, 0]])
#
#print(elements)
#
beams = elements.beams()
# beam[number] = [material, section, node1, node2, roll_angle]
beams[15] = [10, 20, 1, 2]
beams[25] = [20, 30, 1, 2]
beams[35] = [30, 40, 1, 2]
#
print(elements)
#
#
#
#
# ----------------------------------------------------
# Load input
# ----------------------------------------------------
#
#
# ----------------------------------------------------
# Basic Load
#
# loading
load = mesh.load()
#
# load.basic.system = 'local'  # This will affect beam load only (global default)
#
#
# load numbering is automatic (consecutive)
# load.basic([[load_title, 'node', node_number, 'point',  x,y,z,mx,my,mz, comment(optional)],
#             [load_title, 'node', node_number, 'mass' ,  x,y,z, comment(optional)]
#             [load_title, 'beam', beam_number, 'line' ,  qx0,qy0,qz0, qx1,qy1,qz1, L0,L1, comment(optional)],
#             [load_title, 'beam', beam_number, 'point',  L0,x,y,z,mx,my,mz, comment(optional)]])
#
#basic = load.basic([['wind load x', 'node', 2, 'point', 0, -4_000_000_000, 0],
#                    ['wind load x', 'node', 3, 'point', 0, -2_000_000_000, 0],
#                    ['snow load'  , 'beam', 2, 'line' , -1_000_000, 0, 0]])
#
#print(basic)
#
basic = load.basic()
##basic[11] = 'wind load x'
#
# basic[1].node([[node_number, 'point', x,y,z,mx,my,mz, comment(optional)],
#                [node_number, 'mass' , x,y,z, comment(optional)]])
#
# basic[1].beam([[beam_number, 'point', L0,x,y,z,mx,my,mz, comment(optional)],
#                [beam_number, 'line' , qx0,qy0,qz0, qx1,qy1,qz1, L0,L1, comment(optional)]])
#
#basic[11].node([[2, 'load', 0, -4_000_000_000, 'wind_1'],
#                [3, 'load', 0, -2_000_000_000, 'wind_2']])
#
#
#
#
#basic[11] = 'snow load 2'
##basic[11].udl_beam.coordinate_system = 'local'
#blnode = basic[11].node()
#blnode[20].load = [-1_000_000, 0, 'wind_1']
#blnode[30].load = [-1_000_000, 0, 'wind_2']
#
#
#basic[22] = 'snow load'
#blbeam = basic[22].beam()
#blbeam[25].line = [0, -10_000, 0,  'wind_1']
##basic[1].udl_beam[2] = [-10_000_000, 0, 0]
#
##
basic[22] = 'snow load'
#basic[22].beam([25, 'line' , 0, -1_00_000,  0, 'snow load on roof'])
##basic[2].udl_beam.coordinate_system = 'local'
blbeam = basic[22].beam()
#blbeam[25].line = [0, -10_000, 0,  0, -10_000, 0,
#                  0, 5, 'validation_line']
#
#blbeam[25].line = [0, -10_000, 0,  0, -10_000, 0,
#                   'validation_line']
#
#blbeam[2].line = [0, 0, -10_000, 0, 0, -10_000,
#                  0, 0, 'validation']
#
blbeam[25].point = [5, 0 * units.N, -100_000 * units.N, 0 * units.N,
                    'validation_point']
#
#blbeam[2].line = [0, 2, -480_000, 'snow load on roof']
##basic[2].udl_beam[2] = [-10_000_000, 0, 0]
##basic[2].udl_beam[2] = [-4_800_000, 0, 0]
##basic[2].point_beam[2] = [2.5, 0, 0, -2_400_000]
##basic[2].point_beam[2] = [2.0, 0, 0, -1_920_000, 0, 0, 0]
##basic[2].point_beam[2] = [2.5, 0, 0, 0,  -100_000_000, 0, 0]
##
##
##basic = load.basic
##basic[1] = 'wind load y'
##basic[1].point_node[2] = [ 0, 0, -400_000, 0, 0, 0]
##basic[1].point_node[3] = [ 0, 0, -200_000, 0, 0, 0]
##
#basic[33] = 'crane load'
## tilt
##basic[3].point_beam.coordinate_system = 'local'
#blbeam = basic[33].beam()
#blbeam[25].point = [2.5, 0, 0, 0,  0, 0, -100_000_000, 'lifting module A']
##print(basic[3].point_beam.coordinate_system)
##
##basic[3].point_beam[2] = [2.0, -100_000_000]
## flat
##basic[3].point_beam[2] = [2.0, -100_000_000, 0,  0, 0, 0, 0]
##
##basic[2] = 'snow load'
##basic[2].udl_beam.coordinate_system = 'local'
##print(basic[2].udl_beam.coordinate_system)
#blbeam = basic[22].beam()
#blbeam[2].line = [0, -100_000_000, 0, 0, -200_000_000, 0, 1.0, 1.0, 'dead load']
##basic[2].udl_beam[2] = [0, -480_000_000*0.5, 0, 0, -480_000_000*0.5, 0]
##basic[2].udl_beam[2] = [0, -480_000_000*0.5, 0, 0, -480_000_000*0.5, 0]
#
# create new basic load
#basic[44] = 'dead load'
#basic[44].selfweight([50, 0, -1 * units.gravity, 0])
#
#print(basic)
#
#print(load)
#
# ----------------------------------------------------
# Load Combination
#
#comb = load.combination([['factored comb 1', 'basic', 11, 1.20],
#                         ['factored comb 1', 'basic', 22, 1.25],
#                         ['factored comb 1', 'basic', 33, 1.30]])
#
#
#comb = load.combination()
#comb[100] = 'factored comb 1'
#comb[100].basic[11] = 1.20
#comb[100].basic[22] = 1.25
#comb[100].basic[33] = 1.30
##
#comb[200] = 'factored comb 2'
#comb[200].basic[11] = 1.35
#comb[200].basic[22] = 1.40
#comb[200].basic[33] = 1.45
#
#comb[300] = 'factored comb 3'
#comb[300].combination[100] = 1.50
#comb[300].combination[200] = 1.55
#comb[300].basic[44] = 1.60
#
#print(comb)
#
print(load)
#
#
# ----------------------------------------------------
# Meshing input
# ----------------------------------------------------
#
mesh.build()
#
# ----------------------------------------------------
# Plot mesh
# ----------------------------------------------------
#
plot = mesh.plot()
plot.frame()
#
# ----------------------------------------------------
# Structural Analysis
# ----------------------------------------------------
#
#
#frame = Trave3D()
frame = Trave2D()
frame.mesh = mesh
frame.static(second_order=True)
results = frame.solve()
print(results)
print('-->')