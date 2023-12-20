from steelpy import Trave3D
from steelpy import Units
from steelpy import f2uModel


units = Units()
f2umodel = f2uModel(component='frame1')
mesh = f2umodel.mesh()

#
material = mesh.materials()
material[1] = ['elastic', 345.0 * units.MPa]
print(material)
#
section = mesh.sections()
section[1] = ['tubular', 500 * units.mm, 25 * units.mm]
print(section)
#
#
#
# nodes corrdinates [x, y, z]
nodes = mesh.nodes()
nodes[1] = [0 * units.m, 0 * units.m, 0 * units.m]
nodes[2] = [3 * units.m, 0 * units.m, 0 * units.m]
nodes[3] = [6 * units.m, 0 * units.m, 0 * units.m]
#
# boundary type
boundary = mesh.boundaries()
# supports[node_number] =  boundary/parameters
supports = boundary.supports()
supports[1] = [1,1,1,1,1,1]
#supports[3] = [1,1,1,0,0,0]
print(boundary)
#
# elements = [type, node1, node2, material, section]
elements = mesh.elements()
elements[1] = ['beam', 1, 2, 1, 1]
elements[2] = ['beam', 2, 3, 1, 1]
#
#elements[1] = ['beam', 1, 3, 1, 1]
#
# loading
#
load = mesh.load()

#
#load.basic = [['wind load x', 'node', 2, 'point', 0, -4_000_000_000, 0],
#                ['wind load x', 'node', 3, 'point', 0, -2_000_000_000, 0]]
#         ['snow load'  , 'beam', 2, 'line'  , -1_000_000, 0, 0]]
#
basic = load.basic()
#
basic[111] = 'wind load'
#
#basic[111].node = [[2, 'load', 0, -1000, 0, 'test1'],
#                   [3, 'load', 0, -1000, 0, 'test2']]
#
blnode = basic[111].node()
blnode[3].load = [0 * units.N, -1000 * units.N, 0 * units.N, 'ytest']
blnode[3].load = [0 * units.N, 0* units.N, -1000 * units.N, 'ztest']
#
blnode[2].load = [-1000, 0, 0, 'xtest']
#blnode[2].mass = [0, -9.81, 0, 'ymass_test']
#blnode[2].mass = [0, 0, -9.81, 'zmass_test']
#
blnode[3].load = [-1000, 0, 0, 'xtest3']
#
#
blbeam =  basic[111].beam()
blbeam[1].point = [3.0* units.m, 0 * units.N, -1000 * units.N, 0 * units.N, 'test']
blbeam[1].line = [0 * units.N / units.m, -1_000 * units.N / units.m, 0 * units.N / units.m, 'wind_1']
blbeam[2].line = [0 * units.N / units.m, 0 * units.N / units.m, -1_000 * units.N / units.m, 'wind_2']
#
blbeam[2].line = [0 * units.N / units.m, -2_000 * units.N / units.m, 0 * units.N / units.m, 'wind_3']
#
#
#basic[111].beam = [[1, 'point', 3.0, 0, -1000, 0, 'test'],
#                   [1, 'line', 0, -1_000, 0, 'wind_1'],
#                   [2, 'line', 0, 0, -1_000, 'wind_2']]
#
print(load)
#
#for load_name, lcase in basic.items():
#    for name, loads in lcase.node.items():
#        for key, items in loads.point.items():
#            for item in items:
#                print(item)
#    #
#    for name, loads in lcase.beam.items():
#        for key, items in loads.point.items():
#            for item in items:
#                print( item )
#        #
#        for key, items in loads.line.items():
#            for item in items:
#                print( item )
#
mesh.build()
#
frame = Trave3D()
frame.mesh = mesh
frame.static()
results = frame.solve()
print(results)
print('-->')