# Import steelpy modules
from steelpy import Units
from steelpy import f2uModel
from steelpy import Trave2D
from steelpy import Spreadsheet
#
#
units = Units()
#
f2umodel = f2uModel(component="2D_example1")
#
# ----------------------------------------------------
# Data
#
ss = Spreadsheet()
wb = ss.read_book("trave3D.xlsx")
sheets = wb.sheet_names
print(sheets)
#
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
data = wb.sheets["Material"]
matdata = data.to_df()
mesh.materials(df=matdata)
print(mesh.materials())
#
#
# ----------------------------------------------------
# Section Input
# ----------------------------------------------------
#
data = wb.sheets["Sections"]
setdata = data.to_df()
mesh.sections(df=setdata)
print(mesh.sections())
#
#
#
#
# ----------------------------------------------------
# Node input
# ----------------------------------------------------
#
# nodes corrdinates [node_id, x, y, z=0]
#
data = wb.sheets["Nodes"]
nodedata = data.to_df()
mesh.nodes(df=nodedata)
print(mesh.nodes())
#
#
# ----------------------------------------------------
# boundary Input
# ----------------------------------------------------
#
# [node_id, type, fixity]
#mesh.boundaries([[1, 'support', 'fixed'],
#                 [2, 'support', 'fixed'],
#                 [3, 'support', 'fixed']])
#
support =  nodedata.drop(["x", "y", "z"], axis=1)
#support.rename(columns={'ix': 'x', 'iy': 'y', 'iz': 'z'},
#               inplace=True)
support["type"] = "supports"
#
mesh.boundaries(df=support)
print(mesh.boundaries())
#
# ----------------------------------------------------
# Element input
# ----------------------------------------------------
#
# Example:
# Elements[number] = [id, beam, material, section, node1, node2, roll_angle]
# Elements[number] = [id, plate, material, section, node1, node2, node3, node4]
#
#
data = wb.sheets["Elements"]
membdata = data.to_df()
mesh.elements(df=membdata)
print(mesh.elements().beams())
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
#
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
#basic[10] = 'Beam load'
#basic[10].beam([[8, 'line', 0, -1000, 'udl_1'],
#                [9, 'line', 0, -1000, 'udl_2'],
#                [10, 'line', 0, -1000, 'udl_2']])
#
nullLoad = 0 * units.N
pointLoad = -1_000 * units.N
basic[11] = 'Buckling Example'
basic[11].node([[4, 'load', nullLoad, pointLoad, 'buckling_1'],
                [5, 'load', nullLoad, pointLoad, 'buckling_2'],
                [6, 'load', nullLoad, pointLoad, 'buckling_3'],
                [7, 'load', nullLoad, pointLoad, 'buckling_4'],
                [8, 'load', nullLoad, pointLoad, 'buckling_5'],
                [9, 'load', nullLoad, pointLoad, 'buckling_6']])
#
print(basic)
#
#
#
# ----------------------------------------------------
# Meshing input
# ----------------------------------------------------
#
#
mesh.build()
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
# ----------------------------------------------------
# Plot mesh
# ----------------------------------------------------
#
plot = mesh.plot()
plot.frame()
#plot.material()
#
# Loading
#
plotload = load.plot()
plotload.basic()
#
# ----------------------------------------------------
# Structural Analysis
# ----------------------------------------------------
#
frame = Trave2D()
frame.mesh = mesh
frame.static()
results = frame.solve()
print(results)
print('-->')