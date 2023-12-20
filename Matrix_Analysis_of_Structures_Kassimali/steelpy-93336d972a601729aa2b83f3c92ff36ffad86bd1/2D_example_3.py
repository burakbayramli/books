# Import steelpy modules
from steelpy import Units
from steelpy import f2uModel
from steelpy import Trave2D
from steelpy import Trave3D
#
#
units = Units()
#
f2umodel = f2uModel()
#
#
# ----------------------------------------------------
# ----------------------------------------------------
# Mesh 
# ----------------------------------------------------
# ----------------------------------------------------
#
mesh = f2umodel.mesh(sql_file="example2D_2_f2u")
#
#
#
# ----------------------------------------------------
# mesh data
# ----------------------------------------------------
#
#
#
#
# ----------------------------------------------------
# Meshing
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
#
# ----------------------------------------------------
# Plotting
# ----------------------------------------------------
#
# Structure
#
#plot = mesh.plot()
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