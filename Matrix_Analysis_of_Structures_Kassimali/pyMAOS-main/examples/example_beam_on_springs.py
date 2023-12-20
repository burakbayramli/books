from context import pyMAOS

from pyMAOS.nodes import R2Node
from pyMAOS.elements import R2Truss, R2Frame
from pyMAOS.material import LinearElasticMaterial as Material
from pyMAOS.section import Section
import pyMAOS.R2Structure as R2Struct
from pyMAOS.loadcombos import LoadCombo
from pyMAOS.plot_structure import plot_structure


########################################
#                                      #
# CONSISTENT UNIT SYSTEM REQUIRED !!!! #
#                                      #
########################################

# Units
# Length - inches
# Force - kips

loadcase = "D"
loadcombo = LoadCombo("S1", {"D": 1}, ["D"], False, "SLS")

# Nodes
num_nodes = 7
# parametric list of between 0 and 1'
eta = [0 + i * (1 / num_nodes) for i in range(num_nodes)]
length_ft = 10
length_in = length_ft * 12
# Generate Nodes
nodes = []
for pt in eta:
    x = pt * length_in
    nodes.append(R2Node(x, 0))

# Apply the springs
k = 1  # kpi

for node in nodes:
    node.releaseAll()
    node.applySpringUy(k, 0)

# Restrain last node for Ux to prevent rigid body motion in sliding
nodes[-1].restrainUx()

# Nodal Loads
position = 3
nodes[position].loads[loadcase] = [0, -10, 0]

# Materials
BeamMaterial = Material(0.00028, 29000)

# Sections
# W12x72
Area = 21.1
Ixx = 597
Iyy = 195
BeamSection = Section(Area, Ixx, Ixx)

members = []
# Generate Members
for i, node in enumerate(nodes):
    if i == 0:
        pass
    else:
        members.append(R2Frame(nodes[i - 1], node, BeamMaterial, BeamSection))

# Create the 2D Structure
Structure = R2Struct.R2Structure(nodes, members)
Structure.set_node_uids()
Structure.set_member_uids()

Structure.spring_nodes()

FM = Structure.freedom_map()
K = Structure.Kstructure(FM)
U = Structure.solve_linear_static(loadcombo)
Errors = Structure._ERRORS

# Print Output
print("-" * 100)
print("-" * 100)
print("Errors:")
print(Errors)
print(loadcombo)
print("Displacements:")
for i, node in enumerate(nodes):
    tx = node.displacements[loadcombo.name]
    print(f"N{node.uid} -- Ux: {tx[0]:.4E} -- Uy:{tx[1]:.4E} -- Rz:{tx[2]:.4E}")
print("-" * 100)
print("Reactions:")
for i, node in enumerate(nodes):
    rx = node.reactions[loadcombo.name]
    print(f"N{node.uid} -- Rx: {rx[0]:.4E} -- Ry:{rx[1]:.4E} -- Mz:{rx[2]:.4E}")
print("-" * 100)
print("Member Forces:")
for i, member in enumerate(members):
    fx = member.end_forces_local[loadcombo.name]

    print(f"M{member.uid}")
    print(
        f"    i -- Axial: {fx[0,0]:.4E} -- Shear: {fx[1,0]:.4E} -- Moment: {fx[2,0]:.4E}"
    )
    print(
        f"    j -- Axial: {fx[3,0]:.4E} -- Shear: {fx[4,0]:.4E} -- Moment: {fx[5,0]:.4E}"
    )
print("-" * 100)
print("-" * 100)
# Plot the structure
scaling = {
    "axial_load": 1,
    "normal_load": 1,
    "point_load": 1,
    "axial": 1,
    "shear": 1,
    "moment": 0.5,
    "rotation": 100,
    "displacement": 10,
}

plot_structure(nodes, members, loadcombo, scaling)
