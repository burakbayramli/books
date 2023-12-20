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

# Sloped beam to test projected loading
loadcase = "D"
loadcombo = LoadCombo("S1", {"D": 1}, ["D"], False, "SLS")

# Nodes
N1 = R2Node(0, 0)
N2 = R2Node(8.66, 5)
N3 = R2Node(20, 0)
N4 = R2Node(28.66, 5)
N5 = R2Node(0, 10)
N6 = R2Node(8.66, 15)
N7 = R2Node(20, 10)
N8 = R2Node(28.66, 15)

# Node List
nodes = [N1, N2, N3, N4, N5, N6, N7, N8]

# Node Restraints
for node in nodes:
    node.releaseMz()

# Nodal Loads

# Materials
BeamMaterial = Material(1, 29000 * 6894.76)

# Sections
# W24x55
BeamSection = Section(0.0035, 0.000005770875286)

# Members
RF1 = R2Frame(N1, N2, BeamMaterial, BeamSection)
RF2 = R2Frame(N3, N4, BeamMaterial, BeamSection)
RF3 = R2Frame(N5, N6, BeamMaterial, BeamSection)
RF4 = R2Frame(N7, N8, BeamMaterial, BeamSection)

# Member List
members = [RF1, RF2, RF3, RF4]

# Member Release

# Member Loads
RF1.add_distributed_load(
    -1, -1, 0, 100, direction="Y", location_percent=True, projected=False
)
RF2.add_distributed_load(
    -1, -1, 0, 100, direction="Y", location_percent=True, projected=True
)
RF3.add_distributed_load(
    1, 1, 0, 100, direction="X", location_percent=True, projected=False
)
RF4.add_distributed_load(
    1, 1, 0, 100, direction="X", location_percent=True, projected=True
)

# Create the 2D Structure
Structure = R2Struct.R2Structure(nodes, members)
Structure.set_node_uids()
Structure.set_member_uids()

FM = Structure.freedom_map()
K = Structure.Kstructure(FM)
U = Structure.solve_linear_static(loadcombo)
Errors = Structure._ERRORS

# Print Output
print("Errors:")
print(Errors)
print("Displacements:")
for i, node in enumerate(nodes):
    tx = node.displacements[loadcombo.name]
    print(
        f"N{node.uid} -- Ux: {tx[0]:.4E} -- Uy:{tx[1]:.4E} -- Rz:{tx[2]:.4E}"
    )
print("-" * 100)
print("Reactions:")
for i, node in enumerate(nodes):
    rx = node.reactions[loadcombo.name]
    print(
        f"N{node.uid} -- Rx: {rx[0]:.4E} -- Ry:{rx[1]:.4E} -- Mz:{rx[2]:.4E}"
    )
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


# Plot the structure
scaling = {
        "axial_load": 1,
        "normal_load": 1,
        "point_load": 1,
        "axial": 1,
        "shear": 1,
        "moment": 0.1,
        "rotation": 100,
        "displacement": 50,
    }

plot_structure(nodes, members, loadcombo, scaling)