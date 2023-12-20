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

# Sloped beam to test braced frame
loadcase = "D"
loadcase2 = "L"
loadcombo2 = LoadCombo("S1", {"D": 1}, ["D"], False, "SLS")
loadcombo = LoadCombo("S2", {"D":1.2,"L": 1.6}, ["L"], False, "SLS")

# Nodes
N1 = R2Node(0, 0)
N2 = R2Node(6.16 * 12, 5.76 * 12)
N3 = R2Node((6.16 + 3.67) * 12, 5.76 * 12)
N4 = R2Node((6.16 + 3.67 + 6.33) * 12, (5.76 + 5.1) * 12)


# Node Restraints
N1.releaseMz()
N2.releaseAll()
N3.releaseAll()
N4.releaseMz()

# Node List
nodes = [N1, N2, N3, N4]

# Nodal Loads
# N2.loads[loadcase] = [50, 0, 0]


# Materials
CIPMaterial = Material((110 / (12 * 12 * 12.0)), 2408)


# Sections
# 9x44
StairSection = Section(396, 2673, 63888)


# Members
RF1 = R2Frame(N1, N2, CIPMaterial, StairSection)
RF2 = R2Frame(N2, N3, CIPMaterial, StairSection)
RF3 = R2Frame(N3, N4, CIPMaterial, StairSection)

# Member List
members = [RF1, RF2, RF3]

# Member Release
# RF2.hinge_i()
# RF2.hinge_j()

# Member Loads
sw = -0.3025 / 12
RF1.add_distributed_load(sw, sw, 0, 100, loadcase, "Y", location_percent=True)
RF2.add_distributed_load(sw, sw, 0, 100, loadcase, "Y", location_percent=True)
RF3.add_distributed_load(sw, sw, 0, 100, loadcase, "Y", location_percent=True)

sdl = -0.10093 / 12
RF1.add_distributed_load(sdl, sdl, 0, 100, loadcase, "Y", location_percent=True)
RF2.add_distributed_load(sdl, sdl, 0, 100, loadcase, "Y", location_percent=True)
RF3.add_distributed_load(sdl, sdl, 0, 100, loadcase, "Y", location_percent=True)

ll = (-100 * 3.67) / (12 * 1000)
RF1.add_distributed_load(
    ll, ll, 0, 100, loadcase2, "Y", location_percent=True, projected=True
)
RF2.add_distributed_load(
    ll, ll, 0, 100, loadcase2, "Y", location_percent=True, projected=True
)
RF3.add_distributed_load(
    ll, ll, 0, 100, loadcase2, "Y", location_percent=True, projected=True
)

# Create the 2D Structure
Structure = R2Struct.R2Structure(nodes, members)
Structure.set_node_uids()
Structure.set_member_uids()

FM = Structure.freedom_map()
K = Structure.Kstructure(FM)
U = Structure.solve_linear_static(loadcombo)
U2 = Structure.solve_linear_static(loadcombo2)
Errors = Structure._ERRORS

# Print Output
print("Errors:")
print(Errors)
print(f"Loadcase: {loadcase}")
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
print(f"Loadcase: {loadcase2}")
print("Displacements:")
for i, node in enumerate(nodes):
    tx = node.displacements[loadcombo2.name]
    print(f"N{node.uid} -- Ux: {tx[0]:.4E} -- Uy:{tx[1]:.4E} -- Rz:{tx[2]:.4E}")
print("-" * 100)
print("Reactions:")
for i, node in enumerate(nodes):
    rx = node.reactions[loadcombo2.name]
    print(f"N{node.uid} -- Rx: {rx[0]:.4E} -- Ry:{rx[1]:.4E} -- Mz:{rx[2]:.4E}")
print("-" * 100)
print("Member Forces:")
for i, member in enumerate(members):
    fx = member.end_forces_local[loadcombo2.name]

    print(f"M{member.uid}")
    print(
        f"    i -- Axial: {fx[0,0]:.4E} -- Shear: {fx[1,0]:.4E} -- Moment: {fx[2,0]:.4E}"
    )
    print(
        f"    j -- Axial: {fx[3,0]:.4E} -- Shear: {fx[4,0]:.4E} -- Moment: {fx[5,0]:.4E}"
    )

# Max/Min Mz
for member in members:
    print(member.Mzextremes(loadcombo))

# Plot the structure
scaling = {
        "axial_load": 100,
        "normal_load": 100,
        "point_load": 1,
        "axial": 2,
        "shear": 2,
        "moment": 0.1,
        "rotation": 5000,
        "displacement": 100,
    }

plot_structure(nodes, members, loadcombo, scaling)
