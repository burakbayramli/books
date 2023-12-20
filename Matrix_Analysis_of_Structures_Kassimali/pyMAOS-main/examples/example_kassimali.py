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

# Matrix Analysis of Structures, Kassimali -- Section 6.7 Computer Program
loadcase = "D"
loadcombo = LoadCombo("S1", {"D": 1}, ["D"], False, "SLS")

# Nodes
N1 = R2Node(0, 0)
N2 = R2Node(0, 240)
N3 = R2Node(240, 336)
N4 = R2Node(480, 240)
N5 = R2Node(480, 0)

# Node Restraints
N2.releaseAll()
N3.releaseAll()
N4.releaseAll()

# Node List
nodes = [N1, N2, N3, N4, N5]

# Nodal Loads
N2.loads[loadcase] = [75, 0, 0]

# Materials
ColMaterial = Material(29000)
GirderMaterial = Material(10000)

# Sections
ColSection = Section(29.8, 2420)
GirderSection = Section(30.6, 3100)

# Members
RF1 = R2Frame(N1, N2, ColMaterial, ColSection)
RF2 = R2Frame(N2, N3, GirderMaterial, GirderSection)
RF3 = R2Frame(N4, N3, GirderMaterial, GirderSection)
RF4 = R2Frame(N5, N4, ColMaterial, ColSection)

# Member List
members = [RF1, RF2, RF3, RF4]

# Member Loads
RF2.add_distributed_load(
    -0.25, -0.25, 0, 100, loadcase, "yy", location_percent=True
)
RF3.add_point_load(-20, 50, loadcase, "xx", location_percent=True)
RF3.add_point_load(45, 50, loadcase, "yy", location_percent=True)

# Create the 2D Structure
Structure = R2Struct.R2Structure(nodes, members)
Structure.set_node_uids()
Structure.set_member_uids()

FM = Structure.freedom_map()
K = Structure.Kstructure(FM)
U = Structure.solve_linear_static(loadcombo)
Errors = Structure._ERRORS

# Kassimali Reference Values for Section 6.7
kassimali_displacements = [
    [0, 0, 0],
    [3.4472, -9.1684e-3, -1.9513e-2],
    [3.9520, -1.3152, 7.0646e-3],
    [4.4247, -2.116e-2, -9.2708e-3],
    [0, 0, -2.3019e-2],
]

kassimali_memberforces = [
    [[3.3014e1, 6.7356e1, 1.3789e4], [-3.3014e1, -6.7356e1, 2.3767e3]],
    [[1.9358e1, 2.7814e1, -2.3767e3], [-1.9358e1, 3.6808e1, 1.2142e3]],
    [[5.9404e1, -5.8303e1, -8.0403e3], [-3.9404e1, 1.3303e1, -1.2142e3]],
    [[7.6195e1, 3.3501e1, 1.5378e-3], [-7.6195e1, -3.3501e1, 8.0403e3]],
]

kassimali_reactions = [
    [-6.7356e1, 3.3014e1, 1.3789e4],
    [0, 0, 0],
    [0, 0, 0],
    [0, 0, 0],
    [-3.3501e1, 7.6195e1, 0],
]

# Print Output
print("Errors:")
print(Errors)
print("Displacements:")
for i, node in enumerate(nodes):
    tx = node.displacements[loadcombo.name]
    print(
        f"N{node.uid} -- Ux: {tx[0]:.4E} ({tx[0]-kassimali_displacements[i][0]:.2E}) -- Uy:{tx[1]:.4E} ({tx[1]-kassimali_displacements[i][1]:.2E})  -- Rz:{tx[2]:.4E} ({tx[2]-kassimali_displacements[i][2]:.2E})"
    )
print("-" * 100)
print("Reactions:")
for i, node in enumerate(nodes):
    rx = node.reactions[loadcombo.name]
    print(
        f"N{node.uid} -- Rx: {rx[0]:.4E} ({rx[0]-kassimali_reactions[i][0]:.2E}) -- Ry:{rx[1]:.4E} ({rx[1]-kassimali_reactions[i][1]:.2E}) -- Mz:{rx[2]:.4E} ({rx[2]-kassimali_reactions[i][2]:.2E})"
    )
print("-" * 100)
print("Member Forces:")
for i, member in enumerate(members):
    fx = member.end_forces_local[loadcombo.name]

    dai = fx[0, 0] - kassimali_memberforces[i][0][0]
    dsi = fx[1, 0] - kassimali_memberforces[i][0][1]
    dmi = fx[2, 0] - kassimali_memberforces[i][0][2]
    daj = fx[3, 0] - kassimali_memberforces[i][1][0]
    dsj = fx[4, 0] - kassimali_memberforces[i][1][1]
    dmj = fx[5, 0] - kassimali_memberforces[i][1][2]

    print(f"M{member.uid}")
    print(
        f"    i -- Axial: {fx[0,0]:.4E} ({dai:.2E}) -- Shear: {fx[1,0]:.4E} ({dsi:.2E}) -- Moment: {fx[2,0]:.4E} ({dmi:.2E})"
    )
    print(
        f"    j -- Axial: {fx[3,0]:.4E} ({daj:.2E}) -- Shear: {fx[4,0]:.4E} ({dsj:.2E}) -- Moment: {fx[5,0]:.4E} ({dmj:.2E})"
    )

# Max/Min Mz
for member in members:
    print(member.Mzextremes(loadcombo))
    
# Plot the structure
scaling = {
        "axial_load": 100,
        "normal_load": 100,
        "point_load": 1,
        "axial": 1,
        "shear": 1,
        "moment": 0.01,
        "rotation": 5000,
        "displacement": 100,
    }

plot_structure(nodes, members, loadcombo, scaling)