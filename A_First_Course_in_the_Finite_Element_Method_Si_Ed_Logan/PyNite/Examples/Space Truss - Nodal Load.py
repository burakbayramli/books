# Engineering Mechanics: Statics, 4th Edition
# Bedford and Fowler
# Problem 6.64
# Units for this model are meters and kilonewtons

# Import 'FEModel3D' and 'Visualization' from 'PyNite'
from PyNite import FEModel3D
from PyNite import Visualization

# Create a new model
truss = FEModel3D()

# Define the nodes
truss.add_node('A', 1.1, -0.4, 0)
truss.add_node('B', 1, 0, 0)
truss.add_node('C', 0, 0, 0.6)
truss.add_node('D', 0, 0, -0.4)
truss.add_node('E', 0, 0.8, 0)

# Define the supports
truss.def_support('C', True, True, True, True, True, True)
truss.def_support('D', True, True, True, True, True, True)
truss.def_support('E', True, True, True, True, True, True)

# Member properties were not given for this problem, so assumed values will be used
# To make all the members act rigid, the modulus of elasticity will be set to a very large value
E = 99999999
G = 100
nu = 0.3
rho = 1
truss.add_material('Rigid', E, G, nu, rho)


# Create members
truss.add_member('AB', 'A', 'B', 'Rigid', 100, 100, 100, 100)
truss.add_member('AC', 'A', 'C', 'Rigid', 100, 100, 100, 100)
truss.add_member('AD', 'A', 'D', 'Rigid', 100, 100, 100, 100)
truss.add_member('BC', 'B', 'C', 'Rigid', 100, 100, 100, 100)
truss.add_member('BD', 'B', 'D', 'Rigid', 100, 100, 100, 100)
truss.add_member('BE', 'B', 'E', 'Rigid', 100, 100, 100, 100)

# Release the moments at the ends of the members to make truss members
truss.def_releases('AC', False, False, False, False, True, True, \
                           False, False, False, False, True, True)
truss.def_releases('AD', False, False, False, False, True, True, \
                           False, False, False, False, True, True)
truss.def_releases('BC', False, False, False, False, True, True, \
                           False, False, False, False, True, True)
truss.def_releases('BD', False, False, False, False, True, True, \
                           False, False, False, False, True, True)
truss.def_releases('BE', False, False, False, False, True, True, \
                           False, False, False, False, True, True)

# Add nodal loads
truss.add_node_load('A', 'FX', 10)
truss.add_node_load('A', 'FY', 60)
truss.add_node_load('A', 'FZ', 20)

# Analyze the model
truss.analyze(check_statics=True)

# Print results
print('Member BC calculated axial force: ' + str(truss.Members['BC'].max_axial()))
print('Member BC expected axial force: 32.7 Tension')
print('Member BD calculated axial force: ' + str(truss.Members['BD'].max_axial()))
print('Member BD expected axial force: 45.2 Tension')
print('Member BE calculated axial force: ' + str(truss.Members['BE'].max_axial()))
print('Member BE expected axial force: 112.1 Compression')

# Render the model for viewing. The text height will be set to 50 mm.
# Because the members in this example are nearly rigid, there will be virtually no deformation. The deformed shape won't be rendered.
# The program has created a default load case 'Case 1' and a default load combo 'Combo 1' since we didn't specify any. We'll display 'Case 1'.
Visualization.render_model(truss, annotation_size=0.05, render_loads=True, case='Case 1')
