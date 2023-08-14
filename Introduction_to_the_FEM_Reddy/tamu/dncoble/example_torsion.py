import numpy as np
import matplotlib.pyplot as plt
from sklearn.metrics import r2_score
from math import sqrt
"""
Using fem2d to solve a sample problem with torsion in an elliptical cross
section beam.

Mesh node coordinates and element connections are saved in ./example problem/
"""
from fem2d import FEM2DProblemData
#%% problem data
Gtheta = 10
a = 1 # minor axis length
b = 1.3 # major axis length

elem_connectivity = np.load('./example problem/elem.npy')
node_coords = np.load('./example problem/node.npy')
problem_data = {}

problem_data['itype'] = 0
problem_data['igrad'] = 2
problem_data['item'] = 0
problem_data['neign'] = 0
problem_data['ieltyp'] = 1
problem_data['npe'] = 4
problem_data['mesh'] = 0
problem_data['nprnt'] = 1

problem_data['a10'] = 1
problem_data['a1x'] = 0
problem_data['a1y'] = 0
problem_data['a20'] = 1
problem_data['a2x'] = 0
problem_data['a2y'] = 0
problem_data['a00'] = 0

problem_data['iconv'] = 0

problem_data['f0'] = 2*Gtheta
problem_data['fx'] = 0
problem_data['fy'] = 0

# create symmetry and boundary value conditions
ispv = []
vspv = []
issv = []
vssv = []
plt.figure()
for node_num, node in enumerate(node_coords):
    node_num += 1 # 1-indexed
    if(node[0] == 0 or node[1] == 0):
        issv.append([node_num, 1]) # [node number, local degree of freedom]
        vssv.append(0) # 0 secondary variable value at node
    if(abs((node[0]/b)**2 + (node[1]/a)**2 - 1) < .004):
        ispv.append([node_num, 1])
        vspv.append(0)
    
problem_data['issv'] = issv
problem_data['vssv'] = vssv
problem_data['ispv'] = ispv
problem_data['vspv'] = vspv
#%% build and solve problem
mesh = FEM2DProblemData(elem_connectivity, node_coords, print_sol=False, **problem_data)
mesh.build_cards()
solu = mesh.run()
mesh.postprocess(poly_order=2)
#%% plot the mesh
plt.figure()
for elem in elem_connectivity:
    n0 = node_coords[elem[0]-1]
    n1 = node_coords[elem[1]-1]
    n2 = node_coords[elem[2]-1]
    n3 = node_coords[elem[3]-1]
    c='lightsteelblue'
    plt.plot([n0[0], n1[0]], [n0[1], n1[1]], c=c)
    plt.plot([n1[0], n2[0]], [n1[1], n2[1]], c=c)
    plt.plot([n2[0], n3[0]], [n2[1], n3[1]], c=c)
    plt.plot([n3[0], n0[0]], [n3[1], n0[1]], c=c)
plt.xticks([])
plt.yticks([])
plt.axis('equal')
plt.plot(node_coords[:,0], node_coords[:,1], marker='.', linewidth=0)
plt.tight_layout()
#%% vector plot
x0, x1 = np.meshgrid(np.linspace(.05, 1.3, 16), np.linspace(0, 1, 12))
point_mesh = np.vstack((x0.reshape(-1),x1.reshape(-1))).T
in_ellipse = np.array([(x[0]/b)**2 + (x[1]/a)**2 < 1 for x in point_mesh])
point_mesh = point_mesh[in_ellipse]

X = point_mesh[:,0]
Y = point_mesh[:,1]

# list with primary and secondary variable arrays for each mesh
u = mesh.get_var('u', point_mesh)
qx = mesh.get_var('qx', point_mesh).reshape(-1, 1)
qy = mesh.get_var('qy', point_mesh).reshape(-1, 1)
q = np.append(qx, qy, axis=1)

ellipse = np.array([[0,0]] + [[.01*x, a*sqrt(1-(.01*x/b)**2)] for x in range(0, 131)])
el = plt.Polygon(ellipse, facecolor='lightsteelblue', edgecolor='k')

plt.figure()
plt.axis('equal')
plt.gca().add_patch(el)
plt.quiver(X, Y, qy, qx, color='tab:blue', headwidth=3)
plt.xticks([0, 1])
plt.yticks([0, 1])
plt.xlim((-.2, 1.5))
plt.tight_layout()
plt.savefig('./example problem/vector field.png', dpi=500)
