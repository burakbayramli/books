import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

# After finding P, the matrix of joint probabilities, use
# the following code to generate plots
r1p = np.outer(r, np.ones(n))
r2p = np.outer(np.ones(n), r)
P = np.asarray(P.value)
fig = plt.figure()
ax = fig.gca(projection = '3d')
ax.plot_wireframe(r1p, r2p, P.T, rstride=3, cstride=3)
ax.set_xlabel('$R_1$')
ax.set_ylabel('$R_2$')
ax.set_zlabel('density')
plt.savefig('loss_bounds_mesh.eps')
plt.show()

plt.figure()
levels = np.linspace(0.003, 0.014, 7)
plt.contour(r1p, r2p, P.T, levels)
plt.xlabel('$R_1$')
plt.ylabel('$R_2$')
plt.savefig('loss_bounds_cont.eps')
plt.show()
