import matplotlib.pyplot as plt
from scipy import *
from scipy import integrate
from scipy.integrate import ode, odeint
import numpy as np

plt.close()
fig,ax = plt.subplots(1,1,figsize=(8,8))
## Vector field function
def vf1(x,t):
  dx=np.zeros(2)
  dx[0]=x[0]
  dx[1]=x[1]*(1-x[1])
  return dx
#Integrate under curve
# Initial condition
xx0 = np.array([.1,.75])
# Solve
t = np.linspace(0, 5, 1000)
ys = odeint(vf1, xx0, t)

# y0 = [1, -.25]
# t = np.linspace(0, 5, 1000)
# 
# r = integrate.ode(vf1)
# r.set_integrator('dopri5');
# r.set_initial_value(y0, t[0]);
# 
# dt = t[1] - t[0]
# y = np.zeros((len(t), len(y0)))
# idx = 0
# while r.successful() and r.t < t[-1]:
#     y[idx, :] = np.real(r.y)
#     r.integrate(r.t + dt)
#     idx += 1

#Vector field
x1,y1 = np.linspace(0,5,1000),np.linspace(-2,2,1000)
X,Y = np.meshgrid(x1,y1 )
U = X
V = Y*(1-Y)

#Normalize arrows
N = np.sqrt(U**2+V**2)
U2, V2 = U/N, V/N

start = [[2,.5],[1.5,1.5]]

strm = ax.streamplot( X,Y,U2, V2)
strmS = ax.streamplot(x1,y1, U2, V2, start_points=start, color="crimson", linewidth=1)
ax.plot(ys[:,0],ys[:,1],label='scipy.ode')
plt.xlim([0,5])
plt.ylim([-2,2])
plt.xlabel(r"$x$")
plt.ylabel(r"$y$")
plt.legend()
plt.show()
