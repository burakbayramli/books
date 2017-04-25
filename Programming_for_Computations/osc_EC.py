from numpy import zeros, linspace, pi, cos
import matplotlib.pyplot as plt

omega = 2
P = 2*pi/omega
dt = P/20
T = 40*P
T = P
N_t = int(round(T/dt))
t = linspace(0, N_t*dt, N_t+1)
print 'N_t:', N_t

u = zeros(N_t+1)
v = zeros(N_t+1)

# Initial condition
X_0 = 2
u[0] = X_0
v[0] = 0

# Step equations forward in time
for n in range(N_t):
    v[n+1] = v[n] - dt*omega**2*u[n]
    u[n+1] = u[n] + dt*v[n+1]

# Plot the last four periods to illustrate the accuracy
# in long time simulations
N4l = int(round(4*P/dt))  # No of intervals to be plotted
fig = plt.figure()
l1, l2 = plt.plot(t[-N4l:], u[-N4l:], 'b-',
                  t[-N4l:], X_0*cos(omega*t)[-N4l:], 'r--')
fig.legend((l1, l2), ('numerical', 'exact'), 'upper left')
plt.xlabel('t')
plt.show()
plt.savefig('tmp.pdf'); plt.savefig('tmp.png')
print '%.16f %.16f' % (u[-1], v[-1])
