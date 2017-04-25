"""As SIRV1.py, but including time-dependent vaccination."""

from numpy import zeros, linspace
import matplotlib.pyplot as plt

# Time unit: 1 h
beta = 10./(40*8*24)
beta /= 4            # Reduce beta compared to SIR1.py
print 'beta:', beta
gamma = 3./(15*24)
dt = 0.1             # 6 min
D = 100              # Simulate for D days
N_t = int(D*24/dt)   # Corresponding no of hours
nu = 1./(24*50)      # Average loss of immunity: 50 days

t = linspace(0, N_t*dt, N_t+1)
S = zeros(N_t+1)
I = zeros(N_t+1)
R = zeros(N_t+1)
V = zeros(N_t+1)

# Vaccination campaign
p = zeros(N_t+1)
start_index = 6*24/dt  # 6 days = 6*24 h, div. by dt=0.1 gives intervals
stop_index = 15*24/dt
p[start_index:stop_index] = 0.005

# Initial condition
S[0] = 50
I[0] = 1
R[0] = 0
V[0] = 0

# Step equations forward in time
for n in range(N_t):
    S[n+1] = S[n] - dt*beta*S[n]*I[n] + dt*nu*R[n] - dt*p[n]*S[n]
    V[n+1] = V[n] + dt*p[n]*S[n]
    I[n+1] = I[n] + dt*beta*S[n]*I[n] - dt*gamma*I[n]
    R[n+1] = R[n] + dt*gamma*I[n] - dt*nu*R[n]
    loss = int(V[n+1] + S[n+1] + R[n+1] + I[n+1]) - \
           int(V[0] + S[0] + R[0] + I[0])
    if loss > 0:
        print 'loss: %d' % loss

fig = plt.figure()
l1, l2, l3, l4 = plt.plot(t, S, t, I, t, R, t, V)
fig.legend((l1, l2, l3, l4), ('S', 'I', 'R', 'V'), 'upper right')
plt.xlabel('hours')
plt.show()
plt.savefig('tmp.pdf'); plt.savefig('tmp.png')
