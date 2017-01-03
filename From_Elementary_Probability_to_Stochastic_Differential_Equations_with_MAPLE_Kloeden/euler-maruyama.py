import numpy as np
import matplotlib.pyplot as plt

tBegin = 0
tEnd = 2
dt = .00001

t = np.arange(tBegin, tEnd, dt)
N = t.size 
IC = 0
theta = 1
mu = 1.2
sigma = 0.3

sqrtdt = np.sqrt(dt)
y = np.zeros(N)
y[0] = IC
for i in xrange(1, N):
    y[i] = y[i-1] + dt*(theta*(mu-y[i-1])) + sigma*sqrtdt*np.random.normal(loc=0.0, scale=1.0)

ax = plt.subplot(111)
ax.plot(t, y)
plt.show()
