"""Plot three curves. Use compact Easyviz syntax."""
from scitools.std import *

# Plot three curves in the same plot
t = linspace(0, 3, 51)  # 51 points between 0 and 3
y1 = t**2*exp(-t**2)
y2 = t**4*exp(-t**2)
# Pick out each 4 points and add random noise
t3 = t[::4]
random.seed(11)
y3 = y2[::4] + random.normal(loc=0, scale=0.02, size=len(t3))

plot(t, y1, 'r-', t, y2, 'b-', t3, y3, 'bo',
     legend=('t^2*exp(-t^2)', 't^4*exp(-t^2)', 'data'),
     title='Simple Plot Demo',
     axis=(0, 3, -0.05, 0.6),
     xlabel='t', ylabel='y',
     hardcopy='tmp1.eps',
     show=True)

hardcopy('tmp0.png') # this one can be included in HTML
