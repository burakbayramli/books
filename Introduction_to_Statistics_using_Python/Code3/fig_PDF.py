"""
Plot demonstrating the integral as the area under a curve of a PDF.

"""

# author: Thomas Haslwanter, date: Jan-2014

import numpy as np
import matplotlib.pyplot as plt
import os
from matplotlib.patches import Polygon
from scipy import stats


a, b = -0.5, 1.5 #integral limits
x = np.linspace(-3, 3)
func = stats.norm().pdf
y = func(x)

fig, ax = plt.subplots()
plt.plot(x, y, 'r', linewidth=2)
plt.ylim(ymin=0)

# Make the shaded region
ix = np.linspace(a, b)
iy = func(ix)
plt.fill_between(ix, iy, facecolor=[0.9, 0.9, 0.9], edgecolor=[0.5, 0.5, 0.5])

fs = 25
plt.text(0.5 * (a + b), 0.1, r"$\int_a^b p(x)\mathrm{d}x$",
         horizontalalignment='center', fontsize=fs)

plt.figtext(0.9, 0.05, '$x$', fontsize=fs)
plt.figtext(0.2, 0.95, '$p(x)$', fontsize=fs)
plt.axvline(x=-2, ymin=0, ymax=1, hold=None, color='k', linewidth=1)

for loc in ['left', 'right', 'top']:
    ax.spines[loc].set_visible(False)
ax.xaxis.set_ticks_position('bottom')

ax.set_xticks((a, b))
ax.set_xticklabels(('$a$', '$b$'), fontsize=fs)
ax.set_yticks([])

outDir = r'C:\Users\p20529\Documents\Teaching\Master_FH\Stats\Images'
outFile = os.path.join(outDir, 'pdf.png')
plt.savefig(outFile, dpi=200)
print('Figure saved to {0}'.format(outFile))

plt.show()
