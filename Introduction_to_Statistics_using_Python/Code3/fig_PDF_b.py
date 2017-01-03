'''
Graphical display of PDF (probability density function)
'''

# author: Thomas Haslwanter, date: April-2014

import numpy as np
import matplotlib.pyplot as plt
from scipy import stats
import seaborn as sns
from matplotlib import rc

rc('text', usetex=True)

outFile = r'C:\Users\p20529\Documents\Teaching\Master_FH\Stats\Images\PDF.png'
# Calculate the values

nd = stats.norm(5,2)

x = np.linspace(-1,10,100)
yp = nd.pdf(x)
x1 = np.linspace(4, 7)
y1 = nd.pdf(x1)

# Make the plot


plt.plot(x,yp)
plt.fill_between(x1, y1, facecolor='red')
plt.text(5.5, 0.05, r'P(a \le x \le b)', family='cursive', fontsize=14, horizontalalignment='center', style='italic')
plt.xlabel('x')
plt.ylabel('PDF(x)')
ax = plt.gca()


sns.set(context='poster')
plt.savefig(outFile, dpi=200)
print('Figure saved to {0}'.format(outFile))
plt.show()
