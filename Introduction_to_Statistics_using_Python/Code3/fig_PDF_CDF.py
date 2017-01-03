'''
Graphical display of PDF (probability density function) and CDF (cumulative density function)
'''

# author: Thomas Haslwanter, date: April-2014

import numpy as np
import matplotlib.pyplot as plt
from scipy import stats
import seaborn as sns

outFile = r'C:\Users\p20529\Documents\Teaching\Master_FH\Stats\Images\PDF_CDF.png'
# Calculate the values
nd = stats.norm()

x = np.linspace(-3,3,100)
yp = nd.pdf(x)
y = nd.cdf(x)
x1 = np.linspace(-3, 1)
y1 = nd.pdf(x1)

# Make the plot
figs, axs = plt.subplots(1,2)

axs[0].plot(x,yp)
axs[0].fill_between(x1, y1, facecolor='red')
axs[0].text(0, 0.1, 'CDF(x)', family='cursive', fontsize=14, horizontalalignment='center', style='italic')
axs[0].set_xlabel('x')
axs[0].set_ylabel('PDF(x)')

axs[1].plot(x, y, 'r', lw=2)
axs[1].set_xlabel('x')
axs[1].set_ylabel('CDF(x)')

sns.set(context='poster')
plt.tight_layout()
plt.savefig(outFile, dpi=200)
print('Figure saved to {0}'.format(outFile))
plt.show()
