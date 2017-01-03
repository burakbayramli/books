'''
Demonstration of the probplot of a non-normal distribution
'''

# author: Thomas Haslwanter, date: May-2014

import numpy as np
import matplotlib.pyplot as plt
from scipy import stats
import seaborn as sns
import os.path

# Define the skewed distribution
chi2 = stats.chi2(3)

# Generate the data
x = np.linspace(0,10, 100)
y = chi2.pdf(x)
data = chi2.rvs(100)

# Arrange subplots
fig, axs = plt.subplots(1,2)

# Plot distribution
axs[0].plot(x,y)

axs[0].set_xlabel('X')
axs[0].set_ylabel('PDF(X)')
axs[0].set_title('chi2(x), k=3')

x0, x1 = axs[0].get_xlim()
y0, y1 = axs[0].get_ylim()
axs[0].set_aspect((x1-x0)/(y1-y0))


# Plot probplot
plt.axes(axs[1])
stats.probplot(data, plot=plt)

x0, x1 = axs[1].get_xlim()
y0, y1 = axs[1].get_ylim()
axs[1].set_aspect((x1-x0)/(y1-y0))

outDir = r'C:\Users\p20529\Documents\Teaching\Master_FH\Stats\dist\Images'
outFn = 'chi2pp.png'
outFile = os.path.join(outDir, outFn)
print('Image saved to {0}'.format(outFile))
plt.savefig(outFile, dpi=200)
plt.show()