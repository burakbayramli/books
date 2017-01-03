'''
Short demo of how to check for the significance of a mean value.
'''

# author: Thomas Haslwanter, date: April-2014

import numpy as np
import matplotlib.pyplot as plt
from scipy import stats
import seaborn as sns
import os


# Get the data, and fit the normal distribution
weight = np.array([2784, 2632, 2771, 2495, 2435, 2513, 2633, 2737, 2687, 2647], dtype=np.float32)
(md, sd) = stats.norm.fit(weight)
nd = stats.norm(md, sd)

# Plot the data
sns.set_context(context='poster')

x = np.linspace(2300, 3000)
y = nd.pdf(x)

checkVal = 2460
print('p = {0:5.3f}'.format(nd.cdf(checkVal)))

x1 = np.linspace(2300, checkVal)
y1 = nd.pdf(x1)

sns.rugplot(weight, height=0.0005)
plt.hold(True)
plt.plot(x,y)
plt.fill_between(x1, y1, alpha=0.3)

outDir = r'C:\Users\p20529\Documents\Teaching\Master_FH\Stats\Images'
outFile = os.path.join(outDir, 'pdf_checkMean.png')
plt.savefig(outFile, dpi=200)
print('Figure saved to {0}'.format(outFile))

plt.show()
