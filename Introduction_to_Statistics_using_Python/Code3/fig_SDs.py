'''
Graphical display of data within 1,2,3 SD.
'''

# author: Thomas Haslwanter, date: April-2014

import numpy as np
import os
import matplotlib.pyplot as plt
from scipy import stats
import seaborn as sns

nd = stats.norm()
x = np.linspace(-3.5, 3.5,100)
x1 = np.linspace(-1,1)
x2 = np.linspace(-2,2)
x3 = np.linspace(-3,3)

y = nd.pdf(x)
y1 = nd.pdf(x1)
y2 = nd.pdf(x2)
y3 = nd.pdf(x3)

sns.set(context='poster')
sns.set_style('ticks')
fig, axs = plt.subplots(1,3, sharey=True)

def show_SD(axis, xi, yi, text):
    '''Show the area covered by 1/2/3 SDs'''
    
    fc = 'red'
    axis.plot(x,y)
    axis.fill_between(xi, yi, facecolor=fc)
    
    axis.text(0, 0.05, text, horizontalalignment='center', fontsize=25)
    axis.set_xlim([-3.5, 3.5])
    axis.set_ylim([-0.0, 0.5])
    sns.despine(ax=axis, left=True)
    axis.set_yticks([])

show_SD(axs[0], x1, y1, '68.3%')    
show_SD(axs[1], x2, y2, '95.4%')    
show_SD(axs[2], x3, y3, '99.7%')    

plt.tight_layout()
outDir = r'C:\Users\p20529\Documents\Teaching\Master_FH\Stats\Images'
outFile = os.path.join(outDir, 'area_SDs.png')
plt.savefig(outFile, dpi=200)
print('Figure saved to {0}'.format(outFile))
plt.show()
