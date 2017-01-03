'''
Practical demonstration of the central limit theorem

'''

# author: Thomas Haslwanter, date: March-2014

import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import os

def main():
    '''Demonstrate central limit theorem.'''
    # Generate data
    ndata = 1e5
    nbins = 50
    data = np.random.random(ndata)
    
    # Show them
    fig, axs = plt.subplots(1,3)
    sns.set(context='talk')
    
    axs[0].hist(data,bins=nbins)
    axs[0].set_title('Random data')
    axs[0].set_ylabel('Counts')
    
    axs[1].hist( np.mean(data.reshape((ndata/2,2)),  axis=1), bins=nbins)
    axs[1].set_title(' Average over 2')
    
    axs[2].hist( np.mean(data.reshape((ndata/10,10)),axis=1), bins=nbins)
    axs[2].set_title(' Average over 10')
    
    curDir = os.path.abspath(os.path.curdir)
    outFile = 'CentralLimitTheorem.png'
    plt.savefig(outFile, dpi=200)
    print('Data written to {0}'.format(os.path.join(curDir, outFile)))
    
    plt.show()    
    
if __name__ == '__main__':
   main() 
