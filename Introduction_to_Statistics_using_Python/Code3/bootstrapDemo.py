''' Example of bootstrapping the confidence interval for the mean of a sample distribution
Since no bootstrapping is implemented in Python, this function requires
"bootstrap.py", which is included.

Note: The original scikits-bootstrapping module, which works only under
Python 2, is available from http://github.org/cgevans/scikits-bootstrap
   
'''

# author: Thomas Haslwanter, date: Jan-2014

import scipy as sp
import matplotlib.pyplot as plt
from scipy import stats
import bootstrap

def generate_data():
    # To get reproducable values, I provide a seed value
    sp.random.seed(987654321)   
    
    # Generate a non-normally distributed datasample
    data = stats.poisson.rvs(2, size=1000)
    
    # Show the data
    plt.plot(data, '.')
    plt.title('Non-normally distributed dataset: Press any key to continue')
    # plt.show()
    plt.waitforbuttonpress()
    plt.close()    
    
    return(data)
    
def calc_bootstrap(data):
    # --- >>> START stats <<< ---
    # Calculate the bootstrap
    CIs = bootstrap.ci(data=data, statfunction=sp.mean)
    # --- >>> STOP stats <<< ---
    
    # Print the data: the "*" turns the array CIs into a list
    print(('The conficence intervals for the mean are: {0} - {1}'.format(*CIs)))
    
    return CIs

if __name__ == '__main__':
    data = generate_data()
    calc_bootstrap(data)
    input('Done')
