''' Different discrete distribution functions.
- Binomial distribution
- Poisson distribution (PMF, CDF, and PPF)

'''

# author: Thomas Haslwanter, date: April-2013

# Note: here I use the modular approach, which is more appropriate for scripts
import matplotlib.pyplot as plt
import scipy.stats as stats
import numpy as np

#----------------------------------------------------------------------
def show_binomial():
    """Show an example of binomial distributions"""
    
    bd1 = stats.binom(20, 0.5)
    bd2 = stats.binom(20, 0.7)
    bd3 = stats.binom(40, 0.5)
    
    k = np.arange(40)
    plt.plot(k, bd1.pmf(k), 'o-b')
    plt.hold(True)
    plt.plot(k, bd2.pmf(k), 'd-r')
    plt.plot(k, bd3.pmf(k), 's-g')
    plt.title('Binomial distribition')
    plt.legend(['p=0.5 and n=20', 'p=0.7 and n=20', 'p=0.5 and n=40'])
    plt.xlabel('X')
    plt.ylabel('P(X)')
    plt.show()
    
#----------------------------------------------------------------------
def show_poisson():
    """Show different views of a Poisson distribution"""
    
    fig, ax = plt.subplots(3,1)
    
    k = np.arange(25)
    pd = stats.poisson(10)
    
    ax[0].plot(k, pd.pmf(k),'x-')
    ax[0].set_title('Poisson distribition')
    ax[0].set_xticklabels([])
    ax[0].set_ylabel('PMF (X)')
    
    ax[1].plot(k, pd.cdf(k))
    ax[1].set_xlabel('X')
    ax[1].set_ylabel('CDF (X)')
    
    y = np.linspace(0,1,100)
    ax[2].plot(y, pd.ppf(y))
    ax[2].set_xlabel('X')
    ax[2].set_ylabel('PPF (X)')
    
    plt.tight_layout()
    plt.show()
    
# -----------------------------------------------------------------------------------
if __name__ == '__main__':
    show_binomial()
    show_poisson()
