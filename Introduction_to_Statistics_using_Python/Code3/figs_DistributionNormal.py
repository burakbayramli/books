''' Simple manipulations of normal distribution functions.
- Different displays of normally distributed data
- Compare different samples from a normal distribution
- Work with the cumulative distribution function (CDF)

'''

# author: Thomas Haslwanter, date: Sept-2013

import numpy as np
import scipy.stats as stats
import matplotlib.pyplot as plt

myMean = 0
mySD = 3
x = np.arange(-5,15,0.1)

def simple_normal():
    ''' Different aspects of a normal distribution'''
    # Generate the data
    x = np.arange(-4,4,0.1) # generate the desirded x-values
    x2 = np.arange(0,1,0.001)

    nd = stats.norm()   # First simply define the normal distribution;
                        # don't calculate any values yet

    
    # This is a more complex plot-layout: the first row
    # is taken up completely by the PDF
    ax = plt.subplot2grid((3,2),(0,0), colspan=2)

    plt.plot(x,nd.pdf(x))
    plt.xlim([-4,4])
    plt.title('Normal Distribution - PDF')
    
    plt.subplot(323)
    plt.plot(x,nd.cdf(x))
    plt.xlim([-4,4])
    plt.title('CDF: cumulative distribution fct')
    
    plt.subplot(324)
    plt.plot(x,nd.sf(x))
    plt.xlim([-4,4])
    plt.title('SF: survival fct')
    
    plt.subplot(325)
    plt.plot(x2,nd.ppf(x2))
    plt.title('PPF')

    plt.subplot(326)
    plt.plot(x2,nd.isf(x2))
    plt.title('ISF')
    plt.tight_layout()
    plt.show()

def shifted_normal():
    '''PDF, scatter plot, and histogram.'''
    # Generate the data
    # Plot a normal distribution: "Probability density functions"
    myMean = 5
    mySD = 2
    y = stats.norm.pdf(x, myMean, mySD)
    plt.plot(x,y)
    plt.title('Shifted Normal Distribution')
    plt.show()
    
    # Generate random numbers with a normal distribution
    numData = 500
    data = stats.norm.rvs(myMean, mySD, size = numData)
    plt.plot(data, '.')
    plt.title('Normally distributed data')
    plt.show()
    
    plt.hist(data)
    plt.title('Histogram of normally distributed data')
    plt.show()
    plt.close()

def many_normals():
    '''Show multiple samples from the same distribution, and compare means.'''
    # Do this 25 times, and show the histograms
    numRows = 5
    numData = 50
    numData = 100
    plt.figure()
    for ii in range(numRows):
        for jj in range(numRows):
            data = stats.norm.rvs(myMean, mySD, size=numData)
            plt.subplot(numRows,numRows,numRows*ii+jj+1)
            plt.hist(data)
            plt.gca().set_xticklabels(())
            plt.gca().set_yticklabels(())
    
    plt.tight_layout()
    plt.show()
    plt.close()
    
    # Check out the mean of 1000 normally distributded samples
    numTrials = 1000;
    numData = 100
    myMeans = np.ones(numTrials)*np.nan
    for ii in range(numTrials):
        data = stats.norm.rvs(myMean, mySD, size=numData)
        myMeans[ii] = np.mean(data)
    print(('The standard error of the mean, with {0} samples, is {1}'.format(numData, np.std(myMeans))))

def values_fromCDF():
    '''Calculate an empirical cumulative distribution function, compare it with the exact one, and
    find the exact point for a specific data value.'''
    
    # Generate normally distributed random data
    myMean = 5
    mySD = 2
    numData = 100
    data = stats.norm.rvs(myMean, mySD, size=numData)
    
    # Calculate the cumulative distribution function, CDF
    numbins = 20
    counts, bin_edges = np.histogram(data, bins=numbins, normed=True)
    cdf = np.cumsum(counts)
    cdf /= np.max(cdf)
    
    # compare with the exact CDF
    plt.step(bin_edges[1:],cdf)
    plt.hold(True)
    plt.plot(x, stats.norm.cdf(x, myMean, mySD),'r')
    
    # Find out the value corresponding to the x-th percentile: the
    # "cumulative distribution function"
    value = 2
    myMean = 5
    mySD = 2
    cdf = stats.norm.cdf(value, myMean, mySD)
    print(('With a threshold of {0:4.2f}, you get {1}% of the data'.format(value, round(cdf*100))))
    
    # For the percentile corresponding to a certain value: 
    # the "inverse cumulative distribution function" 
    value = 0.025
    icdf = stats.norm.isf(value, myMean, mySD)
    print(('To get {0}% of the data, you need a threshold of {1:4.2f}.'.format((1-value)*100, icdf)))
    plt.show()

if __name__ == '__main__':
    simple_normal()
    shifted_normal()
    many_normals()
    values_fromCDF()

