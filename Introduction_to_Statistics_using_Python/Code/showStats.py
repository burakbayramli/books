''' Show different ways to present statistical data
This script is written in "MATLAB" or "ipython" style, to show how
best to use Python interactively.
Note than in ipython, the "show()" commands are automatically generated.
The examples contain:
- scatter plots
- histograms
- boxplots
- probplots
- cumulative density functions
- regression fits

'''

'''
Author: thomas haslwanter
Date:   April-2013
Version: 1.2
'''

# First, import the libraries that you are going to need. You could also do
# that later, but it is better style to do that at the beginning.

# pylab imports the numpy, scipy, and matplotlib.pyplot libraries into the
# current environment
from pylab import *

import scipy.stats as stats

def main():
    # Univariate data -------------------------
    # Generate data that are normally distributed
    x = randn(500)
    
    # Scatter plot
    plot(x,'.')
    title('Scatter Plot')
    xlabel('X')
    ylabel('Y')
    draw()
    show()
    
    # Histogram
    hist(x)
    xlabel('Data Values')
    ylabel('Frequency')
    title('Histogram, default settings')
    show()
    
    hist(x,25)
    xlabel('Data Values')
    ylabel('Frequency')
    title('Histogram, 25 bins')
    show()
    
    # Cumulative probability density
    numbins = 20
    cdf = stats.cumfreq(x,numbins)
    plot(cdf[0])
    xlabel('Data Values')
    ylabel('Cumulative Frequency')
    title('Cumulative probablity density function')
    show()
    
    # Boxplot
    # The error bars indiacte the range, and the box consists of the
    # first, second (middle) and third quartile
    boxplot(x)
    title('Boxplot')
    ylabel('Values')
    show()
    
    boxplot(x, vert=False)
    title('Boxplot, horizontal')
    xlabel('Values')
    show()
    
    # Check for normality
    _ = stats.probplot(x, plot=plt)
    title('Probplot - check for normality')
    show()
    
    # Bivariate data -------------------------
    
    # Generate data
    x = randn(200)
    y = 10+0.5*x+randn(len(x))
    
    # Scatter plot
    scatter(x,y)
    # This one is quite similar to "plot(x,y,'.')"
    title('Scatter plot of data')
    xlabel('X')
    ylabel('Y')
    show()
    
    # LineFit
    M = vstack((ones(len(x)), x)).T
    pars = linalg.lstsq(M,y)[0]
    intercept = pars[0]
    slope = pars[1]
    scatter(x,y)
    hold(True)
    plot(x, intercept + slope*x, 'r')
    show()
    
if __name__ == '__main__':
    main()