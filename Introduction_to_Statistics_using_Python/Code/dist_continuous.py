''' Different continuous distribution functions.
- Normal distribution
- Exponential distribution
- T-distribution
- F-distribution
- Logistic distribution
- Weibull distribution
- Lognormal distribution
- Uniform distribution

'''

'''
Author:  Thomas Haslwanter
Date:    June-2013
Version: 1.5
'''

# Note: here I use the iPython approach, which is best suited for interactive work
from pylab import *
from scipy import stats
matplotlib.rcParams.update({'font.size': 18})

#----------------------------------------------------------------------
def showDistribution(x, d1, d2, tTxt, xTxt, yTxt, legendTxt, xmin=-10, xmax=10):
    '''Utility function to show the distributions, and add labels and title.'''
    plot(x, d1.pdf(x))
    if d2 != '':
        hold(True)
        plot(x, d2.pdf(x), 'r')
        legend(legendTxt)
        
    xlim(xmin, xmax)
    title(tTxt)
    xlabel(xTxt)
    ylabel(yTxt)
    show()
    close()
    

#----------------------------------------------------------------------
def show_continuous():
    """Show a variety of continuous distributions"""
        
    x = linspace(-10,10,201)
    
    # Normal distribution
    showDistribution(x, stats.norm, stats.norm(loc=2, scale=4),
                     'Normal Distribution', 'Z', 'P(Z)','')
    
    # Exponential distribution
    showDistribution(x, stats.expon, stats.expon(loc=-2, scale=4),
                     'Exponential Distribution', 'X', 'P(X)','')
    
    # Students' T-distribution
    # ... with 4, and with 10 degrees of freedom (DOF)
    plot(x, stats.norm.pdf(x), 'g')
    hold(True)
    showDistribution(x, stats.t(4), stats.t(10),
                     'T-Distribution', 'X', 'P(X)',['normal', 't=4', 't=10'])
    
    # F-distribution
    # ... with (3,4) and (10,15) DOF
    showDistribution(x, stats.f(3,4), stats.f(10,15),
                     'F-Distribution', 'F', 'P(F)',['(3,4) DOF', '(10,15) DOF'])
    
    # Weibull distribution
    # ... with the shape parameter set to 1 and 2
    # Don't worry that in Python it is called "weibull_min": the "weibull_max" is
    # simply mirrored about the origin.
    showDistribution(arange(0,5,0.02), stats.weibull_min(1), stats.weibull_min(2),
                     'Weibull Distribution', 'X', 'P(X)',['k=1', 'k=2'], xmin=0, xmax=4)
    
    # Uniform distribution
    showDistribution(x, stats.uniform,'' ,
                     'Uniform Distribution', 'X', 'P(X)','')
    
    # Logistic distribution
    showDistribution(x, stats.norm, stats.logistic,
                     'Logistic Distribution', 'X', 'P(X)',['Normal', 'Logistic'])
    
    # Lognormal distribution
    x = logspace(-9,1,1001)+1e-9
    showDistribution(x, stats.lognorm(2), '',
                     'Lognormal Distribution', 'X', 'lognorm(X)','', xmin=-0.1)
    
    # The log-lin plot has to be done by hand:
    plot(log(x), stats.lognorm.pdf(x,2))
    xlim(-10, 4)
    title('Lognormal Distribution')
    xlabel('log(X)')
    ylabel('lognorm(X)')
    show()
    
#----------------------------------------------------------------------
if __name__ == '__main__':
    show_continuous()