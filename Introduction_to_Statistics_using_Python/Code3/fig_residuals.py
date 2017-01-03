'''Demo-plot of residuals to a best-fit line

'''

# author: Thomas Haslwanter, date: April-2014

from pylab import *

def main():
    # generate the data
    x = arange(10)
    np.random.seed(10)
    y = 3*x+2+20*np.random.rand(len(x))
    
    # determine the line-fit
    k,d = polyfit(x,y,1)
    yfit = k*x+d
    
    # plot the data
    scatter(x,y)
    hold(True)
    plot(x, yfit, 'r')
    for ii in range(len(x)):
        plot([x[ii], x[ii]], [yfit[ii], y[ii]], 'k')
    xlim((-0.1, 9.1))
    xlabel('X')
    ylabel('Y')
    savefig('residuals.png', dpi=200)
    show()

if __name__ == '__main__':
    main()
