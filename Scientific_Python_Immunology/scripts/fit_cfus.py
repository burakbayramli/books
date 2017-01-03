import pylab
import numpy
from scipy import optimize

def f(t, a, b, k, n):
    return a + b*(t**n)/(t**n + k**n)

def resid(p, y, t):
    a, b, k, n = p
    return y - f(t, a, b, k, n)

if __name__ == '__main__':
    # load the data
    t, x1, x2, d = numpy.loadtxt('cfu.txt', unpack=True)

    # transform the observed CFU values
    y1 = numpy.log10(x1*10**d)
    y2 = numpy.log10(x2*10**d)

    y = numpy.concatenate([y1, y2])

    a0, b0, k0, n0 = 1, 1, 1, 1

    [a, b, k, n], flag  = optimize.leastsq(resid, [a0, b0, k0, n0], 
                                           args=(y1, t))

    print flag, a, b, k, n

    # plot the data
    pylab.plot(t, y1, 'ro')

    # plot the smooth model fit
    ts = numpy.linspace(t[0], t[-1], 100)
    pylab.plot(ts, f(ts, a, b, k, n))

    pylab.title('Fitted log growth curves of E. coli')
    pylab.text(20, 6.8, 
               r'$f(t) = a +b \frac{t^n}{t^n + k^n}$'  ,
               fontsize=18)
    pylab.text(20, 6.5, 'a=%.2f, b=%.2f' % (a, b))
    pylab.text(20, 6.2, 'k=%.2f, n=%.2f' % (k, n))
    pylab.xlabel('Time (mins)')
    pylab.ylabel('$\log_{10}(\mathrm{CFUs})$')

    # show is necessary to display the plot when
    # not in interactive mode
    pylab.show()
