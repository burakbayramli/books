import pylab
import numpy

if __name__ == '__main__':
    # load the data
    t, x1, x2, d = numpy.loadtxt('cfu.txt', unpack=True)

    # transform the observed CFU values
    y1 = numpy.log10(x1*10**d)
    y2 = numpy.log10(x2*10**d)

    # plot the data
    pylab.plot(t, y1, 'ro', t, y2, 'bs')
    pylab.title('Log growth curves of E. coli')
    pylab.xlabel('Time (mins0')
    pylab.ylabel('$\log_{10}(\mathrm{CFUs})$')

    # show is necessary to display the plot when
    # not in interactive mode
    pylab.show()
