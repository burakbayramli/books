import numpy
import pylab

if __name__ == '__main__':
    x = numpy.loadtxt('data.txt')
    n, p = x.shape
    nrows = ncols = p

    xmin, xmax = numpy.min(x), numpy.max(x)

    for row in range(nrows):
        for col in range(ncols):
            pylab.subplot(nrows, ncols, row*ncols+col+1)
            if row==col:
                pylab.hist(x[:,row], bins=50, histtype='stepfilled')
            else:
                pylab.scatter(x[::10,row], x[::10,col], s=1,
                              c='blue', edgecolors='none')
                pylab.axis([xmin, xmax, xmin, xmax])
            pylab.xticks([]); pylab.yticks([])

    # pylab.show()
    pylab.savefig('001.pdf')
