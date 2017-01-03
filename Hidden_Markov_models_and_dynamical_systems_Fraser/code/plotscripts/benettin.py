""" benettin.py <data_file> <plot_file>
"""
DEBUG = False
import sys
def main(argv=None):
    '''

    '''
    import numpy as np
    if sys.version_info >= (3,0):
       print('%s needs matplotlib.  However, no matplotlib for python %s'%(
           sys.argv[0],sys.version_info,))
       return -1
    import matplotlib as mpl
    global DEBUG
    if DEBUG:
        mpl.rcParams['text.usetex'] = False
    else:
        mpl.use('PDF')
    import matplotlib.pyplot as plt
    from utilities import axis, SubPlot
    if argv is None:                    # Usual case
        argv = sys.argv[1:]
    data_file, plot_file = argv

    data = np.array([[float(x) for x in line.split()]
                     for line in file(data_file, 'r').readlines()]).T

    params = {'axes.labelsize': 12,
                   'text.fontsize': 10,
                   'legend.fontsize': 10,
                   'text.usetex': True,
                   'xtick.labelsize': 11,
                   'ytick.labelsize': 11}
    mpl.rcParams.update(params)
    fig = plt.figure(figsize=(6,6))
    X = axis(data=data[0], magnitude=False)
    Y = axis(data=data[5], magnitude=False, ticks=np.arange(0.6, 1.41, 0.2),
             label=r'$\lambda(t)$')
    ax = SubPlot(fig,(2,1,1),X,Y, label=r'95\%', color='b')
    ax.plot(data[0], data[2], 'r', label=r'sample')
    ax.plot(data[0], data[3], 'r', label=r'sample')
    ax.plot(data[0], data[1], 'r', label=r'sample')
    ax.plot(data[0], data[4], 'b', label=r'5\%')
    ax.set_ylim(0.5, 1.5)
    ax.legend()
    ax.yaxis.tick_right()
    ax.yaxis.set_label_position("right")

    X = axis(data=data[0], magnitude=False, label=r'$t$')
    Y = axis(data=data[10], magnitude=False, ticks=np.arange(0.6, 1.41, 0.2),
             label=r'$\lambda(t)$')
    ax = SubPlot(fig,(2,1,2),X,Y, label=r'95\%', color='b')
    ax.plot(data[0], data[6], 'r', label=r'sample')
    ax.plot(data[0], data[7], 'r', label=r'sample')
    ax.plot(data[0], data[8], 'r', label=r'sample')
    ax.plot(data[0], data[9],'b', label=r'5\%')
    ax.set_ylim(0.5, 1.5)
    ax.legend()
    ax.yaxis.tick_right()
    ax.yaxis.set_label_position("right")

    if DEBUG:
        plt.show()
    else:
        fig.savefig(plot_file)
    return 0

if __name__ == "__main__":
    sys.exit(main())
