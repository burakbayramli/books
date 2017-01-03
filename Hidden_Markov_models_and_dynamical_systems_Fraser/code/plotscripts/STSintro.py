""" STSintro.py <data> <plot_file>
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
    name_data, plot_file = argv

    data = np.array([[int(x) for x in line.split()]
                     for line in file(name_data, 'r').readlines()[:100]])

    params = {'axes.labelsize': 12,
                   'text.fontsize': 10,
                   'legend.fontsize': 10,
                   'text.usetex': True,
                   'xtick.labelsize': 11,
                   'ytick.labelsize': 11}
    mpl.rcParams.update(params)
    fig = plt.figure(figsize=(6,2))
    ax = fig.add_subplot(1,1,1)
    ax.plot(data[:,0], data[:,1],'kd')
    ax.set_xlabel(r'$t$')
    ax.set_ylabel(r'$s(t)$')
    ax.set_ylim(-.5, 12)
    ax.set_yticks(np.arange(0,12.1,2))
    ax.set_xticks(np.arange(0,100.1,20))

    if DEBUG:
        plt.show()
    else:
        print('Saving to %s'%plot_file)
        fig.savefig(plot_file)
    return 0

if __name__ == "__main__":
    sys.exit(main())
