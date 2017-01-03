""" TSintro.py <fine> <coarse> <quantized> <plot_file>
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
    name_fine, name_coarse, name_quantized, plot_file = argv

    read_data = lambda name: np.array([[float(x) for x in line.split()]
        for line in file(name, 'r').readlines()]).T

    fine = read_data(name_fine)
    coarse = read_data(name_coarse)
    quantized = read_data(name_quantized)

    params = {'axes.labelsize': 12,
                   'text.fontsize': 10,
                   'legend.fontsize': 10,
                   'text.usetex': True,
                   'xtick.labelsize': 11,
                   'ytick.labelsize': 11}
    mpl.rcParams.update(params)
    fig = plt.figure(figsize=(6,4))
    X = axis(data=fine[0], magnitude=False, label=r'$\tau$')
    Y = axis(data=fine[1], magnitude=False, ticks=np.arange(-10, 10.1, 10),
             label=r'$x_1(\tau)$')
    ax = SubPlot(fig,(2,1,1),X,Y, color='b')
    ax.plot(coarse[0], coarse[1],'ro')
    ax.set_ylim(-17, 17)
    ax.set_xlim(0,6)

    ax = fig.add_subplot(2,1,2)
    ax.plot(quantized[0],quantized[1],'kd')
    ax.set_xlabel(r'$t$')
    ax.set_ylabel(r'$y(t)$')
    ax.set_ylim(0.5, 4.5)
    ax.set_yticks(np.arange(1,4.1,1))
    ax.set_xticks(np.arange(0,40.1,10))
    fig.subplots_adjust(hspace=0.3)

    if DEBUG:
        plt.show()
    else:
        fig.savefig(plot_file)
    return 0

if __name__ == "__main__":
    sys.exit(main())
