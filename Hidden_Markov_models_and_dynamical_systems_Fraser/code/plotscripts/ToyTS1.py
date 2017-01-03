""" ToyTS1.py <data> <plot_file>

data file starts and ends thusly:

100 5.5612 -0.012222908779999209 0.0117590206702 -2.70451277742
101 12.9101 0.0052938629000003345 0.0128238867425 -2.54454959833
102 0.8347 0.007067990016999981 0.0104177187696 -2.51725174077

198 -11.2558 -0.003250828600000588 0.0102573750968 -2.43237261732
199 -6.0091 0.018271883389999743 0.0102985824416 -3.09584002734
200 -5.1505 0.00017005156999960747 0.0100038581104 -2.39975621453

I guess the fields are:
t    y(t)    (y(t) - \mu(t))       \sigma(t)         \log(P(y(t)))
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
    fig = plt.figure(figsize=(6,4))
    x_ticks = np.arange(100,201,50)
    X = axis(data=data[0], magnitude=False, label=r'$t$', ticks=x_ticks)
    Y = axis(data=data[1], magnitude=False, ticks=np.arange(-20, 20.1, 20),
             label=r'$y(t)$')
    ax = SubPlot(fig,(3,1,1),X,Y, color='b')

    ax = fig.add_subplot(3,1,2)
    ax.plot(data[0],data[2],'g', label=r'$(y(t)-\mu_\gamma(t))$')
    ax.plot(data[0],data[3],'k', label=r'$\sigma_\gamma(t)$')
    ax.set_xticks(x_ticks)
    y_ticks = np.arange(-0.06,0.121,0.06)
    ax.set_yticks(y_ticks)
    ax.set_yticklabels([r'$%4.2f$'%x for x in y_ticks])
    ax.legend()

    Y = axis(data=data[4], magnitude=False, ticks=np.arange(-6, 2.1, 2),
             label=r'$\log(P_\gamma (y(t))$')
    ax = SubPlot(fig,(3,1,3),X,Y, color='r')

    fig.subplots_adjust(hspace=0.3)

    if DEBUG:
        plt.show()
    else:
        fig.savefig(plot_file)
    return 0

if __name__ == "__main__":
    sys.exit(main())
