"""
ScalarGaussian.py SGO_sim plot_dir

Makes the following plots for fig:ScalarGaussian:

SGO_b.pdf  Simulated time series of states
SGO_c.pdf  Simulated time series of observations
SGO_d.pdf  Decoded time series of states

"""
DEBUG = False
import sys
def main(argv=None):
    '''

    '''
    import numpy as np
    from utilities import read_data, SubPlot, axis
    from os.path import join

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

    if argv is None:                    # Usual case
        argv = sys.argv[1:]
    sim_file, fig_dir = argv

    params = {'axes.labelsize': 18,     # Plotting parameters for latex
              'text.fontsize': 15,
              'legend.fontsize': 15,
              'text.usetex': True,
              'font.family':'serif',
              'font.serif':'Computer Modern Roman',
              'xtick.labelsize': 15,
              'ytick.labelsize': 15}
    mpl.rcParams.update(params)

    data = read_data(sim_file)
    X = axis(data=data[0], magnitude=False, label=r'$t$',
             ticks=np.arange(0, 100.1, 25))

    def _plot(Y):
        fig = plt.figure(figsize=(3.5,2.5))
        ax = SubPlot(fig,(1,1,1),X,Y, color='b')
        ax.set_ylim(-0.02, 1.02)
        fig.subplots_adjust(bottom=0.15) # Make more space for label
        fig.subplots_adjust(left=.15, bottom=.18)
        return (ax, fig)

    ax, fig_b = _plot(axis(data=data[1], magnitude=False, label=r'$S(t)$',
                         ticks=np.arange(0, 1.1, 1)))
    ax, fig_d = _plot(axis(data=data[3], magnitude=False, label=r'$S(t)$',
                           ticks=np.arange(0, 1.1, 1)))

    ax, fig_c = _plot(axis(data=data[2], magnitude=False, label=r'$y(t)$',
                         ticks=np.arange(-4, 4.1, 4)))
    ax.set_ylim(-5, 5)
    fig_c.subplots_adjust(left=.2)
    if DEBUG:
        plt.show()
    else:
        fig_b.savefig(join(fig_dir, 'SGO_b.pdf'))
        fig_c.savefig(join(fig_dir, 'SGO_c.pdf'))
        fig_d.savefig(join(fig_dir, 'SGO_d.pdf'))
    return 0

if __name__ == "__main__":
    sys.exit(main())

# Local Variables:
# mode: python
# End:
