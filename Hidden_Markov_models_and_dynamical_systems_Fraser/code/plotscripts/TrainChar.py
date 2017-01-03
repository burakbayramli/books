"""
TrainChar.py ../../derived_data/synthetic/TrainChar ../../figs/TrainChar.pdf 

"""
import sys
def main(argv=None):
    '''Call with arguments: data_file, fig_file

    '''
    import numpy as np
    if sys.version_info < (3,0):
        import matplotlib as mpl
        mpl.use('PDF')
        import matplotlib.pyplot as plt
    else:
       print('%s needs matplotlib.  However, no matplotlib for python %s'%(
           sys.argv[0],sys.version_info,))
       return -1

    if argv is None:                    # Usual case
        argv = sys.argv[1:]

    data_file, fig_name = argv
    params = {'axes.labelsize': 12,
                   'text.fontsize': 10,
                   'legend.fontsize': 10,
                   'text.usetex': True,
                   'xtick.labelsize': 11,
                   'ytick.labelsize': 11}
    mpl.rcParams.update(params)

    fig = plt.figure(figsize=(6, 3))
    ax = fig.add_subplot(1, 1, 1)
    ax.set_xlabel(r'$n$')
    #ax.set_xlim(-2, 9)
    ax.set_ylabel(r'$\frac{\log(P(y_1^T|\theta(n))}{T}$')
    #ax.set_ylim(1.4, 3.2)
    data = np.array([
        [float(part) for part in line.split() ] 
        for line in open(data_file, 'r')])
    n_max, n_seeds = data.shape
    for i in range(1,n_seeds):
        ax.semilogx(data[:,0]+1, data[:,i])
        #ax.plot(data[:,0], data[:,i])
    fig.subplots_adjust(bottom=0.15) # Make more space for label
    fig.savefig(fig_name)
    return 0

if __name__ == "__main__":
    sys.exit(main())
# Local Variables:
# mode: python
# End:
