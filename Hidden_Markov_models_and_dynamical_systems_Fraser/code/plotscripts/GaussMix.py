"""
GaussMix.py em.pickle GaussMix.pdf

"""
DEBUG = False
import sys
import pickle
import numpy as np
from numpy.linalg import inv as LAI
from numpy.linalg import eigh as EIG

def main(argv=None):
    '''
    '''
    if sys.version_info < (3,0):
        import matplotlib as mpl
        if not DEBUG:
            mpl.use('PDF')
        import matplotlib.pyplot as plt
        from mpl_toolkits.mplot3d import Axes3D # for  "projection='3d'".
    else:
       print('%s needs matplotlib.  However, no matplotlib for python %s'%(
           sys.argv[0],sys.version_info,))
       return -1

    if argv is None:                    # Usual case
        argv = sys.argv[1:]

    dict_file, fig_name = argv

    params = {'axes.labelsize': 12,
                   'text.fontsize': 10,
                   'legend.fontsize': 10,
                   'text.usetex': True,
                   'xtick.labelsize': 11,
                   'ytick.labelsize': 11}
    if DEBUG:
        params['text.usetex'] = False
    mpl.rcParams.update(params)

    _dict = pickle.load(open(dict_file,'rb'))
    def subplot(i_label):
        x = np.arange(-6, 6, 0.05)
        def Gauss(mean, var):
            d = x - mean
            return (1/(np.sqrt(2*np.pi*var)))*np.exp(-d*d/(2*var))
        def mix(alpha, means):
            return alpha*Gauss(means[0], 1) + (1-alpha)*Gauss(means[1],1)
        for i, label in i_label:
            y = mix(_dict['alpha'][i], _dict['mu_i'][i])
            ax.plot(x, y, label=label)
        ax.legend()

    fig = plt.figure(figsize=(6, 5))
    ax = fig.add_subplot(2, 1, 1)
    subplot(((0,r'$\theta(1)$'),(-1,r'$\theta$')))

    ax = fig.add_subplot(2, 1, 2)
    subplot(((1,r'$\theta(2)$'),))
    x = _dict['Y']
    ax.plot(x, np.ones(len(x))*0.01, 'rd')

    if DEBUG:
        plt.show()
    else:
        fig.savefig(fig_name)
    return 0

if __name__ == "__main__":
    sys.exit(main())
# Local Variables:
# mode: python
# End:
