""" ToyH.py: Creates Fig. 5.3 of the book

python ToyH.py Hsurvey HtauS ToyH.pdf
"""
Debug = False
import numpy as np

from PFsurvey import read_data

def skip_header(name):
    for line in open(name, 'r'):
        if line.startswith('#'):
            continue
        parts = line.split()
        if len(parts) == 0:
            continue
        yield(parts)
def read_rest(name):
    return np.array([[float(part) for part in parts]
                     for parts in skip_header(name)]).T
import sys
def main(argv=None):
    '''

    '''
    if sys.version_info < (3,0):
        import matplotlib as mpl
        if not Debug:
            mpl.use('PDF')
        import matplotlib.pyplot as plt
        from mpl_toolkits.mplot3d import Axes3D # for  "projection='3d'".
    else:
       print('%s needs matplotlib.  However, no matplotlib for python %s'%(
           sys.argv[0],sys.version_info,))
       return -1

    if argv is None:                    # Usual case
        argv = sys.argv[1:]

    survey_file, tauS_file, fig_name = argv

    data = read_rest(tauS_file)
    params = {'axes.labelsize': 12,
                   'text.fontsize': 10,
                   'legend.fontsize': 10,
                   'xtick.labelsize': 11,
                   'ytick.labelsize': 11,
                   'text.usetex': True,}
    if Debug:
        params['text.usetex'] = False
    mpl.rcParams.update(params)

    fig = plt.figure(figsize=(12, 4))
    ax = fig.add_subplot(1, 2, 2)
    ax.plot(data[0], data[1], 'rd', label=r'$\sigma_\epsilon=10^{-4}$')
    ax.plot(data[0], data[2], 'go', label=r'ridge')
    y = -(data[0]*0.906 + 0.959916)
    ax.plot(data[0], y, 'b', label=r'theory')
    ax.legend()
    ax.set_ylabel(r'$-\hat h$')
    ax.set_xlabel(r'$\tau_s$')

    ax = fig.add_subplot(1, 2, 1, projection='3d', azim=45, elev=25)
    ax.set_ylabel(r'$\tau_s$')
    ax.set_xlabel(r'$\log_{10}(\tilde \sigma_\epsilon)$')
    ax.set_zlabel(r'$-\hat h$')
    xs, ys, zs = read_data(survey_file)
    X, Y = np.meshgrid(xs, ys)
    surf = ax.plot_surface(X, Y, zs, rstride=1, cstride=1, cmap=mpl.cm.hsv,
                           linewidth=1)
    if Debug:
        plt.show()
    else:
        fig.savefig(fig_name)
    return 0

if __name__ == "__main__":
    sys.exit(main())
# Local Variables:
# mode: python
# End:
