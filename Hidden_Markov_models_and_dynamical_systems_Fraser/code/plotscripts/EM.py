""" EM.py: Creates Fig. 2.8 of the book

python EM.py outfile.pdf
"""
Debug = False
import numpy as np
import sys
def main(argv=None):
    global Debug
    if argv is None:                    # Usual case
        argv = sys.argv[1:]

    fig_name = argv[0]
    if fig_name is 'debug':
        Debug = True
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

    params = {'axes.labelsize': 12,
                   'text.fontsize': 10,
                   'legend.fontsize': 10,
                   'xtick.labelsize': 11,
                   'ytick.labelsize': 11,
                   'text.usetex': True,}
    if Debug:
        params['text.usetex'] = False
    mpl.rcParams.update(params)

    fig = plt.figure(figsize=(9, 5))
    ax = fig.add_subplot(1, 2, 1, projection='3d', azim=-109, elev=30)
    ax.set_xlabel(r'$\theta$')
    ax.set_ylabel(r"$\theta'$")
    ax.set_zlabel(r"$Q(\theta',\theta)$")
    xs = np.arange(0.1, 0.9, 0.05)
    ys = np.arange(0.2, 0.8, 0.05)
    n_x = len(xs)
    n_y = len(ys)
    zs = np.empty((n_x,n_y)).T
    for i in range(n_x):
        x = xs[i]
        for j in range(n_y):
            y = ys[j]
            zs[j,i] = (1+2*x)*np.log(y) + (1+2*(1-x))*np.log(1-y)
    ax.set_xticks(np.arange(0.2,0.8,.2))
    ax.set_yticks(np.arange(0.3,0.8,.2))
    X, Y = np.meshgrid(xs, ys)
    surf = ax.plot_surface(X, Y, zs, rstride=1, cstride=1, cmap=mpl.cm.hsv,
                           linewidth=1)
    ax = fig.add_subplot(1, 2, 2)
    x = np.arange(0, 1.1, 1)
    y = 0.25 + x/2.0
    ax.plot(x,x, label='slope 1 referece')
    ax.plot(x,y, label=r'$\cal{T}(\theta)$')
    ax.set_xlabel(r'$\theta$')
    ax.set_ylabel(r"$\cal{T}(\theta)$")
    ax.legend(loc='lower right')
    ticks = np.arange(0, 1.1, 0.25)
    ax.set_xticks(ticks)
    ax.set_yticks(ticks)
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
