""" PFsurvey.py: Creates Fig. 6.10 of the book

python PFsurvey.py PFdata PFplot
"""
Debug = False
import numpy as np
def read_data(data_file):
    z_dict = {}
    xs = set()
    ys = set()
    for line in open(data_file, 'r'):
        x, y, z = (float(p) for p in line.split())
        z_dict[(x,y)] = z
        xs.add(x)
        ys.add(y)
    xs = list(xs)
    xs.sort()
    xs = np.array(xs)
    n_x = len(xs)
    x_dict = dict((xs[i],i) for i in range(n_x))
    ys = list(ys)
    ys.sort()
    ys = np.array(ys)
    n_y = len(ys)
    y_dict = dict((ys[i],i) for i in range(n_y))

    zs = np.empty((n_y, n_x))
    zs[:,:] = np.NaN
    for xy, z in z_dict.items():
        x,y = xy
        zs[y_dict[y], x_dict[x]] = z
    return xs, ys, zs

import sys
def main(argv=None):
    '''Call with arguments: data, fig_file

    data is a text file with lines like the following:
        0.500  0.500  0.5024
        0.500  1.500  0.9196
        0.500  2.500  0.9280
        0.500  3.500  0.9272
        0.500  4.500  0.9274
        1.500  0.500  0.5480

    fig_file is a path where this script writes the result.

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

    data_file, fig_name = argv
    params = {'axes.labelsize': 12,
                   'text.fontsize': 10,
                   'legend.fontsize': 10,
                   'xtick.labelsize': 11,
                   'ytick.labelsize': 11,
                   'text.usetex': True,}
    if Debug:
        params['text.usetex'] = False
    mpl.rcParams.update(params)

    fig = plt.figure(figsize=(9, 6))
    #ax = fig.gca(projection='3d')
    ax = fig.add_subplot(1, 1, 1, projection='3d', azim=-10, elev=30)
    ax.set_xlabel('Power')
    ax.set_ylabel('Fudge')
    ax.set_zlabel('Frac. Right')
    #ax.set_ylim(1.4, 3.2)
    xs, ys, zs = read_data(data_file)
    X, Y = np.meshgrid(xs, ys)
    surf = ax.plot_surface(X, Y, zs, rstride=1, cstride=1, cmap=mpl.cm.hsv,
                           linewidth=1)
    #ax.set_zlim(.85, 0.9)
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
