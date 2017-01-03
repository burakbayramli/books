"""laser.py [options select plots] data_dir plot_dir

A script for making any of the plots from laser data.  Selections are
controlled by options.

LaserLP5
LaserLogLike
LaserStates
LaserForecast
LaserHist
"""

import sys
from utilities import axis, SubPlot
from os.path import join
import numpy as np
plot_dict = {} # Will hold dict of functions that make plots
DEBUG = False
def main(argv=None):
    '''

    '''
    
    if sys.version_info >= (3,0):
       print('%s needs matplotlib.  However, no matplotlib for python %s'%(
           sys.argv[0],sys.version_info,))
       return -1

    if argv is None:                    # Usual case
        argv = sys.argv[1:]

    import matplotlib as mpl
    import argparse
    global args, DEBUG, plot_dict, plt, mpl
    params = {'axes.labelsize': 18,     # Plotting parameters for latex
              'text.fontsize': 15,
              'legend.fontsize': 15,
              'text.usetex': True,
              'font.family':'serif',
              'font.serif':'Computer Modern Roman',
              'xtick.labelsize': 15,
              'ytick.labelsize': 15}
    mpl.rcParams.update(params)

    parser = argparse.ArgumentParser(
        description='Make figures based on laser data')
    parser.add_argument('--debug', action='store_true')
    parser.add_argument('--LaserLP5', action='store_true',
                        help='Plot data and simulation')
    parser.add_argument('--LaserLogLike', action='store_true',
                        help='Plot response surface')
    parser.add_argument('--LaserStates', action='store_true',
                        help='Plot EKF estimated state trajectory')
    parser.add_argument('--LaserForecast', action='store_true',
                        help='Plot data and forecast')
    parser.add_argument('--LaserHist', action='store_true',
                        help='Plot histogram')
    parser.add_argument('data_dir', help='path to data')
    parser.add_argument('plot_dir', help='path to plots')
    args = parser.parse_args(argv)
    if args.debug:
        DEBUG = True
        mpl.rcParams['text.usetex'] = False
    else:
        mpl.use('PDF')
    import matplotlib.pyplot as plt
    from mpl_toolkits.mplot3d import Axes3D # for  "projection='3d'".

    _dict = globals()
    for key in args.__dict__:
        if key not in _dict or args.__dict__[key] is False:
            continue
        if args.__dict__[key] == None:
            continue
        fig = _dict[key](args.data_dir)
        if not DEBUG:
            fig.savefig(join(args.plot_dir,key+'.pdf'), format='pdf')
    return 0

def read_data(data_file):
    '''Read in "data_file" as an array.  Works for the following files:

    LaserLP5:

    0 135 133.823664529
    1 117 114.787891788
    2 88 85.8037202823
    3 63 59.814626672

    LaserForecast:

    250 23 1824.28505463
    251 16 1814.39841305
    252 15 1814.3052228
    253 17 1814.30450808
    254 20 1814.30451917
    255 26 1814.30451917

    LaserHist:

    10 0
    11 0
    12 0
    13 0
    14 3
    15 13
    16 24

        LaserStates:

    14.4413979966 31.7358722806
    11.9093985754 25.416302322
    11.8732841716 25.3624990227
    11.8732436926 25.3632842243
    11.8732169865 25.3635914867

    Not LaserLogLike:

    6.95797327186 5.53910327186 -3666.88396793
    6.95797327186 5.54467020983 -3667.58603178
    6.95797327186 5.55023714779 -3668.28992706
    6.95797327186 5.55580408575 -3668.99568406
    6.95797327186 5.56137102371 -3669.70333923
    6.95797327186 5.56693796167 -3670.41291767
    6.95797327186 5.57250489963 -3671.12444826
    6.95797327186 5.5780718376 -3671.8379535
    6.95797327186 5.58363877556 -3672.55345755
    6.95797327186 5.58920571352 -3673.27097651
    6.95797327186 5.59477265148 -3673.99052837

    6.96496620983 5.53910327186 -3666.84950969
    6.96496620983 5.54467020983 -3667.55166188
    '''
    f = file(data_file, 'r')
    data = [[float(x) for x in line.split()] for line in f.xreadlines()]
    f.close()
    return np.array(data).T

def read_xyz(data_file):
    f = file(data_file, 'r')
    x_dict = {}
    y_dict = {}
    xs = []
    ys = []
    z_dict = {}
    for line in f.xreadlines():
        parts = line.split()
        if len(parts) != 3:
            continue
        x, y, z = tuple(float(x) for x in parts)
        ij = []
        for _dict, _list, w in ((x_dict, xs, x), (y_dict, ys, y)):
            if not _dict.has_key(w):
                _dict[w] = len(_dict)
                _list.append(w)
            ij.append(_dict[w])
        z_dict[tuple(ij)] = z
    zs = np.empty((len(xs),len(ys)))
    for ij, z in z_dict.items():
        i, j = ij
        zs[i,j] = z
    f.close()
    return np.array(xs), np.array(ys), zs
def LaserStates(data_dir):
    data = read_data(join(data_dir,'LaserStates'))
    fig = plt.figure(figsize=(6,6))
    X = axis(data=data[0], magnitude=False, label='$x_1$')
    Y = axis(data=data[1], magnitude=False, label='$x_3$')
    ax = SubPlot(fig,(1,1,1),X,Y, color='b', label='Laser')
    fig.subplots_adjust(bottom=0.12) # Make more space for label
    return fig

def LaserLP5(data_dir):
    data = read_data(join(data_dir,'LaserLP5'))
    fig = plt.figure(figsize=(7,5))
    X = axis(data=data[0], magnitude=False, label='t',
             ticks=np.arange(0,250,100))
    Y = axis(data=data[1], magnitude=False, label='y(t)',
             ticks=np.arange(50,256,100))
    ax = SubPlot(fig,(1,1,1),X,Y, color='b', label='Laser')
    ax.plot(X.get_data(), data[2], 'r:', label='Simulation')
    ax.legend()
    fig.subplots_adjust(bottom=0.12) # Make more space for label
    return fig

def LaserForecast(data_dir):
    data = read_data(join(data_dir,'LaserForecast'))
    fig = plt.figure(figsize=(7,5))
    X = axis(data=data[0], magnitude=False, label='t',
             ticks=np.arange(300,650,100))
    Y = axis(data=data[1], magnitude=False, label='y(t)',
             ticks=np.arange(50,256,100))
    ax = SubPlot(fig,(1,1,1),X,Y, color='b', label='Laser')
    ax.plot(X.get_data(), data[2], 'r:', label='Forecast')
    ax.legend()
    fig.subplots_adjust(bottom=0.12) # Make more space for label
    return fig

def LaserHist(data_dir):
    data = read_data(join(data_dir,'LaserHist'))
    fig = plt.figure(figsize=(7,5))
    ax = fig.add_subplot(1,1,1)
    ax.bar(data[0],data[1])
    ax.set_yticks(np.arange(0,25,10))
    ax.set_ylabel('Counts')
    ax.set_xlabel('$x$')
    ax.set_xticks([0,5,50,93,100])
    fig.subplots_adjust(bottom=0.12) # Make more space for label
    return fig

def LaserLogLike(data_dir):
    fig = plt.figure(figsize=(12,8))
    ax = fig.add_subplot(1, 1, 1, projection='3d', azim=-13, elev=20)
    ax.set_xlabel('$s$')
    ax.set_ylabel('$b$')
    ax.set_zlabel(r'$\log(P(y_1^{250}|\theta))$')
    xs, ys, zs = read_xyz(join(data_dir,'LaserLogLike'))
    X, Y = np.meshgrid(xs, ys)
    surf = ax.plot_surface(X, Y, zs, rstride=1, cstride=1, cmap=mpl.cm.hsv,
                           linewidth=1)
    ax.set_yticks(np.arange(0.358, 0.360, 0.002))
    ax.set_zticks(np.arange(-464.5, -463.0, 0.5))
    return fig

if __name__ == "__main__":
    rv = main()
    if DEBUG:
        plt.show()
    sys.exit(rv)

# Local Variables:
# mode: python
# End:
