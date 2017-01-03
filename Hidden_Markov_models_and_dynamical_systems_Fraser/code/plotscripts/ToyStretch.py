"""
ToyStretch.py Save_Hview_T_118 Save_Hview_T_118 ToyStretch.pdf

"""
DEBUG = False
import sys
import numpy as np
from numpy.linalg import inv as LAI
from numpy.linalg import eigh as EIG
def read_FU(name):
    """ Read a Forecast and Update from the file with name "name".  The
    forecast is characterized by the mean "aM" and covariance "aSigma"
    and the update is characterized by mean "alphaM" and covariance
    "alphaSig".  Such files were written by Hview.py
    """
    lines = open(name,'r').readlines()
    def skip_to(pattern):
        for i in xrange(len(lines)):
            if lines[i].startswith(pattern):
                return i
        raise RuntimeError,'%s not found'%pattern
    def read_matrix(pattern,Ni,Nj):
        import re
        start = skip_to(pattern)
        matrix = np.matrix(np.empty((Ni,Nj)))
        for i in xrange(Ni):
            numbers = [float(s) for s in re.sub(r'[\]\[]*','',
                      lines[start+1+i]).split()]
            for j in xrange(Nj):
                matrix[i,j] = numbers[j]
        return matrix
    # These try/except pairs let me read files from either the new or
    # old version of Hview
    try:
        alphaM = read_matrix('alphaM',3,1)
        aM = read_matrix('aM',3,1)
    except:
        alphaM = read_matrix('alphaM',1,3).T
        aM = read_matrix('aM',1,3).T
    try:
        alphaSig = read_matrix('alphaSig',3,3)
    except:
        alphaSig = LAI(read_matrix('alphaSI',3,3))
    aSigma = read_matrix('aSigma',3,3)
    return (aM,aSigma,alphaM,alphaSig)

def ellipse(mu,cov):
    cov = np.mat(cov[::2,::2]) # drop component 1
    mu = np.array(mu).reshape((3,))[::2]
    v,Q = EIG(cov)
    R = np.mat(np.diag(np.sqrt(v)))
    R = Q*R*LAI(Q) # Square root of cov
    NC = 100.0     # Number of points on unit circle
    a = np.arange(0, 2*np.pi*(NC+1)/NC,2*np.pi/NC)
    C = np.mat(np.array([np.cos(a),np.sin(a)])) # A unit circle
    rv = (mu + (R*C).A.T).T
    assert rv.shape[0] == 2,'rv.shape=(%d,%d)'%rv.shape
    return rv

def ellipse_FU(name):
    """Project Forecast and Update onto x_0,x_2.  Calculate and return the
    following:

    ellipse_F  Level set for aM, aSigma
    ellipse_U  Level set for alphaM, alpha_sig
    x_center   Average of min and max of first component of ellipses
    D_x        Difference/2 between min and max of first component of ellipses
    y_center
    D_y

    """
    aM,aSigma,alphaM,alphaSig = read_FU(name)
    ellipse_F = ellipse(aM,aSigma)
    ellipse_U = ellipse(alphaM+aM,alphaSig)
    maxes = np.maximum(ellipse_F.max(1),ellipse_U.max(1))
    mins = np.minimum(ellipse_F.min(1),ellipse_U.min(1))
    x_center,y_center = (maxes+mins)/2
    D_x,D_y = (maxes-mins)/2 *1.2
    return (ellipse_F,ellipse_U,x_center,D_x,y_center,D_y)

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

    h_view1, h_view2, fig_name = argv

    params = {'axes.labelsize': 12,
                   'text.fontsize': 10,
                   'legend.fontsize': 10,
                   'text.usetex': True,
                   'xtick.labelsize': 11,
                   'ytick.labelsize': 11}
    if DEBUG:
        params['text.usetex'] = False
    mpl.rcParams.update(params)

    B_F,B_U,B_xc,B_Dx,B_yc,B_Dy = ellipse_FU(h_view1)
    C_F,C_U,C_xc,C_Dx,C_yc,C_Dy = ellipse_FU(h_view2)
    Dx = max(B_Dx,C_Dx)  # X_width of both plots
    Dy = max(B_Dy,C_Dy)  # Y_width of both plots

    def subplot(ax, _xc, _yc, _F, _U, title):
        ax.set_xlabel(r'$x_1$')
        ax.set_ylabel(r'$x_3$')
        ax.set_xlim(_xc-Dx, _xc+Dx)
        ax.set_ylim(_yc-Dy, _yc+Dy)
        ax.plot(_F[0], _F[1], label='forecast')
        ax.plot(_U[0], _U[1], label='update')
        ax.legend()
        ax.set_title(title)

    fig = plt.figure(figsize=(7, 3.5))
    fig.subplots_adjust(wspace=0.3)
    ax = fig.add_subplot(1, 2, 1)
    subplot(ax, B_xc, B_yc, B_F, B_U, r'\large $t=118$')

    ax = fig.add_subplot(1, 2, 2)
    subplot(ax, C_xc, C_yc, C_F, C_U, r'\large $t=119$')

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
