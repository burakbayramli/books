""" class1.py: Creates Fig. 6.8 of the book

python class1.py pass1_report pass1.pdf
"""
def read_data(data_file):
    rv = {}
    for line in open(data_file, 'r'):
        parts = line.split()
        key = parts[0][0]
        if key not in rv:
            rv[key] = {'llr':[], 'R':[]}
        rv[key]['llr'].append(float(parts[6]))
        rv[key]['R'].append(float(parts[8]))
    return rv

import sys
def main(argv=None):
    '''Call with arguments: report, fig_file

    report is a text file with lines like the following:

        x33 # Low    stat=  1.847 llr= -0.323 R=  2.008
        c05 # Low    stat=  1.707 llr= -0.353 R=  1.883
        a01 # High   stat=  4.777 llr=  4.427 R=  2.563

    fig_file is a path where this script writes the result.

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

    pass1_report, fig_name = argv
    data = read_data(pass1_report)
    params = {'axes.labelsize': 12,
                   'text.fontsize': 10,
                   'legend.fontsize': 10,
                   'text.usetex': True,
                   'xtick.labelsize': 11,
                   'ytick.labelsize': 11}
    mpl.rcParams.update(params)

    fig = plt.figure(figsize=(8, 4))
    ax = fig.add_subplot(1, 1, 1)
    ax.set_xlabel('$llr$')
    ax.set_xlim(-2, 9)
    ax.set_ylabel('$R$')
    ax.set_ylim(1.4, 3.2)
    sym = {'a':'rs', 'b':'go', 'c':'bD', 'x':'mx'}
    for key in ('a', 'b', 'c', 'x'):
            ax.plot(data[key]['llr'], data[key]['R'], sym[key], label=key)
    x = np.array([-2.0, 3.0])
    low_line = 1.82
    high_line = 2.6
    y = low_line-.5*x
    ax.plot(x, y, 'm-', label=r'$R+\frac{llr}{2}=%4.2f$'%low_line)
    y = high_line-.5*x
    ax.plot(x, y, 'k-', label=r'$R+\frac{llr}{2}=%4.2f$'%high_line)
    ax.legend(loc='lower right')
    fig.savefig(fig_name)
    return 0

if __name__ == "__main__":
    args = ['pass1_report', 'pass1.pdf']
    sys.exit(main())
# Local Variables:
# mode: python
# End:
