"""
LDA.py mean AA AN C LDA1 LDA2

"""
import sys
def main(argv=None):
    '''

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

    params = {'axes.labelsize': 18,     # Plotting parameters for latex
              'text.fontsize': 15,
              'legend.fontsize': 15,
              'text.usetex': True,
              'font.family':'serif',
              'font.serif':'Computer Modern Roman',
              'xtick.labelsize': 15,
              'ytick.labelsize': 15}
    mpl.rcParams.update(params)

    if argv is None:                    # Usual case
        argv = sys.argv[1:]

    import argparse
    parser = argparse.ArgumentParser(
        description='''
    Make the figure that illustrates the Fisher linear discriminate analysis''')
    parser.add_argument('mean', help='path to mean.resp')
    parser.add_argument('AA', help='path to AA.resp')
    parser.add_argument('AN', help='path to AN.resp')
    parser.add_argument('C', help='path to C.resp')
    parser.add_argument('LDA1', help='path to file LDA1.pdf')
    parser.add_argument('LDA2', help='path to file LDA2.pdf')
    args = parser.parse_args(argv)

    def read_data(data_file):
        # Read in "data_file" as an array
        f = file(data_file, 'r')
        data = [[float(x) for x in line.split()] for line in f.xreadlines()]
        f.close()
        return np.array(data).T

    mean = read_data(args.mean)
    AA   = read_data(args.AA)
    AN   = read_data(args.AN)
    C    = read_data(args.C)

    # Plot LDA_1
    fig = plt.figure(figsize=(5, 5.5), dpi=80)

    ax1 = fig.add_subplot(2,1,1)  # Upper plot of LDA_1
    ax1.plot(mean[0],mean[1],'r-',mean[0],mean[2],'g-',mean[0],mean[3],'b-')
    ax1.legend((r'$\mu_C$',r'$\mu_N$',r'$\mu_A$'))
    ax1.set_xticks([])
    ax1.set_yticks([])
    ax1.set_yticks([])
    ax1.set_ylabel(r'PSD')
    ax1.set_xlim(0,250)

    ax2 = fig.add_subplot(2,1,2)
    # Plot two basis vectors.  Change sign to match fig in book
    ax2.plot(mean[0], -mean[4], 'r-', mean[0], -mean[5], 'b-') 
    ax2.legend((r'$v_1$',r'$v_2$'),loc='upper right')
    xrng = np.arange(0,250,80)
    ax2.set_xticks(xrng,  [ '$%# 2i$' % l for l in xrng/8 ])
    ax2.set_yticks([])
    ax2.set_xlabel(r'cpm')
    ax2.set_ylabel(r'PSD')
    ax2.set_xlim(0,250)

    fig.savefig(args.LDA1)

    # Plot LDA_2
    fig = plt.figure(figsize=(6, 15), dpi=200)
    box = [-0.9, 0.085, -0.45, 0.7]

    def subplot_b(location, x, y, color, key):
        ax = fig.add_subplot(*location)
        ax.plot(-y, -x, color)  # Change sign and order to match book fig
        ax.axis(box)
        ax.set_xticks([])
        ax.set_yticks([])
        ax.legend((key,), loc='lower right')
        return

    subplot_b((3, 1, 1), C[0],   C[1], 'r,', 'C')
    subplot_b((3, 1, 2), AN[0], AN[1], 'g,', 'N')
    subplot_b((3, 1, 3), AA[0], AA[1], 'b,', 'A')

    fig.savefig(args.LDA2)
    return 0

if __name__ == "__main__":
    sys.exit(main())

# Local Variables:
# mode: python
# End:
