""" like_lor.py: Creates Fig. 5.6 of the book (maybe less data)

"""
def read_data(data_file):
    rv = {'log_res':[], 'cross_entropy':[], 'n_states':[]}
    for line in open(data_file, 'r'):
        parts = line.split()
        rv['log_res'].append(float(parts[0]))
        rv['cross_entropy'].append(-float(parts[1]))
        rv['n_states'].append(int(parts[2]))
    return rv

import sys
def main(argv=None):
    '''Call with arguments: report, fig_file
     python like_lor.py H_study LikeLor.pdf

     Here is a sample from the data file:

     -1.00 -0.235  11488
     -0.50 -0.273   6244
      0.00 -0.302   3343

     The columns are log_2(resolution), -log_likelihood/step, n_states

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

    report, fig_name = argv
    data = read_data(report)
    params = {'axes.labelsize': 12,
                   'text.fontsize': 10,
                   'legend.fontsize': 10,
                   'text.usetex': True,
                   'xtick.labelsize': 11,
                   'ytick.labelsize': 11}
    mpl.rcParams.update(params)

    fig = plt.figure(figsize=(6, 3))
    ax = fig.add_subplot(1, 1, 1)
    ax.set_xlabel(r'$n_{\rm{states}}$')
    #ax.set_xlim(-2, 9)
    ax.set_ylabel(r'$\hat h/\rm{nats}$')
    #ax.set_ylim(1.4, 3.2)
    ax.semilogx(data['n_states'], data['cross_entropy'])
    fig.subplots_adjust(bottom=0.15) # Make more space for label
    fig.savefig(fig_name)
    return 0

if __name__ == "__main__":
    args = ['pass1_report', 'pass1.pdf']
    sys.exit(main())
# Local Variables:
# mode: python
# End:
