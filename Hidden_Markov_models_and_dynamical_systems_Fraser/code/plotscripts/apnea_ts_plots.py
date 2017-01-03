""" apnea_ts_plots.py a03er_seg a03_lphr a01_lphr a12_lphr a03erA_plot
         a03erN_plot a03HR_plot ApneaNLD

This is the first line of the data file a03er_seg and the fields:

3000.01 -0.65 -4814.0 -4002.0  -56.0     70.0 
time     ECG                   1000*ONR  O2sat

This is the first line of the data file a03.lphr and the fields:

0.0   70.5882352941 2.88250231173
time  lphr          bandpass hr
"""
import sys
import numpy as np

minutes_seconds = lambda x:'$%1d\!:\!%02d$' % (x/60,x%60) # Latex time format

def read_data(data_file):
    '''Read in "data_file" as an array'''
    f = file(data_file, 'r')
    data = [[float(x) for x in line.split()] for line in f.xreadlines()]
    f.close()
    return np.array(data).T

def plot_a(plt, seg, start, stop, xbounds, xrng, out_name):
    '''For first two plots of Chapter 6: "a03erA_plot.pdf" and
    "a03erN_plot.pdf".'''
    fig = plt.figure() # figsize=(5,4)

    def subplot(location, y_data, y_label, xrng, ylim, yrng, yscale=1):
        ax = fig.add_subplot(*location)
        ax.plot(seg[0,start:stop]/60, y_data,'k-')
        ax.set_ylabel(y_label)
        ax.set_xticks(xrng)
        ax.set_xticklabels([ minutes_seconds(x) for x in xrng ])
        ax.set_yticks(np.array(yrng) * yscale)
        ax.set_yticklabels([ '$% 2.0f$' % x for x in yrng ])
        ax.set_ylim(ylim)
        ax.set_xlim(xbounds)
        return

    subplot((3,1,1), seg[1,start:stop]/1000, r'$ECG$', [], (-0.015, 0.035),
            np.arange(-10,31,10), 1e-3)
    subplot((3,1,2), seg[4,start:stop]/1000, r'$ONR$', [], (-15, 15),
            np.arange(-10,11,10))
    subplot((3,1,3), seg[5,start:stop], r'$SpO_2$', xrng, (55, 100),
            np.arange(60,101,15))
    fig.savefig(out_name)

    return

def plot_b(plt, seg, a03_lphr, out_name):
    '''For third figure in Chapter 6: "a03HR_plot.pdf"
    '''
    fig = plt.figure()
    xlim = (55, 65) # For both
    ylim = (40, 90) # For lphr only

    ax = fig.add_subplot(2,1,1)
    ax.plot(a03_lphr[0,:], a03_lphr[1,:],'k-')
    ax.set_ylabel(r'$HR$')
    yrng = np.arange(45,86,10)
    ax.set_yticks(yrng)
    ax.set_yticklabels(['$% 2.0f$' % x for x in yrng ])
    ax.set_xticks([])
    ax.set_ylim(ylim)
    ax.set_xlim(xlim)

    ax = fig.add_subplot(2,1,2)
    ax.plot(seg[0,:]/60, seg[5,:],'k-')
    ax.set_ylabel(r'$SpO_2$')
    xrng = np.arange(55,66,5)
    ax.set_xticks(xrng)
    ax.set_xticklabels([ minutes_seconds(x) for x in xrng ])
    yrng = np.arange(60,101,10)
    ax.set_yticks(yrng)
    ax.set_yticklabels([ '$% 2.0f$' % x for x in yrng ])
    ax.set_xlim(xlim)
    
    fig.savefig(out_name)
    return
def plot_c(plt, a01_lphr, a12_lphr, out_name):
    ''' For frourth figure in Chapter 6: "ApneaNLD.pdf".
    '''
    fig = plt.figure()
    
    ax = fig.add_subplot(2,1,1)
    ax.plot(a01_lphr[0,:],a01_lphr[1,:],'k-')
    ax.set_ylabel(r'$a01$HR')
    xrng = np.arange(115,126,5)
    ax.set_xticks(xrng)
    ax.set_xticklabels([ minutes_seconds(x) for x in xrng ])
    yrng = np.arange(40,101,20)
    ax.set_yticks(yrng)
    ax.set_yticklabels([ '$% 3.0f$' % x for x in yrng ])
    ax.set_ylim(40, 100)
    ax.set_xlim(115, 125)
    
    ax = fig.add_subplot(2,1,2)
    ax.plot(a12_lphr[0,:],a12_lphr[1,:],'k-')
    ax.set_ylabel(r'$a12$HR')
    xrng = np.arange(570,577,5)
    ax.set_xticks(xrng)
    ax.set_xticklabels([ minutes_seconds(x) for x in xrng ])
    yrng = np.arange(40,81,20)
    ax.set_yticks(yrng)
    ax.set_yticklabels([ '$% 3.0f$' % x for x in yrng ])
    ax.set_ylim(40, 80)
    ax.set_xlim(568, 577)
    
    fig.savefig(out_name)
    return

def main(argv=None):
    '''

    '''
    
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
        description='Make 4 figures illustrating apnea data')
    parser.add_argument('a03er_seg', help='path to a03er_seg')
    parser.add_argument('a03_lphr', help='path to low_pass_heart_rate/a03')
    parser.add_argument('a01_lphr', help='path to low_pass_heart_rate/a01')
    parser.add_argument('a12_lphr', help='path to low_pass_heart_rate/a12')
    parser.add_argument('a03erA_plot', help='path to file')
    parser.add_argument('a03erN_plot', help='path to file')
    parser.add_argument('a03HR_plot', help='path to file')
    parser.add_argument('ApneaNLD', help='path to file')
    args = parser.parse_args(argv)

    seg = read_data(args.a03er_seg)
    plot_a(plt, seg, 45000, 57005, (57.5, 59.5), (58, 59), args.a03erA_plot) 
    plot_a(plt, seg, 120000, 132001, (70, 72), (70, 71, 72), args.a03erN_plot) 
    plot_b(plt, seg, read_data(args.a03_lphr), args.a03HR_plot)
    plot_c(plt, read_data(args.a01_lphr), read_data(args.a12_lphr),
           args.ApneaNLD)

if __name__ == "__main__":
    sys.exit(main())

# Local Variables:
# mode: python
# End:
