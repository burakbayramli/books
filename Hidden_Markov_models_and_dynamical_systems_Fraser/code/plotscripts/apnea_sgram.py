"""
apnea_sgram.py a11_Rtimes bphr expert cinc_path sgram

A hacked version of old record_view.py.  I created this
script to make a spectrogram figure for the book.
"""

import sys
import numpy as np

LOW    = 0.06
HIGH   = 0.14
W      = 7

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
        description='''
    Make 4 figures illustrating apnea data.  Note: cinc2000 must be in
    PYTHONPATH''')
    parser.add_argument('a11_rtimes', help='path to rtimes/a11')
    parser.add_argument('a11_lphr', help='path to low_pass_heart_rate/a11')
    parser.add_argument('expert', help='path to summary_of_training')
    parser.add_argument('sgram', help='path to file sgram.pdf')
    args = parser.parse_args(argv)
    import cinc2000

    HR_t = []
    HR = []  # The heart rate
    for line in open(args.a11_lphr,'r').readlines():
        part = line.split()
        HR_t.append(float(part[0]))
        HR.append(float(part[2]))
        # time from first column, low pass heart rate from third
    expert = cinc2000.ReadIn(args.expert)
    Class_e = cinc2000.As_Ns_2_Nums(expert['a11'],val_N=-1,val_A=1)
    Class_t = np.arange(0,len(Class_e))
    #Fs = float(1)/6.0# samples per second
    # Next get data for respiration spectrogram
    data = []
    for line in open(args.a11_rtimes,'r').xreadlines():
        data.append(float(line)/100)
    x = cinc2000.R_times2Dev(data,w=1) # Heart Rate Deviations
    W = 6
    NFFT=2**W
    Fs = float(120)      # samples per minute
    First = 4800-32      # 120*40
    Last =  27000+40     # 120*225
    Rxx, Rfreqs, Rts = mpl.mlab.specgram(x[First:Last], NFFT=NFFT, Fs=Fs,
                  noverlap=NFFT/2)
    Rbins = Rts + First/Fs
    p = Rxx*Rxx
    p = p.sum(axis=0)
    p = 1/np.sqrt(p)
    Rxx = Rxx*p

    # Do the plotting
    fig = plt.figure(figsize=(10, 6)) # 2x Gnuplot size

    yrng = np.arange(-5,20,5)
    xrng = np.arange(50,250,50)
    ax1 = fig.add_subplot(3, 1, 1)
    ax1.plot(HR_t, HR)                 # Heart rate
    ax1.set_ylim(-10, 20)
    ax1.set_xlim(40, 225)
    ax1.set_yticks(yrng)
    ax1.set_yticklabels([ '$% 3.1f$' % x for x in yrng ])
    ax1.set_xticks(xrng)
    ax1.set_xticklabels([])
    
    # Plot respiration spectrogram
    ax2 = fig.add_subplot(3, 1, 2)

    Z = -10*np.log10(Rxx[:-10,:]) #FixMe: The yaxis is wrong
    Z =  np.flipud(Z)
    extent = np.amin(Rbins), np.amax(Rbins), np.amin(Rfreqs),\
        np.amax(Rfreqs)/1.8
    Z = Z - Z.min()
    vmin = LOW*Z.max()
    vmax = HIGH*Z.max()

    #ax2.imshow(Z, cmap=mpl.cm.hsv, extent=extent)
    ax2.imshow(Z, cmap=mpl.cm.hsv, aspect='auto')

    yrng = np.arange(0,35,10)
    ax2.set_yticks([])
    ax2.set_yticklabels([])
    #ax2.set_yticks(yrng)
    #ax2.set_yticklabels([ '$% 3.1f$' % x for x in yrng ])
    #ax2.set_xticks(xrng)
    ax2.set_xticklabels([])
    #ax2.set_xlim(40,225)
    #ax2.set_ylim(0,35)

    xrng = np.arange(50,250,50)
    yrng = np.arange(-1,2,2)
    ax3 = fig.add_subplot(3, 1, 3)
    ax3.plot(Class_t, Class_e)
    ax3.set_ylim(-1.2,1.2)
    ax3.set_xlim(40,225)
    ax3.set_yticks(yrng)
    ax3.set_yticklabels( ['$N$','$A$' ])
    ax3.set_xticks(xrng)
    ax3.set_xticklabels([ '$%# 3i$' % l for l in xrng ])

    fig.savefig(args.sgram)
    return 0

if __name__ == "__main__":
    sys.exit(main())

# Local Variables:
# mode: python
# End:
