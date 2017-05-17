"""
Basic IIR Bilinear Transform-Based Digital Filter Design Helper

Mark Wickert October 2016

Development continues!
"""

"""
This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
"""

import numpy as np
import scipy.signal as signal
import matplotlib.pyplot as plt
from matplotlib import pylab
from matplotlib import mlab 


def IIR_lpf(f_pass, f_stop, Ripple_pass, Atten_stop, 
            fs = 1.00, ftype = 'butter'):
    """
    Design an IIR lowpass filter using scipy.signal.iirdesign. 
    The filter order is determined based on 
    f_pass Hz, f_stop Hz, and the desired stopband attenuation
    d_stop in dB, all relative to a sampling rate of fs Hz.

    Parameters
    ----------
    f_pass : Passband critical frequency in Hz
    f_stop : Stopband critical frequency in Hz
    Ripple_pass : Filter gain in dB at f_pass
    Atten_stop : Filter attenuation in dB at f_stop
    fs : Sampling rate in Hz
    ftype : Analog prototype from 'butter' 'cheby1', 'cheby2',
            'ellip', and 'bessel'

    Returns
    -------
    b : ndarray of the numerator coefficients
    a : ndarray of the denominator coefficients
    sos : 2D ndarray of second-order section coefficients

    Notes
    -----
    Additionally a text string telling the user the filter order is
    written to the console, e.g., IIR cheby1 order = 8.

    Examples
    --------
    >>> fs = 48000
    >>> f_pass = 5000
    >>> f_stop = 8000
    >>> b_but,a_but,sos_but = IIR_lpf(f_pass,f_stop,0.5,60,fs,'butter')
    >>> b_cheb1,a_cheb1,sos_cheb1 = IIR_lpf(f_pass,f_stop,0.5,60,fs,'cheby1')
    >>> b_cheb2,a_cheb2,sos_cheb2 = IIR_lpf(f_pass,f_stop,0.5,60,fs,'cheby2')
    >>> b_elli,a_elli,sos_elli = IIR_lpf(f_pass,f_stop,0.5,60,fs,'ellip')


    Mark Wickert October 2016
    """
   
    b,a = signal.iirdesign(2*float(f_pass)/fs, 2*float(f_stop)/fs,
                           Ripple_pass, Atten_stop,
                           ftype = ftype, output='ba')
    sos = signal.iirdesign(2*float(f_pass)/fs, 2*float(f_stop)/fs,
                           Ripple_pass, Atten_stop,
                           ftype = ftype, output='sos')
    tag = 'IIR ' + ftype + ' order'
    print('%s = %d.' % (tag,len(a)-1))
    return b, a, sos


def IIR_hpf(f_stop, f_pass, Ripple_pass, Atten_stop, 
            fs = 1.00, ftype = 'butter'):
    """
    Design an IIR highpass filter using scipy.signal.iirdesign. 
    The filter order is determined based on 
    f_pass Hz, f_stop Hz, and the desired stopband attenuation
    d_stop in dB, all relative to a sampling rate of fs Hz.

    Parameters
    ----------
    f_stop : 
    f_pass : 
    Ripple_pass : 
    Atten_stop : 
    fs : sampling rate in Hz
    ftype : Analog prototype from 'butter' 'cheby1', 'cheby2',
            'ellip', and 'bessel'

    Returns
    -------
    b : ndarray of the numerator coefficients
    a : ndarray of the denominator coefficients
    sos : 2D ndarray of second-order section coefficients

    Examples
    --------
    >>> fs = 48000
    >>> f_pass = 8000
    >>> f_stop = 5000
    >>> b_but,a_but,sos_but = IIR_hpf(f_stop,f_pass,0.5,60,fs,'butter')
    >>> b_cheb1,a_cheb1,sos_cheb1 = IIR_hpf(f_stop,f_pass,0.5,60,fs,'cheby1')
    >>> b_cheb2,a_cheb2,sos_cheb2 = IIR_hpf(f_stop,f_pass,0.5,60,fs,'cheby2')
    >>> b_elli,a_elli,sos_elli = IIR_hpf(f_stop,f_pass,0.5,60,fs,'ellip')

    Mark Wickert October 2016
    """
   
    b,a = signal.iirdesign(2*float(f_pass)/fs, 2*float(f_stop)/fs,
                           Ripple_pass, Atten_stop,
                           ftype = ftype, output='ba')
    sos = signal.iirdesign(2*float(f_pass)/fs, 2*float(f_stop)/fs,
                           Ripple_pass, Atten_stop,
                           ftype =ftype, output='sos')
    tag = 'IIR ' + ftype + ' order'
    print('%s = %d.' % (tag,len(a)-1))
    return b, a, sos


def IIR_bpf(f_stop1, f_pass1, f_pass2, f_stop2, Ripple_pass, Atten_stop, 
            fs = 1.00, ftype = 'butter'):
    """
    Design an IIR bandpass filter using scipy.signal.iirdesign. 
    The filter order is determined based on 
    f_pass Hz, f_stop Hz, and the desired stopband attenuation
    d_stop in dB, all relative to a sampling rate of fs Hz.

    Parameters
    ----------
    f_stop1 : ndarray of the numerator coefficients
    f_pass : ndarray of the denominator coefficients
    Ripple_pass : 
    Atten_stop : 
    fs : sampling rate in Hz
    ftype : Analog prototype from 'butter' 'cheby1', 'cheby2',
            'ellip', and 'bessel'

    Returns
    -------
    b : ndarray of the numerator coefficients
    a : ndarray of the denominator coefficients
    sos : 2D ndarray of second-order section coefficients

    Examples
    --------
    >>> fs = 48000
    >>> f_pass = 8000
    >>> f_stop = 5000
    >>> b_but,a_but,sos_but = IIR_hpf(f_stop,f_pass,0.5,60,fs,'butter')
    >>> b_cheb1,a_cheb1,sos_cheb1 = IIR_hpf(f_stop,f_pass,0.5,60,fs,'cheby1')
    >>> b_cheb2,a_cheb2,sos_cheb2 = IIR_hpf(f_stop,f_pass,0.5,60,fs,'cheby2')
    >>> b_elli,a_elli,sos_elli = IIR_hpf(f_stop,f_pass,0.5,60,fs,'ellip')

    Mark Wickert October 2016
    """
   
    b,a = signal.iirdesign([2*float(f_pass1)/fs, 2*float(f_pass2)/fs],
                           [2*float(f_stop1)/fs, 2*float(f_stop2)/fs],
                           Ripple_pass, Atten_stop,
                           ftype = ftype, output='ba')
    sos = signal.iirdesign([2*float(f_pass1)/fs, 2*float(f_pass2)/fs],
                           [2*float(f_stop1)/fs, 2*float(f_stop2)/fs],
                           Ripple_pass, Atten_stop,
                           ftype =ftype, output='sos')
    tag = 'IIR ' + ftype + ' order'
    print('%s = %d.' % (tag,len(a)-1))
    return b, a, sos

def IIR_bsf(f_pass1, f_stop1, f_stop2, f_pass2, Ripple_pass, Atten_stop, 
            fs = 1.00, ftype = 'butter'):
    """
    Design an IIR bandstop filter using scipy.signal.iirdesign. 
    The filter order is determined based on 
    f_pass Hz, f_stop Hz, and the desired stopband attenuation
    d_stop in dB, all relative to a sampling rate of fs Hz.

    Mark Wickert October 2016
    """
   
    b,a = signal.iirdesign([2*float(f_pass1)/fs, 2*float(f_pass2)/fs],
                           [2*float(f_stop1)/fs, 2*float(f_stop2)/fs],
                           Ripple_pass, Atten_stop,
                           ftype = ftype, output='ba')
    sos = signal.iirdesign([2*float(f_pass1)/fs, 2*float(f_pass2)/fs],
                           [2*float(f_stop1)/fs, 2*float(f_stop2)/fs],
                           Ripple_pass, Atten_stop,
                           ftype =ftype, output='sos')
    tag = 'IIR ' + ftype + ' order'
    print('%s = %d.' % (tag,len(a)-1))
    return b, a, sos

def freqz_resp_list(b,a=np.array([1]),mode = 'dB',fs=1.0,Npts = 1024,fsize=(6,4)):
    """
    A method for displaying digital filter frequency response magnitude,
    phase, and group delay. A plot is produced using matplotlib

    freq_resp(self,mode = 'dB',Npts = 1024)

    A method for displaying the filter frequency response magnitude,
    phase, and group delay. A plot is produced using matplotlib

    freqz_resp(b,a=[1],mode = 'dB',Npts = 1024,fsize=(6,4))

        b = ndarray of numerator coefficients
        a = ndarray of denominator coefficents
     mode = display mode: 'dB' magnitude, 'phase' in radians, or 
            'groupdelay_s' in samples and 'groupdelay_t' in sec, 
            all versus frequency in Hz
     Npts = number of points to plot; default is 1024
    fsize = figure size; defult is (6,4) inches

    Mark Wickert, January 2015
    """
    if type(b) == list:
        # We have a list of filters
        N_filt = len(b)
    f = np.arange(0,Npts)/(2.0*Npts)
    for n in range(N_filt):
        w,H = signal.freqz(b[n],a[n],2*np.pi*f)
        if n == 0:
            plt.figure(figsize=fsize)
        if mode.lower() == 'db':
            plt.plot(f*fs,20*np.log10(np.abs(H)))
            if n == N_filt-1:
                plt.xlabel('Frequency (Hz)')
                plt.ylabel('Gain (dB)')
                plt.title('Frequency Response - Magnitude')

        elif mode.lower() == 'phase':
            plt.plot(f*fs,np.angle(H))
            if n == N_filt-1:
                plt.xlabel('Frequency (Hz)')
                plt.ylabel('Phase (rad)')
                plt.title('Frequency Response - Phase')

        elif (mode.lower() == 'groupdelay_s') or (mode.lower() == 'groupdelay_t'):
            """
            Notes
            -----

            Since this calculation involves finding the derivative of the
            phase response, care must be taken at phase wrapping points 
            and when the phase jumps by +/-pi, which occurs when the 
            amplitude response changes sign. Since the amplitude response
            is zero when the sign changes, the jumps do not alter the group 
            delay results.
            """
            theta = np.unwrap(np.angle(H))
            # Since theta for an FIR filter is likely to have many pi phase
            # jumps too, we unwrap a second time 2*theta and divide by 2
            theta2 = np.unwrap(2*theta)/2.
            theta_dif = np.diff(theta2)
            f_diff = np.diff(f)
            Tg = -np.diff(theta2)/np.diff(w)
            # For gain almost zero set groupdelay = 0
            idx = pylab.find(20*np.log10(H[:-1]) < -400)
            Tg[idx] = np.zeros(len(idx))
            max_Tg = np.max(Tg)
            #print(max_Tg)
            if mode.lower() == 'groupdelay_t':
                max_Tg /= fs
                plt.plot(f[:-1]*fs,Tg/fs)
                plt.ylim([0,1.2*max_Tg])
            else:
                plt.plot(f[:-1]*fs,Tg)
                plt.ylim([0,1.2*max_Tg])
            if n == N_filt-1:
                plt.xlabel('Frequency (Hz)')
                if mode.lower() == 'groupdelay_t':
                    plt.ylabel('Group Delay (s)')
                else:
                    plt.ylabel('Group Delay (samples)')
                plt.title('Frequency Response - Group Delay')
        else:
            s1 = 'Error, mode must be "dB", "phase, '
            s2 = '"groupdelay_s", or "groupdelay_t"'
            print(s1 + s2)


def freqz_cas(sos,w):
    """
    Cascade frequency response
    
    Mark Wickert October 2016
    """
    Ns,Mcol = sos.shape
    w,Hcas = signal.freqz(sos[0,:3],sos[0,3:],w)
    for k in range(1,Ns):
        w,Htemp = signal.freqz(sos[k,:3],sos[k,3:],w)
        Hcas *= Htemp
    return w, Hcas


def freqz_resp_cas_list(sos,mode = 'dB',fs=1.0,Npts = 1024,fsize=(6,4)):
    """
    A method for displaying cascade digital filter form frequency response 
    magnitude, phase, and group delay. A plot is produced using matplotlib

    freq_resp(self,mode = 'dB',Npts = 1024)

    A method for displaying the filter frequency response magnitude,
    phase, and group delay. A plot is produced using matplotlib

    freqz_resp(b,a=[1],mode = 'dB',Npts = 1024,fsize=(6,4))

        b = ndarray of numerator coefficients
        a = ndarray of denominator coefficents
     mode = display mode: 'dB' magnitude, 'phase' in radians, or 
            'groupdelay_s' in samples and 'groupdelay_t' in sec, 
            all versus frequency in Hz
     Npts = number of points to plot; default is 1024
    fsize = figure size; defult is (6,4) inches

    Mark Wickert, January 2015
    """
    if type(sos) == list:
        # We have a list of filters
        N_filt = len(sos)
    f = np.arange(0,Npts)/(2.0*Npts)
    for n in range(N_filt):
        w,H = freqz_cas(sos[n],2*np.pi*f)
        if n == 0:
            plt.figure(figsize=fsize)
        if mode.lower() == 'db':
            plt.plot(f*fs,20*np.log10(np.abs(H)))
            if n == N_filt-1:
                plt.xlabel('Frequency (Hz)')
                plt.ylabel('Gain (dB)')
                plt.title('Frequency Response - Magnitude')

        elif mode.lower() == 'phase':
            plt.plot(f*fs,np.angle(H))
            if n == N_filt-1:
                plt.xlabel('Frequency (Hz)')
                plt.ylabel('Phase (rad)')
                plt.title('Frequency Response - Phase')

        elif (mode.lower() == 'groupdelay_s') or (mode.lower() == 'groupdelay_t'):
            """
            Notes
            -----

            Since this calculation involves finding the derivative of the
            phase response, care must be taken at phase wrapping points 
            and when the phase jumps by +/-pi, which occurs when the 
            amplitude response changes sign. Since the amplitude response
            is zero when the sign changes, the jumps do not alter the group 
            delay results.
            """
            theta = np.unwrap(np.angle(H))
            # Since theta for an FIR filter is likely to have many pi phase
            # jumps too, we unwrap a second time 2*theta and divide by 2
            theta2 = np.unwrap(2*theta)/2.
            theta_dif = np.diff(theta2)
            f_diff = np.diff(f)
            Tg = -np.diff(theta2)/np.diff(w)
            # For gain almost zero set groupdelay = 0
            idx = pylab.find(20*np.log10(H[:-1]) < -400)
            Tg[idx] = np.zeros(len(idx))
            max_Tg = np.max(Tg)
            #print(max_Tg)
            if mode.lower() == 'groupdelay_t':
                max_Tg /= fs
                plt.plot(f[:-1]*fs,Tg/fs)
                plt.ylim([0,1.2*max_Tg])
            else:
                plt.plot(f[:-1]*fs,Tg)
                plt.ylim([0,1.2*max_Tg])
            if n == N_filt-1:
                plt.xlabel('Frequency (Hz)')
                if mode.lower() == 'groupdelay_t':
                    plt.ylabel('Group Delay (s)')
                else:
                    plt.ylabel('Group Delay (samples)')
                plt.title('Frequency Response - Group Delay')
        else:
            s1 = 'Error, mode must be "dB", "phase, '
            s2 = '"groupdelay_s", or "groupdelay_t"'
            print(s1 + s2)


def unique_cpx_roots(rlist,tol = 0.001):
    """
    
    The average of the root values is used when multiplicity 
    is greater than one.

    Mark Wickert October 2016
    """
    uniq = [rlist[0]]
    mult = [1]
    for k in range(1,len(rlist)):
        N_uniq = len(uniq)
        for m in range(N_uniq):
            if abs(rlist[k]-uniq[m]) <= tol:
                mult[m] += 1
                uniq[m] = (uniq[m]*(mult[m]-1) + rlist[k])/float(mult[m])
                break
        uniq = np.hstack((uniq,rlist[k]))
        mult = np.hstack((mult,[1]))
    return np.array(uniq), np.array(mult)


def sos_cascade(sos1,sos2):
    """
    
    Mark Wickert October 2016
    """
    return np.vstack((sos1,sos2))


def sos_zplane(sos,auto_scale=True,size=2,tol = 0.001):
    """
    Create an z-plane pole-zero plot.

    Create an z-plane pole-zero plot using the numerator
    and denominator z-domain system function coefficient
    ndarrays b and a respectively. Assume descending powers of z.

    Parameters
    ----------
    sos : ndarray of the sos coefficients
    auto_scale : bool (default True)
    size : plot radius maximum when scale = False

    Returns
    -------
    (M,N) : tuple of zero and pole counts + plot window
    
    Notes
    -----
    This function tries to identify repeated poles and zeros and will 
    place the multiplicity number above and to the right of the pole or zero.
    The difficulty is setting the tolerance for this detection. Currently it
    is set at 1e-3 via the function signal.unique_roots.

    Examples
    --------
    >>> # Here the plot is generated using auto_scale
    >>> sos_zplane(sos)
    >>> # Here the plot is generated using manual scaling
    >>> sos_zplane(sos,False,1.5)
    """
    Ns,Mcol = sos.shape
    # Extract roots from sos num and den removing z = 0
    # roots due to first-order sections
    N_roots = []
    for k in range(Ns):
        N_roots_tmp = np.roots(sos[k,:3])
        if N_roots_tmp[1] == 0.:
            N_roots = np.hstack((N_roots,N_roots_tmp[0]))
        else:
            N_roots = np.hstack((N_roots,N_roots_tmp))
    D_roots = []
    for k in range(Ns):
        D_roots_tmp = np.roots(sos[k,3:])
        if D_roots_tmp[1] == 0.:
            D_roots = np.hstack((D_roots,D_roots_tmp[0]))
        else:
            D_roots = np.hstack((D_roots,D_roots_tmp))
    # Plot labels if multiplicity greater than 1
    x_scale = 1.5*size
    y_scale = 1.5*size   
    x_off = 0.02
    y_off = 0.01
    M = len(N_roots)
    N = len(D_roots)
    if auto_scale:
        if M > 0 and N > 0:
            size = max(np.max(np.abs(N_roots)),np.max(np.abs(D_roots)))+.1
        elif M > 0:
            size = max(np.max(np.abs(N_roots)),1.0)+.1
        elif N > 0:
            size = max(1.0,np.max(np.abs(D_roots)))+.1
        else:
            size = 1.1
    plt.figure(figsize=(5,5))
    plt.axis('equal')
    r = np.linspace(0,2*np.pi,200)
    plt.plot(np.cos(r),np.sin(r),'r--')
    plt.plot([-size,size],[0,0],'k-.')
    plt.plot([0,0],[-size,size],'k-.')
    if M > 0:
        #N_roots = np.roots(b)
        N_uniq, N_mult=unique_cpx_roots(N_roots,tol=tol)
        plt.plot(np.real(N_uniq),np.imag(N_uniq),'ko',mfc='None',ms=8)
        idx_N_mult = mlab.find(N_mult>1)
        for k in range(len(idx_N_mult)):
            x_loc = np.real(N_uniq[idx_N_mult[k]]) + x_off*x_scale
            y_loc =np.imag(N_uniq[idx_N_mult[k]]) + y_off*y_scale
            plt.text(x_loc,y_loc,str(N_mult[idx_N_mult[k]]),
                     ha='center',va='bottom',fontsize=10)
    if N > 0:
        #D_roots = np.roots(a)
        D_uniq, D_mult=unique_cpx_roots(D_roots,tol=tol)
        plt.plot(np.real(D_uniq),np.imag(D_uniq),'kx',ms=8)
        idx_D_mult = mlab.find(D_mult>1)
        for k in range(len(idx_D_mult)):
            x_loc = np.real(D_uniq[idx_D_mult[k]]) + x_off*x_scale
            y_loc =np.imag(D_uniq[idx_D_mult[k]]) + y_off*y_scale
            plt.text(x_loc,y_loc,str(D_mult[idx_D_mult[k]]),
                     ha='center',va='bottom',fontsize=10)            
    if M - N < 0:
        plt.plot(0.0,0.0,'bo',mfc='None',ms=8)
    elif M - N > 0:
        plt.plot(0.0,0.0,'kx',ms=8)
    if abs(M - N) > 1:
        plt.text(x_off*x_scale,y_off*y_scale,str(abs(M-N)),
                 ha='center',va='bottom',fontsize=10)        
    plt.xlabel('Real Part')
    plt.ylabel('Imaginary Part')
    plt.title('Pole-Zero Plot')
    #plt.grid()
    plt.axis([-size,size,-size,size])
    return M,N
