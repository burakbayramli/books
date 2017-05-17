"""
Basic Linear Phase Digital Filter Design Helper

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
import optfir
import matplotlib.pyplot as plt
from matplotlib import pylab

def firwin_lpf(N_taps, fc, fs = 1.0):
    """
    Design a windowed FIR lowpass filter in terms of passband
    critical frequencies f1 < f2 in Hz relative to sampling rate
    fs in Hz. The number of taps must be provided.
    
    Mark Wickert October 2016
    """
    return signal.firwin(N_taps,2*fc/fs)


def firwin_bpf(N_taps, f1, f2, fs = 1.0, pass_zero=False):
    """
    Design a windowed FIR bandpass filter in terms of passband
    critical frequencies f1 < f2 in Hz relative to sampling rate
    fs in Hz. The number of taps must be provided.

    Mark Wickert October 2016
    """
    return signal.firwin(N_taps,2*(f1,f2)/fs,pass_zero=pass_zero)


def firwin_kaiser_lpf(f_pass, f_stop, d_stop, fs = 1.0, N_bump=0):
    """
    Design an FIR lowpass filter using the sinc() kernel and
    a Kaiser window. The filter order is determined based on 
    f_pass Hz, f_stop Hz, and the desired stopband attenuation
    d_stop in dB, all relative to a sampling rate of fs Hz.
    Note: the passband ripple cannot be set independent of the
    stopband attenuation.

    Mark Wickert October 2016
    """
    wc = 2*np.pi*(f_pass + f_stop)/2/fs
    delta_w = 2*np.pi*(f_stop - f_pass)/fs
    # Find the filter order
    M = np.ceil((d_stop - 8)/(2.285*delta_w))
    # Adjust filter order up or down as needed
    M += N_bump
    N_taps = M + 1
    # Obtain the Kaiser window
    beta = signal.kaiser_beta(d_stop)
    w_k = signal.kaiser(N_taps,beta)
    n = np.arange(N_taps)
    b_k = wc/np.pi*np.sinc(wc/np.pi*(n-M/2)) * w_k
    b_k /= np.sum(b_k)
    print('Kaiser Win filter taps = %d.' % N_taps)
    return b_k


def firwin_kaiser_hpf(f_stop, f_pass, d_stop, fs = 1.0, N_bump=0):
    """
    Design an FIR highpass filter using the sinc() kernel and
    a Kaiser window. The filter order is determined based on 
    f_pass Hz, f_stop Hz, and the desired stopband attenuation
    d_stop in dB, all relative to a sampling rate of fs Hz.
    Note: the passband ripple cannot be set independent of the
    stopband attenuation.

    Mark Wickert October 2016
    """
    # Transform HPF critical frequencies to lowpass equivalent
    f_pass_eq = fs/2. - f_pass
    f_stop_eq = fs/2. - f_stop
    # Design LPF equivalent
    wc = 2*np.pi*(f_pass_eq + f_stop_eq)/2/fs
    delta_w = 2*np.pi*(f_stop_eq - f_pass_eq)/fs
    # Find the filter order
    M = np.ceil((d_stop - 8)/(2.285*delta_w))
    # Adjust filter order up or down as needed
    M += N_bump
    N_taps = M + 1
    # Obtain the Kaiser window
    beta = signal.kaiser_beta(d_stop)
    w_k = signal.kaiser(N_taps,beta)
    n = np.arange(N_taps)
    b_k = wc/np.pi*np.sinc(wc/np.pi*(n-M/2)) * w_k
    b_k /= np.sum(b_k)
    # Transform LPF equivalent to HPF
    n = np.arange(len(b_k))
    b_k *= (-1)**n
    print('Kaiser Win filter taps = %d.' % N_taps)
    return b_k


def firwin_kaiser_bpf(f_stop1, f_pass1, f_pass2, f_stop2, d_stop, 
                      fs = 1.0, N_bump=0):
    """
    Design an FIR bandpass filter using the sinc() kernel and
    a Kaiser window. The filter order is determined based on 
    f_stop1 Hz, f_pass1 Hz, f_pass2 Hz, f_stop2 Hz, and the 
    desired stopband attenuation d_stop in dB for both stopbands,
    all relative to a sampling rate of fs Hz.
    Note: the passband ripple cannot be set independent of the
    stopband attenuation.

    Mark Wickert October 2016    
    """
    # Design BPF starting from simple LPF equivalent
    # The upper and lower stopbands are assumed to have 
    # the same attenuation level. The LPF equivalent critical
    # frequencies:
    f_pass = (f_pass2 - f_pass1)/2
    f_stop = (f_stop2 - f_stop1)/2
    # Continue to design equivalent LPF
    wc = 2*np.pi*(f_pass + f_stop)/2/fs
    delta_w = 2*np.pi*(f_stop - f_pass)/fs
    # Find the filter order
    M = np.ceil((d_stop - 8)/(2.285*delta_w))
    # Adjust filter order up or down as needed
    M += N_bump
    N_taps = M + 1
    # Obtain the Kaiser window
    beta = signal.kaiser_beta(d_stop)
    w_k = signal.kaiser(N_taps,beta)
    n = np.arange(N_taps)
    b_k = wc/np.pi*np.sinc(wc/np.pi*(n-M/2)) * w_k
    b_k /= np.sum(b_k)
    # Transform LPF to BPF
    f0 = (f_pass2 + f_pass1)/2
    w0 = 2*np.pi*f0/fs
    n = np.arange(len(b_k))
    b_k_bp = 2*b_k*np.cos(w0*(n-M/2))
    print('Kaiser Win filter taps = %d.' % N_taps)
    return b_k_bp


def firwin_kaiser_bsf(f_stop1, f_pass1, f_pass2, f_stop2, d_stop, 
                  fs = 1.0, N_bump=0):
    """
    Design an FIR bandstop filter using the sinc() kernel and
    a Kaiser window. The filter order is determined based on 
    f_stop1 Hz, f_pass1 Hz, f_pass2 Hz, f_stop2 Hz, and the 
    desired stopband attenuation d_stop in dB for both stopbands,
    all relative to a sampling rate of fs Hz.
    Note: The passband ripple cannot be set independent of the
    stopband attenuation.
    Note: The filter order is forced to be even (odd number of taps)
    so there is a center tap that can be used to form 1 - H_BPF.

    Mark Wickert October 2016    
    """
    # First design a BPF starting from simple LPF equivalent
    # The upper and lower stopbands are assumed to have 
    # the same attenuation level. The LPF equivalent critical
    # frequencies:
    f_pass = (f_pass2 - f_pass1)/2
    f_stop = (f_stop2 - f_stop1)/2
    # Continue to design equivalent LPF
    wc = 2*np.pi*(f_pass + f_stop)/2/fs
    delta_w = 2*np.pi*(f_stop - f_pass)/fs
    # Find the filter order
    M = np.ceil((d_stop - 8)/(2.285*delta_w))
    # Adjust filter order up or down as needed
    M += N_bump
    # Make filter order even (odd number of taps)
    if ((M+1)/2.0-int((M+1)/2.0)) == 0:
        M += 1
    N_taps = M + 1
    # Obtain the Kaiser window
    beta = signal.kaiser_beta(d_stop)
    w_k = signal.kaiser(N_taps,beta)
    n = np.arange(N_taps)
    b_k = wc/np.pi*np.sinc(wc/np.pi*(n-M/2)) * w_k
    b_k /= np.sum(b_k)
    # Transform LPF to BPF
    f0 = (f_pass2 + f_pass1)/2
    w0 = 2*np.pi*f0/fs
    n = np.arange(len(b_k))
    b_k_bs = 2*b_k*np.cos(w0*(n-M/2))
    # Transform BPF to BSF via 1 - BPF for odd N_taps
    b_k_bs = -b_k_bs
    b_k_bs[int(M/2)] += 1 
    print('Kaiser Win filter taps = %d.' % N_taps)
    return b_k_bs


def fir_remez_lpf(f_pass, f_stop, d_pass, d_stop, fs = 1.0, N_bump=5):
    """
    Design an FIR lowpass filter using remez with order
    determination. The filter order is determined based on 
    f_pass Hz, fstop Hz, and the desired passband ripple 
    d_pass dB and stopband attenuation d_stop dB all 
    relative to a sampling rate of fs Hz.

    Mark Wickert October 2016
    """
    n, ff, aa, wts=optfir.remezord([f_pass,f_stop], [1,0], 
                                   [1-10**(-d_pass/20.),10**(-d_stop/20.)], 
                                   fsamp=fs)
    # Bump up the order by N_bump to bring down the final d_pass & d_stop
    N_taps = n
    N_taps += N_bump
    b = signal.remez(N_taps, ff, aa[0::2], wts,Hz=2)
    print('Remez filter taps = %d.' % N_taps)
    return b


def fir_remez_hpf(f_stop, f_pass, d_pass, d_stop, fs = 1.0, N_bump=5):
    """
    Design an FIR highpass filter using remez with order
    determination. The filter order is determined based on 
    f_pass Hz, fstop Hz, and the desired passband ripple 
    d_pass dB and stopband attenuation d_stop dB all 
    relative to a sampling rate of fs Hz.

    Mark Wickert October 2016
    """
    # Transform HPF critical frequencies to lowpass equivalent
    f_pass_eq = fs/2. - f_pass
    f_stop_eq = fs/2. - f_stop
    # Design LPF equivalent    
    n, ff, aa, wts=optfir.remezord([f_pass_eq,f_stop_eq], [1,0], 
                                   [1-10**(-d_pass/20.),10**(-d_stop/20.)], 
                                   fsamp=fs)
    # Bump up the order by N_bump to bring down the final d_pass & d_stop
    N_taps = n
    N_taps += N_bump
    b = signal.remez(N_taps, ff, aa[0::2], wts,Hz=2)
    # Transform LPF equivalent to HPF
    n = np.arange(len(b))
    b *= (-1)**n
    print('Remez filter taps = %d.' % N_taps)
    return b


def fir_remez_bpf(f_stop1, f_pass1, f_pass2, f_stop2, d_pass, d_stop, 
                  fs = 1.0, N_bump=5):
    """
    Design an FIR bandpass filter using remez with order
    determination. The filter order is determined based on 
    f_stop1 Hz, f_pass1 Hz, f_pass2 Hz, f_stop2 Hz, and the 
    desired passband ripple d_pass dB and stopband attenuation
    d_stop dB all relative to a sampling rate of fs Hz.

    Mark Wickert October 2016
    """
    n, ff, aa, wts=optfir.remezord([f_stop1,f_pass1,f_pass2,f_stop2], 
                                   [0,1,0], 
                                   [10**(-d_stop/20.),1-10**(-d_pass/20.),
                                    10**(-d_stop/20.)], 
                                   fsamp=fs)
    # Bump up the order by N_bump to bring down the final d_pass & d_stop
    N_taps = n
    N_taps += N_bump
    b = signal.remez(N_taps, ff, aa[0::2], wts,Hz=2)
    print('Remez filter taps = %d.' % N_taps)
    return b

def fir_remez_bsf(f_pass1, f_stop1, f_stop2, f_pass2, d_pass, d_stop, 
                  fs = 1.0, N_bump=5):
    """
    Design an FIR bandstop filter using remez with order
    determination. The filter order is determined based on 
    f_pass1 Hz, f_stop1 Hz, f_stop2 Hz, f_pass2 Hz, and the 
    desired passband ripple d_pass dB and stopband attenuation
    d_stop dB all relative to a sampling rate of fs Hz.

    Mark Wickert October 2016
    """
    n, ff, aa, wts=optfir.remezord([f_pass1,f_stop1,f_stop2,f_pass2], 
                                   [1,0,1], 
                                   [1-10**(-d_pass/20.),10**(-d_stop/20.),
                                    1-10**(-d_pass/20.)], 
                                   fsamp=fs)
    # Bump up the order by N_bump to bring down the final d_pass & d_stop
    N_taps = n
    N_taps += N_bump
    b = signal.remez(N_taps, ff, aa[0::2], wts,Hz=2)
    print('Remez filter taps = %d.' % N_taps)
    return b


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
