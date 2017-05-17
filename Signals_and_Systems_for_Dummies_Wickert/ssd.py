#!/usr/bin/env

"""
The code module ssd.py was developed to accompany the book
Signals and Systems for Dummies published by Wiley Publishing.

Copyright 2012, 2013 Mark Wickert, mwickert@uccs.edu

v 1.0

Notes
-----
The primary purpose of this function library is to support the book Signals and Systems for Dummies. Beyond that it should be useful to anyone who wants to use Pylab for general signals and systems modeling and simulation. There is a good collection of digital communication simulation primitives included in the library. More enhancements are planned over time.

The formatted docstrings for the library follow. Click index in the upper right to get an
alphabetical listing of the library functions. In all of the example code given it is assumed that ssd has been imported into your workspace. See  the examples below for import options.

Examples
--------
>>> import ssd
>>> # Commands then need to be prefixed with ssd., i.e.,
>>> ssd.tri(t,tau)
>>> # A full import of the module, to avoid the the need to prefix with ssd, is:
>>> from ssd import *

Function Catalog
----------------
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

from matplotlib import pylab
from matplotlib import mlab 
import numpy as np
from numpy import fft
import matplotlib.pyplot as plt
from scipy import signal
from scipy.io import wavfile

def CIC(M,K):
    """
    % b = CIC(M,K)
    % A functional form implementation of a cascade of integrator comb (CIC) 
    % filters. Commonly used in multirate signal processing digital
    % down-converters and digital up-converters. A true CIC filter requires no
    % multiplies, only add and subtract operations. The functional form created
    % here is a simple FIR requiring real coefficient multiplies via filter()
    % ========================================================================
    %   M = Effective number of taps per section (typically the decimation
    %       factor).
    %   K = The number of CIC sections cascaded (larger K gives the filter a
    %       wider image rejection bandwidth.
    %   b = FIR filter coefficients for a simple direct form implementation
    %       using the filter() function.
    % ========================================================================
    %
    % Mark Wickert November 2007
    """
    
    if K == 1:
        b = np.ones(M)
    else:
        h = np.ones(M)
        b = h
        for i in range(1,K):
            b = signal.convolve(b,h) # cascade by convolving impulse responses
    
    # Make filter have unity gain at DC
    return b/np.sum(b)


def ten_band_eq_filt(x,GdB,Q=3.5):
    """
    Filter the input signal x with a ten-band equalizer having octave gain values in ndarray GdB.
    
    The signal x is filtered using octave-spaced peaking filters starting at 31.25 Hz and
    stopping at 16 kHz. The Q of each filter is 3.5, but can be changed. The sampling rate
    is assumed to be 44.1 kHz. 
    
    Parameters
    ----------
    x : ndarray of the input signal samples
    GdB : ndarray containing ten octave band gain values [G0dB,...,G9dB]
    Q : Quality factor vector for each of the NB peaking filters
    
    Returns
    -------
    y : ndarray of output signal samples
    
    Examples
    --------
    >>> # Test with white noise
    >>> w = randn(100000)
    >>> y = ten_band_eq_filt(x,GdB)
    >>> psd(y,2**10,44.1)
    """
    fs = 44100.0 # Hz
    NB = len(GdB)
    Fc = 31.25*2**np.arange(10)
    B = np.zeros((NB,3))
    A = np.zeros((NB,3))
    
    # Create matrix of cascade coefficients
    for k in range(NB):
        [b,a] = peaking(GdB[k],Fc[k],Q)
        B[k,:] = b
        A[k,:] = a
    #Pass signal x through the cascade of ten filters
    y = np.zeros(len(x))
    for k in range(NB):
        if k == 0:
            y = signal.lfilter(B[k,:],A[k,:],x)
        else:
            y = signal.lfilter(B[k,:],A[k,:],y)
    return y

def ten_band_eq_resp(GdB,Q=3.5):
    """
    Create a frequency response magnitude plot in dB of a ten band equalizer
    using a semilogplot (semilogx()) type plot
    
    
    Parameters
    ----------
    GdB : Gain vector for 10 peaking filters [G0,...,G9]
    Q : Quality factor for each peaking filter (default 3.5)
    
    Returns
    -------
    Nothing : two plots are created
    
    Examples
    --------
    >>> ten_band_eq_resp([0,10.0,0,0,-1,0,5,0,-4,0])
    """
    fs = 44100.0 # Hz
    Fc = 31.25*2**np.arange(10)
    NB = len(GdB)
    B = np.zeros((NB,3));
    A = np.zeros((NB,3));
    
    # Create matrix of cascade coefficients
    for k in range(NB):
        b,a = peaking(GdB[k],Fc[k],Q,fs)
        B[k,:] = b
        A[k,:] = a
    # Create the cascade frequency response
    F = np.logspace(1,np.log10(20e3),1000)
    H = np.ones(len(F))*np.complex(1.0,0.0)
    for k in range(NB):
       w,Htemp = signal.freqz(B[k,:],A[k,:],2*np.pi*F/fs)
       H *= Htemp
    plt.figure(figsize=(6,4))
    plt.subplot(211)
    plt.semilogx(F,20*np.log10(abs(H)))
    plt.axis([10, fs/2, -12, 12])
    plt.grid()
    plt.title('Ten-Band Equalizer Frequency Response')
    plt.xlabel('Frequency (Hz)')
    plt.ylabel('Gain (dB)')
    plt.subplot(212)
    plt.stem(np.arange(NB),GdB,'b','bs')
    #plt.bar(np.arange(NB)-.1,GdB,0.2)
    plt.axis([0, NB-1, -12, 12])
    plt.xlabel('Equalizer Band Number')
    plt.ylabel('Gain Set (dB)')
    plt.grid()

def peaking(GdB, fc, Q=3.5, fs=44100.):
    """
    A second-order peaking filter having GdB gain at fc and approximately
    and 0 dB otherwise.
    
    The filter coefficients returns correspond to a biquadratic system function
    containing five parameters.
    
    Parameters
    ----------
    GdB : Lowpass gain in dB
    fc : Center frequency in Hz
    Q : Filter Q which is inversely proportional to bandwidth
    fs : Sampling frquency in Hz
    
    Returns
    -------
    b : ndarray containing the numerator filter coefficients
    a : ndarray containing the denominator filter coefficients
    
    Examples
    --------
    >>> from scipy import signal
    >>> b,a =  peaking(2.0,500)
    >>> b,a =  peaking(-5.0,500,4)
    >>> # Assuming pylab imported
    >>> f = logspace(1,5,400)
    >>> .w,H = signal.freqz(b,a,2*pi*f/44100)
    >>> semilogx(f,20*log10(abs(H)))
    """
    mu = 10**(GdB/20.)
    kq = 4/(1 + mu)*np.tan(2*np.pi*fc/fs/(2*Q))
    Cpk = (1 + kq *mu)/(1 + kq)
    b1 = -2*np.cos(2*np.pi*fc/fs)/(1 + kq*mu)
    b2 = (1 - kq*mu)/(1 + kq*mu)
    a1 = -2*np.cos(2*np.pi*fc/fs)/(1 + kq)
    a2 = (1 - kq)/(1 + kq)
    b = Cpk*np.array([1, b1, b2])
    a = np.array([1, a1, a2])
    return b,a

def ex6_2(n):
    """
    Generate a triangle pulse as described in Example 6-2
    of Chapter 6.
    
    You need to supply an index array n that covers at least [-2, 5]. 
    The function returns the hard-coded signal of the example.
    
    Parameters
    ----------
    n : time index ndarray covering at least -2 to +5.
    
    Returns
    -------
    x : ndarray of signal samples in x
    
    Examples
    --------
    >>> n = arange(-5,8)
    >>> x = ex6_2(n)
    >>> stem(n,x) # creates a stem plot of x vs n
    """
    x = np.zeros(len(n))
    for k, nn in enumerate(n):
        if nn >= -2 and nn <= 5:
            x[k] = 8 - nn
    return x

def position_CD(Ka,out_type = 'fb_exact'):
    """
    CD sled position control case study of Chapter 18.

    The function returns the closed-loop and open-loop
    system function for a CD/DVD sled position control
    system. The loop amplifier gain is the only variable
    that may be changed. The returned system function can
    however be changed.

    Parameters
    ----------
    Ka : loop amplifier gain, start with 50.
    out_type : 'open_loop' for open loop system function
    out_type : 'fb_approx' for closed-loop approximation
    out_type : 'fb_exact' for closed-loop exact

    Returns
    -------
    b : numerator coefficient ndarray
    a : denominator coefficient ndarray 

    Notes
    -----
    With the exception of the loop amplifier gain, all
    other parameters are hard-coded from Case Study example.

    Examples
    ------
    >>> b,a = position_CD(Ka,'fb_approx')
    >>> b,a = position_CD(Ka,'fb_exact')
    """
    rs = 10/(2*np.pi)
    # Load b and a ndarrays with the coefficients
    if out_type.lower() == 'open_loop':
        b = np.array([Ka*4000*rs])
        a = np.array([1,1275,31250,0])
    elif out_type.lower() == 'fb_approx':
        b = np.array([3.2*Ka*rs])
        a = np.array([1, 25, 3.2*Ka*rs])
    elif out_type.lower() == 'fb_exact':
        b = np.array([4000*Ka*rs])
        a = np.array([1, 1250+25, 25*1250, 4000*Ka*rs])
    else:
        print 'out_type must be: open_loop, fb_approx, or fc_exact'
        return 1
    return b, a

def cruise_control(wn,zeta,T,vcruise,vmax,tf_mode='H'):
    """
    Cruise control with PI controller and hill disturbance.

    This function returns various system function configurations
    for a the cruise control Case Study example found in 
    the supplementary article. The plant model is obtained by the
    linearizing the equations of motion and the controller contains a
    proportional and integral gain term set via the closed-loop parameters
    natuarl frequency wn (rad/s) and damping zeta.

    Parameters
    ----------
    wn : closed-loop natural frequency in rad/s, nominally 0.1
    zeta : closed-loop damping factor, nominally 1.0
    T : vehicle time constant, nominally 10 s
    vcruise : cruise velocity set point, nominally 75 mph
    vmax : maximum vehicle velocity, nominally 120 mph
    tf_mode : 'H', 'HE', 'HVW', or 'HED' controls the system function returned by the function 
    'H'   : closed-loop system function V(s)/R(s)
    'HE'  : closed-loop system function E(s)/R(s)
    'HVW' : closed-loop system function V(s)/W(s)
    'HED' : closed-loop system function E(s)/D(s), where D is the hill disturbance input

    Returns
    -------
    b : numerator coefficient ndarray
    a : denominator coefficient ndarray 

    Examples
    --------
    >>> # return the closed-loop system function output/input velocity
    >>> b,a = cruise_control(wn,zeta,T,vcruise,vmax,tf_mode='H')
    >>> # return the closed-loop system function loop error/hill disturbance
    >>> b,a = cruise_control(wn,zeta,T,vcruise,vmax,tf_mode='HED')
    """
    tau = T/2.*vmax/vcruise
    g = 9.8
    g *= 3*60**2/5280. # m/s to mph conversion
    Kp = T/vmax*(2*zeta*wn-1/tau)
    Ki = T/vmax*wn**2
    K = Kp*vmax/T
    print 'wn = ', np.sqrt(K/(Kp/Ki))
    print 'zeta = ', (K + 1/tau)/(2*wn)
    a = np.array([1, 2*zeta*wn, wn**2])
    if tf_mode == 'H':
        b = np.array([K, wn**2])      
    elif tf_mode == 'HE':
        b = np.array([1, 2*zeta*wn-K, 0.])   
    elif tf_mode == 'HVW':
        b = np.array([ 1, wn**2/K+1/tau, wn**2/(K*tau)])
        b *= Kp
    elif tf_mode == 'HED':
        b = np.array([g, 0])
    else:
        print 'tf_mode must be: H, HE, HVU, or HED'
        return 1
    return b, a

def splane(b,a,auto_scale=True,size=[-1,1,-1,1]):
    """
    Create an s-plane pole-zero plot.
    
    As input the function uses the numerator and denominator 
    s-domain system function coefficient ndarrays b and a respectively. 
    Assumed to be stored in descending powers of s.
    
    Parameters
    ----------
    b : numerator coefficient ndarray. 
    a : denominator coefficient ndarray. 
    auto_scale : True 
    size : [xmin,xmax,ymin,ymax] plot scaling when scale = False 
    
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
    >>> splane(b,a)
    >>> # Here the plot is generated using manual scaling
    >>> splane(b,a,False,[-10,1,-10,10])
    """
    M = len(b) - 1
    N = len(a) - 1
    plt.figure(figsize=(5,5))
    #plt.axis('equal')
    N_roots = np.array([0.0])
    if M > 0:
        N_roots = np.roots(b)
    D_roots = np.array([0.0])
    if N > 0:
        D_roots = np.roots(a)
    if auto_scale:
        size[0] = min(np.min(np.real(N_roots)),np.min(np.real(D_roots)))-0.5
        size[1] = max(np.max(np.real(N_roots)),np.max(np.real(D_roots)))+0.5
        size[1] = max(size[1],0.5)
        size[2] = min(np.min(np.imag(N_roots)),np.min(np.imag(D_roots)))-0.5
        size[3] = max(np.max(np.imag(N_roots)),np.max(np.imag(D_roots)))+0.5
        
    plt.plot([size[0],size[1]],[0,0],'k--')
    plt.plot([0,0],[size[2],size[3]],'r--')
    # Plot labels if multiplicity greater than 1
    x_scale = size[1]-size[0]
    y_scale = size[3]-size[2]
    x_off = 0.03
    y_off = 0.01
    if M > 0:
        #N_roots = np.roots(b)
        N_uniq, N_mult=signal.unique_roots(N_roots,tol=1e-3, rtype='avg')
        plt.plot(np.real(N_uniq),np.imag(N_uniq),'ko',mfc='None',ms=8)
        idx_N_mult = mlab.find(N_mult>1)
        for k in range(len(idx_N_mult)):
            x_loc = np.real(N_uniq[idx_N_mult[k]]) + x_off*x_scale
            y_loc =np.imag(N_uniq[idx_N_mult[k]]) + y_off*y_scale
            plt.text(x_loc,y_loc,str(N_mult[idx_N_mult[k]]),ha='center',va='bottom',fontsize=10)
    if N > 0:
        #D_roots = np.roots(a)
        D_uniq, D_mult=signal.unique_roots(D_roots,tol=1e-3, rtype='avg')
        plt.plot(np.real(D_uniq),np.imag(D_uniq),'kx',ms=8)
        idx_D_mult = mlab.find(D_mult>1)
        for k in range(len(idx_D_mult)):
            x_loc = np.real(D_uniq[idx_D_mult[k]]) + x_off*x_scale
            y_loc =np.imag(D_uniq[idx_D_mult[k]]) + y_off*y_scale
            plt.text(x_loc,y_loc,str(D_mult[idx_D_mult[k]]),ha='center',va='bottom',fontsize=10)
    plt.xlabel('Real Part')
    plt.ylabel('Imaginary Part')
    plt.title('Pole-Zero Plot')
    #plt.grid()
    plt.axis(np.array(size))
    return M,N

def OS_filter(x,h,N,mode=0):
    """
    Overlap and save transform domain FIR filtering.
    
    This function implements the classical overlap and save method of
    transform domain filtering using a length P FIR filter.
    
    Parameters
    ----------
    x : input signal to be filtered as an ndarray
    h : FIR filter coefficients as an ndarray of length P
    N : FFT size > P, typically a power of two
    mode : 0 or 1, when 1 returns a diagnostic matrix
    
    Returns
    -------
    y : the filtered output as an ndarray
    y_mat : an ndarray whose rows are the individual overlap outputs.
    
    Notes
    -----
    y_mat is used for diagnostics and to gain understanding of the algorithm.
    
    Examples
    --------
    >>> n = arange(0,100)
    >>> x cos(2*pi*0.05*n)
    >>> b = ones(10)
    >>> y = OS_filter(x,h,N)
    >>> # set mode = 1
    >>> y, y_mat = OS_filter(x,h,N,1)
    """
    
    P = len(h)
    # zero pad start of x so first frame can recover first true samples of x
    x = np.hstack((np.zeros(P-1),x))
    L = N - P + 1
    Nx = len(x)
    Nframe = int(np.ceil(Nx/float(L)))
    # zero pad end of x to full number of frames needed
    x = np.hstack((x,np.zeros(Nframe*L-Nx)))
    y = np.zeros(Nframe*N)
    # create an instrumentation matrix to observe the overlap and save behavior
    y_mat = np.zeros((Nframe,Nframe*N))

    H = fft.fft(h,N)
    # begin the filtering operation
    for k in range(Nframe):
        xk = x[k*L:k*L+N]
        Xk = fft.fft(xk,N)
        Yk = H*Xk
        yk = np.real(fft.ifft(Yk)) # imag part should be zero
        y[k*L+P-1:k*L+N] = yk[P-1:]
        y_mat[k,k*L:k*L+N] = yk
    if mode == 1:
        return y[P-1:Nx], y_mat[:,P-1:Nx]
    else:
        return y[P-1:Nx]

def OA_filter(x,h,N,mode=0):
    """
    Overlap and add transform domain FIR filtering.
    
    This function implements the classical overlap and add method of
    transform domain filtering using a length P FIR filter.
    
    Parameters
    ----------
    x : input signal to be filtered as an ndarray
    h : FIR filter coefficients as an ndarray of length P
    N : FFT size > P, typically a power of two
    mode : 0 or 1, when 1 returns a diagnostic matrix
    
    Returns
    -------
    y : the filtered output as an ndarray
    y_mat : an ndarray whose rows are the individual overlap outputs.
    
    Notes
    -----
    y_mat is used for diagnostics and to gain understanding of the algorithm.
    
    Examples
    --------
    >>> n = arange(0,100)
    >>> x cos(2*pi*0.05*n)
    >>> b = ones(10)
    >>> y = OA_filter(x,h,N)
    >>> # set mode = 1
    >>> y, y_mat = OA_filter(x,h,N,1)
    """
    P = len(h)
    L = N - P + 1 # need N >= L + P -1
    Nx = len(x)
    Nframe = int(np.ceil(Nx/float(L)))
    # zero pad to full number of frames needed
    x = np.hstack((x,np.zeros(Nframe*L-Nx)))
    y = np.zeros(Nframe*N)
    # create an instrumentation matrix to observe the overlap and add behavior
    y_mat = np.zeros((Nframe,Nframe*N))
    H = fft.fft(h,N)
    # begin the filtering operation
    for k in range(Nframe):
        xk = x[k*L:(k+1)*L]
        Xk = fft.fft(xk,N)
        Yk = H*Xk
        yk = np.real(fft.ifft(Yk))
        y[k*L:k*L+N] += yk
        y_mat[k,k*L:k*L+N] = yk
    if mode == 1:
        return y[0:Nx], y_mat[:,0:Nx]
    else:
        return y[0:Nx]

def lp_samp(fb,fs,fmax,N,shape='tri',fsize=(6,4)):
    """
    Lowpass sampling theorem plotting function.
    
    Display the spectrum of a sampled signal after setting the bandwidth, 
    sampling frequency, maximum display frequency, and spectral shape.
    
    Parameters
    ----------
    fb : spectrum lowpass bandwidth in Hz
    fs : sampling frequency in Hz
    fmax : plot over [-fmax,fmax]
    shape : 'tri' or 'line'
    N : number of translates, N positive and N negative
    fsize : the size of the figure window, default (6,4)
    
    Returns
    -------
    Nothing : A plot window opens containing the spectrum plot
    
    Examples
    --------
    >>> # No aliasing as 10 < 25/2
    >>> lp_samp(10,25,50,10)
    >>> # Aliasing as 15 > 25/2
    >>> lp_samp(15,25,50,10)
    """

    plt.figure(figsize=fsize)
    # define the plot interval
    f = np.arange(-fmax,fmax+fmax/200.,fmax/200.)
    A = 1.0;
    line_ampl = A/2.*np.array([0, 1])
    # plot the lowpass spectrum in black
    if shape.lower() == 'tri':
        plt.plot(f,lp_tri(f,fb))
    elif shape.lower() == 'line':
        plt.plot([fb, fb],line_ampl,'b', linewidth=2)
        plt.plot([-fb, -fb],line_ampl,'b', linewidth=2)
    else:
        print 'shape must be tri or line'
    # overlay positive and negative frequency translates
    for n in range(N):
        if shape.lower() == 'tri':
            plt.plot(f,lp_tri(f-(n+1)*fs,fb),'--r')
            plt.plot(f,lp_tri(f+(n+1)*fs,fb),'--g')
        elif shape.lower() == 'line':
            plt.plot([fb+(n+1)*fs, fb+(n+1)*fs],line_ampl,'--r', linewidth=2)
            plt.plot([-fb+(n+1)*fs, -fb+(n+1)*fs],line_ampl,'--r', linewidth=2)
            plt.plot([fb-(n+1)*fs, fb-(n+1)*fs],line_ampl,'--g', linewidth=2)
            plt.plot([-fb-(n+1)*fs, -fb-(n+1)*fs],line_ampl,'--g', linewidth=2)
        else:
            print 'shape must be tri or line'
    #plt.title('Lowpass Sampling Theorem for a Real Signal: Blk = orig, dotted = translates')
    plt.ylabel('Spectrum Magnitude')
    plt.xlabel('Frequency in Hz')
    plt.axis([-fmax,fmax,0,1])
    plt.grid()
    
def lp_tri(f, fb):
    """
    Triangle spectral shape function used by lp_spec.
    
    This is a support function for the lowpass spectrum plotting function
    lp_spec().
    
    Parameters
    ----------
    f : ndarray containing frequency samples
    fb : the bandwidth as a float constant
    
    Returns
    -------
    x : ndarray of spectrum samples for a single triangle shape
    
    Examples
    --------
    >>> x = lp_tri(f, fb)
    """

    x = np.zeros(len(f))
    for k in range(len(f)):
        if abs(f[k]) <= fb:
            x[k] = 1 - abs(f[k])/float(fb)
    return x    

def sinusoidAWGN(x,SNRdB):
    """
    Add white Gaussian noise to a single real sinusoid.
    
    Input a single sinusoid to this function and it returns a noisy
    sinusoid at a specific SNR value in dB. Sinusoid power is calculated
    using np.var.
    
    Parameters
    ----------
    x : Input signal as ndarray consisting of a single sinusoid
    SNRdB : SNR in dB for output sinusoid
         
    Returns
    -------
    y : Noisy sinusoid return vector

    Examples
    --------
    >>> # set the SNR to 10 dB
    >>> n = arange(0,10000)
    >>> x = cos(2*pi*0.04*n)
    >>> y = sinusoidAWGN(x,10.0)
"""
    # Estimate signal power
    x_pwr = np.var(x)

    # Create noise vector
    noise = np.sqrt(x_pwr/10**(SNRdB/10.))*np.random.randn(len(x));
    return x + noise

def simpleQuant(x,Btot,Xmax,Limit):
    """
    A simple rounding quantizer for bipolar signals having Btot = B + 1 bits.
    
    This function models a quantizer that employs Btot bits that has one of
    three selectable limiting types: saturation, overflow, and none. 
    The quantizer is bipolar and implements rounding.
     
    Parameters
    ----------
    x : input signal ndarray to be quantized
    Btot : total number of bits in the quantizer, e.g. 16
    Xmax : quantizer full-scale dynamic range is [-Xmax, Xmax]
    Limit = Limiting of the form 'sat', 'over', 'none'
    
    Returns
    -------
    xq : quantized output ndarray
    
    Notes
    -----
    The quantization can be formed as e = xq - x
    
    Examples
    --------
    >>> n = arange(0,10000)
    >>> x = cos(2*pi*0.211*n)
    >>> y = sinusoidAWGN(x,90)
    >>> yq = simpleQuant(y,12,1,sat)
    >>> psd(y,2**10,Fs=1);
    >>> psd(yq,2**10,Fs=1)
    """
    B = Btot-1
    x = x/Xmax
    if Limit.lower() == 'over':
        xq = (np.mod(np.round(x*2**B)+2**B,2**Btot)-2**B)/2**B
    elif Limit.lower() == 'sat':
        xq = np.round(x*2**B)+2**B
        s1 = mlab.find(xq >= 2**Btot-1)
        s2 = mlab.find(xq < 0)
        xq[s1] = (2**Btot - 1)*np.ones(len(s1))
        xq[s2] = np.zeros(len(s2))
        xq = (xq - 2**B)/2**B
    elif Limit.lower() == 'none':
        xq = np.round(x*2**B)/2**B
    else:
        print 'limit must be the string over, sat, or none'
    return xq*Xmax


def prin_alias(f_in,fs):
    """
    Calculate the principle alias frequencies.
    
    Given an array of input frequencies the function returns an
    array of principle alias frequencies.
    
    Parameters
    ----------
    f_in : ndarray of input frequencies
    fs : sampling frequency
    
    Returns
    -------
    f_out : ndarray of principle alias frequencies
    
    Examples
    --------
    >>> # Linear frequency sweep from 0 to 50 Hz
    >>> f_in = arange(0,50,0.1)
    >>> # Calculate principle alias with fs = 10 Hz
    >>> f_out = prin_alias(f_in,10)
    """
    return abs(np.rint(f_in/fs)*fs - f_in)

    """
    Principle alias via recursion
    f_out = np.copy(f_in)
    for k in range(len(f_out)):
        while f_out[k] > fs/2.:
            f_out[k] = abs(f_out[k] - fs)
    return f_out
    """

def cascade_filters(b1,a1,b2,a2):
    """
    Cascade two IIR digital filters into a single (b,a) coefficient set.
    
    To cascade two digital filters (system functions) given their numerator
    and denominator coefficients you simply convolve the coefficient arrays.
    
    Parameters
    ----------
    b1 : ndarray of numerator coefficients for filter 1
    a1 : ndarray of denominator coefficients for filter 1
    b2 : ndarray of numerator coefficients for filter 2
    a2 : ndarray of denominator coefficients for filter 2
    
    Returns
    -------
    b : ndarray of numerator coefficients for the cascade
    a : ndarray of denominator coefficients for the cascade
    
    Examples
    --------
    >>> from scipy import signal
    >>> b1,a1 = signal.butter(3, 0.1)
    >>> b2,a2 = signal.butter(3, 0.15)
    >>> b,a = cascade_filters(b1,a1,b2,a2)
    """
    return signal.convolve(b1,b2), signal.convolve(a1,a2)

def soi_snoi_gen(s,SIR_dB,N,fi,fs = 8000):
    """
    Add an interfering sinusoidal tone to the input signal at a given SIR_dB.
    
    The input is the signal of interest (SOI) and number of sinsuoid signals
    not of interest (SNOI) are addedto the SOI at a prescribed signal-to-
    intereference SIR level in dB. 
    
    Parameters
    ----------
    s : ndarray of signal of SOI
    SIR_dB : interference level in dB
    N : Trim input signal s to length N + 1 samples
    fi : ndarray of intereference frequencies in Hz
    fs : sampling rate in Hz, default is 8000 Hz
    
    Returns
    -------
    r : ndarray of combined signal plus intereference of length N+1 samples
    
    Examples
    --------
    >>> # load a speech ndarray and trim to 5*8000 + 1 samples
    >>> fs,s = from_wav('OSR_us_000_0030_8k.wav')
    >>> r = soi_snoi_gen(s,10,5*8000,[1000, 1500])
    """
    
    n = np.arange(0,N+1)
    K = len(fi)
    si = np.zeros(N+1)
    for k in range(K):
        si += np.cos(2*np.pi*fi[k]/fs*n);
    s = s[:N+1]
    Ps = np.var(s)
    Psi = np.var(si)
    r = s + np.sqrt(Ps/Psi*10**(-SIR_dB/10))*si
    return r

def lms_ic(r,M,mu,delta=1):
    """
    Least mean square (LMS) interference canceller adaptive filter.

    A complete LMS adaptive filter simulation function for the case of
    interference cancellation. Used in the digital filtering case study.

    Parameters
    ----------
    M : FIR Filter length (order M-1)
    delta : Delay used to generate the reference signal
    mu : LMS step-size
    delta : decorrelation delay between input and FIR filter input

    Returns
    -------
    n : ndarray Index vector
    r : ndarray noisy (with interference) input signal
    r_hat : ndarray filtered output (NB_hat[n])
    e : ndarray error sequence (WB_hat[n])
    ao : ndarray final value of weight vector
    F : ndarray frequency response axis vector
    Ao : ndarray frequency response of filter

    Examples
    ----------
    >>> # import a speech signal
    >>> fs,s = from_wav('OSR_us_000_0030_8k.wav')
    >>> # add interference at 1kHz and 1.5 kHz and
    >>> # truncate to 5 seconds
    >>> r = soi_snoi_gen(s,10,5*8000,[1000, 1500])
    >>> # simulate with a 64 tap FIR and mu = 0.005
    >>> n,r,r_hat,e,ao,F,Ao = lms_ic(r,64,0.005)
    """

    N = len(r)-1;
    # Form the reference signal y via delay delta
    y = signal.lfilter(np.hstack((np.zeros(delta), np.array([1]))),1,r)
    # Initialize output vector x_hat to zero
    r_hat = np.zeros(N+1)
    # Initialize error vector e to zero
    e = np.zeros(N+1)
    # Initialize weight vector to zero
    ao = np.zeros(M+1)
    # Initialize filter memory to zero
    z = np.zeros(M)
    # Initialize a vector for holding ym of length M+1
    ym = np.zeros(M+1)
    for k in range(N+1):
       # Filter one sample at a time
       r_hat[k],z = signal.lfilter(ao,np.array([1]),np.array([y[k]]),zi=z)
       # Form the error sequence
       e[k] = r[k] - r_hat[k]
       # Update the weight vector
       ao = ao + 2*mu*e[k]*ym
       # Update vector used for correlation with e(k)
       ym = np.hstack((np.array([y[k]]), ym[:-1]))
    # Create filter frequency response
    F, Ao = signal.freqz(ao,1,1024)
    F/= (2*np.pi)
    Ao = 20*np.log10(abs(Ao))
    return np.arange(0,N+1), r, r_hat, e, ao, F, Ao

def fir_iir_notch(fi,fs,r=0.95):
    """
    Design a second-order FIR or IIR notch filter.

    A second-order FIR notch filter is created by placing conjugate
    zeros on the unit circle at angle corresponidng to the notch center
    frequency. The IIR notch variation places a pair of conjugate poles
    at the same angle, but with radius r < 1 (typically 0.9 to 0.95).

    Parameters
    ----------
    fi : notch frequency is Hz relative to fs
    fs : the sampling frequency in Hz, e.g. 8000
    r : pole radius for IIR version, default = 0.95

    Returns
    -------
    b : numerator coefficient ndarray
    a : denominator coefficient ndarray 

    Notes
    -----
    If the pole radius is 0 then an FIR version is created, that is 
    there are no poles except at z = 0.

    Examples
    --------
    >>> b_FIR, a_FIR = fir_iir_notch(1000,8000,0)
    >>> b_IIR, a_IIR = fir_iir_notch(1000,8000)
    """
    w0 = 2*np.pi*fi/float(fs)
    if r >= 1:
        print 'Poles on or outside unit circle.'
    if r == 0:
        a = np.array([1.0])
    else:
        a = np.array([1, -2*r*np.cos(w0), r**2])
    b = np.array([1, -2*np.cos(w0), 1])
    return b, a

def simple_SA(x,NS,NFFT,fs,NAVG=1,window='boxcar'):
    """
    Spectral estimation using windowing and averaging.

    This function implements averaged periodogram spectral estimation
    estimation similar to the NumPy's psd() function, but more
    specialized for the the windowing case study of Chapter 16.

    Parameters
    ----------
    x : ndarray containing the input signal
    NS : The subrecord length less zero padding, e.g. NS < NFFT
    NFFT : FFT length, e.g., 1024 = 2**10
    fs : sampling rate in Hz
    NAVG : the number of averages, e.g., 1 for deterministic signals
    window : hardcoded window 'boxcar' (default) or 'hanning'

    Returns
    -------
    f : ndarray frequency axis in Hz on [0, fs/2]
    Sx : ndarray the power spectrum estimate

    Notes
    -----
    The function also prints the maximum number of averages K possible
    for the input data record.

    Examples
    --------
    >>> n = arange(0,2048)
    >>> x = cos(2*pi*1000/10000*n) + 0.01*cos(2*pi*3000/10000*n)
    >>> f, Sx = simple_SA(x,128,512,10000)
    >>> f, Sx = simple_SA(x,256,1024,10000,window='hanning')
    >>> plot(f, 10*log10(Sx))
    """
    Nx = len(x)
    K = Nx/NS
    print 'K = ', K
    if NAVG > K:
        print 'NAVG exceeds number of available subrecords'
        return 0,0
    if window.lower() == 'boxcar' or window.lower() == 'rectangle':
        w = signal.boxcar(NS)
    elif window.lower() == 'hanning':
        w = signal.hanning(NS)
    xsw = np.zeros((K,NS)) + 1j*np.zeros((K,NS))
    for k in range(NAVG):
        xsw[k,] = w*x[k*NS:(k+1)*NS]
    Sx = np.zeros(NFFT)
    for k in range(NAVG):
        X = fft.fft(xsw[k,],NFFT)
        Sx += abs(X)**2
    Sx /= float(NAVG)
    Sx /= float(NFFT**2)
    if x.dtype != 'complex128':
        n = np.arange(NFFT/2)
        f = fs*n/float(NFFT)
        Sx = Sx[0:NFFT/2]
    else:
        n = np.arange(NFFT/2)
        f = fs*np.hstack((np.arange(-NFFT/2,0),np.arange(NFFT/2)))/float(NFFT)
        Sx = np.hstack((Sx[NFFT/2:],Sx[0:NFFT/2]))
    return f, Sx


def line_spectra(fk,Xk,mode,sides=2,linetype='b',lwidth=2,floor_dB=-100,fsize=(6,4)):
    """
    Plot the Fouier series line spectral given the coefficients.

    This function plots two-sided and one-sided line spectra of a periodic
    signal given the complex exponential Fourier series coefficients and
    the corresponding harmonic frequencies.

    Parameters
    ----------
    fk : vector of real sinusoid frequencies
    Xk : magnitude and phase at each positive frequency in fk
    mode : 'mag' => magnitude plot, 'magdB' => magnitude in dB plot,
    mode cont : 'magdBn' => magnitude in dB normalized, 'phase' => a phase plot in radians 
    sides : 2; 2-sided or 1-sided
    linetype : line type per Matplotlib definitions, e.g., 'b';
    lwidth : 2; linewidth in points
    fsize : optional figure size in inches, default = (6,4) inches 

    Returns
    -------
    Nothing : A plot window opens containing the line spectrum plot

    Notes
    -----
    Since real signals are assumed the frequencies of fk are 0 and/or positive
    numbers. The supplied Fourier coefficients correspond.

    Examples
    --------
    >>> n = arange(0,25)
    >>> # a pulse train with 10 Hz fundamental and 20% duty cycle
    >>> fk = n*10
    >>> Xk = sinc(n*10*.02)*exp(-1j*2*pi*n*10*.01) # 1j = sqrt(-1)
    >>> line_spectra(fk,Xk,'mag')
    >>> line_spectra(fk,Xk,'phase')
    """

    plt.figure(figsize=fsize)
    # Eliminate zero valued coefficients
    idx = pylab.find(Xk != 0)
    Xk = Xk[idx]
    fk = fk[idx]
    if mode == 'mag':
        for k in range(len(fk)):
            if fk[k] == 0 and sides == 2:
                plt.plot([fk[k], fk[k]],[0, np.abs(Xk[k])],linetype, linewidth=lwidth)
            elif fk[k] == 0 and sides == 1:
                plt.plot([fk[k], fk[k]],[0, np.abs(Xk[k])],linetype, linewidth=2*lwidth)            
            elif fk[k] > 0 and sides == 2:
                plt.plot([fk[k], fk[k]],[0, np.abs(Xk[k])],linetype, linewidth=lwidth)
                plt.plot([-fk[k], -fk[k]],[0, np.abs(Xk[k])],linetype, linewidth=lwidth)
            elif fk[k] > 0 and sides == 1:
                plt.plot([fk[k], fk[k]],[0, 2.*np.abs(Xk[k])],linetype, linewidth=lwidth)
            else:
                print 'Invalid sides type'
        plt.grid()
        if sides == 2:
            plt.axis([-1.2*max(fk), 1.2*max(fk), 0, 1.05*max(abs(Xk))])        
        elif sides == 1:
            plt.axis([0, 1.2*max(fk), 0, 1.05*2*max(abs(Xk))])
        else:
            print 'Invalid sides type'
        plt.ylabel('Magnitude')
        plt.xlabel('Frequency (Hz)')
    elif mode == 'magdB':
        Xk_dB = 20*np.log10(np.abs(Xk))
        for k in range(len(fk)):
            if fk[k] == 0 and sides == 2:
                plt.plot([fk[k], fk[k]],[floor_dB, Xk_dB[k]],linetype, linewidth=lwidth)
            elif fk[k] == 0 and sides == 1:
                plt.plot([fk[k], fk[k]],[floor_dB, Xk_dB[k]],linetype, linewidth=2*lwidth)            
            elif fk[k] > 0 and sides == 2:
                plt.plot([fk[k], fk[k]],[floor_dB, Xk_dB[k]],linetype, linewidth=lwidth)
                plt.plot([-fk[k], -fk[k]],[floor_dB, Xk_dB[k]],linetype, linewidth=lwidth)
            elif fk[k] > 0 and sides == 1:
                plt.plot([fk[k], fk[k]],[floor_dB, Xk_dB[k]+6.02],linetype, linewidth=lwidth)
            else:
                print 'Invalid sides type'
        plt.grid()
        max_dB = np.ceil(max(Xk_dB/10.))*10
        min_dB = max(floor_dB,np.floor(min(Xk_dB/10.))*10)
        if sides == 2:
            plt.axis([-1.2*max(fk), 1.2*max(fk), min_dB, max_dB])        
        elif sides == 1:
            plt.axis([0, 1.2*max(fk), min_dB, max_dB])
        else:
            print 'Invalid sides type'
        plt.ylabel('Magnitude (dB)')
        plt.xlabel('Frequency (Hz)')
    elif mode == 'magdBn':
        Xk_dB = 20*np.log10(np.abs(Xk)/max(np.abs(Xk)))
        for k in range(len(fk)):
            if fk[k] == 0 and sides == 2:
                plt.plot([fk[k], fk[k]],[floor_dB, Xk_dB[k]],linetype, linewidth=lwidth)
            elif fk[k] == 0 and sides == 1:
                plt.plot([fk[k], fk[k]],[floor_dB, Xk_dB[k]],linetype, linewidth=2*lwidth)            
            elif fk[k] > 0 and sides == 2:
                plt.plot([fk[k], fk[k]],[floor_dB, Xk_dB[k]],linetype, linewidth=lwidth)
                plt.plot([-fk[k], -fk[k]],[floor_dB, Xk_dB[k]],linetype, linewidth=lwidth)
            elif fk[k] > 0 and sides == 1:
                plt.plot([fk[k], fk[k]],[floor_dB, Xk_dB[k]+6.02],linetype, linewidth=lwidth)
            else:
                print 'Invalid sides type'
        plt.grid()
        max_dB = np.ceil(max(Xk_dB/10.))*10
        min_dB = max(floor_dB,np.floor(min(Xk_dB/10.))*10)
        if sides == 2:
            plt.axis([-1.2*max(fk), 1.2*max(fk), min_dB, max_dB])        
        elif sides == 1:
            plt.axis([0, 1.2*max(fk), min_dB, max_dB])
        else:
            print 'Invalid sides type'
        plt.ylabel('Normalized Magnitude (dB)')
        plt.xlabel('Frequency (Hz)')    
    elif mode == 'phase':
        for k in range(len(fk)):
            if fk[k] == 0 and sides == 2:
                plt.plot([fk[k], fk[k]],[0, np.angle(Xk[k])],linetype, linewidth=lwidth)
            elif fk[k] == 0 and sides == 1:
                plt.plot([fk[k], fk[k]],[0, np.angle(Xk[k])],linetype, linewidth=2*lwidth)
            elif fk[k] > 0 and sides == 2:
                plt.plot([fk[k], fk[k]],[0, np.angle(Xk[k])],linetype, linewidth=lwidth)
                plt.plot([-fk[k], -fk[k]],[0, -np.angle(Xk[k])],linetype, linewidth=lwidth)
            elif fk[k] > 0 and sides == 1:
                plt.plot([fk[k], fk[k]],[0, np.angle(Xk[k])],linetype, linewidth=lwidth)
            else:
                print 'Invalid sides type'
        plt.grid()
        if sides == 2:
            plt.plot([-1.2*max(fk), 1.2*max(fk)], [0, 0],'k')
            plt.axis([-1.2*max(fk), 1.2*max(fk), -1.1*max(np.abs(np.angle(Xk))), 1.1*max(np.abs(np.angle(Xk)))])
        elif sides == 1:
            plt.plot([0, 1.2*max(fk)], [0, 0],'k')
            plt.axis([0, 1.2*max(fk), -1.1*max(np.abs(np.angle(Xk))), 1.1*max(np.abs(np.angle(Xk)))])
        else:
            print 'Invalid sides type'
        plt.ylabel('Phase (rad)')
        plt.xlabel('Frequency (Hz)')
    else:
        print 'Invalid mode type'

def fs_coeff(xp,N,f0,one_side=True):
    """
    Numerically approximate the Fourier series coefficients given periodic x(t).

    The input is assummed to represent one period of the waveform
    x(t) that has been uniformly sampled. The number of samples supplied
    to represent one period of the waveform sets the sampling rate.

    Parameters
    ----------
    xp : ndarray of one period of the waveform x(t)
    N : maximum Fourier series coefficient, [0,...,N]
    f0 : fundamental frequency used to form fk.

    Returns
    -------
    Xk : ndarray of the coefficients over indices [0,1,...,N]
    fk : ndarray of the harmonic frequencies [0, f0,2f0,...,Nf0]

    Notes
    -----
    len(xp) >= 2*N+1 as len(xp) is the fft length.

    Examples
    --------
    >>> t = arange(0,1,1/1024.)
    >>> # a 20% duty cycle pulse starting at t = 0
    >>> x_rect = rect(t-.1,0.2)
    >>> Xk, fk = fs_coeff(x_rect,25,10)
    >>> # plot the spectral lines
    >>> line_spectra(fk,Xk,'mag')
    """
    Nint = len(xp)
    if Nint < 2*N+1:
        print 'Number of samples in xp insufficient for requested N.'
        return 0,0
    Xp = fft.fft(xp,Nint)/float(Nint)
    # To interface with the line_spectra function use one_side mode
    if one_side:
        Xk = Xp[0:N+1]
        fk = f0*np.arange(0,N+1)
    else:
        Xk = np.hstack((Xp[-N:],Xp[0:N+1]))
        fk = f0*np.arange(-N,N+1)        
    return Xk, fk

def fs_approx(Xk,fk,t):
    """
    Synthesize periodic signal x(t) using Fourier series coefficients at harmonic frequencies

    Assume the signal is real so coefficients Xk are supplied for nonnegative
    indicies. The negative index coefficients are assumed to be complex
    conjugates.

    Parameters
    ----------
    Xk : ndarray of complex Fourier series coefficients
    fk : ndarray of harmonic frequencies in Hz
    t : ndarray time axis corresponding to output signal array x_approx

    Returns
    -------
    x_approx : ndarray of periodic waveform approximation over time span t

    Examples
    --------
    >>> t = arange(0,2,.002)
    >>> # a 20% duty cycle pulse train
    >>> n = arange(0,20,1) # 0 to 19th harmonic
    >>> fk = 1*n % period = 1s
    >>> t, x_approx = fs_approx(Xk,fk,t)
    >>> plot(t,x_approx)
    """
    x_approx = np.zeros(len(t))
    for k,Xkk in enumerate(Xk):
        if fk[k] == 0:
            x_approx += Xkk*np.ones(len(t))
        else:
            x_approx += 2*np.abs(Xkk)*np.cos(2*np.pi*fk[k]*t+np.angle(Xkk))
    return x_approx
    
def conv_sum(x1,nx1,x2,nx2,extent=('f','f')):
    """ 
    Discrete convolution of x1 and x2 with proper tracking of the output time axis.

    Convolve two discrete-time signals using the SciPy function signal.convolution.
    The time (sequence axis) are managed from input to output. y[n] = x1[n]*x2[n].

    Parameters
    ----------
    x1 : ndarray of signal x1 corresponding to nx1
    nx1 : ndarray time axis for x1
    x2  : ndarray of signal x2 corresponding to nx2
    nx2 : ndarray time axis for x2
    extent : ('e1','e2') where 'e1', 'e2' may be 'f' finite, 'r' right-sided, or 'l' left-sided

    Returns
    -------
    y : ndarray of output values y
    ny : ndarray of the corresponding sequence index n

    Notes
    -----
    The output time axis starts at the sum of the starting values in x1 and x2 
    and ends at the sum of the two ending values in x1 and x2. The default 
    extents of ('f','f') are used for signals that are active (have support) 
    on or within n1 and n2 respectively. A right-sided signal such as 
    a^n*u[n] is semi-infinite, so it has extent 'r' and the
    convolution output will be truncated to display only the valid results.

    Examples
    --------
    >>> nx = arange(-5,10)
    >>> x = drect(nx,4) 
    >>> y,ny = conv_sum(x,nx,x,nx)
    >>> stem(ny,y)
    >>> # Consider a pulse convolved with an exponential ('r' type extent)
    >>> h = 0.5**nx*dstep(nx)
    >>> y,ny = conv_sum(x,nx,h,nx,('f','r')) # note extents set
    >>> stem(ny,y) # expect a pulse charge and discharge sequence
    """
    nnx1 = np.arange(0,len(nx1))
    nnx2 = np.arange(0,len(nx2))
    n1 = nnx1[0]
    n2 = nnx1[-1]
    n3 = nnx2[0]
    n4 = nnx2[-1]
    # Start by finding the valid output support or extent interval to insure that
    # for no finite extent signals ambiquous results are not returned.
    # Valid extents are f (finite), r (right-sided), and l (left-sided)
    if extent[0] == 'f' and extent[1] == 'f':
        nny = np.arange(n1+n3,n2+1+n4+1-1)
        ny = np.arange(0,len(x1)+len(x2)-1) + nx1[0]+nx2[0]
    elif extent[0] == 'f' and extent[1] == 'r':
        nny = np.arange(n1+n3,n1+1+n4+1-1)
        ny = nny + nx1[0]+nx2[0]
    elif extent[0] == 'r' and extent[1] == 'f':
        nny = np.arange(n1+n3,n2+1+n3+1-1)
        ny = nny + nx1[0]+nx2[0]
    elif extent[0] == 'f' and extent[1] == 'l':
        nny = np.arange(n2+n3,n2+1+n4+1-1)
        ny = nny + nx1[-1]+nx2[0]
    elif extent[0] == 'l' and extent[1] == 'f':
        nny = np.arange(n1+n4,n2+1+n4+1-1)
        ny = nny + tx1[0]+tx2[-1]
    elif extent[0] == 'r' and extent[1] == 'r':
        nny = np.arange(n1+n3,min(n1+1+n4+1,n2+1+n3+1)-1)
        ny = nny + nx1[0]+nx2[0]
    elif extent[0] == 'l' and extent[1] == 'l':
        nny = np.arange(max(n1+n4,n2+n3),n2+1+n4+1-1)
        ny = nny + max(nx1[0]+nx2[-1],nx1[-1]+nx2[0])
    else:
        print 'Invalid x1 x2 extents specified or valid extent not found!'
        return 0,0
    # Finally convolve the sequences
    y = signal.convolve(x1, x2)
    print "Output support: (%+d, %+d)" % (ny[0],ny[-1])
    return y[nny], ny

def conv_integral(x1,tx1,x2,tx2,extent = ('f','f')):
    """ 
    Continuous-time convolution of x1 and x2 with proper tracking of the output time axis.

    Appromimate the convolution integral for the convolution of two continuous-time signals using the SciPy function signal. The time (sequence axis) are managed from input to output. y(t) = x1(t)*x2(t).

    Parameters
    ----------
    x1 : ndarray of signal x1 corresponding to tx1
    tx1 : ndarray time axis for x1
    x2  : ndarray of signal x2 corresponding to tx2
    tx2 : ndarray time axis for x2
    extent : ('e1','e2') where 'e1', 'e2' may be 'f' finite, 'r' right-sided, or 'l' left-sided

    Returns
    -------
    y : ndarray of output values y
    ty : ndarray of the corresponding time axis for y

    Notes
    -----
    The output time axis starts at the sum of the starting values in x1 and x2 
    and ends at the sum of the two ending values in x1 and x2. The time steps used in
    x1(t) and x2(t) must match. The default extents of ('f','f') are used for signals
    that are active (have support) on or within t1 and t2 respectively. A right-sided
    signal such as exp(-a*t)*u(t) is semi-infinite, so it has extent 'r' and the
    convolution output will be truncated to display only the valid results.

    Examples
    --------
    >>> tx = arange(-5,10,.01)
    >>> x = rect(tx-2,4) # pulse starts at t = 0 
    >>> y,ty = conv_integral(x,tx,x,tx)
    >>> plot(ty,y) # expect a triangle on [0,8]
    >>> # Consider a pulse convolved with an exponential ('r' type extent)
    >>> h = 4*exp(-4*tx)*step(tx)
    >>> y,ty = conv_integral(x,tx,h,tx,extent=('f','r')) # note extents set
    >>> plot(ty,y) # expect a pulse charge and discharge waveform
    """
    dt = tx1[1] - tx1[0]
    nx1 = np.arange(0,len(tx1))
    nx2 = np.arange(0,len(tx2))
    n1 = nx1[0]
    n2 = nx1[-1]
    n3 = nx2[0]
    n4 = nx2[-1]
    # Start by finding the valid output support or extent interval to insure that
    # for no finite extent signals ambiquous results are not returned.
    # Valid extents are f (finite), r (right-sided), and l (left-sided)
    if extent[0] == 'f' and extent[1] == 'f':
        ny = np.arange(n1+n3,n2+1+n4+1-1)
        ty = np.arange(0,len(x1)+len(x2)-1)*dt + tx1[0]+tx2[0]
    elif extent[0] == 'f' and extent[1] == 'r':
        ny = np.arange(n1+n3,n1+1+n4+1-1)
        ty = ny*dt + tx1[0]+tx2[0]
    elif extent[0] == 'r' and extent[1] == 'f':
        ny = np.arange(n1+n3,n2+1+n3+1-1)
        ty = ny*dt + tx1[0]+tx2[0]
    elif extent[0] == 'f' and extent[1] == 'l':
        ny = np.arange(n2+n3,n2+1+n4+1-1)
        ty = ny*dt + tx1[-1]+tx2[0]
    elif extent[0] == 'l' and extent[1] == 'f':
        ny = np.arange(n1+n4,n2+1+n4+1-1)
        ty = ny*dt + tx1[0]+tx2[-1]
    elif extent[0] == 'r' and extent[1] == 'r':
        ny = np.arange(n1+n3,min(n1+1+n4+1,n2+1+n3+1)-1)
        ty = ny*dt + tx1[0]+tx2[0]
    elif extent[0] == 'l' and extent[1] == 'l':
        ny = np.arange(max(n1+n4,n2+n3),n2+1+n4+1-1)
        ty = ny*dt + max(tx1[0]+tx2[-1],tx1[-1]+tx2[0])
    else:
        print 'Invalid x1 x2 extents specified or valid extent not found!'
        return 0,0
    # Finally convolve the sampled sequences and scale by dt
    y = signal.convolve(x1, x2)*dt
    print "Output support: (%+2.2f, %+2.2f)" % (ty[0],ty[-1])
    return y[ny], ty

def delta_eps(t,eps):
    """
    Rectangular pulse approximation to impulse function.
    
    Parameters
    ----------
    t : ndarray of time axis
    eps : pulse width
    
    Returns
    -------
    d : ndarray containing the impulse approximation
    
    Examples
    --------
    >>> t = arange(-2,2,.001)
    >>> d = delta_eps(t,.1)
    >>> plot(t,d)
    """
    d = np.zeros(len(t))
    for k,tt in enumerate(t):
        if abs(tt) <= eps/2.:
            d[k] = 1/float(eps)
    return d

def step(t):
    """
    Approximation to step function signal u(t).
    
    In this numerical version of u(t) the step turns on at t = 0.
    
    Parameters
    ----------
    t : ndarray of the time axis
    
    Returns
    -------
    x : ndarray of the step function signal u(t)
    
    Examples
    --------
    >>> t = arange(-1,5,.01)
    >>> x = step(t)
    >>> plot(t,x)
    >>> # to turn on at t = 1 shift t
    >>> x = step(t - 1.0)
    >>> plot(t,x)
    """
    x = np.zeros(len(t))
    for k,tt in enumerate(t):
        if tt >= 0:
            x[k] = 1.0
    return x

def rect(t,tau):
    """
    Approximation to the rectangle pulse Pi(t/tau).
    
    In this numerical version of Pi(t/tau) the pulse is active
    over -tau/2 <= t <= tau/2.
    
    Parameters
    ----------
    t : ndarray of the time axis
    tau : the pulse width
    
    Returns
    -------
    x : ndarray of the signal Pi(t/tau)
    
    Examples
    --------
    >>> t = arange(-1,5,.01)
    >>> x = rect(t,1.0)
    >>> plot(t,x)
    >>> # to turn on at t = 1 shift t
    >>> x = rect(t - 1.0,1.0)
    >>> plot(t,x)
    """
    x = np.zeros(len(t))
    for k,tk in enumerate(t):
        if np.abs(tk) > tau/2.:
            x[k] = 0
        else:
            x[k] = 1
    return x

def tri(t,tau):
    """
    Approximation to the triangle pulse Lambda(t/tau).
    
    In this numerical version of Lambda(t/tau) the pulse is active
    over -tau <= t <= tau.
    
    Parameters
    ----------
    t : ndarray of the time axis
    tau : one half the triangle base width
    
    Returns
    -------
    x : ndarray of the signal Lambda(t/tau)
    
    Examples
    --------
    >>> t = arange(-1,5,.01)
    >>> x = tri(t,1.0)
    >>> plot(t,x)
    >>> # to turn on at t = 1 shift t
    >>> x = tri(t - 1.0,1.0)
    >>> plot(t,x)   
    """
    x = np.zeros(len(t))
    for k,tk in enumerate(t):
        if np.abs(tk) > tau/1.:
            x[k] = 0
        else:
            x[k] = 1 - np.abs(tk)/tau
    return x

def dimpulse(n):
    """
    Discrete impulse function delta[n].
    
    Parameters
    ----------
    n : ndarray of the time axis
    
    Returns
    -------
    x : ndarray of the signal delta[n]
    
    Examples
    --------
    >>> n = arange(-5,5)
    >>> x = dimpulse(n)
    >>> stem(n,x)
    >>> # shift the delta left by 2
    >>> x = dimpulse(n+2)
    >>> stem(n,x)
    """
    x = np.zeros(len(n))
    for k,nn in enumerate(n):
        if nn == 0:
            x[k] = 1.0
    return x

def dstep(n):
    """
    Discrete step function u[n].
    
    Parameters
    ----------
    n : ndarray of the time axis
    
    Returns
    -------
    x : ndarray of the signal u[n]
    
    Examples
    --------
    >>> n = arange(-5,5)
    >>> x = dstep(n)
    >>> stem(n,x)
    >>> # shift the delta left by 2
    >>> x = dstep(n+2)
    >>> stem(n,x)
    """
    x = np.zeros(len(n))
    for k,nn in enumerate(n):
        if nn >= 0:
            x[k] = 1.0
    return x

def drect(n,N):
    """
    Discrete rectangle function of duration N samples.
    
    The signal is active on the interval 0 <= n <= N-1. Also known
    as the rectangular window function, which is available in 
    scipy.signal.
    
    Parameters
    ----------
    n : ndarray of the time axis
    N : the pulse duration
    
    Returns
    -------
    x : ndarray of the signal
    
    Notes
    -----
    The discrete rectangle turns on at n = 0, off at n = N-1 and 
    has duration of exactly N samples.
    
    Examples
    --------
    >>> n = arange(-5,5)
    >>> x = drect(n)
    >>> stem(n,x)
    >>> # shift the delta left by 2
    >>> x = drect(n+2)
    >>> stem(n,x)
    """ 
    x = np.zeros(len(n))
    for k,nn in enumerate(n):
        if nn >= 0 and nn < N:
            x[k] = 1.0
    return x

def rc_imp(Ns,alpha,M=6):
    """
    A truncated raised cosine pulse used in digital communications.

    The pulse shaping factor 0< alpha < 1 is required as well as the 
    truncation factor M which sets the pulse duration to be 2*M*Tsymbol.

    Parameters
    ----------
    Ns : number of samples per symbol
    alpha : excess bandwidth factor on (0, 1), e.g., 0.35
    M : equals RC one-sided symbol truncation factor

    Returns
    -------
    b : ndarray containing the pulse shape

    Notes
    -----
    The pulse shape b is typically used as the FIR filter coefficients
    when forming a pulse shaped digital communications waveform.

    Examples
    --------
    >>> # ten samples per symbol and alpha = 0.35
    >>> b = rc_imp(10,0.35)
    >>> n = arange(-10*6,10*6+1)
    >>> stem(n,b)
    """
    # Design the filter
    n = np.arange(-M*Ns,M*Ns+1)
    b = np.zeros(len(n));
    a = alpha;
    Ns *= 1.0
    for i in range(len(n)):
        if (1 - 4*(a*n[i]/Ns)**2) == 0:
            b[i] = np.pi/4*np.sinc(1/(2.*a))
        else:
            b[i] = np.sinc(n[i]/Ns)*np.cos(np.pi*a*n[i]/Ns)/(1 - 4*(a*n[i]/Ns)**2)
    return b

def sqrt_rc_imp(Ns,alpha,M=6):
    """
    A truncated square root raised cosine pulse used in digital communications.

    The pulse shaping factor 0< alpha < 1 is required as well as the 
    truncation factor M which sets the pulse duration to be 2*M*Tsymbol.
     

    Parameters
    ----------
    Ns : number of samples per symbol
    alpha : excess bandwidth factor on (0, 1), e.g., 0.35
    M : equals RC one-sided symbol truncation factor

    Returns
    -------
    b : ndarray containing the pulse shape

    Notes
    -----
    The pulse shape b is typically used as the FIR filter coefficients
    when forming a pulse shaped digital communications waveform. When 
    square root raised cosine (SRC) pulse is used generate Tx signals and 
    at the receiver used as a matched filter (receiver FIR filter), the 
    received signal is now raised cosine shaped, this having zero 
    intersymbol interference and the optimum removal of additive white 
    noise if present at the receiver input.

    Examples
    --------
    >>> # ten samples per symbol and alpha = 0.35
    >>> b = sqrt_rc_imp(10,0.35)
    >>> n = arange(-10*6,10*6+1)
    >>> stem(n,b)
    """
    # Design the filter
    n = np.arange(-M*Ns,M*Ns+1)
    b = np.zeros(len(n))
    Ns *= 1.0
    a = alpha
    for i in range(len(n)):
       if abs(1 - 16*a**2*(n[i]/Ns)**2) <= np.finfo(np.float).eps/2:
           b[i] = 1/2.*((1+a)*np.sin((1+a)*np.pi/(4.*a))-(1-a)*np.cos((1-a)*np.pi/(4.*a))+(4*a)/np.pi*np.sin((1-a)*np.pi/(4.*a)))
       else:
           b[i] = 4*a/(np.pi*(1 - 16*a**2*(n[i]/Ns)**2))
           b[i] = b[i]*(np.cos((1+a)*np.pi*n[i]/Ns) + np.sinc((1-a)*n[i]/Ns)*(1-a)*np.pi/(4.*a))
    return b

def PN_gen(N_bits,m=5):
    """
    Maximal length sequence signal generator.

    Generates a sequence 0/1 bits of N_bit duration. The bits themselves
    are obtained from an m-sequence of length m. Available m-sequence
    (PN generators) include m = 2,3,...,12, & 16.
            
    Parameters
    ----------
    N_bits : the number of bits to generate
    m : the number of shift registers. 2,3, .., 12, & 16

    Returns
    -------
    PN : ndarray of the generator output over N_bits

    Notes
    -----
    The sequence is periodic having period 2**m - 1 (2^m - 1).

    Examples
    --------
    >>> # A 15 bit period signal nover 50 bits
    >>> PN = PN_gen(50,4)
    """
    c = m_seq(m)
    Q = len(c)
    max_periods = int(np.ceil(N_bits/float(Q)))
    PN = np.zeros(max_periods*Q)
    for k in range(max_periods):
        PN[k*Q:(k+1)*Q] = c
    PN = np.resize(PN, (1,N_bits))
    return PN.flatten()

def m_seq(m):
    """
    Generate an m-sequence ndarray using an all-ones initialization.

    Available m-sequence (PN generators) include m = 2,3,...,12, & 16.

    Parameters
    ----------
    m : the number of shift registers. 2,3, .., 12, & 16

    Returns
    -------
    c : ndarray of one period of the m-sequence

    Notes
    -----
    The sequence period is 2**m - 1 (2^m - 1).  

    Examples
    --------
    >>> c = m_seq(5)
    """
    # Load shift register with all ones to start
    sr = np.ones(m)
    # M-squence length is:
    Q = 2**m - 1
    c = np.zeros(Q)

    if m == 2:
        taps = np.array([1, 1, 1])
    elif m == 3:
        taps = np.array([1, 0, 1, 1])
    elif m == 4:
        taps = np.array([1, 0, 0, 1, 1])
    elif m == 5:
        taps = np.array([1, 0, 0, 1, 0, 1])
    elif m == 6:
        taps = np.array([1, 0, 0, 0, 0, 1, 1])
    elif m == 7:
        taps = np.array([1, 0, 0, 0, 1, 0, 0, 1])
    elif m == 8:
        taps = np.array([1, 0, 0, 0, 1, 1, 1, 0, 1])
    elif m == 9:
        taps = np.array([1, 0, 0, 0, 0, 1, 0, 0, 0, 1])
    elif m == 10:
        taps = np.array([1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1])
    elif m == 11:
        taps = np.array([1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1])
    elif m == 12:
        taps = np.array([1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 1])
    elif m == 16:
        taps = np.array([1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1])
    else:
        print 'Invalid length specified'
    for n in range(Q):
        tap_xor = 0
        c[n] = sr[-1]
        for k in range(1,m):
            if taps[k] == 1:
                tap_xor = np.bitwise_xor(tap_xor,np.bitwise_xor(int(sr[-1]),int(sr[m-1-k])))
        sr[1:] = sr[:-1]
        sr[0] = tap_xor
    return c

def BPSK_tx(N_bits,Ns,ach_fc=2.0,ach_lvl_dB=-100,pulse='rect',alpha = 0.25,M=6):
    """
    Genrates biphase shift keyed (BPSK) transmitter with adjacent channel interference.

    Generates three BPSK signals with rectangular or square root raised cosine (SRC) 
    pulse shaping of duration N_bits and Ns samples per bit. The desired signal is
    centered on f = 0, which the adjacent channel signals to the left and right
    are also generated at dB level relative to the desired signal. Used in the 
    digital communications Case Study supplement.

    Parameters
    ----------
    N_bits : the number of bits to simulate
    Ns : the number of samples per bit
    ach_fc : the frequency offset of the adjacent channel signals (default 2.0)
    ach_lvl_dB : the level of the adjacent channel signals in dB (default -100)
    pulse :the pulse shape 'rect' or 'src'
    alpha : square root raised cosine pulse shape factor (default = 0.25)
    M : square root raised cosine pulse truncation factor (default = 6)

    Returns
    -------
    x : ndarray of the composite signal x0 + ach_lvl*(x1p + x1m)
    b : the transmit pulse shape 
    data0 : the data bits used to form the desired signal; used for error checking

    Notes
    -----

    Examples
    --------
    >>> x,b,data0 = BPSK_tx(1000,10,'src')
    """
    x0,b,data0 = NRZ_bits(N_bits,Ns,pulse,alpha,M)
    x1p,b,data1p = NRZ_bits(N_bits,Ns,pulse,alpha,M)
    x1m,b,data1m = NRZ_bits(N_bits,Ns,pulse,alpha,M)
    n = np.arange(len(x0))
    x1p = x1p*np.exp(1j*2*np.pi*ach_fc/float(Ns)*n)
    x1m = x1m*np.exp(-1j*2*np.pi*ach_fc/float(Ns)*n)
    ach_lvl = 10**(ach_lvl_dB/20.)
    return x0 + ach_lvl*(x1p + x1m), b, data0
     
#def BPSK_rx(r,b,):

def NRZ_bits(N_bits,Ns,pulse='rect',alpha = 0.25,M=6):
    """
    Generate non-return-to-zero (NRZ) data bits with pulse shaping.

    A baseband digital data signal using +/-1 amplitude signal values
    and including pulse shaping.

    Parameters
    ----------
    N_bits : number of NRZ +/-1 data bits to produce
    Ns : the number of samples per bit,
    pulse_type : 'rect' , 'rc', 'src' (default 'rect')
    alpha : excess bandwidth factor(default 0.25)
    M : single sided pulse duration (default = 6) 

    Returns
    -------
    x : ndarray of the NRZ signal values
    b : ndarray of the pulse shape
    data : ndarray of the underlying data bits

    Notes
    -----
    Pulse shapes include 'rect' (rectangular), 'rc' (raised cosine), 
    'src' (root raised cosine). The actual pulse length is 2*M+1 samples.
    This function is used by BPSK_tx in the Case Study article.

    Examples
    --------
    >>> x,b,data = NRZ_bits(100,10)
    >>> t = arange(len(x))
    >>> plot(t,x)
    """
    data = np.random.randint(0,2,N_bits) 
    x = np.hstack((2*data.reshape(N_bits,1)-1,np.zeros((N_bits,Ns-1))))
    x =x.flatten()
    if pulse.lower() == 'rect':
        b = np.ones(Ns)
    elif pulse.lower() == 'rc':
        b = rc_imp(Ns,alpha,M)
    elif pulse.lower() == 'src':
        b = sqrt_rc_imp(Ns,alpha,M)
    else:
        print 'pulse type must be rec, rc, or src'
    x = signal.lfilter(b,1,x)
    return x,b/float(Ns),data

def NRZ_bits2(data,Ns,pulse='rect',alpha = 0.25,M=6):
    """
    Generate non-return-to-zero (NRZ) data bits with pulse shaping with user data

    A baseband digital data signal using +/-1 amplitude signal values
    and including pulse shaping. The data sequence is user supplied.

    Parameters
    ----------
    data : ndarray of the data bits as 0/1 values
    Ns : the number of samples per bit,
    pulse_type : 'rect' , 'rc', 'src' (default 'rect')
    alpha : excess bandwidth factor(default 0.25)
    M : single sided pulse duration (default = 6) 

    Returns
    -------
    x : ndarray of the NRZ signal values
    b : ndarray of the pulse shape

    Notes
    -----
    Pulse shapes include 'rect' (rectangular), 'rc' (raised cosine), 
    'src' (root raised cosine). The actual pulse length is 2*M+1 samples.

    Examples
    --------
    >>> x,b = NRZ_bits2([m_seq(5),10)
    >>> t = arange(len(x))
    >>> plot(t,x)
    """
    N_bits = len(data)
    x = np.hstack((2*data.reshape(N_bits,1)-1,np.zeros((N_bits,Ns-1))))
    x = x.flatten()
    if pulse.lower() == 'rect':
        b = np.ones(Ns)
    elif pulse.lower() == 'rc':
        b = rc_imp(Ns,alpha,M)
    elif pulse.lower() == 'src':
        b = sqrt_rc_imp(Ns,alpha,M)
    else:
        print 'pulse type must be rec, rc, or src'
    x = signal.lfilter(b,1,x)
    return x,b/float(Ns)

def eye_plot(x,L,S=0):
    """
    Eye pattern plot of a baseband digital communications waveform.

    The signal must be real, but can be multivalued in terms of the underlying
    modulation scheme. Used for BPSK eye plots in the Case Study article.

    Parameters
    ----------
    x : ndarray of the real input data vector/array
    L : display length in samples (usually two symbols)
    S : start index

    Returns
    -------
    Nothing : A plot window opens containing the eye plot
    
    Notes
    -----
    Increase S to eliminate filter transients.
    
    Examples
    --------
    >>> # 1000 bits at 10 samples per bit with 'rc' shaping
    >>> x,b, data = NRZ_bits(1000,10,'rc')
    >>> eye_plot(x,20,60)
    """
    plt.figure(figsize=(6,4))
    idx = np.arange(0,L+1)
    plt.plot(idx,x[S:S+L+1],'b')
    k_max = int((len(x) - S)/L)-1
    for k in range(1,k_max):
         plt.plot(idx,x[S+k*L:S+L+1+k*L],'b')
    plt.grid()
    plt.xlabel('Time Index - n')
    plt.ylabel('Amplitude')
    plt.title('Eye Plot')
    return 0

def scatter(x,Ns,start):
    """
    Sample a baseband digital communications waveform at the symbol spacing.

    Parameters
    ----------
    x : ndarray of the input digital comm signal
    Ns : number of samples per symbol (bit)
    start : the array index to start the sampling

    Returns
    -------
    xI : ndarray of the real part of x following sampling
    xQ : ndarray of the imaginary part of x following sampling

    Notes
    -----
    Normally the signal is complex, so the scatter plot contains 
    clusters at point  in the complex plane. For a binary signal 
    such as BPSK, the point centers are nominally +/-1 on the real
    axis. Start is used to eliminate transients from the FIR
    pulse shaping filters from appearing in the scatter plot.

    Examples
    --------
    >>> x,b, data = NRZ_bits(1000,10,'rc')
    >>> # add some noise so points are now scattered about +/-1
    >>>  y = cpx_AWGN(x,20,10)
    >>>  yI,yQ = scatter(y,10,60)
    >>> plot(yI,yQ,'.')
    >>> axis('equal')
    """
    xI = np.real(x[start::Ns])
    xQ = np.imag(x[start::Ns])
    return xI, xQ

def bit_errors(z,data,start,Ns):
    """
    A simple bit error counting function.
    
    In its present form this function counts bit errors between
    hard decision BPSK bits in +/-1 form and compares them with
    0/1 binary data that was transmitted. Timing between the Tx
    and Rx data is the responsibility of the user. An enhanced 
    version of this function, which features automatic synching
    will be created in the future.

    Parameters
    ----------
    z : ndarray of hard decision BPSK data prior to symbol spaced sampling
    data : ndarray of reference bits in 1/0 format
    start : timing reference for the received
    Ns : the number of samples per symbol

    Returns
    -------
    Pe_hat : the estimated probability of a bit error

    Notes
    -----
    The Tx and Rx data streams are exclusive-or'd and the then the bit errors
    are summed, and finally divided by the number of bits observed to form an
    estimate of the bit error probability. This function needs to be 
    enhanced to be more useful.

    Examples
    --------
    >>> from scipy import signal
    >>> x,b, data = NRZ_bits(1000,10)
    >>> # set Eb/N0 to 8 dB
    >>>  y = cpx_AWGN(x,8,10)
    >>> # matched filter the signal
    >>> z = signal.lfilter(b,1,y)
    >>> # make bit decisions at 10 and Ns multiples thereafter
    >>> Pe_hat = bit_errors(z,data,10,10)
    """
    Pe_hat = np.sum(data[0:len(z[start::Ns])]^np.int64((np.sign(np.real(z[start::Ns]))+1)/2))/float(len(z[start::Ns]))
    return Pe_hat

def cpx_AWGN(x,EsN0,Ns):
    """
    Apply white Gaussian noise to a digital communications signal.

    This function represents a complex baseband white Gaussian noise
    digital communications channel. The input signal array may be real
    or complex.

    Parameters
    ----------
    x : ndarray noise free complex baseband input signal.
    EsNO : set the channel Es/N0 (Eb/N0 for binary) level in dB
    Ns : number of samples per symbol (bit)

    Returns
    -------
    y : ndarray x with additive noise added.

    Notes
    -----
    Set the channel energy per symbol-to-noise power spectral 
    density ratio (Es/N0) in dB.

    Examples
    --------
    >>> x,b, data = NRZ_bits(1000,10)
    >>> # set Eb/N0 = 10 dB
    >>> y = cpx_AWGN(x,10,10)
    """
    w = np.sqrt(Ns*np.var(x)*10**(-EsN0/10.)/2.)*(np.random.randn(len(x)) + 1j*np.random.randn(len(x)))                            
    return x+w       

def my_psd(x,NFFT=2**10,Fs=1):
    """
    A local version of NumPy's PSD function that returns the plot arrays.

    A mlab.psd wrapper function that returns two ndarrays;
    makes no attempt to auto plot anything.

    Parameters
    ----------
    x : ndarray input signal
    NFFT : a power of two, e.g., 2**10 = 1024
    Fs : the sampling rate in Hz

    Returns
    -------
    Px : ndarray of the power spectrum estimate
    f : ndarray of frequency values
    
    Notes
    -----
    This function makes it easier to overlay spectrum plots because
    you have better control over the axis scaling than when using psd()
    in the autoscale mode.
    
    Examples
    --------
    >>> x,b, data = NRZ_bits(10000,10)
    >>> Px,f = my_psd(x,2**10,10)
    >>> plot(f, 10*log10(Px))

    """
    Px,f = pylab.mlab.psd(x,NFFT,Fs)
    return Px.flatten(), f
    

def am_tx(m,a_mod,fc=75e3):
    """
    AM transmitter for Case Study of Chapter 17.

    Assume input is sampled at 8 Ksps and upsampling
    by 24 is performed to arrive at fs_out = 192 Ksps.

    Parameters
    ----------
    m : ndarray of the input message signal 
    a_mod : AM modulation index, between 0 and 1
    fc : the carrier frequency in Hz

    Returns
    -------
    x192 : ndarray of the upsampled by 24 and modulated carrier
    t192 : ndarray of the upsampled by 24 time axis
    m24 : ndarray of the upsampled by 24 message signal

    Notes
    -----
    The sampling rate of the input signal is assumed to be 8 kHz.

    Examples
    --------
    >>> n = arange(0,1000)
    >>> # 1 kHz message signal
    >>> m = cos(2*pi*1000/8000.*n)
    >>> x192, t192 = am_tx(m,0.8,fc=75e3)
    """
    m24 = interp24(m)
    t192 = np.arange(len(m24))/192.0e3
    #m24 = np.cos(2*np.pi*2.0e3*t192)
    m_max = np.max(np.abs(m24))
    x192 = (1 + a_mod*m24/m_max)*np.cos(2*np.pi*fc*t192) 
    return x192, t192, m24

def am_rx(x192):
    """
    AM envelope detector receiver for the Chapter 17 Case Study
    
    The receiver bandpass filter is not included in this function.
    
    Parameters
    ----------
    x192 : ndarray of the AM signal at sampling rate 192 ksps
    
    Returns
    -------
    m_rx8 : ndarray of the demodulated message at 8 ksps
    t8 : ndarray of the time axis at 8 ksps
    m_rx192 : ndarray of the demodulated output at 192 ksps
    x_edet192 : ndarray of the envelope detector output at 192 ksps
    
    Notes
    -----
    The bandpass filter needed at the receiver front-end can be designed
    using b_bpf,a_bpf = am_rx_BPF().
    
    Examples
    --------
    >>> n = arange(0,1000)
    >>> # 1 kHz message signal
    >>> m = cos(2*pi*1000/8000.*n)
    >>> m_rx8,t8,m_rx192,x_edet192 = am_rx(x192)
    """
    x_edet192 = env_det(x192)
    m_rx8 = deci24(x_edet192)
    # remove DC offset from the env_det + LPF output
    m_rx8 -= np.mean(m_rx8)
    t8 = np.arange(len(m_rx8))/8.0e3
    """
    For performance testing also filter x_env_det
    192e3 using a Butterworth cascade.
    The filter cutoff is 5kHz, the message BW.
    """
    b192,a192 = signal.butter(5,2*5.0e3/192.0e3)
    m_rx192 = signal.lfilter(b192,a192,x_edet192)
    m_rx192 = signal.lfilter(b192,a192,m_rx192)
    m_rx192 -= np.mean(m_rx192)
    return m_rx8,t8,m_rx192,x_edet192
    
def am_rx_BPF(N_order = 7, ripple_dB = 1, B = 10e3, fs = 192e3):
    """
    Bandpass filter design for the AM receiver Case Study of Chapter 17.

    Design a 7th-order Chebyshev type 1 bandpass filter to remove/reduce
    adjacent channel intereference at the envelope detector input.
    
    Parameters
    ----------
    N_order : the filter order (default = 7)
    ripple_dB : the passband ripple in dB (default = 1)
    B : the RF bandwidth (default = 10e3)
    fs : the sampling frequency 

    Returns
    -------
    b_bpf : ndarray of the numerator filter coefficients
    a_bpf : ndarray of the denominator filter coefficients

    Examples
    --------
    >>> from scipy import signal
    >>> # Use the default values
    >>> b_bpf,a_bpf = am_rx_BPF()
    >>> # plot the filter pole-zero plot
    >>> zplane(b_bpf,a_bpf)
    >>> # plot the frequency response
    >>> f = arange(0,192/2.,.1)
    >>> w, Hbpf = signal.freqz(b_bpf,a_bpf,2*pi*f/192)
    >>> plot(f,20*log10(abs(Hbpf)))
    >>> axis([0,192/2.,-80,10])
    """
    b_bpf,a_bpf = signal.cheby1(N_order,ripple_dB,2*np.array([75e3-B/2.,75e3+B/2.])/fs,'bandpass')
    return b_bpf,a_bpf
    
    
def env_det(x):
    """
    Ideal envelope detector.

    This function retains the positive half cycles of the input signal.

    Parameters
    ----------
    x : ndarray of the input sugnal

    Returns
    -------
    y : ndarray of the output signal

    Examples
    --------
    >>> n = arange(0,100)
    >>> # 1 kHz message signal
    >>> m = cos(2*pi*1000/8000.*n)
    >>> x192, t192 = am_tx(m,0.8,fc=75e3)
    >>> y = env_det(x192)
    """
    y = np.zeros(len(x))
    for k,xx in enumerate(x):
        if xx >= 0:
            y[k] = xx
    return y 

def interp24(x):
    """
    Interpolate by L = 24 using Butterworth filters.

    The interpolation is done using three stages. Upsample by 
    L = 2 and lowpass filter, upsample by 3 and lowpass filter, then
    upsample by L = 4 and lowpass filter. In all cases the lowpass
    filter is a 10th-order Butterworth lowpass.

    Parameters
    ----------
    x : ndarray of the input signal

    Returns
    -------
    y : ndarray of the output signal

    Notes
    -----
    The cutoff frequency of the lowpass filters is 1/2, 1/3, and 1/4 to 
    track the upsampling by 2, 3, and 4 respectively.

    Examples
    --------
    >>> y = interp24(x)
    """
    # Stage 1: L = 2
    b2,a2 = signal.butter(10,1/2.)
    y1 = upsample(x,2)
    y1 = signal.lfilter(b2,a2,2*y1)

    # Stage 2: L = 3
    b3,a3 = signal.butter(10,1/3.)
    y2 = upsample(y1,3)
    y2 = signal.lfilter(b3,a3,3*y2)

    # Stage 3: L = 4
    b4,a4 = signal.butter(10,1/4.)
    y3 = upsample(y2,4)
    y3 = signal.lfilter(b4,a4,4*y3)
    return y3

def deci24(x):
    """
    Decimate by L = 24 using Butterworth filters.
    
    The decimation is done using two three stages. Downsample sample by 
    L = 2 and lowpass filter, downsample by 3 and lowpass filter, then
    downsample by L = 4 and lowpass filter. In all cases the lowpass
    filter is a 10th-order Butterworth lowpass.
    
    Parameters
    ----------
    x : ndarray of the input signal
    
    Returns
    -------
    y : ndarray of the output signal
    
    Notes
    -----
    The cutoff frequency of the lowpass filters is 1/2, 1/3, and 1/4 to 
    track the upsampling by 2, 3, and 4 respectively.
    
    Examples
    --------
    >>> y = deci24(x)
    """
    # Stage 1: M = 2
    b2,a2 = signal.butter(10,1/2.)
    y1 = signal.lfilter(b2,a2,x)
    y1 = downsample(y1,2)
    
    # Stage 2: M = 3
    b3,a3 = signal.butter(10,1/3.)
    y2 = signal.lfilter(b3,a3,y1)
    y2 = downsample(y2,3)

    # Stage 3: L = 4
    b4,a4 = signal.butter(10,1/4.)
    y3 = signal.lfilter(b4,a4,y2)
    y3 = downsample(y3,4)
    return y3

def upsample(x,L):
    """
    Upsample by factor L
    
    Insert L - 1 zero samples in between each input sample.
    
    Parameters
    ----------
    x : ndarray of input signal values
    L : upsample factor
    
    Returns
    -------
    y : ndarray of the output signal values
    
    Examples
    --------
    >>> y = upsample(x,3)
    """
    N_input = len(x)
    y = np.hstack((x.reshape(N_input,1),np.zeros((N_input,L-1))))
    y = y.flatten()
    return y

def downsample(x,M,p=0):
    """
    Downsample by factor M

    Keep every Mth sample of the input. The phase of the input samples
    kept can be selected.

    Parameters
    ----------
    x : ndarray of input signal values
    M : upsample factor
    p : phase of decimated value, 0 (default), 1, ..., M-1

    Returns
    -------
    y : ndarray of the output signal values

    Examples
    --------
    >>> y = downsample(x,3)
    >>> y = downsample(x,3,1)
    """
    x = x[0:np.floor(len(x)/M)*M]
    x = x.reshape((len(x)/M,M))
    y = x[:,p]
    return y

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

def zplane(b,a,auto_scale=True,size=2,detect_mult=True,tol=0.001):
    """
    Create an z-plane pole-zero plot.

    Create an z-plane pole-zero plot using the numerator
    and denominator z-domain system function coefficient
    ndarrays b and a respectively. Assume descending powers of z.

    Parameters
    ----------
    b : ndarray of the numerator coefficients
    a : ndarray of the denominator coefficients
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
    >>> zplane(b,a)
    >>> # Here the plot is generated using manual scaling
    >>> zplane(b,a,False,1.5)
    """
    M = len(b) - 1
    N = len(a) - 1
    # Plot labels if multiplicity greater than 1
    x_scale = 1.5*size
    y_scale = 1.5*size   
    x_off = 0.02
    y_off = 0.01
    #N_roots = np.array([1.0])
    if M > 0:
        N_roots = np.roots(b)
    #D_roots = np.array([1.0])
    if N > 0:
        D_roots = np.roots(a)
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
        if detect_mult == True:
            N_uniq, N_mult = unique_cpx_roots(N_roots,tol=tol)
            plt.plot(np.real(N_uniq),np.imag(N_uniq),'ko',mfc='None',ms=8)
            idx_N_mult = mlab.find(N_mult>1)
            for k in range(len(idx_N_mult)):
                x_loc = np.real(N_uniq[idx_N_mult[k]]) + x_off*x_scale
                y_loc =np.imag(N_uniq[idx_N_mult[k]]) + y_off*y_scale
                plt.text(x_loc,y_loc,str(N_mult[idx_N_mult[k]]),ha='center',va='bottom',fontsize=10)
        else:
            plt.plot(np.real(N_roots),np.imag(N_roots),'ko',mfc='None',ms=8)                
    if N > 0:
        if detect_mult == True:
            D_uniq, D_mult=unique_cpx_roots(D_roots,tol=tol)
            plt.plot(np.real(D_uniq),np.imag(D_uniq),'kx',ms=8)
            idx_D_mult = mlab.find(D_mult>1)
            for k in range(len(idx_D_mult)):
                x_loc = np.real(D_uniq[idx_D_mult[k]]) + x_off*x_scale
                y_loc =np.imag(D_uniq[idx_D_mult[k]]) + y_off*y_scale
                plt.text(x_loc,y_loc,str(D_mult[idx_D_mult[k]]),ha='center',va='bottom',fontsize=10)            
        else:
            plt.plot(np.real(D_roots),np.imag(D_roots),'kx',ms=8)                
    if M - N < 0:
        plt.plot(0.0,0.0,'bo',mfc='None',ms=8)
    elif M - N > 0:
        plt.plot(0.0,0.0,'kx',ms=8)
    if abs(M - N) > 1:
        plt.text(x_off*x_scale,y_off*y_scale,str(abs(M-N)),ha='center',va='bottom',fontsize=10)        
    plt.xlabel('Real Part')
    plt.ylabel('Imaginary Part')
    plt.title('Pole-Zero Plot')
    #plt.grid()
    plt.axis([-size,size,-size,size])
    return M,N


def rect_conv(n,N_len):
    """
    The theoretical result of convolving two rectangle sequences.
    
    The result is a triangle. The solution is
    based on pure analysis. Simply coded as opposed 
    to efficiently coded.
    
    Parameters
    ----------
    n : ndarray of time axis
    N_len : rectangle pulse duration
    
    Returns
    -------
    y : ndarray of of output signal
    
    Examples
    --------
    >>> n = arange(-5,20)
    >>> y = rect_conv(n,6)
    """
    y = np.zeros(len(n))
    for k in range(len(n)):
        if n[k] >= 0 and n[k] < N_len-1:
            y[k] = n[k] + 1
        elif n[k] >= N_len-1 and n[k] <= 2*N_len-2:
            y[k] = 2*N_len-1-n[k]
            
    return y
             

def biquad2(w_num, r_num, w_den, r_den):
    """
    A biquadratic filter in terms of conjugate pole and zero pairs.

    Parameters
    ----------
    w_num : zero frequency (angle) in rad/sample
    r_num : conjugate zeros radius
    w_den : pole frequency (angle) in rad/sample
    r_den : conjugate poles radius; less than 1 for stability

    Returns
    -------
    b : ndarray of numerator coefficients
    a : ndarray of denominator coefficients

    Examples
    --------
    b,a = biquad2(pi/4., 1, pi/4., 0.95)
    """
    b = np.array([1, -2*r_num*np.cos(w_num), r_num**2])
    a = np.array([1, -2*r_den*np.cos(w_den), r_den**2])
    return b, a

def plot_na(x,y,mode='stem'):
    pylab.figure(figsize=(5,2))
    frame1 = pylab.gca()
    if mode.lower() == 'stem':
         pylab.stem(x,y)
    else:
        pylab.plot(x,y)
    
    frame1.axes.get_xaxis().set_visible(False)
    frame1.axes.get_yaxis().set_visible(False) 
    pylab.show()

def from_wav(filename):
    """
    Read a wave file.

    A wrapper function for scipy.io.wavfile.read
    that also includes int16 to float [-1,1] scaling.

    Parameters
    ----------
    filename : file name string

    Returns
    -------
    fs : sampling frequency in Hz
    x : ndarray of normalized to 1 signal samples

    Examples
    --------
    >>> fs,x = from_wav('test_file.wav')
    """
    fs, x = wavfile.read(filename)
    return fs, x/32767.

def to_wav(filename,rate,x):
    """
    Write a wave file.

    A wrapper function for scipy.io.wavfile.write
    that also includes int16 scaling and conversion.
    Assume input x is [-1,1] values.

    Parameters
    ----------
    filename : file name string
    rate : sampling frequency in Hz

    Returns
    -------
    Nothing : writes only the *.wav file

    Examples
    --------
    >>> to_wav('test_file.wav', 8000, x)

    """
    x16 = np.int16(x*32767)
    wavfile.write(filename, rate, x16)

if __name__ == '__main__':
    
    b = CIC(10,1)
    print b
     
    
    """
    x = np.random.randn(10)
    print x
    
    b = signal.remez(16,[0,.1,.2,.5], [1,0], [1,1], 1)
    w,H = signal.freqz(b,[1],512)
    plot(w,20*log10(abs(H)))
    figure(figsize=(6,4))
    #plot(arange(0,len(b)),b)
    
    y = signal.lfilter(b, [1], x,)
    print y
    
    zplane([1,1,1,1,1],[1,-.8],1.25)
    """
