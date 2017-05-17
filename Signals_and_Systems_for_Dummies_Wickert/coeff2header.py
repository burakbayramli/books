"""
Digital Filter Coefficient Conversion to C Header Files

Mark Wickert January 2015 - October 2016

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

def FIR_header(fname_out,h):
    """
    Write FIR Filter Header Files 
    
    Mark Wickert February 2015
    """
    M = len(h)
    N = 3 # Coefficients per line
    f = open(fname_out,'wt')
    f.write('//define a FIR coefficient Array\n\n')
    f.write('#include <stdint.h>\n\n')
    f.write('#ifndef M_FIR\n')
    f.write('#define M_FIR %d\n' % M)
    f.write('#endif\n')
    f.write('/************************************************************************/\n');
    f.write('/*                         FIR Filter Coefficients                      */\n');
    f.write('float32_t h_FIR[M_FIR] = {')
    kk = 0;
    for k in range(M):
        #k_mod = k % M
        if (kk < N-1) and (k < M-1):
            f.write('%15.12f,' % h[k])
            kk += 1
        elif (kk == N-1) & (k < M-1):
            f.write('%15.12f,\n' % h[k])
            if k < M:
                f.write('                          ')
                kk = 0
        else:
            f.write('%15.12f' % h[k])    
    f.write('};\n')
    f.write('/************************************************************************/\n')
    f.close()


def FIR_fix_header(fname_out,h):
    """
    Write FIR Fixed-Point Filter Header Files 
    
    Mark Wickert February 2015
    """
    M = len(h)
    hq = int16(rint(h*2**15))
    N = 8 # Coefficients per line
    f = open(fname_out,'wt')
    f.write('//define a FIR coefficient Array\n\n')
    f.write('#include <stdint.h>\n\n')
    f.write('#ifndef M_FIR\n')
    f.write('#define M_FIR %d\n' % M)
    f.write('#endif\n')
    f.write('/************************************************************************/\n');
    f.write('/*                         FIR Filter Coefficients                      */\n');
    f.write('int16_t h_FIR[M_FIR] = {')
    kk = 0;
    for k in range(M):
        #k_mod = k % M
        if (kk < N-1) and (k < M-1):
            f.write('%5d,' % hq[k])
            kk += 1
        elif (kk == N-1) & (k < M-1):
            f.write('%5d,\n' % hq[k])
            if k < M:
                f.write('                        ')
                kk = 0
        else:
            f.write('%5d' % hq[k])    
    f.write('};\n')
    f.write('/************************************************************************/\n')
    f.close()


def IIR_sos_header(fname_out,SOS_mat):
    """
    Write IIR SOS Header Files
    File format is compatible with CMSIS-DSP IIR 
    Directform II Filter Functions
    
    Mark Wickert March 2015-October 2016
    """
    Ns,Mcol = SOS_mat.shape
    f = open(fname_out,'wt')
    f.write('//define a IIR SOS CMSIS-DSP coefficient array\n\n')
    f.write('#include <stdint.h>\n\n')
    f.write('#ifndef STAGES\n')
    f.write('#define STAGES %d\n' % Ns)
    f.write('#endif\n')
    f.write('/*********************************************************/\n');
    f.write('/*                     IIR SOS Filter Coefficients       */\n');
    f.write('float32_t ba_coeff[%d] = { //b0,b1,b2,a1,a2,... by stage\n' % (5*Ns))
    for k in range(Ns):
        if (k < Ns-1):
            f.write('    %+-13e, %+-13e, %+-13e,\n' % \
                    (SOS_mat[k,0],SOS_mat[k,1],SOS_mat[k,2]))
            f.write('    %+-13e, %+-13e,\n' % \
                    (-SOS_mat[k,4],-SOS_mat[k,5]))
        else:
            f.write('    %+-13e, %+-13e, %+-13e,\n' % \
                    (SOS_mat[k,0],SOS_mat[k,1],SOS_mat[k,2]))
            f.write('    %+-13e, %+-13e\n' % \
                    (-SOS_mat[k,4],-SOS_mat[k,5]))
    # for k in range(Ns):
    #     if (k < Ns-1):
    #         f.write('    %15.12f, %15.12f, %15.12f,\n' % \
    #                 (SOS_mat[k,0],SOS_mat[k,1],SOS_mat[k,2]))
    #         f.write('    %15.12f, %15.12f,\n' % \
    #                 (-SOS_mat[k,4],-SOS_mat[k,5]))
    #     else:
    #         f.write('    %15.12f, %15.12f, %15.12f,\n' % \
    #                 (SOS_mat[k,0],SOS_mat[k,1],SOS_mat[k,2]))
    #         f.write('    %15.12f, %15.12f\n' % \
    #                 (-SOS_mat[k,4],-SOS_mat[k,5]))
    f.write('};\n')
    f.write('/*********************************************************/\n')
    f.close()



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
