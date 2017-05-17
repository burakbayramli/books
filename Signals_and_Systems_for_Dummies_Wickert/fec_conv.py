"""
A Convolutional Encoding and Decoding

Mark Wickert February 2014 - September 2014

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


"""
A forward error correcting coding (FEC) class which defines methods 
for performing convolutional encoding and decoding. Arbitrary 
polynomials are supported, but the rate is presently limited to r = 1/n,
where n = 2. Punctured (perforated) convolutional codes are also supported. 
The puncturing pattern (matrix) is arbitrary.

Two popular encoder polynomial sets are:

K = 3 ==> G1 = '111', G2 = '101' and 
K = 7 ==> G1 = '1011011', G2 = '1111001'.

A popular puncturing pattern to convert from rate 1/2 to rate 3/4 is
a G1 output puncture pattern of '110' and a G2 output puncture 
pattern of '101'.

Graphical display functions are included to allow the user to
better understand the operation of the Viterbi decoder.

Mark Wickert: February 2014.
"""

import numpy as np
from math import factorial
import matplotlib.pyplot as plt
import scipy.special as special
from sys import exit

# Data structure support classes
class trellis_nodes(object):
    """
    A structure to hold the trellis from nodes and to nodes.
    Ns is the number of states = 2**(K-1).
    """
    def __init__(self,Ns):
        self.Ns = Ns
        self.fn = np.zeros((Ns,1),dtype=int) 
        self.tn = np.zeros((Ns,1),dtype=int)
        self.out_bits = np.zeros((Ns,1),dtype=int)

class trellis_branches(object):
    """
    A structure to hold the trellis states, bits, and input values
    for both '1' and '0' transitions.
    Ns is the number of states = 2**(K-1).
    """
    def __init__(self,Ns):
        self.Ns = Ns
        self.states1 = np.zeros((Ns,1),dtype=int)
        self.states2 = np.zeros((Ns,1),dtype=int)
        self.bits1 = np.zeros((Ns,1),dtype=int)
        self.bits2 = np.zeros((Ns,1),dtype=int)
        self.input1 = np.zeros((Ns,1),dtype=int)
        self.input2 = np.zeros((Ns,1),dtype=int)

class trellis_paths(object):
    """
    A structure to hold the trellis paths in terms of traceback_states,
    cumulative_metrics, and traceback_bits. A full decision depth history
    of all this infomation is not essential, but does allow the graphical
    depiction created by the method traceback_plot().
    Ns is the number of states = 2**(K-1) and D is the decision depth.
    As a rule, D should be about 5 times K.
    """
    def __init__(self,Ns,D):
        self.Ns = Ns
        self.decision_depth = D
        self.traceback_states = np.zeros((Ns,self.decision_depth),dtype=int)
        self.cumulative_metric = np.zeros((Ns,self.decision_depth),dtype=float)
        self.traceback_bits = np.zeros((Ns,self.decision_depth),dtype=int)

def binary(num, length=8):
        """
        Format an integer to binary without the leading '0b'
        """
        return format(num, '0{}b'.format(length))

class fec_conv(object):
    def __init__(self,G = ('111','101'), Depth = 10):
        """
        cc1 = fec_conv(G = ('111','101'), Depth = 10)
        Make Rate 1/2 convolutional coder/decoder object. 
        Polys G1 and G2 are entered as binary strings, e.g,
        G1 = '111' and G2 = '101' for K = 3 and
        G1 = '1011011' and G2 = '1111001' for K = 7.

        Viterbi decoding has a decision depth of Depth.

        Data structures than manage the VA are created 
        upon instantiation via the __init__ method.

        Mark Wickert February 2014
        """
        self.G_polys = G
        self.constraint_length = len(self.G_polys[0]) 
        self.Nstates = 2**(self.constraint_length-1) # number of states
        self.decision_depth = Depth
        self.input_zero = trellis_nodes(self.Nstates)
        self.input_one = trellis_nodes(self.Nstates)
        self.paths = trellis_paths(self.Nstates,self.decision_depth)

        for m in range(self.Nstates):
            self.input_zero.fn[m] = m
            self.input_one.fn[m] = m
            # state labeling with LSB on right (more common)
            output0,state0 = self.conv_encoder([0],
                             binary(m,self.constraint_length-1))
            output1,state1 = self.conv_encoder([1],
                             binary(m,self.constraint_length-1))
            self.input_zero.tn[m] = int(state0,2)
            self.input_one.tn[m] = int(state1,2)
            self.input_zero.out_bits[m] = 2*output0[0] + output0[1]
            self.input_one.out_bits[m] = 2*output1[0] + output1[1]

        # Now organize the results into a branches_from structure that holds the
        # from state, the u2 u1 bit sequence in decimal form, and the input bit.
        # The index where this information is stored is the to state where survivors
        # are chosen from the two input branches.
        self.branches = trellis_branches(self.Nstates)

        for m in range(self.Nstates):
            match_zero_idx = np.where(self.input_zero.tn == m)
            match_one_idx = np.where(self.input_one.tn == m)
            if len(match_zero_idx[0]) != 0:
                self.branches.states1[m] = self.input_zero.fn[match_zero_idx[0][0]]
                self.branches.states2[m] = self.input_zero.fn[match_zero_idx[0][1]]
                self.branches.bits1[m] = self.input_zero.out_bits[match_zero_idx[0][0]]
                self.branches.bits2[m] = self.input_zero.out_bits[match_zero_idx[0][1]]
                self.branches.input1[m] = 0
                self.branches.input2[m] = 0
            elif len(match_one_idx[0]) != 0:
                self.branches.states1[m] = self.input_one.fn[match_one_idx[0][0]]
                self.branches.states2[m] = self.input_one.fn[match_one_idx[0][1]]
                self.branches.bits1[m] = self.input_one.out_bits[match_one_idx[0][0]]
                self.branches.bits2[m] = self.input_one.out_bits[match_one_idx[0][1]]
                self.branches.input1[m] = 1
                self.branches.input2[m] = 1
            else:
                print('branch calculation error')
                exit(1)
        # self.branches, self.input_zero, self.input_one

    def viterbi_decoder(self,x,metric_type='three_bit'):
        """
        y = viterbi_decoder(x,metric_type = 'three_bit')
        
        Viterbi decoder method
        Mark Wickert February 2014
        """
        # Initialize cummulative metrics array
        cm_present = np.zeros((self.Nstates,1))

        NS = len(x) # number of channel symbols to process; 
                     # must be even for rate 1/2
        y = np.zeros(NS-self.decision_depth) # Decoded bit sequence
        k = 0

        # Calculate branch metrics and update traceback states and traceback bits
        for n in range(0,NS,2):
            cm_past = self.paths.cumulative_metric[:,0]
            tb_states_temp = self.paths.traceback_states[:,:-1].copy()
            tb_bits_temp = self.paths.traceback_bits[:,:-1].copy()
            for m in range(self.Nstates):
                d1 = self.bm_calc(self.branches.bits1[m],
                                  x[n:n+2],metric_type)
                d1 = d1 + cm_past[self.branches.states1[m]]
                d2 = self.bm_calc(self.branches.bits2[m],
                                  x[n:n+2],metric_type)
                d2 = d2 + cm_past[self.branches.states2[m]]
                if d1 <= d2: # Find the survivor assuming minimum distance wins
                    cm_present[m] = d1
                    self.paths.traceback_states[m,:] = np.hstack((self.branches.states1[m],
                                  tb_states_temp[int(self.branches.states1[m]),:]))
                    self.paths.traceback_bits[m,:] = np.hstack((self.branches.input1[m],
                                  tb_bits_temp[int(self.branches.states1[m]),:]))
                else:
                    cm_present[m] = d2
                    self.paths.traceback_states[m,:] = np.hstack((self.branches.states2[m],
                                  tb_states_temp[int(self.branches.states2[m]),:]))
                    self.paths.traceback_bits[m,:] = np.hstack((self.branches.input2[m],
                                  tb_bits_temp[int(self.branches.states2[m]),:]))
            # Update cumulative metric history
            self.paths.cumulative_metric = np.hstack((cm_present, 
                                           self.paths.cumulative_metric[:,:-1]))
            
            # Obtain estimate of input bit sequence from the oldest bit in 
            # the traceback having the smallest (most likely) cumulative metric
            min_metric = min(self.paths.cumulative_metric[:,0])
            min_idx = np.where(self.paths.cumulative_metric[:,0] == min_metric)
            if n >= 2*self.decision_depth-2:  # 2 since Rate = 1/2
                y[k] = self.paths.traceback_bits[min_idx[0][0],-1]
                k += 1
        y = y[:k] # trim final length
        return y

    def bm_calc(self,ref_code_bits, rec_code_bits, metric_type):
        """
        distance = bm_calc(ref_code_bits, rec_code_bits, metric_type)
        Branch metrics calculation

        Mark Wickert February 2014
        """

        if metric_type == 'three_bit': # squared distance metric
            bits = binary(int(ref_code_bits),2)
            ref_MSB = 7*int(bits[0],2)
            ref_LSB = 7*int(bits[1],2)
            distance = (rec_code_bits[0] - ref_MSB)**2
            distance += (rec_code_bits[1] - ref_LSB)**2
        else:
            print('Invalid metric type specified')
        return distance 

    def conv_encoder(self,input,state):
        """
        output, state = conv_encoder(input,state)
        We assume a rate 1/2 encoder.
        Polys G1 and G2 are entered as binary strings, e.g,
        G1 = '111' and G2 = '101' for K = 3
        G1 = '1011011' and G2 = '1111001' for K = 7
        Input state as a binary string of length K-1, e.g., '00' or '0000000' 
        e.g., state = '00' for K = 3
        e.g., state = '000000' for K = 7
        Mark Wickert February 2014
        """

        output = []
        for n in range(len(input)):
            u1 = int(input[n])
            u2 = int(input[n])
            for m in range(1,self.constraint_length):
                if int(self.G_polys[0][m]) == 1: # XOR if we have a connection
                    u1 = u1 ^ int(state[m-1])
                if int(self.G_polys[1][m]) == 1: # XOR if we have a connection
                    u2 = u2 ^ int(state[m-1])
            # G1 placed first, G2 placed second
            output = np.hstack((output, [u1, u2]))
            state = bin(int(input[n]))[-1] + state[:-1]
        return output, state

    def puncture(self,code_bits,puncture_pattern = ('110','101')):
        """
        y = puncture(code_bits,puncture_pattern = ('110','101'))
        Apply puncturing to the serial bits produced by convolutionally
        encoding.  
        """
        # Check to see that the length of code_bits is consistent with a rate 
        # 1/2 code.
        L_pp = len(puncture_pattern[0])
        N_codewords = int(np.floor(len(code_bits)/float(2)))
        if 2*N_codewords != len(code_bits):
            print('Number of code bits must be even!')
            print('Truncating bits to be compatible.')
            code_bits = code_bits[:2*N_codewords]
        # Extract the G1 and G2 encoded bits from the serial stream.
        # Assume the stream is of the form [G1 G2 G1 G2 ...   ]
        x_G1 = code_bits.reshape(N_codewords,2).take([0],
                                 axis=1).reshape(1,N_codewords).flatten()
        x_G2 = code_bits.reshape(N_codewords,2).take([1],
                                 axis=1).reshape(1,N_codewords).flatten()
        # Check to see that the length of x_G1 and x_G2 is consistent with the
        # length of the puncture pattern
        N_punct_periods = int(np.floor(N_codewords/float(L_pp)))
        if L_pp*N_punct_periods != N_codewords:
            print('Code bit length is not a multiple pp = %d!' % L_pp)
            print('Truncating bits to be compatible.')
            x_G1 = x_G1[:L_pp*N_punct_periods]
            x_G2 = x_G2[:L_pp*N_punct_periods]
        #Puncture x_G1 and x_G1
        g1_pp1 = [k for k,g1 in enumerate(puncture_pattern[0]) if g1 == '1']
        g2_pp1 = [k for k,g2 in enumerate(puncture_pattern[1]) if g2 == '1']
        N_pp = len(g1_pp1)
        y_G1 = x_G1.reshape(N_punct_periods,L_pp).take(g1_pp1,
                            axis=1).reshape(N_pp*N_punct_periods,1)
        y_G2 = x_G2.reshape(N_punct_periods,L_pp).take(g2_pp1,
                            axis=1).reshape(N_pp*N_punct_periods,1)
        # Interleave y_G1 and y_G2 for modulation via a serial bit stream
        y = np.hstack((y_G1,y_G2)).reshape(1,2*N_pp*N_punct_periods).flatten()
        return y

    def depuncture(self,soft_bits,puncture_pattern = ('110','101'),
                   erase_value = 3.5):
        """
        y = depuncture(soft_bits,puncture_pattern = ('110','101'),
                       erase_value = 4)
        Apply de-puncturing to the soft bits coming from the channel. Erasure bits
        are inserted to return the soft bit values back to a form that can be
        Viterbi decoded.  
        """
        # Check to see that the length of soft_bits is consistent with a rate 
        # 1/2 code.
        L_pp = len(puncture_pattern[0])
        L_pp1 = len([g1 for g1 in puncture_pattern[0] if g1 == '1'])
        L_pp0 = len([g1 for g1 in puncture_pattern[0] if g1 == '0'])
        #L_pp0 = len([g1 for g1 in pp1 if g1 == '0'])
        N_softwords = int(np.floor(len(soft_bits)/float(2)))
        if 2*N_softwords != len(soft_bits):
            print('Number of soft bits must be even!')
            print('Truncating bits to be compatible.')
            soft_bits = soft_bits[:2*N_codewords]
        # Extract the G1p and G2p encoded bits from the serial stream.
        # Assume the stream is of the form [G1p G2p G1p G2p ...   ],
        # which for QPSK may be of the form [Ip Qp Ip Qp Ip Qp ...    ]
        x_G1 = soft_bits.reshape(N_softwords,2).take([0],
                                 axis=1).reshape(1,N_softwords).flatten()
        x_G2 = soft_bits.reshape(N_softwords,2).take([1],
                                 axis=1).reshape(1,N_softwords).flatten()
        # Check to see that the length of x_G1 and x_G2 is consistent with the
        # puncture length period of the soft bits
        N_punct_periods = int(np.floor(N_softwords/float(L_pp1)))
        if L_pp1*N_punct_periods != N_softwords:
            print('Number of soft bits per puncture period is %d' % L_pp1)
            print('The number of soft bits is not a multiple')
            print('Truncating soft bits to be compatible.')
            x_G1 = x_G1[:L_pp1*N_punct_periods]
            x_G2 = x_G2[:L_pp1*N_punct_periods]
        x_G1 = x_G1.reshape(N_punct_periods,L_pp1)
        x_G2 = x_G2.reshape(N_punct_periods,L_pp1)
        #Depuncture x_G1 and x_G1
        g1_pp1 = [k for k,g1 in enumerate(puncture_pattern[0]) if g1 == '1']
        g1_pp0 = [k for k,g1 in enumerate(puncture_pattern[0]) if g1 == '0']
        g2_pp1 = [k for k,g2 in enumerate(puncture_pattern[1]) if g2 == '1']
        g2_pp0 = [k for k,g2 in enumerate(puncture_pattern[1]) if g2 == '0']
        x_E = erase_value*np.ones((N_punct_periods,L_pp0))
        y_G1 = np.hstack((x_G1,x_E))
        y_G2 = np.hstack((x_G2,x_E))
        [g1_pp1.append(val) for idx,val in enumerate(g1_pp0)]
        g1_comp = zip(g1_pp1,range(L_pp))
        g1_comp.sort()
        G1_col_permute = [g1_comp[idx][1] for idx in range(L_pp)]
        [g2_pp1.append(val) for idx,val in enumerate(g2_pp0)]
        g2_comp = zip(g2_pp1,range(L_pp))
        g2_comp.sort()
        G2_col_permute = [g2_comp[idx][1] for idx in range(L_pp)]
        #permute columns to place erasure bits in the correct position
        y = np.hstack((y_G1[:,G1_col_permute].reshape(L_pp*N_punct_periods,1),
                       y_G2[:,G2_col_permute].reshape(L_pp*N_punct_periods,
                       1))).reshape(1,2*L_pp*N_punct_periods).flatten()
        return y

    def trellis_plot(self,fsize=(6,4)):
        """
        trellis_plot()
        
        Mark Wickert February  2014
        """

        branches_from = self.branches
        plt.figure(figsize=fsize)

        plt.plot(0,0,'.')
        plt.axis([-0.01, 1.01, -(self.Nstates-1)-0.05, 0.05])
        for m in range(self.Nstates):
            if branches_from.input1[m] == 0:
                plt.plot([0, 1],[-branches_from.states1[m], -m],'b')
                plt.plot([0, 1],[-branches_from.states1[m], -m],'r.')
            if branches_from.input2[m] == 0:
                plt.plot([0, 1],[-branches_from.states2[m], -m],'b')
                plt.plot([0, 1],[-branches_from.states2[m], -m],'r.')
            if branches_from.input1[m] == 1:
                plt.plot([0, 1],[-branches_from.states1[m], -m],'g')
                plt.plot([0, 1],[-branches_from.states1[m], -m],'r.')
            if branches_from.input2[m] == 1:
                plt.plot([0, 1],[-branches_from.states2[m], -m],'g')
                plt.plot([0, 1],[-branches_from.states2[m], -m],'r.')
        #plt.grid()
        plt.xlabel('One Symbol Transition')
        plt.ylabel('-State Index')
        msg = 'Rate 1/2, K = %d Trellis' % (int(np.ceil(np.log2(self.Nstates)+1)))
        plt.title(msg)

    def traceback_plot(self,fsize=(6,4)):
        """
        traceback_plot()
        
        
        Mark Wickert February 2014
        """
        traceback_states = self.paths.traceback_states
        plt.figure(figsize=fsize)
        plt.axis([-self.decision_depth+1, 0, 
                  -(self.Nstates-1)-0.5, 0.5])
        M,N = traceback_states.shape
        traceback_states = -traceback_states[:,::-1]

        plt.plot(range(-(N-1),0+1),traceback_states.T)
        plt.xlabel('Traceback Symbol Periods')
        plt.ylabel('State Index $0$ to -$2^{(K-1)}$')
        plt.title('Survivor Paths Traced Back From All %d States' % self.Nstates)
        plt.grid()

def conv_Pb_bound(R,dfree,Ck,SNRdB,hard_soft,M=2):
    """
    Pb = conv_Pb_bound(R,dfree,Ck,SNR,hard_soft,M=2)
    
    Convolution coding bit error probability upper bound
    according to Ziemer & Peterson 7-16, p. 507
    
    Mark Wickert November 2014
    """
    Pb = np.zeros_like(SNRdB)
    SNR = 10.**(SNRdB/10.)

    for n,SNRn in enumerate(SNR):
        for k in range(dfree,len(Ck)+dfree):
            if hard_soft == 0: # Evaluate hard decision bound
                Pb[n] += Ck[k-dfree]*hard_Pk(k,R,SNRn,M)
            elif hard_soft == 1: # Evaluate soft decision bound
                Pb[n] += Ck[k-dfree]*soft_Pk(k,R,SNRn,M)
            else: # Compute Uncoded Pe
                if M == 2:
                    Pb[n] = Q_fctn(np.sqrt(2.*SNRn))
                else:
                    Pb[n] = 4./np.log2(M)*(1 - 1/np.sqrt(M))*\
                            gaussQ(np.sqrt(3*np.log2(M)/(M-1)*SNRn));
    return Pb

def hard_Pk(k,R,SNR,M=2):
    """
    Pk = hard_Pk(k,R,SNR)
    
    Calculates Pk as found in Ziemer & Peterson eq. 7-12, p.505
    
    Mark Wickert November 2014
    """

    if M == 2:
        p = Q_fctn(np.sqrt(2.*R*SNR))
    else:
        p = 4./np.log2(M)*(1 - 1./np.sqrt(M))*\
            Q_fctn(np.sqrt(3*R*log2(M)/float(M-1)*SNR))
    Pk = 0
    if 2*k//2 == k:
        for e in range(k/2+1,k+1):
            Pk += float(factorial(k))/(factorial(e)*factorial(k-e))*p**e*(1-p)**(k-e);
        Pk += 1./2*float(factorial(k))/(factorial(k/2)*factorial(k-k/2))*\
              p**(k/2)*(1-p)**(k//2);
    else:
        for e in range((k+1)//2,k+1):
            Pk += factorial(k)/(factorial(e)*factorial(k-e))*p**e*(1-p)**(k-e);
    return Pk

def soft_Pk(k,R,SNR,M=2):
    """
    Pk = soft_Pk(k,R,SNR)
    
    Calculates Pk as found in Ziemer & Peterson eq. 7-13, p.505
    
    Mark Wickert November 2014
    """
    if M == 2:
        Pk = Q_fctn(np.sqrt(2.*k*R*SNR))
    else:
        Pk = 4./np.log2(M)*(1 - 1./np.sqrt(M))*\
             Q_fctn(np.sqrt(3*k*R*np.log2(M)/float(M-1)*SNR))
    
    return Pk

def Q_fctn(x):
    """
    Gaussian Q-function
    """
    return 1./2*special.erfc(x/np.sqrt(2.))

if __name__ == '__main__':
    #x = np.arange(12)
    """
    cc2 = fec_conv()
    y = cc2.puncture(x,('011','101'))
    z = cc2.depuncture(y,('011','101'))
    #x = ssd.m_seq(7)
    """
    x = [0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 1, 1, 0,
         1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 0, 1, 1,
         0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 0, 0,
         0, 0, 1, 0, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 1, 1, 0, 0, 1, 0, 1, 0, 1,
         1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 0, 1, 0, 1, 1, 1, 0, 1,
         0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 1]
    cc1 = fec_conv()
    output, states = cc1.conv_encoder(x,'00')
    y = cc1.viterbi_decoder(7*output,'three_bit')
    
    print('Xor of input/output bits:')
    errors = np.int32(x[:80])^np.int32(y[:80])
    print(errors)
