""" respire.py

python3 respire.py r_times_dir summary_of_training resp_dir

This code takes "real    4m59.623s" to process all of the CINC2000 data

Calculate high frequency periodograms for each record at 0.1 minute
intervals.  Collect these vectors into three groups:

1. Those from 'c' records
2. Those from 'a' records during normal sleep
3. Those from 'a' records during apnea

From these three groups, calculate two Fisher LDA basis vectors.

For each sample time for each record, project the corresponding vector
onto the Fisher basis and write the result to data/'record'.resp2

"""
Copyright = '''Copyright 2005, 2008, 2012 Andrew M. Fraser, and 2013
Andrew M. Fraser and Los Alamos National Laboroatory

This file is part of hmmds3.

Hmmds3 is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

Hmmds3 is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

See the file gpl.txt in the root directory of the hmmds3 distribution
or see <http://www.gnu.org/licenses/>.
'''
from ApOb import fetch_annotations
import numpy as np
import numpy.linalg as LA
import math
import sys
from os.path import join

SamPerMin = 10               # Samples per minute for output
Dt_in = 0.5                  # Sampling interval of jitter in seconds
SpM = 60                     # Seconds per minute
Dt_out = SpM/SamPerMin       # Output sampling interval in seconds
RDt = int(Dt_out/Dt_in)      # Ratio of sampling intervals
Fw = 1024                    # Channels for FFT
cpm_chan = SpM/(Dt_in*Fw)    # (cycles per minute)/(channel #)
Glength = 1024               # Length of data sequence in samples
Gl_2 = Glength/2
CUT = 1.0                    # Exclude high magnitude samples from
                             # basis function calculation
sigma = 50.0                 # Width of Gaussian window in samples.
                             # Easier to change sigma than Fw, etc

# Calculate Gaussian window function
Gw = np.zeros((Glength,),np.float64)
for t in range(Glength):
    Gw[t] = math.exp( -((t-Gl_2)**2)/(2*sigma**2) )

def record2vecs(File):
    # Read the rr data (Note that it is sampled irregularly and
    # consists of the r-times in centiseconds) and return a high
    # frequency spectrogram.
    import cinc2000
    from numpy.fft import rfft, irfft
    data = []
    for line in File:
        data.append(float(line)/100)
    File.close()
    # Now data[i] is an r-time in seconds.
    hrd = cinc2000.R_times2Dev(data)
    # hrd are heart rate deviations sampled at 2 Hz
    pad = np.zeros(((Glength+len(hrd)),),np.float64)
    pad[Gl_2:len(hrd)+Gl_2] = hrd
    # Now pad is hrd with Gl_2 zeros before and after
    N_out = len(hrd)//RDt
    result = np.zeros((N_out,Fw/2),np.float64)
    mags = []
    for k in range(N_out):
        i = int(RDt*k)
        WD = Gw*pad[i:i+Glength] # Multiply data by window fuction
        FT = rfft(WD,n=Fw)
        SP = np.conjugate(FT)*FT  # Periodogram
        temp = rfft(SP,n=Fw//2)
        SP = irfft(temp[0:int(0.1*Fw/2)],n=Fw/2)
        # Low pass filter in freq domain.  Pass below 0.1 Nyquist
        temp = SP.real[:Fw/2]
        mag = math.sqrt(np.dot(temp,temp))
        result[k,:] = temp/mag
        mags.append(math.log(mag))
        # result[k,:] is a unit vector and a smoothed periodogram
    return [result,mags]
def LDA(vec_dict, annotations, summary_dir):
    #import ApOb
    # Collect three classes of vectors
    C_vecs  = [] # Vectors from c records
    AA_vecs = [] # Vectors from a records in apnea time
    AN_vecs = [] # Vectors from a records in normal time
    for r, v in vec_dict.items():
        vecs, mags = v
        if r.startswith('b') or r.startswith('x'): # Not used for LDA
            continue
        if r.startswith('c'):
            for vec in vecs[300:-60]:
                C_vecs.append(vec)
            continue
        Ap_notes = fetch_annotations(annotations, r)
        # Now Ap_notes[t] = Mark where t is in tenths of a minute and
        # Mark=1 for Apnea and Mark=0 for Normal
        for t in range(300,len(vecs)-300):
            if mags[t] > CUT:
                continue
            if Ap_notes[t] == 1:
                AA_vecs.append(vecs[t])
            elif Ap_notes[t] == 0:
                AN_vecs.append(vecs[t])


    # Calculate mean and scatter of the three classes
    def mean_var(IN):
        vecs = np.array(IN)
        mean = np.sum(vecs,axis=0)/len(vecs)
        d = np.transpose(vecs-mean)
        var = np.inner(d, d)
        return mean, var, vecs, len(vecs)
    C_mean,  C_var,  C_vecs,  C_n  = mean_var(C_vecs)
    AA_mean, AA_var, AA_vecs, AA_n = mean_var(AA_vecs)
    AN_mean, AN_var, AN_vecs, AN_n = mean_var(AN_vecs)

    # Calculate Sw, the within class scatter
    Sw = C_var + AA_var + AN_var
    # Calculate Sb, the between class scatter
    n = C_n + AA_n + AN_n
    mean = (C_n*C_mean + AA_n*AA_mean + AN_n*AN_mean)/n
    def Sb_t(tmean, tn):
        d = tmean-mean
        return tn*np.outer(d,d)
    Sb = Sb_t(C_mean, C_n)+Sb_t(AA_mean, AA_n)+Sb_t(AN_mean, AN_n)
    # Calculate a 2-d basis of linear discriminant vectors
    n = Sw.shape[0]
    vals, vecs = LA.eigh(np.dot(LA.inv(
                            Sw+np.eye(n)*100),Sb))
    # Find largest two eigenvalues
    i = np.argsort(vals)
    basis = vecs[:,i[-2:]]

    # Write files of information to characterize LDA
    open_file = lambda x: open(join(summary_dir, x), 'w')
    File = open_file('mean.resp')
    for i in range(len(C_mean)):
        print('%5d %8.5f %8.5f %8.5f %8.5f %8.5f'%
              (i, C_mean[i], AN_mean[i], AA_mean[i], basis[i,0], basis[i,1]),
              file=File)

    for name, data in (('C.resp',C_vecs),
                       ('AN.resp',AN_vecs),
                       ('AA.resp',AA_vecs)):
        dots = np.dot(data, basis)
        File = open_file(name)
        for i in range(dots.shape[0]):
            print('%8.5f %8.5f'%(dots[i,0], dots[i,1]), file=File)
    return basis

def main(argv=None):
    
    import argparse
    import os

    if argv is None:                    # Usual case
        argv = sys.argv[1:]

    parser = argparse.ArgumentParser(
        description='Extract respiration information from r_times files')
    parser.add_argument('Annotations', help='File of expert classifications')
    parser.add_argument('In_Dir', help='Directory of r_times files to read')
    parser.add_argument(
        'Summary_Dir', help='Directory for files that illustrate LDA')
    parser.add_argument(
        'Resp_Dir', help='Directory of resp files to write')
    args = parser.parse_args(argv)

    # Read r_times data
    records = os.listdir(args.In_Dir)
    vec_dict = {}
    for r in records:
        vecs, mags = record2vecs(open(join(args.In_Dir, r), 'r'))
        vec_dict[r] = [vecs,mags]
    # Do linear discriminant analysis and write summary
    basis = LDA(vec_dict, args.Annotations, args.Summary_Dir)
    # Write "respiration: time series for each record
    for r in records:
        vecs, mags = vec_dict[r]
        pairs = np.dot(vecs, basis)
        File = open(join(args.Resp_Dir, r), 'w')
        for i in range(len(pairs)):
            print(i/float(SamPerMin),pairs[i,0],pairs[i,1],mags[i], file=File)
    return 0
        
if __name__ == "__main__":
    sys.exit(main())

#Local Variables:
#mode:python
#End:
