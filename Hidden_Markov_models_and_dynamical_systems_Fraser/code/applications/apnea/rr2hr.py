""" rr2hr.py Translates r-time anotations to heart rate

python3 rr2hr.py r_times low_pass_heart_rate a01 a02 a03 ... c09 c10

For each record R listed, read R.rtimes, calulate the lowpass filtered
heart rate and write R.lphr.
"""
Copyright = '''Copyright 2005, 2007, 2008 Andrew M. Fraser, and 2013
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
import numpy as np
import sys
import getopt
from os.path import join
from numpy.fft import rfft, irfft
SamPerMin = 10

def record2lphr(record_name, # eg 'a01'
                r_times,     # path to directory of r_times
                lp_hr        # path to directory of low pass heart rate data
                ):
    '''Read the r_times data (Note that it is sampled irregularly and
     consists of the r-times in centiseconds), calculate the low pass
     heart rate and write it to the lp_hr directory.
    '''
    # frequency spectrogram.
    Tdata = []
    File = open(join(r_times, record_name))
    for line in File:
        Tdata.append(float(line)/100)
    File.close()
        # Now Tdata[i] is an r-time in seconds.
    Ddata = np.zeros(len(Tdata)-1) # Difference between consecutive Rtimes,
                                  # ie, rr-times
    for i in range(len(Ddata)):
        Ddata[i] = Tdata[i+1] - Tdata[i]
    sortedD = np.sort(Ddata)
    L = float(len(sortedD))
    ti = int(0.95*L)
    bi = int(0.05*L)
    top = 1.25*sortedD[ti]
    bottom = 0.8*sortedD[bi]
    data = [] # To contain list of pairs [time(in seconds), rr-time]
              # after culling bad rr intervals
    # hack to get an rr time for time zero and first rtime
    rr = Tdata[1] - Tdata[0]
    if rr > top or rr < bottom: # If rr-interval is bad, use median rr-time
        rr = sortedD[int(0.5*len(sortedD))]
    if Tdata[0] > 0:
        data = [[0,rr],[Tdata[0],rr]]
    else:
        data = [[Tdata[0],rr]]
    # Assign good rr times for samples after first and before last
    for i in range(len(Tdata)-2):
        rr = Tdata[i+1] - Tdata[i]
        if rr < top and rr > bottom:
            data.append([Tdata[i+1],rr])
    # hack to force rr time for last time
    rr = Tdata[-1] - Tdata[-2]
    if rr > top or rr < bottom:
        rr = sortedD[int(0.5*len(sortedD))]
    data.append([Tdata[-1],rr])

    # Now data[i][0] is an r-time in seconds and data[i][1] is a "good"
    # r-r interval.

    # Create an array of heart rates that is uniformly sampled at 2 HZ

    TF = data[-1][0] # Time is measured in seconds
    L = int(2*TF)
    hr = np.zeros(L)
    t_old = data[0][0]
    rr_old = data[0][1]
    t_new = data[1][0]
    rr_new = data[1][1]
    i=1
    for k in range(L):
        t = k/2.0
        while t > t_new:
            i += 1
            if i >= len(data):
                break
            t_old = t_new
            rr_old = rr_new
            t_new = data[i][0]
            rr_new = data[i][1]
        if i >= len(data):
            break
        drdt = float(rr_new-rr_old)/float(t_new-t_old)
        rr =  rr_old + (t-t_old)*drdt
        hr[k] = rr
    # Transform rr_times to heart rate
    hr = 60/(hr)
    Avg = hr.sum()/L
    hrL = hr - Avg
    # Now, hr is heart rate sampled at 2HZ, and hrL is the same with
    # mean subtracted

    HR = rfft(hrL,131072) # 131072 is 18.2 Hrs at 2HZ
    HR[0:100] *=0 # Drop frequencies below (100*60)/65536=0.09/min
    HR[4000:] *=0 # and above (4000*60)/65536=3.66/min
    hrL = irfft(HR)
    File = open(join(lp_hr, record_name),'w')
    for i in range(0,L,(2*60)//SamPerMin):
        print('%6.1f  %6.3f  %6.3f'%(i/120.0, hr[i], hrL[i]), file=File)
    # Time in minutes, Unfiltered hr in beats per minute, low pass hr
    File.close()

def main(argv=None):
    
    import argparse

    if argv is None:                    # Usual case
        argv = sys.argv[1:]

    parser = argparse.ArgumentParser(
        description='Calculate low pass heart rate from r_times files')
    parser.add_argument('In_Dir', help='Directory of r_times files to read')
    parser.add_argument(
        'Out_Dir', help='Directory of low pass heart rate files to write')
    parser.add_argument(
        'records', nargs='*', help='List of record names to process')
    args = parser.parse_args(argv)
    for r in args.records:
        record2lphr(r, args.In_Dir, args.Out_Dir)
    return 0
        
if __name__ == "__main__":
    sys.exit(main())

#Local Variables:
#mode:python
#End:
