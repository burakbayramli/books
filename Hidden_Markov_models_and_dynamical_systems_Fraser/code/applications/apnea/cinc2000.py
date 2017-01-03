"""
cinc2000.py

Utility routines for handling cinc2000 files.
"""

Copyright = '''Copyright 2005 Andrew M. Fraser

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
import numpy as N
#Returns dictionary.  Keys are wfdb record names, eg, 'a01' and values
#are strings, eg, 'NNNNNNNNNNNNNAAAAA...AAAAAAAAA'
def ReadIn(FileName):
    fp = open(FileName)
    d = {}
    # Get first record name by finding a line with len(parts) == 1
    line = fp.readline()
    for i in range(len(line)):
        if line[i] == '#': # drop comments, ie ' # words ...'
            line = line[:i]
            break
    parts = line.split()
    while not len(parts) == 1:
        line = fp.readline()
        parts = line.split()
    key = parts[0]
    value = []
    # Read rest of the file
    lines = fp.readlines()
    if len(lines) < 4:
        raise RuntimeError("Less than 4 lines after record in file: %s"%
                           FileName)
    for line in lines:
        for i in range(len(line)):
            if line[i] == '#': # drop comments, ie ' # words ...'
                line = line[:i]
                break
        parts = line.split()
        if len(parts) == 0: # Empty line
            continue
        if len(parts) == 2: # Classification data for current record
            value += parts[1]
        if len(parts) == 1: # New record name
            d[key] = value
            key = parts[0]
            value = []
    d[key] = value
    return d

def As_Ns_2_Nums(AN,S_min=10,val_A=1,val_N=0):
    """
    AN is a string of As and Ns
    S_min is samples per minute
    val_A is the number for A (apnea)
    val_N is the value for N (normal)
    """
    Nums = []
    num = {'A':val_A,'N':val_N}
    for c in range(len(AN)):
        n = num[AN[c]]
        Nums.append(n)
        """
        for i in xrange(S_min):
            s = c*S_min + i # Time in seconds for x axis
            Nums.append([s,n])
        """
    return(Nums)

def R_times2Dev(data,w=1):
    """ From respire.py.
    Input:
      data,   A list of R times (peak of ecg in a heartbeat)
      w,      window size.  Look backwards and forwards at w R-times.

    Return value:
      hrd,    A list of heart rate deviations sampled at 2 HZ.  The
              diviation is the jitter interpolated between the R time
              before the sample and the R time after the sample.  The
              jitter is the fraction of a pulse period by which an
              actual R time differs from the expected R time (the
              average time of the time before and the time after the
              beat in question).
    """
  
    # Create a list of jitters
    jitter = []
    for i in range(w,len(data)-w):
        sum = 0.0
        for d in (list(range(-w,0))+list(range(1,w+1))):
            sum += data[i+d]
        T = sum/(2*w) # Expected time for data[i] if uniform
        P = (data[i+w] - data[i-w])/(2*w) # Avg pulse period
        j = (data[i] - T)/P # Fraction of pulse period by which
                            # data[i] is early or late
        j = max(min(0.25,j),-0.25) # Clip to +/- 0.25
        jitter.append([data[i],j])
    # Create an array of heart rate deviations that is uniformly
    # sampled at 2 HZ
    TF = jitter[-1][0]
    L = int(2*TF)
    hrd = N.zeros((L,),N.float)
    t_old = 0
    j_old = 1.0
    t_new = jitter[0][0]
    j_new = jitter[0][1]
    i=0
    for k in range(L):
        t = k/2.0
        if t > t_new:
            i += 1
            if i >= len(jitter):
                break
            t_old = t_new
            j_old = j_new
            t_new = jitter[i][0]
            j_new = jitter[i][1]
        djdt = (j_new-j_old)/(t_new-t_old)
        hrd[k] = j_old + (t-t_old)*djdt

    return hrd
#Local Variables:
#mode:python
#End:
