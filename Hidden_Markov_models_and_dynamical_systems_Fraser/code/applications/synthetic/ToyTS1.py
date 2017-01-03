"""ToyTS1.py Makes a plot from data selected using Hview.py

Command:

python3 ToyTS1.py Save_Hview_T_100 ToyTS1

I pulled this from old hmmdsbook and only modified it slightly.  I'd
like to polish it up.

"""
Copyright = '''
Copyright 2005, 2007 and 2013 Andrew M. Fraser and Los Alamos National
Laboroatory

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
import os, sys
import numpy as np
if len(sys.argv) != 3:
    print("Call should be: ",sys.argv[0],\
          " InputDataFile OuputDataFile\n","sys.argv was: ", sys.argv)
    raise SystemExit

inputdata  = sys.argv[1]
outputdata = sys.argv[2]

f = open(inputdata,'r')
lines = f.readlines()
f.close()

if lines[1].split()[0] != 'Tmin=':
    print('unexpected format in file',inputdata,\
          'missed "Tmin=" in line 1\n', lines[1])
else:
    Tmin = int(lines[1].split()[1])
if lines[1].split()[2] != 'Tmax=':
    print('unexpected format in file',inputdata,\
          'missed "Tmax=" in line 1\n', lines[1])
    raise SystemExit
else:
    Tmax = int(lines[1].split()[3])


f = open(outputdata, 'w')
for t in range(Tmin,Tmax):
    fields = lines[t-Tmin+4].split()
    if int(fields[0]) != t:
        print('first field of line should be',t,'found',fields[0])
        raise SystemExit
    y  = float(fields[1])
    error = y-float(fields[2])
    dev = np.sqrt(float(fields[3]))
    like = float(fields[4])
    print(t, y, error, dev, like, file=f)
f.close()


# Local Variables:
# mode: python
# End:
