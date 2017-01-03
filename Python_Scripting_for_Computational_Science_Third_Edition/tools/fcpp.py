#!/usr/bin/env python
"""
Enable running the C preprocessor on Fortran files.
Used in compilations in src/app/wavesim2D/F77.
"""
# Developed by Hans Petter Langtangen, Dept. of Informatics, 2001.

import os, sys, re, string
if len(sys.argv) == 1:
    print "Usage: %s [gcc options] file1.fcp" % sys.argv[0]
    sys.exit(1)

FORTRAN_VERSION=77
#FORTRAN_VERSION=95

# the last argument is the filename:
fcp_file = sys.argv[len(sys.argv)-1]
# the rest of the files are options:
if len(sys.argv) == 2:
    # no options (sys.argv contains just the script's filename
    # and the Fortran filename):
    cpp_options = ''
else:
    # join the list elements sys.argv[1] up to the next last
    # sys.argv argument into a string to be passed to gcc:
    cpp_options = ' '.join(sys.argv[1:-1])

if fcp_file[-4:] != '.fcp':
    print 'wrong suffix'; sys.exit(1)
if not os.path.isfile(fcp_file):
    print fcp_file, 'not found...'; sys.exit(1)

# let file now be the filestem:
file = fcp_file[:-4]

if FORTRAN_VERSION == 95: suffix = '.f95'
else:                     suffix = '.f'
# run gcc:
import shutil
shutil.copy(fcp_file, 'tmp.c') # fake C file for gcc -E -c
cmd = 'gcc -E %(cpp_options)s -c tmp.c > %(file)s%(suffix)s' % vars()
failure = os.system(cmd)
if failure:
    print 'could not run gcc on the file %s.fcp' % file; sys.exit(1)
os.remove('tmp.c')

# split lines that are longer than maxlen chars:
maxlen = 72
f = open(file+suffix, 'r'); lines = f.readlines(); f.close()
for i in range(len(lines)):
    line = lines[i]
    if len(line) > maxlen:
        # split line
        nrest = len(line) - maxlen
        splitline = line[0:maxlen]
        start = maxlen
        while nrest > 0:
            if FORTRAN_VERSION == 77:
                splitline = splitline + '\n     &' + line[start:start+maxlen-6]
            else:
                splitline = splitline + '&\n     ' + line[start:start+maxlen-6]
            start = start + maxlen-6
            nrest = nrest - (maxlen-6)
        #print 'start=%d, len=%d, nrest=%d, rest=<%s>' % (start,len(line),nrest,line[start:len(line)])
        if len(line) - start > 0:
            print line[start:len(line)],'is a rest - bug...'
        #print 'len(line)=',len(line),'splitline=\n',splitline,'\n\n'
        lines[i] = splitline
        # NOTE: instead of i counter and in-place change of lines,
        # one could read the file line by line, modify line, and
        # simply write each line to the output.
        # The present approach has the possibility for further
        # adjustments, e.g., removing spaces around (, ), and commas,
        # see below (can do this line-wise to...)

# newer C preprocessors preserve indentation, but minimize whitespace
# elsewhere such that labels "10    CONTINUE" appear as "10 CONTINUE";
# ensure that labels in column 1-6 appear correctly:
c = re.compile(r'^(\s*)(\d+)(\s*)')
for i in range(len(lines)):
    # remove lines starting with #
    lines[i] = re.sub(r'^#.*', '      ', lines[i])
    if len(lines[i]) >= 5:
        column1to5 = lines[i][0:5]
        if re.search(r'\w',column1to5):  # letter after label?
            m = re.search(r'(\s*)(\d+)(\s+)\w+', column1to5)
            if m:
                # insert extra white space after group 3
                n = len(m.group(1)) + len(m.group(2)) + len(m.group(3))
                space = string.join([' ']*(6 - n), '')
                lines[i] = m.group(1) + m.group(2) + m.group(3) + \
                           space + lines[i][n:]
                #print "column1-5 has '%s' found '%s' and groups '%s' '%s' '%s' and space '%s' and constructed" % (column1to5,m.group(0),m.group(1),m.group(2),m.group(3),space)
                #print lines[i]
    
filestr = ''.join(lines)
f = open(file+'.f', 'w'); f.write(filestr); f.close()

# test file macros.i:
"""
#define DDx(u, i, j, dx) \
  (u(i+1,j) - 2*u(i,j) + u(i-1,j))/(dx*dx)
#define DDy(u, i, j, dy) \
  (u(i,j+1) - 2*u(i,j) + u(i,j-1))/(dy*dy)
"""
# test file wave1.fcp:
"""
#include <macros.i>

C234567 column numbers 1-7 are important in F77!
      SUBROUTINE WAVE1(SOL, SOL_PREV, SOL_PREV2, NX, NY,
     &                 DX, DY, DT)
C     variable declarations:
      INTEGER NX, NY     /* no of points in x and y dir */
      REAL*8 DX, DY, DT  /* cell and time increments */
      REAL*8 SOL(NX,NY), SOL_PREV(NX,NY), SOL_PREV2(NX,NY)

C     update SOL:
      DO 20 J=1, NY
        DO 10 I=1, NX
        /*
          a 2nd-order time difference combined with
          2nd-order differences in space results in
          the standard explicit finite difference scheme
          for the wave equation:
        */
          SOL(I,J) = 2*SOL_PREV(I,J) - SOL_PREV2(I,J) +
     &               DT*DT*(DDx(SOL_PREV, I, J, DX) +
     &                      DDy(SOL_PREV, I, J, DY))
#ifdef DEBUG
          WRITE(*,*) 'SOL(',I,',',J,')=',SOL(I,J)
#endif
 10     CONTINUE
 20   CONTINUE

      RETURN
      END
"""
# running
#   fcpp.py -I. -DDEBUG wave1.fcp
# results in the file wave1.f:
"""
      
      
      
      
      
      

C234567 column numbers 1-7 are important in F77!
      SUBROUTINE WAVE1(SOL, SOL_PREV, SOL_PREV2, NX, NY,
     & DX, DY, DT)
C variable declarations:
      INTEGER NX, NY
      REAL*8 DX, DY, DT
      REAL*8 SOL(NX,NY), SOL_PREV(NX,NY), SOL_PREV2(NX,NY)

C update SOL:
      DO 20 J=1, NY
        DO 10 I=1, NX






          SOL(I,J) = 2*SOL_PREV(I,J) - SOL_PREV2(I,J) +
     & DT*DT*((SOL_PREV(I+1,J) - 2*SOL_PREV(I,J) + SOL_PREV(I-1,J))/(DX*
     &DX) +
     & (SOL_PREV(I,J+1) - 2*SOL_PREV(I,J) + SOL_PREV(I,J-1))/(DY*DY))

          WRITE(*,*) 'SOL(',I,',',J,')=',SOL(I,J)

 10   CONTINUE
 20   CONTINUE

      RETURN
      END
"""










