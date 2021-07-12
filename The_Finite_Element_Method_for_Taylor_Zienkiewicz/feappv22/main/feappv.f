      program feappv

c-----[--.----+----.----+----.-----------------------------------------]
c      * * F E A P p v * * Finite Element Analysis Program
c                          -      -       -        -
c....        Copyright (c) 1984-2009: Robert L. Taylor
c                                     All rights reserved

c     Finite Element Analysis Program -- personal version -- (FEAPpv) for
c     solution of general problem classes using the finite element method.
c     Problem size is controlled by dimension of blank common  & value of
c     variable 'maxm' as set in the parameter statement below.

c     Programmed by:
c                R. L. Taylor
c                Department of Civil & Environmental Engineering
c                University of California at Berkeley
c                Berkeley, California 94720-1710
c              E-mail:
c                feap@ce.berkeley.edu
c-----[--.----+----.----+----.-----------------------------------------]
c     Notes:

c     1. Precision is controlled by ipr:

c        Set ipr = 1 for 8-byte integers; = 2 for 4-byte integers.

c     2. User written subprograms should include type specification
c        for all variables in each subprogram.

c        e.g.    implicit  none
c                real*8    a(12)
c                integer   name
c                character word*6
c                logical   flag
c                etc.

c     3. FEAPpv may create temporary input files during use.
c        Users should periodically check and delete files
c        which are no longer needed.  File names are normally
c        either the name of the data input file with an extender
c        or the name of the plot save file with an extender.

c     4. Input/Output is performed to files during execution of FEAPpv.
c        In general, the following files are used during executions:

c           a.  iop = 11 : Used for read/write delayed inputs.
c           b.  ios = 12 : Used for read/write scratch files.
c           c.  ird = 13 : Used to read results data from disk.
c           d.  iwd = 14 : Used to write results data to disk.
c           e.  ior = 15 : Use to read from the input data file.
c                          (specified when a problem is initiated).
c           f.  iow = 16 : Use to write output result data to file.
c                          (specified when a problem is initiated).
c           g.  lun = 17 : For PostScript file outputs.
c           h.  icl = 18+: Used for include file inputs.
c                          (additional include files may be opened).
c     End of Notes
c-----[--.----+----.----+----.-----------------------------------------]
      implicit      none

      include      'cdata.h'
      include      'codat.h'
      include      'hlpdat.h'
      include      'iodata.h'
      include      'iofile.h'
      include      'prmptd.h'
      include      'psize.h'
      include      'pathn.h'
      include      'setups.h'
      include      'vdata.h'
c-----[--.----+----.----+----.-----------------------------------------]
c     Set parameter for memory capacity of blank common

      integer    mmax
      parameter (mmax = 80000000)

      integer    m
      common     m( mmax )

      maxm     = mmax

c     Set version header for output to file and screen

      versn(1) = '2.2 Revision a'
      versn(2) = '08 January 2009'

c     Set precision for real variables:

      ipr = 2                  ! 32 bit version

c     Set default logical unit numbers for files

      iop = 11
      ios = 12
      ird = 13
      iwd = 14
      ior = 15
      iow = 16
      lun = 17
      icl = 18

c     Set data input parsing flag

         coflg = .true.  ! Parse all input as expressions   (slower mode)

                         ! N.B. Use of 'parse' and 'noparse' mesh commands
                         !      permit change of this default setting.

c     Set graphics default options

         defalt = .true. ! Graphics runs with default contour intervals, etc.
         prompt = .true. ! Prompt for graphics inputs when defalt = .false.

c     Set PostScript default mode

         pscolr = .true. ! PostScript outputs are in color
         psrevs = .false.! Color order is normal

c     Set help display level: (0=Basic, 1=Intermediate, 2=Advanced)

         hlplev = 0      ! Basic

c     Set increment for reducing array size

         incred = 2      ! No reduction unless array is less by 'incred'

c     Set solver flag: Program - solver = .true.; User - solver = .false.

         solver    = .true.
         processor = 1       ! Anticipates multiprocessor use

c-----[--.----+----.----+----.-----------------------------------------]

c     Initialize solution system

      call pstart()

c     Check installation options

      call pinstall()

c     Open files: Must open unit ior (input) and iow (output)

      call filnam()

c     Initialize clock

      call stime()

c     Solve problem

      call pcontr()

c     Close plot

      call plstop()

c     No additional calls to routines after PLSTOP

      end
