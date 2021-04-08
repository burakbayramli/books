      program feappv

!-----[--.----+----.----+----.-----------------------------------------]
!      * * F E A P p v * * Finite Element Analysis Program
!                          -      -       -        -
!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!     Finite Element Analysis Program - personal version - (FEAPpv) for
!     solution of general problem types using the finite element method.

!     Programmed by:
!                R. L. Taylor
!                Department of Civil & Environmental Engineering
!                University of California at Berkeley
!                Berkeley, California 94720-1710
!              E-mail:
!                feap@berkeley.edu
!-----[--.----+----.----+----.-----------------------------------------]
!     Notes:

!     1. Precision is controlled by parameter "ipr":

!        Set ipr = 1 for 8-byte integers; = 2 for 4-byte integers.

!     2. User written subprograms should include type specification
!        for all variables in each subprogram.

!        e.g.    implicit  none
!                real      (kind=8) :: a(12)
!                integer   (kind=4) :: name
!                character  (len=6) :: word
!                logical            :: flag
!                etc.

!     3. FEAPpv may create temporary input files during use.
!        Users should periodically check and delete files
!        which are no longer needed.  File names are normally
!        either the name of the data input file with an extender
!        or the name of the plot save file with an extender.

!     4. Input/Output is performed to files during execution of FEAPpv.
!        In general, the following files are used during executions:

!           a.  iop = 11 : Used for read/write delayed inputs.
!           b.  ios = 12 : Used for read/write scratch files.
!           c.  ird = 13 : Used to read results data from disk.
!           d.  iwd = 14 : Used to write results data to disk.
!           e.  ior = 15 : Use to read from the input data file.
!                          (specified when a problem is initiated).
!           f.  iow = 16 : Use to write output result data to file.
!                          (specified when a problem is initiated).
!           g.  lun = 17 : For PostScript file outputs.
!           h.  icl = 18+: Used for include file inputs.
!                          (additional include files may be opened).
!     End of Notes
!-----[--.----+----.----+----.-----------------------------------------]
      implicit      none

      include      'cdata.h'
      include      'codat.h'
      include      'hlpdat.h'
      include      'iodata.h'
      include      'iofile.h'
      include      'prmptd.h'
      include      'setups.h'
      include      'vdata.h'
!-----[--.----+----.----+----.-----------------------------------------]
!     Set version header for output to file and screen

      versn(1) = 'Release 5.1.1c'
      versn(2) = '18 June 2020'

!-----[--.----+----.----+----.-----------------------------------------]
!     Set precision for real variables:

      ipr = 2                  ! 32-bit integer arrays version
!                              ! Pointers to arrays are 64 bit

!-----[--.----+----.----+----.-----------------------------------------]
!     Set default logical unit numbers for files

      iop = 11
      ios = 12
      ird = 13
      iwd = 14
      ior = 15
      iow = 16
      lun = 17
      icl = 18

!-----[--.----+----.----+----.-----------------------------------------]
!     Set data input parsing flag

      coflg = .true.  ! Parse all input as expressions   (slower mode)
                      ! N.B. Use of 'parse' and 'noparse' mesh commands
                      !      permit change of this default setting.

      ciflg = .true.  ! Get input file from menu window if .true.
!     ciflg = .false. ! or from keyboard entry if .false.
                      ! N.B. Used for Windows version only

!-----[--.----+----.----+----.-----------------------------------------]
!     Set graphics default options

      defalt = .true. ! Graphics with default contour intervals, etc.
      prompt = .true. ! Prompt for graphics inputs when defalt = .false.

!-----[--.----+----.----+----.-----------------------------------------]
!     Set PostScript default mode

      pscolr = .true. ! PostScript outputs are in color
      psrevs = .false.! Color order is normal

!-----[--.----+----.----+----.-----------------------------------------]
!     Set help display level: (0=Basic, 1=Intermediate, 2=Advanced)

      hlplev = 0      ! Basic

!-----[--.----+----.----+----.-----------------------------------------]
!     Set solver flag: Program-solver = .true.; User-solver = .false.

      solver    = .true.
!-----[--.----+----.----+----.-----------------------------------------]

!     Initialize solution system
      call pstart()

!     Solve problem
      call pcontr()

!-----[--.----+----.----+----.-----------------------------------------]
!     No additional calls to routines after PLSTOP

      end program feappv
