!$Id:$
      subroutine errclr (subnam)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Input error clearing routine

!      Inputs:
!         subnam - Character array storing calling subroutine name

!      Outputs:
!         none
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'comfil.h'
      include  'iofile.h'
      include  'ioincl.h'

      character    :: subnam*(*)

      if (ior.gt.0)  then
         write(iow,2000) subnam,fincld(isf),irecrd(isf),record
         call pdelfl()
         call plstop(.true.)
      endif

!     Format

 2000 format (/'  Inconsistency occurred from ',a,' in data file ',a,/
     &         '  at or near record number',i6,'.  Input record is:',
     &         //2x,a78//,
     &         '  If this record is correct error may result from'/
     &         '  missing blank record before new command type.')

      end subroutine errclr
