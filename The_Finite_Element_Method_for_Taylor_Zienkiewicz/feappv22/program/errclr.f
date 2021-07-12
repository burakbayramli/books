c$Id:$
      subroutine errclr (subnam)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Input error clearing routine

c      Inputs:
c         subnam - Character array storing calling subroutine name

c      Outputs:
c         none
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'comfil.h'
      include  'iofile.h'
      include  'ioincl.h'

      character subnam*(*)

      if (ior.gt.0)  then
         write(iow,2000) subnam,fincld(isf),irecrd(isf),record
         call pdelfl()
         call plstop()
      endif

c     Format

 2000 format (/'  Inconsistency occurred from ',a,' in data file ',a,/
     &         '  at or near record number',i6,'.  Input record is:',
     &         //2x,a78//,
     &         '  If this record is correct error may result from'/
     &         '  missing blank record before new command type.')

      end
