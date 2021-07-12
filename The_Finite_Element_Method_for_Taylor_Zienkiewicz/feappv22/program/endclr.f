c$Id:$
      subroutine    endclr (subnam,chr)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: End-of-file clearing routine

c      Inputs:
c         subnam - Character array storing calling subroutine name

c      Outputs:
c         chr    - Blank character to clear error
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'iofile.h'

      character subnam*(*),chr*(*)

      if (ior.gt.0)  then
        write(iow,2000) subnam
        call pdelfl()
        call plstop()
      else
        write(*,2000) subnam
        chr = ' '
      endif

c     Format

 2000 format (' *ERROR in ',a,' ** end of file encountered')

      end
