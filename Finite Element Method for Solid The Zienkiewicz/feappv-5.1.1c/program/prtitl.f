!$Id:$
      subroutine prtitl(prtf)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Output title to file/screen

!      Inputs:
!         prtf      - Flag, output title if true

!      Outputs:
!         None      - Outputs to file/screen
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'bdata.h'
      include  'iofile.h'

      logical       :: prtf

      save

      if(prtf) then
        write(iow,2000) o,head
        if(ior.lt.0) then
          write(*,2000) o,head
        endif
      endif

2000  format(/a1,19a4,a2/1x)

      end subroutine prtitl
