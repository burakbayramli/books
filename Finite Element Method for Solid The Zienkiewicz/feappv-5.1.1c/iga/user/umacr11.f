!$Id:$
      subroutine umacr11(lct,ctl)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:  Compute extraction operator for 1-d forms.

!      Use:      EXKNots (outputs extraction operator for all knots)

!      Inputs:
!         lct       - Command character parameters
!         ctl(3)    - Command numerical parameters

!      Outputs:
!         N.B.  Users are responsible for command actions.  See
!               programmers manual for example.
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'iofile.h'
      include  'umac1.h'

      include   'pointer.h'
      include   'comblk.h'

      logical   pcomp
      character lct*15
      integer   nd_bez, ne_bez
      real*8    ctl(3)

      save

!     Set command word

      if(pcomp(uct,'ma11',4)) then      ! Usual    form
        uct = 'exkn'                    ! Specify 'EXKN'ot Operator
        nd_bez = 0
        ne_bez = 0
      elseif(urest.eq.1) then           ! Read  restart data

      elseif(urest.eq.2) then           ! Write restart data

      else                              ! Perform user operation

        if(ior.lt.0) then
           write(*,2000) ' --> Construct Extraction matrix for knots'
        endif
        call knotext(hr(np(298)),mr(np(308)))
      endif

!     Format

2000  format(a/)

      end
