!$Id:$
      subroutine int1dn(l,sw)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Nodal quadrature for 1-d element

!      Inputs:
!         l     - Number of points

!      Outputs:
!         sw(1,*) - Gauss points
!         sw(2,*) - Gauss weights
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'iofile.h'

      integer       :: l
      real (kind=8) :: sw(2,*)

      save

!     Set end points

      sw(1,1) = -1.d0
      sw(1,l) =  1.d0

      if(l.eq.2) then

        sw(2,1) =  1.0d0
        sw(2,2) =  1.0d0

      elseif(l.eq.3) then

        sw(1,2) =  0.0d0
        sw(2,1) =  1.0d0/3.0d0
        sw(2,2) =  4.0d0/3.0d0
        sw(2,3) =  1.0d0/3.0d0

      elseif(l.eq.4) then

        sw(1,2) = -1.d0/3.d0
        sw(1,3) =  1.d0/3.d0
        sw(2,1) =  0.25d0
        sw(2,2) =  0.75d0
        sw(2,3) =  0.75d0
        sw(2,4) =  0.25d0

      else
        write(iow,2000) l
        write(*,2000) l
        call plstop(.true.)

      endif

!     Format

2000  format(' *ERROR* INT1DN: Illegal quadrature order =',i4)

      end subroutine int1dn
