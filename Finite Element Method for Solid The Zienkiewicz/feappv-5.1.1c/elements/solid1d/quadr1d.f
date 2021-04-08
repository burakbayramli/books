!$Id:$
      subroutine quadr1d(d)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:  1-D quadrature

!      Inputs:
!         d(*)        - Material set parameters

!      Outputs:
!         sg1(2,*)    - Quadrature points
!         shp1(2,*,*) - shape funcitons and first derivatives
!-----[--.----+----.----+----.-----------------------------------------]
      implicit none

      include  'eldata.h'
      include  'qudshp.h'

      real (kind=8) :: d(*), sg(2,10)

!     Set quadrature

      lint = min(5,nint(d(5)))
      if(lint.eq.0) then
        lint = nel
      endif
      call int1d(lint,sg)
      sg1(1,1:lint) = sg(1,1:lint)
      sg1(2,1:lint) = sg(2,1:lint)
      quad = .true.
      npm  =  nel - 1

      end subroutine quadr1d
