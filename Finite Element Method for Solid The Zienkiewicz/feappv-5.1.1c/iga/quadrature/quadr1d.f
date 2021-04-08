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
!         shp1(2,*,*) - shape functions and first derivatives
!-----[--.----+----.----+----.-----------------------------------------]
      implicit none

      include  'cnurb.h'
      include  'eldata.h'
      include  'qudshp.h'

      real*8    d(*)

!     Set quadrature

      quad    = .false.
      nurbfl  = .false.

!     Nurb and extraction operator quadrature set

      if(nint(d(189)).ge.1 .or. eltyp.gt.0) then
        nurbfl  = .true.
        lint    = nint(d(190))
        call int1dg(lint,sg1)
        npm     = 1

!     Lagrange element quadrature

      else
        quad = .true.
        lint = min(5,nint(d(5)))
        if(lint.le.0) then
          lint = nel
        endif
        call int1dg(lint,sg1)
        npm = nel - 1
      endif

      end
