!$Id:$
        subroutine interp1d(l, xl, ndm,nel, flag)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:  1-D shape functions

!      Inputs:
!         l           - Quadrature point number
!         xl(ndm,*)   - Element nodal coordinates
!         ndm         - Space dimension of mesh
!         nel         - Number of nodes/element
!         flag        - Compute global derivatives if false

!      Outputs:
!         shp(2,*,l)  - Shape functions and first derivatives
!         jac(l)      - Jacobian of point
!-----[--.----+----.----+----.-----------------------------------------]
      implicit     none

      include     'qudshp.h'

      logical       :: flag
      integer       :: l, ndm, nel
      real (kind=8) :: xl(ndm,*)

      if(quad) then
        call shp1d(sg1(1,l),xl,shp1(1,1,l),ndm,nel,jac(l))
        jac(l) = jac(l)*sg1(2,l)
      elseif(flag) then
        call shp1dn(sg1(1,l),shp1(1,1,l),nel)
      endif

      end subroutine interp1d
