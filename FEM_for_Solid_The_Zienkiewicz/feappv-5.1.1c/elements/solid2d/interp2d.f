!$Id:$
      subroutine interp2d(l, xl,ix, ndm,nel, flag)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Interpolation functions for 2-D elements

!      Inputs:
!         l            - Quadrature point
!         xl(ndm,*)    - Nodal coordinates
!         ix(*)        - Global nodal connections
!         ndm          - Mesh coordinate dimension
!         nel          - Number of element nodes
!         flag         - Global derivatives if .false.

!      Outputs: Through common block /qudshp*/
!         shp(3,*,l)   - Shape functions
!         jac(l)       - Jacobian
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'qudshp.h'

      logical       :: flag
      integer       :: l, ndm,nel, ix(*)
      real (kind=8) :: xl(ndm,*)

      save

      if(quad) then         ! Quadrilateral element
        call shp2d(sg2(1,l),xl,shp2(1,1,l),jac(l),ndm,nel,ix,flag)
        jac(l) = jac(l)*sg2(3,l)
      else                  ! Triangular element
        call shptri(el2(1,l),xl, ndm, nel, jac(l),shp2(1,1,l),flag)
        if(flag) then
          sg2(3,l) = 0.5d0*el2(4,l) ! For proper cross products area
        endif
        jac(l) = jac(l)*el2(4,l)
      endif

      end subroutine interp2d
