!$Id:$
      subroutine interp2d(l, xl,ix, ndm,nll, flag)

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
!         nll          - Number of element nodes
!         flag         - Global derivatives if .false.

!      Outputs: Through common block /qudshp*/
!         shp(3,16,l)  - Shape functions
!         jac(l)       - Jacobian
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'cnurb.h'
      include   'eldata.h'
      include   'qudshp.h'

      include   'pointer.h'
      include   'comblk.h'

      logical          :: flag
      integer (kind=4) :: ii,l, ndm,nll, ix(*)
      real    (kind=8) :: xl(ndm,*)

!     Nurb interpolations

      if(nurbfl) then

!       NURBS patch interpolation

        call shp2d_nurb(sg2(1,l),xl,hr(np(264)),shp2(1,1,l),
     &                  shpm(1,l),jac(l),mr(np(308)),mr(np(311)),
     &                  hr(np(298)), ndm, flag)

!       Multiply jacobian by quadrature weight

        jac(l) = jac(l)*sg2(3,l)

!     Finite element quadrilateral interpolations

      elseif(quad) then     ! Quadrilateral element
        call shp2d(sg2(1,l),xl,shp2(1,1,l),jac(l),ndm,nll,ix,flag)
        jac(l) = jac(l)*sg2(3,l)

        shpm(1,l) = (0.5d0 - 0.5d0*sg2(1,l))*(0.5d0 - 0.5d0*sg2(2,l))
        shpm(2,l) = (0.5d0 + 0.5d0*sg2(1,l))*(0.5d0 - 0.5d0*sg2(2,l))
        shpm(3,l) = (0.5d0 + 0.5d0*sg2(1,l))*(0.5d0 + 0.5d0*sg2(2,l))
        shpm(4,l) = (0.5d0 - 0.5d0*sg2(1,l))*(0.5d0 + 0.5d0*sg2(2,l))
        do ii = 5,nll
          shpm(ii,l) = 0.0d0
        end do ! ii
      elseif(ttfl) then     ! Triangular element
        call shptri(el2(1,l),xl, ndm, nll, jac(l),shp2(1,1,l),flag)
        if(flag) then
          sg2(3,l) = 0.5d0*el2(4,l) ! For proper cross products area
        endif
        jac(l) = jac(l)*el2(4,l)
        shpm(1,l) = el2(1,l)
        shpm(2,l) = el2(2,l)
        shpm(3,l) = el2(3,l)
        do ii = 4,nll
          shpm(ii,l) = 0.0d0
        end do ! ii
      endif

      end subroutine interp2d
