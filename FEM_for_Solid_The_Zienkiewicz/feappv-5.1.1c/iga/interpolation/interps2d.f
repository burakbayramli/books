!$Id:$
      subroutine interps2d(l, xl,ix, ndm,nll, flag)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Interpolation functions for 2-D elements with second
!               derivatives.

!      Inputs:
!         l              - Quadrature point
!         xl(ndm,*)      - Nodal coordinates
!         ix(*)          - Global nodal connections
!         ndm            - Mesh coordinate dimension
!         nll            - Number of element nodes
!         flag           - Global derivatives if .false.

!      Outputs: Through common block /qudshp*/
!         shp2 (3,64,l)  - Shape functions
!         shps2(3,64,l)  - Shape functions
!         jac(l)         - Jacobian
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'cnurb.h'
      include   'eldata.h'
      include   'qudshp.h'

      include   'pointer.h'
      include   'comblk.h'

      logical          :: flag
      integer (kind=4) :: jj,l, ndm,nll, ix(*)
      real    (kind=8) :: xl(ndm,*),shpl(6,64)

!     Nurb interpolations

      if(nurbfl) then

!       NURBS Blocks

        call shp2d_shl(sg2(1,l),xl,hr(np(264)),shpl,jac(l),
     &                 mr(np(308)),mr(np(311)),hr(np(298)),ndm, flag)

!       Move functions to element variables

        do jj = 1,nel
          shp2 (1,jj,l) = shpl(2,jj)    ! N,x
          shp2 (2,jj,l) = shpl(3,jj)    ! N,y
          shp2 (3,jj,l) = shpl(1,jj)    ! N
          shps2(1,jj,l) = shpl(4,jj)    ! N,xx
          shps2(2,jj,l) = shpl(5,jj)    ! N,yy
          shps2(3,jj,l) = shpl(6,jj)    ! N,xy
        end do ! jj

!       Multiply jacobian by quadrature weight

        jac(l) = jac(l)*sg2(3,l)

!     Finite element quadrilateral interpolations

      elseif(quad) then

        call shp2d(sg2(1,l),xl,shp2(1,1,l),jac(l),ndm,nll,ix,flag)
        jac(l) = jac(l)*sg2(3,l)

!     Finite element triangular interpolations

      elseif(ttfl) then

        call shptri(el2(1,l),xl,ndm,nll-4,jac(l),shp2(1,1,l),flag)
        jac(l) = jac(l)*el2(4,l)

      endif

      end subroutine interps2d
