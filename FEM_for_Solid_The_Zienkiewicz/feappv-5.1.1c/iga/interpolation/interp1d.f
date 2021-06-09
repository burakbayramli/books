!$Id:$
        subroutine interp1d(l, xl, ndm,nll, flag)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:  1-D shape functions

!      Inputs:
!         l           - Quadrature point number
!         xl(ndm,*)   - Element nodal coordinates
!         ndm         - Space dimension of mesh
!         nll         - Number of nodes/element
!         flag        - Global NURBS and T-spline derivatives if false

!      Outputs:
!         shp(2,*,l)  - Shape functions and first derivatives
!         jac(l)      - Jacobian of point
!-----[--.----+----.----+----.-----------------------------------------]
      implicit     none

      include     'cnurb.h'
      include     'eldata.h'
      include     'qudshp.h'
      include     'pointer.h'
      include     'comblk.h'

      logical          :: flag
      integer (kind=4) :: l, ndm, nll, i
      real    (kind=8) :: xl(ndm,*)

!     Nurb shape functions

      if(nurbfl) then

!       NURBS form

        call shp1d_nurb(sg1(1,l),xl,hr(np(264)),shp3(1,1,l),jac(l),
     &                  mr(np(308)),mr(np(311)),hr(np(298)),ndm, flag)

!       Copy shape functions to other arrays

        do i = 1,nll
           shp1(1,i,l) = shp3(1,i,l)   ! Stores function & 1st derivs
           shp1(2,i,l) = shp3(4,i,l)

           shp2(1,i,l) = shp3(1,i,l)   ! Stores function & 2nd derivs
           shp2(2,i,l) = shp3(2,i,l)
           shp2(3,i,l) = shp3(4,i,l)
         end do ! i

!     Lagrangian shape functions

      else
        call shp1d(sg1(1,l),xl,shp1(1,1,l),ndm,nll,jac(l))
      endif

!     Multiply jacobian by quadrature weight

      jac(l) = jac(l)*sg1(2,l)

      end subroutine interp1d
