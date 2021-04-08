!$Id:$
      subroutine interp2d(l, xl,ix, ndm,nel, flag)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Modification log                                Date (dd/mm/year)
!       Original version                                    11/11/2008
!       1. Add B-spline option for 6-node triangles         01/05/2010
!       2. Add 'shptri' for natural derivative only option  11/12/2012
!       3. Add set of VEM shape functions                   31/03/2017
!       4. Replace triangular shape function forms          27/05/2018
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
!         shp2(3,64,l) - Shape functions
!         jac(l)       - Jacobian
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'debugs.h'
      include   'qudshp.h'
      include   'vem_data.h'

      logical       :: flag
      integer       :: l, ndm,nel, ix(*)
      real (kind=8) :: xl(ndm,*)
      integer       :: a,i

      save

      if(vemfl) then

!       Compute reference configuration shape function derivatives

        if(k_order.eq.1 .and. ltot.eq.1) then
          do a = 1,nel
            shp2(1:2,a,l) = Pdmat(1,:,a)
            shp2(3,a,l)   = P0mat(1,a)
          end do ! a

          jac(l) = vol

        else

          do a = 1,nel
            shp2(1:2,a,l) = Pdmat(1,:,a)
            do i = 2,nkm1
              shp2(1:2,a,l) = shp2(1:2,a,l)
     &                      + mm(i,l)*Pdmat(i,1:2,a)
            end do ! i loop

!           Compute shape function at quadrature pt.

            shp2(3,a,l)    = P0mat(1,a)
            do i = 2,nk
              shp2(3,a,l) = shp2(3,a,l) + mm(i,l)*P0mat(i,a)
            end do ! i
          end do ! a loop
          jac(l) = dvol(l)

        endif

!       call mprint(shp2(1,1,l),3,nel,3,'SHP2')
!       call mprint(jac(l),1,1,1,'JAC(L)')

      elseif(quad) then         ! Quadrilateral element
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
