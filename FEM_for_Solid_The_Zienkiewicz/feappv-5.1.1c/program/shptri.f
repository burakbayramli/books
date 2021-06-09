!$Id:$
      subroutine shptri(el, xl, ndm,nel, xsj,shp,flg)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Modification log                                Date (dd/mm/year)
!       Original version                                    10/12/2012
!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Computes shape function and derivatives for
!               triangular elements

!      Inputs:
!         el(2)     - Natural coordinates for point
!         xl(ndm,*) - Nodal coordinates
!         ndm       - Nodal dimension
!         nel       - Number of nodes on element
!         flg       - Compute global derivatives if false

!         Derivs:   d/dxi_1 = d/dL_1 - d/dL_3
!                   d/dxi_2 = d/dL_2 - d/dL_3

!      Outputs:
!         xsj       - Jacobian determinant of triangle
!         shp(3,*)  - Shape functions and derivatives at point
!                     shp(1,i) = dN_i/dx_1 or dN_i/dxi_1
!                     shp(2,i) = dN_i/dx_2 or dN_i/dxi_2
!                     shp(3,i) = N_i
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'iofile.h'

      logical          ::  flg
      integer          ::  ndm,nel, k
      real    (kind=8) ::  xsj, fac
      real    (kind=8) ::  el(4), xl(ndm,*), shp(3,*), xs(3,2)
      real    (kind=8) ::  xsi(2,2), dx(3)

      save

      if(nel.le.7) then

!       3-node triangle

        if(nel.eq.3) then
          shp(1,1) =  1.0d0
          shp(2,1) =  0.0d0
          shp(3,1) =  el(1)

          shp(1,2) =  0.0d0
          shp(2,2) =  1.0d0
          shp(3,2) =  el(2)

          shp(1,3) = -1.0d0
          shp(2,3) = -1.0d0
          shp(3,3) =  1.0d0 - el(1) - el(2)

!       6- and 7-node triangle

        else
          shp(1,1) =  4.0d0*el(1) - 1.0d0
          shp(2,1) =  0.0d0
          shp(3,1) =  2.0d0*el(1)*el(1) - el(1)

          shp(1,2) =  0.0d0
          shp(2,2) =  4.0d0*el(2) - 1.0d0
          shp(3,2) =  2.0d0*el(2)*el(2) - el(2)

          shp(1,3) =  1.0d0 - 4.0d0*el(3)
          shp(2,3) =  1.0d0 - 4.0d0*el(3)
          shp(3,3) =  2.0d0*el(3)*el(3) - el(3)

          shp(1,4) =  4.0d0*el(2)
          shp(2,4) =  4.0d0*el(1)
          shp(3,4) =  4.0d0*el(1)*el(2)

          shp(1,5) = -4.0d0*el(2)
          shp(2,5) =  4.0d0*(el(3) - el(2))
          shp(3,5) =  4.0d0*el(2)*el(3)

          shp(1,6) =  4.0d0*(el(3) - el(1))
          shp(2,6) = -4.0d0*el(1)
          shp(3,6) =  4.0d0*el(1)*el(3)

          if(nel.eq.7) then    ! Hierarchic bubble
            shp(1,7) = 27.0d0*el(2)*(el(3) - el(1))
            shp(2,7) = 27.0d0*el(1)*(el(3) - el(2))
            shp(3,7) = 27.0d0*el(1)*el(2)*el(3)
          endif

        endif

!       Jacobian transformation
        xs(:,:) = 0.0d0
        do k = 1,nel
          xs(:,1) = xs(:,1) + xl(1:ndm,k)*shp(1,k)
          xs(:,2) = xs(:,2) + xl(1:ndm,k)*shp(2,k)
        end do ! k

        if(ndm.eq.2) then
          xsj = xs(1,1)*xs(2,2) - xs(1,2)*xs(2,1)

          if(.not.flg) then
            xsi(1,1) =  xs(2,2)/xsj
            xsi(2,2) =  xs(1,1)/xsj
            xsi(1,2) = -xs(1,2)/xsj
            xsi(2,1) = -xs(2,1)/xsj

            do k = 1,10
              fac      = xsi(1,1)*shp(1,k) + xsi(2,1)*shp(2,k)
              shp(2,k) = xsi(1,2)*shp(1,k) + xsi(2,2)*shp(2,k)
              shp(1,k) = fac
            end do ! k
          endif

        elseif(ndm.eq.3) then
          dx(1) = xs(2,1)*xs(3,2) - xs(3,1)*xs(2,2)
          dx(2) = xs(3,1)*xs(1,2) - xs(1,1)*xs(3,2)
          dx(3) = xs(1,1)*xs(2,2) - xs(2,1)*xs(1,2)
          xsj = sqrt(dx(1)**2 + dx(2)**2 + dx(3)**2)

          if(.not.flg) then
            write(iow,4000)
            write(  *,4000)
            call plstop(.true.)
          endif
        endif

        xsj = xsj*0.5d0

!     10-node triangle

      else

        write(*,*)
     &        ' --> ERROR: 10-Node triaangles not available in FEAPpv'
        call plstop(.true.)

      endif

!     Formats

4000  format(5x,'*ERROR* SHPTRI: Global 3-d derivative not coded')

      end subroutine shptri
