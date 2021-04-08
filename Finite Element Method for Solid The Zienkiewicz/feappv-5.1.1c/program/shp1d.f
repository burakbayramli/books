!$Id:$
      subroutine shp1d(s,xl,shp,ndm,nel,xjac)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Purpose: Compute shape functions, natural derivatives, and
!              jacobian for 3-D line at natural coordinate s.
!              Linear (2 nodes) or quadratic (3 nodes) element.

!     Inputs:
!       s         : natural coordinate
!       xl(3,nel) : nodal global coordinates
!       ndm       : coordinate dimension of mesh
!       nel       : number of nodes of element

!     Outputs:
!       shp(2,nel): shape functions and derivatives at s
!                   shp(1,1 to nel): derivatives of shape functions
!                   shp(2,1 to nel): shape functions
!       xjac      : jacobian at s
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer       :: ndm,nel,i
      real (kind=8) :: s,xjac
      real (kind=8) :: xl(ndm,nel),shp(2,nel)

      save

!     Linear element

      if(nel.eq.2) then

        xjac = 0.0d0
        do i = 1,ndm
          xjac = xjac + (xl(i,2) - xl(i,1))**2
        end do ! i
        xjac = sqrt(xjac)

        shp(2,1) = (1.d0 - s)*0.5d0
        shp(2,2) = (1.d0 + s)*0.5d0

        shp(1,1) = -1.d0/xjac
        shp(1,2) =  1.d0/xjac

!     Quadratic element

      elseif(nel.eq.3) then

!       Shape function natural derivatives

        shp(1,1) =  s - 0.5d0
        shp(1,2) =  s + 0.5d0
        shp(1,3) = -s*2.d0

        shp(2,1) =  s*(s - 1.d0)*0.5d0
        shp(2,2) =  s*(s + 1.d0)*0.5d0
        shp(2,3) =  1.d0 - s*s

!       Jacobian

        xjac = 0.0d0
        do i = 1,ndm
          xjac = xjac + (shp(1,1)*xl(i,1)
     &                +  shp(1,2)*xl(i,2)
     &                +  shp(1,3)*xl(i,3))**2
        end do ! i
        xjac = sqrt(xjac)

        shp(1,1:3) = shp(1,1:3)/xjac
      endif

      end subroutine shp1d
