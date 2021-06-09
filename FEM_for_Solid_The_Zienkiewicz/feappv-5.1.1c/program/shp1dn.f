!$Id:$
      subroutine shp1dn(s,shp,nel)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Purpose: Compute shape functions and natural derivatives
!              at natural coordinate s.
!              Linear (2 nodes) or quadratic (3 nodes) element.

!     Inputs:
!       s         : natural coordinate
!       nel       : number of nodes of element

!     Outputs:
!       shp(2,nel): shape functions and derivatives at s
!                   shp(1,1 to nel): derivatives of shape functions
!                   shp(2,1 to nel): shape functions
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer       :: nel
      real (kind=8) :: s,shp(2,nel)

      save

!     Linear element

      if(nel.eq.2) then

        shp(1,1) = -0.5d0
        shp(1,2) =  0.5d0

        shp(2,1) =  0.5d0 - 0.5d0*s
        shp(2,2) =  0.5d0 + 0.5d0*s

!     Quadratic element

      elseif(nel.eq.3) then

        shp(1,1) =  s - 0.5d0
        shp(1,2) =  s + 0.5d0
        shp(1,3) = -2.d0*s

        shp(2,1) =  s*(s - 1.d0)*0.5d0
        shp(2,2) =  s*(s + 1.d0)*0.5d0
        shp(2,3) =  1.d0 - s*s

!     Cubic element

      elseif(nel.eq.4) then

        shp(1,1) = 0.0625d0*( 1.d0 + 18.d0*s - 27.d0*s*s)
        shp(1,2) = 0.0625d0*(-1.d0 + 18.d0*s + 27.d0*s*s)
        shp(1,3) = 0.5625d0*(-3.d0 - 2.d0*s + 9.d0*s*s)
        shp(1,4) = 0.5625d0*( 3.d0 - 2.d0*s - 9.d0*s*s)

        shp(2,1) = 0.0625d0*(9.d0*s*s - 1.d0)*(1.d0 - 3.d0*s)
        shp(2,2) = 0.0625d0*(9.d0*s*s - 1.d0)*(1.d0 + 3.d0*s)
        shp(2,3) = 0.5625d0*(1.d0 - s*s)*(1.d0 - 3.d0*s)
        shp(2,4) = 0.5625d0*(1.d0 - s*s)*(1.d0 + 3.d0*s)

      endif

      end subroutine shp1dn
