!$Id:$
      subroutine pstr2d(sig,pp)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Compute principal stresses for 2-d problems.

!      Input:
!         sig(1) - Stresses in order: sig-xx, sig-yy, sig-zz, sig-xy
!      Output:
!         pp(1)  - Principal stresses in order: sig-1, sig-2, angle
!                  (degrees): sig-xx to sig-1
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      real (kind=8) :: sig(4), pp(3), xi1, xi2, rho

      save

      xi1 = (sig(1) + sig(2))*0.5d0
      xi2 = (sig(1) - sig(2))*0.5d0
      rho = sqrt(xi2*xi2 + sig(4)*sig(4))
      pp(1)  = xi1 + rho
      pp(2)  = xi1 - rho
      if(xi2.ne.0.0d0) then
        pp(3) = 22.5d0*atan2(sig(4),xi2)/atan(1.0d0)
      else
        pp(3)  = 45.0d0
      endif

      end subroutine pstr2d
