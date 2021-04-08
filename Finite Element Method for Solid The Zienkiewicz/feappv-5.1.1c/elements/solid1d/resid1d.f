!$Id:$
      subroutine resid1d(cfac,lfac,xsj,xsj0,shp,sig,d,vl,al,p,ndf)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Plane and axisymmetric residual routine

!      Inputs:

!      Outputs:
!         p(ndf,*)  - Element residual
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'eldata.h'
      include  'eltran.h'
      include  'pmod2d.h'

      integer       :: ndf, j
      real (kind=8) :: aj1,aj2,aj0,rr,lfac,cfac,xsj,xsj0, vc,ac
      real (kind=8) :: d(*),vl(ndf,*),al(ndf,*)
      real (kind=8) :: p(ndf,*),shp(2,*),sig(*),bf(3)

      save

!     Compute body force values

      call sbodyf(d, bf)

!     Compute accelerations

      ac = 0.0d0
      do j = 1,nel
        ac = ac + shp(2,j)*al(1,j)
      end do ! j
      rr = d(4)
      ac = rr*ac*cfac

!     For Rayleigh Mass Damping: Compute velocity

      if(d(77).ne.0.0d0) then
        vc = 0.0d0
        do j = 1,nel
          vc = vc + shp(2,j)*vl(1,j)
        end do ! j
        vc = rr*vc*cfac*d(77)

        do j = 1,nel
          aj0 = lfac*d(77)*rr
          p(1,j) = p(1,j) - (vc + aj0*vl(1,j))*shp(2,j)*xsj
        end do ! j

      endif

!     Loop over nodes

      do j = 1,nel
        aj1 = shp(1,j)*xsj
        aj2 = shp(2,j)*xsj0
        aj0 = lfac*rr


!       Compute gravity, thermal, inertia, and stress contributions

        p(1,j) = p(1,j) + (bf(1) - ac - aj0*al(1,j))*shp(2,j)*xsj
     &                  - aj1*sig(1) - aj2*sig(3)
!       Check for spherical symmetry

        if(stype.eq.9) then
          p(1,j) = p(1,j) - aj2*sig(2)
        endif

      end do ! j

      end subroutine resid1d
