!$Id:$
      subroutine resid2d(cfac,lfac,xsj,xsj0,shp,eps,sig,d,vl,al,p,
     &                   ndf,l)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Plane and axisymmetric residual routine

      implicit  none

      include  'eldata.h'
      include  'elplot.h'
      include  'eltran.h'
      include  'prld1.h'

      integer       :: ndf,l, j,k
      real (kind=8) :: xsj,xsj0

      real (kind=8) :: b1,b2,rr
      real (kind=8) :: aj1,aj2,aj3,aj0,lfac,cfac

      real (kind=8) :: d(*),vl(ndf,*),al(ndf,*),p(ndf,*)
      real (kind=8) :: shp(3,*),eps(*),sig(*),vc(2),ac(2)

      save

!     Compute stress-divergence vector (p)

      if(int(d(74)).gt.0) then
        b1 = d(11) + prldv(int(d(74)))*d(71)
      else
        b1 = d(11)*dm
      endif
      if(int(d(75)).gt.0) then
        b2 = d(12) + prldv(int(d(75)))*d(72)
      else
        b2 = d(12)*dm
      endif

      rr   = d(4)

!     Store time history plot data for element

      k = 10*(l-1)
      do j = 1,6
        tt(j+k) = sig(j)
      end do ! j
      k = k + 6
      do j = 1,4
        tt(j+k) = eps(j)
      end do ! j

!     Compute accelerations

      ac(1) = 0.0d0
      ac(2) = 0.0d0
      do j = 1,nel
        ac(1) = ac(1) + shp(3,j)*al(1,j)
        ac(2) = ac(2) + shp(3,j)*al(2,j)
      end do ! j
      ac(1)   = rr*ac(1)*cfac
      ac(2)   = rr*ac(2)*cfac

!     For Rayleigh Mass Damping: Compute velocity

      if(d(77).ne.0.0d0) then
        vc(1) = 0.0d0
        vc(2) = 0.0d0
        do j = 1,nel
          vc(1) = vc(1) + shp(3,j)*vl(1,j)
          vc(2) = vc(2) + shp(3,j)*vl(2,j)
        end do ! j
        vc(1)   = rr*vc(1)*cfac*d(77)
        vc(2)   = rr*vc(2)*cfac*d(77)

        aj0 = lfac*d(77)*rr
        do j = 1,nel
          p(1,j) = p(1,j) - (vc(1) + aj0*vl(1,j))*shp(3,j)*xsj
          p(2,j) = p(2,j) - (vc(2) + aj0*vl(2,j))*shp(3,j)*xsj
        end do ! j

      endif

!     Loop over rows

      do j = 1,nel
        aj1 = shp(1,j)*xsj
        aj2 = shp(2,j)*xsj
        aj3 = shp(3,j)*xsj0
        aj0 = lfac*rr

!       Compute gravity, thermal, inertia, and stress contributions

        p(1,j) = p(1,j) + (b1 - ac(1) - aj0*al(1,j))*shp(3,j)*xsj
     &                  - aj1*sig(1) - aj2*sig(4) - aj3*sig(3)
        p(2,j) = p(2,j) + (b2 - ac(2) - aj0*al(2,j))*shp(3,j)*xsj
     &                  - aj1*sig(4) - aj2*sig(2)

      end do ! j

      end subroutine resid2d
