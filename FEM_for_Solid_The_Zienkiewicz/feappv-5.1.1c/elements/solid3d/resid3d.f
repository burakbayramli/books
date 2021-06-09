!$Id:$
      subroutine resid3d(xsj,shp,sig,d,vl,al,p,ndf,l)

!     Purpose: 3-D residual routine

      implicit  none

      include  'eldata.h'
      include  'elplot.h'
      include  'eltran.h'
      include  'prld1.h'

      integer       :: ndf,l, j,k
      real (kind=8) :: xsj

      real (kind=8) :: b1,b2,b3,rr
      real (kind=8) :: aj1,aj2,aj3,aj0,lfac,cfac

      real (kind=8) :: d(*),vl(ndf,*),al(ndf,*),p(ndf,*)
      real (kind=8) :: shp(4,*),sig(*),ac(3),vc(3)

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

      if(int(d(76)).gt.0) then
        b3 = d(13) + prldv(int(d(76)))*d(73)
      else
        b3 = d(13)*dm
      endif

      rr   = d(4)
      if(d(7).ge.0.0d0) then
        cfac = d(7)
        lfac = 1.d0 - cfac
      else
        cfac = 0.0d0
        lfac = 0.0d0
      endif

!     Store time history plot data for element

      k = 6*(l-1)
      do j = 1,6
        tt(j+k) = sig(j)
      end do ! j

!     Compute accelerations

      ac(1) = 0.0d0
      ac(2) = 0.0d0
      ac(3) = 0.0d0
      do j = 1,nel
        ac(1) = ac(1) + shp(4,j)*al(1,j)
        ac(2) = ac(2) + shp(4,j)*al(2,j)
        ac(3) = ac(3) + shp(4,j)*al(3,j)
      end do ! j
      ac(1)   = rr*ac(1)*cfac
      ac(2)   = rr*ac(2)*cfac
      ac(3)   = rr*ac(3)*cfac

!     For Rayleigh Mass Damping: Compute velocity

      if(d(77).ne.0.0d0) then
        vc(1) = 0.0d0
        vc(2) = 0.0d0
        vc(3) = 0.0d0
        do j = 1,nel
          vc(1) = vc(1) + shp(4,j)*vl(1,j)
          vc(2) = vc(2) + shp(4,j)*vl(2,j)
          vc(3) = vc(3) + shp(4,j)*vl(3,j)
        end do ! j
        vc(1)   = vc(1)*cfac
        vc(2)   = vc(2)*cfac
        vc(3)   = vc(3)*cfac

        do j = 1,nel
          aj0    = shp(4,j)*xsj*rr*d(77)
          p(1,j) = p(1,j) - (vc(1) + lfac*vl(1,j))*aj0
          p(2,j) = p(2,j) - (vc(2) + lfac*vl(2,j))*aj0
          p(3,j) = p(3,j) - (vc(3) + lfac*vl(3,j))*aj0
        end do ! j

      endif

!     Loop over rows

      do j = 1,nel
        aj1 = shp(1,j)*xsj
        aj2 = shp(2,j)*xsj
        aj3 = shp(3,j)*xsj
        aj0 = lfac*rr

!       Compute gravity, thermal, inertia, and stress contributions

        p(1,j) = p(1,j) + (b1 - ac(1) - aj0*al(1,j))*shp(4,j)*xsj
     &                  - aj1*sig(1)  - aj2*sig(4)  - aj3*sig(6)
        p(2,j) = p(2,j) + (b2 - ac(2) - aj0*al(2,j))*shp(4,j)*xsj
     &                  - aj1*sig(4)  - aj2*sig(2)  - aj3*sig(5)
        p(3,j) = p(3,j) + (b3 - ac(3) - aj0*al(3,j))*shp(4,j)*xsj
     &                  - aj1*sig(6)  - aj2*sig(5)  - aj3*sig(3)

      end do ! j

      end subroutine resid3d
