!$Id:$
      subroutine pltlfl(ns,xl,v,vc,nc)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Plot of mesh contours: With inter-element smoothing

!      Inputs:
!         ns       - Number of segments
!         xl(3,*)  - Nodal coordinates of line segment
!         v(*)     - Nodal solution values
!         vc(*)    - Contour values
!         nc       - Number of contour intervals

!      Outputs:
!         none      - Plot outputs to screen/file
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer       :: ns,nc,icol,icx,icn,i,j,k
      real (kind=8) :: s,vci

      integer       :: ilc(9)
      real (kind=8) :: xl(3,*),v(*),vc(*) ,xp(9),yp(9),zp(9)

      save

!     Color pallet

      integer   ipal(7)
      data      ipal/1,6,4,5,3,7,2/

      call pltcor(ns+1,ilc,v,vc,nc)

      vc(nc+1) = max ( vc(1),vc(nc) )
      vci = 0.0d0
      icx = ilc(1)
      icn = ilc(1)
      do i = 1,ns
        vci      = max(vci,abs(v(i)))
        vc(nc+1) = max(vc(nc+1),v(i))
        icx      = max(ilc(i),icx)
        icn      = min(ilc(i),icn)
      end do
      vc(nc+1)   = vc(nc+1)*1.001 + vci + 1.0d-8
      do icol = icn,icx
        i = ipal(icol)
        call pppcol(i,0)
        k = 0
        i = ns
        do j = 1,ns
          if((ilc(j).ge.icol .and. ilc(i).le.icol)
     &                      .and. (ilc(i).ne.ilc(j))) then
            if(icol-1.ge.ilc(i)) then
              s = (vc(icol-1)-v(i))/(v(j)-v(i))
              k = k + 1
              xp(k) = xl(1,i) + (xl(1,j)-xl(1,i))*s
              yp(k) = xl(2,i) + (xl(2,j)-xl(2,i))*s
              zp(k) = xl(3,i) + (xl(3,j)-xl(3,i))*s
            endif
            s = (vc(icol)-v(i))/(v(j)-v(i))
            if(s.lt.1.0d0) then
              k = k + 1
              xp(k) = xl(1,i) + (xl(1,j)-xl(1,i))*s
              yp(k) = xl(2,i) + (xl(2,j)-xl(2,i))*s
              zp(k) = xl(3,i) + (xl(3,j)-xl(3,i))*s
            endif
          elseif((ilc(i).ge.icol .and. ilc(j).le.icol)
     &                          .and. (ilc(i).ne.ilc(j))) then
            s = (vc(icol)-v(i))/(v(j)-v(i))
            if(s.ge.0.0d0) then
              k = k + 1
              xp(k) = xl(1,i) + (xl(1,j)-xl(1,i))*s
              yp(k) = xl(2,i) + (xl(2,j)-xl(2,i))*s
              zp(k) = xl(3,i) + (xl(3,j)-xl(3,i))*s
            endif
            if(icol-1.ge.ilc(j)) then
              s = (vc(icol-1)-v(i))/(v(j)-v(i))
              k = k + 1
              xp(k) = xl(1,i) + (xl(1,j)-xl(1,i))*s
              yp(k) = xl(2,i) + (xl(2,j)-xl(2,i))*s
              zp(k) = xl(3,i) + (xl(3,j)-xl(3,i))*s
            endif
          endif
          if(ilc(j).eq.icol) then
            k = k + 1
            xp(k) = xl(1,j)
            yp(k) = xl(2,j)
            zp(k) = xl(3,j)
          endif
          i = j
        end do

!       Plot line of this color

        call plotl(xp(1),yp(1),zp(1),3)
        do j = 2,k
          call plotl(xp(j),yp(j),zp(j),2)
        end do
        call pppcol(0,0)
      end do

      end subroutine pltlfl
