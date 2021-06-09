!$Id:$
      subroutine pltefl (nel,ic,x,v,vc,nc)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--+---------+---------+---------+---------+---------+---------+-]
!      Purpose: Element fill routine for contour panel plots

!      Inputs:
!         nel       - Number of nodes/element
!         ic(*)     - Contour value at nodes
!         x(3,*)    - Nodal coordinates of element
!         v(*)      - Nodal values of contour
!         nc        - Number of values for contours

!      Outputs:
!         none      - Plot outputs to screen/file
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit  none

      integer       :: i, j, k, icol, icx, icn, nel, nc
      real (kind=8) :: vci, s

      integer       :: ic(*), ipal(7)
      real (kind=8) :: xp(10),yp(10),zp(10),x(3,*),v(*),vc(*)

      save

!     color pallet

      data ipal/1,6,4,5,3,7,2/

      vc(nc+1) = max ( vc(1),vc(nc) )
      vci = 0.0d0
      icx = ic(1)
      icn = ic(1)
      do i = 1,nel
        vci      = max(vci,abs(v(i)))
        vc(nc+1) = max(vc(nc+1),v(i))
        icx      = max(ic(i),icx)
        icn      = min(ic(i),icn)
      end do ! i
      vc(nc+1)   = vc(nc+1)*1.001 + vci + 1.0d-8
      do icol = icn,icx
        i = ipal(icol)
        call pppcol(i,2)
        k = 0
        i = nel
        do j = 1,nel
          if((ic(j).ge.icol .and. ic(i).le.icol)
     &                      .and. (ic(i).ne.ic(j))) then
            if(icol-1.ge.ic(i)) then
              s = (vc(icol-1)-v(i))/(v(j)-v(i))
              k = k + 1
              xp(k) = x(1,i) + (x(1,j)-x(1,i))*s
              yp(k) = x(2,i) + (x(2,j)-x(2,i))*s
              zp(k) = x(3,i) + (x(3,j)-x(3,i))*s
            endif
            s = (vc(icol)-v(i))/(v(j)-v(i))
            if(s.le.1.0d0) then
              k = k + 1
              xp(k) = x(1,i) + (x(1,j)-x(1,i))*s
              yp(k) = x(2,i) + (x(2,j)-x(2,i))*s
              zp(k) = x(3,i) + (x(3,j)-x(3,i))*s
            endif
          elseif((ic(i).ge.icol .and. ic(j).le.icol)
     &                          .and. (ic(i).ne.ic(j))) then
            s = (vc(icol)-v(i))/(v(j)-v(i))
            if(s.ge.0.0d0) then
              k = k + 1
              xp(k) = x(1,i) + (x(1,j)-x(1,i))*s
              yp(k) = x(2,i) + (x(2,j)-x(2,i))*s
              zp(k) = x(3,i) + (x(3,j)-x(3,i))*s
            endif
            if(icol-1.ge.ic(j)) then
              s = (vc(icol-1)-v(i))/(v(j)-v(i))
              k = k + 1
              xp(k) = x(1,i) + (x(1,j)-x(1,i))*s
              yp(k) = x(2,i) + (x(2,j)-x(2,i))*s
              zp(k) = x(3,i) + (x(3,j)-x(3,i))*s
            endif
          endif
          if(ic(j).eq.icol) then
            k = k + 1
            xp(k) = x(1,j)
            yp(k) = x(2,j)
            zp(k) = x(3,j)
          endif

!         Check for duplicate points

          if(k.gt.1) then
            if(xp(k).eq.xp(k-1) .and.
     &         yp(k).eq.yp(k-1) .and.
     &         zp(k).eq.zp(k-1) ) then
              k = k - 1
            endif
          endif
          i = j
        end do ! j

!       Plot panel of this color

        call plotl(xp(1),yp(1),zp(1),1)
        do j = 2,k
          call plotl(xp(j),yp(j),zp(j),2)
        end do ! j
        call clpan
      end do ! icol

      end subroutine pltefl
