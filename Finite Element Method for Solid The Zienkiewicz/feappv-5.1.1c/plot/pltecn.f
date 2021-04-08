!$Id:$
      subroutine pltecn(xt,vt,vc,nc)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Plot contours/fills for 2-d surface elements.
!               N.B. All facets are divided into triangle for plots

!      Inputs:
!         xt(3,3)   - Triangle coordinates
!         vt(3)     - Triangle vertex values
!         vc(*)     - Contour values
!         nc        - Number of contours

!      Outputs:
!         none      - Plot outputs to screen/file
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'pdata2.h'
      include  'pdatri.h'
      include  'plcon1.h'
      include  'plflag.h'

      integer       :: i,j,ii,ivc,jvc,nc,nn
      real (kind=8) :: s,x1,y1,z1,vv
      real (kind=8) :: vc(12),vt(3),xt(3,3)

      save

!     Plot all contours which intersect element

      do nn = 1,nc
        vv = vc(nn)
        if(vv.ge.vlu(1).and.vv.le.vlu(2)) then
          call pppcol(nlabi+nn,1)

!         Loop over sides of triangle to find plot points

          j = 3
          do i = 1,3
            ii = mod(i,3) + 1
            if(vv.eq.vt(i)) then
              x1 = xt(1,i)
              y1 = xt(2,i)
              z1 = xt(3,i)
              call plotl(x1,y1,z1,j)
              j = 2
            elseif((vt(i)-vv)*(vt(ii)-vv).lt.0.0d0) then
              s = (vv - vt(i))/(vt(ii)-vt(i))
              x1 = xt(1,i) + s*(xt(1,ii) - xt(1,i))
              y1 = xt(2,i) + s*(xt(2,ii) - xt(2,i))
              z1 = xt(3,i) + s*(xt(3,ii) - xt(3,i))
              call plotl(x1,y1,z1,j)
              j = 2
            endif

!           Add labels

            if(vflg.and.j.eq.2) then
              ivc = max(1,min(9,nint((x1-xmn)*xmx + 1)))
              jvc = max(1,min(9,nint((y1-ymn)*ymx + 1)))
              if(tvc(ivc,jvc)) then
                tvc(ivc,jvc) = .false.
                call plotl(x1-dx1,y1,z1,3)
                if(clip) call plabl(nlabi + nn)
                call plotl(x1,y1,z1,3)
              endif
            endif
          end do ! i
        endif
      end do ! nn

      end subroutine pltecn
