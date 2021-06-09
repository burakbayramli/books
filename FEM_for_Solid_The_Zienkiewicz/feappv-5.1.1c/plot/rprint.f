!$Id:$
      subroutine rprint(dr,ndf,nfl)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:  Computes range and profile of plot value requested

!      Inputs:
!         dr(ndf,*) - Values for plot (N.B. checks dr(1,i) values)
!         ndf       - Dimension of dr-array

!      Outputs:
!         nfl       - Returns -nfl if all values are < 1.0d-08
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'cdata.h'
      include  'fdata.h'
      include  'iofile.h'
      include  'pdata2.h'
      include  'pointer.h'
      include  'prmptd.h'
      include  'rpdata.h'
      include  'comblk.h'

      logical       :: non0fl
      integer       :: ndf,nfl, i,n
      real (kind=8) :: drv,rs

      real (kind=8) :: dr(ndf,*),pr(9)

      save

!     Compute profile of values

      non0fl = .true.
      call pzero (pr,9)
      drv = 100.d0/numnp
      do n = 1,numnp
        if(mr(npty-1+n) .ge. 0) then
          if(non0fl) then
            rmx    = dr(1,n)
            rmn    = dr(1,n)
            non0fl = .false.
          else
            rmx = max(rmx,dr(1,n))
            rmn = min(rmn,dr(1,n))
          endif
        endif
      end do ! n

!     Check range for contour outputs

      if(abs(rmx-rmn).gt.1.d-5*max(abs(rmx),abs(rmn))) then
        do n = 1,numnp
          if(mr(npty-1+n) .ge. 0) then
            rs = (dr(1,n) - rmn)/(rmx - rmn)
            do i = 1,9
              if(rs.ge.0.1d0*i) pr(i) = pr(i) + drv
            end do
          endif
        end do
      else
        if(max(abs(rmx),abs(rmn)).gt.1.d-8) then
          rmn = rmn - 0.1*max(abs(rmx),abs(rmn))
          rmx = rmx + 0.1*max(abs(rmx),abs(rmn))
        else
          rmn = -1.0d-08
          rmx =  1.0d-08
          nfl = -abs(nfl)
        endif
      endif

!     Output range for min/max

      if(prompt) then
        if(pfr) write(iow,2000) rmn,rmx
        if(ior.lt.0 .and. .not.defalt) then
          write(*,2000) rmn,rmx
        endif
      endif

2000  format('    Minimum is ',1p,e10.2,' Maximum is ',1p,e10.2:/
     &  22x,'10%   20%   30%   40%   50%   60%   70%   80%   90%'/
     &       '    Profile above is:',9f6.1)

      end subroutine rprint
