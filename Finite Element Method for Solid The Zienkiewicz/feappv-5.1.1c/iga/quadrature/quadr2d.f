!$Id:$
      subroutine quadr2d(d,stiff)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: 2-D Quadrature formulae

!      Inputs:
!         d(*)      - Element parameters
!         stiff     - Flag to set for stiffness or mass order

!      Outputs:
!         lint      - Number of quadrature points
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'cnurb.h'
      include   'eldata.h'
      include   'pmod2d.h'
      include   'qudshp.h'
      include   'pointer.h'
      include   'comblk.h'

      logical    stiff
      integer    i,j,l
      real*8     d(*)

!     Compute Gauss quadrature points and weights for 2-d elements

      ttfl    = .false.
      quad    = .false.
      nurbfl  = .false.

!     Nurb quadratures

      if(nint(d(189)).ge.1 .or. eltyp.gt.0) then
        nurbfl  = .true.
        iq1     = nint(d(190))
        iq2     = nint(d(191))
        call int1dg(iq1,s1w)
        call int1dg(iq2,s2w)
        l = 0
        do j = 1,iq2
          do i = 1,iq1
            l        = l + 1
            sg2(1,l) = s1w(1,i)
            sg2(2,l) = s2w(1,j)
            sg2(3,l) = s1w(2,i)*s2w(2,j)
          end do ! i
        end do ! j
        lint = l

!       Set number of volume constraint functions

        npm = 1

!     Element quadratures

      else
        l = min(5,nint(d(5)))
        if(nel.eq.3) then
          if(d(182).gt.0.0d0) then
            call tint2dn(nel,lint,el2)
          else
            if(stype.le.2 .and. stiff) then
              l =  1
            else
              l = -3
            endif
            call tint2d (l,lint,el2)
          endif
          npm  =  1
          ttfl = .true.
        elseif(nel.eq.6 .or. nel.eq.7 ) then
          if(d(182).gt.0.0d0) then
            call tint2dn(nel,lint,el2)
          else
            l =  7
            call tint2d (l,lint,el2)
          endif
          if(nel.eq.6) then
            npm =  1
          else
            npm =  3
          endif
          ttfl = .true.
        else
          if(nel.le.4) then
            npm = 1
            if(l.eq.0) l = 2
          elseif(nel.le.9) then
            npm = 3
            if(l.eq.0) l = 3
          else
            npm = 6
            if(l.eq.0) l = 4
          endif
          if(nint(d(182)).gt.0) then
            call int2dn(nel,lint,sg2)
          else
            call int2d(l,lint,sg2)
          endif
          quad = .true.
        endif
      endif

      end
