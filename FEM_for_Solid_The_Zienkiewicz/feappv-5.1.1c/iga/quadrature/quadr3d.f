!$Id:$
      subroutine quadr3d(d,stiff)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: 3-D quadrature routine

!     Inputs:
!        d(*)    - Material parameters
!        stiff   - Flag to set for stiffness or mass order

!     Outputs:
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'cnurb.h'
      include   'eldata.h'
      include   'qudshp.h'
      include   'pointer.h'
      include   'comblk.h'

      logical    stiff
      integer    i,j,k,l
      real*8     d(*)

      ttfl    = .false.
      quad    = .false.
      nurbfl  = .false.

!     Nurb quadratures

      if(nint(d(189)).ge.1 .or. eltyp.gt.0) then
        nurbfl  = .true.
        iq1     = nint(d(190))
        iq2     = nint(d(191))
        iq3     = nint(d(192))
        call int1dg(iq1,s1w)
        call int1dg(iq2,s2w)
        call int1dg(iq3,s3w)
        l = 0
        do k = 1,iq3
          do j = 1,iq2
            do i = 1,iq1
              l        = l + 1
              sg3(1,l) = s1w(1,i)
              sg3(2,l) = s2w(1,j)
              sg3(3,l) = s3w(1,k)
              sg3(4,l) = s1w(2,i)*s2w(2,j)*s3w(2,k)
            end do ! i
          end do ! j
        end do ! k
        lint = iq1*iq2*iq3
        npm  = 1
      else
        if(nel.eq.4) then
          ttfl = .true.
          if(stiff) then
            l = 1
          else
            l = 2
          endif
          call tint3d(l,lint,el3)
          npm  = 1
          nvn  = 4
        elseif(nel.eq.10 .or. nel.eq.11) then
          ttfl = .true.
          if(stiff) then
            if(nel.eq.10) then
              l =  2
            else
              l = -4
            endif
          else
            l = 14
          endif
          call tint3d(l,lint,el3)
          if(nel.eq.10) then
            npm = 1
          else
            npm = 4
          endif
          nvn  = 4
        elseif(nel.eq.14 .or. nel.eq.15) then
          ttfl = .true.
          if(nel.eq.14) then
            npm = 1
          else
            npm = 4
          endif
          nvn = 4
          l   = 16
          call tint3d(l,lint,el3)
        else
          if(nel.le.8) then            !  8-node brick case
            npm = 1
            l   = 2
          elseif(nel.le.27) then       ! 27-node brick case
            npm = 4
            l   = 3
          else                         ! 64-node brick case
            npm = 10
            l   = 4
          endif
          nvn = 8
          call int3d(l,lint,sg3)
          quad = .true.
        endif
      endif

      end
