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

      include   'eldata.h'
      include   'qudshp.h'
      include   'pointer.h'
      include   'comblk.h'

      logical       :: stiff
      integer       :: l
      real (kind=8) :: d(*)

      ttfl    = .false.
      quad    = .false.

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

      end subroutine quadr3d
