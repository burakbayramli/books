!$Id:$
      subroutine perspj(xp,ntyp,numnp,errv)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Compute perspective projection of coordinates

!      Inputs:
!         xp(3,*) - Global coordinates
!         ntyp(*) - Active node indicator
!         numnp   - Total number of nodal points

!      Outputs:
!         xp(3,*) -Perspective projected coordinates
!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'iofile.h'
      include  'ppers.h'
      include  'pview.h'

      logical       :: errv
      integer       :: numnp,i,n
      integer       :: ntyp(*)
      real (kind=8) :: t1(3),xp(3,numnp), alpha

      save

!     Loop over data points and find projection

      do n=1,numnp
        if(ntyp(n).ge.0) then
          lview = .false.
          do i=1,3
            t1(i) = xp(i,n) - e(i)
          end do

          do i=1,3
            xp(i,n) = xlbda(i,1)*t1(1) + xlbda(i,2)*t1(2)
     &              + xlbda(i,3)*t1(3)
          end do

          alpha = -enorm/(t1(1)*q(1,3)+t1(2)*q(2,3)+t1(3)*q(3,3))
          if(alpha.lt.0.d0) then
            errv = .true.
            if(ior.lt.0) then
              write(*,2000)
              return
            else
              write(iow,2000)
              call plstop(.true.)
            endif
          elseif(xp(3,n).gt.zview) then
            lview = .true.
            errv  = .false.
          else
            errv  = .false.
          endif
          do i=1,3
            xp(i,n) = alpha * xp(i,n)
          end do
        endif
      end do

2000  format(//1x,' Point too close, choose another one!'//)

      end subroutine perspj
