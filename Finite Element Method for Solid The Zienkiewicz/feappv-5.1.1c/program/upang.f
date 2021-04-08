!$Id:$
      subroutine upang(ia,ang,ul,ndf,isw)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Update solution for sloping 2-d boundary

!      Inputs:

!      Outputs:
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      integer       :: ia(*), ndf,isw, j
      real (kind=8) :: ang,sn,cs,tm,ul(ndf,*)

      if(ia(1).ne.0 .and. ia(2).ne.0) then
        call pdegree(ang, sn,cs)
        if(isw.eq.1) then
          tm          =   cs*ul(ia(1),1) + sn*ul(ia(2),1)
          ul(ia(2),1) =  -sn*ul(ia(1),1) + cs*ul(ia(2),1)
          ul(ia(1),1) =   tm
        else
          do j = 1,3
            tm          =  cs*ul(ia(1),j) - sn*ul(ia(2),j)
            ul(ia(2),j) =  sn*ul(ia(1),j) + cs*ul(ia(2),j)
            ul(ia(1),j) =  tm
          end do ! j
        endif
      endif

      end subroutine upang
