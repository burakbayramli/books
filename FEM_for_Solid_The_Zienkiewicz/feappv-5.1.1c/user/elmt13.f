!$Id:$
      subroutine elmt13(d,ul,xl,ix,tl,s,p,ndf,ndm,nst,isw)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved
      implicit  none

      include 'eldata.h'
      include 'umac1.h'                    ! utx(1)

      integer       :: ndf , ndm , nst , isw
      integer       :: ix(*)
      real (kind=8) :: d(*), ul(*), xl(*), tl(*), s(*), p(*)

      if(isw.lt.0) then
!       utx(1) = 'Name_U_Want'  ! 15 character naming option
!                               ! Access: MATErial "ma"
!                               !   Name_U_Want (instead of USER 13)
      elseif(isw.gt.0) then
        write(*,2000)
      endif

2000  format('    Elmt 13: *WARNING* Dummy subroutine called')

      end subroutine elmt13
