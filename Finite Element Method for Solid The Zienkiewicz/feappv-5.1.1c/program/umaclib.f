!$Id:$
      subroutine umaclib(i,lct,ct)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Interface for user command language instructions

!      Inputs:
!         i      - Command number
!         lct    - Character array describing option
!         ct(3)  - Command parameters

!      Outputs:
!         N.B.  Users are responsible for generating command options
!               See programmer manual for example.
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      character     :: lct*(*)
      integer       :: i
      real (kind=8) :: ct(3)

      save

      if(    i.eq.1) then
        call umacr1(lct,ct)
      elseif(i.eq.2) then
        call umacr2(lct,ct)
      elseif(i.eq.3) then
        call umacr3(lct,ct)
      elseif(i.eq.4) then
        call umacr4(lct,ct)
      elseif(i.eq.5) then
        call umacr5(lct,ct)
      elseif(i.eq.6) then
        call umacr6(lct,ct)
      elseif(i.eq.7) then
        call umacr7(lct,ct)
      elseif(i.eq.8) then
        call umacr8(lct,ct)
      elseif(i.eq.9) then
        call umacr9(lct,ct)
      elseif(i.eq.10) then
        call umacr10(lct,ct)
      elseif(i.eq.11) then
        call umacr11(lct,ct)
      elseif(i.eq.12) then
        call umacr12(lct,ct)
      endif

      end subroutine umaclib
