!$Id:$
      subroutine umshlib(i,tx,prt)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Interface for user mesh commands

!      Inputs:
!         i      - Command number
!         tx(*)  - Commnand line input data
!         prt    - Flag, output if true

!      Outputs:
!         None   - Users are responsible for providing outputs in
!                  umeshi routines
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      character (len=15) :: tx(*)
      logical      ::  prt
      integer      :: i

      save

      if(i.eq.1) then
        call umesh1(tx,prt)
      elseif(i.eq.2) then
        call umesh2(tx,prt)
      elseif(i.eq.3) then
        call umesh3(tx,prt)
      elseif(i.eq.4) then
        call umesh4(tx,prt)
      elseif(i.eq.5) then
        call umesh5(tx,prt)
      elseif(i.eq.6) then
        call umesh6(tx,prt)
      elseif(i.eq.7) then
        call umesh7(tx,prt)
      elseif(i.eq.8) then
        call umesh8(tx,prt)
      elseif(i.eq.9) then
        call umesh9(tx,prt)
      elseif(i.eq.10) then
        call umesh10(tx,prt)
      elseif(i.eq.11) then
        call umesh11(tx,prt)
      elseif(i.eq.12) then
        call umesh12(tx,prt)
      endif

      end subroutine umshlib
