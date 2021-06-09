!$Id:$
      subroutine usetlib(i)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Interface for user mesh manipulation set  commands

!      Inputs:
!         i      - Command number

!      Outputs:
!         None   - Users are responsible for providing outputs in
!                  umanii routines
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer       :: i

      save

      if(i.eq.1) then
        call umani1
      elseif(i.eq.2) then
        call umani2
      elseif(i.eq.3) then
        call umani3
      elseif(i.eq.4) then
        call umani4
      elseif(i.eq.5) then
        call umani5
      elseif(i.eq.6) then
        call umani6
      elseif(i.eq.7) then
        call umani7
      elseif(i.eq.8) then
        call umani8
      elseif(i.eq.9) then
        call umani9
      elseif(i.eq.10) then
        call umani10
      elseif(i.eq.11) then
        call umani11
      elseif(i.eq.12) then
        call umani12
      endif

      end subroutine usetlib
