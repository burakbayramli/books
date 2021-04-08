!$Id:$
      subroutine pltprf(jp,neq,lower)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Purpose: Display plot of profile layout

!     Inputs:
!        jp(*)   - Column pointers
!        neq     - Number of equations

!     Outputs:
!        none
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'eqsym.h'
      include  'pdata1.h'

      logical       :: lower
      integer       :: n,neq,jp(neq)
      real (kind=8) :: x0,y0, x,y,c, pfact,rfact,ptone,ptnin,onept

      save

!     Set plot factors and start coordinate

      rfact = 0.5d0/(scaleg*fact)
      pfact = 0.8d0/dble(neq)*rfact

      x0    = 0.5d0*sx(1) - 0.5d0*rfact
      y0    = 0.5d0*sx(2) - 0.5d0*rfact

      ptone = 0.1d0*rfact
      onept = 1.0d0*rfact
      ptnin = 0.9d0*rfact

!     Upper part

      call pppcol(3,1)
      do n = 2,neq
        x = ptone + dble(n)*pfact
        y = onept - x + y0
        c = dble(jp(n) - jp(n-1))*pfact
        x = x + x0

        call plotl( x    , y    , 0.0d0, 3)
        call plotl( x    , y + c, 0.0d0, 2)

      end do ! n

!     Lower part

      if(lower) then
        call pppcol(4,1)
        do n = 2,neq
          x = ptone + dble(n)*pfact
          y = onept - x + y0
          c = dble(jp(n) - jp(n-1))*pfact
          x = x + x0

          call plotl( x - c, y    , 0.0d0, 3)
          call plotl( x    , y    , 0.0d0, 2)

        end do ! n
      end if

!     Diagonal

      call pppcol(2,1)
      call plotl( ptone + pfact + x0, ptnin - pfact + y0, 0.0d0, 3)
      call plotl( ptnin         + x0, ptone         + y0, 0.0d0, 2)

      end subroutine pltprf
