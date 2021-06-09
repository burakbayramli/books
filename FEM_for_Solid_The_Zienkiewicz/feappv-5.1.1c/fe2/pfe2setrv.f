!$Id:$
      subroutine pfe2setrv(rbuf, ns)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Modification log                                Date (dd/mm/year)
!       Original version                                    01/05/2018
!-----[--.----+----.----+----.-----------------------------------------]
!     Purpose: Set temperature and/or displacement gradient.

!     Inputs:
!       rbuf(*)    - FE2 read buffer deformation and gradient data
!       ns         - Number stress components

!     Outputs:
!       Gradients  - Output in elpers.h
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'elpers.h'    ! gradu(3,3), gradt(3)

      integer       :: ns, i,j,k
      real (kind=8) :: rbuf(*)

      save

!     Thermal problem

      if(prtype.eq.1) then

        gradt(1:3) = rbuf(1:3)
        ttemp      = rbuf(4)   ! Temperature

!     Mechanical problem

      elseif(prtype.eq.2) then

        if(finflg) then
          k = 0
          do j = 1,3
            do i = 1,3
              k          = k + 1
              gradu(i,j) = rbuf(k)
            end do ! i
          end do ! j
        else
          do i = 1,3
            gradu(i,i) = rbuf(i)
          end do ! i
          gradu(1,2) = 0.5d0*rbuf(4)
          gradu(2,1) = gradu(1,2)
          if(ns.gt.4) then
            gradu(2,3) = 0.5d0*rbuf(5)
            gradu(3,2) = gradu(2,3)
            gradu(3,1) = 0.5d0*rbuf(6)
            gradu(1,3) = gradu(3,1)
          endif
        endif
        fdet    = rbuf(10)    ! Det F

      endif

      end subroutine pfe2setrv
