!$Id:$
      subroutine udynam(du,u,ud,nneq,isw)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Perform updates to solution vectors

!      Inputs :
!        du(*)      - Increments to solution (isw=2 only)
!        u(nneq,2)  - Solution states at t_n+1
!        ud(nneq,*) - User controlled vectors
!        nneq       - Number nodal parameters (ndf*numnp)
!        isw        - Switch: 1 - initial updates at start of step
!                             2 - iterative updates withini step
!                             3 - restore solution to values at start
!                                 of step

!      Outputs:
!        ud(nneq,*) - User controlled vectors
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer       :: n,nneq,isw
      real (kind=8) :: du(*),u(nneq,2),ud(nneq,*)

!     Loop structure for partitions
      do n = 1,nneq
!       Perform steps here
      end do

      end subroutine udynam
