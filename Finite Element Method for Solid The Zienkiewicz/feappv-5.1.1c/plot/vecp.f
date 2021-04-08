!$Id:$
      subroutine vecp (e1,e2,e3)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Vector (cross) product of two 3-d vectors

!      Inputs:
!        e1(3),e2(3) - vectors to be multiplied

!      Outputs:
!        e3(3)       - vector product ( e3 = e1xe2 )
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      real (kind=8) :: e1(3),e2(3),e3(3)

      save

      e3(1) = e1(2)*e2(3) - e1(3)*e2(2)
      e3(2) = e1(3)*e2(1) - e1(1)*e2(3)
      e3(3) = e1(1)*e2(2) - e1(2)*e2(1)

      end subroutine vecp
