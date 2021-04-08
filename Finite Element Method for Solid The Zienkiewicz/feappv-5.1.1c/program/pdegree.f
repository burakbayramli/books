!$Id:$
      subroutine pdegree(angle, sind,cosd)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Compute sin and cos in terms of degree angle.
!               F77 version

!      Input:
!         angle  - Angle in degrees

!      Outputs:
!         sind   - Sine of angle
!         cosd   - Cosine of angle
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      real (kind=8) :: angle, sind,cosd

      sind = sin(atan(1.d0)*angle/45.d0)
      cosd = cos(atan(1.d0)*angle/45.d0)

      end subroutine pdegree
