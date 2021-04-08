!$Id:$
      function nbuck(x,xmd,xmn,ndm,nsize)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Determine bucket number for bucket searche routine

!      Inputs:
!         x(3)     - Coordinate to determine bucket number
!         xmd(3)   - Size of bucket
!         xmn      - Minimum coordinate of bucket
!         ndm      - Spatial dimension of mesh
!         nsize(4) - Number of buckets/direction: (4) indicates
!                    if all points to be placed in one bucket.

!      Outputs:
!         nbuck    - Bucket number containing data point x
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer       :: nbuck
      integer       :: ndm,nn,ni,i, nsize(4)
      real (kind=8) :: x(3),xmd(3),xmn(3)

      if(nsize(4).gt.1) then
        nn = nint((x(1) - xmn(1))/xmd(1))
        nn = max(0,min(nsize(1)-1,nn))
        do i = 2,ndm
          ni = nint((x(i) - xmn(i))/xmd(i))
          nn = nsize(i)*nn + max(0,min(nsize(i)-1,ni))
        end do
        nbuck = nn + 1
      else
        nbuck = 1
      endif

      end function nbuck
