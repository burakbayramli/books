!$Id:$
      subroutine pmovec(id,a,b,nn)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:  Move compressed array a into uncompressed array b

!      Inputs:
!         a(*)      - Compressed array to move
!         nn        - Length of uncompressed array

!      Outputs:
!         b(*)    - Uncompressed move of a (zero undefined values)
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer       :: n,nn,j
      integer       :: id(*)
      real (kind=8) :: a(nn),b(nn)

      save

!     Move a-array into b-array

      do n = 1,nn
        j = id(n)
        if (j.gt.0) then
          b(n) = a(j)
        else
          b(n) = 0.0d0
        endif
      end do

      end subroutine pmovec
