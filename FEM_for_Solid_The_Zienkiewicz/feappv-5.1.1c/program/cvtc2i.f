!$Id:$
      function cvtc2i(cdev)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Convert a character to an integer

!      Inputs:
!         cdev    - Character array to convert

!      Outputs:
!         cvtc2i  - Integer value of character
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      character (len=1) :: cdev(12)

      integer       :: cvtc2i
      integer       :: i,n,nz

      nz= ichar('0')
      n = 0
      do i = 1,12
        if(cdev(i).eq.char(0) .or. cdev(i).eq.' ') go to 200
        n = 10*n + (ichar(cdev(i)) - nz)
      end do

200   cvtc2i = n

      end function cvtc2i
