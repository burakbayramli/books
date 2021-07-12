c$Id:$
      function cvtc2i(cdev)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Convert a character to an integer

c      Inputs:
c         cdev    - Character array to convert

c      Outputs:
c         cvtc2i  - Integer value of character
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer   cvtc2i
      character cdev(12)*1
      integer   i,n,nz

      nz= ichar('0')
      n = 0
      do i = 1,12
        if(cdev(i).eq.char(0) .or. cdev(i).eq.' ') go to 200
        n = 10*n + (ichar(cdev(i)) - nz)
      end do

200   cvtc2i = n

      end
