c$Id:$
      function ipos(file,nn)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Locate last character in character array

c      Inputs:
c         file(*) - Array to search
c         nn      - Length of array

c      Outputs:
c         ipos   - Position of last character
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer   ipos
      integer   n,nn
      character file(nn)*1

      do n = nn,1,-1
        if(file(n).ne.' ') go to 100
      end do
      n    = 0
100   ipos = n

      end
