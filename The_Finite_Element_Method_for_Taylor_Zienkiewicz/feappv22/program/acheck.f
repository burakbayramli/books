c$Id:$
      subroutine acheck(x,y,n0,nl,nlc)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose:   Parse a string to find fields separated by commas

c      Inputs:
c         x(*) -  Character string of data to parse
c         n0   -  Field width for parsed data
c         nl   -  Total number of characters in x-array
c         nlc  -  Total number of characters in y-array

c      Outputs:
c         y(*) -  Parsed data in field widths of n0
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      logical   cksep
      integer   n0,nl,nlc, i,k,ii,il
      character x*(*),y*(*)

      save

c     Find last character in 'x' array

      do ii = nl,1,-1
        if(x(ii:ii).ne.' ') go to 110
      end do

c     Zero the y array

110   y(1:nlc) = ' '

c     Locate separator ','

      k = 0
      il= 0
      do i = 1,ii
        if(cksep(x(i:i))) then
          k  = k + n0
          if(k.gt.nlc-n0) go to 210
          il = k - i
        else
          y(i+il:i+il) = x(i:i)
        endif
      end do
      k  = k + n0

c     Justify numerical and character data

210   call just(y,k,n0)

      end
