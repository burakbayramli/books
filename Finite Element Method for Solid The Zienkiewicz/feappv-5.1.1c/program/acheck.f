!$Id:$
      subroutine acheck(x,y,n0,nl,nlc)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:   Parse a string to find fields separated by commas

!      Inputs:
!         x(*) -  Character string of data to parse
!         n0   -  Field width for parsed data
!         nl   -  Total number of characters in x-array
!         nlc  -  Total number of characters in y-array

!      Outputs:
!         y(*) -  Parsed data in field widths of n0
!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      character    :: x*(*),y*(*)
      logical      :: cksep
      integer      :: n0,nl,nlc, i,k,ii,il

      save

!     Find last character in 'x' array

      do ii = nl,1,-1
        if(x(ii:ii).ne.' ') go to 110
      end do

!     Zero the y array

110   y(1:nlc) = ' '

!     Locate separator ','

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

!     Justify numerical and character data

210   call just(y,k,n0)

      end subroutine acheck
