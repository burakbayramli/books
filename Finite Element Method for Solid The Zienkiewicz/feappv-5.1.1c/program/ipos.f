!$Id:$
      function ipos(ifile,nn)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Locate last character in character array

!      Inputs:
!         ifile(*) - Array to search
!         nn       - Length of array

!      Outputs:
!         ipos    - Position of last character
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      character (len=1) :: ifile(nn)

      integer       :: ipos
      integer       :: n,nn

      do n = nn,1,-1
        if(ifile(n).ne.' ') go to 100
      end do
      n    = 0
100   ipos = n

      end function ipos
