!$Id:$
      subroutine wprojm(a,nn,ah)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Outputs projected subspace arrays: G and H

!      Inputs:
!         a(*)        - Array to output
!         nn          - Number row/columns in array
!         ah          - Header to write (G or H)

!      Outputs:
!         none
!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'iofile.h'

      character     :: ah*(*)
      integer       :: nn,i,k,n
      real (kind=8) :: a(*)

      save

      i = 0
      write(iow,2000) ah
      do n = 1,nn
        write(iow,2001) (a(i+k),k=1,n)
        i = i + n
      end do
      if(ior.lt.0) then
        i = 0
        write(  *,2000) ah
        do n = 1,nn
          write(  *,2001) (a(i+k),k=1,n)
          i = i + n
        end do
      endif

!     Formats

2000  format(' ',a,'-Matrix ')

2001  format(1p,8d10.2)

      end subroutine wprojm
