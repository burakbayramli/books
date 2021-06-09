!$Id:$
      subroutine poutnurb(x,wt, l1,l2)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:  Output NURB mesh

!      Inputs:
!        x(ndm,*)   - Global NURB coordinate array
!        wt(*)      - Global NURB weight vector
!        l1,l2      - Number NURB vectors in two block directions

!      Outputs:
!        To file
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'cnurb.h'
      include   'iodata.h'
      include   'sdata.h'

      integer    numx, n,j, l1,l2
      real*8     x(ndm,*),wt(*)

      numx = l1*l2
!     write(ios,'(/a,i8)') 'NURBs ALL',numx
      write(ios,'(/a,i8)') 'NURBs', numx
      do n = 1,numx
        write(ios,2001) n+nnurnp,0,(x(j,n),j=1,ndm),wt(n)
      end do !
      nnurnp = nnurnp + numx

!     Formats

!2001  format(i8,i3,1p,4e24.15)
2001  format(i8,i3,1p,4e16.8)

      end
