!$Id:$
      function pdiff(x,i,ndm,numnp)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Compute the difference between maximum and minimum
!               nodal coordinates in direction-i.

!      Inputs:
!         x(ndm,* ) - Nodal coordinates for mesh
!         i         - Direction of comparison
!         ndm       - Spatial dimension of mesh
!         numnp     - Number of nodes in mesh

!      Outputs:
!         pdiff     - Difference between maximum and minimum
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'iofile.h'
      include  'pointer.h'
      include  'comblk.h'

      integer       :: i, n, ndm, numnp
      real (kind=8) :: pdiff,  xmx, xmn, x(ndm,numnp)

      save

      do n = 1,numnp
         if(mr(np(190)-1+n).ge.0) go to 110
      end do ! n

      if(ior.gt.0) then
        write(iow,3000)
      else
        write(*,3000)
      endif
      pdiff = 0.0
      return

110   xmx = x(i,n)
      xmn = x(i,n)
      do n = 1,numnp
        if(mr(np(190)-1+n).ge.0) then
          xmx = max(xmx,x(i,n))
          xmn = min(xmn,x(i,n))
        endif
      end do ! n

      pdiff = xmx - xmn

!     Format

3000  format(' *ERROR* PDIFF: Coodinates are unspecified')

      end function pdiff
