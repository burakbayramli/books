!$Id:$
      subroutine pfacev(ix,x,ndm,iln,ct,ip,nface)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Plot visible exterior faces of 3-d objects

!      Inputs:
!         ix(4)      - List of face nodes
!         x(ndm,*)   - Nodal coordinates
!         iln(2)     - Line type data
!         ct         - Also plot back faces when > 0
!         ip(*)      - Sort data for hidden surface representations
!         nface      - Number of faces

!      Outputs:
!         none       - Plot outputs to screen/file
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'comblk.h'
      include  'pointer.h'

      logical       :: visbl
      integer       :: j, k, ndm, nface

      integer       :: ix(4), ip
      integer       :: iln(2),ilnd(2)
      real (kind=8) :: x(ndm,*),xl(3,4),ct

      save

      data      ilnd(1)/1/

      ilnd(2) = iln(2)

!     Plot face

      do j = 1,4
        do k = 1,3
          xl(k,j) = x(k,ix(j))
        end do
      end do

      if(visbl(xl) .or. (ix(1).eq.ix(4) .and. ix(2).eq.ix(3))) then

        nface = nface + 1
        if(ct.ge.0.0d0) then
          call plline(iln)
          call plotl(xl(1,4),xl(2,4),xl(3,4),3)
          do j = 1,4
            call plotl(xl(1,j),xl(2,j),xl(3,j),2)
            mr(np(66)+ix(j)-1) = 1
          end do
        end if

      elseif(ct.gt.0.0d0) then

        ip = 0
        call plline(ilnd)
        call plotl(xl(1,4),xl(2,4),xl(3,4),3)
        do j = 1,4
          call plotl(xl(1,j),xl(2,j),xl(3,j),2)
        end do

      else

        ip = 0

      endif

      end subroutine pfacev
