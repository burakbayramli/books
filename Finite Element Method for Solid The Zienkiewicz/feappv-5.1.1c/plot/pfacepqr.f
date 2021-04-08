!$Id:$
        subroutine pfacepqr(np,iu,nfac)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Set plot data for brick elements

!      Inputs:
!         np      - Order of element (2 for 27 node; 3 for 64 node; etc)

!      Output:
!         iu(4,*) - 4-node quadrilateral face
!         nfac    - Number of faces
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'ublk1.h'

      integer        :: np, nfac
      integer        :: iu(4,*)
      integer        :: ne(3),ns(3), i,j, ii

      do i = 1,3
        ne(i) = np
        ns(i) = ne(i) + 1
      end do ! i

!     Negative 1-face

      nfac = 0
      do j = 1,ne(3)
        ii = ns(1)*ns(2)*(j-1)
        do i = 1,ne(2)
          nfac       = nfac + 1
          iu(1,nfac) = ns(1)*(i-1) + ii + 1
          iu(2,nfac) = iu(1,nfac) + ns(1)*ns(2)
          iu(3,nfac) = iu(2,nfac) + ns(1)
          iu(4,nfac) = iu(1,nfac) + ns(1)
        end do ! i
      end do ! j

!     Positive 1-face

      do j = 1,ne(3)
        ii = ns(1)*ns(2)*(j-1) + ne(1)
        do i = 1,ne(2)
          nfac       = nfac + 1
          iu(1,nfac) = ns(1)*(i-1) + ii + 1
          iu(2,nfac) = iu(1,nfac) + ns(1)
          iu(4,nfac) = iu(1,nfac) + ns(1)*ns(2)
          iu(3,nfac) = iu(4,nfac) + ns(1)
        end do ! i
      end do ! j

!     Negative 2-face

      do j = 1,ne(3)
        ii = ns(1)*ns(2)*(j-1)
        do i = 1,ne(1)
          nfac       = nfac + 1
          iu(1,nfac) = ii + i
          iu(2,nfac) = iu(1,nfac) + 1
          iu(4,nfac) = iu(1,nfac) + ns(1)*ns(2)
          iu(3,nfac) = iu(4,nfac) + 1
        end do ! i
      end do ! j

!     Positive 2-face

      do j = 1,ne(3)
        ii = ns(1)*ns(2)*j - ns(1)
        do i = 1,ne(1)
          nfac       = nfac + 1
          iu(1,nfac) = ii + i
          iu(2,nfac) = iu(1,nfac) + ns(1)*ns(2)
          iu(3,nfac) = iu(2,nfac) + 1
          iu(4,nfac) = iu(1,nfac) + 1
        end do ! i
      end do ! j

!     Negative 3-face

      do j = 1,ne(2)
        ii = ns(1)*(j-1)
        do i = 1,ne(1)
          nfac       = nfac + 1
          iu(1,nfac) = ii + i
          iu(2,nfac) = iu(1,nfac) + ns(1)
          iu(3,nfac) = iu(2,nfac) + 1
          iu(4,nfac) = iu(1,nfac) + 1
        end do ! i
      end do ! j

!     Positive 3-face

      do j = 1,ne(2)
        ii = ns(1)*(j-1) + ns(1)*ns(2)*(ns(3)-1)
        do i = 1,ne(1)
          nfac       = nfac + 1
          iu(1,nfac) = ii + i
          iu(2,nfac) = iu(1,nfac) + 1
          iu(4,nfac) = iu(1,nfac) + ns(1)
          iu(3,nfac) = iu(4,nfac) + 1
        end do ! i
      end do ! j

      end subroutine pfacepqr
