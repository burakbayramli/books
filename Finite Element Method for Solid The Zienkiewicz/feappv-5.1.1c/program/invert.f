!$Id:$
      subroutine invert(a,nmax,ndm)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Invert small square matrix

!      Inputs:
!         a(ndm,*) - Matrix to be inverted
!         nmax     - Size of upper submatrix to invert
!         ndm      - Dimension of array

!      Outputs:
!         a(ndm,*) - Submatrix replaces original terms, others not
!                    changed
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer       :: i,j,n,ndm,nmax
      real (kind=8) :: d, a(ndm,*)

      save

      do n = 1,nmax
        if(a(n,n).ne.0.0d0) then
          d = 1.d0/a(n,n)
          do j = 1,nmax
            a(n,j) = -a(n,j)*d
          end do

          do i = 1,nmax
            if(n.ne.i) then
              do j = 1,nmax
                if(n.ne.j) a(i,j) = a(i,j) +a(i,n)*a(n,j)
              end do
            endif
            a(i,n) = a(i,n)*d
          end do
          a(n,n) = d
        else
          write(*,*) ' *WARNING* Zero pivot in INVERT'
        endif
      end do

      end subroutine invert
