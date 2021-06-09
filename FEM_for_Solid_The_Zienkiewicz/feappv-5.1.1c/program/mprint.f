!$Id:$
      subroutine mprint(a,ii,jj,mm,namea)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Output array of integer values

!      Inputs:
!         a(mm,*)  - Array to output
!         ii       - Number of rows to output
!         jj       - Number of columns to output
!         mm       - Dimension of array
!         namea    - Name of array to appear with outputs

!      Outputs:
!         none
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'iofile.h'

      character (len=30) :: aname*30
      character          :: namea*(*)

      integer       :: ii,jj,mm,nn, ja,jb,j,i,n,irow
      real (kind=8) :: a(mm,*)

!     Print an ii x jj array whose dimension is mm for first subscript

      aname = namea
      nn    = (jj+5)/6
      jb    = 0

      do n = 1,nn

        ja = jb + 1
        jb = min(jj,ja + 5)

!       Output to O file

        if(n.eq.1 .or. ii.gt.1) then
          write(iow,2000) aname,(j,j=ja,jb)
        endif
        do i = 1,ii
          if(ii.eq.1) then
            irow = n
          else
            irow = i
          endif
          write(iow,2001) irow,(a(i,j),j=ja,jb)
        end do

!       Output to screen if interactive

        if(ior.lt.0) then
          if(n.eq.1 .or. ii.gt.1) then
            write(*,2000) aname,(j,j=ja,jb)
          endif
          do i = 1,ii
            if(ii.eq.1) then
              irow = n
            else
              irow = i
            endif
            write(*,2001) irow,(a(i,j),j=ja,jb)
          end do
        endif

      end do

!     Formats

 2000 format(/4x,'Matrix: ',a30/4x,'row/col',i6,5i12)
 2001 format(i8,1p,6e12.4)

      end subroutine mprint
