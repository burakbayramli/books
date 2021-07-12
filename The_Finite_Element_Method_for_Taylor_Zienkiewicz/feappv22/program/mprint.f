c$Id:$
      subroutine mprint(a,ii,jj,mm,name)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Output array of integer values

c      Inputs:
c         a(mm,*)  - Array to output
c         ii       - Number of rows to output
c         jj       - Number of columns to output
c         mm       - Dimension of array
c         name     - Name of array to appear with outputs

c      Outputs:
c         none
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'iofile.h'

      character name*(*), aname*30
      integer   ii,jj,mm,nn, ja,jb,j,i,n,irow
      real*8    a(mm,*)

c     Print an ii x jj array whose dimension is mm for first subscript

      aname = name
      nn    = (jj+5)/6
      jb    = 0

      do n = 1,nn

        ja = jb + 1
        jb = min(jj,ja + 5)

c       Output to O file

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

c       Output to screen if interactive

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

c     Formats

 2000 format(/4x,'Matrix: ',a30/4x,'row/col',i6,5i12)
 2001 format(i8,1p,6e12.4)

      end
