c$Id:$
      subroutine pprtd

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Show current dictionary entries

c      Inputs:
c         none

c      Outputs:
c         none      - To screen/file
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'allotn.h'
      include  'allotd.h'
      include  'cdata.h'
      include  'iofile.h'
      include  'pdata2.h'
      include  'psize.h'

      character c*1
      logical   skip
      integer   point, lengt, i, j, lines, rmain, ipa, irp(2,2)

      save

      data      irp  / 3*1, 2 /
      data      lines /9/

c     Output dictionary names

      skip = idev.ne.1 .and. ior.lt.0
      write(  *,2000)
      write(iow,2000)
      do j = 1,ndict,lines
        do i = j,min(j+lines-1,ndict)

c         Assign pointer, length, and precision

          ipa   =  irp(iprec(i),ipr)
          point = (ipoint(i) + ipa - 1)/ipa - ipr*(2 - iprec(i))
          lengt = (ipoint(i+1) - ipoint(i))/ipa
          rmain =  maxm - ipoint(i+1)
          write(  *,2001) i,dict(i),ddict(i),iprec(i),point,lengt,rmain
          write(iow,2001) i,dict(i),ddict(i),iprec(i),point,lengt,rmain
        end do
        if(skip .and. min(j+lines,ndict).ne.ndict) then
          write(*,*) '   ** PRESS ENTER **'
          read(*,1000) c
          write(  *,2000)
        endif
      end do

c     Formats

1000  format(a)
2000  format(5x,'D i c t i o n a r y    o f   A r r a y s'//
     & 10x,' Entry  Array   Array  Array   Array     Array    Space  '/
     & 10x,'Number  Names  Number  Precn  Pointer   Length  Available')

2001  format(10x,i5,3x,a5,2i7,2i9,i11)

      end
