!$Id:$
      subroutine plabl(m)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--+---------+---------+---------+---------+---------+---------+-]
!      Purpose: Place numerical labels on plot data
!               N.B. Must be preceded by a move to location where
!                    value is centered

!      Inputs:
!         m         - Number to place on plot

!      Outputs:
!         none      - Plot output to screen/file
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit  none

!     include  'pdata2.h'
      include  'pdatap.h'
      include  'pdatps.h'
      include  'pdatxt.h'
      include  'plflag.h'
      include  'psdat3.h'
!     include  'x11f.h'

      character (len=6) :: yyy

      integer       :: m,n,nchar
      real (kind=8) :: x1,y1

      save

!     Set number of characters

      n = abs(m)

      if    (n.ge.0   .and. n.lt.10   ) then
        write(yyy,'(i1)') n
        nchar = 1
      elseif(n.ge.10  .and. n.lt.100  ) then
        write(yyy,'(i2)') n
        nchar = 2
      elseif(n.ge.100 .and. n.lt.1000 ) then
        write(yyy,'(i3)') n
        nchar = 3
      elseif(n.ge.1000 .and. n.lt.10000 ) then
        write(yyy,'(i4)') n
        nchar = 4
      elseif(n.ge.10000 .and. n.lt.100000 ) then
        write(yyy,'(i5)') n
        nchar = 5
      else
        write(yyy,'(i6)') n
        nchar = 6
      endif

!     Windows graphics

      dtext = 0.0d0
      x1    = jx1/22000.d0
      y1    = jy1/22000.d0
      if(screfl) call tplot(x1,y1,yyy,nchar,0)

      end subroutine plabl
