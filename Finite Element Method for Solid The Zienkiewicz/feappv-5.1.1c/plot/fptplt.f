!$Id:$
      subroutine fptplt(xp,yp,tx,nn,nctr)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Subroutine to place text into PostScript file

!      Inputs:
!         xp(2)     - X-location to start placing text
!         yp(2)     - Y-location to start placing text
!         tx        - Character array qith text
!         nn        - Number of characters in text
!         nctr      - Text counter value

!      Outputs:
!         none      - Output written to PostScript file
!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'plflag.h'
      include  'plpost.h'
      include  'psdat2.h'
      include  'psdat5.h'
      include  'psdat6.h'

      character (len=10) :: coord
      character (len=1)  :: tx(nn)

      integer       :: nn, j,nctr
      integer       :: x,y
      real (kind=4) :: xp(*), yp(*)

      save

!     Close out a stroke if necessary

      if(lstrk) then
        call fppsin('s')
      endif
      call fppsdu()

!       Place text on plot

      call fppsin('H')
      call fppsdu()

!       Position text

      x = nint(5400.0*xp(2) + 360.0)
      y = nint(5400.0*yp(2) + 360.0)

      xll = min(x,xll)
      yll = min(y,yll)
      xur = max(x,xur) + nint(2.5*float(nn))
      yur = max(y,yur) + 7

      write(coord,'(i4,1x,i4,1x)') x,y

      call fppsin( coord//'m ')
      call fppsdu()

      if (nctr .eq. 1) then
        call fppsin('(')

        do j = 1,nn
          call fppsin(tx(j))
        end do

        call fppsin(') w')
        call fppsdu()
      endif
      call fppsin('(')

      do j = 1,nn
        call fppsin(tx(j))
      end do

      call fppsin(') '//clin//'show')
      call fppsdu()

      oclin = ' '

      lstrk = .false.
      lfill = .false.

      xold = -9980
      yold = -9980

      end subroutine fptplt
