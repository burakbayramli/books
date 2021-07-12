c$Id:$
      subroutine fptplt(xp,yp,tx,nn,nctr)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Subroutine to place text into PostScript file

c      Inputs:
c         xp(2)     - X-location to start placing text
c         yp(2)     - Y-location to start placing text
c         tx        - Character array qith text
c         nn        - Number of characters in text
c         nctr      - Text counter value

c      Outputs:
c         none      - Output written to PostScript file
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'plflag.h'
      include  'plpost.h'
      include  'psdat2.h'
      include  'psdat5.h'
      include  'psdat6.h'

      integer   nn, j,nctr
      character tx(nn)*1, coord*10
      integer   x,y
      real*4    xp(*), yp(*)

      save

c     Close out a stroke if necessary

      if(lstrk) then
        call fppsin('s')
      endif
      call fppsdu()

c       Place text on plot

      call fppsin('H')
      call fppsdu()

c       Position text

      x = 5400.0*xp(2) + 360.0
      y = 5400.0*yp(2) + 360.0

      xll = min(x,xll)
      yll = min(y,yll)
      xur = max(x,xur) + 2.5*float(nn)
      yur = max(y,yur) + 7.0

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

      end
