c$Id:$
      subroutine fppspl(num,xp,yp)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Save a set of plot points for PostScript panel fills

c      Inputs:
c         num       - Number of points
c         xp(*)     - X-coordinates for plots
c         yp(*)     - Y-coordinates for plots

c      Outputs:
c         none      - Outputs are written to PostScript file
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'plflag.h'
      include  'plpost.h'
      include  'prmptd.h'
      include  'psdat2.h'
      include  'psdat5.h'
      include  'psdat6.h'

      character coord*10
      integer   num, i, numc, x,y
      real*4    xp(*), yp(*)

      save

c     Close out a line

      if(lstrk) then
        call fppsin('s')
        call fppsdu()
      endif

      if(xp(1).eq.xp(num) .and. yp(1).eq.yp(num)) then
        numc = num - 1
      else
        numc = num
      endif

c     Draw a fill area

      x = 5400.0*xp(1) + 360.0
      y = 5400.0*yp(1) + 360.0

      xll = min(x,xll)
      yll = min(y,yll)
      xur = max(x,xur)
      yur = max(y,yur)

      write(coord,'(i4,1x,i4,1x)') x,y
      call fppsin(coord//'m ')

      do i = 2, numc

        x = 5400.0*xp(i) + 360.0
        y = 5400.0*yp(i) + 360.0

        xll = min(x,xll)
        yll = min(y,yll)
        xur = max(x,xur)
        yur = max(y,yur)

        write(coord,'(i4,1x,i4,1x)') x,y
        call fppsin(coord//'l ')

      end do

      if( pscolr ) then

        call fppsin( 'c h'//colv//' f ')
        ocolv = colv

      else

        call fppsin( 'c'//cvar//'f ')
        ocvar = cvar

      endif

      lstrk = .false.
      lfill = .true.
      call fppsdu()

      xold  = x
      yold  = y

      oclin = ' '

      end
