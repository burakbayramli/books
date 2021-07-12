c$Id:$
      subroutine fpplps(num,xp,yp)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Output list of points to PostScript file
c               First point is a move, remaining are line draws.

c      Inputs:
c         num       - Number of points
c         xp(*)     - X-coordinates of points
c         yp(*)     - Y-coordinates of points

c      Outputs:
c         none      - Outputs written to PostScript file
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'plflag.h'
      include  'plpost.h'
      include  'psdat2.h'
      include  'psdat5.h'
      include  'psdat6.h'

      logical   pcomp
      character coord*10
      integer   i, num, x,y
      real*4    xp(*), yp(*)

      save

c     Draw line

      x = 5400.0*xp(1) + 360.0
      y = 5400.0*yp(1) + 360.0

      if( (x.ne.xold .or. y.ne.yold) .or. lfill .or.
     &                      .not.pcomp(clin,oclin,3)) then
        if( lstrk ) then
          call fppsin('s ')
        endif
        write(coord,'(i4,1x,i4,1x)') x,y
        if(.not.pcomp(clin,oclin,3)) then
          call fppsin(coord//'m '//clin)
          oclin =  clin
        else
          call fppsin(coord//'m ')
        end if

        xll = min(x,xll)
        yll = min(y,yll)
        xur = max(x,xur)
        yur = max(y,yur)

      end if

      do i=2, num

        if( xp(i).ne.xp(i-1) .or. yp(i).ne.yp(i-1) ) then
          x   = 5400.0*xp(i) + 360.0
          y   = 5400.0*yp(i) + 360.0

          xll = min(x,xll)
          yll = min(y,yll)
          xur = max(x,xur)
          yur = max(y,yur)

          write(coord,'(i4,1x,i4,1x)') x,y
          call fppsin(coord//'l ')
        endif

      end do

c     Save last drawn line point

      xold = x
      yold = y

      lstrk = .true.
      lfill = .false.

      ocvar = ' g'

      end
