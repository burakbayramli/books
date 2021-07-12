c$Id: pppcol.f,v 1.1 2000/08/24 20:49:58 rlt Exp $
      subroutine pppcol(icol,jsw)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--+---------+---------+---------+---------+---------+---------+-]
c      Purpose: Set color for plot lines, panels, text

c      Inputs:
c         iclr     - Color parameter
c         isw      - Switch:

c      Outputs:
c         none     - Set values returned in common blocks
c-----[--+---------+---------+---------+---------+---------+---------+-]

      implicit  none

      include  'pdata2.h'
      include  'pdatps.h'
      include  'plflag.h'
      include  'prmptd.h'
      include  'psdat2.h'

      integer   icol, ii, isw, jsw,jj
      real*4    c1,c2

      integer   j
      integer   ipal(7), ipsm(7)

      save

      data      j       / 0 /
      data      ipal   / 1, 6, 4, 5, 3, 7, 2/
      data      ipsm   / 1, 0, 3, 2, 5, 4, 6/

c     Set color of all quantities

      isw = abs(jsw)

c     Grayscale and color for postscript

      if (hdcpy) then

c       Line for postscript

        if(isw.eq.0 .or. isw.eq.1) then

c         Reverse line color for black background

          if(blk) then
            clin = 'g1 '
          else
            clin = 'g0 '
          endif
        endif

c       Gray scale for postscript

        if(isw.eq.0 .or. isw.eq.2) then

          do ii=1,7
            if (icol .eq. ipal(ii)) goto 110
          end do
          ii = 1

  110     if (icol .lt. 0) then
            jj = 1
          elseif (icol .eq. 0) then
            jj = 2
          else
            jj = ipsm(ii) + 2
          endif
          write(cvar,4001) jj

c         Color for postscript

          if(icol.le.0) then
            if(blk) then
              colv = '0 '
            else
              colv = '8 '
            endif
          else
            if(psrevs) then
              ii = max( 1, min(7, 8 - ii) )
            else
              ii = max( 1, min(7, ii    ) )
            endif
            write( colv, '(i1,1x)' ) ii
          endif
          if(isw.eq.0) then
            write(clin,4002) ii
          endif
        endif
      endif


c     X11 device

c     Limit to colors 0 through 7

      j  = icol
      j  = max(0,min(7,j))
      c1 = j
      c2 = j+1
      if(screfl) then
        call gdx11(8,c1,c2)
      endif

c     Format

4001  format(' g',i1)
4002  format('h',i1,' ')

      end
