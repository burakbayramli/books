c$Id: pppcol.f,v 1.1 2000/08/24 20:49:59 rlt Exp $
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
      integer   coli,status,vstcol,vslcol

      integer   ipal(15), ipsm(15)

      save

      data      ipal   / 1, 6, 4, 5, 3, 7, 2, 8, 9,14,12,13,11,15,10/
      data      ipsm   / 1, 0, 3, 2, 5, 4, 6, 7, 9, 8,11,10,13,12,14/

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

          do ii=1,15
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
              ii = max( 1, min( 7, 8 - ii) )
            else
              ii = max( 1, min( 7, ii    ) )
            endif
            write( colv, '(i1,1x)' ) ii
          endif
          if(isw.eq.0) then
            write(clin,4002) ii
          endif
        endif
      endif

      coli = max(0,icol)
      if (coli .gt. 7 ) coli = mod(coli,7)
      if(screfl) status = vstcol(coli)
      if(screfl) status = vslcol(coli)

c     Format

4001  format(' g',i1)
4002  format('h',i1,' ')


      end
