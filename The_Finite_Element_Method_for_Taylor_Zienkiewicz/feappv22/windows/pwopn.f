c$Id: pwopn.f90,v 1.1 2000/08/24 20:49:59 rlt Exp $
      subroutine pwopn ()

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose:

c      Inputs:

c      Outputs:
c-----[--.----+----.----+----.-----------------------------------------]
      use       DFLIB

      implicit  none

      include  'pdata2.h'
      include  'pdatps.h'
      include  'plclip.h'
      include  'plflag.h'
      include  'print.h'
      include  'wdata.h'

      integer         idxl,idyl,jfill
      common /vgraph/ idxl,idyl,jfill

      integer*2       nxpix,nypix,nrows,ncols
      common /vgsize/ nxpix,nypix,nrows,ncols

      type(qwinfo)        :: winfo
      type(windowconfig)  :: textscreen
      type(xycoord)       :: xy
      integer             :: status,i
      integer(2)          :: numfonts
      real*4              :: fact,cons,inc
      character*8         :: gtext

c     Open Child Console Window

      open (unit = 0, file = 'CON' )

c     Get Current Input Data Window Size

      status = getwindowconfig(textscreen)
      nxpix = 97*textscreen.numxpixels/100
      nypix = 82*textscreen.numypixels/100
      ncols = textscreen.numtextcols
      nrows = textscreen.numtextrows

c     Set up Input Data Window

      textscreen.numxpixels   =  nxpix
      textscreen.numypixels   =  nypix
      textscreen.numtextcols  =  ncols
      textscreen.numtextrows  =  nrows
      textscreen.numcolors    =  -1
      textscreen.fontsize     = #0007000E
      textscreen.title =
     &"Finite  Element  Analysis  Program - Personal  Version"C
      textscreen.bitsperpixel = -1
      status = setwindowconfig(textscreen)
      if(status.eq.0) status = setwindowconfig(textscreen)

c     Maximize window

      winfo.type = QWIN$SET
      winfo.x   =  0
      winfo.y   =  0
      winfo.w   =  nxpix
      winfo.h   =  nypix
      status     = setwsizeqq(0,winfo)

c     Set font for Helvetica Bold outputs vector mode

      numfonts=initializefonts()
      if (numfonts.le.0) print *,"INITIALIZEFONTS error"
      if (grstatus().ne.$GROK) print *,'INITIALIZEFONTS GRSTATUS error.'
c     status = setfont( 't''helv''h10w5b' )
      status = setfont( 't''Arial''h16b' )

c     Set up scaling and sizing for FEAPpv graphics

      idx   = 22000
      idy   = 22000
      idyl  = nint(22480.0/(nypix*0.7))
      idxl  = idyl
      jfill = 2

      if(nxpix.lt.800) then
        fact = 1.14
        cons = 0.9
        inc  = 0.0005
      else
        fact = 1.0
        cons = 1.0
        inc  = 0.000625
      endif

c     Establish window coords and scales

      iwindow = 1

      xpxl(1,1)  = idxl
      xpxl(2,1)  = idyl
      wxy(1,1,1) = 10
      wxy(2,1,1) =  2
      wxy(1,2,1) = nypix*0.9
      wxy(2,2,1) = nypix

      xpxl(1,2)  = cons*(5*idxl/3)
      xpxl(2,2)  = cons*(5*idyl/3)
      wxy(1,1,2) = fact*nypix+3
      wxy(2,1,2) = 3
      wxy(1,2,2) = nxpix
      wxy(2,2,2) = nypix/2

      xpxl(1,3)  = cons*(5*idxl/3)
      xpxl(2,3)  = cons*(5*idyl/3)
      wxy(1,1,3) = fact*nypix + 3
      wxy(2,1,3) = nypix/2 + 3
      wxy(1,2,3) = nxpix
      wxy(2,2,3) = nypix

c     Set flags to permit drawing colors

      clchk  = .false.
      fwin   = .true.
      screfl = .true.

c     Establish initial graphics window size and location

      do i = 2,3
        status = setcolorrgb(#FF0000)   ! blue
        call setviewport(int2(wxy(1,1,i)),int2(wxy(2,1,i)),
     &                   int2(wxy(1,2,i)),int2(wxy(2,2,i)))
        status = rectangle($GBORDER, int2(0),int2(0),
     &           int2(wxy(1,2,i) - wxy(1,1,i)-3),
     &           int2(wxy(2,2,i) - wxy(2,1,i)-i))
        status = setcolorrgb(#FFFFFF)   ! white
        call moveto(int2((wxy(1,2,i) - wxy(1,1,i))/3),
     &              int2((wxy(2,2,i) - wxy(2,1,i))/2), xy )
        write(gtext,'(a6,i2)') 'Window',i
        call outgtext( gtext )
      end do ! i

      call setviewport(int2(0),int2(0),nypix,nypix)
      status = displaycursor($gcursoroff)

      status = setcolorrgb(#00FF00) ! green
      do i = 2,6
        status = rectangle($GBORDER, int2(i),int2(i),
     &           int2(nypix-i), int2(0.4*nypix-i))
      end do ! i

      do i = 1,11
        call pfeap(-0.1d0+i*inc,0.55d0-i*inc,1.3d0,3,3) ! Border
      end do ! i
      call pfeap(-0.1d0,0.55d0,1.3d0,2,1) ! Fill

c     Establish initial text window size and location

      call pppcol(1,1)
      call settextwindow(int2(4*nrows/10+1),int2(1),
     &                   int2(nrows)       ,int2(80))
      status = displaycursor($gcursoron)
      status = setfont( 't''Arial''h10b' )

      end
