c$Id: pltdos.f90,v 1.1 2000/08/24 20:49:59 rlt Exp $

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----------------------------------------------------------------
c      Purpose:  Library of plot outputs for Win95/NT systems

c      Inputs:
c         See individual routines

c      Outputs:
c         See individual routines
c-----------------------------------------------------------------

      integer function vgrwin()

      use      DFLIB

      implicit none

      integer          idxl,idyl,jfill
      common  /vgraph/ idxl,idyl,jfill

      integer*2       nxpix,nypix,nrows,ncols
      common /vgsize/ nxpix,nypix,nrows,ncols

      include 'iofile.h'
      include 'wdata.h'

      integer*2       :: status
      integer         :: iwinold

      save

      if(iwindow.ne.iwinold .and. iwinold.gt.1) then
        call setviewport(int2(wxy(1,1,iwinold)),int2(wxy(2,1,iwinold)),
     &                   int2(wxy(1,2,iwinold)),int2(wxy(2,2,iwinold)))
        status = displaycursor($gcursoroff)
        status = setcolorrgb(#FFFFFF)
        status = rectangle($GBORDER, int2(0),int2(0),
     &               int2(wxy(1,2,iwinold) - wxy(1,1,iwinold)-3),
     &               int2(wxy(2,2,iwinold) - wxy(2,1,iwinold)-iwinold))
      endif

      idxl = xpxl(1,iwindow)
      idyl = xpxl(2,iwindow)

      call setviewport(int2(wxy(1,1,iwindow)),int2(wxy(2,1,iwindow)),
     &                 int2(wxy(1,2,iwindow)),int2(wxy(2,2,iwindow)))
      status = displaycursor($gcursoroff)

      if(iwindow.gt.1) then
        status =  setfont( 't''Arial''h8b' )
        if(status.lt.0) write(*,*) ' WRONG FONT: n=',status
        if(ior.lt.0) then
          status = setcolorrgb(#FF0000)
          status = rectangle($GBORDER, int2(0),int2(0),
     &             int2(wxy(1,2,iwindow) - wxy(1,1,iwindow)-3),
     &             int2(wxy(2,2,iwindow) - wxy(2,1,iwindow)-iwindow))
        endif
      else
        status =  setfont( 't''Arial''h12b' )
        if(status.lt.0) write(*,*) ' WRONG FONT: n=',status
      endif
      iwinold = iwindow

      vgrwin = 0

      end

      integer function vtxwin()

      use      DFLIB

      implicit none

      integer*2       nxpix,nypix,nrows,ncols
      common /vgsize/ nxpix,nypix,nrows,ncols

      include 'iofile.h'
      include 'wdata.h'

      integer*2       :: status
      integer         :: i
      logical         :: first,textw

      data               first /.true./
      data               textw /.true./

      save

c     Reset text window size

      if(textw) then
        call settextwindow(int2(7*nrows/10+1),int2(1),
     &                     int2(nrows)       ,int2(80))
        textw = .false.
      endif

c     Put up Window 2 and 3 boxes

      if(ior.lt.0) then
        if(first) then
          do i = 2,3
            call setviewport(int2(wxy(1,1,i)),int2(wxy(2,1,i)),
     &                       int2(wxy(1,2,i)),int2(wxy(2,2,i)))
            status = displaycursor($gcursoroff)
            status = setcolorrgb(#FFFFFF)
            status = rectangle($GBORDER, int2(0),int2(0),
     &               int2(wxy(1,2,i) - wxy(1,1,i) - 3),
     &               int2(wxy(2,2,i) - wxy(2,1,i) - i))
          end do ! i
    	  first = .false.
        endif

c       Establish text window size and location

        call setviewport(int2(0),int2(0),nypix,nypix)
        status = displaycursor($gcursoron)
      endif

      call pppcol(1,0)
      vtxwin = 0

      end

      integer function vtxsiz(isw)

c     Set font size for Helvetica Bold outputs vector mode

      use      DFLIB

      implicit  none

      integer   isw

      save

      if(isw.eq.1) then
        vtxsiz =  setfont( 't''Arial''h12b' )
      elseif(isw.eq.2) then
        vtxsiz =  setfont( 't''Arial''h16b' )
      elseif(isw.eq.3) then
        vtxsiz =  setfont( 't''Arial''h20b' )
      endif
      vtxsiz = 0

      end

      integer function vclrwk()

c     Clear the workstation

      use      DFLIB

      implicit  none

      include  'iofile.h'
      include  'wdata.h'

      integer  :: status

      save

      call pppcol(-1,0)
      call ppbox(0.0d0,-0.02d0,1.47d0,1.02d0,1)
      if(iwindow.gt.1) then
        if(ior.lt.0) then
          status = setcolorrgb(#FF0000)
        else
          status = setcolorrgb(#FFFFFF)
        endif
        status = rectangle($GBORDER, int2(0),int2(0),
     &           int2(wxy(1,2,iwindow) - wxy(1,1,iwindow) - 3),
     &           int2(wxy(2,2,iwindow) - wxy(2,1,iwindow) - iwindow))
      endif
      vclrwk = 0

      end

      integer function vhomwk()

c     Home cursor - text mode

      use      DFLIB

      implicit  none

      type(rccoord) s

      save

      call settextposition( int2(1) , int2(1) , s )
      vhomwk = 0

      end

      integer function vclswk()

c     Function to close plot (if necessary)

      implicit  none

      save

      close(20, status='delete')

      vclswk = 0

      end

      integer function vgtxts(xi,yi,nn,cstr)

c     Place graphics text on screen

      use      DFLIB

      implicit  none

      integer          idxl,idyl,jfill
      common  /vgraph/ idxl,idyl,jfill

      integer          :: n,nn
      integer(2)       :: ix,iy
      real(8)          :: xi,yi
      character(len=1) :: cstr(nn)
      type(xycoord)    :: xy

      save

c     x,y locations for outgtext

      ix = xi*22000/idxl
      iy = (22200 - yi*22000)/idyl

      call moveto( ix , iy , xy )

c     Output characters one at a time

      do n = 1,nn
         call outgtext(cstr(n))
      end do ! n

      vgtxts = 0

      end

      integer function vsltyp(it)

      use      DFLIB

      implicit  none

      integer   :: it
      integer*2 :: mask(7)

      save

      data         mask /  2#1111111111111111 ,
     &                     2#1111111100000000 ,
     &                     2#1100110011001100 ,
     &                     2#1111000011110000 ,
     &                     2#1000100010001000 ,
     &                     2#1111001100110010 ,
     &                     2#1111100011111000 /

c     Set line patterns (1-bit draws; 0-skips)

      call setlinestyle( mask(it) )
      vsltyp = 0

      end

      integer function vipal(it)

      use      DFLIB

      implicit  none

      integer         idxl,idyl,jfill
      common /vgraph/ idxl,idyl,jfill

      integer :: it

      integer   ipal(15)

      save

      data  ipal/  #FFFFFF      , !   BRIGHTWHITE
     &             #0000FF      , !   RED
     &             #00FF00      , !   GREEN
     &             #FF0000      , !   BLUE
     &             #00FFFF      , !   YELLOW
     &             #FFFF00      , !   CYAN
     &             #FF00FF      , !   MAGENTA
     &             #002020      , !   BROWN
     &             #303030      , !   WHITE/GRAY
     &             #00003F      , !   LIGHTRED
     &             #003F00      , !   LIGHTGREEN
     &             #3F0000      , !   LIGHTBLUE
     &             #003F3F      , !   LIGHTYELLOW
     &             #3F3F00      , !   LIGHTCYAN
     &             #3F003F      / !   LIGHTMAGENTA

c     Set color pallet

      if(it.gt.0 .and. it.le.15 ) then
        vipal = ipal(it)
        if(jfill.lt.2) vipal = 1
      else
        vipal = #000000 ! Black
      endif

      end

      integer function vstcol(it)

      use      DFLIB

      implicit  none

c     Set text color for graphics output

      integer    :: icll
      integer    :: it, vipal

      save

      icll   = vipal(it)
      vstcol = settextcolorrgb( icll )

      end

      integer function vslcol(it)

      use      DFLIB

      implicit  none

c     Set line color for graphics output

      integer    :: icll
      integer    :: it, vipal

      save

      icll   = vipal(it)
      vslcol = setcolorrgb( icll )

      end

      integer function vpline(ixy,ipen)

c     Move/draw for lines

      use      DFLIB

      implicit  none

      integer         idxl,idyl,jfill
      common /vgraph/ idxl,idyl,jfill

      type(xycoord) :: xy
      integer(2)    :: ix,iy
      integer       :: ipen
      integer       :: ixy(2,*)

      save

c     Set cocordinates

      ix = ixy(1,1)/idxl
      iy = (22000 - ixy(2,1))/idyl

c     Draw line

      if(ipen.eq.2) then
        vpline = lineto( ix , iy )

c     Move without draw

      elseif(ipen.eq.3) then
        call     moveto( ix , iy , xy )
      end if

      end

      integer function vfarea(npt,ixy)

      use      DFLIB

c     Panel fill

      implicit  none

      integer         idxl,idyl,jfill
      common /vgraph/ idxl,idyl,jfill

      type(xycoord) :: poly (62)
      integer       :: npt, ixy(2,npt)
      integer(2)    :: n, nn

      save

c     Trace area to fill

      nn = min(31,npt)
      do n = 1,nn
        poly(n).xcoord = ixy(1,n)/idxl
        poly(n).ycoord = (22000 - ixy(2,n))/idyl
      end do ! n

c     Perform fill

      vfarea = polygon( $GFILLINTERIOR, poly , nn )
      vfarea = 0

      end
