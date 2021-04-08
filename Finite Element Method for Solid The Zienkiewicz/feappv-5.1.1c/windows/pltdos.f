!$Id:$

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Modification log                                Date (dd/mm/year)
!       Original version                                    01/11/2006
!       1. Change DFLIB to IFQWIN                           10/04/2014
!       2. Force Fit To Size for text window                28/09/2017
!-----[----------------------------------------------------------------]
!      Purpose:  Library of plot outputs for Windows systems

!      Inputs:
!         See individual routines

!      Outputs:
!         See individual routines
!-----[----------------------------------------------------------------]
      integer function vopnwk()

      use        IFQWIN

      implicit   none

      include  'iofile.h'
      include  'plflag.h'
      include  'pdata2.h'

      integer         idxl,idyl,jfill
      common /vgraph/ idxl,idyl,jfill

      type(windowconfig) :: myscreen
      type(qwinfo)       :: winfo,frmwindow
      logical            :: status
      integer(2)         :: numfonts
      integer            :: uchild

!     Open workstation, home cursor, set up scaling

      if(screfl) then

        uchild = 7
        open(unit=uchild,file='user',title='F E A P p v    P l o t s')

!       Get window size data for plot outputs

        status  = getwsizeqq(uchild,qwin$sizecurr,winfo)
        status  = getwsizeqq(qwin$framewindow,qwin$sizemax,frmwindow)

!       Position for Graphics screen (in text row/columns shifts)

        winfo.X = winfo.X + 6*winfo.w/10  ! frmwindow.W - winfo.W
        winfo.Y = winfo.Y                 ! frmwindow.H - winfo.H

        winfo.type=qwin$set

        status  = setwsizeqq(uchild,winfo)
        if(.not.status) status = setwsizeqq(uchild,winfo)

!       Get screen capability

        status = getwindowconfig(myscreen)

!       Set sizes to maximum available

        myscreen.numtextcols=-1
        myscreen.numtextrows=-1
        myscreen.fontsize   =-1

!       Sizing graphics screen

        myscreen.numypixels= (myscreen.numypixels)*0.6
        myscreen.numxpixels= (myscreen.numypixels)*1.3

!       Set window configuration

        status = setwindowconfig(myscreen)
        if(.not.status) status = setwindowconfig(myscreen)

!       Set sizing for lines drawn by FEAP

        idyl   = nint(22480.0/(myscreen.numypixels))
        idxl   = idyl

        if(myscreen.numcolors .le. 4) then
          jfill = 1
        else
          jfill = 2
        endif

        vopnwk = displaycursor ( $GCURSOROFF   )
        call     clearscreen   ( $GCLEARSCREEN )

!       Set font for Arial outputs vector mode

        numfonts=initializefonts()
        if (numfonts.le.0) print *,"INITIALIZEFONTS error"
        if (grstatus().ne.$GROK) then
          write(*,*) 'INITIALIZEFONTS GRSTATUS error.'
        endif
        vopnwk =  setfont( 't''Arial''h14b' )

      endif

!     Tile Windows for Maximum Viewing

      status = focusqq(uchild)
      status = clickmenuqq(loc(WINSIZETOFIT))
      status = focusqq(5)             ! Input window active
      status = clickmenuqq(loc(WINSIZETOFIT))
      status = setactiveqq(uchild)

      vopnwk = 0

      end

      integer function vclrwk()

!     Clear the workstation

      use        IFQWIN

      implicit   none

      save

      call clearscreen( $GCLEARSCREEN )
      vclrwk = 0

      end

      integer function vclswk()

!     Function to close plot (if necessary)

      implicit   none

      save

      close(7, status='delete')

      vclswk = 0
      end

      integer function vgtxts(xi,yi,nn,cstr)

!     Place graphics text on screen

      use        IFQWIN

      implicit   none

      integer         idxl,idyl,jfill
      common /vgraph/ idxl,idyl,jfill

      integer          :: n,nn
      integer(2)       :: ix,iy
      real(8)          :: xi,yi
      character(len=1) :: cstr(nn)
      type(xycoord)    :: xy

      save

!     x,y locations for outgtext

      ix = xi*22000/idxl
      iy = (22200 - yi*22000)/idyl

      call moveto( ix , iy , xy )

!     Output characters one at a time

      do n = 1,nn
         call outgtext(cstr(n))
      end do ! n

      vgtxts = 0

      end

      integer function vipal(it)

      use        IFQWIN

      implicit   none

      integer         idxl,idyl,jfill
      common /vgraph/ idxl,idyl,jfill

      integer :: it
      integer    ipal(15)

      save

      data  ipal/  #FFFFFF      , !   1: BRIGHTWHITE
     &             #0000FF      , !   2: RED
     &             #00FF00      , !   3: GREEN
     &             #FF0000      , !   4: BLUE
     &             #00FFFF      , !   5: YELLOW
     &             #FFFF00      , !   6: CYAN
     &             #FF00FF      , !   7: MAGENTA
     &             #002020      , !   8: BROWN
     &             #303030      , !   9: WHITE/GRAY
     &             #00003F      , !  10: LIGHTRED
     &             #003F00      , !  11: LIGHTGREEN
     &             #3F0000      , !  12: LIGHTBLUE
     &             #003F3F      , !  13: LIGHTYELLOW
     &             #3F3F00      , !  14: LIGHTCYAN
     &             #3F003F      / !  15: LIGHTMAGENTA

!     Set color pallet

      if(it.gt.0 .and. it.le.15 ) then
        vipal = ipal(it)
        if(jfill.lt.2) vipal = 1
      else
        vipal = #000000 ! Black
      endif

      end

      integer function vstcol(it)

      use        IFQWIN

      implicit   none

!     Set text color for graphics output

      integer    :: icll
      integer    :: it, vipal

      save

      icll   = vipal(it)
      vstcol = settextcolorrgb( icll )

      end

      integer function vslcol(it)

      use        IFQWIN

      implicit   none

!     Set line color for graphics output

      integer    :: icll
      integer    :: it, vipal

      save

      icll   = vipal(it)
      vslcol = setcolorrgb( icll )

      end

      integer function vpline(ixy,ipen)

!     Move/draw for lines

      use        IFQWIN

      implicit   none

      integer         idxl,idyl,jfill
      common /vgraph/ idxl,idyl,jfill

      type(xycoord) :: xy
      integer(2)    :: ix,iy
      integer       :: ipen
      integer       :: ixy(2,*)

      save

!     Set cocordinates

      ix = ixy(1,1)/idxl
      iy = (22000 - ixy(2,1))/idyl

!     Draw line

      if(ipen.eq.2) then
        vpline = lineto( ix , iy )

!     Move without draw

      elseif(ipen.eq.3) then
        call     moveto( ix , iy , xy )
      end if

      end

      integer function vfarea(npt,ixy)

      use        IFQWIN

!     Panel fill

      implicit   none

      integer         idxl,idyl,jfill
      common /vgraph/ idxl,idyl,jfill

      type(xycoord) :: poly (62)
      integer       :: npt, ixy(2,npt)
      integer(2)    :: n, nn

      save

!     Trace area to fill

      nn = min(31,npt)
      do n = 1,nn
        poly(n).xcoord = ixy(1,n)/idxl
        poly(n).ycoord = (22000 - ixy(2,n))/idyl
      end do ! n

!     Perform fill

      vfarea = polygon( $GFILLINTERIOR, poly , nn )
      vfarea = 0

      end
