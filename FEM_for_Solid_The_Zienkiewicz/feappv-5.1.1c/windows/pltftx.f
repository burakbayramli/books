!$Id:$
      subroutine pltftx(vc,ic,mc)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--+---------+---------+---------+---------+---------+---------+-]
!      Purpose: Place contour fill description on right hand side
!               of plot region.

!      Inputs:
!         vc(*)     - Values of contours to plotted
!         ic        - Component to plot
!         mc        - Plot type: 1 = stress;       2 = displacement;
!                                3 = velocity;     4 = acceleration;
!                                5 = prin. stress; 6 = streamline.
!                                7 = contact var.;

!      Outputs:
!         none      - Plot outputs to screen/file
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit  none

      include  'pdata1.h'
      include  'pdata2.h'
      include  'pdatxt.h'
      include  'pdatps.h'
      include  'pdatap.h'
      include  'plcapt.h'
      include  'psdat1.h'
      include  'rpdata.h'
      include  'sdata.h'

      character (len=17) :: yy,blnk
      character (len=13) :: strs(7)
      character (len=4)  :: slab(7)

      integer       :: ic, mc, i
      real (kind=4) :: xph,yph

      real (kind=8) :: xdv,dy,xleft,xright,xtext,xhead
      real (kind=8) :: ycor,yphbot,yphtop

      integer       :: ipal(7)
      real (kind=8) :: vc(6),yfr(4)

      save

      data      ipal/1,6,4,5,3,7,2/

      data strs/' S T R E S S ',' DISPLACEMENT','  VELOCITY',
     &          ' ACCELERATION',' PRIN. STRESS','  STREAMLINE ',
     &          ' CONTACT VAR.'/

      data slab/'  1 ','  2 ','  3 ',' Ang',' I_1',' J_2',' J_3'/
      data blnk/'_________________'/

!     Try some other y-positions for multiple contours

      data yfr/0.805d0,0.625d0,0.445d0,0.265d0/

!     PC device position parameters

      dtext  = 0.0600d0
      xhead  = 1.0000d0
      xleft  = 1.0400d0
      xright = 1.0800d0
      xtext  = 1.0900d0

!     Put box around caption region

      if(ifrm.eq.0) then
        call pppcol(-1,0)
        call ppbox(0.98d0,0.10d0,0.29d0,0.70d0,1)
        xdv  = 20.d0
        ycor = 0.75d0
      else
        xdv  = 70.d0
        ycor = yfr(ifrm)
      endif
      dy  = 1.d0/xdv
      xph = 1./1.28
      yph = real(ycor/1.28d0)

!     Draw color bars - with values

      do i = 1,7
        call pppcol(ipal(i),2)
        yphbot = ycor - 1.35d0*dy - 5.d0/7.d0*i/xdv
        yphtop = ycor - 1.35d0*dy - 5.d0/7.d0*(i-1)/xdv
        call dplot( xleft,yphbot,1)
        call dplot(xright,yphbot,2)
        call dplot(xright,yphtop,2)
        call dplot( xleft,yphtop,2)
        call clpan
        call pppcol(1,1)
        if(i.lt.7) then
          yphbot = yphbot - 0.0075d0
          if(ifrm.eq.0 .or. i.eq.1 .or. i.eq.6) then
            write(yy, '(1p1e9.2)' ) vc(i)
            call tplot(xtext,yphbot,yy,9,1)
          endif
        endif
      end do

      if(ifrm.eq.0) then
        write(yy, '(1p1e9.2)' ) rmn
        yphbot = ycor - 1.35d0*dy - 0.0075d0
        call tplot(xtext,yphbot,yy,9,1)
        write(yy, '(1p1e9.2)' ) rmx
        yphbot = yphbot - 5.d0/xdv
        call tplot(xtext,yphbot,yy,9,1)
      endif

      dtext = 0.11d0
      call tplot(xhead,ycor,blnk,17,1)
      if(mc.eq.5) then
        if(ic.eq.6) then
          write(yy,'(a17)' ) '  MISES STRESS   '
        else
          write(yy,'(a13,a4)' ) strs(mc),slab(min(7,ic))
        endif
      else
        write(yy,'(a13,i2)' ) strs(mc),ic
      endif
      if(ncapt.gt.0) then
        yy       = ' '
        yy(2:16) = caption
      endif
      call tplot(xhead,ycor,yy,17,1)
      ncapt = 0

!     Draw box to contain color bars

      call dplot( xleft , ycor - 1.35d0*dy           , 3)
      call dplot( xright, ycor - 1.35d0*dy           , 2)
      call dplot( xright, ycor - 1.35d0*dy - 5.d0/xdv, 2)
      call dplot( xleft , ycor - 1.35d0*dy - 5.d0/xdv, 2)
      call dplot( xleft , ycor - 1.35d0*dy           , 2)

!     Add horizonal divider lines / ticks

      do i = 1,6
        call dplot(xleft        , ycor-1.35d0*dy-5.d0/7.d0*i/xdv,3)
        call dplot(xright+.005d0, ycor-1.35d0*dy-5.d0/7.d0*i/xdv,2)
      end do

!     Write min/max for current view

      if(ifrm.eq.0) then

!       Display current view values

        write(yy, '(12hCurrent View)' )
        call tplot(xhead ,ycor - 1.25d0*dy - 6./xdv,yy,12,1)

!       Display min for current view - with coords

        write(yy, '(6hMin = ,1p1e9.2)' ) psmn
        call tplot(xhead+0.25d0*dy,ycor-1.85d0*dy-6./xdv,yy,15,1)

        write(yy, '(3hX =,1p,1e9.2)' ) xpsn(1)
        call tplot(xhead ,ycor - 2.35d0*dy - 6./xdv,yy,12,1)
        write(yy, '(3hY =,1p,1e9.2)' ) xpsn(2)
        call tplot(xhead ,ycor - 2.85d0*dy - 6./xdv,yy,12,1)
        if(ndm.eq.3) then
          write(yy, '(3hZ =,1p,1e9.2)' ) xpsn(3)
          call tplot(xhead ,ycor - 3.35d0*dy - 6./xdv,yy,12,1)
        endif

!       Display max for current view - with coords

        write(yy, '(6hMax = ,1p1e9.2)' ) psmx
        call tplot(xhead+0.25d0*dy,ycor-3.85d0*dy-6./xdv,yy,15,1)

        write(yy, '(3hX =,1p,1e9.2)' ) xpsx(1)
        call tplot(xhead ,ycor - 4.35d0*dy - 6./xdv,yy,12,1)
        write(yy, '(3hY =,1p,1e9.2)' ) xpsx(2)
        call tplot(xhead ,ycor - 4.85d0*dy - 6./xdv,yy,12,1)
        if(ndm.eq.3) then
          write(yy, '(3hZ =,1p,1e9.2)' ) xpsx(3)
          call tplot(xhead ,ycor - 5.35d0*dy - 6./xdv,yy,12,1)
        endif

      endif

      end subroutine pltftx
