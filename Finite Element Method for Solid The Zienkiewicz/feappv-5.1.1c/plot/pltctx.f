!$Id:$
      subroutine pltctx(vc,ic,iv,nc,mc)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Place contour line description on right hand side
!               of plot region.

!      Inputs:
!         vc(*)     - Values of contours to plotted
!         ic        - Component to plot
!         iv        - Color indicator for line
!         nc        - Number of contours
!         mc        - Plot type: 1 = stress;       2 = displacement;
!                                3 = velocity;     4 = acceleration;
!                                5 = prin. stress; 6 = streamline.
!                                7 = contact variables

!      Outputs:
!         none      - Plot outputs to screen/file
!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'pdata2.h'
      include  'pdatxt.h'

      character (len=17) :: yy
      character (len=13) :: strs(7)
      character (len=4)  :: slab(7)
      character (len=2)  :: ci

      integer       :: ic,iv,nc,mc, i,ivi
      real (kind=4) :: xph,yph
      real (kind=8) :: xdv,ycor,xhead,ycoi
      real (kind=8) :: vc(*),yfr(4)

      save

      data strs/' S T R E S S ',' DISPLACEMENT','  VELOCITY',
     &          ' ACCELERATION',' PRIN. STRESS','  STREAMLINE ',
     &          ' CONTACT VAR.'/

      data slab/'  1 ','  2 ','  3 ',' Ang',' I_1',' J_2',' J_3'/

      data yfr/0.90d0,0.70d0,0.50d0,0.30d0/

      xdv = 20.d0
      if(ifrm.ne.0) xdv = 40.d0
      ycor = 0.75d0
      if(ifrm.ne.0) ycor = yfr(ifrm)
      call pppcol(1,1)
      call dplot(1.00d0,ycor,3)
      write(ci,'(i2)' ) ic

!     X11 and DOS devices

      if(mc.eq.5) then
        write(yy,'(a13,a4)' ) strs(mc),slab(min(7,ic))
      else
        write(yy,'(a13,i2)' ) strs(mc),ic
      endif

      xph    = 1.0/1.28
      yph    = real(ycor/1.28d0)

      xhead  = 1.00d0
      dtext  = 0.06d0
      call pppcol(2,1)
      call tplot(xhead,ycor,yy,17,1)

      do i = 1,nc
        ivi = iv + i
        call pppcol(ivi,1)
        call dplot(1.02d0,ycor - (ivi)/xdv,3)
        write(yy, '(i3,1p1e11.3)' ) ivi,vc(i)

!       PHIGS or X11 devices

        xhead  = 1.00d0
        ycoi   = ycor - (ivi)/xdv
        xph = 1.02/1.28
        yph = real(ycoi/1.28d0)
        call tplot(xhead,ycoi,yy,14,1)

      end do

      end subroutine pltctx
