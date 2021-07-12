c$Id:$
      subroutine pltctx(vc,ic,iv,nc,mc)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Place contour line description on right hand side
c               of plot region.

c      Inputs:
c         vc(*)     - Values of contours to plotted
c         ic        - Component to plot
c         iv        - Color indicator for line
c         nc        - Number of contours
c         mc        - Plot type: 1 = stress;       2 = displacement;
c                                3 = velocity;     4 = acceleration;
c                                5 = prin. stress; 6 = streamline.
c                                7 = contact variables

c      Outputs:
c         none      - Plot outputs to screen/file
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'pdata2.h'
      include  'pdatxt.h'

      character ci*2,strs(7)*13,slab(7)*4,yy*17
      integer   ic,iv,nc,mc, i,ivi
      real*4    xph,yph
      real*8    xdv,ycor,xhead,ycoi
      real*8    vc(*),yfr(4)

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

c     X11 and DOS devices

      if(mc.eq.5) then
        write(yy,'(a13,a4)' ) strs(mc),slab(min(7,ic))
      else
        write(yy,'(a13,i2)' ) strs(mc),ic
      endif

      xph    = 1.0/1.28
      yph    = ycor/1.28

      xhead  = 1.00d0
      dtext  = 0.06d0
      call pppcol(2,1)
      call tplot(xhead,ycor,yy,17,1)

      do i = 1,nc
        ivi = iv + i
        call pppcol(ivi,1)
        call dplot(1.02d0,ycor - (ivi)/xdv,3)
        write(yy, '(i3,1p1e11.3)' ) ivi,vc(i)

c       PHIGS or X11 devices

        xhead  = 1.00d0
        ycoi   = ycor - (ivi)/xdv
        xph = 1.02/1.28
        yph = ycoi/1.28
        call tplot(xhead,ycoi,yy,14,1)

      end do

      end
