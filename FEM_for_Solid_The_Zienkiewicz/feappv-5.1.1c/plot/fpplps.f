!$Id:$
      subroutine fpplps(num,xp,yp)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Output list of points to PostScript file
!               First point is a move, remaining are line draws.

!      Inputs:
!         num       - Number of points
!         xp(*)     - X-coordinates of points
!         yp(*)     - Y-coordinates of points

!      Outputs:
!         none      - Outputs written to PostScript file
!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'plflag.h'
      include  'plpost.h'
      include  'psdat2.h'
      include  'psdat5.h'
      include  'psdat6.h'

      character (len=10) :: coord

      logical       :: pcomp
      integer       :: i, num, x,y
      real (kind=4) :: xp(*), yp(*)

      save

!     Draw line

      x = nint(5400.0*xp(1) + 360.0)
      y = nint(5400.0*yp(1) + 360.0)

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
          x   = nint(5400.0*xp(i) + 360.0)
          y   = nint(5400.0*yp(i) + 360.0)

          xll = min(x,xll)
          yll = min(y,yll)
          xur = max(x,xur)
          yur = max(y,yur)

          write(coord,'(i4,1x,i4,1x)') x,y
          call fppsin(coord//'l ')
        endif

      end do

!     Save last drawn line point

      xold = x
      yold = y

      lstrk = .true.
      lfill = .false.

      ocvar = ' g'

      end subroutine fpplps
