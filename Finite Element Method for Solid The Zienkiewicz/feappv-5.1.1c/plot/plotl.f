!$Id:$
      subroutine plotl(xx1,xx2,xx3,ipen)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Line drawing command: Converts mesh coordinates to
!               screen coordinate values

!      Inputs:
!         xx1       - X-coordinate for plot
!         xx2       - Y-coordinate for plot
!         xx3       - Z-coordinate for plot
!         ipen      - Pen position

!      Outputs:
!         none      - Plot outputs to screen/file
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'plclip.h'
      include  'pdata1.h'
      include  'ppers.h'
      include  'pview.h'

      logical       :: errv
      integer       :: ipen, ntyp(1)
      real (kind=8) ::  xx1,xx2,xx3, x1,x2,x3, s1,s2
      real (kind=8) ::  xg(3)

      save

      data      ntyp / 1 /

!     Recover plot coordinates

      x1 = xx1
      x2 = xx2
      x3 = xx3

!     Perform perspective tranformation

      if(kpers.eq.1) then
        xg(1) = x1
        xg(2) = x2
        xg(3) = x3
        call perspj(xg,ntyp,1,errv)
        if(lview) return
        x1 = xg(1)
        x2 = xg(2)
        x3 = xg(3)
      endif

!     Compute the normal coordinates


      s1 = scalef*(x1 + x1 - sx(1)) + s0(1)
      s2 = scalef*(x2 + x2 - sx(2)) + s0(2)

      s1 = max(0.0d0,min(1.45d0,s1))
      s2 = max(0.0d0,min(1.00d0,s2))

      clchk = .true.
      call dplot(s1,s2,ipen)
      clchk = .false.

      end subroutine plotl
