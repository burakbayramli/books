!$Id:$
      subroutine pcurvel(les,ord,lek,deg, ns,Uw, x,wt, leq,leu)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:  Curve: Elevation and knot insertion

!      Inputs:
!         les       - Length of original side control vector
!         ord       - Order of original vector
!         lek       - Length of original knot vector
!         deg       - Elevation order
!         ns(*)     - Control node list
!         Uw(*)     - Knot vector
!         x(ndm,*)  - Nurb coordinates
!         wt(*)     - Nurb weights
!         prt       - Flag, output if true

!      Outputs:
!         leq       - Length of final control vector
!         leu       - Length of final knot vector
!         UU2(*)    - Final knot vector     (in /cnurb/)
!         QQ2(*)    - Final control vector  (in /cnurb/)
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'sdata.h'
      include   'cnurb.h'
      include   'iofile.h'

      include   'pointer.h'
      include   'comblk.h'

      integer    deg, les,lek,ord, leq,leu
      integer    ns(*)
      real*8     Uw(*),x(ndm,*), wt(*)

      integer    dknot, l, pq,nd1

      save

!     Extract the control vector times weight

      call psetPw(les, ndm, ns,x,wt,PP1)

!     Compute number of distinct knots

      dknot = 1
      do l = 1,lek-1
        if(Uw(l+1).ne.Uw(l)) then
          dknot = dknot + 1
        endif
      end do ! l

      nd1 = ndm + 1

      call degreeElevateCurve(les,ord,Uw,PP1, deg,QQ2,UU2,pq, nd1)

      leq = les + (dknot-1)*deg
      leu = les + ord + dknot*deg + 1

      call psetQw(leq, ndm, ndm+1, QQ2)

      end
