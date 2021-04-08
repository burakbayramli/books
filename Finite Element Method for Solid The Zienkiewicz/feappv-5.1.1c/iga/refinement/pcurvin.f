!$Id:$
      subroutine pcurvin(les,ord,lek,uu,r, ns,Uw, x,wt, leq,leu)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:  Curve: Elevation and knot insertion

!      Inputs:
!         les       - Length of original side control vector
!         ord       - Order of original vector
!         lek       - Length of original knot vector
!         uu        - Knot location to insert
!         r         - Repeated times uu added
!         ns(*)     - Control node list
!         Uw(*)     - Knot vector
!         x(ndm,*)  - Nurb coordinates
!         wt(*)     - Nurb weights

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

      integer    les,ord,lek, r, leq,leu
      real*8     uu
      integer    ns(*)
      real*8     Uw(*),x(ndm,*),wt(*)

      integer    k, nd1, s

      save

!     Extract the control vector times weight

      call psetPw(les, ndm, ns,x,wt,PP1)

!     Compute number of distinct knots

      nd1 = ndm + 1

      call psetUw_ks(lek,Uw, uu, k,s)

      call curveknotins(les-1,ord, Uw,PP1, uu, k,s,r, leq,UU2,QQ2, ndm)

      leq = leq + 1
      leu = les + ord + r + 1

      call psetQw(leq, ndm, ndm+1, QQ2)

      end
