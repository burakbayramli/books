!$id:$
      subroutine  psetnurb(x,wt, Qw, l, leq, linc)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:  Move local NURB into global coordinates and weights

!      Inputs:
!         Qw(ndm+1,*) - NURB vector
!         l           - Initial NURB number less 1
!         leq         - Length of NURB vector
!         linc        - Increment to node

!      Outputs:
!         x(ndm,*)    - Global coordinates of NURBS
!         wt(*)       - Global weight of NURBS
!-----[--.----+----.----+----.-----------------------------------------]
      implicit    none

      include    'sdata.h'

      integer     l,leq,linc
      real*8      x(ndm,*),wt(*), Qw(ndm+1,*)

      integer     ls,j,n

      ls = l + 1
      do n = 1,leq
        do j = 1,ndm
          x(j,ls) = Qw(j,n)
        end do ! j
        wt(ls) = Qw(ndm+1,n)
        ls = ls + linc
      end do ! n

      end
