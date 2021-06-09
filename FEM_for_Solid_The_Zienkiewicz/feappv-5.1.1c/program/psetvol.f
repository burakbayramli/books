!$Id:$
      subroutine psetvol(x,ndm,numnp)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Modification log                                Date (dd/mm/year)
!       Original version                                    01/05/2018
!-----[--+---------+---------+---------+---------+---------+---------+-]
!     Purpose: Compute volume of RVE

!     Inputs
!       x(ndm,numnp) - Nodal coordinates
!       nmd          - Problem space dimension
!       numnp        - Number of nodes on RVE

!     Output:          in common /elpers.h/
!       volm0        - Volume of RVE
!       xc(3)        - Side lengths coordinate
!       xc0(3)       - Mean coordinate
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit   none

      include   'debugs.h'
      include   'elpers.h'
      include   'iofile.h'
      include   'print.h'

      integer       :: ndm,numnp, n,i
      real (kind=8) :: x(ndm,numnp), xmn,xmx

      save

      volm0 = 1.0d0
      do i = 1,ndm
        xmn = x(i,1)
        xmx = x(i,1)
        do n = 2,numnp
          xmn = min(xmn,x(i,n))
          xmx = max(xmx,x(i,n))
        end do ! n
        volm0  = volm0*(xmx - xmn)
        xc(i)  = (xmx - xmn)*0.5d0
        xc0(i) = (xmx + xmn)*0.5d0
      end do ! i

!     Output values of RVE

      if(prnt) then
        write(iow,2000) volm0,(i,xc (i),i,xc0(i),i=1,ndm)
        if(debug) then
          write(*,2000) volm0,(i,xc (i),i,xc0(i),i=1,ndm)
        endif
      endif

2000  format(5x,'RVE Size Data'/
     &       10x,'Volume   =',1p,1e11.3/
     &      (10x,'Side h-',i1,' =',1p,1e11.3,
     &        ' Center x-',i1,' =',1p,1e11.3))

      end subroutine psetvol
