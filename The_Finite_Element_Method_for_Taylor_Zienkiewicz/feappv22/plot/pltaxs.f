c$Id:$
      subroutine pltaxs(xi,ndm,ct)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Draw vectors on screen for direction of coord. axes

c      Inputs:
c         xi(*)     - Location on origin for axes
c         ndm       - Spatial dimension of mesh
c         ct        - Size factor for plot

c      Outputs:
c         none      - Plot outputs to screen/file
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      integer   ndm,m,n
      real*8    ct,fac1,fac2,fac3
      real*8    dd(3,3),xx(3,5),xi(ndm)

      save

c     Perspective projecton of axes

      call pzero(dd,9)
      do m = 1,ndm
        dd(m,m) = ct
      end do

c     Compute plot location for axes

      do m = 1,ndm
        call pzero(xx,15)
        do n = 1,ndm
          fac1 = dd(n,m)
          xx(n,1) = xi(n)
          xx(n,2) = xx(n,1) + fac1
          xx(n,5) = xx(n,2)
        end do
        fac1 = dd(1,m)*0.1d0
        fac2 = dd(2,m)*0.1d0
        fac3 = dd(3,m)*0.1d0
        xx(1,3) = xx(1,2) - 3.d0*fac1 -  fac2 - fac3
        xx(2,3) = xx(2,2) - 3.d0*fac2 +  fac1 + fac3
        xx(3,3) = xx(3,2) - 3.d0*fac3 +  fac1 + fac2
        xx(1,4) = xx(1,2) - 3.d0*fac1 +  fac2 + fac3
        xx(2,4) = xx(2,2) - 3.d0*fac2 -  fac1 - fac3
        xx(3,4) = xx(3,2) - 3.d0*fac3 -  fac1 - fac2

c       Plot vector

        call plotl(xx(1,1),xx(2,1),xx(3,1),3)
        do n = 2,5
          call plotl(xx(1,n),xx(2,n),xx(3,n),2)
        end do
        call plotl(xx(1,2),xx(2,2),xx(3,2),3)

c       Add label

        call plabl(m)

      end do

      end
