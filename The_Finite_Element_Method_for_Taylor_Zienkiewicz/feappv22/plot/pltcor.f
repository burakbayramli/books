c$Id:$
      subroutine pltcor(nel,ic,v,vc,nc)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Compute number of contour at element corners for
c               use by contour plot routines

c      Inputs:
c         nel       - Number of nodes on element
c         v(*)      - Contour value at node
c         vc(*)     - Contour values to plot
c         nc        - Number of contours plotted

c      Outputs:
c         ic(*)     - Contour number at node
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      integer   nel,nc, i,n
      integer   ic(nel)
      real*8    v(nel),vc(nc)

      save

      do i = 1,nel
         ic(i) = 1
      end do
      do n = 1,nc
        do i = 1,nel
          if(v(i).ge.vc(n)) then
            ic(i) = n + 1
          endif
        end do
      end do

      end
