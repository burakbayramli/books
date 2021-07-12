c$Id:$
      subroutine trishp(el,xl,ndm,iord, xsj,shp)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Triangular shape function routine

c       Type:  |iord| = 1:  Linear  three-node
c              |iord| = 2:  Quadratic six-node
c              |iord| = 3:  Quadratic seven-node
c              |iord| = 4:  Quadratic + 3 bubbles (Zienkiewicz/Lefebre)

c               iord  > 0:  Mid-side and center node are  global  coords.
c               iord  < 0:  Mid-side and center node heirarchical coords.

c      Inputs:
c         el(3)     - Area coordinates for point
c         xl(ndm,*) - Nodal coordinates for element
c         ndm       - Spatial dimension of mesh
c         iord      - Order of shape functions (see above)

c      Outputs:
c         xsj       - Jacobian determinant at point
c         shp(3,*)  - Shape functions and derivatives at point
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      integer   ndm,iord, i
      real*8    xsj,xsjr, fel1,fel2,fel3, fel12,fel23,fel31
      real*8    one3,four9
      real*8    x1,x2,x3,x4,x5,x6,x7, y1,y2,y3,y4,y5,y6,y7
      real*8    el(3),xl(ndm,*), shp(3,*)

      save

      data one3 /0.3333333333333333d0/, four9 /0.4444444444444444d0/

c     Form Jacobian terms

      x1 = xl(1,1)
      x2 = xl(1,2)
      x3 = xl(1,3)

      y1 = xl(2,1)
      y2 = xl(2,2)
      y3 = xl(2,3)

      if(abs(iord).gt.1) then

        fel1 = 4.d0*el(1)
        fel2 = 4.d0*el(2)
        fel3 = 4.d0*el(3)

        x4   = xl(1,4)
        x5   = xl(1,5)
        x6   = xl(1,6)

        y4   = xl(2,4)
        y5   = xl(2,5)
        y6   = xl(2,6)

c       Form shape functions in total coordinates

        if(iord.gt.0) then

          x4 = x4 - 0.5d0*(x1 + x2)
          x5 = x5 - 0.5d0*(x2 + x3)
          x6 = x6 - 0.5d0*(x3 + x1)
          x7 = one3*(x1 + x2 + x3) + four9*(x4 + x5 + x6)

          y4 = y4 - 0.5d0*(y1 + y2)
          y5 = y5 - 0.5d0*(y2 + y3)
          y6 = y6 - 0.5d0*(y3 + y1)
          y7 = one3*(y1 + y2 + y3) + four9*(y4 + y5 + y6)

        endif

        x1   = x1 + x4*fel2 + x6*fel3
        x2   = x2 + x5*fel3 + x4*fel1
        x3   = x3 + x6*fel1 + x5*fel2

        y1   = y1 + y4*fel2 + y6*fel3
        y2   = y2 + y5*fel3 + y4*fel1
        y3   = y3 + y6*fel1 + y5*fel2

        fel12 = el(1)*el(2)
        fel23 = el(2)*el(3)
        fel31 = el(3)*el(1)

        if(iord.eq.3) then

          fel12 = 27.d0*fel12
          fel23 = 27.d0*fel23
          fel31 = 27.d0*fel31

          x7    = xl(1,7) - x7
          x1    = x1 + x7*fel23
          x2    = x2 + x7*fel31
          x3    = x3 + x7*fel12

          y7    = xl(2,7) - y7
          y1    = y1 + y7*fel23
          y2    = y2 + y7*fel31
          y3    = y3 + y7*fel12

        endif
      endif

      xsj  = x1*(y2-y3) + x2*(y3-y1) + x3*(y1-y2)
      xsjr = 1.0d0
      if(xsj.ne.0.0d0) xsjr = 1.0d0/xsj
      xsj  = 0.5d0*xsj

c     Specify shape functions and their derivatives

      shp(1,1) = (y2-y3)*xsjr
      shp(2,1) = (x3-x2)*xsjr
      shp(3,1) = el(1)

      shp(1,2) = (y3-y1)*xsjr
      shp(2,2) = (x1-x3)*xsjr
      shp(3,2) = el(2)

      shp(1,3) = (y1-y2)*xsjr
      shp(2,3) = (x2-x1)*xsjr
      shp(3,3) = el(3)

c     Add quadratic hierarchical functions

      if(abs(iord).gt.1) then

        shp(1,4) = shp(1,1)*fel2 + shp(1,2)*fel1
        shp(2,4) = shp(2,1)*fel2 + shp(2,2)*fel1
        shp(3,4) = el(1)*fel2

        shp(1,5) = shp(1,2)*fel3 + shp(1,3)*fel2
        shp(2,5) = shp(2,2)*fel3 + shp(2,3)*fel2
        shp(3,5) = el(2)*fel3

        shp(1,6) = shp(1,3)*fel1 + shp(1,1)*fel3
        shp(2,6) = shp(2,3)*fel1 + shp(2,1)*fel3
        shp(3,6) = el(3)*fel1

c       Bubble at baricenter

        if(abs(iord).eq.3) then
          shp(1,7) = shp(1,1)*fel23 + shp(1,2)*fel31 + shp(1,3)*fel12
          shp(2,7) = shp(2,1)*fel23 + shp(2,2)*fel31 + shp(2,3)*fel12
          shp(3,7) = fel12*el(3)
        elseif(abs(iord).eq.4) then
          shp(1,7) = (shp(1,1)*fel23*2.d0
     &              + shp(1,2)*fel31
     &              + shp(1,3)*fel12)*el(1)
          shp(2,7) = (shp(2,1)*fel23*2.d0
     &              + shp(2,2)*fel31
     &              + shp(2,3)*fel12)*el(1)
          shp(3,7) = fel12*fel31

          shp(1,8) = (shp(1,1)*fel23
     &              + shp(1,2)*fel31*2.d0
     &              + shp(1,3)*fel12)*el(2)
          shp(2,8) = (shp(2,1)*fel23
     &              + shp(2,2)*fel31*2.d0
     &              + shp(2,3)*fel12)*el(2)
          shp(3,8) = fel12*fel23

          shp(1,9) = (shp(1,1)*fel23
     &              + shp(1,2)*fel31
     &              + shp(1,3)*fel12*2.d0)*el(3)
          shp(2,9) = (shp(2,1)*fel23
     &              + shp(2,2)*fel31
     &              + shp(2,3)*fel12*2.d0)*el(3)
          shp(3,9) = fel31*fel23
        endif

c       Modify shape functions for mid-side and interior values

        if(iord.gt.1) then

c         Modify vertex and mid-side values for bubble

          if(iord.eq.3) then
            do i = 1,3
              shp(i,1) = shp(i,1) -  one3*shp(i,7)
              shp(i,2) = shp(i,2) -  one3*shp(i,7)
              shp(i,3) = shp(i,3) -  one3*shp(i,7)
              shp(i,4) = shp(i,4) - four9*shp(i,7)
              shp(i,5) = shp(i,5) - four9*shp(i,7)
              shp(i,6) = shp(i,6) - four9*shp(i,7)
            end do
          endif

c         Modify vertex shape functions for mid-side values

          do i = 1,3
            shp(i,1) = shp(i,1) - 0.5d0*(shp(i,4) + shp(i,6))
            shp(i,2) = shp(i,2) - 0.5d0*(shp(i,5) + shp(i,4))
            shp(i,3) = shp(i,3) - 0.5d0*(shp(i,6) + shp(i,5))
          end do

        endif
      endif

      end
