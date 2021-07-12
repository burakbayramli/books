c$Id:$
      subroutine shp3d(ss,xsj,shp,xl,ndm)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Compute 3-d isoparametric 8-node element shape
c               functions and their derivatives w/r x,y,z

c      Inputs:
c         ss(3)     - Natural coordinates of point
c         xl(ndm,*) - Nodal coordinates for element
c         ndm       - Spatial dimension of mesh

c      Outputs:
c         xsj       - Jacobian determinant at point
c         shp(4,*)  - Shape functions and derivatives at point
c                     shp(1,i) = dN_i/dx
c                     shp(2,i) = dN_i/dy
c                     shp(3,i) = dN_i/dz
c                     shp(4,i) =  N_i
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      integer   ndm , i , j , k
      real*8    rxsj,xsj, ap1,am1,ap2,am2,ap3,am3, c1,c2,c3

      real*8    ss(3),shp(4,8),xl(ndm,8),xs(3,3),ad(3,3)

      save

c     Compute shape functions and their natural coord. derivatives

      ap1 = 1.0d0 + ss(1)
      am1 = 1.0d0 - ss(1)
      ap2 = 1.0d0 + ss(2)
      am2 = 1.0d0 - ss(2)
      ap3 = 1.0d0 + ss(3)
      am3 = 1.0d0 - ss(3)

c     Compute for ( - , - ) values

      c1      = 0.125d0*am1*am2
      c2      = 0.125d0*am2*am3
      c3      = 0.125d0*am1*am3
      shp(1,1) = -c2
      shp(1,2) =  c2
      shp(2,1) = -c3
      shp(2,4) =  c3
      shp(3,1) = -c1
      shp(3,5) =  c1
      shp(4,1) =  c1*am3
      shp(4,5) =  c1*ap3

c     Compute for ( + , + ) values

      c1      = 0.125d0*ap1*ap2
      c2      = 0.125d0*ap2*ap3
      c3      = 0.125d0*ap1*ap3
      shp(1,8) = -c2
      shp(1,7) =  c2
      shp(2,6) = -c3
      shp(2,7) =  c3
      shp(3,3) = -c1
      shp(3,7) =  c1
      shp(4,3) =  c1*am3
      shp(4,7) =  c1*ap3

c     Compute for ( - , + ) values

      c1      = 0.125d0*am1*ap2
      c2      = 0.125d0*am2*ap3
      c3      = 0.125d0*am1*ap3
      shp(1,5) = -c2
      shp(1,6) =  c2
      shp(2,5) = -c3
      shp(2,8) =  c3
      shp(3,4) = -c1
      shp(3,8) =  c1
      shp(4,4) =  c1*am3
      shp(4,8) =  c1*ap3

c     Compute for ( + , - ) values

      c1      = 0.125d0*ap1*am2
      c2      = 0.125d0*ap2*am3
      c3      = 0.125d0*ap1*am3
      shp(1,4) = -c2
      shp(1,3) =  c2
      shp(2,2) = -c3
      shp(2,3) =  c3
      shp(3,2) = -c1
      shp(3,6) =  c1
      shp(4,2) =  c1*am3
      shp(4,6) =  c1*ap3
      if(ndm.lt.3) return

c     Compute jacobian transformation

      do j = 1,3
        xs(j,1) = (xl(j,2) - xl(j,1))*shp(1,2)
     &          + (xl(j,3) - xl(j,4))*shp(1,3)
     &          + (xl(j,6) - xl(j,5))*shp(1,6)
     &          + (xl(j,7) - xl(j,8))*shp(1,7)

        xs(j,2) = (xl(j,3) - xl(j,2))*shp(2,3)
     &          + (xl(j,4) - xl(j,1))*shp(2,4)
     &          + (xl(j,7) - xl(j,6))*shp(2,7)
     &          + (xl(j,8) - xl(j,5))*shp(2,8)

        xs(j,3) = (xl(j,5) - xl(j,1))*shp(3,5)
     &          + (xl(j,6) - xl(j,2))*shp(3,6)
     &          + (xl(j,7) - xl(j,3))*shp(3,7)
     &          + (xl(j,8) - xl(j,4))*shp(3,8)
      end do

c     Compute adjoint to jacobian

      ad(1,1) = xs(2,2)*xs(3,3) - xs(2,3)*xs(3,2)
      ad(1,2) = xs(3,2)*xs(1,3) - xs(3,3)*xs(1,2)
      ad(1,3) = xs(1,2)*xs(2,3) - xs(1,3)*xs(2,2)

      ad(2,1) = xs(2,3)*xs(3,1) - xs(2,1)*xs(3,3)
      ad(2,2) = xs(3,3)*xs(1,1) - xs(3,1)*xs(1,3)
      ad(2,3) = xs(1,3)*xs(2,1) - xs(1,1)*xs(2,3)

      ad(3,1) = xs(2,1)*xs(3,2) - xs(2,2)*xs(3,1)
      ad(3,2) = xs(3,1)*xs(1,2) - xs(3,2)*xs(1,1)
      ad(3,3) = xs(1,1)*xs(2,2) - xs(1,2)*xs(2,1)

c     Compute determinant of jacobian

      xsj  = xs(1,1)*ad(1,1) + xs(1,2)*ad(2,1) + xs(1,3)*ad(3,1)
      rxsj = 1.d0/xsj

c     Compute jacobian inverse

      do j = 1,3
        do i = 1,3
          xs(i,j) = ad(i,j)*rxsj
        end do
      end do

c     Compute derivatives with repect to global coords.

      do k = 1,8

        c1 = shp(1,k)*xs(1,1) + shp(2,k)*xs(2,1) + shp(3,k)*xs(3,1)
        c2 = shp(1,k)*xs(1,2) + shp(2,k)*xs(2,2) + shp(3,k)*xs(3,2)
        c3 = shp(1,k)*xs(1,3) + shp(2,k)*xs(2,3) + shp(3,k)*xs(3,3)

        shp(1,k) = c1
        shp(2,k) = c2
        shp(3,k) = c3

      end do

      end
