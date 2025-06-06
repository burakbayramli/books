c$Id:$
      subroutine tetshp( xi, xl, ndm, xsj, shp )

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Compute 3-d tetrahedral element shape
c               functions and their derivatives w/r x,y,z

c      Inputs:
c         xi(4)     - Natural volume coordinates of point
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

      include  'iofile.h'

      integer   ndm
      integer   i, j, k, l

      real*8    xsj, detr
      real*8    xi(4), xl(ndm,*), shp(4,*)
      real*8    a(4,3), xip(4)

c     Compute determinants for transformation minors

      do i = 1,4
        j      = mod(i,4) + 1
        k      = mod(j,4) + 1
        l      = mod(k,4) + 1
        a(i,1) = xl(2,j)*(xl(3,k) - xl(3,l))
     &         + xl(2,k)*(xl(3,l) - xl(3,j))
     &         + xl(2,l)*(xl(3,j) - xl(3,k))

        a(i,2) = xl(3,j)*(xl(1,k) - xl(1,l))
     &         + xl(3,k)*(xl(1,l) - xl(1,j))
     &         + xl(3,l)*(xl(1,j) - xl(1,k))

        a(i,3) = xl(1,j)*(xl(2,k) - xl(2,l))
     &         + xl(1,k)*(xl(2,l) - xl(2,j))
     &         + xl(1,l)*(xl(2,j) - xl(2,k))
        xip(i) = 256.d0*xi(j)*xi(k)*xi(l)
      end do ! i

c     Correct signs on determinants

      do i = 1,3
        a(1,i) = -a(1,i)
        a(3,i) = -a(3,i)
      end do ! i

c     Determinant for element volume

      xsj  = (xl(1,1)*a(1,1) + xl(1,2)*a(2,1)
     &      + xl(1,3)*a(3,1) + xl(1,4)*a(4,1))

      if(xsj.ne.0.0d0) then
        detr = 1.d0/xsj
      else
        write(iow,*) ' TETSHP: Determinant =',xsj
        detr = 1.d0
      endif

c     Linear and bubble mode shape functions

      shp(1,5) = 0.0d0
      shp(2,5) = 0.0d0
      shp(3,5) = 0.0d0
      shp(4,5) = 256.d0*xi(1)*xi(2)*xi(3)*xi(4)
      do i = 1,4
        shp(1,i) = a(i,1)*detr
        shp(2,i) = a(i,2)*detr
        shp(3,i) = a(i,3)*detr
        shp(4,i) = xi(i)

        shp(1,5) = shp(1,5) + shp(1,i)*xip(i)
        shp(2,5) = shp(2,5) + shp(2,i)*xip(i)
        shp(3,5) = shp(3,5) + shp(3,i)*xip(i)

      end do ! i

      end
