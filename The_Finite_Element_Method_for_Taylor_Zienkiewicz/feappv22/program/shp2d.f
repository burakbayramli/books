c$Id:$
      subroutine shp2d(ss,xl,shp,xsj,ndm,nel,ix,flg)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Computes shape function and derivatives for
c               quadrilateral elements

c      Inputs:
c         ss(2)     - Natural coordinates for point
c         xl(ndm,*) - Nodal coordinates for element
c         ndm       - Spatial dimension of mesh
c         nel       - Number of nodes on element
c         ix(*)     - Nodes attached to element
c         flg       - Flag, compute global x/y derivatives if false,
c                           else derivatives are w/r natural coords.

c      Outputs:
c         shp(3,*)  - Shape functions and derivatives at point
c                     shp(1,i) = dN_i/dx or dN_i/dxi_1
c                     shp(2,i) = dN_i/dy or dN_i/dxi_2
c                     shp(3,i) = N_i
c         xsj       - Jacobian determinant at point
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      logical   flg
      integer   ndm,nel, i,j,k
      real*8    xsj, temp

      integer   ix(*)
      real*8    shp(3,*),xl(ndm,*), s(4),t(4),xs(3,2),sx(2,2),ss(2)

      save

c     Set values of half natural coords at nodes

      data s/-0.5d0,0.5d0,0.5d0,-0.5d0/,t/-0.5d0,-0.5d0,0.5d0,0.5d0/

c     Form 4-node quadrilateral shape functions

      if(nel.eq.4 .and. .not.flg) then
        call shapef(ss(1),ss(2),xl,shp,xsj,ndm,flg)
      else
        do i = 1,4
          shp(3,i) = (0.5d0+s(i)*ss(1))*(0.5d0+t(i)*ss(2))
          shp(1,i) = s(i)*(0.5d0+t(i)*ss(2))
          shp(2,i) = t(i)*(0.5d0+s(i)*ss(1))
        end do

c       Form triangle by adding third and fourth together

        if(nel.eq.3) then
          do i = 1,3
            shp(i,3) = shp(i,3)+shp(i,4)
          end do
        end if

c       Add quadratic terms if necessary

        if(nel.gt.4) call shap2(ss(1),ss(2),shp,ix,nel)

c       Construct jacobian and its inverse

        do i = 1,max(3,ndm)
          do j = 1,2
            xs(i,j) = 0.0d0
            do k = 1,nel
              xs(i,j) = xs(i,j) + xl(i,k)*shp(j,k)
            end do
          end do
        end do
        if(ndm.eq.2) then
          xsj = xs(1,1)*xs(2,2)-xs(1,2)*xs(2,1)
        elseif(ndm.eq.3) then
          xsj = sqrt((xs(2,1)*xs(3,2)-xs(2,2)*xs(3,1))**2
     &             + (xs(3,1)*xs(1,2)-xs(3,2)*xs(1,1))**2
     &             + (xs(1,1)*xs(2,2)-xs(1,2)*xs(2,1))**2)
        endif
        if(.not.flg) then
          if(xsj.eq.0.0d0) then
            temp = 1.0d0
          else
            temp = 1.d0/xsj
          endif
          sx(1,1) = xs(2,2)*temp
          sx(2,2) = xs(1,1)*temp
          sx(1,2) =-xs(1,2)*temp
          sx(2,1) =-xs(2,1)*temp

c         Form global derivatives

          do i = 1,nel
            temp     = shp(1,i)*sx(1,1)+shp(2,i)*sx(2,1)
            shp(2,i) = shp(1,i)*sx(1,2)+shp(2,i)*sx(2,2)
            shp(1,i) = temp
          end do

c         Return center node in hierarchical form for 8-nodes

          if(nel.eq.8) then
            temp     = shp(1,9)*sx(1,1)+shp(2,9)*sx(2,1)
            shp(2,9) = shp(1,9)*sx(1,2)+shp(2,9)*sx(2,2)
            shp(1,9) = temp
          endif
        endif
      endif

      end
