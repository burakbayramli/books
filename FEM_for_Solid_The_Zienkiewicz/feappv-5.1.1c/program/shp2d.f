!$Id:$
      subroutine shp2d(ss,xl,shp,xsj,ndm,nel,ix,flg)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Computes shape function and derivatives for
!               quadrilateral elements

!      Inputs:
!         ss(2)     - Natural coordinates for point
!         xl(ndm,*) - Nodal coordinates for element
!         ndm       - Spatial dimension of mesh
!         nel       - Number of nodes on element
!         ix(*)     - Nodes attached to element
!         flg       - Flag, compute global x/y derivatives if false,
!                           else derivatives are w/r natural coords.

!      Outputs:
!         shp(3,*)  - Shape functions and derivatives at point
!                     shp(1,i) = dN_i/dx or dN_i/dxi_1
!                     shp(2,i) = dN_i/dy or dN_i/dxi_2
!                     shp(3,i) = N_i
!         xsj       - Jacobian determinant at point
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      logical       :: flg
      integer       :: ndm,nel, i,j,k
      real (kind=8) :: xsj, temp

      integer       :: ix(*)
      real (kind=8) :: shp(3,*),xl(ndm,*)
      real (kind=8) :: s(4),t(4),xs(3,2),sx(2,2),ss(2)

      save

!     Set values of half natural coords at nodes

      data s/-0.5d0,0.5d0,0.5d0,-0.5d0/,t/-0.5d0,-0.5d0,0.5d0,0.5d0/

!     Form 4-node quadrilateral shape functions

      if(nel.eq.4 .and. .not.flg) then
        call shapef(ss(1),ss(2),xl,shp,xsj,ndm,flg)
      else
        do i = 1,4
          shp(3,i) = (0.5d0+s(i)*ss(1))*(0.5d0+t(i)*ss(2))
          shp(1,i) = s(i)*(0.5d0+t(i)*ss(2))
          shp(2,i) = t(i)*(0.5d0+s(i)*ss(1))
        end do

!       Form triangle by adding third and fourth together

        if(nel.eq.3) then
          do i = 1,3
            shp(i,3) = shp(i,3)+shp(i,4)
          end do
        end if

!       Add quadratic terms if necessary

        if(nel.gt.4) call shap2(ss(1),ss(2),shp,ix,nel)

!       Construct jacobian and its inverse

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

!         Form global derivatives

          do i = 1,nel
            temp     = shp(1,i)*sx(1,1)+shp(2,i)*sx(2,1)
            shp(2,i) = shp(1,i)*sx(1,2)+shp(2,i)*sx(2,2)
            shp(1,i) = temp
          end do

!         Return center node in hierarchical form for 8-nodes

          if(nel.eq.8) then
            temp     = shp(1,9)*sx(1,1)+shp(2,9)*sx(2,1)
            shp(2,9) = shp(1,9)*sx(1,2)+shp(2,9)*sx(2,2)
            shp(1,9) = temp
          endif
        endif
      endif

      end subroutine shp2d
