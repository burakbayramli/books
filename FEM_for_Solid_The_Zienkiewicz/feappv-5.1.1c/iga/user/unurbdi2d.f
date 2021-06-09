!$Id:$
      subroutine unurbdi2d(shp,xx,dxdxi,jac,ndisp, nel,ndf, fl)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:  2-d  user displacements for NURBS

!      Inputs:
!        shp(4,nel)  - Shape functions and natural derivatives
!                      1 = derv 1; 2 = derv 2; 3 = derv 3; 4 = N_a
!        xx(ndm)     - Coordinates at quadrature point
!        dxdxi(3,3)  - Natural derivatives * jacobian * quad wt.
!        jac         - Jacobian * quad wt
!        ndisp(9)    - Load parameters 1 to 7; 9 = model number
!        nel         - Number of control points on boundary
!        ndf         - Dof's per control point

!      Outputs:
!        fl(ndf,nel) - displacements for control points
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'iofile.h'

      logical    psflag
      integer    nel,ndf, j

      real*8     jac, ux,uy
      real*8     shp(4,nel),xx(2),dxdxi(3,3), ndisp(9), fl(ndf,nel)

!     Set flag true for plane strain

!     Plane strain
!     psflag = .true.
!     Plane stress
      psflag = .false.

      if(nint(ndisp(9)).eq.1) then
        call ucircle (xx,ndisp, ux,uy, psflag)
      elseif(nint(ndisp(9)).eq.2) then
        call uellipse(xx,ndisp, ux,uy, psflag)
      else
        write(*,*) ' Incorrect user model for NDISp'
        call plstop(.true.)
      endif

!     Add to load vector

      do j = 1,nel
        fl(1,j) = fl(1,j) + ux*shp(4,j)*jac
        fl(2,j) = fl(2,j) + uy*shp(4,j)*jac
      end do ! j

      end
