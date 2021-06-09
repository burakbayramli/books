!$Id:$
      subroutine strn2n(shp,xl,ul,theta,irad,ndm,ndf,nel,nen,eps)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Compute mixed strain of near incompressible formulation

!      Inputs:
!        shp(3,nen,*)  = Shape functions
!        xl(ndm,nen)   = Nodal coordinates
!        ul(ndf,nen,*) = Nodal solution parameters
!        theta(2)      = Volume change (mixed form)
!        irad          = Inverse radius (or zero for plane).
!        ndm           = Spatial dimension of mesh
!        ndf           = DOF/node (max)
!        nel           = Number nodes on element (4 or 9)
!        nen           = Max nodes/element (dimension uses only)

!      Outputs:
!        eps(9,3)      = Mixed strain at point
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'elcoor.h'
      include  'pmod2d.h'

      integer   ndm,      ndf,         nel,         nen,     k
      real*8    irad,     theta(2),    dtheta
      real*8    shp(3,*), xl(ndm,nen), ul(ndf,nen,*), eps(9,*)

!     Compute strain tensor for constitutive equations

      do k = 1,6
        eps(k,1) = 0.0d0
        eps(k,2) = 0.0d0
      end do ! k
      do k = 1,3
        xref(k) = 0.0d0
        xcur(k) = 0.0d0
      end do ! k

      do k = 1,nel
        eps(1,1)  = eps(1,1)  + shp(1,k)*ul(1,k,1)
        eps(2,1)  = eps(2,1)  + shp(2,k)*ul(2,k,1)
        eps(3,1)  = eps(3,1)  + shp(3,k)*ul(1,k,1)
        eps(4,1)  = eps(4,1)  + shp(1,k)*ul(2,k,1)
     &                        + shp(2,k)*ul(1,k,1)
        eps(1,2)  = eps(1,2)  + shp(1,k)*ul(1,k,2)
        eps(2,2)  = eps(2,2)  + shp(2,k)*ul(2,k,2)
        eps(3,2)  = eps(3,2)  + shp(3,k)*ul(1,k,2)
        eps(4,2)  = eps(4,2)  + shp(1,k)*ul(2,k,2)
     &                        + shp(2,k)*ul(1,k,2)
        xref(1)   = xref(1)   + shp(3,k)*xl(1,k)
        xref(2)   = xref(2)   + shp(3,k)*xl(2,k)
        xcur(1)   = xcur(1)   + shp(3,k)*ul(1,k,1)
        xcur(2)   = xcur(2)   + shp(3,k)*ul(2,k,1)
      end do ! k

!     Set current coords

      xcur(1) = xcur(1) + xref(1)
      xcur(2) = xcur(2) + xref(2)

!     Modification for plane/axisymmetry

      eps(3,1) = eps(3,1)*irad
      eps(3,2) = eps(3,2)*irad

!     Compute strains at t_n

      do k = 1,4
        eps(k,2) = eps(k,1) - eps(k,2)
      end do ! k

!     Correct strains and incremental strains for mixed formulation

      dtheta   = (theta(1) - eps(1,1) - eps(2,1) - eps(3,1))/3.0d0

      eps(1,1) = eps(1,1) + dtheta
      eps(2,1) = eps(2,1) + dtheta
      eps(3,1) = eps(3,1) + dtheta

      dtheta   = theta(1) - theta(2)
      dtheta   = (dtheta - eps(1,2) - eps(2,2) - eps(3,2))/3.0d0
      eps(1,2) = eps(1,2) + dtheta
      eps(2,2) = eps(2,2) + dtheta
      eps(3,2) = eps(3,2) + dtheta

      end
