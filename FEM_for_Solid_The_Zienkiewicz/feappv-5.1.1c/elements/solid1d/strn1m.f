!$Id:$
      subroutine strn1m(shp,xl,ul,theta,irad,jrad,ndm,ndf,nel,nen,eps)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Compute mixed strain of near incompressible formulation

!      Inputs:
!        shp(2,*)      = Shape functions
!        xl(ndm,nen)   = Nodal coordinates
!        ul(ndf,nen,*) = Nodal solution parameters
!        theta         = Volume change (mixed form)
!        irad          = Inverse radius (or zero for plane).
!        jrad          = Inverse radius (or zero for plane).
!        ndm           = Spatial dimension of mesh
!        ndf           = DOF/node (max)
!        nel           = Number nodes on element
!        nen           = Max nodes/element (dimension uses only)

!      Outputs:
!        eps(*)        = Mixed strain at point
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'elcoor.h'
      include  'elpers.h'

      integer       :: ndm,      ndf,         nel,         nen,     k
      real (kind=8) :: irad,     jrad,        theta,       dtheta
      real (kind=8) :: shp(2,*), xl(ndm,nen), ul(ndf,nen), eps(*)

!     Compute strain tensor for constitutive equations

      eps(1:3)  = 0.0d0
      xref(1:3) = 0.0d0
      xcur(1:3) = 0.0d0

      do k = 1,nel
        eps(1)  = eps(1)  + shp(1,k)*ul(1,k)
        eps(3)  = eps(3)  + shp(2,k)*ul(1,k)
        xref(1) = xref(1) + shp(2,k)*xl(1,k)
        xcur(1) = xcur(1) + shp(2,k)*ul(1,k)
      end do ! k

!     Set current coords

      xcur(1) = xcur(1) + xref(1)

!     Modification for plane/axisymmetry

      eps(2) = eps(3)*jrad
      eps(3) = eps(3)*irad

!     Correct strains and incremental strains for mixed formulation

      dtheta = (theta - eps(1) - eps(2) - eps(3))/3.0d0
      eps(1) = eps(1) + dtheta
      eps(2) = eps(2) + dtheta
      eps(3) = eps(3) + dtheta

      end subroutine strn1m
