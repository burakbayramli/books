!$Id:$
      subroutine interp3d(l, xl, ndm,nel)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: 3-D interpolation functions for solid elements

!      Inputs:
!        l            - Quadrature point
!        xl(ndm,*)    - Coordinates for element nodes
!        ndm          - Space dimension of coordinates
!        nel          - Number of nodes on element

!      Outputs:
!        shp(4,nel,l) - Shape functions and derivatives
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'qudshp.h'

      include   'pointer.h'
      include   'comblk.h'

      integer (kind=4) :: l, ndm,nel, ii
      real    (kind=8) :: xl(ndm,*)
      real    (kind=8) :: s1m,s1p, s2m,s2p, s3m,s3p


!     Check for NURBS types

      if(nurbfl) then

        call shp3d_nurb(sg3(1,l),xl,hr(np(264)),shp3(1,1,l),shpm(1,l),
     &                  jac(l), mr(np(308)),mr(np(311)),hr(np(298)),ndm)
        jac(l) = sg3(4,l)*jac(l)

!     Finite element quadrilateral interpolations

      elseif(quad) then
        call shp3d(sg3(1,l),jac(l),shp3(1,1,l),xl,ndm)
        jac(l) = sg3(4,l)*jac(l)

!       Form shpm for linear elements

        s1m       = (0.5d0 - 0.5d0*sg3(1,l))
        s2m       = (0.5d0 - 0.5d0*sg3(2,l))
        s3m       = (0.5d0 - 0.5d0*sg3(3,l))

        s1p       = (0.5d0 + 0.5d0*sg3(1,l))
        s2p       = (0.5d0 + 0.5d0*sg3(2,l))
        s3p       = (0.5d0 + 0.5d0*sg3(3,l))

        shpm(1,l) = s1m*s2m*s3m
        shpm(2,l) = s1p*s2m*s3m
        shpm(3,l) = s1p*s2p*s3m
        shpm(4,l) = s1m*s2p*s3m
        shpm(5,l) = s1m*s2m*s3p
        shpm(6,l) = s1p*s2m*s3p
        shpm(7,l) = s1p*s2p*s3p
        shpm(8,l) = s1m*s2p*s3p
        do ii = 9,nel
          shpm(ii,l) = 0.0d0
        end do ! ii

!     Finite element tetrahedral interpolations

      elseif(ttfl) then
        call tetshp(el3(1,l),xl,ndm,jac(l),shp3(1,1,l))
        jac(l) = el3(5,l)*jac(l)

!       Form shpm for linear elements

        shpm(1,l) = el3(1,l)
        shpm(2,l) = el3(2,l)
        shpm(3,l) = el3(3,l)
        shpm(4,l) = el3(4,l)
        do ii = 5,nel
          shpm(ii,l) = 0.0d0
        end do ! ii
      endif

      end subroutine interp3d
