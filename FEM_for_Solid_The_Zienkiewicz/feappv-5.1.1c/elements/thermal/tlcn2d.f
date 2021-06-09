!$Id:$
      subroutine tlcn2d(flux,p,s,nel)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Project element thermal variables to nodes

!      Inputs:
!        flux(2,*) - Flux values
!        nel       - Number nodes on element

!      Outputs:
!        p(nen)    - Weights for 'lumped' projection
!        s(nen,*)  - Integral of variables
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'cdata.h'
      include  'qudshp.h'
      include  'prstrs.h'
      include  'strnum.h'

      integer       :: nel, i,l
      real (kind=8) :: flux(2,*),p(*),s(nen,*), xg

      save

      do l = 1,lint
        do i = 1,nel
          xg    = shp2(3,i,l)*jac(l)
          p(i) = p(i) + xg

!         Stress projections

          s(i,1) = s(i,1) + flux(1,l)*xg
          s(i,2) = s(i,2) + flux(2,l)*xg
        end do ! i
      end do ! l

      iste = 2

      end subroutine tlcn2d
