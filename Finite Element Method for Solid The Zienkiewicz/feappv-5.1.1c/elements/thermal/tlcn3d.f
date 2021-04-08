!$Id:$
      subroutine tlcn3d(flux,p,s,se,nel)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'cdata.h'
      include  'prstrs.h'
      include  'strnum.h'
      include  'qudshp.h'

      integer       :: j,l,nel
      real (kind=8) :: xg

      real (kind=8) :: flux(3,*),p(*),s(nen,*),se(*)

      save

      do l = 1,lint

!       Compute lumped projection and assemble stress integrals

        do j = 1,nel
          xg     = jac(l)*shp3(4,j,l)
          p(j)   = p(j) + xg
          s(j,7) = s(j,7) + flux(1,l)*xg
          s(j,8) = s(j,8) + flux(2,l)*xg
          s(j,9) = s(j,9) + flux(3,l)*xg
          se(j)  = se(j)  + erav*xg
        end do ! j
      end do ! l

      iste = 9

      end
