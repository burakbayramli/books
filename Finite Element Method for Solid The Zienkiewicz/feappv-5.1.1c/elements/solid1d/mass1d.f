!$Id:$
      subroutine mass1d(d,xl,s,p,ndf,ndm,nst)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Compute mass matrix for plane and axisymmetric problems

!      Inputs:
!         d(*)      - Material set parameters
!         xl(ndm,*) - Nodal coordinates for element
!         ndf       - Number dof/node
!         ndm       - Spatial dimension of mesh
!         nst       - Size of element arrays

!      Outputs:
!         s(nst,*)  - Consistent or interpolated mass
!         p(nst)    - Diagonal (lumped) mass
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'eldata.h'
      include  'pmod2d.h'
      include  'qudshp.h'

      integer       :: ndf,ndm,nst, ii,i1, jj,j1, l
      real (kind=8) :: d(*),xl(ndm,*),s(nst,nst),p(nst)
      real (kind=8) :: dv, aj1,xx,lfac,cfac

      save

!     Set quadrature order

      call quadr1d(d)

!     Set mass factors

      cfac = d(7)
      lfac = 1.d0 - cfac

      do l = 1,lint

!       Compute shape functions

        call interp1d(l, xl, ndm,nel,.false.)
        dv = abs(jac(l))*d(4)
        if(stype.eq.3) then
          xx = 0.0d0
          do jj = 1,nel
            xx = xx + shp1(2,jj,l)*xl(1,jj)
          end do ! jj
          dv = dv*xx
        end if

!       Compute db = rho*shape*dv

        j1 = 1
        do jj = 1,nel

!         Compute lumped mass matrices

          aj1      = shp1(2,jj,l)*dv
          p(j1)    = p(j1)    + aj1
          s(j1,j1) = s(j1,j1) + aj1*lfac

!         Compute consistent mass matrix

          aj1 = aj1*cfac
          i1  = 1
          do ii = 1,nel
            s(i1,j1) = s(i1,j1) + shp1(2,ii,l)*aj1
            i1       = i1 + ndf
          end do ! ii
          j1 = j1 + ndf
        end do ! jj
      end do ! l

      end subroutine mass1d
