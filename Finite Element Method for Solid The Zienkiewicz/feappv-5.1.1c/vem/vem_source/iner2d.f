!$Id:$
      subroutine iner2d(d,xl,ix,vl,al,s,r, nel,ndf,ndm,nst, isw)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Modification log                                Date (dd/mm/year)
!       Original version                                    23/08/2017
!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Compute inertial effects for 2-d elements
!               Includes effects of Rayleigh mass damping.

!      Inputs:
!         d(*)      - Material set parameters
!         xl(ndm,*) - Nodal coordinates for element
!         ix(*)     - Element nodal connections
!         vl(ndf,*) - Velocity for element
!         al(ndf,*) - Acceleration for element
!         ctan3     - Mass tangent factor
!         nel       - Number of element nodes
!         ndf       - Number dof/node
!         ndm       - Spatial dimension of mesh
!         nst       - Size of element arrays

!      Outputs:
!         s(nst,*)  - Consistent or interpolated mass
!         r(ndf,*)  - Element inertial force
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'eltran.h'   ! ctan(3)
      include  'pmod2d.h'   ! stype
      include  'qudshp.h'   ! shp2(3,*,*), jac(*), lint, sa(*), sg

      integer       :: nel,ndf,ndm,nst, isw, i, ii,jj, l
      real (kind=8) :: dv, dvm, aj1,aj2,aj3,lfac,cfac, xx
      integer       :: ix(*)
      real (kind=8) :: d(*),xl(ndm,nel),vl(ndf,nel),al(ndf,nel)
      real (kind=8) :: s(nst,nst),r(ndf,nel)
      real (kind=8) :: ac(3)

      save

!     Compute mass quadrature

      call quadr2d(d,.false.)

!     Set mass interpolation factor between consistent(1) & lumped(0)

      if(stype.eq.8) then
        cfac = 1.0d0    ! Axi with torsion has consistent only
      else
        cfac = d(7)
      endif
      lfac = 1.d0 - cfac
      dvm  = ctan(3) + d(77)*ctan(2)

!     Do quadrature

      do l = 1,lint

!       Compute shape functions

        call interp2d(l, xl,ix, ndm,nel,.true.)
        dv = jac(l)*d(4)

!       Multiply jacobian by radius for axisymmetry

        if(stype.eq.3 .or. stype.eq.8) then
          xx = 0.0d0
          do i = 1,nel
            xx = xx + shp2(3,i,l)*xl(1,i)
          end do ! i
          dv = dv*xx
        end if

!       Compute acceleration

        do i = 1,2
          ac(i) = 0.0d0
          do ii = 1,nel
            ac(i) = ac(i) + shp2(3,ii,l)*(al(i,ii) + d(77)*vl(i,ii))
          end do ! ii
          ac(i) = ac(i)*cfac
        end do ! i

!       Compute mass

        do jj = 1,nel

!         Compute db = rho*shape*dv

          aj1 = shp2(3,jj,l)*dv
          aj2 = aj1*lfac

!         Inertial residual with Rayleigh mass damping

          do i = 1,2
            r(i,jj) = r(i,jj) - ac(i)*aj1
     &                        - (al(i,jj) + d(77)*vl(i,jj))*aj2
          end do ! i

!         Compute inertial tangent

          if(isw.eq.3) then
            aj1 = aj1*dvm
            aj2 = cfac*aj1
            aj1 = lfac*aj1
            do i = 1,2
              s(sa(jj)+i,sa(jj)+i) = s(sa(jj)+i,sa(jj)+i) + aj1
            end do ! j
            do ii = 1,nel
              aj3 = shp2(3,ii,l)*aj2
              do i = 1,2
                s(sa(ii)+i,sa(jj)+i) = s(sa(ii)+i,sa(jj)+i) + aj3
              end do ! i
              if(stype.eq.8) then
                s(sa(ii)+3,sa(jj)+3) = s(sa(ii)+3,sa(jj)+3) + aj3
              endif
            end do ! ii
          endif
        end do ! jj

      end do ! l

      end subroutine iner2d
