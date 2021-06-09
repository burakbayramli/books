!$Id:$
      subroutine iner3d(d,xl,vl,al,s,r, nel,ndf,ndm,nst)

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Compute inertial effects for 3-d elements
!               Includes effects of Rayleigh mass damping.

!      Inputs:
!         d(*)      - Material set parameters
!         xl(ndm,*) - Nodal coordinates for element
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

      logical       :: tetfl
      integer       :: nel,ndf,ndm,nst, i,ii,i1, jj,j1, l,lint
      real (kind=8) :: xsj,dv,dvm, aj1,aj2,lfac,cfac
      real (kind=8) :: d(*),xl(ndm,nel),vl(ndf,nel),al(ndf,nel)
      real (kind=8) :: s(nst,nst),r(ndf,nel)
      real (kind=8) :: shp(4,8),sg(4,9),sv(5,16), cmass(8,8)

      save

!     Compute mass quadrature

      if(nel.eq.4) then
        tetfl = .true.
        l     =  2
        call tint3d (l,lint,sv)
      else
        tetfl = .false.
        l     = nint(d(5))
        call int3d(l,lint,sg)
      endif

!     Set mass interpolation factor between consistent (1) and lumped (0)

      cfac = d(7)
      lfac = 1.d0 - cfac
      dvm  = ctan(3) + d(77)*ctan(2)

!     Initialize mass

      do jj = 1,nel
        do ii = 1,nel
          cmass(ii,jj) = 0.0d0
        end do ! ii
      end do ! jj

      do l = 1,lint

!       Compute shape functions

        if(tetfl) then
          call tetshp(sv(1,l),xl,ndm,xsj,shp)
          dv = sv(5,l)*xsj*d(4)
        else
          call shp3d(sg(1,l),xsj,shp,xl,ndm)
          dv = sg(4,l)*xsj*d(4)
        endif

!       Compute mass

        do jj = 1,nel

!         Compute db = rho*shape*dv

          aj1 = shp(4,jj)*dv
          aj2 = cfac*aj1
          aj1 = lfac*aj1
          cmass(jj,jj) = cmass(jj,jj) + aj1
          do ii = 1,nel
            cmass(ii,jj) = cmass(ii,jj) + shp(4,ii)*aj2
          end do ! ii
        end do ! jj

      end do ! l

!     Compute inertial effect

      do ii = 1,nel
        do jj = 1,nel
          do i = 1,3
            r(i,ii) = r(i,ii) - (al(i,jj) + d(77)*vl(i,jj))*cmass(ii,jj)
          end do ! i
        end do ! jj
      end do ! ii

!     Expand mass into element array

      j1 = 0
      do jj = 1,nel
        i1 = 0
        do ii = 1,nel
          cmass(ii,jj) = cmass(ii,jj)*dvm
          do i = 1,ndm
            s(i1+i,j1+i) = s(i1+i,j1+i) + cmass(ii,jj)
          end do ! i
          i1 = i1 + ndf
        end do ! ii
        j1 = j1 + ndf
      end do ! jj

      end subroutine iner3d
