!$Id:$
      subroutine kine2d (shps,xl,ul,f,df,detf,ndm,ndf,nel,nen)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Compute kinematic quantities for finite deformations

!      Inputs:
!         shps(3,nel) - Reference configuration shape functions
!         xl(ndm,nel) - Nodal reference coordinates
!         ul(ndf,nel) - Nodal displacements
!         ndm         - Number mesh dimensions
!         ndf         - Number dof/node
!         nel         - Number nodes/element
!         nen         - Maximum number nodes/element

!      Outputs:
!         f(3,3,2)    - deformation gradient
!         df(3,3)     - incremental deformation gradient
!         detf(2)     - determinant of deformation gradient
!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'pmod2d.h'

      integer       :: ndm,ndf,nel,nen, i,j,k
      real (kind=8) :: detfi,temp,xx1

      real (kind=8) :: shps(3,*),xl(ndm,*),ul(ndf,nen,*)
      real (kind=8) :: df(3,3),f(3,3,*),fi(3,3),detf(*)

      save

!     Deformation gradient at t_n+1 : F_n+1 = I + GRAD u_n+1

      do i = 1,2
        do j = 1,2
          f(i,j,1)  = 0.0d0
          df(i,j) = 0.0d0
          do k = 1,nel
            f(i,j,1) = f(i,j,1) + ul(i,k,1)*shps(j,k)
            df(i,j ) = df(i,j ) + ul(i,k,2)*shps(j,k)
          end do ! k
        end do ! j
        f(i,i,1) = f(i,i,1) + 1.0d0
      end do ! i

!     Deformation gradient at t_n: F_n

      f(1,1,2)  = f(1,1,1) - df(1,1)
      f(2,1,2)  = f(2,1,1) - df(2,1)
      f(1,2,2)  = f(1,2,1) - df(1,2)
      f(2,2,2)  = f(2,2,1) - df(2,2)

      f(1,3,1)  = 0.0d0
      f(3,1,1)  = 0.0d0

      f(2,3,1)  = 0.0d0
      f(3,2,1)  = 0.0d0

      f(1,3,2)  = 0.0d0
      f(3,1,2)  = 0.0d0

      f(2,3,2)  = 0.0d0
      f(3,2,2)  = 0.0d0

      df(1,3)   = 0.0d0
      df(3,1)   = 0.0d0

      df(2,3)   = 0.0d0
      df(3,2)   = 0.0d0

      if(stype.eq.3) then
        f(3,3,1) = 0.0d0
        xx1      = 0.0d0
        df(3,3)  = 0.0d0
        do k = 1,nel
          xx1      = xx1      + xl(1,k  )*shps(3,k)
          f(3,3,1) = f(3,3,1) + ul(1,k,1)*shps(3,k)
          df(3,3)  = df(3,3)  + ul(1,k,2)*shps(3,k)
        end do
        f(3,3,1) = 1.d0 + f(3,3,1)/xx1
        df(3,3)  = df(3,3)/xx1
        f(3,3,2) = f(3,3,1) - df(3,3)
      else
        f(3,3,1) = 1.0d0
        f(3,3,2) = 1.0d0
        df(3,3)  = 0.0d0
      endif

!     Invert F

      detf(1) = f(1,1,1)*f(2,2,1) - f(1,2,1)*f(2,1,1)
      detf(2) = f(1,1,2)*f(2,2,2) - f(1,2,2)*f(2,1,2)

      detfi   =  1.d0/detf(1)
      fi(1,1) =  f(2,2,1)*detfi
      fi(1,2) = -f(1,2,1)*detfi
      fi(1,3) =  0.0d0
      fi(2,1) = -f(2,1,1)*detfi
      fi(2,2) =  f(1,1,1)*detfi
      fi(2,3) =  0.0d0
      fi(3,1) =  0.0d0
      fi(3,2) =  0.0d0
      fi(3,3) =  1.0d0/f(3,3,1)

!     Determinants

      detf(1) = detf(1)*f(3,3,1)
      detf(2) = detf(2)*f(3,3,2)

!     Transform shape functions to current configuration

      do k = 1,nel
        temp      = fi(1,1)*shps(1,k) + fi(2,1)*shps(2,k)
        shps(2,k) = fi(1,2)*shps(1,k) + fi(2,2)*shps(2,k)
        shps(1,k) = temp
      end do

      end subroutine kine2d
