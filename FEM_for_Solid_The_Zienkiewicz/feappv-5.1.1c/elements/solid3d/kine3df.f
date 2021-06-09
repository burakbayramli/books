!$Id:$
      subroutine kine3df(shp,ul,f,fi,df,detfi,ndf,nel,nen)

!-----[--.----+----.----+----.-----------------------------------------]
!     Purpose: Compute deformation gradient and its inverse at tn+1

!     Inputs:
!        shp(4,*)  - Shape functions
!        ul(ndf,*) - Nodal solution values
!        ndf       - Degrees of freedom / node
!        nel       - Number of element nodes
!        nen       - Dimension for ul

!     Outputs:
!        f(3,3,*)  - Deformation gradients
!        fi(3,3)   - Inverse deformation gradient
!        df(3,3)   - Incremental deformation gradient
!        detfi(*)  - Determinant of deformation gradient
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      integer       :: ndf,nel,nen, i,j,k
      real (kind=8) :: shp(4,*),ul(ndf,nen,*)
      real (kind=8) :: f(3,3,*),fi(3,3),df(3,3),detfi(*), deti

!     Compute compatible deformation gradient at t-n+1: F = I + GRAD u

      do i = 1,3
        do j = 1,3
          f(i,j,1)  = 0.0d0
          f(i,j,2)  = 0.0d0
          df(i,j)   = 0.0d0
          do k = 1,nel
            f(i,j,1) = f(i,j,1) + ul(i,k,1)*shp(j,k)
            df(i,j)  = df(i,j)  + ul(i,k,2)*shp(j,k)
          end do ! k
          f(i,j,2) = f(i,j,1) - df(i,j)
        end do ! j
        f(i,i,1) = f(i,i,1) + 1.0d0
        f(i,i,2) = f(i,i,2) + 1.0d0
      end do ! i

!     Invert F_n

      detfi(2) = f(1,1,2)*f(2,2,2)*f(3,3,2) + f(1,2,2)*f(2,3,2)*f(3,1,2)
     &         + f(1,3,2)*f(2,1,2)*f(3,2,2) - f(3,1,2)*f(2,2,2)*f(1,3,2)
     &         - f(3,2,2)*f(2,3,2)*f(1,1,2) - f(3,3,2)*f(2,1,2)*f(1,2,2)

!     Invert F_n+1

      detfi(1) = f(1,1,1)*f(2,2,1)*f(3,3,1) + f(1,2,1)*f(2,3,1)*f(3,1,1)
     &         + f(1,3,1)*f(2,1,1)*f(3,2,1) - f(3,1,1)*f(2,2,1)*f(1,3,1)
     &         - f(3,2,1)*f(2,3,1)*f(1,1,1) - f(3,3,1)*f(2,1,1)*f(1,2,1)

      deti    = 1.d0/detfi(1)
      fi(1,1) = (f(2,2,1)*f(3,3,1) - f(3,2,1)*f(2,3,1))*deti
      fi(1,2) =-(f(1,2,1)*f(3,3,1) - f(3,2,1)*f(1,3,1))*deti
      fi(1,3) = (f(1,2,1)*f(2,3,1) - f(2,2,1)*f(1,3,1))*deti
      fi(2,1) =-(f(2,1,1)*f(3,3,1) - f(3,1,1)*f(2,3,1))*deti
      fi(2,2) = (f(1,1,1)*f(3,3,1) - f(3,1,1)*f(1,3,1))*deti
      fi(2,3) =-(f(1,1,1)*f(2,3,1) - f(2,1,1)*f(1,3,1))*deti
      fi(3,1) = (f(2,1,1)*f(3,2,1) - f(3,1,1)*f(2,2,1))*deti
      fi(3,2) =-(f(1,1,1)*f(3,2,1) - f(3,1,1)*f(1,2,1))*deti
      fi(3,3) = (f(1,1,1)*f(2,2,1) - f(2,1,1)*f(1,2,1))*deti

!     Push forward standard shape functions

      do i = 1,nel
        call pushv3f(fi,shp(1,i))
      end do ! i

      end subroutine kine3df
