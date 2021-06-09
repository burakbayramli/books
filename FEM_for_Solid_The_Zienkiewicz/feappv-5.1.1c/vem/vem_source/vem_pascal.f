!$Id:$
      subroutine vem_pascal(k, xx, xc, hVm1, p, nm)

!-----[--+---------+---------+---------+---------+---------+---------+-]
!     Purpose: Compute 2-d polynomial and derivative in Pascal triangle

!     Inputs:
!       k        - Order of polynomials (k = 0, 1, 2, ....)
!       xx(2)    - Polynomial point to use
!       xc(2)    - Centroid   coordinates
!       hVm1     - Reciprocal scaling factor (No rescaling if <= 0)

!     Outputs:
!       p(0,nm)  - Polynomial (0); First derivative (1:2)
!       nm       - Number of terms (nm = 1, 3, 6, 10, ...)
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit   none

      integer          :: k            ! Order of methoe
      integer          :: nm           ! Number of terms in triangle
      real    (kind=8) :: hVm1         ! Reciprocal scaling factor
      real    (kind=8) :: xx(2)        ! Cartesian coordinate
      real    (kind=8) :: xc(2)        ! Centroid  coordinate
      real    (kind=8) :: p(0:2,0:*)   ! Pascal polynmoials

!     Monomial functions

      integer          :: i, j         ! Loop counters
      real    (kind=8) :: xi(2)        ! Scaled coordinates
      real    (kind=8) :: mm(2,0:10)   ! Monomial function
      real    (kind=8) :: dm(2,0:10)   ! Monomial functions

      save

!     Compute scaled coordinates

      if(hVm1.gt.0.0d0) then
        xi(:) = (xx(:) - xc(:))*hVm1
      else
        xi(:) =  xx(:)
      endif

!     Build coordinate powers for xi(1:2)

      mm(:,0) = 1.0d0
      do i = 1, k
        mm(:,i) = mm(:,i-1)*xi(:)
      end do ! i

      dm(:,0) = 0.0d0
      dm(:,1) = 1.0d0
      do i = 2,k
        dm(:,i) = dble(k)*dm(:,i-1)*xi(:)
      end do ! i
      dm(:,1:k) = dm(:,1:k) * hVm1

!     Functions and derivatives from Pascal triangle

      p(:,0) = 0.0d0
      p(0,0) = 1.0d0
      nm   = 0
      do i = 1,k
        do j = 0,i
          nm    = nm + 1
          p(0,nm) = mm(1,i-j)*mm(2,j)  ! Pascal function
          p(1,nm) = dm(1,i-j)*mm(2,j)  ! Pascal x_1-derivative
          p(2,nm) = mm(1,i-j)*dm(2,j)  ! Pascal x_2-derivative
        end do ! j
      end do ! i

      nm = nm + 1 ! to account for the "0" entry

      end subroutine vem_pascal
