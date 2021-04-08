!$Id:$
      subroutine BezierToPowerMatrix(p, M)

!-----[--+---------+---------+---------+---------+---------+---------+-]
!     Purpose: Compute p-th degree Bezier matrix

!     Inputs:
!        p       - Degree of polynomial

!     Outputs:
!        M(*,*)  - Matrix of conversion (p+1 x p+1)
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit   none

      integer    p
      real*8     m(0:5,0:5)

      integer    i,j, i1,p1
      real*8     sign
      real*8     binom

!     Set upper triangle to zero
      do i = 0,p-1
        do j = i+1,p
          m(i,j) = 0.0d0
        end do ! j
      end do ! i

!     Set corner elements

      m(0,0) = 1.0d0
      m(p,p) = 1.0d0

!     Test for p even or odd

      if(mod(p,2).eq.0) then
        m(p,0) =  1.0d0
      else
        m(p,0) = -1.0d0
      endif

!     Compute first column and last row

      sign = -1.0d0
      do i = 1,p-1
        m(i,i)   =  binom(p,i)
        m(i,0)   =  sign*m(i,i)
        m(p,p-i) =  m(i,0)
        sign     = -sign
      end do ! i

!     Compute remaining elements of array

      i1 = (p + 1)/2
      p1 =  p - 1
      do i = 1,i1-1
        sign = -1.0d0
        do j = i+1,p1
          m(j ,i  ) =  sign*binom(p,i)*binom(p-i,j-i)
          m(p1,p-j) =  m(j ,i )
          sign      = -sign
        end do ! j
        p1 = p1 - 1
      end do ! i

      end

