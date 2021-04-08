!$Id:$
      subroutine BasisFuns(i, uu,p, U, N)

!-----[--+---------+---------+---------+---------+---------+---------+-]
!      Purpose: Compute non-vanishing basis functions for span [i:i+p)

!      Algorithm A2.2: The NURBS Book, Page 70

!      Inputs :
!        i      - Start of span
!        uu     - Value along knot vector
!        p      - Polynomial order
!        U(0:*) - Knot vector

!      Outputs:
!        N(*)   - Non-zero basis functions within span: [u_i:u_i+p)
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit  none

      integer   i,p, j,r
      real*8    uu, U(0:*), N(0:*)
      real*8    saved, temp, left(11),right(11)

      N(0) = 1.0d0
      do j = 1,p
        left(j)  = uu - U(i+1-j)
        right(j) = U(i+j) - uu
        saved    = 0.0d0
        do r = 0,j-1
          temp  = N(r)/(right(r+1) + left(j-r))
          N(r)  = saved + right(r+1)*temp
          saved = left(j-r)*temp
        end do ! r
        N(j) = saved
      end do ! j

      end
