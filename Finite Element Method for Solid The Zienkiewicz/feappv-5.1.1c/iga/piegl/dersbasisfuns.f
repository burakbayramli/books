!$Id:$
      subroutine DersBasisFuns(i, uu,p,nd, U, Ders)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--+---------+---------+---------+---------+---------+---------+-]
!      Purpose: Compute nonzero basis functions and their derivatives.
!               First section is A2.2 modified to store functions and
!               knot differences

!      Algorithm A2.3: The NURBS Book, Page 72

!      Inputs:
!         i            - Span range [i:i+p)
!         uu           - Location on knot vector within [U_i:U_i+p)
!         p            - Basis function order
!         nd           - Maximum order of derivatives (nd <= p)
!         U(0:*)       - Knot vector

!      Working arrays:
!         Ndu(0:p,0:p) - Basis functions and knot differences
!         A(0:1,0:p)   - Stores A_k,j and A_k-1,j

!      Outputs:
!         Ders(k,j)     - k-th derivative of N_i-p+j,p
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit  none

      integer   i,p,n,nd
      real*8    uu, U(0:*), Ders(0:nd,0:p)

      integer   j,k,r, j1,j2, s1,s2, rk,pk
      real*8    saved, temp, d
      real*8    left(21),right(21), Ndu(0:20,0:20), A(0:1,0:20)

!     n = min(p,nd)

!     do i = p+1,nd
!       do k = 0,p
!         Ders(i,k) = 0.0d0
!       end do ! k
!     end do ! i
      n = nd

      Ndu(0,0) = 1.0d0
      do j = 1,p
        left(j)  = uu - U(i+1-j)
        right(j) = U(i+j) - uu
        saved    = 0.0d0
        do r = 0,j-1

!         Lower triangle

          Ndu(j,r) = right(r+1) + left(j-r)
          temp     = Ndu(r,j-1)/Ndu(j,r)

!         Upper triangle

          Ndu(r,j) = right(r+1)*temp + saved
          saved    = left (j-r)*temp
        end do ! r

!       Diagonal entry

        Ndu(j,j) = saved
      end do ! j

!     Load basis functions

      do j = 0,p
        Ders(0,j) = Ndu(j,p)
      end do ! j

!     This section computes derivatives (Eq 2.9)

      do r = 0,p

!       Alternate rows  in array A

        s1     = 0
        s2     = 1
        A(0,0) = 1.0d0

!       Loop to compute k-th derivative

        do k = 1,n
          d  = 0.0d0
          rk = r - k
          pk = p - k
          if(r.ge.k) then
            A(s2,0) = A(s1,0)/Ndu(pk+1,rk)
            d       = A(s2,0)*Ndu(rk  ,pk)
          endif
          if(rk .ge. -1) then
            j1 = 1
          else
            j1 = -rk
          endif
          if(r-1 .le. pk) then
            j2 = k-1
          else
            j2 = p - r
          endif
          do j = j1,j2
            A(s2,j) = (A(s1,j) - A(s1,j-1))/Ndu(pk+1,rk+j)
            d       = d        + A(s2,j  ) *Ndu(rk+j,pk  )
          end do ! j

          if(r .le. pk) then
            A(s2,k) =   - A(s1,k-1)/Ndu(pk+1,r)
            d       = d + A(s2,k  )*Ndu(r   ,pk)
          endif
          Ders(k,r) = d

!         Switch rows

          j         = s1
          s1        = s2
          s2        = j
        end do ! k
      end do ! r

!     Multiply through by the correct factors (Eq. 2.9)

      r =  p
      do k = 1,n
        do j = 0,p
          Ders(k,j) = Ders(k,j)*dble(r)
        end do ! j
        r = r*(p-k)
      end do ! k

      end
