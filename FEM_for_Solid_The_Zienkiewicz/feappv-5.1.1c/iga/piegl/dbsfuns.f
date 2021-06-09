!$Id:$
      subroutine dbsfuns(i,p,ncp,u,u_knot,n,ders)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Coded by:    Robert L. Taylor
!      Date:        January 11, 2006
!      Release:     1.0

!      Modified by: Rossana Dimitri
!      Date:        February 08, 2012
!      Release:     1.0

!      Purpose: Compute nonzero basis functions and their derivatives.
!               First section is A2.2 modified to store functions and
!               knot differences

!      Algorithm A2.3: The NURBS Book, Page 72

!      Inputs:
!         i            - Span range [i:i+p)
!         u            - Location on knot vector within [U_i:U_i+p)
!         p            - Basis function order
!         n            - Maximum order of derivatives (n <= p)
!         u_knot       - Knot vector

!      Working arrays:
!         ndu(p+1,p+1) - Basis functions and knot differences
!         a(2,p+1)     - Stores A_k,j and A_k-1,j

!      Outputs:
!         Ders(k,j)     - k-th derivative of N_i-p+j,p
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit  none

      integer i, p, ncp, j, r, k, j1, j2, s1, s2, rk, pk, n
      real*8  u, d
      real*8  u_knot(p+ncp+1)
      real*8  ders(n+1,p+1), ndu(p+1,p+1)
      real*8  left(p+1), right(p+1)
      real*8  saved, temp
      real*8  a(2,p+1)

      ndu(1,1) = 1

      do j = 1,p
         left(j+1)  = u - u_knot(i+1-j)
         right(j+1) = u_knot(i+j) - u
         saved      = 0.d0
         do r = 0, j-1
            ndu(j+1,r+1) = right(r+2) + left(j-r+1)
            temp         = ndu(r+1,j) / ndu(j+1,r+1)
            ndu(r+1,j+1) = saved + right(r+2)*temp
            saved        = left(j-r+1) * temp
         enddo
         ndu(j+1,j+1) = saved
      enddo

      ! load basis functions
      do j = 0,p
         ders(1,j+1) = ndu(j+1,p+1)
      enddo

      ! compute derivatives
      ! loop over function index
      do r = 0,p
         s1     = 0
         s2     = 1
         ! alternate rows in array a
         a(1,1) = 1.d0

         ! loop to compute kth derivative
         do k = 1,n
            d  = 0.0d0
            rk = r-k
            pk = p-k
            if (r >= k) then
                a(s2+1,1) = a(s1+1,1)/ndu(pk+2,rk+1)
                d         = a(s2+1,1)*ndu(rk+1,pk+1)
            endif
            if (rk >= -1) then
                j1 = 1
            else
                j1 = -rk
            endif
            if ((r-1) <= pk) then
                j2 = k-1
            else
               j2 = p-r
            endif
            do j = j1,j2
               a(s2+1,j+1) = (a(s1+1,j+1) - a(s1+1,j))/ndu(pk+2,rk+j+1)
               d           = d + a(s2+1,j+1)*ndu(rk+j+1,pk+1)
            enddo
            if (r <= pk) then
               a(s2+1,k+1) = -a(s1+1,k)/ndu(pk+2,r+1)
               d           = d + a(s2+1,k+1)*ndu(r+1,pk+1)
            endif
            ders(k+1,r+1) = d
            ! switch rows
            j  = s1
            s1 = s2
            s2 = j
         enddo
      enddo

      !Multiply through by the correct factors
      r = p
      do k = 1,n
         do j = 0,p
            ders(k+1,j+1) = ders(k+1,j+1)*dble(r)
         enddo
         r = r*(p-k)
      enddo

      end
