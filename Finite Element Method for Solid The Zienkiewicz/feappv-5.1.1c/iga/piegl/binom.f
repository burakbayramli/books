!$Id:$
      function binom(n,k)

!-----[--+---------+---------+---------+---------+---------+---------+-]
!      Purpose: Compute binomial coefficients

!      Inputs :
!         n        - Upper parameter
!         k        - Lower parameter

!      Outputs:
!         binom    - Binomial coefficient
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit  none

      integer   n,k, i, num,den
      real*8    binom

      if    (k.lt.0 .or. k.gt.n) then
        binom = 0.0d0
      elseif(k.eq.0 .or. k.eq.n) then
        binom = 1.0d0
      else
        den = 1
        do i = 1,min(k,n-k)
          den = den*i
        end do ! i
        num = 1
        do i = max(k,n-k)+1,n
          num = num*i
        end do ! i
        binom = dble(num)/dble(den)
      endif

      end
