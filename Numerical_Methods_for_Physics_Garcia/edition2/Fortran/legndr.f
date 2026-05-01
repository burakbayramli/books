      subroutine legndr( n, x, p)
      integer*4 n
      real*8 x, p(*)
! Legendre polynomials function
! Inputs
!    n    Highest order polynomial returned
!    x    Value at which polynomial is evaluated
! Output
!    p    Vector containing P(x) for order 0,1,...,n

      integer*4 i
      !* Perform upward recursion
      p(1) = 1      ! P(x) for n=0
      if(n .eq. 0) return
      p(2) = x      ! P(x) for n=1
      ! Use upward recursion to obtain other n's
      do i=3,(n+1)
        p(i) = ((2*i-3)*x*p(i-1) - (i-2)*p(i-2))/(i-1)
      enddo
      return
      end
