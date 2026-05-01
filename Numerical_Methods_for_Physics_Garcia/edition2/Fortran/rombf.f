      subroutine rombf( a, b, N,MAXN, func, param, R )
      integer*4 N, MAXN
      real*8 a, b, param(*), R(MAXN,MAXN)
      external func
!  Function to compute integrals by Romberg algorithm
!  R = rombf(a,b,N,MAXN,func,param)
!  Inputs
!    a,b    Lower and upper bound of the integral
!    N      Romberg table is computed to N by N
!    R      Array R is dimensioned as R(MAXN,MAXN)
!    func   Integrand function; the calling sequence
!           is: double (*func)( double x, Matrix param )
!    param  Set of parameters to be passed to function
!  Output
!     R     Romberg table; Entry R(N,N) is best estimate of
!           the value of the integral

      integer*4 np, i, j, k, m
      real*8 h, sumT

      !* Compute the first term R(1,1)
      h = b - a     ! This is the coarsest panel size
      np = 1        ! Current number of panels
      R(1,1) = h/2 * ( func(a,param) + func(b,param) )

      !* Loop over the desired number of rows, i = 2,...,N
      do i=2,N

        !* Compute the summation in the recursive trapezoidal rule
        h = h/2.0          ! Use panels half the previous size
        np = 2*np          ! Use twice as many panels
        sumT = 0.0
        do k=1,(np-1),2
          sumT = sumT + func( a + k*h, param)
        enddo

        !* Compute Romberg table entries R(i,1), R(i,2), ..., R(i,i)
        R(i,1) = 0.5 * R(i-1,1) + h * sumT
        m = 1
        do j=2,i
          m = 4*m;
          R(i,j) = R(i,j-1) + (R(i,j-1) - R(i-1,j-1))/(m-1)
        enddo
      enddo

      return
      end
