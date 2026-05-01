      real*8 function zeroj( m_order, n_zero)
      integer*4 m_order, n_zero
! Zeros of the Bessel function J(x)
! Inputs
!   m_order   Order of the Bessel function
!   n_zero    Index of the zero (first, second, etc.)
! Output
!   z         The "n_zero"th zero of the Bessel function (Return value)
! NOTE: Uses the subroutine bess.f

      integer*4 MAXm_order
      parameter( MAXm_order = 200 )
      integer*4 i
      real*8 beta, mu, beta8, z, jj(MAXm_order+2), deriv

      !* Use asymtotic formula for initial guess
      beta = (n_zero + 0.5*m_order - 0.25)*(3.141592654)
      mu = 4*m_order**2
      beta8 = 8*beta
      z = beta - (mu-1)/beta8
     &        - 4*(mu-1)*(7*mu-31)/(3*beta8**3)

      !* Use Newton's method to locate the root
      do i=1,5
        call bess( m_order+1, z, jj )  ! Remember j(1) is J_0(z)
        ! Use the recursion relation to evaluate derivative
        deriv = -jj(m_order+2) + m_order/z * jj(m_order+1)
        z = z - jj(m_order+1)/deriv    ! Newton's root finding
      enddo
      zeroj = z

      return
      end
