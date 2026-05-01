      subroutine bess( m_max, x, jj )
      integer*4 m_max
      real*8 x, jj(*)
! Bessel function
! Inputs
!    m_max  Largest desired order
!    x = Value at which Bessel function J(x) is evaluated
! Output
!    jj = Vector of J(x) for order m = 0, 1, ..., m_max

      integer*4 MAXm_top
      parameter( MAXm_top = 1000 )
      integer*4 maxmx, m_top, m
      real*8 tinyNumber, j(MAXm_top+1), norm

      !* Perform downward recursion from initial guess
      if( m_max .gt. x ) then
        maxmx = m_max
      else             ! maxmx = Max(m_max,x)
        maxmx = int(x)
      endif
      ! Recursion is downward from m_top (which is even)
      m_top = 2*(int( (maxmx+15)/2 + 1 ))
      j(m_top+1) = 0.0
      j(m_top) = 1.0
      tinyNumber = 1e-16
      do m=(m_top-2), 0, -1       ! Downward recursion
        j(m+1) = 2*(m+1)/(x+tinyNumber)*j(m+2) - j(m+3)
      enddo

      !* Normalize using identity and return requested values
      norm = j(1)                ! NOTE: Be careful, m=0,1,... but
      do m=2, m_top, 2           ! vector goes j(1),j(2),...
        norm = norm + 2*j(m+1)
      enddo
      do m=0,m_max               ! Send back only the values for
        jj(m+1) = j(m+1)/norm    ! m=0,...,m_max and discard values
      enddo                      ! for m=m_max+1,...,m_top
      return
      end
