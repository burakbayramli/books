      subroutine rka( x, nX, t, tau, err, derivsRK, param )
      integer*4 MAXnX, MAXnparam
      parameter( MAXnX = 50, MAXnparam = 1000 )
      integer*4 nX
      real*8 x(MAXnX), t, tau, err, param(MAXnparam)
      external derivsRK

! Adaptive Runge-Kutta routine
! Inputs
!   x          Current value of the dependent variable
!   nX         Number of elements in dependent variable x
!   t          Independent variable (usually time)
!   tau        Step size (usually time step)
!   err        Desired fractional local truncation error
!   derivsRK   Right hand side of the ODE; derivsRK is the
!              name of the function which returns dx/dt
!              Calling format derivsRK(x,t,param).
!   param      Extra parameters passed to derivsRK
! Outputs
!   x          New value of the dependent variable
!   t          New value of the independent variable
!   tau        Suggested step size for next call to rka

      integer*4 i, iTry, maxTry
      real*8 tSave, safe1, safe2, xSmall(MAXnX), xBig(MAXnX)
      real*8 errorRatio, eps, scale, xDiff, ratio, tau_old, half_tau

      !* Set initial variables
      tSave = t      ! Save initial value
      safe1 = 0.9
      safe2 = 4.0    ! Safety factors

      !* Loop over maximum number of attempts to satisfy error bound
      maxTry = 100
      do iTry=1,maxTry

        !* Take the two small time steps
        half_tau = 0.5 * tau
        do i=1,nX
          xSmall(i) = x(i)
        enddo
        call rk4(xSmall,nX,tSave,half_tau,derivsRK,param)
        t = tSave + half_tau
        call rk4(xSmall,nX,t,half_tau,derivsRK,param)

        !* Take the single big time step
        t = tSave + tau
        do i=1,nX
          xBig(i) = x(i)
        enddo
        call rk4(xBig,nX,tSave,tau,derivsRK,param)

        !* Compute the estimated truncation error
        errorRatio = 0.0
        eps = 1.0e-16
        do i=1,nX
          scale = err * (abs(xSmall(i)) + abs(xBig(i)))/2.0
          xDiff = xSmall(i) - xBig(i)
          ratio = abs(xDiff)/(scale + eps)
          if( ratio .gt. errorRatio ) then
            errorRatio = ratio
          endif
        enddo

        !* Estimate new tau value (including safety factors)
        tau_old = tau
        tau = safe1*tau_old*errorRatio**(-0.20)
        if( tau .lt. tau_old/safe2 ) then
          tau = tau_old/safe2
        else if( tau .gt. safe2*tau_old ) then
          tau = safe2*tau_old
        endif

        !* If error is acceptable, return computed values
        if (errorRatio .lt. 1) then
          do i=1,nX
            x(i) = xSmall(i)
          enddo
          return
        endif
      enddo

      !* Issue error message if error bound never satisfied
      write(*,*) 'ERROR: Adaptive Runge-Kutta routine failed'

      return
      end
