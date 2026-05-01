      subroutine rk4( x, nX, t, tau, derivsRK, param )
      integer*4 MAXnX, MAXnparam
      parameter( MAXnX = 50, MAXnparam = 1000 )
      integer*4 nX
      real*8 x(nX), t, tau, param(MAXnparam)
! Runge-Kutta integrator (4th order)
! Inputs
!   x          Current value of dependent variable
!   nX         Number of elements in dependent variable x
!   t          Independent variable (usually time)
!   tau        Step size (usually time step)
!   derivsRK   Right hand side of the ODE; derivsRK is the
!              name of the function which returns dx/dt
!              Calling format derivsRK(x,t,param,dxdt).
!   param      Extra parameters passed to derivsRK
! Output
!   x          New value of x after a step of size tau

      integer*4 i
      real*8 half_tau, t_half, t_full
      real*8 F1(MAXnX), F2(MAXnX), F3(MAXnX), F4(MAXnX), xtemp(MAXnX)

      !* Evaluate F1 = f(x,t).
      call derivsRK( x, t, param, F1 )

      !* Evaluate F2 = f( x+tau*F1/2, t+tau/2 ).
      half_tau = 0.5*tau
      t_half = t + half_tau
      do i=1,nX
        xtemp(i) = x(i) + half_tau*F1(i)
      enddo
      call derivsRK( xtemp, t_half, param, F2 )

      !* Evaluate F3 = f( x+tau*F2/2, t+tau/2 ).
      do i=1,nX
        xtemp(i) = x(i) + half_tau*F2(i)
      enddo
      call derivsRK( xtemp, t_half, param, F3 )

      !* Evaluate F4 = f( x+tau*F3, t+tau ).
      t_full = t + tau
      do i=1,nX
        xtemp(i) = x(i) + tau*F3(i)
      enddo
      call derivsRK( xtemp, t_full, param, F4 )

      !* Return x(t+tau) computed from fourth-order R-K.
      do i=1,nX
        x(i) = x(i) + tau/6.*(F1(i) + F4(i) + 2.*(F2(i)+F3(i)))
      enddo

      return
      end
