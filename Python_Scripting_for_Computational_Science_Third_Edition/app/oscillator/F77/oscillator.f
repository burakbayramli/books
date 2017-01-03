      program oscillator

      call scan1()
      call timeloop()
      end

      subroutine scan1()
C     Read input data (physical and numerical parameters)
C     from standard input. The data are stored in a common block.

      real*8 m_, b_, c_, A_, w_, y0_, tstop_, dt_
      character func_*20
      read(*,*) m_, b_, c_, func_, A_, w_, y0_, tstop_, dt_

C      write(*,*) 'scan1', ' m=', m_, ' b=', b_, ' c=', c_, ' A=', A_,
C     >           ' w=', w_, ' y0=', y0_, ' tstop=', tstop_, ' dt=', dt_, 
C     >           ' c-term function:', func_
      call scan2(m_, b_, c_, A_, w_, y0_, tstop_, dt_, func_)
      return 
      end

      subroutine scan2(m_, b_, c_, A_, w_, y0_, tstop_, dt_, func_)
C     Initialize the data needed in the solver, and store in a
C     common block /data/, from the arguments to this subroutine.

      real*8 m_, b_, c_, A_, w_, y0_, tstop_, dt_
      character func_*(*)

      real*8 m, b, c, A, w, y0, tstop, dt
      character func*20
      common /data/ m, b, c, A, w, y0, tstop, dt, func

      m = m_
      b = b_
      c = c_
      A = A_
      w = w_
      y0 = y0_
      tstop = tstop_
      dt = dt_
      func = func_
      return
      end

      subroutine adv_fe(y, n, t, dt, scratch)
C     Advance the solution one time step using the
C     Forward Euler method.

      integer n, i
      real*8 y(n), t, dt, scratch(n)

      call rhs(scratch, y, n, t)
C     Forward Euler scheme:
      do 10 i = 1,n
         y(i) = y(i) + dt*scratch(i)
 10   continue
      return
      end

      subroutine adv_rk2(y, n, t, dt, scratch1, scratch2)
C     Advance the solution one time step using a
C     2nd order Runge-Kutta method.

      integer n, i
      real*8 y(n), t, dt, scratch1(n), scratch2(n), t2

      call rhs(scratch1, y, n, t)
      do 10 i = 1,n
         scratch2(i) = y(i) + dt*scratch1(i)
 10   continue
      t2 = t + dt
      call rhs(scratch2, scratch2, n, t2)
      do 20 i = 1,n
         y(i) = y(i) + 0.5*dt*(scratch1(i) + scratch2(i))
 20   continue
      return
      end

      subroutine rhs(f, y, n, t)
C     Define the right-hand side of the ODE system.

      integer n
      real*8 f(n), y(n), t
      real*8 cterm

      real*8 m, b, c, A, w, y0, tstop, dt
      character func*20
      common /data/ m, b, c, A, w, y0, tstop, dt, func

      if (func .eq. 'y') then
         cterm = y(1)
      else if (func .eq. 'siny') then
         cterm = sin(y(1))
      else if (func .eq. 'y3') then
         cterm = y(1) - (y(1)**3)/6.0
      else
         write(*,*) 'Error: spring-term ',func,' illegal'
         cterm = 0.0
      endif

      f(1) = y(2)
      f(2) = ( -b*y(2) - c*cterm + A*cos(w*t) )/m
      return
      end

      subroutine timeloop()
C     Integrate the ODEs in time by calling adv_fe or
C     adv_rk2 at every time step.

      integer n, nmax
      parameter (nmax=2)
      real*8 time, y(nmax), scratch1(nmax), scratch2(nmax)
      real*8 m, b, c, A, w, y0, tstop, dt
      character func*20
      common /data/ m, b, c, A, w, y0, tstop, dt, func
      integer nsteps, i

C      write(*,*) 'timeloop', ' m=', m, ' b=', b, ' c=', c, ' A=', A, 
C     >           ' w=', w, ' y0=', y0, ' tstop=', tstop, ' dt=', dt, 
C     >           ' c-term function:', func

      n = 2
C     initial conditions:
      y(1) = y0
      y(2) = 0.0

      open(21, file='sim.dat', status='unknown', form='formatted')
      nsteps = int(tstop/dt)
      do 10 i = 1, nsteps
         time = i*dt
C         call adv_fe (y, n, time, dt, scratch1)
         call adv_rk2 (y, n, time, dt, scratch1, scratch2)
         write(21,1000) time, y(1)
 10   continue
      close(21)
      return
 1000 FORMAT(F12.4, F12.4)
      end



