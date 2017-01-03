      subroutine timeloop2(y, n, maxsteps, step, time, nsteps)

      integer n, step, nsteps, maxsteps
      real*8 time, y(n,0:maxsteps-1)

Cf2py intent(in,out) step
Cf2py intent(in,out) time
Cf2py intent(in,out) y
Cf2py intent(in)     nsteps

C     internal scratch arrays:
      integer nmax
      parameter (nmax=5)
      real*8 scratch1(n), scratch2(n), scratch3(n)

c     y : y(i,j) store the solution of comp. i at time step j
c     step : initial time step number for the current run
c     time : time value at step step
c     nsteps : the number of time steps to be simulated
c     scratch1-scratch3 : work arrays of length n
c     n : no of components in the diff.eq. system (here 2)
c     maxsteps : size

      integer i

      real*8 m, b, c, A, w, y0, tstop, dt
      character func*20
      common /data/ m, b, c, A, w, y0, tstop, dt, func

c      write(*,*) 'n=',n,' nsteps=',nsteps, ' step=',step,
c     > ' time=',time,' maxsteps=',maxsteps, ' scatch1(1)=',
c     > scratch1(1)

      if (nmax .lt. n) then
         write(*,*) 'timeloop2, ERROR: nmax=',nmax,' too small'
         return
      endif

c     insert initial values from common block?
      if (step .eq. 0) then
         y(1,step) = y0
         y(2,step) = 0
      endif
c     else: just start with y(:,step)

c     the ODE solution algorithms updates a one-dim. array
c     with the solution, copy initial y values to this array:
      do i=1,n
         scratch3(i) = y(i, step)
      end do

c     run a time loop for nsteps steps:
      do step = step+1, step+nsteps, 1
         time = time + dt
c        call adv_fe (scratch3, n, time, dt, scratch1)
c        2nd-order Runge-Kutta algorithm:
         call adv_rk2 (scratch3, n, time, dt, scratch1, scratch2)

c        store solution scratch3 in y:
         do i=1,n
            y(i,step) = scratch3(i)
         end do
      end do
      step = step-1
      return
      end
