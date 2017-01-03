C A very simple implementation of a 2D wave equation solver,
C aimed at testing high-performance computing techniques.
C To make the code as simple as possible, the initial condition
C is implemented in a rough way.


      PROGRAM wave2D
      INTEGER n, nsteps, dumpsolution
      PARAMETER (n=31)
C      PARAMETER (n=501)
C      PARAMETER (n=101)

      REAL*8 up(n,n), u(n,n), um(n,n), lambda(n,n)
      REAL*8 dt, tstop, delta, h1, bell1
      EXTERNAL h1, bell1
C     domain has size 10x10 in x and y direction
C     delta is the cell size:
      delta = 10.0/(n-1)

      dt = SQRT(1.0/(1.0/(delta*delta) + 1.0/(delta*delta)))
C     dt = delta
      write(*,*) 'Give number of time steps:'
      read(*,*) nsteps
      tstop = nsteps*dt
      write(*,*) 'time step =',dt
      dumpsolution = 1
      call timeloop(up, u, um, lambda, n, tstop, dt, h1, bell1, 
     >              dumpsolution)

      END


C     function for coefficient in the wave equation ("lambda"):
      REAL*8 FUNCTION h1(x, y)
      REAL*8 x,y
      h1 = 1.0
      RETURN
      END

C     initial surface shape:
      REAL*8 FUNCTION bell1(x, y)
      REAL*8 x,y
      bell1 = exp(-x*x - y*y)
      RETURN
      END

      SUBROUTINE setIC(u, um, n, surface)
C     set initial conditions (rough approximations)
      INTEGER n
      REAL*8 u(n,n), um(n,n), surface
      EXTERNAL surface
Cf2py intent(in, out) u
C2fpy intent(in, out) um
      INTEGER i, j
      REAL*8 x, y, delta
C     domain has size 10x10 in x and y direction
C     delta is the cell size:
      delta = 10.0/(n-1)
      
      DO 20 j=1,n
         DO 10 i=1,n
            x = (i-1)*delta
            y = (j-1)*delta
            u(i,j) = surface(x,y)
C           this is a rough approximation to du/dt=0:
            um(i,j) = u(i,j)
 10      CONTINUE
 20   CONTINUE
      RETURN
      END


      SUBROUTINE setBottom(lambda, n, bottom)
      INTEGER n
      REAL*8 lambda(n,n), bottom
      EXTERNAL bottom
C2fpy intent(in, out) lambda
      INTEGER i, j
      REAL*8 x, y, delta
C     domain has size 10x10 in x and y direction
C     delta is the cell size:
      delta = 10.0/(n-1)
      
      DO 20 j=1,n
         DO 10 i=1,n
            x = (i-1)*delta
            y = (j-1)*delta
            lambda(i,j) = bottom(x,y)
 10      CONTINUE
 20   CONTINUE
      RETURN
      END


      SUBROUTINE timeloop(up, u, um, lambda, n, tstop, dt,
     >                    bottom, surface, dumpsolution)
      INTEGER n, dumpsolution
      REAL*8 up(n,n), u(n,n), um(n,n), lambda(n,n)
      REAL*8 tstop, dt, t, bottom, surface
      EXTERNAL bottom, surface
Cf2py intent(in, out) up

      INTEGER nsteps, timelevel
      nsteps = int(tstop/dt)

      t = 0
      call setIC(u, um, n, surface)
      call setBottom(lambda, n, bottom)
      if (dumpsolution .eq. 1) then
         call dump(u, n, 0, 0.0D0)
      end if

      DO 30 timelevel=1,nsteps
         t = t + dt
         write(*,*) 'time level', timelevel
         call solveAtThisTimeStep(up,u,um,lambda,n,dt)
         if (dumpsolution .eq. 1) then
            call dump(u, n, timelevel, t)
         end if

C        update for next step:
         DO 20 j=1,n
            DO 10 i=1,n
               um(i,j) = u(i,j)
               u(i,j) = up(i,j)
 10         CONTINUE
 20      CONTINUE
 30   CONTINUE
      RETURN
      END

      SUBROUTINE solveAtThisTimeStep(up, u, um, lambda, n, dt)
      INTEGER n
      REAL*8 up(n,n), u(n,n), um(n,n), lambda(n,n)
Cf2py intent(in, out) up
      REAL*8 dt

      REAL*8 delta, a, b, c

      delta = 10.0/(n-1)
      a = 1.0
      b = 1.0
      c = 1.0

      call F77WAVE(up, u, um, lambda, a, b, c, n, n, dt, delta, delta)
      END

C     dump solution in ASCII plotmtv format:
      SUBROUTINE dump(u, n, timelevel, time)
      INTEGER n, i, j, timelevel, iunit
      REAL*8 u(n,n), time
      CHARACTER*40 filename
      WRITE(filename,1000) 'tmp_',timelevel,'.mtv'
 1000 FORMAT(A,I5.5,A)
      iunit = 20
      OPEN(iunit, FILE=filename, STATUS='UNKNOWN', FORM='FORMATTED')
      WRITE(iunit,*) '$ DATA=CONTOUR'
      WRITE(iunit,*) '% toplabel="t =',time,'"'
      WRITE(iunit,*) '% contstyle=2 nsteps=30 nx=',n,' ny=',n,
     & ' xmin=0 xmax=10 ymin=0 ymax=10' 
      WRITE(iunit,*) '% cmin=-0.2 cmax=0.4 zmin=-0.2 zmax=0.6'
      WRITE(iunit,*) '% leftworld=False'
      WRITE(iunit,*) '% eyepos.x=0.5 eyepos.y=-1.5 eyepos.z=0.5'
C     plotmtv requires i to have the fastest variation:
      DO 20 j=1,n
         DO 10 i=1,n
            WRITE(iunit,*) u(i,j)
 10      CONTINUE
 20   CONTINUE
      CLOSE(iunit)
      RETURN
      END

C     dump solution in binary plotmtv format:
      SUBROUTINE dumpb(u, n, timelevel)
C     note: this subroutine does not work; the ASCII output
C     must be formatted first in a string and then dumped
C     as un unformatted string
      INTEGER n, i, j, timelevel, iunit
      REAL*8 u(n,n)
      CHARACTER*40 filename
      WRITE(filename,1000) 'tmp_',timelevel,'.mtv'
 1000 FORMAT(A,I5.5,A)
      iunit = 20
      OPEN(iunit, FILE=filename, STATUS='UNKNOWN', FORM='UNFORMATTED')
      WRITE(iunit,*) '$ DATA=CONTOUR\n'
      WRITE(iunit,*) '% contstyle=2 nsteps=30 nx=',n,' ny=',n,
     & ' xmin=0 xmax=10 ymin=0 ymax=10\n' 
      WRITE(iunit,*) '% cmin=-0.2 cmax=0.4 zmin=-0.2 zmax=0.6\n'
      WRITE(iunit,*) '% leftworld=False\n'
      WRITE(iunit,*) '% eyepos.x=0.5 eyepos.y=-1.5 eyepos.z=0.5\n'
      WRITE(iunit,*) '% binary=T\n'
C     plotmtv requires i to have the fastest variation:
      WRITE(iunit) ((u(i,j), i=1,n), j=1,n)
      CLOSE(iunit)
      RETURN
      END

      
      
