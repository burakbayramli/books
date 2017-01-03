C A very simple implementation of a 2D wave equation solver,
C aimed at testing high-performance computing techniques.
C To make the code as simple as possible, the initial condition
C is implemented in a rough way.


      PROGRAM wave2D
      INTEGER n, nsteps
      PARAMETER (n=501)
C      PARAMETER (n=101)

      REAL*8 up(n,n), u(n,n), um(n,n), lambda(n,n)
      REAL*8 dt, tstop, delta
C     domain has size 10x10 in x and y direction
C     delta is the cell size:
      delta = 10.0/(n-1)

      dt = SQRT(1.0/(1.0/(delta*delta) + 1.0/(delta*delta)))
C     dt = delta
      write(*,*) 'Give number of time steps:'
      read(*,*) nsteps
      tstop = nsteps*dt
      write(*,*) 'time step =',dt
      call timeloop(up, u, um, lambda, n, tstop, dt)

      END


C     function for coefficient in the wave equation ("lambda"):
      REAL*8 FUNCTION h(x, y)
      REAL*8 x,y
      h = 1.0
      RETURN
      END

C     initial surface shape:
      REAL*8 FUNCTION bell(x, y)
      REAL*8 x,y
      bell = exp(-x*x - y*y)
      RETURN
      END

      SUBROUTINE setIC(u, um, lambda, n)
C     set initial conditions (rough approximations)
C     set lambda values as well
      INTEGER n
      REAL*8 u(n,n), um(n,n), lambda(n,n)
      INTEGER i, j
      REAL*8 x, y, delta, bell, h
C     domain has size 10x10 in x and y direction
C     delta is the cell size:
      delta = 10.0/(n-1)
      
      DO 20 j=1,n
         DO 10 i=1,n
            x = (i-1)*delta
            y = (j-1)*delta
            u(i,j) = bell(x,y)
C           this is a rough approximation to du/dt=0:
            um(i,j) = u(i,j)

C           initialize the variable coefficient as an array:
            lambda(i,j) = h(x,y)
 10      CONTINUE
 20   CONTINUE
      RETURN
      END


      SUBROUTINE timeloop(up, u, um, lambda, n, tstop, dt)
      INTEGER n
      REAL*8 up(n,n), u(n,n), um(n,n), lambda(n,n)
      REAL*8 tstop, dt, t
      INTEGER nsteps, timelevel
      REAL*8 x, y, delta, bell, h, a, b, c
C     domain has size 10x10 in x and y direction
C     delta is the cell size:
      delta = 10.0/(n-1)
      nsteps = int(tstop/dt)

      t = 0
      call setIC(u, um, lambda, n)
      call dump(u, n, 0)
      a = 1.0
      b = 1.0
      c = 1.0

      DO 30 timelevel=1,nsteps
         t = t + dt
         write(*,*) 'time level', timelevel
         call F77WAVE(up, u, um, lambda, a, b, c, n, n, dt,
     &                delta, delta)
C        update for next step:
         DO 20 j=1,n
            DO 10 i=1,n
               um(i,j) = u(i,j)
               u(i,j) = up(i,j)
 10         CONTINUE
 20      CONTINUE
         call dump(u, n, timelevel)
 30   CONTINUE
      RETURN
      END

C     dump solution in plotmtv format:
      SUBROUTINE dump(u, n, timelevel)
      INTEGER n, i, j, timelevel, iunit
      REAL*8 u(n,n)
      CHARACTER*40 filename
      WRITE(filename,1000) 'tmp_',timelevel,'.mtv'
 1000 FORMAT(A,I5.5,A)
      iunit = 20
      OPEN(iunit, FILE=filename, STATUS='UNKNOWN', FORM='FORMATTED')
      WRITE(iunit,*) '$ DATA=CONTOUR'
      WRITE(iunit,*) '% contstyle=2 nsteps=30 nx=',n,' ny=',n,
     & ' xmin=0 xmax=10 ymin=0 ymax=10' 
      WRITE(iunit,*) '% cmin=-0.2 cmax=0.4 zmin=-0.2 zmax=0.6'
      WRITE(iunit,*) '% leftworld=False'
      WRITE(iunit,*) '% eyepos.x=0.5 eyepos.y=-1.5 eyepos.z=0.5'
      DO 20 j=1,n
         DO 10 i=1,n
            WRITE(iunit,*) u(i,j)
 10      CONTINUE
 20   CONTINUE
      CLOSE(iunit)
      RETURN
      END

      
      
