      program driver
c
c  Generic driver routine for claw1
c
c  Author: Randall J. LeVeque
c  Version of March, 1999 --  CLAWPACK Version 4.0
c
c
      implicit double precision (a-h,o-z)

c     # set parameters for maximum array sizes used in declarations
c     # these must be increased for larger problems.
c
c
      parameter (maxmx = 500)
      parameter (mwork = 4032)

      parameter (mbc = 2)
      parameter (meqn = 1)
      parameter (mwaves = 1)
      parameter (maux = 0)
c
      dimension q(1-mbc:maxmx+mbc, meqn)
      dimension  aux(1)   !# dummy variable since no aux arrays used
      dimension work(mwork)
      dimension mthlim(mwaves)

      call claw1ez(maxmx,meqn,mwaves,mbc,maux,mwork,mthlim,
     &		 q,work,aux)

      stop 
      end
c
c
c =========================================================
       subroutine qinit(maxmx,meqn,mbc,mx,xlower,dx,q,maux,aux)
c =========================================================
c
c     # Set initial conditions for q.
c
c
      implicit double precision (a-h,o-z)
      dimension q(1-mbc:maxmx+mbc, meqn)
      dimension aux(1-mbc:maxmx+mbc, *)
      common /comic/ beta
c
c
      pi2 = 8.d0*datan(1.d0)  !# = 2 * pi
      do 150 i=1,mx
	 xcell = xlower + (i-0.5d0)*dx
	 q(i,1) = dexp(-beta * (xcell-0.25d0)**2)
  150    continue
c
      return
      end
c
c
c =========================================================
      subroutine rp1(maxmx,meqn,mwaves,mbc,mx,ql,qr,auxl,auxr,
     &		 wave,s,amdq,apdq)
c =========================================================
c
c     # solve Riemann problems for the 1D advection equation q_t + u*q_x = 0.
c     # For constant advection velocity u, passed in common block.
c
c     # The advection speed u is passed in the common block comrp
c     # On input, ql contains the state vector at the left edge of each cell
c     #           qr contains the state vector at the right edge of each cell
c     # On output, wave contains the waves,
c     #            s the speeds,
c     #            amdq the  left-going flux difference  A^- \Delta q
c     #            apdq the right-going flux difference  A^+ \Delta q
c
c     # Note that the i'th Riemann problem has left state qr(i-1,:)
c     #                                    and right state ql(i,:)
c     # From the basic clawpack routine step1, rp is called with ql = qr = q.
c
c
      implicit double precision (a-h,o-z)
      dimension   ql(1-mbc:maxmx+mbc, meqn)
      dimension   qr(1-mbc:maxmx+mbc, meqn)
      dimension    s(1-mbc:maxmx+mbc, mwaves)
      dimension wave(1-mbc:maxmx+mbc, meqn, mwaves)
      dimension amdq(1-mbc:maxmx+mbc, meqn)
      dimension apdq(1-mbc:maxmx+mbc, meqn)
      common /comrp/ u
c
c
c
      do 30 i=2-mbc,mx+mbc
c
c        # Compute the wave and speed
c
         wave(i,1,1) = ql(i,1) - qr(i-1,1)
         s(i,1) = u
c
c
c        # Compute the wave and speed
c
         wave(i,1,1) = ql(i,1) - qr(i-1,1)
         s(i,1) = u
         amdq(i,1) = dmin1(u, 0.d0) * wave(i,1,1)
         apdq(i,1) = dmax1(u, 0.d0) * wave(i,1,1)
   30    continue
c
      return
      end
      subroutine setprob
      implicit double precision (a-h,o-z)
      common /comrp/ u
      common /comic/ beta
      common /comsrc/ rate
c
c     # Set the velocity for scalar advection
c     # This value is passed to the Riemann solver rp1.f in a common block
c
c     # Set the width of the initial Gaussian pulse
c     # beta is passed to qinit.f in comic
c
c     # Set the rate in the source term
c     # beta is passed to src1.f in comsrc
c
      open(unit=7,file='setprob.data',status='old',form='formatted')

      read(7,*) u
      read(7,*) beta
      read(7,*) rate

      return
      end

c
c
c =========================================================
      subroutine src1(maxmx,meqn,mbc,mx,xlower,dx,q,aux,t,dt)
c =========================================================
      implicit double precision (a-h,o-z)
      dimension q(1-mbc:maxmx+mbc, meqn)
      common /comsrc/ rate
c
c     # solve q' = -rate*q 
c     # use the exact solution operator
c
      do i=1,mx
	 xcell = xlower + (i-0.5d0) * dx
	 rate = 1.d0 - xcell

c        # 2-stage Runge-Kutta method:
	 qstar = (1.d0 - 0.5d0*dt*rate) * q(i,1)
	 q(i,1) = q(i,1) - dt*rate*qstar
c
c        # Note that one could instead use the exact solution operator
c        # for this source term:
c        q(i,1) = dexp(-dt*rate) * q(i,1)

	 enddo
c
      return
      end

c
c
c     =====================================================
      subroutine bc1(maxmx,meqn,mbc,mx,xlower,dx,q,maux,aux,t,mthbc)
c     =====================================================
c
c     # Standard boundary condition choices for claw2
c
c     # At each boundary  k = 1 (left),  2 (right):
c     #   mthbc(k) =  0  for user-supplied BC's (must be inserted!)
c     #            =  1  for zero-order extrapolation
c     #            =  2  for periodic boundary coniditions
c     #            =  3  for solid walls, assuming this can be implemented
c     #                  by reflecting the data about the boundary and then
c     #                  negating the 2'nd component of q.
c     ------------------------------------------------
c
c     # Extend the data from the computational region
c     #      i = 1, 2, ..., mx2
c     # to the virtual cells outside the region, with
c     #      i = 1-ibc  and   i = mx+ibc   for ibc=1,...,mbc
c
      implicit double precision (a-h,o-z)
      dimension q(1-mbc:maxmx+mbc, meqn)
      dimension aux(1-mbc:maxmx+mbc, *)

      dimension mthbc(2)

c
c
c-------------------------------------------------------
c     # left boundary:
c-------------------------------------------------------
      go to (100,110,120,130) mthbc(1)+1
c
  100 continue
c     # user-specified boundary conditions go here in place of error output
      write(6,*) '*** ERROR *** mthbc(1)=0 and no BCs specified in bc1'
      stop
      go to 199
c
  110 continue
c     # zero-order extrapolation:
      do 115 m=1,meqn
         do 115 ibc=1,mbc
               q(1-ibc,m) = q(1,m)
  115       continue
      go to 199

  120 continue
c     # periodic:  
      do 125 m=1,meqn
         do 125 ibc=1,mbc
               q(1-ibc,m) = q(mx+1-ibc,m)
  125       continue
      go to 199

  130 continue
c     # solid wall (assumes 2'nd component is velocity or momentum in x):
      do 135 m=1,meqn
         do 135 ibc=1,mbc
               q(1-ibc,m) = q(ibc,m)
  135       continue
c     # negate the normal velocity:
      do 136 ibc=1,mbc
            q(1-ibc,2) = -q(ibc,2)
  136    continue
      go to 199

  199 continue

c
c-------------------------------------------------------
c     # right boundary:
c-------------------------------------------------------
      go to (200,210,220,230) mthbc(2)+1
c
  200 continue
c     # user-specified boundary conditions go here in place of error output
      write(6,*) '*** ERROR *** mthbc(2)=0 and no BCs specified in bc2'
      stop
      go to 299

  210 continue
c     # zero-order extrapolation:
      do 215 m=1,meqn
         do 215 ibc=1,mbc
               q(mx+ibc,m) = q(mx,m)
  215       continue
      go to 299

  220 continue
c     # periodic:  
      do 225 m=1,meqn
         do 225 ibc=1,mbc
               q(mx+ibc,m) = q(ibc,m)
  225       continue
      go to 299

  230 continue
c     # solid wall (assumes 2'nd component is velocity or momentum in x):
      do 235 m=1,meqn
         do 235 ibc=1,mbc
               q(mx+ibc,m) = q(mx+1-ibc,m)
  235       continue
      do 236 ibc=1,mbc
            q(mx+ibc,2) = -q(mx+1-ibc,2)
  236    continue
      go to 299

  299 continue
c
      return
      end
c     ============================================
      subroutine setaux(maxmx,mbc,mx,xlower,dx,maux,aux)
c     ============================================
c
c     # set auxiliary arrays 
c     # dummy routine when no auxiliary arrays
c
c     
      implicit double precision (a-h,o-z)
      dimension aux(1-mbc:maxmx+mbc, *)
c
       return
       end
c
c
c
      subroutine claw1ez(maxmx,meqn,mwaves,mbc,maux,mwork,mthlim,
     &                   q,work,aux)
c
c     An easy-to-use clawpack driver routine for simple applications
c
c     Author: Randall J. LeVeque
c     Version of March, 1999 --  CLAWPACK Version 4.0
c
      implicit double precision (a-h,o-z)
      external bc1,rp1,src1

      dimension    q(1-mbc:maxmx+mbc, meqn)
      dimension  aux(1-mbc:maxmx+mbc, maux)
      dimension work(mwork)
      dimension mthlim(mwaves)
c
      dimension method(7),dtv(5),cflv(4),nv(2),mthbc(2)
      dimension tout(100)
      logical outt0
c
      open(55,file='claw1ez.data',status='old',form='formatted')
      open(10,file='fort.info',status='unknown',form='formatted')
      open(11,file='fort.nplot',status='unknown',form='formatted')
c
c
c     # Read the input in standard form from input.claw:
c     # See input.doc for description of each input variable

c     Number of grid cells:
      read(55,*) mx

c     i/o variables
      read(55,*) nout
      read(55,*) outstyle
        if (outstyle.eq.1) read(55,*) tfinal
        if (outstyle.eq.2) read(55,*) (tout(i), i=1,nout)
        if (outstyle.eq.3) read(55,*) nsteps


c     timestepping variables
      read(55,*) dtv(1)
      read(55,*) dtv(2)
      read(55,*) cflv(1)
      read(55,*) cflv(2)
      read(55,*) nv(1)
c

c     # input parameters for clawpack routines
      read(55,*) method(1)
      read(55,*) method(2)
      read(55,*) method(3)
      read(55,*) method(4)
      read(55,*) method(5)
      read(55,*) method(6)  
      read(55,*) method(7) 

      read(55,*) meqn1
      read(55,*) mwaves1
      read(55,*) (mthlim(mw), mw=1,mwaves)

c     # physical domain:
      read(55,*) t0
      read(55,*) xlower
      read(55,*) xupper
c
c     # boundary conditions:
      read(55,*) mbc1
      read(55,*) mthbc(1)
      read(55,*) mthbc(2)

      if (method(7) .ne. maux) then
         write(6,*) '*** ERROR ***  maux set wrong in input or driver'
         stop
         endif

      if (meqn1 .ne. meqn) then
	 write(6,*) '*** ERROR ***  meqn set wrong in input or driver'
	 stop
	 endif
      if (mwaves1 .ne. mwaves) then
	 write(6,*) '*** ERROR ***  mwaves set wrong in input or driver'
	 stop
	 endif
      if (mbc1 .ne. mbc) then
	 write(6,*) '*** ERROR ***  mbc set wrong in input or driver'
	 stop
	 endif
c
      if ((mthbc(1).eq.2 .and. mthbc(2).ne.2) .or.
     &    (mthbc(2).eq.2 .and. mthbc(1).ne.2)) then
	 write(6,*) '*** ERROR ***  periodic boundary conditions require'
	 write(6,*) '  mthbc(1) and mthbc(2) BOTH be set to 2'
	 stop 
	 endif

c
c     # check that enough storage has been allocated:
c

      mwork1 = (maxmx + 2*mbc) * (2 + 4*meqn + mwaves + meqn*mwaves)
c
      if (mx.gt.maxmx .or. mwork.lt.mwork1) then
c        # insufficient storage
         maxmx1 = max0(mx,maxmx)

         mwork1 = (maxmx1 + 2*mbc) * (2 + 4*meqn + mwaves + meqn*mwaves)

         write(6,*) ' '
         write(6,*) '*** ERROR *** Insufficient storage allocated'
         write(6,*) 'Recompile after increasing values in driver.f:'
         write(6,611) maxmx1
         write(6,613) mwork1
 611     format(/,'parameter (maxmx = ',i5,')')
 613     format('parameter (mwork = ',i7,')',/)
         stop
         endif

c
c
      write(6,*) 'running...'
      write(6,*) ' '
c
c     # grid spacing
      dx = (xupper - xlower) / float(mx)
c

c     # time increments between outputing solution:
      if (outstyle .eq. 1) then
         dtout = (tfinal - t0)/float(nout)
         endif
c
      write(11,1101) nout
c
 1101 format(i5)
c        
c     # call user's routine setprob to set any specific parameters
c     # or other initialization required.
c
      call setprob
c        
c     # set aux array:
c
      if (maux .gt. 0)  then
         call setaux(maxmx,mbc,mx,xlower,dx,maux,aux)
         endif
c
c     # set initial conditions:
c
      call qinit(maxmx,meqn,mbc,mx,xlower,dx,q,maux,aux)
c
      outt0 = .true.
      if (outt0) then
c        # output initial data
         call out1(maxmx,meqn,mbc,mx,xlower,dx,q,t0,0)
         endif

c
c     ----------
c     Main loop:
c     ----------
c
      tend = t0
      do 100 n=1,nout
         tstart = tend
         if (outstyle .eq. 2) then
              tend = tout(n)
            else
              tend = tstart + dtout
            endif
c
         call claw1(maxmx,meqn,mwaves,mbc,mx,
     &		 q,aux,xlower,dx,tstart,tend,dtv,cflv,nv,method,mthlim,
     &           mthbc,work,mwork,info,bc1,rp1,src1)
c
c        # check to see if an error occured:
         if (info .ne. 0) then
            write(6,*) '*** ERROR in claw1 ***  info =',info
	    if (info.eq.1) then
	       write(6,*) '***   either mx > maxmx or mbc < 2'
	       endif
	    if (info.eq.2) then
	       write(6,*) '***   dt does not divide (tend - tstart)'
	       write(6,*) '***   and dt is fixed since method(1)=0'
	       endif
	    if (info.eq.3) then
	       write(6,*) '***   method(1)=1 and cflv(2) > cflv(1)'
	       endif
	    if (info.eq.4) then
	       write(6,*) '***   mwork is too small'
	       endif
	    if (info.eq.11) then
	       write(6,*) '***   Too many times steps, n > nv(1)'
	       endif
	    if (info.eq.12) then
	       write(6,*) '***   The Courant number is greater than cflv(1)'
	       write(6,*) '***   and dt is fixed since method(1)=0'
	       endif

            go to 999
            endif
c
         dtv(1) = dtv(5)  !# use final dt as starting value on next call
c
c        # output solution at this time
c        ------------------------------
         do 50 i=1,mx
               if (dabs(q(i,1)) .lt. 1d-90) q(i,1) = 0.d0
   50          continue
c
         call out1(maxmx,meqn,mbc,mx,xlower,dx,q,tend,n)
c
c        # write out information about this call to claw:
c
         write(6,601) nv(2),tend,info
  601    format('CLAW1 returning after ',i3,' steps,  t =',e13.5,
     &         '  info =',i3,/)
c
         write(10,1010) tend,info,dtv(3),dtv(4),dtv(5),
     &           cflv(3),cflv(4),nv(2)
 1010    format('tend =',d15.4,/,
     &       'info =',i5,/,'smallest dt =',d15.4,/,'largest dt =',
     &       d15.4,/,'last dt =',d15.4,/,'largest cfl =',
     &         d15.4,/,'last cfl =',d15.4,/,'steps taken =',i4,/)
c
  100    continue
c
  999 continue
c
      return 
      end
c
c
c =========================================================
      subroutine out1(maxmx,meqn,mbc,mx,xlower,dx,q,t,iframe)
c =========================================================
c
c     # Output the results for a general system of conservation laws
c     # in 1 dimension
c
      implicit double precision (a-h,o-z)
      dimension q(1-mbc:maxmx+mbc, meqn)
      character*10 fname1, fname2
c
c     # Write the results to the file fort.q<iframe>
c     # Use format required by matlab script  plotclaw1.m
c     # The same format is used by the amrclaw package.  
c     # Here it's adapted to output just the single grid.
c
c     # first create the file name and open file
c
         fname1 = 'fort.qxxxx'
         fname2 = 'fort.txxxx'
         nstp = iframe
         do 55 ipos = 10, 7, -1
            idigit = mod(nstp,10)
            fname1(ipos:ipos) = char(ichar('0') + idigit)
            fname2(ipos:ipos) = char(ichar('0') + idigit)
            nstp = nstp / 10
 55      continue

         open(unit=50,file=fname1,status='unknown',form='formatted')
         open(unit=60,file=fname2,status='unknown',form='formatted')

c
c     # the following parameters are used in amrclaw where there are
c     # multiple grids.  Here they are all set to 1:
      ngrids = 1
      mptr = 1
      level = 1

      write(50,1001) mptr,level,mx
 1001 format(i5,'                 grid_number',/,
     &       i5,'                 AMR_level',/,
     &       i5,'                 mx')

      write(50,1002) xlower,dx
 1002 format(e18.8,'    xlow', /,
     &       e18.8,'    dx', /)
c
        do 10 i=1,mx
          do m=1,meqn
c            # exponents with more than 2 digits cause problems reading
c            # into matlab... reset tiny values to zero:
             if (dabs(q(i,m)) .lt. 1d-99) q(i,m) = 0.d0
             enddo
c
          write(50,1005) (q(i,m), m=1,meqn)
 1005     format(4e16.8)
c
 10       continue
        write(50,*) ' '
 20     continue
      write(50,*) ' '

      write(60,1000) t,meqn,ngrids
 1000 format(e18.8,'    time', /, 
     &       i5,'                 meqn'/,
     &       i5,'                 ngrids'/,/)
c
      close(50)
      close(60)

      return
      end
c
c
c
c     ==============================================================
      subroutine claw1(maxmx,meqn,mwaves,mbc,mx,
     &		 q,aux,xlower,dx,tstart,tend,dtv,cflv,nv,method,mthlim,
     &           mthbc,work,mwork,info,bc1,rp1,src1)
c     ==============================================================
c
c  Solves a hyperbolic system of conservation laws in one space dimension
c  of the general form 
c
c     capa * q_t + A q_x = psi
c
c  The "capacity function" capa(x) and source term psi are optional
c  (see below).
c
c  For a more complete description see the documentation in the directory
c  claw/doc,  especially note1.ps and note2.ps.
c
c  Sample driver programs and user-supplied subroutines are available.
c  see the the directory claw/clawpack/1d/example for one example, and
c  codes in claw/applications for more extensive examples.
c
c  --------------------------------------------------------
c
c  The user must supply the following subroutines:
c
c    bc1, rp1        subroutines specifying the boundary conditions and 
c                    Riemann solver.
c                    These are described in greater detail below.
c
c  In addition, if the equation contains source terms psi, then the user
c  must provide:
c
c    src1               subroutine that solves capa * q_t = psi
c                       over a single time step.
c
c  These routines must be declared EXTERNAL in the main program.
c  For description of the calling sequences, see below.
c
c
c
c  Description of parameters...
c  ----------------------------
c
c
c    maxmx is the maximum number of interior grid points in x, 
c          and is used in declarations of the array q.
c
c    meqn is the number of equations in the system of
c         conservation laws.
c
c    mwaves is the number of waves that result from the
c           solution of each Riemann problem.  Often mwaves = meqn but
c           for some problems these may be different.
c
c    mbc is the number of "ghost cells" that must be added on to each
c       side of the domain to handle boundary conditions.  The cells
c       actually in the physical domain are labelled from 1 to mx in x.
c       The arrays are dimensioned actually indexed from 1-mbc to mx+mbc.
c       For the methods currently implemented, mbc = 2 should be used.
c       If the user implements another method that has a larger stencil and
c       hence requires more ghost cells, a larger value of mbc could be used.
c       q is extended from the physical domain to the ghost cells by the
c       user-supplied routine bc1.
c
c    mx is the number of grid cells in the x-direction, in the
c       physical domain.  In addition there are mbc grid cells
c       along each edge of the grid that are used for boundary
c       conditions.
c       Must have mx .le. maxmx
c 
c    q(1-mbc:maxmx+mbc, meqn) 
c        On input:  initial data at time tstart.
c        On output: final solution at time tend.
c        q(i,m) = value of mth component in the i'th cell.
c        Values within the physical domain are in q(i,m) 
c                for i = 1,2,...,mx
c        mbc extra cells on each end are needed for boundary conditions
c        as specified in the routine bc1.
c
c    aux(1-mbc:maxmx+mbc, maux)
c        Array of auxiliary variables that are used in specifying the problem.
c        If method(7) = 0 then there are no auxiliary variables and aux
c                         can be a dummy variable.
c        If method(7) = maux > 0 then there are maux auxiliary variables
c                         and aux must be dimensioned as above.
c
c        Capacity functions are one particular form of auxiliary variable.
c        These arise in some applications, e.g. variable coefficients in
c        advection or acoustics problems.
c        See Clawpack Note # 5 for examples.
c
c        If method(6) = 0 then there is no capacity function.
c        If method(6) = mcapa > 0  then there is a capacity function and
c            capa(i), the "capacity" of the i'th cell, is assumed to be
c            stored in aux(i,mcapa).
c            In this case we require method(7).ge.mcapa.
c
c    dx = grid spacing in x.  
c         (for a computation in ax <= x <= bx,  set dx = (bx-ax)/mx.)
c
c    tstart = initial time.
c
c    tend = Desired final time (on input).
c         = Actual time reached (on output).
c
c    dtv(1:5) = array of values related to the time step:
c               (Note: method(1)=1 indicates variable size time steps)
c         dtv(1) = value of dt to be used in all steps if method(1) = 0
c                = value of dt to use in first step if method(1) = 1
c         dtv(2) = unused if method(1) = 0.
c                = maximum dt allowed if method(1) = 1.
c         dtv(3) = smallest dt used (on output)
c         dtv(4) = largest dt used (on output)
c         dtv(5) = dt used in last step (on output)
c
c    cflv(1:4) = array of values related to Courant number:
c         cflv(1) = maximum Courant number to be allowed.  With variable
c                   time steps the step is repeated if the Courant
c                   number is larger than this value.  With fixed time
c                   steps the routine aborts.  Usually cflv(1)=1.0
c                   should work.
c         cflv(2) = unused if method(1) = 0.
c                 = desired Courant number if method(1) = 1.
c                   Should be somewhat less than cflv(1), e.g. 0.9
c         cflv(3) = largest Courant number observed (on output).
c         cflv(4) = Courant number in last step (on output).
c
c    nv(1:2) = array of values related to the number of time steps:
c         nv(1) = unused if method(1) = 0
c               = maximum number of time steps allowed if method(1) = 1
c         nv(2) = number of time steps taken (on output).
c
c    method(1:7) = array of values specifying the numerical method to use
c         method(1) = 0 if fixed size time steps are to be taken.
c                       In this case, dt = dtv(1) in all steps.
c                   = 1 if variable time steps are to be used.
c                       In this case, dt = dtv(1) in the first step and
c                       thereafter the value cflv(2) is used to choose the
c                       next time step based on the maximum wave speed seen
c                       in the previous step.  Note that since this value
c                       comes from the previous step, the Courant number will
c                       not in general be exactly equal to the desired value
c                       If the actual Courant number in the next step is
c                       greater than 1, then this step is redone with a 
c                       smaller dt.
c
c         method(2) = 1 if Godunov's method is to be used, with no 2nd order
c                       corrections.
c                   = 2 if second order correction terms are to be added, with
c                       a flux limiter as specified by mthlim.  
c                   = 3 if "third order" correction terms are to be added,
c                       based on my paper my paper "A high-resolution
c                       conservative algorithm for advection in
c                       incompressible flow".
c                       This is currently recommended only for problems
c                       with smooth solutions, using no limiter (mthlim = 0)
c
c         method(3)  is not used in one-dimension.
c
c         method(4) = 0 to suppress printing
c                   = 1 to print dt and Courant number every time step
c
c         method(5) = 0 if there is no source term psi.  In this case
c                       the subroutine src1 is never called so a dummy
c                       parameter can be given.
c                   = 1 if there is a source term.  In this case
c                       the subroutine src1 must be provided and a
c                       fractional step method is used.
c                       In each time step the following sequence is followed:
c                            call bc to extend data to ghost cells
c                            call step1 to advance hyperbolic eqn by dt
c                            call src1 to advance source terms by dt
c                   = 2 if there is a source term and Strang splitting is to
c                       be used instead of the Godunov splitting above.
c                       In each time step the following sequence is followed:
c                            call bc to extend data to ghost cells
c                            call src1 to advance source terms by dt/2
c                            call step1 to advance hyperbolic equation by dt
c                            call src1 to advance source terms by dt/2
c                       For most problems 1 is recommended rather than 2
c                       since it is less expensive and works essentially as
c                       well on most problems.

c
c         method(6) = 0 if there is no capacity function capa.
c                   = mcapa > 0 if there is a capacity function.  In this case
c                       aux(i,mcapa) is the capacity of the i'th cell and you
c                       must also specify method(7) .ge. mcapa and set aux.
c
c         method(7) = 0 if there is no aux array used.
c                   = maux > 0  if there are maux auxiliary variables.
c
c
c         The recommended choice of methods for most problems is
c            method(1) = 1,  method(2) = 2.
c
c    mthlim(1:mwaves) = array of values specifying the flux limiter to be used
c                     in each wave family mw.  Often the same value will be used
c                     for each value of mw, but in some cases it may be
c                     desirable to use different limiters.  For example,
c                     for the Euler equations the superbee limiter might be
c                     used for the contact discontinuity (mw=2) while another
c                     limiter is used for the nonlinear waves.  Several limiters
c                     are built in and others can be added by modifying the
c                     subroutine philim.
c
c        mthlim(mw) = 0 for no limiter
c                   = 1 for minmod
c                   = 2 for superbee
c                   = 3 for van Leer
c                   = 4 for monotonized centered
c
c
c    work(mwork) = double precision work array of length at least mwork
c
c    mwork = length of work array.  Must be at least
c               (maxmx + 2*mbc) * (2 + 4*meqn + mwaves + meqn*mwaves)
c            If mwork is too small then the program returns with info = 4
c            and prints the necessary value of mwork to unit 6.
c
c            
c    info = output value yielding error information:
c         = 0 if normal return.
c         = 1 if mx.gt.maxmx   or  mbc.lt.2
c         = 2 if method(1)=0 and dt doesn't divide (tend - tstart).
c         = 3 if method(1)=1 and cflv(2) > cflv(1).
c         = 4 if mwork is too small.
c         = 11 if the code attempted to take too many time steps, n > nv(1).
c              This could only happen if method(1) = 1 (variable time steps).
c         = 12 if the method(1)=0 and the Courant number is greater than 1
c              in some time step.
c
c           Note: if info.ne.0, then tend is reset to the value of t actually
c           reached and q contains the value of the solution at this time.
c
c    User-supplied subroutines
c    -------------------------
c
c    bc1 = subroutine that specifies the boundary conditions.  
c         This subroutine should extend the values of q from cells
c         1:mx to the mbc ghost cells along each edge of the domain.
c
c          The form of this subroutine is
c  -------------------------------------------------
c     subroutine bc1(maxmx,meqn,mbc,mx,q,aux,t)
c     implicit double precision (a-h,o-z)
c     dimension   q(1-mbc:maxmx+mbc, meqn)
c     dimension aux(1-mbc:maxmx+mbc, *)
c  -------------------------------------------------
c
c
c    rp1 = user-supplied subroutine that implements the Riemann solver
c
c          The form of this subroutine is
c  -------------------------------------------------
c     subroutine rp1(maxmx,meqn,mwaves,mbc,mx,ql,qr,auxl,auxr,wave,s,amdq,apdq)
c     implicit double precision (a-h,o-z)
c     dimension   ql(1-mbc:maxmx+mbc, meqn)
c     dimension   qr(1-mbc:maxmx+mbc, meqn)
c     dimension auxl(1-mbc:maxmx+mbc, *)
c     dimension auxr(1-mbc:maxmx+mbc, *)
c     dimension wave(1-mbc:maxmx+mbc, meqn, mwaves)
c     dimension    s(1-mbc:maxmx+mbc, mwaves)
c     dimension amdq(1-mbc:maxmx+mbc, meqn)
c     dimension apdq(1-mbc:maxmx+mbc, meqn)
c  -------------------------------------------------
c
c         On input, ql contains the state vector at the left edge of each cell
c                   qr contains the state vector at the right edge of each cell
c                 auxl contains auxiliary values at the left edge of each cell
c                 auxr contains auxiliary values at the right edge of each cell
c
c         Note that the i'th Riemann problem has left state qr(i-1,:)
c                                            and right state ql(i,:)
c         In the standard clawpack routines, this Riemann solver is
c         called with ql=qr=q along this slice.  More flexibility is allowed
c         in case the user wishes to implement another solution method
c         that requires left and rate states at each interface.

c         If method(7)=maux > 0 then the auxiliary variables along this slice
c         are passed in using auxl and auxr.  Again, in the standard routines
c         auxl=auxr=aux in the call to rp1.
c
c          On output, 
c              wave(i,m,mw) is the m'th component of the jump across
c                              wave number mw in the ith Riemann problem.
c              s(i,mw) is the wave speed of wave number mw in the
c                              ith Riemann problem.
c              amdq(i,m) = m'th component of A^- Delta q,
c              apdq(i,m) = m'th component of A^+ Delta q,
c                     the decomposition of the flux difference
c                         f(qr(i-1)) - f(ql(i))
c                     into leftgoing and rightgoing parts respectively.
c
c           It is assumed that each wave consists of a jump discontinuity
c           propagating at a single speed, as results, for example, from a
c           Roe approximate Riemann solver.  An entropy fix can be included
c           into the specification of amdq and apdq.
c
c    src1 = subroutine for the source terms that solves the equation
c               capa * q_t = psi 
c           over time dt.
c
c           If method(5)=0 then the equation does not contain a source
c           term and this routine is never called.  A dummy argument can
c           be used with many compilers, or provide a dummy subroutine that
c           does nothing (such a subroutine can be found in
c           clawpack/1d/misc/src1xx.f)
c
c          The form of this subroutine is
c  -------------------------------------------------
c     subroutine src1(maxmx,meqn,mbc,mx,q,aux,t,dt)
c     implicit double precision (a-h,o-z)
c     dimension   q(1-mbc:maxmx+mbc, meqn)
c     dimension aux(1-mbc:maxmx+mbc, *)
c  -------------------------------------------------
c      If method(7)=0  or the auxiliary variables are not needed in this solver,
c      then the latter dimension statement can be omitted, but aux should
c      still appear in the argument list.
c
c      On input, q(i,m) contains the data for solving the
c                source term equation.
c      On output, q(i,m) should have been replaced by the solution to
c                 the source term equation after a step of length dt.

c
c
c  NOTES:
c  ------
c
c  -- This code is written for clarity rather than efficiency in many
c     respects.
c
c  -- Most of this routine is concerned with checking for errors and 
c     choosing time steps.  The main work of each time step is done in step1.
c
c  -- Strang splitting is used to handle the source term.  This works well
c     and can be used to achieve second order accuracy on smooth solutions
c     provided that the source terms are not "stiff".  If they are, meaning
c     that the time scale on which solutions to the ODE vary are much faster
c     than the time scales of the homogeneous conservation law, then to
c     obtain good results it may be necessary to take dx and dt small enough
c     that the rapid transients are well-resolved.
c
c  -- Note that with variable time steps, the value dtv(2)
c     may be used to limit the time step to some maximum value, independent
c     of the Courant number.  This may be chosen based on the source terms.
c
c  
c
c =========================================================================
c
c  Copyright 1994 R. J. LeVeque
c
c  This software is made available for research and instructional use only. 
c  You may copy and use this software without charge for these non-commercial
c  purposes, provided that the copyright notice and associated text is
c  reproduced on all copies.  For all other uses (including distribution of
c  modified versions), please contact the author at the address given below. 
c  
c  *** This software is made available "as is" without any assurance that it
c  *** will work for your purposes.  The software may in fact have defects, so
c  *** use the software at your own risk.
c
c  --------------------------------------
c    CLAWPACK Version 2.1,  October, 1995
c    available from netlib@research.att.com in pdes/claw
c    Homepage: http://www.amath.washington.edu/~rjl/clawpack.html
c  --------------------------------------
c    Author:  Randall J. LeVeque
c             Applied Mathematics
c             Box 352420
c             University of Washington, 
c             Seattle, WA 98195-2420
c             rjl@amath.washington.edu
c =========================================================================
c
c
c
c            
c    ======================================================================
c    Beginning of claw1 code
c    ======================================================================
c 
      implicit double precision (a-h,o-z)
      dimension q(1-mbc:maxmx+mbc, meqn)
      dimension aux(1-mbc:maxmx+mbc, *)
      dimension work(mwork)
      dimension mthlim(mwaves),method(7),dtv(5),cflv(4),nv(2)
      dimension mthbc(2)
c
c
      info = 0
      t = tstart
      maxn = nv(1)
      dt = dtv(1)   !# initial dt
      cflmax = 0.d0
      dtmin = dt
      dtmax = dt
      nv(2) = 0
      maux = method(7)
c
c     # check for errors in data:
c
      if (mx .gt. maxmx) then
	 info = 1
	 go to 900
	 endif
c
      if (method(1) .eq. 0) then
c        # fixed size time steps.  Compute the number of steps:
	 maxn = (tend - tstart + 1d-10) / dt
	 if (dabs(maxn*dt - (tend-tstart)) .gt. 1d-8) then
c           # dt doesn't divide time interval integer number of times
	    info = 2
	    go to 900
	    endif
	 endif
c
      if (method(1).eq.1 .and. cflv(1).gt.1.d0) then
	 info = 3
	 go to 900
	 endif
c
c     # partition work array into pieces for passing into step1:
      i0f = 1
      i0wave = i0f + (maxmx + 2*mbc) * meqn
      i0s = i0wave + (maxmx + 2*mbc) * meqn * mwaves
      i0dtdx = i0s + (maxmx + 2*mbc) * mwaves
      i0qwork = i0dtdx + (maxmx + 2*mbc) 
      i0amdq = i0qwork + (maxmx + 2*mbc) * meqn
      i0apdq = i0amdq + (maxmx + 2*mbc) * meqn
      i0dtdx = i0apdq + (maxmx + 2*mbc) * meqn
      i0end = i0dtdx + (maxmx + 2*mbc) - 1
c
      if (mwork .lt. i0end) then
	 write(6,*) 'mwork must be increased to ',i0end
	 info = 4
	 go to 900
	 endif
c
c     -----------
c     # main loop
c     -----------
c
      if (maxn.eq.0) go to 900
      do 100 n=1,maxn
	 told = t
	 if (told+dt .gt. tend) dt = tend - told
	 if (method(1).eq.1) then
c           # save old q in case we need to retake step with smaller dt:
	    call copyq1(maxmx,meqn,mbc,mx,q,work(i0qwork))
	    endif
c	    
   40    continue
	 dt2 = dt / 2.d0
	 thalf = t + dt2  !# midpoint in time for Strang splitting
	 t = told + dt    !# time at end of step
c
c        ------------------------------------------------------------------
c        # main steps in algorithm:
c        ------------------------------------------------------------------
c
c        # extend data from grid to bordering boundary cells:
         call bc1(maxmx,meqn,mbc,mx,xlower,dx,q,maux,aux,told,mthbc)
c
	 if (method(5).eq.2) then
c            # with Strang splitting for source term:
 	     call src1(maxmx,meqn,mbc,mx,q,aux,told,dt2)
	     endif
c
c        # take a step on the homogeneous conservation law:
         call step1(maxmx,meqn,mwaves,mbc,mx,q,aux,dx,dt,
     &             method,mthlim,cfl,work(i0f),work(i0wave),
     &             work(i0s),work(i0amdq),work(i0apdq),work(i0dtdx),
     &             rp1)
c
	 if (method(5).eq.2) then
c            # source terms over a second half time step for Strang splitting:
c            # Note it is not so clear what time t should be used here if
c            # the source terms are time-dependent!
 	     call src1(maxmx,meqn,mbc,mx,q,aux,thalf,dt2)
	     endif

         if (method(5).eq.1) then
c            # source terms over a full time step:
 	     call src1(maxmx,meqn,mbc,mx,q,aux,t,dt)
             endif
c

c
c        ------------------------------------------------------------------
c
         if (method(4) .eq. 1) write(6,601) n,cfl,dt,t
  601    format('CLAW1... Step',i4,
     &                   '   Courant number =',f6.3,'  dt =',d12.4,
     &                   '  t =',d12.4)
c
         if (method(1) .eq. 1) then
c           # choose new time step if variable time step
            if (cfl .gt. 0.d0) then
		dt = dmin1(dtv(2), dt * cflv(2)/cfl)
		dtmin = dmin1(dt,dtmin)
		dtmax = dmax1(dt,dtmax)
	      else
		dt = dtv(2)
	      endif
	    endif
c
c        # check to see if the Courant number was too large:
c
	 if (cfl .le. cflv(1)) then
c               # accept this step
	        cflmax = dmax1(cfl,cflmax)
	      else
c               # reject this step
		t = told
	        call copyq1(maxmx,meqn,mbc,mx,work(i0qwork),q)
c
		if (method(4) .eq. 1) then
		   write(6,602) 
  602		   format('CLAW1 rejecting step... ',
     &			       'Courant number too large')
		   endif
	        if (method(1).eq.1) then
c                   # if variable dt, go back and take a smaller step
		    go to 40
	          else
c                   # if fixed dt, give up and return
	            cflmax = dmax1(cfl,cflmax)
	            go to 900
	          endif
	       endif
c
c        # see if we are done:
	 nv(2) = nv(2) + 1
	 if (t .ge. tend) go to 900
c
  100    continue
c
  900  continue
c 
c      # return information
c
       if (method(1).eq.1 .and. t.lt.tend .and. nv(2) .eq. maxn) then
c         # too many timesteps
	  info = 11
	  endif
c
       if (method(1).eq.0 .and. cflmax .gt. cflv(1)) then
c         # Courant number too large with fixed dt
	  info = 12
	  endif
       tend = t
       cflv(3) = cflmax
       cflv(4) = cfl
       dtv(3) = dtmin
       dtv(4) = dtmax
       dtv(5) = dt
       return 
       end
c
c
c ===================================================================
      subroutine step1(maxmx,meqn,mwaves,mbc,mx,q,aux,dx,dt,
     &              method,mthlim,cfl,f,wave,s,amdq,apdq,dtdx,rp1)
c ===================================================================
c
c     # Take one time step, updating q.
c
c     method(1) = 1   ==>  Godunov method
c     method(1) = 2   ==>  Slope limiter method
c     mthlim(p)  controls what limiter is used in the pth family
c
c
c     amdq, apdq, wave, s, and f are used locally:
c
c     amdq(1-mbc:maxmx+mbc, meqn) = left-going flux-differences
c     apdq(1-mbc:maxmx+mbc, meqn) = right-going flux-differences
c        e.g. amdq(i,m) = m'th component of A^- \Delta q from i'th Riemann
c                         problem (between cells i-1 and i).
c
c     wave(1-mbc:maxmx+mbc, meqn, mwaves) = waves from solution of
c                                           Riemann problems,
c            wave(i,m,mw) = mth component of jump in q across
c                           wave in family mw in Riemann problem between
c                           states i-1 and i.
c
c     s(1-mbc:maxmx+mbc, mwaves) = wave speeds,
c            s(i,mw) = speed of wave in family mw in Riemann problem between
c                      states i-1 and i.
c
c     f(1-mbc:maxmx+mbc, meqn) = correction fluxes for second order method
c            f(i,m) = mth component of flux at left edge of ith cell 
c     --------------------------------------------------------------------
c
      implicit double precision (a-h,o-z)
      dimension    q(1-mbc:maxmx+mbc, meqn)
      dimension  aux(1-mbc:maxmx+mbc, *)
      dimension    f(1-mbc:maxmx+mbc, meqn)
      dimension    s(1-mbc:maxmx+mbc, mwaves)
      dimension wave(1-mbc:maxmx+mbc, meqn, mwaves)
      dimension amdq(1-mbc:maxmx+mbc, meqn)
      dimension apdq(1-mbc:maxmx+mbc, meqn)
      dimension dtdx(1-mbc:maxmx+mbc)
      dimension method(7),mthlim(mwaves)
      logical limit
c
c     # check if any limiters are used:
      limit = .false.
      do 5 mw=1,mwaves
	 if (mthlim(mw) .gt. 0) limit = .true.
   5     continue
c
      mcapa = method(6)
      do 10 i=1-mbc,mx+mbc
	 if (mcapa.gt.0) then
	     if (aux(i,mcapa) .le. 0.d0) then
		write(6,*) 'Error -- capa must be positive'
		stop
		endif
             dtdx(i) = dt / (dx*aux(i,mcapa))
	    else
             dtdx(i) = dt/dx
	    endif
   10	 continue
c
c
c
c     # solve Riemann problem at each interface 
c     -----------------------------------------
c
      call rp1(maxmx,meqn,mwaves,mbc,mx,q,q,aux,aux,wave,s,amdq,apdq)
c
c     # Modify q for Godunov update:
c     # Note this may not correspond to a conservative flux-differencing
c     # for equations not in conservation form.  It is conservative if
c     # amdq + apdq = f(q(i)) - f(q(i-1)).
c
      do 40 i=1,mx+1
         do 40 m=1,meqn
            q(i,m) = q(i,m) - dtdx(i)*apdq(i,m)
            q(i-1,m) = q(i-1,m) - dtdx(i-1)*amdq(i,m)
   40       continue

c
c     # compute maximum wave speed:
      cfl = 0.d0
      do 50 mw=1,mwaves
	 do 45 i=1,mx+1
c          # if s>0 use dtdx(i) to compute CFL,
c          # if s<0 use dtdx(i-1) to compute CFL:
	   cfl = dmax1(cfl, dtdx(i)*s(i,mw), -dtdx(i-1)*s(i,mw))
   45      continue
   50    continue
c
      if (method(2) .eq. 1) go to 900
c
c     # compute correction fluxes for second order q_{xx} terms:
c     ----------------------------------------------------------
c
      do 100 m = 1, meqn
            do 100 i = 1-mbc, mx+mbc
               f(i,m) = 0.d0
  100          continue
c
c      # apply limiter to waves:
      if (limit) call limiter(maxmx,meqn,mwaves,mbc,mx,wave,s,mthlim)
c
      do 120 i=1,mx+1
	 do 120 m=1,meqn
	    do 110 mw=1,mwaves
	       dtdxave = 0.5d0 * (dtdx(i-1) + dtdx(i))
	       f(i,m) = f(i,m) + 0.5d0 * dabs(s(i,mw))
     &		   * (1.d0 - dabs(s(i,mw))*dtdxave) * wave(i,m,mw)
c
c              # third order corrections:
c              # (still experimental... works well for smooth solutions
c              # with no limiters but not well with limiters so far.
c

               if (method(2).lt.3) go to 110
               if (s(i,mw) .gt. 0.d0) then
                   dq2 = wave(i,m,mw) - wave(i-1,m,mw)
                 else
                   dq2 = wave(i+1,m,mw) - wave(i,m,mw)
                 endif
               f(i,m) = f(i,m) - s(i,mw)/6.d0 *
     &                     (1.d0 - (s(i,mw)*dtdxave)**2) * dq2

  110          continue
  120       continue
c
c
  140 continue
c
c     # update q by differencing correction fluxes 
c     ============================================
c
c     # (Note:  Godunov update has already been performed above)
c
      do 150 m=1,meqn
	 do 150 i=1,mx
	    q(i,m) = q(i,m) - dtdx(i) * (f(i+1,m) - f(i,m))
  150       continue
c
  900 continue
      return
      end
c
c
c =========================================================
      subroutine copyq1(maxmx,meqn,mbc,mx,q1,q2)
c =========================================================
c
c     # copy the contents of q1 into q2
c
      implicit double precision (a-h,o-z)
      dimension q1(1-mbc:maxmx+mbc, meqn)
      dimension q2(1-mbc:maxmx+mbc, meqn)
c
      do 10 i = 1-mbc, mx+mbc
          do 10 m=1,meqn
	     q2(i,m) = q1(i,m)
   10        continue
      return
      end
c
c
c     =====================================================
      subroutine limiter(maxm,meqn,mwaves,mbc,mx,wave,s,mthlim)
c     =====================================================
c
c     # Apply a limiter to the waves.
c     # The limiter is computed by comparing the 2-norm of each wave with
c     # the projection of the wave from the interface to the left or
c     # right onto the current wave.  For a linear system this would
c     # correspond to comparing the norms of the two waves.  For a 
c     # nonlinear problem the eigenvectors are not colinear and so the 
c     # projection is needed to provide more limiting in the case where the
c     # neighboring wave has large norm but points in a different direction
c     # in phase space.
c
c     # The specific limiter used in each family is determined by the
c     # value of the corresponding element of the array mthlim, as used in
c     # the function philim.
c     # Note that a different limiter may be used in each wave family.
c
c     # dotl and dotr denote the inner product of wave with the wave to
c     # the left or right.  The norm of the projections onto the wave are then
c     # given by dotl/wnorm2 and dotr/wnorm2, where wnorm2 is the 2-norm
c     # of wave.
c
      implicit real*8(a-h,o-z)
      dimension mthlim(mwaves)
      dimension wave(1-mbc:maxm+mbc, meqn, mwaves)
      dimension    s(1-mbc:maxm+mbc, mwaves)
c
c
      do 50 mw=1,mwaves
         if (mthlim(mw) .eq. 0) go to 50
         dotr = 0.d0
         do 40 i = 0, mx+1
            wnorm2 = 0.d0
            dotl = dotr
            dotr = 0.d0
            do 20 m=1,meqn
               wnorm2 = wnorm2 + wave(i,m,mw)**2
               dotr = dotr + wave(i,m,mw)*wave(i+1,m,mw)
   20          continue
            if (i.eq.0) go to 40
            if (wnorm2.eq.0.d0) go to 40
c
            if (s(i,mw) .gt. 0.d0) then
                wlimitr = philim(wnorm2, dotl, mthlim(mw))
              else
                wlimitr = philim(wnorm2, dotr, mthlim(mw))
              endif
c
            do 30 m=1,meqn
               wave(i,m,mw) = wlimitr * wave(i,m,mw)
   30          continue
   40       continue
   50    continue
c
      return
      end
c
c
c     =====================================================
      double precision function philim(a,b,meth)
c     =====================================================
      implicit real*8(a-h,o-z)
c
c     # Compute a limiter based on wave strengths a and b.
c     # meth determines what limiter is used.
c     # a is assumed to be nonzero.
c
      r = b/a
      go to (10,20,30,40,50) meth

c
   10 continue
c     --------
c     # minmod
c     --------
      philim = dmax1(0.d0, dmin1(1.d0, r))
      return
c
   20 continue
c     ----------
c     # superbee
c     ----------
      philim = dmax1(0.d0, dmin1(1.d0, 2.d0*r), dmin1(2.d0, r))
      return
c
   30 continue
c     ----------
c     # van Leer
c     ----------
      philim = (r + dabs(r)) / (1.d0 + dabs(r))
      return
c
   40 continue
c     ------------------------------
c     # monotinized centered 
c     ------------------------------
      c = (1.d0 + r)/2.d0
      philim = dmax1(0.d0, dmin1(c, 2.d0, 2.d0*r))
      return
c
   50 continue
c     ------------------------------
c     # Beam-Warming
c     ------------------------------
      philim = r

      return
      end
