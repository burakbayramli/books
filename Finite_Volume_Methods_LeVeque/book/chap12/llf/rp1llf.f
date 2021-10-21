c
c
c =========================================================
      subroutine rp1(maxmx,meqn,mwaves,mbc,mx,ql,qr,auxl,auxr,
     &		       wave,s,amdq,apdq)
c =========================================================
c
c     # solve Riemann problems for the 1D Burgers' equation.
c     # using Lax-Friedrichs or Local LxF methods (see comment below)
c
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
      common /comlxf/ alxf
c
c
c
      do 30 i=2-mbc,mx+mbc
c
c        # Compute the wave and speed
c
         wave(i,1,1) = ql(i,1) - qr(i-1,1)
         s(i,1) = 0.5d0 * (qr(i-1,1) + ql(i,1))
c
c
c        # compute left-going and right-going flux differences:
c        ------------------------------------------------------
c
c        # local Lax-Friedrichs:
 	 alxf = dmax1(dabs(ql(i,1)), dabs(qr(i-1,1)))
c        # comment out the above line to get standard LxF,
c        # in which case alxf=dx/dt, as set in b4step1
	 
	 df = 0.5d0*(ql(i,1)**2 - qr(i-1,1)**2)
	 amdq(i,1) = 0.5d0*(df - alxf*(ql(i,1)-qr(i-1,1)))
	 apdq(i,1) = 0.5d0*(df + alxf*(ql(i,1)-qr(i-1,1)))
c
   30   continue
c
      return
      end
