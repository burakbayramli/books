c
c
c =========================================================
      subroutine rp1(maxmx,meqn,mwaves,mbc,mx,ql,qr,auxl,auxr,
     &		 wave,s,amdq,apdq)
c =========================================================
c
c     # solve Riemann problems for the 1D advection equation q_t + (u*q)_x = 0.
c
c       -----------------------------------------------------------
c     # In conservation form, with interface velocities specified in
c     # the auxiliary variable
c     # aux(i,1)  =  u-velocity at left edge of cell i
c       -----------------------------------------------------------
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
      dimension auxl(1-mbc:maxmx+mbc, 1)
      dimension auxr(1-mbc:maxmx+mbc, 1)
      dimension    s(1-mbc:maxmx+mbc, mwaves)
      dimension wave(1-mbc:maxmx+mbc, meqn, mwaves)
      dimension amdq(1-mbc:maxmx+mbc, meqn)
      dimension apdq(1-mbc:maxmx+mbc, meqn)
c
c
c     # set fim1 = f(q_{i-1}) at left boundary for flux differencing below:
      i = 1-mbc
      fim1 = 0.5d0*(qr(i,1) + ql(i,1)) *
     &            (dmin1(auxl(i+1,1),0.d0)
     &             + dmax1(auxl(i,1),0.d0))
c
c
c
      do 30 i=2-mbc,mx+mbc
c
c        # Compute the wave and speed
c
	 u = auxl(i,1)
         wave(i,1,1) = ql(i,1) - qr(i-1,1)
         s(i,1) = u
c
c
c        # conservative form
c        -------------------
c        # amdq and apdq are chosen as flux differences for the
c        # conservative equation  q_t + (u*q)_x = 0
c
c        # compute the flux at the interface between cells i-1 and i:
         if (u.gt.0.d0) then
               f0 = u*qr(i-1,1)
             else
               f0 = u*ql(i,1)
             endif
c
c        # compute a value for the flux in cell i:
c        # note that we have velocities only at the interfaces
         fi = 0.5d0*(ql(i,1) + qr(i,1)) *
     &             (dmin1(auxl(i+1,1),0.d0) + dmax1(u,0.d0))
c
c        # flux differences:
         amdq(i,1) = f0 - fim1
         apdq(i,1) = fi - f0
c
         fim1 = fi

   30   continue
c
      return
      end
