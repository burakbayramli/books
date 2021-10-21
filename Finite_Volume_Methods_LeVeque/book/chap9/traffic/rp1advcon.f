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
c     # In conservation form, with cell-centered velocities specified in
c     # the auxiliary variable
c     # aux(i,1)  =  u-velocity in cell i
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
c
      do 30 i=2-mbc,mx+mbc
c
	 ui = auxl(i,1)
	 uim = auxr(i-1,1)
	 qi = ql(i,1)
	 qim = qr(i-1,1)
c
	 if (ui .gt. 0.d0) then
	     qstar = uim*qim/ui
	     wave(i,1,1) = qi - qstar
	     s(i,1) = ui
	     amdq(i,1) = 0.d0
	     apdq(i,1) = ui*qi - uim*qim
	  else
	     qstar = ui*qi/uim
	     wave(i,1,1) = qstar - qim
	     s(i,1) = uim
	     amdq(i,1) = ui*qi - uim*qim
	     apdq(i,1) = 0.d0
          endif

   30   continue
c
      return
      end
