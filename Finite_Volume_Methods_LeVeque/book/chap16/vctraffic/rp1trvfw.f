c
c
c =========================================================
      subroutine rp1(maxmx,meqn,mwaves,mbc,mx,ql,qr,auxl,auxr,
     &		 fwave,s,amdq,apdq)
c =========================================================
c
c     # solve Riemann problems for the traffic equation.
c     # with variable speed limit umax stored in aux(i,1)
c
c     # returns fwave's instead of waves
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
      dimension fwave(1-mbc:maxmx+mbc, meqn, mwaves)
      dimension amdq(1-mbc:maxmx+mbc, meqn)
      dimension apdq(1-mbc:maxmx+mbc, meqn)
      dimension  auxl(1-mbc:maxmx+mbc, 1)
      dimension  auxr(1-mbc:maxmx+mbc, 1)
      common /comlxf/ alxf
c
c
c
      do 30 i=2-mbc,mx+mbc
c
c        # Compute the fwave and speed, and fluctuations
c
c        # compute flux in each cell and flux difference:
         fim1 = auxl(i-1,1)*qr(i-1,1)*(1.d0 - qr(i-1,1))
         fi = auxl(i,1)*ql(i,1)*(1.d0 - ql(i,1))
         fwave(i,1,1) = fi - fim1

c        # compute characteristic speed in each cell:
	 sim1 = auxl(i-1,1)*(1.d0 - 2.d0*ql(i-1,1))
	 si = auxl(i,1)*(1.d0 - 2.d0*ql(i,1))

         if (sim1 .lt. 0.d0 .and. si .le. 0.d0) then
c             # left-going
              s(i,1) = sim1
	      amdq(i,1) = fwave(i,1,1)
	      apdq(i,1) = 0.d0
            else if (sim1 .ge. 0.d0 .and. si .gt. 0.d0) then
c             # right-going
              s(i,1) = si
	      amdq(i,1) = 0.d0
	      apdq(i,1) = fwave(i,1,1)
            else if (sim1 .lt. 0.d0 .and. si .gt. 0.d0) then
c             # transonic rarefaction
c             # split fwave between amdq and apdq:
              s(i,1) = 0.5d0*(sim1 + si)
              dq = ql(i,1) - qr(i-1,1)

c             # entropy fix:  (perhaps doesn't work for all cases!!!)
c             # This assumes the flux in the transonic case should
c             # correspond to q=0.5 on the side with the smaller umax value.
              f0 = dmin1(auxl(i-1,1),auxl(i,1))*0.25d0
              amdq(i,1) = f0 - fim1
              apdq(i,1) = fi - f0

            else
c             # transonic shock
              s(i,1) = 0.5d0*(sim1 + si)
              if (s(i,1) .lt. 0.d0) then 
                   amdq(i,1) = fwave(i,1,1)
                   apdq(i,1) = 0.d0
                else if (s(i,1) .gt. 0.d0) then 
                   amdq(i,1) = 0.d0
                   apdq(i,1) = fwave(i,1,1)
                else
	           amdq(i,1) = 0.5d0 * fwave(i,1,1) 
	           apdq(i,1) = 0.5d0 * fwave(i,1,1)
                endif
            endif
c
   30    continue
c
      return
      end
