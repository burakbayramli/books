c =========================================================
      subroutine rp1(maxmx,meqn,mwaves,mbc,mx,ql,qr,auxl,auxr,
     &		 wave,s,amdq,apdq)
c =========================================================
c
c     # solve Riemann problems for the 1D Buckley-Leverett equation.
c
c     # On input, ql contains the state vector at the left edge of each cell
c     #           qr contains the state vector at the right edge of each cell
c     # On output, wave contains the waves, 
c     #            s the speeds, 
c     #            amdq the  left-going flux difference  A^- \Delta q
c     #            apdq the right-going flux difference  A^+ \Delta q
c     # Note that the i'th Riemann problem has left state qr(i-1,:)
c     #                                    and right state ql(i,:)
c     # From the basic clawpack routine step1, rp is called with ql = qr = q.
c
      implicit double precision (a-h,o-z)
      dimension   ql(1-mbc:maxmx+mbc, meqn)
      dimension   qr(1-mbc:maxmx+mbc, meqn)
      dimension    s(1-mbc:maxmx+mbc, mwaves)
      dimension wave(1-mbc:maxmx+mbc, meqn, mwaves)
      dimension amdq(1-mbc:maxmx+mbc, meqn)
      dimension apdq(1-mbc:maxmx+mbc, meqn)   
      common /comprob/ a
      
      do 30 i=2-mbc,mx+mbc
        ur = ql(i,1)
        ul = qr(i-1,1)
        fr = ur**2/(ur**2+a*(1.d0-ur)**2)
        fl = ul**2/(ul**2+a*(1.d0-ul)**2)
c
	wave(i,1,1) = ur - ul
	if (ul.ne.ur) then
	   s(i,1) = (fr-fl)/(ur-ul)
	  else
	   s(i,1) = 0.d0
	  endif

c        # compute left-going and right-going flux differences:
c        ------------------------------------------------------
c
         amdq(i,1) = 0.d0
         apdq(i,1) = fr - fl
   30   continue
c
      return
      end
