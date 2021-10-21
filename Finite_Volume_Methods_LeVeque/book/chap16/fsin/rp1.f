c =========================================================
      subroutine rp1(maxmx,meqn,mwaves,mbc,mx,ql,qr,auxl,auxr,
     &		 wave,s,amdq,apdq)
c =========================================================
c
c     # solve Riemann problems for the nonconvex scalar conservation law
c     # with flux f(q) = sin(q).
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
      
      pi = 4.d0*datan(1.d0)

      do 30 i=2-mbc,mx+mbc
        ur = ql(i,1)
        ul = qr(i-1,1)
        fr = sin(ur)         
	fl = sin(ul)
c
c       # the flux fedge at the cell interface is obtained by minimizing
c       # or maximizing the function sin(u) over the interval between ul and ur
c       # Assume ul and ur lie between 0 and 4*pi
c
	if (ul .lt. ur)  then
	    fedge = dmin1(fl,fr)
	    if ((ul-1.5d0*pi)*(ur-1.5d0*pi) .lt. 0.d0) then
	        fedge = -1.d0
		endif
	    if ((ul-3.5d0*pi)*(ur-3.5d0*pi) .lt. 0.d0) then
	        fedge = -1.d0
		endif
	   else
	    fedge = dmax1(fl,fr)
	    if ((ul-0.5d0*pi)*(ur-0.5d0*pi) .lt. 0.d0) then
	        fedge = 1.d0
		endif
	    if ((ul-2.5d0*pi)*(ur-2.5d0*pi) .lt. 0.d0) then
	        fedge = 1.d0
		endif
	   endif

	wave(i,1,1) = ur - ul

	if (ul.ne.ur) then
	   s(i,1) = (fr-fl)/(ur-ul)
	  else
	   s(i,1) = 0.d0
	  endif

c        # compute left-going and right-going flux differences:
c        ------------------------------------------------------
c
         amdq(i,1) = fedge - fl
         apdq(i,1) = fr - fedge
   30   continue
c
      return
      end

