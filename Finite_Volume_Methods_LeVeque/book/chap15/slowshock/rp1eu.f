c
c
c
c =========================================================
      subroutine rp1(maxmx,meqn,mwaves,mbc,mx,ql,qr,auxl,auxr,
     &		 wave,s,amdq,apdq)
c =========================================================
c
c     # solve Riemann problems for the 1D Euler equations using Roe's 
c     # approximate Riemann solver.  
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
c
c     # local storage
c     ---------------
      parameter (max2 = 4002)  !# assumes at most 4000 grid points with mbc=2
      dimension delta(3)
      dimension u(-1:max2),enth(-1:max2),a(-1:max2)
      logical efix
      common /param/  gamma,gamma1
c
      data efix /.true./    !# use entropy fix for transonic rarefactions
c
      if (mx+mbc .gt. max2) then
         write(6,*) '*** Error *** need to increase max2 in rp1eux'
         stop 
         endif
c
c
c     # Compute Roe-averaged quantities:
c
      do 20 i=2-mbc,mx+mbc
	 rhsqrtl = dsqrt(qr(i-1,1))
	 rhsqrtr = dsqrt(ql(i,1))
	 pl = gamma1*(qr(i-1,3) - 0.5d0*(qr(i-1,2)**2)/qr(i-1,1))
	 pr = gamma1*(ql(i,3) - 0.5d0*(ql(i,2)**2)/ql(i,1))
	 rhsq2 = rhsqrtl + rhsqrtr
	 u(i) = (qr(i-1,2)/rhsqrtl + ql(i,2)/rhsqrtr) / rhsq2
	 enth(i) = (((qr(i-1,3)+pl)/rhsqrtl
     &		   + (ql(i,3)+pr)/rhsqrtr)) / rhsq2
         a2 = gamma1*(enth(i) - .5d0*u(i)**2)
         a(i) = dsqrt(a2)
   20    continue
c
c
      do 30 i=2-mbc,mx+mbc
c
c        # find a1 thru a3, the coefficients of the 3 eigenvectors:
c
         delta(1) = ql(i,1) - qr(i-1,1)
         delta(2) = ql(i,2) - qr(i-1,2)
         delta(3) = ql(i,3) - qr(i-1,3)
         a2 = gamma1/a(i)**2 * ((enth(i)-u(i)**2)*delta(1) 
     &      + u(i)*delta(2) - delta(3))
         a3 = (delta(2) + (a(i)-u(i))*delta(1) - a(i)*a2) / (2.d0*a(i))
         a1 = delta(1) - a2 - a3
c
c        # Compute the waves.
c
         wave(i,1,1) = a1
         wave(i,2,1) = a1*(u(i)-a(i))
         wave(i,3,1) = a1*(enth(i) - u(i)*a(i))
         s(i,1) = u(i)-a(i)
c
         wave(i,1,2) = a2
         wave(i,2,2) = a2*u(i)
         wave(i,3,2) = a2*0.5d0*u(i)**2
         s(i,2) = u(i)
c
         wave(i,1,3) = a3
         wave(i,2,3) = a3*(u(i)+a(i))
         wave(i,3,3) = a3*(enth(i)+u(i)*a(i))
         s(i,3) = u(i)+a(i)
   30    continue
c
c     # compute Godunov flux f0:
c     --------------------------
c
c
      if (efix) go to 110
c
c     # no entropy fix
c     ----------------
c
c     # amdq = SUM s*wave   over left-going waves
c     # apdq = SUM s*wave   over right-going waves
c
      do 100 m=1,3
         do 100 i=2-mbc, mx+mbc
            amdq(i,m) = 0.d0
            apdq(i,m) = 0.d0
            do 90 mw=1,mwaves
               if (s(i,mw) .lt. 0.d0) then
                   amdq(i,m) = amdq(i,m) + s(i,mw)*wave(i,m,mw)
                 else
                   apdq(i,m) = apdq(i,m) + s(i,mw)*wave(i,m,mw)
                 endif
   90          continue
  100       continue
      go to 900
c
c-----------------------------------------------------
c
  110 continue
c
c     # With entropy fix
c     ------------------
c
c    # compute flux differences amdq and apdq.
c    # First compute amdq as sum of s*wave for left going waves.
c    # Incorporate entropy fix by adding a modified fraction of wave
c    # if s should change sign.
c
      do 200 i=2-mbc,mx+mbc
c
c        # check 1-wave:
c        ---------------
c
	 rhoim1 = qr(i-1,1)
	 pim1 = gamma1*(qr(i-1,3) - 0.5d0*qr(i-1,2)**2 / rhoim1)
	 cim1 = dsqrt(gamma*pim1/rhoim1)
	 s0 = qr(i-1,2)/rhoim1 - cim1     !# u-c in left state (cell i-1)

c        # check for fully supersonic case:
	 if (s0.ge.0.d0 .and. s(i,1).gt.0.d0)  then
c            # everything is right-going
	     do 60 m=1,3
		amdq(i,m) = 0.d0
   60           continue
	     go to 200 
	     endif
c
         rho1 = qr(i-1,1) + wave(i,1,1)
         rhou1 = qr(i-1,2) + wave(i,2,1)
         en1 = qr(i-1,3) + wave(i,3,1)
         p1 = gamma1*(en1 - 0.5d0*rhou1**2/rho1)
         c1 = dsqrt(gamma*p1/rho1)
         s1 = rhou1/rho1 - c1  !# u-c to right of 1-wave
         if (s0.lt.0.d0 .and. s1.gt.0.d0) then
c            # transonic rarefaction in the 1-wave
	     sfract = s0 * (s1-s(i,1)) / (s1-s0)
	   else if (s(i,1) .lt. 0.d0) then
c	     # 1-wave is leftgoing
	     sfract = s(i,1)
	   else
c	     # 1-wave is rightgoing
             sfract = 0.d0   !# this shouldn't happen since s0 < 0
	   endif
	 do 120 m=1,3
	    amdq(i,m) = sfract*wave(i,m,1)
  120       continue
c
c        # check 2-wave:
c        ---------------
c
         if (s(i,2) .ge. 0.d0) go to 200  !# 2-wave is rightgoing
	 do 140 m=1,3
	    amdq(i,m) = amdq(i,m) + s(i,2)*wave(i,m,2)
  140       continue
c
c        # check 3-wave:
c        ---------------
c
	 rhoi = ql(i,1)
	 pi = gamma1*(ql(i,3) - 0.5d0*ql(i,2)**2 / rhoi)
	 ci = dsqrt(gamma*pi/rhoi)
	 s3 = ql(i,2)/rhoi + ci     !# u+c in right state  (cell i)
c
         rho2 = ql(i,1) - wave(i,1,3)
         rhou2 = ql(i,2) - wave(i,2,3)
         en2 = ql(i,3) - wave(i,3,3)
         p2 = gamma1*(en2 - 0.5d0*rhou2**2/rho2)
         c2 = dsqrt(gamma*p2/rho2)
         s2 = rhou2/rho2 + c2   !# u+c to left of 3-wave
         if (s2 .lt. 0.d0 .and. s3.gt.0.d0) then
c            # transonic rarefaction in the 3-wave
	     sfract = s2 * (s3-s(i,3)) / (s3-s2)
	   else if (s(i,3) .lt. 0.d0) then
c            # 3-wave is leftgoing
	     sfract = s(i,3)
	   else 
c            # 3-wave is rightgoing
	     go to 200
	   endif
c
	 do 160 m=1,3
	    amdq(i,m) = amdq(i,m) + sfract*wave(i,m,3)
  160       continue
  200    continue
c
c     # compute the rightgoing flux differences:
c     # df = SUM s*wave   is the total flux difference and apdq = df - amdq
c
      do 220 m=1,3
	 do 220 i = 2-mbc, mx+mbc
	    df = 0.d0
	    do 210 mw=1,mwaves
	       df = df + s(i,mw)*wave(i,m,mw)
  210          continue
	    apdq(i,m) = df - amdq(i,m)
  220       continue
c

c
  900 continue
      return
      end
