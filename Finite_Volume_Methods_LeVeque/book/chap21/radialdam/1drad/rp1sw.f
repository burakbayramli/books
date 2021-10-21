c =========================================================
      subroutine rp1(maxmx,meqn,mwaves,mbc,mx,ql,qr,auxl,auxr,
     &		       wave,s,amdq,apdq)
c =========================================================
c
c     # solve Riemann problems for the 1D shallow water equations
c     #   (h)_t + (u h)_x = 0 
c     #   (uh)_t + ( uuh + .5*gh^2 )_x = 0 
c     # using Roe's approximate Riemann solver with entropy fix for
c     # transonic rarefractions.  
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
c     Here meqn=mwaves=2 should be passed from the calling routine
c
      implicit double precision (a-h,o-z)
      dimension   ql(1-mbc:maxmx+mbc, meqn)
      dimension   qr(1-mbc:maxmx+mbc, meqn)
      dimension    s(1-mbc:maxmx+mbc, mwaves)
      dimension wave(1-mbc:maxmx+mbc, meqn, mwaves)
      dimension amdq(1-mbc:maxmx+mbc, meqn)
      dimension apdq(1-mbc:maxmx+mbc, meqn)
      dimension auxl(1-mbc:maxmx+mbc, *)
      dimension auxr(1-mbc:maxmx+mbc, *)
c
c     # local storage
c     ---------------
      dimension delta(2)
      logical efix
      common /comrp/ grav
      data efix /.true./    !# use entropy fix for transonic rarefactions
          
c
      do 30 i=2-mbc,mx+mbc
c
c         
c     # compute  Roe-averaged quantities: 
         ubar = (qr(i-1,2)/dsqrt(qr(i-1,1)) + ql(i,2)/dsqrt(ql(i,1)))/
     .       ( dsqrt(qr(i-1,1)) + dsqrt(ql(i,1)) )
         cbar=dsqrt(0.5d0*grav*(qr(i-1,1) + ql(i,1)))
         
c     # delta(1)=h(i)-h(i-1) and  delta(2)=hu(i)-hu(i-1)
      delta(1) = ql(i,1) - qr(i-1,1)
      delta(2) = ql(i,2) - qr(i-1,2)

c # compute coeffs in the evector expansion of delta(1),delta(2)
      a1 = 0.5d0*(-delta(2) + (ubar + cbar) * delta(1))/cbar
      a2 = 0.5d0*( delta(2) - (ubar - cbar) * delta(1))/cbar

c     # finally, compute the waves.
         wave(i,1,1) = a1
         wave(i,2,1) = a1*(ubar - cbar)
         s(i,1) = ubar - cbar
         
         wave(i,1,2) = a2
         wave(i,2,2) = a2*(ubar + cbar)
         s(i,2) = ubar + cbar
         
   30 continue

c     # compute Godunov flux f0:
c     --------------------------

      if (efix) go to 110

c     # no entropy fix
c     ----------------------------------------------
c     # amdq = SUM s*wave   over left-going waves
c     # apdq = SUM s*wave   over right-going waves

      do 100 m=1,2
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
c    -----------------------------------------------
c

  110 continue

c     # With entropy fix
c     ------------------
c
c    # compute flux differences amdq and apdq.
c    # First compute amdq as sum of s*wave for left going waves.
c    # Incorporate entropy fix by adding a modified fraction of wave
c    # if s should change sign.
c
      do 200 i=2-mbc,mx+mbc
      
c ------------------------------------------------------
c        # check 1-wave:
c        ---------------
c
c        # u-c in left state (cell i-1)
         s0 = qr(i-1,2)/qr(i-1,1) - dsqrt(grav*qr(i-1,1))
         
c        # check for fully supersonic case:
	 if (s0.ge.0.d0 .and. s(i,1).gt.0.d0)  then
c            # everything is right-going
	     do 60 m=1,2
		amdq(i,m) = 0.d0
   60           continue
	     go to 200 
	     endif
c
c        # u-c to right of 1-wave
         hr1  = qr(i-1,1) + wave(i,1,1) 
         uhr1 = qr(i-1,2) + wave(i,2,1)
         s1 =  uhr1/hr1 - dsqrt(grav*hr1)
                 
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

	 do 120 m=1,2
	    amdq(i,m) = sfract*wave(i,m,1)
  120       continue
  
c -------------------------------------------------------
c        # check 2-wave:
c        ---------------
c        # u+c in right state  (cell i)
         s3 = ql(i,2)/ql(i,1) + dsqrt(grav*ql(i,1))
              
c        # u+c to left of 2-wave
         hl2  = ql(i,1) - wave(i,1,2) 
         uhl2 = ql(i,2) - wave(i,2,2)
         s2 = uhl2/hl2 + dsqrt(grav*hl2)
                  
         if (s2 .lt. 0.d0 .and. s3.gt.0.d0) then
c            # transonic rarefaction in the 2-wave
	     sfract = s2 * (s3-s(i,2)) / (s3-s2)
	   else if (s(i,2) .lt. 0.d0) then
c            # 2-wave is leftgoing
	     sfract = s(i,2)
	   else 
c            # 2-wave is rightgoing
	     go to 200
	   endif
c
	 do 160 m=1,2
	    amdq(i,m) = amdq(i,m) + sfract*wave(i,m,2)
  160       continue
  200    continue
c
c
c     # compute the rightgoing flux differences:
c     # df = SUM s*wave   is the total flux difference and apdq = df - amdq
c
      do 220 m=1,2
	 do 220 i = 2-mbc, mx+mbc
	    df = 0.d0
	    do 210 mw=1,mwaves
	       df = df + s(i,mw)*wave(i,m,mw)
  210          continue
	    apdq(i,m) = df - amdq(i,m)
  220       continue

  900 continue
      return
      end



