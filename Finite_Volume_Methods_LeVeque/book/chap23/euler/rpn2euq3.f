
c
c
c     =====================================================
      subroutine rpn2(ixy,maxm,meqn,mwaves,mbc,mx,ql,qr,auxl,auxr,
     &			wave,s,amdq,apdq)
c     =====================================================
c
c     # Roe-solver for the Euler equations on a curvilinear grid
c     # mwaves = 3
c
c     # solve Riemann problems along one slice of data.
c
c     # On input, ql contains the state vector at the left edge of each cell
c     #           qr contains the state vector at the right edge of each cell
c
c     # This data is along a slice in the x-direction if ixy=1 
c     #                            or the y-direction if ixy=2.
c     # On output, wave contains the waves, s the speeds, 
c     # and amdq, apdq the decomposition of the flux difference
c     #   f(qr(i-1)) - f(ql(i))  
c     # into leftgoing and rightgoing parts respectively.
c     # With the Roe solver we have   
c     #    amdq  =  A^- \Delta q    and    apdq  =  A^+ \Delta q
c     # where A is the Roe matrix.  An entropy fix can also be incorporated
c     # into the flux differences.
c
c     # Note that the i'th Riemann problem has left state qr(i-1,:)
c     #                                    and right state ql(i,:)
c     # From the basic clawpack routines, this routine is called with ql = qr
c
c
      implicit double precision (a-h,o-z)
c
      dimension wave(1-mbc:maxm+mbc, meqn, mwaves)
      dimension    s(1-mbc:maxm+mbc, mwaves)
      dimension   ql(1-mbc:maxm+mbc, meqn)
      dimension   qr(1-mbc:maxm+mbc, meqn)
      dimension  apdq(1-mbc:maxm+mbc, meqn)
      dimension  amdq(1-mbc:maxm+mbc, meqn)
      dimension auxl(1-mbc:maxm+mbc, 7)
      dimension auxr(1-mbc:maxm+mbc, 7)
c
c     local arrays -- common block comroe is passed to rpt2eu
c     ------------
      parameter (maxm2 = 502)  !# assumes at most 500x500 grid with mbc=2
      dimension delta(4)
      logical efix
      dimension u2v2(-1:maxm2),
     &       u(-1:maxm2),v(-1:maxm2),enth(-1:maxm2),a(-1:maxm2),
     &       g1a2(-1:maxm2),euv(-1:maxm2) 
      dimension q2l(-1:maxm2), q2r(-1:maxm2)
      dimension q3l(-1:maxm2), q3r(-1:maxm2)
      dimension ax(-1:maxm2)
      dimension ay(-1:maxm2)
      common /param/ gamma, gamma1
c
      data efix /.true./    !# use entropy fix for transonic rarefactions
c
      if (-1.gt.1-mbc .or. maxm2 .lt. maxm+mbc) then
	 write(6,*) 'need to increase maxm2 in rpn2'
	 stop
	 endif
c
c
c     # rotate the velocities q(2) and q(3) so that it is aligned with grid
c     # normal.  The normal vector for the face at the i'th Riemann problem
c     # is stored in the aux array
c     # in locations (1,2) if ixy=1 or (4,5) if ixy=2.  The ratio of the
c     # length of the cell side to the length of the computational cell
c     # is stored in aux(3) or aux(6) respectively.
c
c
      if (ixy.eq.1) then
          inx = 1
          iny = 2
          ilenrat = 3
        else
          inx = 4
          iny = 5
          ilenrat = 6
        endif
c
c       # determine rotation matrix
c               [ ax  ay ]
c               [-ay  ax ]
c
c       # note that this reduces to identity on standard cartesian grid
c
        do i=2-mbc,mx+mbc
           ax(i) = auxl(i,inx)
           ay(i) = auxl(i,iny)
           q2l(i) = ax(i)*ql(i,2) + ay(i)*ql(i,3)
           q2r(i-1) = ax(i)*qr(i-1,2) + ay(i)*qr(i-1,3)
           q3l(i) = -ay(i)*ql(i,2) + ax(i)*ql(i,3)
           q3r(i-1) = -ay(i)*qr(i-1,2) + ax(i)*qr(i-1,3)
           enddo
c
c
      do 10 i = 2-mbc, mx+mbc
         rhsqrtl = dsqrt(qr(i-1,1))
         rhsqrtr = dsqrt(ql(i,1))
         pl = gamma1*(qr(i-1,4) - 0.5d0*(qr(i-1,2)**2 +
     &        qr(i-1,3)**2)/qr(i-1,1))
         pr = gamma1*(ql(i,4) - 0.5d0*(ql(i,2)**2 +
     &        ql(i,3)**2)/ql(i,1))
         rhsq2 = rhsqrtl + rhsqrtr
         u(i) = (q2l(i)/rhsqrtr + q2r(i-1)/rhsqrtl) / rhsq2
         v(i) = (q3l(i)/rhsqrtr + q3r(i-1)/rhsqrtl) / rhsq2
         enth(i) = (((qr(i-1,4)+pl)/rhsqrtl
     &             + (ql(i,4)+pr)/rhsqrtr)) / rhsq2
	 u2v2(i) = u(i)**2 + v(i)**2
         a2 = gamma1*(enth(i) - .5d0*u2v2(i))
         a(i) = dsqrt(a2)
	 g1a2(i) = gamma1 / a2
	 euv(i) = enth(i) - u2v2(i) 
   10    continue
c
c
c     # now split the jump in q at each interface into waves
c
c     # find a1 thru a4, the coefficients of the 4 eigenvectors:
      do 20 i = 2-mbc, mx+mbc
         delta(1) = ql(i,1) - qr(i-1,1)
         delta(2) = q2l(i) - q2r(i-1)
         delta(3) = q3l(i) - q3r(i-1)
         delta(4) = ql(i,4) - qr(i-1,4)
         a3 = g1a2(i) * (euv(i)*delta(1) 
     &      + u(i)*delta(2) + v(i)*delta(3) - delta(4))
         a2 = delta(3) - v(i)*delta(1)
         a4 = (delta(2) + (a(i)-u(i))*delta(1) - a(i)*a3) / (2.d0*a(i))
         a1 = delta(1) - a3 - a4
c
c        # Compute the waves.
c        # Note that the 2-wave and 3-wave travel at the same speed and 
c        # are lumped together in wave(.,.,2).  The 4-wave is then stored in
c        # wave(.,.,3).
c

         wave(i,1,1) = a1
         wave(i,2,1) = a1*(u(i)-a(i)) 
         wave(i,3,1) = a1*v(i)
         wave(i,4,1) = a1*(enth(i) - u(i)*a(i))
         s(i,1) = (u(i)-a(i))
c
         wave(i,1,2) = a3
         wave(i,2,2) = a3*u(i)
         wave(i,3,2) = a3*v(i) + a2
         wave(i,4,2) = a3*0.5d0*u2v2(i)  + a2*v(i)
         s(i,2) = u(i)
c
         wave(i,1,3) = a4
         wave(i,2,3) = a4*(u(i)+a(i))
         wave(i,3,3) = a4*v(i)
         wave(i,4,3) = a4*(enth(i)+u(i)*a(i))
         s(i,3) = (u(i)+a(i)) 
   20    continue
c
c
c    # compute flux differences amdq and apdq.
c    ---------------------------------------
c
      if (efix) go to 110
c
c     # no entropy fix
c     ----------------
c
      do 80 i=2-mbc, mx+mbc
         do 80 mw=1,mwaves
c
c           # scale wave speeds by ratio of cell side length to dxc:
	    s(i,mw) = s(i,mw) * auxl(i,ilenrat)
c
c           # rotate momentum components of waves back to x-y:
	    wave2 = ax(i)*wave(i,2,mw) - ay(i)*wave(i,3,mw)
	    wave3 = ay(i)*wave(i,2,mw) + ax(i)*wave(i,3,mw)
	    wave(i,2,mw) = wave2
	    wave(i,3,mw) = wave3
   80       continue
c	 
c     # amdq = SUM s*wave   over left-going waves
c     # apdq = SUM s*wave   over right-going waves
c
      do 100 m=1,4
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
      do 200 i = 2-mbc, mx+mbc
c
c        # check 1-wave:
c        ---------------
c
	 rhoim1 = qr(i-1,1)
	 pim1 = gamma1*(qr(i-1,4) - 0.5d0*u2v2(i)*rhoim1)
	 cim1 = dsqrt(gamma*pim1/rhoim1)
c	 # speed of left-most signal: s0 = u-c in left state (cell i-1)
	 s0 = q2r(i-1)/rhoim1 - cim1

c        # check for fully supersonic case:
	 if (s0.ge.0.d0 .and. s(i,1).gt.0.d0)  then
c            # everything is right-going
	     do 60 m=1,4
		amdq(i,m) = 0.d0
   60           continue
	     go to 200 
	     endif
c
         rho1 = qr(i-1,1) + wave(i,1,1)
         rhou1 = q2r(i-1) + wave(i,2,1)
         rhov1 = q3r(i-1) + wave(i,3,1)
         en1 = qr(i-1,4) + wave(i,4,1)
         p1 = gamma1*(en1 - 0.5d0*(rhou1**2 + rhov1**2)/rho1)
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
	 do 120 m=1,4
	    amdq(i,m) = sfract*wave(i,m,1)
  120       continue
c
c        # check 2-wave:
c        ---------------
c
         if (s(i,2) .ge. 0.d0) go to 200  !# 2- and 3- waves are rightgoing
	 do 140 m=1,4
	    amdq(i,m) = amdq(i,m) + s(i,2)*wave(i,m,2)
  140       continue
c
c        # check 3-wave:
c        ---------------
c
	 rhoi = ql(i,1)
	 pi = gamma1*(ql(i,4) - 0.5d0*u2v2(i)*rhoi)
	 ci = dsqrt(gamma*pi/rhoi)
	 s3 = q2l(i)/rhoi + ci     !# u+c in right state  (cell i)
c
         rho2 = ql(i,1) - wave(i,1,3)
         rhou2 = q2l(i) - wave(i,2,3)
         rhov2 = q3l(i) - wave(i,3,3)
         en2 = ql(i,4) - wave(i,4,3)
         p2 = gamma1*(en2 - 0.5d0*(rhou2**2 + rhov2**2)/rho2)
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
	 do 160 m=1,4
	    amdq(i,m) = amdq(i,m) + sfract*wave(i,m,3)
  160       continue
  200    continue
c
      do 190 i=2-mbc, mx+mbc
         do 180 mw=1,mwaves
c
c           # scale wave speeds by ratio of cell side length to dxc:
	    s(i,mw) = s(i,mw) * auxl(i,ilenrat)
c
c           # rotate momentum components of waves back to x-y:
	    wave2 = ax(i)*wave(i,2,mw) - ay(i)*wave(i,3,mw)
	    wave3 = ay(i)*wave(i,2,mw) + ax(i)*wave(i,3,mw)
	    wave(i,2,mw) = wave2
	    wave(i,3,mw) = wave3
  180       continue
c
c        # flux difference must also be rotated and scaled:
	 amdq2 = (ax(i)*amdq(i,2) - ay(i)*amdq(i,3)) 
	 amdq3 = (ay(i)*amdq(i,2) + ax(i)*amdq(i,3))
c
	 amdq(i,1) = amdq(i,1) * auxl(i,ilenrat)
	 amdq(i,2) = amdq2     * auxl(i,ilenrat)
	 amdq(i,3) = amdq3     * auxl(i,ilenrat)
	 amdq(i,4) = amdq(i,4) * auxl(i,ilenrat)
  190    continue
c	 
c
c     # compute the rightgoing flux differences:
c     # df = SUM s*wave   is the total flux difference and apdq = df - amdq
c
      do 220 m=1,4
	 do 220 i = 2-mbc, mx+mbc
	    df = 0.d0
	    do 210 mw=1,mwaves
	       df = df + s(i,mw)*wave(i,m,mw)
  210          continue
	    apdq(i,m) = df - amdq(i,m)
  220       continue
c
  900 continue
      return
      end
