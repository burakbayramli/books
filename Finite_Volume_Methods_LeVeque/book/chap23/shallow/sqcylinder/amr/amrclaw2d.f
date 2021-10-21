
c
c
c
c     =====================================================
       subroutine qinit(maxmx,maxmy,meqn,mbc,mx,my,xlower,ylower,
     &                   dx,dy,q,maux,aux)
c     =====================================================
c
c     # Set initial conditions for q.
c
       implicit double precision (a-h,o-z)
       dimension q(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc, meqn)
       common /comic/ hl,hr,hul,hur

c
       do 20 i=1,mx
	  xc = xlower + (i-0.5d0)*dx
	  xclow = xlower + (i-1.0d0)*dx
          do 20 j=1,my
	     yc = ylower + (j-0.5d0)*dy
	     yclow = ylower + (j-1.0d0)*dy

c            # map the center of this computational cell to physical
c            # coordinates before evaluating the initial value funcion:
	     call mapc2p(xc,yc,xp,yp)

c	     q(i,j,1) = 1.d0  + 
c    &		3.d0*dexp(-50.d0*((xp-1.0d0)**2 + (yp-1.0d0)**2))
c    &		dexp(-100.d0*((xp-0.8d0)**2 + (yp-1.0d0)**2))

             call cellave(xclow,yclow,dx,dy,win)
	     q(i,j,1) = hl*win + hr*(1.d0-win)
             q(i,j,2) = hul*win + hur*(1.d0-win)
             q(i,j,3) = 0.d0

  20         continue
       return
       end

c
c
c     =====================================================
      subroutine rpn2(ixy,maxm,meqn,mwaves,mbc,mx,ql,qr,auxl,auxr,
     &			wave,s,amdq,apdq)
c     =====================================================
c
c     # Roe-solver for the 2D shallow water equations
c     # on a quadrilateral grid.   The velocity components of data are
c     # rotated to the normal direction and then the standard Roe
c     # solver and entropy fix are applied, and rotation back is applied
c     # at the end.
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
c     local arrays -- common block comroe is passed to rpt2sh
c     ------------
      parameter (maxm2 = 1002)  !# assumes at most 1000x1000 grid with mbc=2
      dimension delta(3)
      logical efix
      dimension unorl(-1:maxm2), unorr(-1:maxm2)
      dimension utanl(-1:maxm2), utanr(-1:maxm2)
      dimension ax(-1:maxm2)
      dimension ay(-1:maxm2)

      common /sw/  g
      common /comroe/ u(-1:maxm2),v(-1:maxm2),a(-1:maxm2),h(-1:maxm2)
c
      data efix /.true./    !# use entropy fix for transonic rarefactions
c
      if (-1.gt.1-mbc .or. maxm2 .lt. maxm+mbc) then
	 write(6,*) 'need to increase maxm2 in rpA'
	 stop
	 endif
c
c
c     # rotate the velocities q(2) and q(3) so that it is aligned with grid
c     # normal.  The normal vector for the face at the i'th Riemann problem
c     # is stored in the aux array in locations (1,2) if ixy=1 or 
c     # (4,5) if ixy=2.  This normal is called (ax,ay) below.
c     # The ratio of the length of the cell side to the length of the 
c     # computational cell is stored in aux(3) or aux(6) respectively.
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
           unorl(i) = ax(i)*ql(i,2) + ay(i)*ql(i,3)
           unorr(i-1) = ax(i)*qr(i-1,2) + ay(i)*qr(i-1,3)
           utanl(i) = -ay(i)*ql(i,2) + ax(i)*ql(i,3)
           utanr(i-1) = -ay(i)*qr(i-1,2) + ax(i)*qr(i-1,3)
           enddo
c
c
c     # compute the Roe-averaged variables needed in the Roe solver.
c     # These are stored in the common block comroe since they are
c     # later used in routine rpt2 to do the transverse wave splitting.
c
        do 10 i = 2-mbc, mx+mbc
         h(i) = (qr(i-1,1)+ql(i,1))*0.50d0
         hsqrtl = dsqrt(qr(i-1,1))
         hsqrtr = dsqrt(ql(i,1))
         hsq2 = hsqrtl + hsqrtr
         u(i) = (unorr(i-1)/hsqrtl + unorl(i)/hsqrtr) / hsq2
         v(i) = (utanr(i-1)/hsqrtl + utanl(i)/hsqrtr) / hsq2
         a(i) = dsqrt(g*h(i))
   10    continue
c
c
c     # now split the jump in q at each interface into waves
c
c     # find a1 thru a3, the coefficients of the 3 eigenvectors:
      do 20 i = 2-mbc, mx+mbc
         delta(1) = ql(i,1) - qr(i-1,1)
         delta(2) = unorl(i) - unorr(i-1)
         delta(3) = utanl(i) - utanr(i-1)
         a1 = ((u(i)+a(i))*delta(1) - delta(2))*(0.50d0/a(i))
         a2 = -v(i)*delta(1) + delta(3)
         a3 = (-(u(i)-a(i))*delta(1) + delta(2))*(0.50d0/a(i))
c
c        # Compute the waves.
c
         wave(i,1,1) = a1
         wave(i,2,1) = ax(i)*a1*(u(i)-a(i)) - ay(i)*a1*v(i)
         wave(i,3,1) = ay(i)*a1*(u(i)-a(i)) + ax(i)*a1*v(i)
         s(i,1) = (u(i)-a(i)) * auxl(i,ilenrat)
c
         wave(i,1,2) = 0.0d0
         wave(i,2,2) = -ay(i)*a2
         wave(i,3,2) = ax(i)*a2
         s(i,2) = u(i) * auxl(i,ilenrat)
c
         wave(i,1,3) = a3
         wave(i,2,3) = ax(i)*a3*(u(i)+a(i)) - ay(i)*a3*v(i)
         wave(i,3,3) = ay(i)*a3*(u(i)+a(i)) + ax(i)*a3*v(i)
         s(i,3) = (u(i)+a(i)) * auxl(i,ilenrat)
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
c           check 1-wave
            him1 = qr(i-1,1)
            s0 =  (unorr(i-1)/him1 - dsqrt(g*him1)) * auxl(i,ilenrat)
c           check for fully supersonic case :
            if (s0.gt.0.0d0.and.s(i,1).gt.0.0d0) then
               do 60 m=1,3
                  amdq(i,m)=0.0d0
   60          continue
               goto 200
            endif
c
            h1 = qr(i-1,1)+wave(i,1,1)
            hu1= unorr(i-1)+ ax(i)*wave(i,2,1) + ay(i)*wave(i,3,1)
            s1 = (hu1/h1 - dsqrt(g*h1))* auxl(i,ilenrat)
                   !speed just to right of 1-wave
            if (s0.lt.0.0d0.and.s1.gt.0.0d0) then
c              transonic rarefaction in 1-wave
               sfract = s0*((s1-s(i,1))/(s1-s0))
            else if (s(i,1).lt.0.0d0) then
c              1-wave is leftgoing
               sfract = s(i,1)
            else
c              1-wave is rightgoing
               sfract = 0.0d0
            endif
            do 120 m=1,3
               amdq(i,m) = sfract*wave(i,m,1)
  120       continue
c           check 2-wave
            if (s(i,2).gt.0.0d0) then
c	       #2 and 3 waves are right-going
	       go to 200 
	       endif

            do 140 m=1,3
               amdq(i,m) = amdq(i,m) + s(i,2)*wave(i,m,2)
  140       continue
c
c           check 3-wave
c
            hi = ql(i,1)
            s03 = (unorl(i)/hi + dsqrt(g*hi)) * auxl(i,ilenrat)
            h3=ql(i,1)-wave(i,1,3)
            hu3=unorl(i)- (ax(i)*wave(i,2,3) + ay(i)*wave(i,3,3))
            s3=(hu3/h3 + dsqrt(g*h3)) * auxl(i,ilenrat)
            if (s3.lt.0.0d0.and.s03.gt.0.0d0) then
c              transonic rarefaction in 3-wave
               sfract = s3*((s03-s(i,3))/(s03-s3))
            else if (s(i,3).lt.0.0d0) then
c              3-wave is leftgoing
               sfract = s(i,3)
            else
c              3-wave is rightgoing
               goto 200
            endif
            do 160 m=1,3
               amdq(i,m) = amdq(i,m) + sfract*wave(i,m,3)
  160       continue
  200       continue
c
c           compute rightgoing flux differences :
c
            do 220 m=1,3
               do 220 i = 2-mbc,mx+mbc
                  df = 0.0d0
                  do 210 mw=1,mwaves
                     df = df + s(i,mw)*wave(i,m,mw)
  210             continue
                  apdq(i,m)=df-amdq(i,m)
  220          continue
c
c
  900          continue
               return
               end


c
c
c     =====================================================
      subroutine rpt2(ixy,maxm,meqn,mwaves,mbc,mx,
     &                  ql,qr,aux1,aux2,aux3,
     &			imp,asdq,bmasdq,bpasdq)
c     =====================================================
      implicit double precision (a-h,o-z)
c
c     # Riemann solver in the transverse direction for the shallow water
c     # equations  on a quadrilateral grid.
c
c     # Split asdq (= A^* \Delta q, where * = + or -)
c     # into down-going flux difference bmasdq (= B^- A^* \Delta q)
c     #    and up-going flux difference bpasdq (= B^+ A^* \Delta q)
c
c     # Uses Roe averages and other quantities which were
c     # computed in rpn2sh and stored in the common block comroe.
c
      dimension     ql(1-mbc:maxm+mbc, meqn)
      dimension     qr(1-mbc:maxm+mbc, meqn)
      dimension   asdq(1-mbc:maxm+mbc, meqn)
      dimension bmasdq(1-mbc:maxm+mbc, meqn)
      dimension bpasdq(1-mbc:maxm+mbc, meqn)
      dimension   aux1(1-mbc:maxm+mbc, 7)
      dimension   aux2(1-mbc:maxm+mbc, 7)
      dimension   aux3(1-mbc:maxm+mbc, 7)
c
      parameter (maxm2 = 1002)  !# assumes at most 1000x1000 grid with mbc=2
      dimension u(-1:maxm2),v(-1:maxm2),a(-1:maxm2),h(-1:maxm2)
      dimension ax(-1:maxm2)
      dimension ay(-1:maxm2)
      dimension wave(-1:maxm2, 3, 3)
      dimension    s(-1:maxm2, 3)
      dimension delta(3)
      common /sw/  g
c
      if (-1.gt.1-mbc .or. maxm2 .lt. maxm+mbc) then
	 write(6,*) 'need to increase maxm2 in rpB'
	 stop
      endif
c
c
      if (ixy.eq.1) then
          inx = 4
          iny = 5
          ilenrat = 6
        else
          inx = 1
          iny = 2
          ilenrat = 3
        endif
c
c        # imp is used to flag whether wave is going to left or right,
c        # since states and grid are different on each side
c
         if (imp.eq.1) then
c            # asdq = amdq, moving to left
             ix1 = 2-mbc
	     ixm1 = mx+mbc
           else
c            # asdq = apdq, moving to right
             ix1 = 1-mbc
	     ixm1 = mx+mbc
           endif
c
c        --------------
c        # up-going:
c        --------------
c

c       # determine rotation matrix for interface above cell, using aux3
c               [ ax  ay ]
c               [-ay  ax ]
c
        do i=ix1,ixm1
c
         if (imp.eq.1) then
             i1 = i-1
           else
             i1 = i
           endif
c
           ax(i) = aux3(i1,inx)
           ay(i) = aux3(i1,iny)
           h(i) = ql(i1,1)
           u(i) = (ax(i)*ql(i1,2) + ay(i)*ql(i1,3)) / h(i)
           v(i) = (-ay(i)*ql(i1,2) + ax(i)*ql(i1,3)) / h(i)
	   a(i) = dsqrt(g*h(i))
           enddo
c
c
c
c     # now split asdq into waves:
c
c     # find a1 thru a3, the coefficients of the 3 eigenvectors:
      do 20 i = ix1,ixm1
         delta(1) = asdq(i,1) 
         delta(2) = ax(i)*asdq(i,2) + ay(i)*asdq(i,3)
         delta(3) = -ay(i)*asdq(i,2) + ax(i)*asdq(i,3)
         a1 = ((u(i)+a(i))*delta(1) - delta(2))*(0.50d0/a(i))
         a2 = -v(i)*delta(1) + delta(3)
         a3 = (-(u(i)-a(i))*delta(1) + delta(2))*(0.50d0/a(i))
c
c        # Compute the waves.
c
         wave(i,1,1) = a1
         wave(i,2,1) = ax(i)*a1*(u(i)-a(i)) - ay(i)*a1*v(i)
         wave(i,3,1) = ay(i)*a1*(u(i)-a(i)) + ax(i)*a1*v(i)
         s(i,1) = (u(i)-a(i)) * aux3(i1,ilenrat)
c
         wave(i,1,2) = 0.0d0
         wave(i,2,2) = -ay(i)*a2
         wave(i,3,2) = ax(i)*a2
         s(i,2) = u(i) * aux3(i1,ilenrat)
c
         wave(i,1,3) = a3
         wave(i,2,3) = ax(i)*a3*(u(i)+a(i)) - ay(i)*a3*v(i)
         wave(i,3,3) = ay(i)*a3*(u(i)+a(i)) + ax(i)*a3*v(i)
         s(i,3) = (u(i)+a(i)) * aux3(i1,ilenrat)
   20    continue
c
c
c    # compute flux difference bpasdq
c    --------------------------------
c
      do 40 m=1,3
         do 40 i=ix1,ixm1
	    bpasdq(i,m) = 0.d0
	    do 30 mw=1,mwaves
	       bpasdq(i,m) = bpasdq(i,m) 
     &	           + dmax1(s(i,mw),0.d0)*wave(i,m,mw)
   30          continue
   40       continue
c
c
c        --------------
c        # down-going:
c        --------------
c

c       # determine rotation matrix for interface below cell, using aux2
c               [ ax  ay ]
c               [-ay  ax ]
c
        do i=ix1,ixm1
c
         if (imp.eq.1) then
             i1 = i-1
           else
             i1 = i
           endif
c
           ax(i) = aux2(i1,inx)
           ay(i) = aux2(i1,iny)
           u(i) = (ax(i)*ql(i1,2) + ay(i)*ql(i1,3)) / h(i)
           v(i) = (-ay(i)*ql(i1,2) + ax(i)*ql(i1,3)) / h(i)
           enddo
c
c
c
c     # now split asdq into waves:
c
c     # find a1 thru a3, the coefficients of the 3 eigenvectors:
      do 80 i = ix1,ixm1
         delta(1) = asdq(i,1) 
         delta(2) = ax(i)*asdq(i,2) + ay(i)*asdq(i,3)
         delta(3) = -ay(i)*asdq(i,2) + ax(i)*asdq(i,3)
         a1 = ((u(i)+a(i))*delta(1) - delta(2))*(0.50d0/a(i))
         a2 = -v(i)*delta(1) + delta(3)
         a3 = (-(u(i)-a(i))*delta(1) + delta(2))*(0.50d0/a(i))
c
c        # Compute the waves.
c
         wave(i,1,1) = a1
         wave(i,2,1) = ax(i)*a1*(u(i)-a(i)) - ay(i)*a1*v(i)
         wave(i,3,1) = ay(i)*a1*(u(i)-a(i)) + ax(i)*a1*v(i)
         s(i,1) = (u(i)-a(i)) * aux2(i1,ilenrat)
c
         wave(i,1,2) = 0.0d0
         wave(i,2,2) = -ay(i)*a2
         wave(i,3,2) = ax(i)*a2
         s(i,2) = u(i) * aux2(i1,ilenrat)
c
         wave(i,1,3) = a3
         wave(i,2,3) = ax(i)*a3*(u(i)+a(i)) - ay(i)*a3*v(i)
         wave(i,3,3) = ay(i)*a3*(u(i)+a(i)) + ax(i)*a3*v(i)
         s(i,3) = (u(i)+a(i)) * aux2(i1,ilenrat)
   80    continue
c
c
c    # compute flux difference bmasdq
c    --------------------------------
c
      do 100 m=1,3
         do 100 i=ix1,ixm1
	    bmasdq(i,m) = 0.d0
	    do 90 mw=1,mwaves
	       bmasdq(i,m) = bmasdq(i,m) 
     &	                    + dmin1(s(i,mw), 0.d0)*wave(i,m,mw)
   90          continue
  100       continue
c
c
      return
      end
      subroutine setprob
      implicit double precision (a-h,o-z)
c
c     # Copy this file to your directory and modify to set up problem
c     # parameters or read other data.
c
      common /sw/  g
      common/cdisc/ x0,y0,alf,beta,r0,idisc
      common /comic/ hl,hr,hul,hur

      g = 1.d0

c     # data for flow into cylinder:
      idisc = 1
      x0 = -2.d0
      y0 = 0.d0
      alf = 1.d0
      beta = 0.d0

      hl = 4.d0
      hr = 1.d0
      hur = 0.d0
      hul = hur + (hl-hr)*(hur/hr + dsqrt(g*hr + 0.5d0*g*(hl-hr)*
     &              (3.d0 + (hl-hr)/hr)))

      return
      end
c     ============================================
      subroutine setaux(maxmx,maxmy,mbc,mx,my,xlower,ylower,dxc,dyc,
     &                  maux,aux)
c     ============================================
c
c
c     #    aux(i,j,1)  = ax
c     #    aux(i,j,2)  = ay   where (ax,ay) is unit normal to left face
c     #    aux(i,j,3)  = ratio of length of left face to dyc
c
c     #    aux(i,j,4)  = bx
c     #    aux(i,j,5)  = by   where (bx,by) is unit normal to bottom face
c     #    aux(i,j,6)  = ratio of length of bottom face to dxc
c
c     #    aux(i,j,7)  = ratio of cell area to dxc*dyc
c     #                  (approximately Jacobian of mapping function)
c
c     
      implicit double precision (a-h,o-z)
      dimension aux(1-mbc:maxmx+mbc,1-mbc:maxmy+mbc, 7)
      dimension xccorn(5),yccorn(5),xpcorn(5),ypcorn(5)
c

      dx2 = dxc/2.d0
      dy2 = dyc/2.d0
c
      do 20 j=1-mbc,my+mbc
         do 20 i=1-mbc,mx+mbc
c
c           # computational points (xc,yc) are mapped to physical
c           # coordinates (xp,yp) by mapc2p:
c
c           # lower left corner:
	    xccorn(1) = xlower + (i-1)*dxc
	    yccorn(1) = ylower + (j-1)*dyc
	    call mapc2p(xccorn(1),yccorn(1),xpcorn(1),ypcorn(1))

c           # upper left corner:
	    xccorn(2) = xccorn(1)
	    yccorn(2) = yccorn(1) + dyc
	    call mapc2p(xccorn(2),yccorn(2),xpcorn(2),ypcorn(2))
c
c           # upper right corner:
	    xccorn(3) = xccorn(1) + dxc
	    yccorn(3) = yccorn(1) + dyc
	    call mapc2p(xccorn(3),yccorn(3),xpcorn(3),ypcorn(3))
c
c           # lower right corner:
	    xccorn(4) = xccorn(1) + dxc
	    yccorn(4) = yccorn(1)
	    call mapc2p(xccorn(4),yccorn(4),xpcorn(4),ypcorn(4))
c
c           # compute normals to left and bottom side:
c
	    ax =  (ypcorn(2) - ypcorn(1))
	    ay = -(xpcorn(2) - xpcorn(1))
            anorm = dsqrt(ax*ax + ay*ay)
	    aux(i,j,1) = ax/anorm
	    aux(i,j,2) = ay/anorm
	    aux(i,j,3) = anorm/dyc
c
	    bx = -(ypcorn(4) - ypcorn(1))
	    by =  (xpcorn(4) - xpcorn(1))
            bnorm = dsqrt(bx*bx + by*by)
	    aux(i,j,4) = bx/bnorm
	    aux(i,j,5) = by/bnorm
	    aux(i,j,6) = bnorm/dxc
c
c           # compute area of physical cell from four corners:
            

	    xpcorn(5) = xpcorn(1)
	    ypcorn(5) = ypcorn(1)
	    area = 0.d0
	    do ic=1,4
	      area = area + 0.5d0 * (ypcorn(ic)+ypcorn(ic+1)) *
     &               (xpcorn(ic+1)-xpcorn(ic))
	      enddo
	    aux(i,j,7) = area / (dxc*dyc)
c
   20       continue
c
       return

       end
c
c
c
c     =================================================
      function fdisc(xc,yc)
c     =================================================
      implicit double precision (a-h,o-z)
      common/cdisc/ x0,y0,alf,beta,r0,idisc
c
c     # for computing cell averages for initial data that has a
c     # discontinuity along some curve.  fdisc should be negative to the 
c     # left of the curve and positive to the right
c     # idisc specifies the nature of the discontinuity for two
c     # particular cases (a straight line and circle) but this routine
c     # can be modified for any other curve.
c
c     # map to quadrilateral grid:
      call mapc2p(xc,yc,x,y)

      go to (10,20) idisc
c
   10 continue
c     # straight line through (x0,y0) with normal (alf,beta) pointing 
c     # into right state
c
      fdisc = (x-x0)*alf + (y-y0)*beta
      return
c
   20 continue
c     # circle of radius r0:
      fdisc = (x-x0)**2 + (y-y0)**2 - r0**2
c
      return
      end

c
c ------------------------------------------------------------------
c
      subroutine bc2amr(val,aux,nrow,ncol,meqn,naux,
     1                  hx, hy, level, time,
     2                  xleft,  xright,  ybot, ytop,
     3                  xlower, ylower,xupper,yupper,
     4                  xperiodic, yperiodic)
 
c
c
c :::::::::: bc2amr ::::::::::::::::::::::::::::::::::::::::::::::;
c
c     Take a grid patch with mesh widths hx,hy, of dimensions nrow by
c     ncol,  and set the values of any piece of
c     of the patch which extends outside the physical domain 
c     using the boundary conditions. 
c
c     ------------------------------------------------
c     # Standard boundary condition choices for amr2ez in clawpack
c     # modified for a general quadrilateral grid in the case mthbc(k)=3
c
c     # At each boundary  k = 1 (left),  2 (right),  3 (top), 4 (bottom):
c     #   mthbc(k) =  0  for user-supplied BC's (must be inserted!)
c     #            =  1  for zero-order extrapolation
c     #            =  2  for periodic boundary coniditions
c     #            =  3  for solid walls, assuming this can be implemented
c     #                  by reflecting the data about the boundary and then
c     #                  negating the normal component of the velocity.
c     #                  On a quadrilateral grid we know the normal to each
c     #                  edge and assume this is stored in the aux array:
c
c     #    aux(i,j,1)  = ax
c     #    aux(i,j,2)  = ay   where (ax,ay) is unit normal to left face
c     #    aux(i,j,4)  = bx
c     #    aux(i,j,5)  = by   where (bx,by) is unit normal to bottom face
c     ------------------------------------------------
c
c     The corners of the grid patch are at 
c        (xleft,ybot)  --  lower left corner
c        (xright,ytop) --  upper right corner
c
c     The physical domain itself is a rectangle bounded by
c        (xlower,ylower)  -- lower left corner
c        (xupper,yupper)  -- upper right corner
c     
c     the picture is the following: 
c
c               _____________________ (xupper,yupper)
c              |                     |  
c          _________ (xright,ytop)   |
c          |   |    |                |
c          |   |    |                |
c          |   |    |                |
c          |___|____|                |
c (xleft,ybot) |                     |
c              |                     |
c              |_____________________|
c   (xlower,ylower)
c        
c
c     Any cells that lie outside the physical domain are ghost cells whose
c     values should be set in this routine.  This is tested for by comparing
c     xleft with xlower to see if values need to be set at the left, as in
c     the figure above, and similarly at the other boundaries.
c
c     Patches are guaranteed to have at least 1 row of cells filled
c     with interior values so it is possible to  extrapolate. 
c     Fix trimbd if you want more than 1 row pre-set.
c
c     Make sure the order the boundaries are specified is correct
c     so that diagonal corner cells are also properly taken care of.
c
c     Periodic boundaries are set before calling this routine, so if the
c     domain is periodic in one direction only you
c     can safely extrapolate in the other direction. 
c
c     Don't overwrite ghost cells in periodic directions!
c
c ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::;

      implicit double precision (a-h,o-z)

      common /combc2/ mthbc(4)

      dimension val(nrow,ncol,meqn), aux(nrow,ncol,naux)
      logical xperiodic, yperiodic

      hxmarg = hx*.01
      hymarg = hy*.01

      if (xperiodic .and. yperiodic) go to 499
c
c
c-------------------------------------------------------
c     # left boundary:
c-------------------------------------------------------
      if (xleft .ge. xlower-hxmarg) then
c        # not a physical boundary -- ghost cells lie within another
c        # grid and values are set elsewhere in amr code.
	 go to 199
	 endif
c
c     # number of ghost cells lying outside physical domain:
      nxl = (xlower+hxmarg-xleft)/hx
c
      go to (100,110,120,130) mthbc(1)+1
c
  100 continue
c     # user-specified boundary conditions go here in place of error output
      do 105 i=1,nxl
         do 105 j = 1,ncol
	    val(i,j,1) = rhol
	    val(i,j,2) = rhoul
	    val(i,j,3) = 0.d0
	    val(i,j,4) = el
  105       continue
      go to 199
c
  110 continue
c     # zero-order extrapolation:
      do 115 m=1,meqn
         do 115 i=1,nxl
            do 115 j = 1,ncol
               val(i,j,m) = val(nxl+1,j,m)
  115       continue
      go to 199

  120 continue
c     # periodic:   handled elsewhere in amr
      go to 199

  130 continue
c     # solid wall (assumes 2'nd component is velocity or momentum in x):
      do 135 m=1,meqn
         do 135 i=1,nxl
            do 135 j = 1,ncol
               val(i,j,m) = val(2*nxl+1-i,j,m)
  135       continue
c
c     # negate the normal velocity:
c     # (for a general quadrilateral grid)
c
      do 136 i=1,nxl
         do 136 j = 1,ncol
            alf = aux(nxl+1,j,1)
            beta = aux(nxl+1,j,2)
            unorm = alf*val(2*nxl+1-i,j,2) + beta*val(2*nxl+1-i,j,3)
            utang = -beta*val(2*nxl+1-i,j,2) + alf*val(2*nxl+1-i,j,3)
            unorm = -unorm
            val(i,j,2) = alf*unorm - beta*utang
            val(i,j,3) = beta*unorm + alf*utang
  136    continue
      go to 199

  199 continue
c
c-------------------------------------------------------
c     # right boundary:
c-------------------------------------------------------
      if (xright .le. xupper+hxmarg) then
c        # not a physical boundary -- ghost cells lie within another
c        # grid and values are set elsewhere in amr code.
	 go to 299
	 endif
c
c     # number of ghost cells lying outside physical domain:
      nxr = (xright - xupper + hxmarg)/hx
      ibeg = max0(nrow-nxr+1, 1)
c
      go to (200,210,220,230) mthbc(2)+1
c
  200 continue
c     # user-specified boundary conditions go here in place of error output
      write(6,*) 
     &   '*** ERROR *** mthbc(2)=0 and no BCs specified in bc2amr'
      stop
      go to 299

  210 continue
c     # zero-order extrapolation:
      do 215 m=1,meqn
         do 215 i=ibeg,nrow
            do 215 j = 1,ncol
               val(i,j,m) = val(ibeg-1,j,m)
  215       continue
      go to 299

  220 continue
c     # periodic:   handled elsewhere in amr
      go to 299

  230 continue
c     # solid wall (assumes 2'nd component is velocity or momentum in x):
      do 235 m=1,meqn
         do 235 i=ibeg,nrow
            do 235 j = 1,ncol
               val(i,j,m) = val(2*ibeg-1-i,j,m)
  235       continue
c
c     # negate the normal velocity:
c     # (for a general quadrilateral grid)
c
      do 236 i=ibeg,nrow
         do 236 j = 1,ncol
            alf = aux(ibeg,j,1)
            beta = aux(ibeg,j,2)
            unorm = alf*val(2*ibeg-1-i,j,2) + beta*val(2*ibeg-1-i,j,3)
            utang = -beta*val(2*ibeg-1-i,j,2) + alf*val(2*ibeg-1-i,j,3)
            unorm = -unorm
            val(i,j,2) = alf*unorm - beta*utang
            val(i,j,3) = beta*unorm + alf*utang
  236    continue
      go to 299

  299 continue
c
c-------------------------------------------------------
c     # bottom boundary:
c-------------------------------------------------------
      if (ybot .ge. ylower-hymarg) then
c        # not a physical boundary -- ghost cells lie within another
c        # grid and values are set elsewhere in amr code.
	 go to 399
	 endif
c
c     # number of ghost cells lying outside physical domain:
      nyb = (ylower+hymarg-ybot)/hy
c
      go to (300,310,320,330) mthbc(3)+1
c
  300 continue
c     # user-specified boundary conditions go here in place of error output
      write(6,*) 
     &   '*** ERROR *** mthbc(3)=0 and no BCs specified in bc2amr'
      stop
      go to 399
c
  310 continue
c     # zero-order extrapolation:
      do 315 m=1,meqn
         do 315 j=1,nyb
            do 315 i=1,nrow
                val(i,j,m) = val(i,nyb+1,m)
  315       continue
      go to 399

  320 continue
c     # periodic:   handled elsewhere in amr
      go to 399

  330 continue
c     # solid wall (assumes 3'rd component is velocity or momentum in y):
      do 335 m=1,meqn
         do 335 j=1,nyb
            do 335 i=1,nrow
               val(i,j,m) =  val(i,2*nyb+1-j,m)
  335       continue
c
c     # negate the normal velocity:
c     # (for a general quadrilateral grid)
c
      do 336 j=1,nyb
         do 336 i=1,nrow
            alf = aux(i,nyb+1,4)
            beta = aux(i,nyb+1,5)
            unorm = alf*val(i,2*nyb+1-j,2) + beta*val(i,2*nyb+1-j,3)
            utang = -beta*val(i,2*nyb+1-j,2) + alf*val(i,2*nyb+1-j,3)
            unorm = -unorm
            val(i,j,2) = alf*unorm - beta*utang
            val(i,j,3) = beta*unorm + alf*utang
  336    continue
      go to 399

  399 continue
c
c-------------------------------------------------------
c     # top boundary:
c-------------------------------------------------------
      if (ytop .le. yupper+hymarg) then
c        # not a physical boundary -- ghost cells lie within another
c        # grid and values are set elsewhere in amr code.
	 go to 499
	 endif
c
c     # number of ghost cells lying outside physical domain:
      nyt = (ytop - yupper + hymarg)/hy
      jbeg = max0(ncol-nyt+1, 1)
c
      go to (400,410,420,430) mthbc(4)+1
c
  400 continue
c     # user-specified boundary conditions go here in place of error output
      write(6,*) 
     &   '*** ERROR *** mthbc(4)=0 and no BCs specified in bc2amr'
      stop
      go to 499

  410 continue
c     # zero-order extrapolation:
      do 415 m=1,meqn
         do 415 j=jbeg,ncol
            do 415 i=1,nrow
               val(i,j,m) =  val(i,jbeg-1,m)
  415       continue
      go to 499

  420 continue
c     # periodic:   handled elsewhere in amr
      go to 499

  430 continue
c     # solid wall (assumes 3'rd component is velocity or momentum in y):
      do 435 m=1,meqn
         do 435 j=jbeg,ncol
            do 435 i=1,nrow
               val(i,j,m) =  val(i,2*jbeg-1-j,m)
  435       continue
c
c     # negate the normal velocity:
c     # (for a general quadrilateral grid)
c
      do 436 j=jbeg,ncol
         do 436 i=1,nrow
            alf = aux(i,jbeg,4)
            beta = aux(i,jbeg,5)
            unorm = alf*val(i,2*jbeg-1-j,2) + beta*val(i,2*jbeg-1-j,3)
            utang = -beta*val(i,2*jbeg-1-j,2) + alf*val(i,2*jbeg-1-j,3)
            unorm = -unorm
            val(i,j,2) = alf*unorm - beta*utang
            val(i,j,3) = beta*unorm + alf*utang
  436    continue
      go to 499

  499 continue

      return
      end

c
c     =====================================================
      subroutine mapc2p(xc,yc,xp,yp)
c     =====================================================
c
c     # on input,  (xc,yc) is a computational grid point
c     # on output, (xp,yp) is corresponding point in physical space
c
      implicit double precision (a-h,o-z)
c
c     # squared off radial coordinates, xc = fraction of radius,  yc = theta
c
      r0 = 1.d0
      r1 = 5.d0
      pi = 4.d0*datan(1.d0)

      yc1 = dmin1(yc, dabs(pi/2-yc))
      yc1 = dmin1(yc1, dabs(pi-yc))
      yc1 = dmin1(yc1, dabs(3*pi/2-yc))
      yc1 = dmin1(yc1, dabs(2*pi-yc))
      r = r0 + (r1*dsqrt(1.d0 + dtan(yc1)**2) - r0) * xc
      xp = r * dcos(yc)
      yp = r * dsin(yc)

c
      return
      end
c
c ----------------------------------------------------------------
c
       program amr2ez
c
c  Use adaptive mesh refinement to solve the hyperbolic 2-d equation:
c
c              u  +  f(u)    + g(u)   = 0
c               t         x        y
c
c or the more general non-conservation law form:
c              u  +  A u     + B u    = 0
c               t         x        y
c
c  using the wave propagation method as in CLAWPACK in combination
c  with the locally uniform embedded grids of AMR.

c  Estimate error with Richardson extrap. (in errest.f)
c  + gradient checking (in errsp.f).  Initial conditions set
c  in (qinit.f), b.c.'s in (physbd.f).

c  Specify rectangular domain from
c           (xlower,ylower) to (xupper,yupper).
c
c  No rotated rectangles are used in this version.
c  Periodic b.c.'s finally implemented.
c
c =========================================================================
c  Copyright 1996,  Marsha J. Berger and Randall J. LeVeque
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
c    AMRCLAW Version 0.4,  June, 1999
c     compatible with CLAWPACK Version 4.0
c    Homepage: http://www.amath.washington.edu/~claw/
c  --------------------------------------
c
c   Authors:
c
c             Marsha J. Berger
c             Courant Institute of Mathematical Sciences
c             New York University
c             251 Mercer St.
c             New York, NY 10012
c             berger@cims.nyu.edu
c
c             Randall J. LeVeque
c             Applied Mathematics
c             Box 352420
c             University of Washington,
c             Seattle, WA 98195-2420
c             rjl@amath.washington.edu
c
c =========================================================================
c


c
c ----------------------------------------------------------------
c
      implicit double precision (a-h,o-z)

      include "call.i"
      common /combc2/ mthbc(4)

      character * 12     pltfile,infile,outfile,rstfile,dbugfile,matfile
      character*10       matname2
      logical            vtime,rest
      dimension          tout(maxout)

      integer oldmode
c
c
c  you may want to turn this on for SUN workstation, or replace
c  set to signal on overflow, divide by zero, and illegal operation
c
c       oldmode = ieee_handler("set","common",SIGFPE_ABORT)
c       if (oldmode .ne. 0) then
c           write(outunit,*)' could not set ieee trapper '
c           write(*,*)      ' could not set ieee trapper '
c           stop
c        endif
c
      infile   = 'amr2ez.data'
      outfile  = 'fort.amr'
      pltfile  = 'fort.ncar'
      rstfile  = 'restart.data'
      dbugfile = 'fort.debug'
      matfile  = 'fort.nplot'

      open(inunit,  file=infile,status='old',form='formatted')
      open(outunit, file=outfile,status='unknown',form='formatted')
      open(dbugunit,file=dbugfile,status='unknown',form='formatted')
c
c     domain variables
      read(inunit,*) nx
      read(inunit,*) ny
      read(inunit,*) mxnest
      if (mxnest .gt. maxlv) then
         write(outunit,*)
     &    'Error ***   mxnest > max. allowable levels (maxlv) in common'
         write(*,*)
     &    'Error ***   mxnest > max. allowable levels (maxlv) in common'
         stop
      endif
      read(inunit,*) (intrat(i),i=1,max(1,mxnest-1))


      read(inunit,*) nout
      if (nout .gt. maxout) then
         write(outunit,*) 'Error ***   nout > maxout in common'
         write(*,*)       'Error ***   nout > maxout in common'
         stop
      endif
      read(inunit,*) outstyle
        if (outstyle.eq.1) then
           read(inunit,*) tfinal
c          # array tout is set below after reading t0
           endif
        if (outstyle.eq.2) then
           read(inunit,*) (tout(i), i=1,nout)
           endif
        if (outstyle.eq.3) then
           read(inunit,*) iout,nstop
           nout = 0
           endif
      read(inunit,*) possk(1)
      read(inunit,*) dtv2
      read(inunit,*) cflv1
      read(inunit,*) cfl
      read(inunit,*) nv1
      if (outstyle.eq.1 .or. outstyle.eq.2) then
         nstop = nv1
      endif

      read(inunit,*) method(1)
      vtime = (method(1) .eq. 1)
      read(inunit,*) method(2)
      iorder = method(2)
      read(inunit,*) method(3)
      if (method(3) .lt. 0) then
         write(6,*) '*** ERROR ***  method(3) < 0'
         write(6,*) '    dimensional splitting not supported in amrclaw'
         stop
         endif

      read(inunit,*) method(4)
      read(inunit,*) method(5)
      read(inunit,*) mcapa1
      read(inunit,*) naux
      if (naux .gt. maxaux) then
         write(outunit,*) 'Error ***   naux > maxaux in common'
         write(*,*)       'Error ***   naux > maxaux in common'
         stop
      endif
      do iaux = 1, naux
        read(inunit,*) auxtype(iaux)
        end do


      read(inunit,*) nvar
      read(inunit,*) mwaves
      if (mwaves .gt. maxwave) then
         write(outunit,*) 'Error ***   mwaves > maxwave in common'
         write(*,*)       'Error ***   mwaves > maxwave in common'
         stop
      endif
      read(inunit,*) (mthlim(mw), mw=1,mwaves)

      read(inunit,*) t0
      read(inunit,*) xlower
      read(inunit,*) xupper
      read(inunit,*) ylower
      read(inunit,*) yupper

      read(inunit,*) nghost
      read(inunit,*) mthbc(1)
      read(inunit,*) mthbc(2)
      read(inunit,*) mthbc(3)
      read(inunit,*) mthbc(4)

      xperdom = (mthbc(1).eq.2 .and. mthbc(2).eq.2)
      yperdom =  (mthbc(3).eq.2 .and. mthbc(4).eq.2)

      if ((mthbc(1).eq.2 .and. mthbc(2).ne.2) .or.
     &    (mthbc(2).eq.2 .and. mthbc(1).ne.2)) then
         write(6,*) '*** ERROR ***  periodic boundary conditions: '
         write(6,*) '  mthbc(1) and mthbc(2) must BOTH be set to 2'
         stop
         endif

      if ((mthbc(3).eq.2 .and. mthbc(4).ne.2) .or.
     &    (mthbc(4).eq.2 .and. mthbc(3).ne.2)) then
         write(6,*) '*** ERROR ***  periodic boundary conditions: '
         write(6,*) '  mthbc(3) and mthbc(4) must BOTH be set to 2'
         stop
         endif


      if (outstyle.eq.1) then
	   do i=1,nout
	      tout(i) = t0 + i*(tfinal-t0)/float(nout)
	      enddo
           endif


c     restart and checkpointing
      read(inunit,*) rest
      read(inunit,*) ichkpt
c
c     refinement variables
      read(inunit,*) tol
      read(inunit,*) tolsp
      read(inunit,*) kcheck
      read(inunit,*) ibuff
      read(inunit,*) cut
c
c     style of output
c
      read(inunit,*) printout
      read(inunit,*) ncarout
      read(inunit,*) matlabout

c
c
c     # read verbose/debugging flags
      read(inunit,*) dprint
      read(inunit,*) eprint
      read(inunit,*) edebug
      read(inunit,*) gprint
      read(inunit,*) nprint
      read(inunit,*) pprint
      read(inunit,*) rprint
      read(inunit,*) sprint
      read(inunit,*) tprint
      read(inunit,*) uprint

c     # look for capacity function via auxtypes:
      mcapa = 0
      do 20 iaux = 1, naux
        if (auxtype(iaux) .eq. "capacity") then
          if (mcapa .ne. 0) then
            write(*,*)" only 1 capacity allowed"
            stop
          else
            mcapa = iaux
          endif
        endif

c       # change to Version 4.1 terminology:
        if (auxtype(iaux) .eq. "leftface") auxtype(iaux) = "xleft"
        if (auxtype(iaux) .eq. "bottomface") auxtype(iaux) = "yleft"

        if (.not. (auxtype(iaux) .eq."xleft" .or.
     .             auxtype(iaux) .eq. "yleft".or.
     .             auxtype(iaux) .eq. "capacity".or.
     .             auxtype(iaux) .eq. "center"))  then
                  write(*,*)" unknown type for auxiliary variables"
                  write(*,*)" use  xleft/yleft/center/capacity"
                  stop
        endif
   20   continue

c     ::: error checking of input data :::::::::::::::::::::::

      if (mcapa .ne. mcapa1) then
         write(outunit,*) 'Error ***  mcapa does not agree with auxtype'
         write(*,*) 'Error ***  mcapa does not agree with auxtype'
         stop
      endif
      if (nvar .gt. maxvar) then
         write(outunit,*) 'Error ***   nvar > maxvar in common'
         write(*,*)       'Error ***   nvar > maxvar in common'
         stop
      endif
      if (2*nghost .gt. min(nx,ny)) then
         mindim = 2*nghost
         write(outunit,*) 'Error ***   need finer domain >',
     .         mindim, ' cells'
         write(*,*)       'Error ***   need finer domain >',
     .         mindim, ' cells'
         stop
      endif
      if (mcapa .gt. naux) then
         write(outunit,*) 'Error ***   mcapa > naux in input file'
         write(*,*)       'Error ***   mcapa > naux in input file'
         stop
      endif
      if (.not. vtime .and. nout .ne. 0) then
         write(outunit,*)' cannot specify output times with fixed dt'
         write(*,*)      ' cannot specify output times with fixed dt'
         stop
      endif
c
c
      if (ncarout)
     .   open(pltunit1,file=pltfile,status='unknown',form='formatted')
c

c     # call user routine to set up problem parameters:
      call setprob()
c
      matlabu   = 0
      hxposs(1) = (xupper - xlower) / nx
      hyposs(1) = (yupper - ylower) / ny
      cflmax = 0.d0
c
c
c
      if (rest) then
         open(rstunit,file=rstfile,status='old',form='unformatted')
         rewind rstunit
         call restrt(nsteps,time,nvar)
         nstart  = nsteps
      else
         lentot = 0
         lenmax = 0
         lendim = 0
         rvol   = 0.0d0
         do 8 i   = 1, mxnest
 8         rvoll(i) = 0.0d0
         evol   = 0.0d0
         call   stst1
         call   domain (nvar,vtime,nx,ny,naux)
         call   setgrd (nvar,cut,naux)
         time = 0.0d0
         nstart = 0
      endif
c
c  print out program parameters for this run
c
      write(outunit,107)tol,tolsp,iorder,kcheck,ibuff,nghost,cut,
     1            mxnest,ichkpt,cfl
      write(outunit,109) xupper,yupper,xlower,ylower,nx,ny,
     1             (intrat(i),i=1,mxnest)
      write(outunit,119) naux
      write(outunit,129) (iaux,auxtype(iaux),iaux=1,naux)
      if (mcapa .gt. 0) write(outunit,139) mcapa
107   format(/
     *       ' amrclaw parameters:',//,
     *       ' error tol            ',e12.5,/,
     *       ' spatial error tol    ',e12.5,/,
     *       ' order of integrator     ',i9,/,
     *       ' error checking interval ',i9,/,
     *       ' buffer zone size        ',i9,/,
     *       ' nghost                  ',i9,/,
     *       ' volume ratio cutoff  ',e12.5,/,
     *       ' max. refinement level   ',i9,/,
     *       ' user sub. calling times ',i9,/,
     *       ' cfl # (if var. delt) ',e12.5,/)
109   format(' xupper(upper corner) ',e12.5,/,
     *       ' yupper(upper corner) ',e12.5,/,
     *       ' xlower(lower corner) ',e12.5,/,
     *       ' ylower(lower corner) ',e12.5,/,
     *       ' nx = no. cells in x dir.',i9,/,
     *       ' ny = no. cells in y dir.',i9,/,
     *       ' refinement ratios       ',6i5,/,/)
119   format(' no. auxiliary vars.     ',i9)
129   format('       var ',i5,' of type ', a10)
139   format(' capacity fn. is aux. var',i9)
c
      write(6,*) ' '
      write(6,*) 'running amr2ez...  '
      write(6,*) ' '

      call outtre (mstart,printout,nvar,naux)
      call conck(1,nvar)
      call valout(1,lfine,time,nvar,naux)
c
c     --------------------------------------------------------
c     # tick is the main routine which drives the computation:
c     --------------------------------------------------------
      call tick(nvar,iout,nstart,nstop,cut,vtime,time,ichkpt,naux,
     &          nout,tout)
c     --------------------------------------------------------

c     # Done with computation, cleanup:


      lentotsave = lentot
      call cleanup(nvar,naux)
      if (lentot .ne. 0) then
        write(outunit,*) lentot," words not accounted for ",
     &                   "in memory cleanup"
        write(*,*)        lentot," words not accounted for ",
     &                   "in memory cleanup"
      endif
c
c report on statistics
c
      open(matunit,file=matfile,status='unknown',form='formatted')
      write(matunit,*) matlabu-1
      write(matunit,*) mxnest

      write(outunit,*)
      write(outunit,*)
      write(outunit,901) lentotsave
      write(outunit,902) lenmax
      write(outunit,903) lendim

      write(outunit,904) rvol
      do 60 level = 1,mxnest
 60     write(outunit,905) level, rvoll(level)

      write(outunit,906) evol
      if (evol+rvol .gt. 0.) then
         ratmet = rvol / (evol+rvol) * 100.0d0
      else
         ratmet = 0.0d0
      endif
      write(outunit,907) ratmet
      write(outunit,908) cflmax

 901  format('current  space usage = ',i12)
 902  format('maximum  space usage = ',i12)
 903  format('need space dimension = ',i12,/)
 904  format('number of cells advanced for time integration = ',f20.6)
 905  format(3x,'# cells advanced on level ',i4,' = ',f20.2)
 906  format('number of cells advanced for error estimation = ',f20.6,/)
 907  format(' percentage of cells advanced in time  = ', f10.2)
 908  format(' maximum Courant number seen = ', f10.2)
c
      write(outunit,909)
 909  format(//,' ------  end of AMRCLAW integration --------  ')
c
c
      stop
      end
c     ============================================
      subroutine b4step2(maxmx,maxmy,mbc,mx,my,meqn,q,
     &            xlower,ylower,dx,dy,time,dt,maux,aux)
c     ============================================
c
c     # called before each call to step
c     # use to set time-dependent aux arrays or perform other tasks.
c
c     # dummy routine 
c
c     
      implicit double precision (a-h,o-z)
      dimension q(1-mbc:maxmx+mbc,1-mbc:maxmy+mbc, meqn)
c     dimension aux(1-mbc:maxmx+mbc,1-mbc:maxmy+mbc, maux)
c
       return
       end
c
c -------------------------------------------------------------
c
       subroutine qad(valbig,mitot,mjtot,nvar,
     .                svdflx,qc1d,lenbc,lratio,hx,hy,
     .                maux,aux,auxc1d,delt,mptr)

       implicit double precision (a-h, o-z)

       include "call.i"

       logical qprint

       dimension valbig(mitot,mjtot,nvar)
       dimension qc1d(lenbc,nvar)
       dimension svdflx(nvar,lenbc)
       dimension aux(mitot,mjtot,maux)
       dimension auxc1d(lenbc,maux)

c
c ::::::::::::::::::::::::::: QAD ::::::::::::::::::::::::::::::::::
c  solve RP between ghost cell value on fine grid and coarse grid
c  value that ghost cell overlaps. The resulting fluctuations
c  are added in to coarse grid value, as a conservation fixup. 
c  Done each fine grid time step. If source terms are present, the
c  coarse grid value is advanced by source terms each fine time step too.

c Side 1 is the left side of the fine grid patch.  Then go around clockwise.
c ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c
c      # local storage
c      # note that dimension here are bigger than dimensions used
c      # in rp2, but shouldn't matter since wave is not used in qad
c      # and for other arrays it is only the last parameter that is wrong
c      #  ok as long as meqn, mwaves < maxvar

       parameter (max1dp1 = max1d+1)
       dimension ql(max1dp1,maxvar),    qr(max1dp1,maxvar)
       dimension wave(max1dp1,maxvar,maxvar),  s(max1dp1,maxvar)
       dimension amdq(max1dp1,maxvar),  apdq(max1dp1,maxvar)
       dimension auxl(max1dp1,maxaux),  auxr(max1dp1,maxaux)

       data qprint/.false./
c
c      aux is auxiliary array with user parameters needed in Riemann solvers
c          on fine grid corresponding to valbig
c      auxc1d is coarse grid stuff from around boundary, same format as qc1d
c      auxl, auxr are work arrays needed to pass stuff to rpn2
c      maux is the number of aux variables, which may be zero.
c

       if (qprint) write(dbugunit,*)" working on grid ",mptr
       tgrid = rnode(timemult, mptr)
       nc = mjtot-2*nghost
       nr = mitot-2*nghost
c
c--------
c  side 1
c--------
c
       do 10 j = nghost+1, mjtot-nghost
       if (maux.gt.0) then
          do 5 ma = 1,maux
             if (auxtype(ma).eq."xleft") then
c                # Assuming velocity at left-face, this fix
c                # preserves conservation in incompressible flow:
                 auxl(j-nghost+1,ma) = aux(nghost+1,j,ma)
               else
c                # Normal case -- we set the aux arrays 
c                # from the cell corresponding  to q
                 auxl(j-nghost+1,ma) = aux(nghost,j,ma)
               endif
  5          continue
          endif
       do 10 ivar = 1, nvar
         ql(j-nghost+1,ivar) = valbig(nghost,j,ivar)
 10    continue

       lind = 0
       index = 0
       ncrse = (mjtot-2*nghost)/lratio
       do 20 jc = 1, ncrse
         index = index + 1
         do 25 l = 1, lratio
         lind = lind + 1
         if (maux.gt.0) then
            do 24 ma=1,maux
               auxr(lind,ma) = auxc1d(index,ma)
   24          continue
            endif
         do 25 ivar = 1, nvar
 25         qr(lind,ivar) = qc1d(index,ivar)
 20    continue
       index = ncrse
    
       if (qprint) then
         write(dbugunit,*) 'side 1, ql and qr:'
         do i=2,nc
            write(dbugunit,4101) i,qr(i-1,1),ql(i,1)
          enddo
 4101      format(i3,4e16.6)
         if (maux .gt. 0) then
             write(dbugunit,*) 'side 1, auxr:'
             do i=2,nc
                write(dbugunit,4101) i,(auxr(i-1,ma),ma=1,maux)
                enddo
             write(dbugunit,*) 'side 1, auxl:'
             do i=2,nc
                write(dbugunit,4101) i,(auxl(i,ma),ma=1,maux)
                enddo
         endif
       endif
 
       call rpn2(1,max1dp1-2*nghost,nvar,mwaves,nghost,nc+1-2*nghost,
     .              ql,qr,auxl,auxr,wave,s,amdq,apdq)
c
c we have the wave. for side 1 add into sdflxm
c
       influx = 0
       do 30 j = 1, nc/lratio
          influx  = influx + 1
          jfine = (j-1)*lratio
          do 40 ivar = 1, nvar
            do 50 l = 1, lratio
              svdflx(ivar,influx) = svdflx(ivar,influx) 
     .                     + amdq(jfine+l+1,ivar) * hy * delt
     .                     + apdq(jfine+l+1,ivar) * hy * delt
 50         continue
 40       continue
 30    continue

c--------
c  side 2
c--------
c
       do 210 i = nghost+1, mitot-nghost
        if (maux.gt.0) then
          do 205 ma = 1,maux
             auxr(i-nghost,ma) = aux(i,mjtot-nghost+1,ma)
 205         continue
          endif
        do 210 ivar = 1, nvar
            qr(i-nghost,ivar) = valbig(i,mjtot-nghost+1,ivar)
 210    continue

       lind = 0
       ncrse = (mitot-2*nghost)/lratio
       do 220 ic = 1, ncrse
         index = index + 1
         do 225 l = 1, lratio
         lind = lind + 1
         if (maux.gt.0) then
            do 224 ma=1,maux
             if (auxtype(ma).eq."yleft") then
c                # Assuming velocity at bottom-face, this fix
c                # preserves conservation in incompressible flow:
                 ifine = (ic-1)*lratio + nghost + l
                 auxl(lind+1,ma) = aux(ifine,mjtot-nghost+1,ma)
               else
                 auxl(lind+1,ma) = auxc1d(index,ma)
               endif
  224          continue
            endif
         do 225 ivar = 1, nvar
 225         ql(lind+1,ivar) = qc1d(index,ivar)
 220    continue
    
       if (qprint) then
         write(dbugunit,*) 'side 2, ql and qr:'
         do i=1,nr
            write(dbugunit,4101) i,ql(i+1,1),qr(i,1)
            enddo
         if (maux .gt. 0) then
             write(dbugunit,*) 'side 2, auxr:'
             do i = 1, nr
                write(dbugunit,4101) i, (auxr(i,ma),ma=1,maux)
                enddo
             write(dbugunit,*) 'side 2, auxl:'
             do i = 1, nr
                write(dbugunit,4101) i, (auxl(i,ma),ma=1,maux)
                enddo
         endif
       endif
       call rpn2(2,max1dp1-2*nghost,nvar,mwaves,nghost,nr+1-2*nghost,
     .              ql,qr,auxl,auxr,wave,s,amdq,apdq)
c
c we have the wave. for side 2. add into sdflxp
c
       do 230 i = 1, nr/lratio
          influx  = influx + 1
          ifine = (i-1)*lratio
          do 240 ivar = 1, nvar
            do 250 l = 1, lratio
              svdflx(ivar,influx) = svdflx(ivar,influx) 
     .                     - amdq(ifine+l+1,ivar) * hx * delt
     .                     - apdq(ifine+l+1,ivar) * hx * delt
 250         continue
 240       continue
 230    continue

c--------
c  side 3
c--------
c
       do 310 j = nghost+1, mjtot-nghost
        if (maux.gt.0) then
          do 305 ma = 1,maux
             auxr(j-nghost,ma) = aux(mitot-nghost+1,j,ma)
 305         continue
          endif
        do 310 ivar = 1, nvar
          qr(j-nghost,ivar) = valbig(mitot-nghost+1,j,ivar)
 310      continue

       lind = 0
       ncrse = (mjtot-2*nghost)/lratio
       do 320 jc = 1, ncrse
         index = index + 1
         do 325 l = 1, lratio
         lind = lind + 1
         if (maux.gt.0) then
            do 324 ma=1,maux
             if (auxtype(ma).eq."xleft") then
c                # Assuming velocity at left-face, this fix
c                # preserves conservation in incompressible flow:
                 jfine = (jc-1)*lratio + nghost + l
                 auxl(lind+1,ma) = aux(mitot-nghost+1,jfine,ma)
               else
                 auxl(lind+1,ma) = auxc1d(index,ma)
               endif
  324          continue
            endif
         do 325 ivar = 1, nvar
 325         ql(lind+1,ivar) = qc1d(index,ivar)
 320    continue
    
       if (qprint) then
         write(dbugunit,*) 'side 3, ql and qr:'
         do i=1,nc
            write(dbugunit,4101) i,ql(i,1),qr(i,1)
            enddo
       endif
       call rpn2(1,max1dp1-2*nghost,nvar,mwaves,nghost,nc+1-2*nghost,
     .              ql,qr,auxl,auxr,wave,s,amdq,apdq)
c
c we have the wave. for side 3 add into sdflxp
C
       do 330 j = 1, nc/lratio
          influx  = influx + 1
          jfine = (j-1)*lratio
          do 340 ivar = 1, nvar
            do 350 l = 1, lratio
              svdflx(ivar,influx) = svdflx(ivar,influx) 
     .                     - amdq(jfine+l+1,ivar) * hy * delt
     .                     - apdq(jfine+l+1,ivar) * hy * delt
 350         continue
 340       continue
 330    continue

c--------
c  side 4
c--------
c
       do 410 i = nghost+1, mitot-nghost
        if (maux.gt.0) then
          do 405 ma = 1,maux
             if (auxtype(ma).eq."yleft") then
c                # Assuming velocity at bottom-face, this fix
c                # preserves conservation in incompressible flow:
                 auxl(i-nghost+1,ma) = aux(i,nghost+1,ma)
               else
                 auxl(i-nghost+1,ma) = aux(i,nghost,ma)
               endif
 405         continue
          endif
        do 410 ivar = 1, nvar
          ql(i-nghost+1,ivar) = valbig(i,nghost,ivar)
 410      continue

       lind = 0
       ncrse = (mitot-2*nghost)/lratio
       do 420 ic = 1, ncrse
         index = index + 1
         do 425 l = 1, lratio
         lind = lind + 1
         if (maux.gt.0) then
            do 424 ma=1,maux
               auxr(lind,ma) = auxc1d(index,ma)
  424          continue
            endif
         do 425 ivar = 1, nvar
 425         qr(lind,ivar) = qc1d(index,ivar)
 420    continue
    
       if (qprint) then
         write(dbugunit,*) 'side 4, ql and qr:'
         do i=1,nr
            write(dbugunit,4101) i, ql(i,1),qr(i,1)
            enddo
       endif
       call rpn2(2,max1dp1-2*nghost,nvar,mwaves,nghost,nr+1-2*nghost,
     .              ql,qr,auxl,auxr,wave,s,amdq,apdq)
c
c we have the wave. for side 4. add into sdflxm
c
       do 430 i = 1, nr/lratio
          influx  = influx + 1
          ifine = (i-1)*lratio
          do 440 ivar = 1, nvar
            do 450 l = 1, lratio
              svdflx(ivar,influx) = svdflx(ivar,influx) 
     .                     + amdq(ifine+l+1,ivar) * hx * delt
     .                     + apdq(ifine+l+1,ivar) * hx * delt
 450         continue
 440       continue
 430    continue

c      # for source terms:
       if (method(5) .ne. 0) then
           call src1d(nvar,nghost,lenbc,qc1d,maux,auxc1d,tgrid,delt)
           endif

       return
       end
c
c
c =========================================================
      subroutine src2(maxmx,maxmy,meqn,mbc,mx,my,xlower,ylower,dx,dy,
     &             q,maux,aux,t,dt)
c =========================================================
      implicit double precision (a-h,o-z)
      dimension   q(1-mbc:maxmx+mbc,1-mbc:maxmy+mbc, meqn)
      dimension aux(1-mbc:maxmx+mbc,1-mbc:maxmy+mbc, maux)
c
c     # dummy source routine... does nothing
c
      return
      end
c
c
c =========================================================
      subroutine src1d(meqn,mbc,mx1d,q1d,maux,aux1d,t,dt)
c =========================================================
      implicit double precision (a-h,o-z)
      dimension   q1d(mx1d, meqn)
      dimension aux1d(mx1d, maux)
c
c     # dummy source routine... does nothing
c
c     # This routine should be a simplified version of src2 
c     # which applies source terms for a 1-d slice of data along the
c     # edge of a grid.  This is called only from qad where the conservative
c     # fix-up is applied and is used to apply source terms over partial
c     # time steps to the coarse grid cell values used in solving Riemann 
c     # problems at the interface between coarse and fine grids.
c
c     # If the source terms depend only on q, it should be easy to 
c     # adapt src2 to create this routine, just loop over 1:mx1d.
c     # If the source terms are more complicated, it may not be easy.
c
c     # The code may work fine without applying source terms in this
c     # context, so using this dummy routine might be successful even when
c     # source terms are present. 
c
      return
      end
c
c --------------------------------------------------------------
c
      subroutine errf1(rctfine,nvar,rctcrse,mptr,mi2tot,mj2tot,
     2                 mitot,mjtot,rctflg,sperr)
      implicit double precision (a-h,o-z)

      include  "call.i"
 
      dimension  rctfine(mitot,mjtot,nvar)
      dimension  rctcrse(mi2tot,mj2tot,nvar)
      dimension  rctflg(mitot,mjtot,nvar)
      dimension  sperr(mitot,mjtot)
      logical    allowed
c
c
c ::::::::::::::::::::::::::::: ERRF1 ::::::::::::::::::::::::::::::::
c
c  compare error estimates in rctfine, rctcrse. If exceed tol, flag.
c  We put in a hook which allows us to ignore the error estimates
c  and not refine if we are outside some prescribed region.
c
c  sperr is the spatial component of the error estimate only
c  provides simple way for user to specify extra flagging in errsp
c
c ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c
c     # to allow refinement anywhere:
      allowed(x,y,level) = .true.
c
c     # This function can be changed to allow refinement only on some portion
c     # of the grid.
c     # This is useful if you wish to zoom in on some structure in a 
c     # known location but don't want the same level of refinement elsewhere.  
c     # Points are flagged only if one of the errors is greater than the 
c     # corresponding tolerance.
c
c     # For example, to allow refinement of Level 1 grids everywhere but
c     # of finer grids only for  y >= 0.4:
c
c     allowed(x,y,level) = 
c    &        (level.le.1 .or. y.ge.0.4d0) 
c
 
      time  = rnode(timemult, mptr)
      xleft = rnode(cornxlo,mptr)
      levm  = node(nestlevel, mptr)
      hx    = hxposs(levm)
      ybot  = rnode(cornylo,mptr)
      hy    = hyposs(levm)
      dt    = possk(levm)
      numsp = 0
 
      errmax = 0.0d0
      err2   = 0.0d0
c     order  = dt*float(2**(iorder+1) - 2)
      order  = float(2**(iorder+1) - 2)
c
      if (.not. (edebug)) go to 20
         write(outunit,107) mptr
 107     format(//,' coarsened grid values for grid ',i4)
         do 10 jj = nghost+1, mj2tot-nghost
            j = mj2tot + 1 - jj
            write(outunit,101) (rctcrse(i,j,1),
     .                          i = nghost+1, mi2tot-nghost)
10       continue
         write(outunit,108) mptr
 108     format(//, ' fine grid values for grid ',i4)
         do 15 jj = nghost+1, mjtot-nghost
            j = mjtot + 1 - jj
            write(outunit,101) (rctfine(i,j,1),i=nghost+1,mitot-nghost)
15       continue
101      format(' ',13f6.3)
c
c zero out the exterior locations so they don't affect err.est.
c
 20   continue
      jfine = nghost+1
      do 35  j = nghost+1, mj2tot-nghost
      yofj  = ybot + (float(jfine) - .5d0)*hy
      ifine = nghost+1
c
      do 30  i  = nghost+1, mi2tot-nghost
          rflag = goodpt
          xofi  = xleft + (float(ifine) - .5d0)*hx
          term1 = rctfine(ifine,jfine,1)
          term2 = rctfine(ifine+1,jfine,1)
          term3 = rctfine(ifine+1,jfine+1,1)
          term4 = rctfine(ifine,jfine+1,1)
c         # divide by (aval*order) for relative error
          aval  = (term1+term2+term3+term4)/4.d0
          est   =  dabs((aval-rctcrse(i,j,1))/ order)
          if (est .gt. errmax) errmax = est
          err2 = err2 + est*est
c         write(outunit,102) i,j,est
 102      format(' i,j,est ',2i5,e12.5)
c         rctcrse(i,j,2) = est
c
          if (est .ge. tol .and. allowed(xofi,yofj,levm)) then
             rflag  = badpt
          endif 
      rctcrse(i,j,1) = rflag
      ifine = ifine + 2
 30   continue
      jfine = jfine + 2
 35   continue
c
c  transfer flagged points on cell centered coarse grid
c  to cell centered fine grid. count flagged points.
c
c  initialize rctflg to 0.0 (no flags)  before flagging
c
      do 40 j = 1, mjtot
      do 40 i = 1, mitot
 40      rctflg(i,j,1) = goodpt
c
c  print out intermediate flagged rctcrse (for debugging)
c
      if (eprint) then
         err2 = dsqrt(err2/float((mi2tot-2*nghost)*(mj2tot-2*nghost)))
         write(outunit,103) mptr, levm, errmax, err2
 103     format(' grid ',i4,' level ',i4,
     .          ' max. error = ',e15.7,' err2 = ',e15.7)
         if (edebug) then
           write(outunit,*) ' flagged points on coarsened grid ',
     .                      'for grid ',mptr
           do 45 jj = nghost+1, mj2tot-nghost
              j = mj2tot + 1 - jj
              write(outunit,106) (nint(rctcrse(i,j,1)),
     .                            i=nghost+1,mi2tot-nghost)
106           format(1h ,80i1)
45         continue
         endif
      endif
c
      jfine   = nghost+1
      do 70 j = nghost+1, mj2tot-nghost
      ifine   = nghost+1
      do 60 i = nghost+1, mi2tot-nghost
         if (rctcrse(i,j,1) .eq. goodpt) go to 55
            rctflg(ifine,jfine,1)    = badpt
            rctflg(ifine+1,jfine,1)  = badpt
            rctflg(ifine,jfine+1,1)  = badpt
            rctflg(ifine+1,jfine+1,1)= badpt
 55       ifine   = ifine + 2
 60     continue
        jfine   = jfine + 2
 70   continue
c
      if (edebug) then
         write(outunit,*)" spatial error for grid ",mptr
         do 75 jjfine = nghost+1, mjtot-nghost
            jfine = mjtot + 1 - jjfine
            write(outunit,101)(sperr(ifine,jfine),
     .                         ifine=nghost+1,mitot-nghost)
 75      continue
      endif

      do 80 jfine = nghost+1, mjtot-nghost
      yofj  = ybot + (float(jfine) - nghost - .5d0)*hy
      do 80 ifine = nghost+1, mitot-nghost
        xofi  = xleft + (float(ifine) - nghost - .5d0)*hx
        if (sperr(ifine,jfine) .gt. tolsp .and. allowed(xofi,yofj,levm))
     &     then
               rflag = rctflg(ifine,jfine,1)
               if (rflag .ne. badpt) then
                 rctflg(ifine,jfine,1) = badpt
                 numsp = numsp + 1
               endif
          endif
 80   continue

      if (eprint) then
         write(outunit,118) numsp,mptr
 118     format( i5,' more pts. flagged for spatial error on grid',i4,/)
        if (edebug) then
          do 56 jj = nghost+1, mjtot-nghost
           j = mjtot + 1 - jj
           write(outunit,106)
     &      (nint(rctflg(i,j,1)),i=nghost+1,mitot-nghost)
 56       continue
        endif
      endif

      return
      end
c
c --------------------------------------------------------------
c
      subroutine advanc (level,nvar,dtlevnew,vtime,naux)
c
      implicit double precision (a-h,o-z)

      include  "call.i"

      logical    vtime

c
c  ::::::::::::::; ADVANC :::::::::::::::::::::::::::::::::::::::::::
c  integrate all grids at the input  'level' by one step of its delta(t)
c  this includes:  setting the ghost cells 
c                  advancing the solution on the grid
c                  adjusting fluxes for flux conservation step later
c :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c
      mptr = lstart(level)
      hx   = hxposs(level)
      hy   = hyposs(level)
      delt = possk(level)

 3    continue
          nx     = node(ndihi,mptr) - node(ndilo,mptr) + 1
          ny     = node(ndjhi,mptr) - node(ndjlo,mptr) + 1
          mitot  = nx + 2*nghost
          mjtot  = ny + 2*nghost
          locnew = node(store1,mptr)
          locaux = node(storeaux,mptr)
          time   = rnode(timemult,mptr)
c
          call bound(time,nvar,nghost,alloc(locnew),mitot,mjtot,mptr,
     1               alloc(locaux),naux)

        mptr = node(levelptr, mptr)
        if (mptr .ne. 0) go to 3
c
c save coarse level values if there is a finer level for wave fixup
      if (level+1 .le. mxnest) then
         if (lstart(level+1) .ne. null) then
            call saveqc(level+1,nvar,naux)
         endif
      endif
c
      dtlevnew = rinfinity
      mptr  = lstart(level)
 5    continue
          locold = node(store2, mptr)
          locnew = node(store1, mptr)
          nx     = node(ndihi,mptr) - node(ndilo,mptr) + 1
          ny     = node(ndjhi,mptr) - node(ndjlo,mptr) + 1
          time   = rnode(timemult,mptr)
c
          mitot  = nx + 2*nghost
          mjtot  = ny + 2*nghost
c         ::: get scratch storage for fluxes and slopes
          locfp = igetsp(mitot*mjtot*nvar)
          locfm = igetsp(mitot*mjtot*nvar)
          locgp = igetsp(mitot*mjtot*nvar)
          locgm = igetsp(mitot*mjtot*nvar)
c
c  copy old soln. values into  next time step's soln. values
c  since integrator will overwrite it. only for grids not at
c  the finest level. finest level grids do not maintain copies
c  of old and new time solution values.
c
          if (level .lt. mxnest) then
             ntot   = mitot * mjtot * nvar
cdir$ ivdep
             do 10 i = 1, ntot
 10            alloc(locold + i - 1) = alloc(locnew + i - 1)
          endif
c
      xlow = rnode(cornxlo,mptr) - nghost*hx
      ylow = rnode(cornylo,mptr) - nghost*hy
      rvol = rvol + nx * ny
      rvoll(level) = rvoll(level) + nx * ny
      locaux = node(storeaux,mptr)
c
      if (node(ffluxptr,mptr) .ne. 0) then
         lenbc  = 2*(nx/intrat(level-1)+ny/intrat(level-1))
         locsvf = node(ffluxptr,mptr)
         locsvq = locsvf + nvar*lenbc
         locx1d = locsvq + nvar*lenbc
         call qad(alloc(locnew),mitot,mjtot,nvar,
     1            alloc(locsvf),alloc(locsvq),lenbc,
     2            intrat(level-1),hx,hy,
     3            naux,alloc(locaux),alloc(locx1d),delt,mptr)
      endif
c
      call stepgrid(alloc(locnew),alloc(locfm),alloc(locfp),
     1            alloc(locgm),alloc(locgp),
     2            mitot,mjtot,nghost,
     3            delt,dtnew,hx,hy,nvar,
     4            xlow,ylow,time,mptr,vtime,naux,alloc(locaux))

      if (node(cfluxptr,mptr) .ne. 0)
     1   call fluxsv(mptr,alloc(locfm),alloc(locfp),
     2               alloc(locgm),alloc(locgp),
     3               alloc(node(cfluxptr,mptr)),mitot,mjtot,
     4               nvar,listsp(level),delt,hx,hy)
      if (node(ffluxptr,mptr) .ne. 0) then
         lenbc = 2*(nx/intrat(level-1)+ny/intrat(level-1))
         locsvf = node(ffluxptr,mptr)
         call fluxad(alloc(locfm),alloc(locfp),
     1               alloc(locgm),alloc(locgp),
     2               alloc(locsvf),mptr,mitot,mjtot,nvar,
     4               lenbc,intrat(level-1),nghost,delt,hx,hy)
      endif
c
          call reclam(locfp, mitot*mjtot*nvar)
          call reclam(locfm, mitot*mjtot*nvar)
          call reclam(locgp, mitot*mjtot*nvar)
          call reclam(locgm, mitot*mjtot*nvar)
c
          dtlevnew = dmin1(dtlevnew,dtnew)
c
          rnode(timemult,mptr)  = rnode(timemult,mptr)+delt
          mptr            = node(levelptr, mptr)
          if (mptr .ne. 0) go to 5
c
      return
      end
c
c --------------------------------------------------------------
c
      subroutine bound(time,nvar,ng,valbig,mitot,mjtot,mptr,
     1                 aux,naux)

c
      implicit double precision (a-h,o-z)

      include  "call.i"

      dimension valbig(mitot,mjtot,nvar), aux(mitot,mjtot,naux)

c
c  :::::::::::::: BOUND :::::::::::::::::::::::::::::::::::::::::::
c     This routine sets the boundary values for a given grid 
c     at level level.
c     We are setting the values for a strip ng zones wide all
c     the way around the border, in 4 rectangular strips.
c
c     Outputs from this routine:
c     The values around the border of the grid are inserted
c     directly into the enlarged valbig array.
c
c     This routine calls the routine filpatch
c     which for any block of mesh points on a given level,
c     intersects that block with all grids on that level and with
c     the physical boundaries, copies the values into the
c     appropriate intersecting regions, and interpolates the remaining
c     cells from coarser grids as required.
c :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

      xleft  = rnode(cornxlo, mptr)
      xright = rnode(cornxhi, mptr)
      ybot   = rnode(cornylo, mptr)
      ytop   = rnode(cornyhi, mptr)
      ilo    = node(ndilo, mptr)
      ihi    = node(ndihi, mptr)
      jlo    = node(ndjlo, mptr)
      jhi    = node(ndjhi, mptr)
      level  = node(nestlevel, mptr)
      hx     = hxposs(level)
      hy     = hyposs(level)


c     left boundary

      xl = xleft - ng*hx
      xr = xleft
      yb = ybot 
      yt = ytop

      if ((xl .lt. xlower) .and. xperdom) then
        call  prefilp(level,nvar,valbig,aux,naux,time,mitot,mjtot,
     1                1,ng+1,
     2                ilo-ng,ilo-1,jlo,jhi)
      else
        call filpatch(level,nvar,valbig,aux,naux,time,mitot,mjtot,
     1                1,ng+1,
     2                ilo-ng,ilo-1,jlo,jhi)
      endif


c     right boundary

      xl = xright
      xr = xright + ng*hx
      yb = ybot
      yt = ytop

      if ((xr .gt. xupper) .and. xperdom) then
        call  prefilp(level,nvar,valbig,aux,naux,time,mitot,mjtot,
     1                mitot-ng+1,ng+1,
     2                ihi+1,ihi+ng,jlo,jhi)
      else
        call filpatch(level,nvar,valbig,aux,naux,time,mitot,mjtot,
     1                mitot-ng+1,ng+1,
     2                ihi+1,ihi+ng,jlo,jhi)
      endif


c       bottom boundary
        xl = xleft  - ng*hx
        xr = xright + ng*hx
        yb = ybot - ng*hy
        yt = ybot
        if (((yb .lt. ylower) .and. yperdom) .or. 
     1   (((xl .lt. xlower) .or. (xr .gt. xupper)) .and. xperdom) ) then
           call prefilp(level,nvar,valbig,aux,naux,time,mitot,mjtot,
     1                   1,1,
     2                   ilo-ng,ihi+ng,jlo-ng,jlo-1)
        else
           call filpatch(level,nvar,valbig,aux,naux,time,mitot,mjtot,
     1                   1,1,
     2                   ilo-ng,ihi+ng,jlo-ng,jlo-1)
        endif

c       top boundary
        xl = xleft - ng*hx
        xr = xright + ng*hx
        yb = ytop
        yt = ytop + ng*hy
        if (((yt .gt. yupper) .and. yperdom) .or. 
     1   (((xl .lt. xlower) .or. (xr .gt. xupper)) .and. xperdom) ) then
          call prefilp(level,nvar,valbig,aux,naux,time,mitot,mjtot,
     1                  1,mjtot-ng+1,
     2                  ilo-ng,ihi+ng,jhi+1,jhi+ng)
        else
          call filpatch(level,nvar,valbig,aux,naux,time,mitot,mjtot,
     1                  1,mjtot-ng+1,
     2                  ilo-ng,ihi+ng,jhi+1,jhi+ng)
        endif

c
c external boundary conditions
c
      if (.not. (xperdom .and. yperdom)) then
        xl = xleft  - ng*hx
        yb = ybot   - ng*hy
        xr = xright + ng*hx
        yt = ytop   + ng*hy

        call bc2amr( valbig,aux,mitot,mjtot,nvar,naux,
     1               hx,hy,level,time,xl,xr,yb,yt,
     3               xlower,ylower,xupper,yupper,
     4               xperdom,yperdom)
      endif
c
      return
      end
c
c -------------------------------------------------------------
c
      subroutine stepgrid(q,fm,fp,gm,gp,mitot,mjtot,mbc,dt,dtnew,dx,dy,
     &                  nvar,xlow,ylow,time,mptr,vtime,maux,aux)
c
c          
c ::::::::::::::::::: STEPGRID ::::::::::::::::::::::::::::::::::::
c take a time step on a single grid. overwrite solution array q. 
c A modified version of the clawpack routine step2 is used.
c
c return fluxes in fm,fp and gm,gp.
c patch has room for ghost cells (mbc of them) around the grid.
c everything is the enlarged size (mitot by mjtot).
c
c mbc       = number of ghost cells  (= lwidth)
c mptr      = grid number  (for debugging)
c xlow,ylow = lower left corner of enlarged grid (including ghost cells).
c dt         = incoming time step
c dx,dy      = mesh widths for this grid
c dtnew      = return suggested new time step for this grid's soln.
c
c :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

      implicit double precision (a-h,o-z)
      external rpn2,rpt2

      include  "call.i"
      common /comxyt/ dtcom,dxcom,dycom,tcom,icom,jcom

      parameter (msize=max1d+4)
      parameter (mwork=msize*(maxvar*maxvar + 13*maxvar + 3*maxaux +2))

      dimension q(mitot,mjtot,nvar)
      dimension fp(mitot,mjtot,nvar),gp(mitot,mjtot,nvar)
      dimension fm(mitot,mjtot,nvar),gm(mitot,mjtot,nvar)
      dimension aux(mitot,mjtot,maux)
      dimension work(mwork)

      logical    debug, vtime, dump
      data       debug/.false./,  dump/.false./
c
c     # set tcom = time.  This is in the common block comxyt that could
c     # be included in the Riemann solver, for example, if t is explicitly
c     # needed there.

      tcom = time

      if (dump) then
         do i = 1, mitot
         do j = 1, mjtot
            write(outunit,545) i,j,(q(i,j,ivar),ivar=1,nvar)
 545        format(2i4,4e15.7)
         end do
         end do
      endif
c
      meqn   = nvar
      mx = mitot - 2*mbc
      my = mjtot - 2*mbc
      maxm = max(mx,my)       !# size for 1d scratch array
      mbig = maxm
      xlowmbc = xlow + mbc*dx
      ylowmbc = ylow + mbc*dy

c     # method(2:7) and mthlim
c     #    are set in the amr2ez file (read by amr)
c
      method(1) = 0
c
c
c     # fluxes initialized in step2
c
      mwork0 = (maxm+2*mbc)*(12*meqn + mwaves + meqn*mwaves + 2) 
c
      if (mwork .lt. mwork0) then
         write(outunit,*) 'CLAW2 ERROR... mwork must be increased to ',
     &               mwork0
         write(*      ,*) 'CLAW2 ERROR... mwork must be increased to ',
     &               mwork0
         stop
      endif
c
c     # partition work array into pieces needed for local storage in 
c     # step2 routine. Find starting index of each piece:
c
      i0faddm = 1
      i0faddp = i0faddm + (maxm+2*mbc)*meqn
      i0gaddm = i0faddp + (maxm+2*mbc)*meqn
      i0gaddp = i0gaddm + 2*(maxm+2*mbc)*meqn
      i0q1d   = i0gaddp + 2*(maxm+2*mbc)*meqn 
      i0dtdx1 = i0q1d + (maxm+2*mbc)*meqn  
      i0dtdy1 = i0dtdx1 + (maxm+2*mbc)
      i0aux1 = i0dtdy1 + (maxm+2*mbc)
      i0aux2 = i0aux1 + (maxm+2*mbc)*maux
      i0aux3 = i0aux2 + (maxm+2*mbc)*maux
c
c
      i0next = i0aux3 + (maxm+2*mbc)*maux    !# next free space
      mused  = i0next - 1                    !# space already used
      mwork1 = mwork - mused              !# remaining space (passed to step2)

c
c
      call b4step2(mx,my,mbc,mx,my,nvar,q,
     &             xlowmbc,ylowmbc,dx,dy,time,dt,maux,aux)
c
c
c     # take one step on the conservation law:
c
      call step2(mbig,mx,my,nvar,maux,
     &           mbc,mx,my,
     &              q,aux,dx,dy,dt,cflgrid,
     &              fm,fp,gm,gp,
     &              work(i0faddm),work(i0faddp),
     &              work(i0gaddm),work(i0gaddp),
     &              work(i0q1d),work(i0dtdx1),work(i0dtdy1),
     &              work(i0aux1),work(i0aux2),work(i0aux3),
     &              work(i0next),mwork1,rpn2,rpt2)
c
c
        write(outunit,*) ' Courant # of grid ',mptr, '  is  ',cflgrid
c
        cflmax = dmax1(cflmax,cflgrid)
c
c       # update q
        dtdx = dt/dx
        dtdy = dt/dy
        do 50 m=1,nvar
        do 50 i=mbc+1,mitot-mbc
        do 50 j=mbc+1,mjtot-mbc
         if (mcapa.eq.0) then
c
c            # no capa array.  Standard flux differencing:

           q(i,j,m) = q(i,j,m) 
     &           - dtdx * (fm(i+1,j,m) - fp(i,j,m)) 
     &           - dtdy * (gm(i,j+1,m) - gp(i,j,m)) 
         else
c            # with capa array.
           q(i,j,m) = q(i,j,m) 
     &           - (dtdx * (fm(i+1,j,m) - fp(i,j,m))
     &           + dtdy * (gm(i,j+1,m) - gp(i,j,m))) / aux(i,j,mcapa)
         endif

 50      continue
c
c
      if (method(5).eq.1) then
c        # with source term:   use Godunov splitting
         call src2(mx,my,nvar,mbc,mx,my,xlower,ylower,dx,dy,
     &             q,maux,aux,time,dt)
         endif
c
c
c
c     # output fluxes for debugging purposes:
      if (debug) then
         write(dbugunit,*)" fluxes for grid ",mptr
         do 830 i = mbc+1, mitot-1
            do 830 j = mbc+1, mjtot-1
               write(dbugunit,831) i,j,fm(i,j,1),fp(i,j,1),
     .                             gm(i,j,1),gp(i,j,1)
               do 830 m = 2, meqn
                  write(dbugunit,832) fm(i,j,m),fp(i,j,m),
     .            gm(i,j,m),gp(i,j,m)
  831          format(2i4,4d16.6)
  832          format(8x,4d16.6)
  830    continue
      endif

c
c
c For variable time stepping, use max speed seen on this grid to 
c choose the allowable new time step dtnew.  This will later be 
c compared to values seen on other grids.
c
      if (vtime) then
         if (cflgrid .gt. 0.d0) then
             dtnew = dt*cfl/cflgrid
           else
c            # velocities are all zero on this grid so there's no 
c            # time step restriction coming from this grid.
             dtnew = rinfinity
           endif
      else
         dtnew = dt
      endif

c     # give a warning if Courant number too large...
c
      if (cflgrid .gt. cflv1) then
            write(*,810) cflgrid
            write(outunit,810) cflgrid
  810       format('*** WARNING *** Courant number  =', d12.4,
     &              '  is larger than cflv(1) ')
            endif
c
      return
      end


c
c ----------------------------------------------------------------
c
       subroutine auxcoarsen(auxdub,midub,mjdub,auxbgc,
     1                       mi2tot,mj2tot,naux,auxtype)
      
       implicit double precision (a-h, o-z)

       dimension     auxdub(midub, mjdub, naux)
       dimension     auxbgc(mi2tot,mj2tot,naux)
       character*10  auxtype(naux)

c :::::::::::::::::::::::: COARSEN ::::::::::::::::::::::::::::::::
c coarsen = coarsen the fine grid auxiliary data (with double the usual
c           number of ghost cells to prepare coarsened data
c           for error estimation.
c :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

       do 50 iaux = 1, naux

       if (auxtype(iaux) .eq. "center" .or. 
     .     auxtype(iaux) .eq. "capacity") then
         do 20 j = 1, mj2tot
            jfine = 2*(j-1) + 1
            do 20 i = 1, mi2tot
               ifine = 2*(i-1) + 1
               auxbgc(i,j,iaux) = (auxdub(ifine,jfine,iaux) +
     &                             auxdub(ifine+1,jfine,iaux)+
     &                             auxdub(ifine,jfine+1,iaux) +
     &                             auxdub(ifine+1,jfine+1,iaux))/4.d0
20       continue

       elseif (auxtype(iaux) .eq. "xleft") then 
         do 10 j = 1, mj2tot
            jfine = 2*(j-1) + 1
            do 10 i = 1, mi2tot
               ifine = 2*(i-1) + 1
               auxbgc(i,j,iaux) = (auxdub(ifine,jfine,iaux) +
     &                          auxdub(ifine,jfine+1,iaux)) /2.d0 
10       continue

       elseif (auxtype(iaux) .eq. "yleft") then 
         do 15 j = 1, mj2tot
            jfine = 2*(j-1) + 1
            do 15 i = 1, mi2tot
               ifine = 2*(i-1) + 1
               auxbgc(i,j,iaux) = (auxdub(ifine,jfine,iaux) +
     &                             auxdub(ifine+1,jfine,iaux))/2.d0
15       continue

       endif

50     continue

       return
       end
c
        subroutine fixcapaq(val,aux,mitot,mjtot,valc,auxc,mic,mjc,
     &                      nvar,naux,lratio)

      implicit double precision (a-h,o-z)

      include "call.i"
c
c :::::::::::::::::::::::  FIXCAPAQ ::::::::::::::::::::::::::::::
c  new fine grid solution q was linearly interpolated. but want
c  to conserve kappa*q, not q. calculate the discrepancy
c  in kappa*q using this q, and modify q to account for it.
c ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

      dimension   val(mitot,mjtot,nvar), valc(mic,mjc,nvar)
      dimension   aux(mitot,mjtot,naux), auxc(mic,mjc,naux)

      dcapamax = 0.d0

      do 10 ic = 2, mic-1
      do 10 jc = 2, mjc-1


       do 15 ivar = 1, nvar

       capaqfine = 0.d0

       do 20 ico = 1, lratio
       ifine = (ic-2)*lratio + nghost + ico
       do 20 jco = 1, lratio
         jfine = (jc-2)*lratio + nghost + jco
         capaqfine = capaqfine + aux(ifine,jfine,mcapa)*
     &                           val(ifine,jfine,ivar)
20     continue

       dcapaq = auxc(ic,jc,mcapa)*valc(ic,jc,ivar)-
     &          capaqfine/(lratio*lratio)
       dcapamax = dmax1(dcapamax,dabs(dcapaq))
      
       do 30 ico = 1, lratio
       ifine = (ic-2)*lratio + nghost + ico
       do 30 jco = 1, lratio
         jfine = (jc-2)*lratio + nghost + jco
         val(ifine,jfine,ivar) = val(ifine,jfine,ivar) +
     &                           dcapaq/aux(ifine,jfine,mcapa)
30     continue

15     continue

10     continue

c      write(6,*)" max discrepancy ", dcapamax

       return
       end
c
c-----------------------------------------------------------------------
c
       subroutine estdt(val,mitot,mjtot,nvar,dx,dy,dt,nghost,aux,naux)
c
c :::::::::::::::::::::::: ESTDT :::::::::::::::::::::::::::;
c  estimate the initial time step for the given values
c ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::;

       implicit double precision (a-h, o-z)
       dimension val(mitot,mjtot,nvar)
       dimension aux(mitot,mjtot,naux)
c
c
       return
       end
c
c ----------------------------------------------------------
c
      function igetsp (nwords)
c
      implicit double precision (a-h,o-z)

      include  "call.i"
c
c ::::::::::::::::::::::::::: IGETSP ::::::::::::::::::::::::::::
c
c  allocate contiguous space of length nword in main storage array
c  alloc. user code (or pointer to the owner of this storage)
c  is  mptr.  lenf = current length of lfree list.
c
c ::::::::::::::::::::::::::: IGETSP ::::::::::::::::::::::::::::
c
c  find first fit from free space list
c
      itake = 0
      do 20 i = 1, lenf
         if (lfree(i,2) .lt. nwords) go to 20
         itake = i
         go to 25
 20   continue
      go to 900
c
c  anything left?
c
 25   left = lfree(itake,2) - nwords
      igetsp = lfree(itake,1)
      iendtake = lfree(itake,1) + nwords
      if (lendim .lt. iendtake) lendim = iendtake
c
c  the following code which is ignored for now adds the new
      if (left .le. 0) go to 30
      lfree(itake,2) = left
      lfree(itake,1) = iendtake
      go to 99
c
c  item is totally removed.  move next items in list up one.
c
 30   lenf = lenf - 1
      do 40 i = itake, lenf
      lfree(i,1) = lfree(i+1,1)
 40   lfree(i,2) = lfree(i+1,2)
      go to 99
c
 900  write(outunit,901) nwords
      write(*,901)       nwords
 901  format('  require ',i10,' words - either none left or not big',
     1         '  enough space')
      write(outunit,902) ((lfree(i,j),j=1,2),i=1,lenf)
      write(*,902)       ((lfree(i,j),j=1,2),i=1,lenf)
 902  format(' free list: ',//,2x,50(i10,4x,i10,/,2x))
      stop
c
 99   lentot = lentot + nwords
      if (lenmax .lt. lentot) lenmax = lentot
      if (sprint) write(outunit,100) nwords, igetsp, lentot, lenmax
 100  format('  allocating ',i8,' words in location ',i8,
     1       ' lentot, lenmax ', 2i10)
      return
      end
c
c  -----------------------------------------------------------
c
      subroutine reclam (index, nwords)
c
c ::::::::::::::::::::::::: RECLAM :::::::::::::::::::::::::::
c
c  return of space. add to free list.
c  iplace points to next item on free list with larger index than
c  the item reclaiming, unless said item is greater then
c  everything on the list.
c
c ::::::::::::::::::::::::::::::::::;:::::::::::::::::::::::::
c
      implicit double precision (a-h,o-z)

      include  "call.i"

c
      do 20 i = 1, lenf
      iplace  = i
      if (lfree(i,1) .gt. index) go to 30
 20   continue
         write(outunit,902)
         write(*,902)
 902     format(' no insertion pointer into freelist. error stop')
         stop
c
c  check previous segment for merging
c
 30      iprev = iplace - 1
         if (lfree(iprev,1)+lfree(iprev,2) .lt. index) go to 40
         lfree(iprev,2) = lfree(iprev,2) + nwords
         go to 50
c
c  check after segment - no previous merge case
c
 40   nexti = index + nwords
      if (lfree(iplace,1).ne. nexti) go to 70
         lfree(iplace,1) = index
         lfree(iplace,2) = lfree(iplace,2) + nwords
         go to 99
c
c  check following segment - yes previous merge case
c
 50   nexti = index + nwords
      if (lfree(iplace,1) .ne. nexti) go to 99
c
c forward merge as well, bump all down 1
c
      lfree(iprev,2) = lfree(iprev,2)+lfree(iplace,2)
      ipp1           = iplace + 1
         do 60 i = ipp1, lenf
         lfree(i-1,1) = lfree(i,1)
 60      lfree(i-1,2) = lfree(i,2)
         lenf = lenf - 1
         go to 99
c
c  no merges case - insert and bump future segments up to make room
c
 70   if (lenf .eq. lfdim) go to 900
      do 80 ii = iplace, lenf
      i          = lenf + 1 - ii + iplace
      lfree(i,1) = lfree(i-1,1)
 80   lfree(i,2) = lfree(i-1,2)
      lenf            = lenf + 1
      lfree(iplace,1) = index
      lfree(iplace,2) = nwords
      go to 99
c
 900  write(outunit,901) lfdim
      write(*,901)       lfdim
 901  format('  free list full with ',i5,' items')
      stop
c
 99   lentot = lentot - nwords
      if (sprint) write(outunit,100) nwords, index, lentot
 100  format('     reclaiming ',i8,' words at loc. ',i8,' lentot ',i10)
      return
      end
c
c --------------------------------------------------
c
      subroutine birect(mptr1)
c
      implicit double precision (a-h,o-z)

      include  "call.i"

c
c :::::::::::::  BIRECT :::::::::::::::::::::::::::::::::::::::
c check each grid, starting with mptr1 (either newstl or lstart)
c to see that it has no more than max1d points in either dimensions.
c needed so that scratch array space in stepgrid not exceeded.
c
c also check for too small grids - but has never happened.
c :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c
      mptr  = mptr1
      level = node(nestlevel,mptr)
      hx    = hxposs(level)
      hy    = hyposs(level)
c
10    continue
      cxlo    = rnode(cornxlo,mptr)
      cxhi    = rnode(cornxhi,mptr)
      cylo    = rnode(cornylo,mptr)
      cyhi    = rnode(cornyhi,mptr)
      nx      = node(ndihi,mptr) - node(ndilo,mptr) + 1
      ny      = node(ndjhi,mptr) - node(ndjlo,mptr) + 1
      minsize = 2*nghost
c
c check number of rows first - if too many, bisect grid with vertical
c line down the middle. make sure new grid corners are anchored
c on coarse grid point. make sure if bisecting coarse grid that
c new grids have even number of points
c
      if (nx + 2*nghost .gt. max1d) then
 
        nxl    = nx/2
        if (level .gt. 1) then 
           lratio = intrat(level-1)
        else
           lratio = 2
        endif
        nxl = (nxl/lratio)*lratio 
        nxr    = nx - nxl 
        cxmid  = cxlo + nxl*hx
 
        mptrnx = nodget(dummy)
        node(levelptr,mptrnx) = node(levelptr,mptr)
        node(levelptr,mptr)   = mptrnx
 
        rnode(cornxhi,mptr) = cxmid
        node(ndihi,mptrnx)  = node(ndihi,mptr)
        node(ndihi,mptr)    = node(ndilo,mptr) + nxl - 1
        node(ndilo,mptrnx)  = node(ndihi,mptr) + 1
        node(ndjhi,mptrnx)  = node(ndjhi,mptr)
        node(ndjlo,mptrnx)  = node(ndjlo,mptr)

        rnode(cornxlo,mptrnx)    = cxmid
        rnode(cornylo,mptrnx)    = cylo
        rnode(cornyhi,mptrnx)    = cyhi
        rnode(cornxhi,mptrnx)    = cxhi
        rnode(timemult,mptrnx)   = rnode(timemult,mptr)
        node(nestlevel,mptrnx)   = node(nestlevel,mptr)

        go to 10
c
c check number of columns next - if too many, bisect grid with horizontal
c line down the middle
c
      else if (ny + 2*nghost .gt. max1d) then
 
        nyl    = ny/2
        if (level .gt. 1) then 
           lratio = intrat(level-1)
        else
           lratio = 2
        endif
        nyl = (nyl/lratio)*lratio
        nyr    = ny - nyl 
        cymid  =  cylo + nyl*hy
 
        mptrnx = nodget(dummy)
        node(levelptr,mptrnx) = node(levelptr,mptr)
        node(levelptr,mptr)   = mptrnx
 
        rnode(cornyhi,mptr)   = cymid

        node(ndjhi,mptrnx) = node(ndjhi,mptr)
        node(ndjhi,mptr)   = node(ndjlo,mptr) + nyl - 1
        node(ndjlo,mptrnx) = node(ndjhi,mptr) + 1
        node(ndihi,mptrnx) = node(ndihi,mptr)
        node(ndilo,mptrnx) = node(ndilo,mptr)

        rnode(cornxlo,mptrnx)   = cxlo
        rnode(cornylo,mptrnx)   = cymid
        rnode(cornyhi,mptrnx)   = cyhi
        rnode(cornxhi,mptrnx)   = cxhi
        node(nestlevel,mptrnx)  = node(nestlevel,mptr)
        rnode(timemult,mptrnx)  = rnode(timemult,mptr)
        go to 10
c
c  grid ok - check the next
c
      else
        mptr = node(levelptr,mptr)
        if (mptr.ne.0) go to 10
 
      endif
 
      return
      end
c
c ---------------------------------------------------------
c
      subroutine check(nsteps,time,nvar,naux)
c
c :::::::::::::::::::::: CHECK ::::::::::::::::::::::::::::::::;
c   check point routine - can only call at end of coarse grid cycle
c :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::;

      implicit double precision (a-h,o-z)
      character  chkname*12
      include  "call.i"
c
c     ###  make the file name showing the time step
c
      chkname = 'fort.chkxxxx'
      nstp = nsteps
      do 20 ipos = 12, 9, -1
         idigit = mod(nstp,10)
         chkname(ipos:ipos) = char(ichar('0') + idigit)
         nstp = nstp / 10
 20   continue
      open(unit=chkunit,file=chkname,status='unknown',
     .     form='unformatted')
c
c     ###  dump the data
c
      write(chkunit) lenmax,lendim,memsize,(alloc(i),i=1,lendim)
      write(chkunit) hxposs,hyposs,possk,icheck
      write(chkunit) lfree,lenf
      write(chkunit) rnode,node,lstart,newstl,listsp,tol,
     1          ibuff,mstart,ndfree,lfine,iorder,mxnest,
     2          intrat,iregsz,jregsz,kcheck,nsteps,time,matlabu
      write(chkunit) evol, rvol, rvoll, lentot
c
      close(chkunit)
c
      return
      end
c
c ---------------------------------------------------------
c
      subroutine cleanup(nvar,naux)
c
c :::::::::::::::::::::: CLEANUP ::::::::::::::::::::::::::::::::;
c   this is just a check to make sure all storage was accounted for.
c   routine is called after all the data has been checkpointed.
c :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::;

      implicit double precision (a-h,o-z)
      include  "call.i"
c
c      ## clean up storage to double check that everything taken care of
c      ## done after the checkpoint so pointers sitll work on restart
       do  120 level = 1, lfine
         call putsp(1,level,nvar,naux)
         mptr =  lstart(level)
 110        nx      = node(ndihi,mptr) - node(ndilo,mptr) + 1
            ny      = node(ndjhi,mptr) - node(ndjlo,mptr) + 1
            mitot  = nx + 2*nghost
            mjtot  = ny + 2*nghost
            nwords  = mitot*mjtot*nvar
            call reclam(node(store1, mptr), nwords)
            if (level .lt. mxnest) 
     .         call reclam(node(store2, mptr), nwords)
            if (naux .gt. 0) 
     .         call reclam(node(storeaux, mptr), mitot*mjtot*naux)
        mptr = node(levelptr, mptr)
        if (mptr .ne. 0) go to 110
120    continue 

      return
      end
c
c -----------------------------------------------------------
c
      subroutine colate (badpts, len, lcheck, 
     1                   iflags,domflags,isize,jsize,npts)
c
      implicit  double precision (a-h,o-z)
      dimension badpts(2,len)
      integer   iflags  (0:isize+1,0:jsize+1)
      integer   domflags(0:isize+1,0:jsize+1)

      include  "call.i"
c
c
c *************************************************************
c
c colate = takes the error plane with flagged pts at level lcheck
c          and puts their (i,j) cell centered
c          indices into the badpts array.
c          To insure proper nesting,  get rid of flagged point
c          that don't fit into properly nested domain (in iflags2)
c
c *************************************************************
c
c     # if pt. flagged but domain not flagged, turn it off
c     # note that this results in flags of 1,  not 2 of 3.


      if (dprint) then
         write(outunit,*)" from colate: iflags"
         do 48 jj = 1, jsize
           j = jsize + 1 - jj
           write(outunit,101)(iflags(i,j),i=1,isize)
 48      continue
         write(outunit,*)" from colate: domflags"
         do 49 jj = 1, jsize
           j = jsize + 1 - jj
           write(outunit,101)(domflags(i,j),i=1,isize)
 101       format(80i1)
 49      continue
      endif

      do 10 j = 1, jsize 
      do 10 i = 1, isize 
        iflags(i,j) = min(iflags(i,j),domflags(i,j))
 10   continue


      index  = 0
c
c     give points the indices from integer region space.
      do 20 j   = 1, jsize
      do 20 i   = 1, isize
        if (iflags(i,j) .ne. goodpt) then
          index = index + 1
          badpts(1,index) = float(i)-.5
          badpts(2,index) = float(j)-.5
        endif
 20   continue
c
 99   npts = index 
      if (gprint) then
        write(outunit,100) npts, lcheck
 100    format( i5,' flagged points colated on level ',i4)
      endif

      return
      end
c
c -------------------------------------------------------------
c
      subroutine errest (nvar,naux,numbad,lcheck,iflags,isize,jsize)
c
      implicit double precision (a-h,o-z)

      include  "call.i"

      dimension  iflags (0:isize+1,0:jsize+1)
      logical    vtime
      data       vtime/.false./
c
c     #   no sense computing new time step if just for error estimation,
c     #   so vtime set to false here.
 
c :::::::::::::::::::::::::: ERREST :::::::::::::::::::::::::::::::::::
c for all grids at level lcheck:
c  estimate the error by taking a large (2h,2k) step based on the
c  values in the old storage loc., then take one regular (and for
c  now wasted) step based on the new info.   compare using an
c  error relation for a pth order  accurate integration formula.
c  flag error plane as either bad (needs refinement), or good.
c
c  call the regular integrator on a grid twice as coarse.
c  initialize such a grid directly, instead of trick dimensioning.
c  this is to make other l1 type estimates easier to program.
c :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c
       numbad = 0
       do 4 i = 1, isize
       do 4 j = 1, jsize
 4        iflags(i,j) = 0
c
       hx   = hxposs(lcheck)
       hy   = hyposs(lcheck)
       hx2  = 2.d0*hx
       hy2  = 2.d0*hy
       mptr = lstart(lcheck)
 5     continue
          nx     = node(ndihi,mptr) - node(ndilo,mptr) + 1
          ny     = node(ndjhi,mptr) - node(ndjlo,mptr) + 1
          mitot  = nx + 2*nghost
          mjtot  = ny + 2*nghost
          locnew = node(store1,mptr)
          locold = node(store2,mptr)
          locaux = node(storeaux,mptr)
          mi2tot = nx/2  + 2*nghost
          mj2tot = ny/2  + 2*nghost
          time   = rnode(timemult,mptr)
          dt     = possk(lcheck)
          tpre   = time - dt
c
c     prepare double the stencil size worth of boundary values,
c            then coarsen them for the giant step integration.
c 
          midub = nx+4*nghost
          mjdub = ny+4*nghost
          locdub = igetsp(midub*mjdub*(nvar+naux))
          locbgc = igetsp(mi2tot*mj2tot*(nvar+naux))
          node(errptr,mptr) = locbgc
          ng2 = 2*nghost

c         # transfer soln. into grid with twice the ghost cells
          call copysol(alloc(locdub),alloc(locold),nvar,mitot,mjtot,
     1              nghost,midub,mjdub,ng2)

c
          if (naux .gt. 0) then
              locaxb = locdub + midub*mjdub*nvar
              locaxc = locbgc + mi2tot*mj2tot*nvar
              xl     = rnode(cornxlo, mptr)
              yb     = rnode(cornylo, mptr)
              maxmx = midub - 4*nghost
              mx = maxmx
              maxmy = mjdub - 4*nghost
              my = maxmy
              call setaux(maxmx,maxmy,2*nghost,mx,my,xl,yb,hx,hy,
     &                    naux,alloc(locaxb))
              call auxcoarsen(alloc(locaxb),midub,mjdub,
     1                     alloc(locaxc),mi2tot,mj2tot,naux,auxtype)
          else
              locaxb = 1
          endif

c         # fill it - use enlarged (before coarsening) aux arrays
          call bound(tpre,nvar,ng2,alloc(locdub),midub,mjdub,mptr,
     1               alloc(locaxb),naux)

c         coarsen by 2 in every direction
          call coarsen(alloc(locdub),midub,mjdub,
     1                 alloc(locbgc),mi2tot,mj2tot,nvar)
          call reclam(locdub,midub*mjdub*(nvar+naux))
c
c
c We now fill bndry values at time t = time, in preparation
c for calculating the solution on the grid mptr for error estimation.
c 
          locbig = igetsp(mitot*mjtot*nvar)
          node(tempptr,mptr) = locbig
c         # straight copy into scratch array so don't mess up latest soln.
          do 10 i = 1, mitot*mjtot*nvar
 10          alloc(locbig+i-1) = alloc(locnew+i-1)

          call bound(time,nvar,nghost,alloc(locbig),mitot,mjtot,mptr,
     1               alloc(locaux),naux)

c
       mptr = node(levelptr,mptr)
       if (mptr .ne. 0) go to 5
c
       hx   = hxposs(lcheck)
       hy   = hyposs(lcheck)
       hx2  = 2.d0*hx
       hy2  = 2.d0*hy
       dt   = possk(lcheck)
       dt2    = 2. * dt

       mptr = lstart(lcheck)
 25    continue
          nx     = node(ndihi,mptr) - node(ndilo,mptr) + 1
          ny     = node(ndjhi,mptr) - node(ndjlo,mptr) + 1
          mitot  = nx+ 2*nghost
          mjtot  = ny+ 2*nghost
          mi2tot = nx/2 + 2*nghost
          mj2tot = ny/2 + 2*nghost
c
c         # this scratch storage will be used both for regular and half
c         # sized grid. different dimensions in stepgrid - do not reuse.
          locfp = igetsp(mitot*mjtot*nvar)
          locfm = igetsp(mitot*mjtot*nvar)
          locgp = igetsp(mitot*mjtot*nvar)
          locgm = igetsp(mitot*mjtot*nvar)
          locaux = node(storeaux,mptr)
c
          locbgc = node(errptr,mptr)
c
          locaxc = locbgc+nvar*mi2tot*mj2tot
c should we set to 1 if naux=0?
c
          evol = evol + (nx/2)*(ny/2)
          xlow = rnode(cornxlo,mptr) - nghost*hx2
          ylow = rnode(cornylo,mptr) - nghost*hy2
          call stepgrid(alloc(locbgc),alloc(locfm),alloc(locfp),
     1                alloc(locgm),alloc(locgp),
     2                mi2tot,mj2tot,nghost,
     3                dt2,dtnew2,hx2,hy2,nvar,
     4                xlow,ylow,tpre,mptr,vtime,naux,alloc(locaxc))
c
c  the one giant step based on old values is done. now take
c  one regular step based on new values. 
c
      evol   = evol + nx * ny
      xlow   = rnode(cornxlo,mptr) - nghost*hx
      ylow   = rnode(cornylo,mptr) - nghost*hy
      locbig = node(tempptr,mptr)
      loctmp = node(store2,mptr)
c
c estimate spatial component of error - use old values before
c integration to get accurate boundary gradients
c
      locerrsp = igetsp(mitot*mjtot)
      call errsp(alloc(locbig), alloc(locerrsp), mitot,mjtot,
     &           nvar, mptr,nghost, eprint, outunit)


      call stepgrid(alloc(locbig),alloc(locfm),alloc(locfp),
     1            alloc(locgm),alloc(locgp),
     2            mitot,mjtot,nghost,
     3            dt,dtnew,hx,hy,nvar,
     4            xlow,ylow,time,mptr,vtime,naux,alloc(locaux))
c
      call reclam(locfp, mitot*mjtot*nvar)
      call reclam(locfm, mitot*mjtot*nvar)
      call reclam(locgp, mitot*mjtot*nvar)
      call reclam(locgm, mitot*mjtot*nvar)
c
      call errf1(alloc(locbig),nvar,
     1          alloc(locbgc),mptr,mi2tot,mj2tot,
     2          mitot,mjtot,alloc(loctmp),alloc(locerrsp))
      call reclam(locbgc,mi2tot*mj2tot*(nvar+naux))
      call reclam(locerrsp, mitot*mjtot)
      call reclam(locbig,mitot*mjtot*nvar)
c
      mptr = node(levelptr, mptr)
      if (mptr .ne. 0) go to 25
c
c copy flagged arrays in individual grids (now stored in loctmp)
c into 1 big iflag array (iflags)
c
      mptr = lstart(lcheck)
 41   continue
         nx     = node(ndihi,mptr) - node(ndilo,mptr) + 1
         ny     = node(ndjhi,mptr) - node(ndjlo,mptr) + 1
         mitot  = nx + 2*nghost
         mjtot  = ny + 2*nghost
         loctmp = node(store2, mptr)
         call setflags(iflags,isize,jsize,
     .                 alloc(loctmp),nvar,mitot,mjtot,mptr)
      mptr = node(levelptr,mptr)
      if (mptr .ne. 0) go to 41

      if (eprint) then
         write(outunit,*)" flagged points before buffering on level", 
     .                   lcheck
         do 47 jj = 1, jsize
           j = jsize + 1 - jj
           write(outunit,100)(iflags(i,j),i=1,isize)
 100       format(80i1)
 47      continue
      endif
c
c  project finer grids to insure level nesting
      numpro = 0
      if (lcheck+2 .le. maxlv) then
         lrat2 = intrat(lcheck)*intrat(lcheck+1)
         call projec(lcheck,numpro,iflags,isize,jsize,lrat2)
      endif

      if (eprint) then
         write(outunit,*)" flagged points after projecting to level", 
     .                    lcheck
         write(outunit,*) " with ",numpro," additional points projected"
         do 49 jj = 1, jsize
           j = jsize + 1 - jj
           write(outunit,100)(iflags(i,j),i=1,isize)
 49      continue
      endif

c
c  diffuse flagged points in all 4 directions to make buffer zones 
c  note that this code flags with a same value as true flagged
c  points, not a different number.
c
c    # first get scratch work space (not that other scratch
c    # arrays above have been reclaimed. 
c
      ldom3 = igetsp((isize+2)*(jsize+2))
c
      do 55 inum = 1, ibuff

          call shiftset(iflags,alloc(ldom3),+1,0,isize,jsize)
          call shiftset(iflags,alloc(ldom3),-1,0,isize,jsize)
          call shiftset(iflags,alloc(ldom3),0,+1,isize,jsize)
          call shiftset(iflags,alloc(ldom3),0,-1,isize,jsize)

 55   continue

      if (eprint) then
         write(outunit,*)" flagged points after buffering on level", 
     .                    lcheck
         do 48 jj = 1, jsize
           j = jsize + 1 - jj
           write(outunit,100)(iflags(i,j),i=1,isize)
 48      continue
      endif
c   
c   count up
c
       numbad = 0 
       do 82 i = 1, isize
       do 82 j = 1, jsize
         if (iflags(i,j) .ne. goodpt) numbad = numbad + 1
 82    continue
       write(outunit,116) numbad, lcheck
 116   format(i5,' points flagged on level ',i4)

      call reclam(ldom3,(isize+2)*(jsize+2))

      return
      end
c
c ----------------------------------------------------------
c
      subroutine errsp(rect,sperr,mitot,mjtot,nvar,mptr,ng,
     &                 eprint, outunit)

c
c ::::::::::::::::::::: ERRSP ::::::::::::::::::::::::::::::::::
c estimate spatial only component of the error
c rect   = grid values including ghost cells
c sperr  = user-supplied function indicating where to flag cells
c           (if sperr(i,j) > tolsp cell will be flagged)
c          In the default version, the maximum magnitude of the jump
c          between grid cells over all components is computed.
c ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c
      implicit double precision (a-h, o-z)

c      include  "call.i"

      dimension   rect(mitot,mjtot,nvar)
      dimension  sperr(mitot,mjtot)
      integer    outunit
      logical    eprint

      do 20 j = ng+1, mjtot-ng
      do 10 i = ng+1, mitot-ng

      dq = 0.d0
      do 5 m = 1,nvar
         dqi = dabs(rect(i+1,j,m) - rect(i-1,j,m))
         dqj = dabs(rect(i,j+1,m) - rect(i,j-1,m))
         dq  = dmax1(dq,dqi, dqj)
  5      continue
      sperr(i,j) = dq

 10      continue
 20      continue

      return
      end
c
c  -----------------------------------------------------------
c
      subroutine gfixup(lbase, lfnew, nvar, naux)
c
      implicit double precision (a-h,o-z)

      include  "call.i"

c
c ::::::::::::::::::::::::: GFIXUP ::::::::::::::::::::::::::::::::;
c        interpolate initial values for the newly created grids.
c        the start of each level is located in newstl array.
c        since only levels greater than lbase were examined, start
c        looking there.
c :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::;
c
c   reclaim old storage (position 8) and list space 15 and 16
c   before allocating new storage. remember, finest level grids
c  (if level = mxnest so that error never estimated) don't have
c  2 copies of solution values at old and new times.
c
c
      call putsp(lbase,lbase,nvar,naux)
      level = lbase + 1
 1    if (level .gt. lfine) go to 4
      call putsp(lbase,level,nvar,naux)
          mptr = lstart(level)
 2        if (mptr .eq. 0) go to 3
              nx = node(ndihi,mptr) - node(ndilo,mptr) + 1
              ny = node(ndjhi,mptr) - node(ndjlo,mptr) + 1
              mitot = nx + 2*nghost
              mjtot = ny + 2*nghost
              nwords        = mitot*mjtot*nvar
              if (level .lt. mxnest) 
     .           call reclam(node(store2, mptr), nwords)
              node(store2, mptr) = 0
              mptr          = node(levelptr, mptr)
          go to 2
 3        level   = level + 1
          go to 1
c
 4    lcheck = lbase + 1
 5    if (lcheck .gt. maxlv) go to 99
          hx = hxposs(lcheck)
          hy = hyposs(lcheck)
c
c  interpolate level lcheck
c
          mptr   = newstl(lcheck)
 10       if (mptr .eq. 0) go to 80
              nx = node(ndihi,mptr) - node(ndilo,mptr) + 1
              ny = node(ndjhi,mptr) - node(ndjlo,mptr) + 1
              mitot = nx + 2*nghost
              mjtot = ny + 2*nghost
              corn1 = rnode(cornxlo,mptr)
              corn2 = rnode(cornylo,mptr)
              loc    = igetsp(mitot * mjtot * nvar)
              node(store1, mptr)  = loc
              if (naux .gt. 0) then
                locaux = igetsp(mitot * mjtot * naux)
                maxmx = mitot - 2*nghost
                mx = maxmx
                maxmy = mjtot - 2*nghost
                my = maxmy
                call setaux(maxmx,maxmy,nghost,mx,my,corn1,corn2,hx,hy,
     &                    naux,alloc(locaux))
              else
                locaux = 1
              endif
              node(storeaux, mptr)  = locaux
              time   = rnode(timemult, mptr)
c
c      We now fill in the values for grid mptr using filvar. It uses
c      piecewise linear interpolation to obtain values from the
c      (lcheck - 1) grid, then overwrites those with whatever (lcheck)
c      grids are available. We take advantage of the fact that the
c      (lcheck - 1) grids have already been set, and that the distance
c      between any point in mptr and a (lcheck - 1) and (lcheck - 2)
c      interface is at least one (lcheck - 1) cell wide.
c
 
c          # make a coarsened patch with ghost cells so can use
c          # grid interpolation routines, but only set "interior".
c          # extra 2 cells so that can use linear interp. on
c          # "interior" of coarser patch to fill fine grid.
           mic = nx/intrat(lcheck-1) + 2
           mjc = ny/intrat(lcheck-1) + 2
           ivalc  = igetsp(mic*mjc*(nvar+naux))
           ivalaux  = ivalc + nvar*mic*mjc
           xl = rnode(cornxlo,mptr)
           xr = rnode(cornxhi,mptr)
           yb = rnode(cornylo,mptr)
           yt = rnode(cornyhi,mptr)
           hx = hxposs(lcheck)
           hy = hyposs(lcheck)
           ilo    = node(ndilo, mptr)
           ihi    = node(ndihi, mptr)
           jlo    = node(ndjlo, mptr)
           jhi    = node(ndjhi, mptr)
 
           call filval(alloc(loc),mitot,mjtot,hx,hy,lcheck,time,
     1                 alloc(ivalc),alloc(ivalaux),mic,mjc,
     2                 xl,xr,yb,yt,nvar,
     3                 mptr,ilo,ihi,jlo,jhi,
     4                 alloc(locaux),naux)
 
           call reclam(ivalc,mic*mjc*(nvar+naux))
 
           mptr = node(levelptr, mptr)
           go to 10
c
c  done filling new grids at level. move them into lstart from newstl
c  (so can use as source grids for filling next level). can also
c  get rid of loc. 7 storage for old level.
c
 80   mptr = lstart(lcheck)
 85   if (mptr .eq. 0) go to 90
          nx = node(ndihi,mptr) - node(ndilo,mptr) + 1
          ny = node(ndjhi,mptr) - node(ndjlo,mptr) + 1
          mitot = nx + 2*nghost
          mjtot = ny + 2*nghost
          call reclam(node(store1,mptr),mitot*mjtot*nvar)
          if (naux .gt. 0) then
            call reclam(node(storeaux,mptr),mitot*mjtot*naux)
          endif
          mold   = mptr
          mptr   = node(levelptr,mptr)
          call putnod(mold)
          go to 85
 90   lstart(lcheck) = newstl(lcheck)
      lcheck = lcheck + 1
      go to 5
c
 99   lfine = lfnew
c
c     initialize 2nd (old time) storage block for new grids not at top level
c
      levend = min(lfine,mxnest-1)
      do 110 level = lbase+1, levend
         mptr = lstart(level)
 105     if (mptr .eq. 0) go to 110
            nx = node(ndihi,mptr) - node(ndilo,mptr) + 1
            ny = node(ndjhi,mptr) - node(ndjlo,mptr) + 1
            mitot = nx + 2*nghost
            mjtot = ny + 2*nghost
            nwords = mitot*mjtot*nvar
            node(store2,mptr) = igetsp(nwords)
         mptr = node(levelptr,mptr)
         go to 105
 110   continue

c
c  grid structure now complete again. safe to print, etc. assuming
c  things initialized to zero in nodget.
c
      return
      end
c
c ------------------------------------------------------------------
c
      subroutine filval(val,mitot,mjtot,hx,hy,lev,time,
     1                  valc,auxc,mic,mjc,
     2                  xleft,xright,ybot,ytop,nvar,
     3                  mptr,ilo,ihi,jlo,jhi,aux,naux)
 
      implicit double precision (a-h,o-z)

      include "call.i"

      dimension   val(mitot,mjtot,nvar), valc(mic,mjc,nvar)
      dimension   aux(mitot,mjtot,nvar), auxc(mic,mjc,nvar)
      dimension   dudx(max1d), dudy(max1d)
c
c :::::::::::::::::::::::::::::: FILVAL ::::::::::::::::::::::::::
c
c create and fill coarser (lev-1) patch with one extra coarse cell all
c around, plus the ghost cells . will interpolate from this patch to grid mptr 
c without needing special boundary code. 
c
c ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c
      levc   = lev - 1
      lratio = intrat(levc)
      hxcrse = hx*lratio
      hycrse = hy*lratio
      xl     = xleft  - hxcrse 
      xr     = xright + hxcrse
      yb     = ybot   - hycrse 
      yt     = ytop   + hycrse
c
c     set integer indices for coarser patch enlarged by 1 cell 
c     (can stick out of domain). proper nesting will insure this one
c     call is sufficient.
      iclo   = ilo/lratio - 1
      jclo   = jlo/lratio - 1
      ichi   = (ihi+1)/lratio - 1 + 1
      jchi   = (jhi+1)/lratio - 1 + 1
      ng     = 0

c    :::  mcapa  is the capacity function index

      if (mcapa .eq. 0) then
        if (xperdom .or. yperdom) then
          call preintcopy(valc,mic,mjc,nvar,iclo,ichi,jclo,jchi,levc)
        else
          call intcopy(valc,mic,mjc,nvar,iclo,ichi,jclo,jchi,levc,1,1)
        endif
      else
        if (xperdom .or. yperdom) then
          call preicall(valc,auxc,mic,mjc,nvar,naux,iclo,ichi,jclo,
     &                  jchi,levc)
        else
          call icall(valc,auxc,mic,mjc,nvar,naux,iclo,ichi,jclo,jchi,
     &               levc,1,1)
        endif
      endif
      call bc2amr(valc,auxc,mic,mjc,nvar,naux,
     1            hxcrse,hycrse,levc,time,
     2            xl,xr,yb,yt,
     3            xlower,ylower,xupper,yupper,
     4            xperdom,yperdom)
c
c     prepare slopes - use min-mod limiters
c
      do 30 j    = 2, mjc-1
      do 30 ivar = 1, nvar
      do 10 i    = 2, mic-1

         slp = valc(i+1,j,ivar) - valc(i,j,ivar)
         slm = valc(i,j,ivar)   - valc(i-1,j,ivar)
         slopex = dmin1(dabs(slp),dabs(slm))*
     .            dsign(1.0d0,valc(i+1,j,ivar) - valc(i-1,j,ivar))
c        # if there's a sign change, set slope to 0.
         if ( slm*slp .gt. 0.d0) then
           dudx(i) = slopex
         else
           dudx(i) = 0.d0
         endif

         slp = valc(i,j+1,ivar) - valc(i,j,ivar)
         slm = valc(i,j,ivar)   - valc(i,j-1,ivar)
         slopey = dmin1(dabs(slp),dabs(slm))*
     .            dsign(1.0d0,valc(i,j+1,ivar) - valc(i,j-1,ivar))
         if ( slm*slp .gt. 0.d0) then
           dudy(i) = slopey
         else
           dudy(i) = 0.d0
         endif

 10   continue
c
c     interp. from coarse cells to fine grid
c
      do 20 ico = 1,lratio
      xoff = (float(ico) - .5)/lratio - .5
         do 20 jco = 1,lratio
         jfine = (j-2)*lratio + nghost + jco
         yoff  = (float(jco) - .5)/lratio - .5
            do 20 i = 2, mic-1
            ifine   = (i-2)*lratio + nghost + ico
            val(ifine,jfine,ivar) = valc(i,j,ivar)
     1                                + xoff*dudx(i) + yoff*dudy(i)
 20   continue
c
 30   continue

      if (mcapa .ne. 0) then
        call fixcapaq(val,aux,mitot,mjtot,valc,auxc,mic,mjc,
     &                nvar,naux,lratio)
      endif
c
c  overwrite interpolated values with fine grid values, if available.
c
      call intcopy(val,mitot,mjtot,nvar,ilo-nghost,ihi+nghost,
     &             jlo-nghost,jhi+nghost,lev,1,1)

      return
      end
c
c ---------------------------------------------------------------
c
        subroutine filpatch(level,nvar,valbig,aux,naux,
     1                      time,mitot,mjtot,
     2                      nrowst,ncolst,ilo,ihi,jlo,jhi)

c :::::::::::::::::::::::::::: FILPATCH :::::::::::::::::::::::::;
c
c  fill the portion of valbig from rows  nrowst
c                             and  cols  ncolst
c  the patch can also be described by the corners (xlp,ybp) by (xrp,ytp).
c  vals are needed at time time, and level level,
c
c  first fill with  values obtainable from the level level
c  grids. if any left unfilled, then enlarge remaining rectangle of
c  unfilled values by 1 (for later linear interp), and recusively
c  obtain the remaining values from  coarser levels.
c
c :::::::::::::::::::::::::::::::::::::::;:::::::::::::::::::::::;

      implicit double precision (a-h,o-z)

      include  "call.i"

      logical   set, sticksout
      dimension valbig(mitot,mjtot,nvar), aux(mitot,mjtot,naux)

      iadflag(i,j)    =  locuse + i-1+(j-1)*nrowp
      ivalc(i,j,ivar) =  loccrse + (i - 1) + nrowc*(j - 1)
     &                     + nrowc*ncolc*(ivar-1)
      sticksout(iplo,iphi,jplo,jphi)  =
     &            (iplo .lt. 0 .or. jplo .lt. 0 .or.
     &             iphi .ge. iregsz(levc) .or. jphi .ge. jregsz(levc))
c
c We begin by filling values for grids at level level. If all values can be
c filled in this way, we return;

        hxf     = hxposs(level)
        hyf     = hyposs(level)
        xlp     = xlower + ilo*hxf
        xrp     = xlower + (ihi+1)*hxf
        ybp     = ylower + jlo*hyf
        ytp     = ylower + (jhi+1)*hyf
        nrowp   = ihi - ilo + 1
        ncolp   = jhi - jlo + 1
        locuse  = igetsp(nrowp*ncolp)

c 2/27/02 : added naux to intfil call.
        call intfil
     &  (valbig,mitot,mjtot,time,locuse,nrowst,ncolst,
     &   ilo,ihi,jlo,jhi,level,nvar,naux)
c
c Trimbd returns set = true if all of the entries are filled (=1.).
c set = false, otherwise. If set = true, then no other levels are
c are required to interpolate, and we return.
c
c Note that the used array is filled entirely in intfil, i.e. the
c marking done there also takes into account the points filled by
c the boundary conditions. bc2amr will be called later (from bound), after
c all 4 boundary pieces filled.

        call trimbd(alloc(locuse),nrowp,ncolp,set,il,ir,jb,jt)

        if (set) then
           call reclam(locuse,nrowp*ncolp)
           return
        else if (level .eq. 1) then
           write(outunit,*)" error in filpatch - level 1 not set"
           write(outunit,900) nrowst,ncolst
           write(*,*)" error in filpatch - level 1 not set"
           write(*,900) nrowst,ncolst
900        format("starting at row: ",i4," col ",i4)
           stop
        endif

c set = false. we will have to interpolate some values from coarser
c levels. We begin by initializing the level level arrays, so that we can use
c recursive formulation for interpolating.
c IS THIS TRUE ? - the fine grid patch remaining unfilled is always
c anchored to a coarse cell.

        levc = level - 1
        hxc  = hxposs(levc)
        hyc  = hyposs(levc)

        isl  = il + ilo - 1
        isr  = ir + ilo - 1
        jsb  = jb + jlo - 1
        jst  = jt + jlo - 1
c
c       coarsen
        lratio = intrat(levc)
c       iplo   = (isl-lratio/2+nghost*lratio)/lratio - nghost
c       jplo   = (jsb-lratio/2+nghost*lratio)/lratio - nghost
c       iphi   = (isr+lratio/2)/lratio
c       jphi   = (jst+lratio/2)/lratio

        iplo   = (isl-lratio  +nghost*lratio)/lratio - nghost
        jplo   = (jsb-lratio  +nghost*lratio)/lratio - nghost
        iphi   = (isr+lratio  )/lratio
        jphi   = (jst+lratio  )/lratio


        xlc  =  xlower + iplo*hxc
        ybc  =  ylower + jplo*hyc
        xrc  =  xlower + (iphi+1)*hxc
        ytc  =  ylower + (jphi+1)*hyc

        nrowc   =  iphi - iplo + 1
        ncolc   =  jphi - jplo + 1
        ntot    = nrowc*ncolc*(nvar+naux)
        loccrse = igetsp(ntot)
        locauxc = loccrse + nrowc*ncolc*nvar
        if (naux.gt.0) then
              maxmx = nrowc - 2*nghost
              mx = maxmx
              maxmy = ncolc - 2*nghost
              my = maxmy
              xl = xlc + nghost*hxc
              yb = ybc + nghost*hyc
              call setaux(maxmx,maxmy,nghost,mx,my,xl,yb,hxc,hyc,
     &                    naux,alloc(locauxc))
        endif


        if ((xperdom .or. yperdom) .and.
     &       sticksout(iplo,iphi,jplo,jphi)) then
             call prefil2(levc,nvar,alloc(loccrse),alloc(locauxc),naux,
     1                    time,nrowc,ncolc,1,1,
     2                    iplo,iphi,jplo,jphi)
        else
          call filpatch2(levc,nvar,alloc(loccrse),alloc(locauxc),naux,
     1                   time,nrowc,ncolc,1,1,
     2                   iplo,iphi,jplo,jphi)
        endif


c       interpolate back up

20      continue

        do 100 iff = 1,nrowp
          ic = 2 + (iff - (isl - ilo) - 1)/lratio
          eta1 = (-0.5d0+dble(mod(iff-1,lratio)))/dble(lratio)

        do 100 jf  = 1,ncolp
          jc = 2 + (jf -(jsb-jlo)-1)/lratio
          eta2 = (-0.5d0+dble(mod(jf -1,lratio)))/dble(lratio)

           flag = alloc(iadflag(iff,jf))
           if (flag .eq. 0.0) then

c               xif = xlp + (.5 + float(iff-1))*hxf
c               yjf = ybp + (.5 + float(jf -1))*hyf

c               ic=idint((xif-xlc+.5*hxc)/hxc)
c               jc=idint((yjf-ybc+.5*hyc)/hyc)

c               xc = xlc + (float(ic) - .5)*hxc
c               yc = ybc + (float(jc) - .5)*hyc

c               eta1 = (xif - xc)/hxc
c               eta2 = (yjf - yc)/hyc

                do 101 ivar = 1,nvar

c                  valc00 = alloc(ivalc(ic,jc,ivar))
c                  valc10 = alloc(ivalc(ic+1,jc,ivar))
c                  valc01 = alloc(ivalc(ic,jc+1,ivar))
c                  valc11 = alloc(ivalc(ic+1,jc+1,ivar))

                   valp10 = alloc(ivalc(ic+1,jc,ivar))
                   valm10 = alloc(ivalc(ic-1,jc,ivar))
                   valc   = alloc(ivalc(ic  ,jc,ivar))
                   valp01 = alloc(ivalc(ic  ,jc+1,ivar))
                   valm01 = alloc(ivalc(ic  ,jc-1,ivar))

                   dupc = valp10 - valc
                   dumc = valc   - valm10
                   ducc = valp10 - valm10
                   du   = dmin1(dabs(dupc),dabs(dumc))
                   du   = dmin1(2.d0*du,.5d0*dabs(ducc))
                   fu = dmax1(0.d0,dsign(1.d0,dupc*dumc))

                   dvpc = valp01 - valc
                   dvmc = valc   - valm01
                   dvcc = valp01 - valm01
                   dv   = dmin1(dabs(dvpc),dabs(dvmc))
                   dv   = dmin1(2.d0*dv,.5d0*dabs(dvcc))
                   fv = dmax1(0.d0,dsign(1.d0,dvpc*dvmc))

                   valint = valc + eta1*du*dsign(1.d0,ducc)*fu
     .                           + eta2*dv*dsign(1.d0,dvcc)*fv

c                  valint = (1. - eta2)*
c    &               ((1. - eta1)*valc00 + eta1*valc10)
c    &               + eta2*((1. - eta1)*valc01 + eta1*valc11)

                   valbig(iff+nrowst-1,jf+ncolst-1,ivar) = valint

101             continue

           endif

100     continue

        call reclam(loccrse,ntot)

        call reclam(locuse,nrowp*ncolp)

        return
        end
c
c ---------------------------------------------------------------
c
        subroutine filpatch2(level,nvar,valbig,aux,naux,
     1                      time,mitot,mjtot,
     2                      nrowst,ncolst,ilo,ihi,jlo,jhi)

c :::::::::::::::::::::::::::: FILPATCH :::::::::::::::::::::::::;
c
c  fill the portion of valbig from rows  nrowst
c                             and  cols  ncolst
c  the patch can also be described by the corners (xlp,ybp) by (xrp,ytp).
c  vals are needed at time time , and level level,
c
c  first fill with  values obtainable from the level level
c  grids. if any left unfilled, then enlarge remaining rectangle of
c  unfilled values by 1 (for later linear interp), and recusively
c  obtain the remaining values from  coarser levels.
c
c :::::::::::::::::::::::::::::::::::::::;:::::::::::::::::::::::;

      implicit double precision (a-h,o-z)

      include  "call.i"

      logical   set, sticksout
      dimension valbig(mitot,mjtot,nvar), aux(mitot,mjtot,naux)

      iadflag(i,j) =  locuse + i-1+(j-1)*nrowp
      ivalc(i,j,ivar) = loccrse + (i - 1) + nrowc*(j - 1)
     &                    + nrowc*ncolc*(ivar-1)
      sticksout(iplo,iphi,jplo,jphi)  =
     &            (iplo .lt. 0 .or. jplo .lt. 0 .or.
     &             iphi .ge. iregsz(levc) .or. jphi .ge. jregsz(levc))
c
c We begin by filling values for grids at level level. If all values can be
c filled in this way, we return;

        nrowp   = ihi - ilo + 1
        ncolp   = jhi - jlo + 1
        locuse  = igetsp(nrowp*ncolp)
        hxf     = hxposs(level)
        hyf     = hyposs(level)
        xlp     = xlower + ilo*hxf
        xrp     = xlower + (ihi+1)*hxf
        ybp     = ylower + jlo*hyf
        ytp     = ylower + (jhi+1)*hyf
        hmargin = dmin1(hxf,hyf)/10.d0

        call intfil
     &  (valbig,mitot,mjtot,time,locuse,nrowst,ncolst,
     &   ilo,ihi,jlo,jhi,level,nvar,naux)
c
c  only call bc2amr for coarse patches (where bc2amr sets the whole
c  patch. otherwise, orig. caller of filpatch2 will take care of this
c

c Trimbd returns set = true if all of the entries are filled (=1.).
c set = false, otherwise. If set = true, then no other levels are
c are required to interpolate, and we return.
c
c Note that the used array is filled entirely in intfil, i.e. the
c marking done there also takes c into account the points filled by
c the boundary conditions. bc2amr will be called later, after all 4
c boundary pieces filled.

        call trimbd(alloc(locuse),nrowp,ncolp,set,il,ir,jb,jt)

        if (set) then
           call bc2amr(valbig,aux,mitot,mjtot,nvar,naux,
     1                 hxf,hyf,level,time,
     2                 xlp,xrp,ybp,ytp,
     3                 xlower,ylower,xupper,yupper,
     4                 xperdom,yperdom)
           call reclam(locuse,nrowp*ncolp)
           return
        else if (level .eq. 1) then
           write(outunit,*)" error in filpatch2 - level 1 not set"
           write(outunit,900) nrowst,ncolst
           write(*,*)" error in filpatch2 - level 1 not set"
           write(*,900) nrowst,ncolst
900        format("start at row: ",i4," col ",i4)
           stop
        endif

c set = false. we will have to interpolate some values from coarser
c levels. We begin by initializing the level level arrays, so that we can use
c purely recursive formulation for interpolating.


        levc = level - 1
        hxc  = hxposs(levc)
        hyc  = hyposs(levc)

        isl  = il + ilo - 1
        isr  = ir + ilo - 1
        jsb  = jb + jlo - 1
        jst  = jt + jlo - 1
c
c       coarsen
        lratio = intrat(levc)
        iplo   = (isl-lratio+nghost*lratio)/lratio - nghost
        jplo   = (jsb-lratio+nghost*lratio)/lratio - nghost
        iphi   = (isr+lratio)/lratio
        jphi   = (jst+lratio)/lratio

        xlc  =  xlower + iplo*hxc
        ybc  =  ylower + jplo*hyc
        xrc  =  xlower + (iphi+1)*hxc
        ytc  =  ylower + (jphi+1)*hyc

        nrowc   =  iphi - iplo + 1
        ncolc   =  jphi - jplo + 1
        ntot    = nrowc*ncolc*(nvar+naux)
        loccrse = igetsp(ntot)
        locauxc = loccrse + nrowc*ncolc*nvar
        if (naux.gt.0) then
              maxmx = nrowc - 2*nghost
              mx = maxmx
              maxmy = ncolc - 2*nghost
              my = maxmy
              xl = xlc + nghost*hxc
              yb = ybc + nghost*hyc
              call setaux(maxmx,maxmy,nghost,mx,my,xl,yb,hxc,hyc,
     &                    naux,alloc(locauxc))
        endif

        if ((xperdom .or. yperdom) .and.
     &       sticksout(iplo,iphi,jplo,jphi)) then
             call prefil3(levc,nvar,alloc(loccrse),alloc(locauxc),naux,
     1                    time,nrowc,ncolc,1,1,
     2                    iplo,iphi,jplo,jphi)
        else
           call filpatch3(levc,nvar,alloc(loccrse),alloc(locauxc),naux,
     1                    time,nrowc,ncolc,1,1,
     2                    iplo,iphi,jplo,jphi)
        endif

c       interpolate back up

20      continue

        do 100 iff = 1,nrowp
          ic = 2 + (iff-(isl-ilo)-1)/lratio
          eta1 = (-0.5d0+dble(mod(iff-1,lratio)))/dble(lratio)

        do 100 jf  = 1,ncolp
          jc = 2 + (jf -(jsb-jlo)-1)/lratio
          eta2 = (-0.5d0+dble(mod(jf -1,lratio)))/dble(lratio)

           flag = alloc(iadflag(iff,jf))
           if (flag .eq. 0.0) then

c               xif = xlp + (.5 + float(iff-1))*hxf
c               yjf = ybp + (.5 + float(jf -1))*hyf

c               ic=idint((xif-xlc+.5*hxc)/hxc)
c               jc=idint((yjf-ybc+.5*hyc)/hyc)

c               xc = xlc + (float(ic) - .5)*hxc
c               yc = ybc + (float(jc) - .5)*hyc

c               eta1 = (xif - xc)/hxc
c               eta2 = (yjf - yc)/hyc

                do 101 ivar = 1,nvar

                   valp10 = alloc(ivalc(ic+1,jc,ivar))
                   valm10 = alloc(ivalc(ic-1,jc,ivar))
                   valc   = alloc(ivalc(ic  ,jc,ivar))
                   valp01 = alloc(ivalc(ic  ,jc+1,ivar))
                   valm01 = alloc(ivalc(ic  ,jc-1,ivar))

                   dupc = valp10 - valc
                   dumc = valc   - valm10
                   ducc = valp10 - valm10
                   du   = dmin1(dabs(dupc),dabs(dumc))
                   du   = dmin1(2.d0*du,.5d0*dabs(ducc))
                   fu = dmax1(0.d0,dsign(1.d0,dupc*dumc))

                   dvpc = valp01 - valc
                   dvmc = valc   - valm01
                   dvcc = valp01 - valm01
                   dv   = dmin1(dabs(dvpc),dabs(dvmc))
                   dv   = dmin1(2.d0*dv,.5d0*dabs(dvcc))
                   fv = dmax1(0.d0,dsign(1.d0,dvpc*dvmc))

                   valint = valc + eta1*du*dsign(1.d0,ducc)*fu
     .                           + eta2*dv*dsign(1.d0,dvcc)*fv


c                  valc00 = alloc(ivalc(ic,jc,ivar))
c                  valc10 = alloc(ivalc(ic+1,jc,ivar))
c                  valc01 = alloc(ivalc(ic,jc+1,ivar))
c                  valc11 = alloc(ivalc(ic+1,jc+1,ivar))

c                  valint = (1. - eta2)*
c    &               ((1. - eta1)*valc00 + eta1*valc10)
c    &               + eta2*((1. - eta1)*valc01 + eta1*valc11)

                   valbig(iff+nrowst-1,jf+ncolst-1,ivar) = valint

101             continue

           endif

100     continue

        call bc2amr(valbig,aux,mitot,mjtot,nvar,naux,
     1              hxf,hyf,level,time,
     2              xlp,xrp,ybp,ytp,
     3              xlower,ylower,xupper,yupper,
     4              xperdom,yperdom)

        call reclam(loccrse,ntot)

        call reclam(locuse,nrowp*ncolp)

        return
        end
c
c ---------------------------------------------------------------
c
        subroutine filpatch3(level,nvar,valbig,aux,naux,
     1                      time,mitot,mjtot,
     2                      nrowst,ncolst,ilo,ihi,jlo,jhi)

c :::::::::::::::::::::::::::: FILPATCH :::::::::::::::::::::::::;
c
c  fill the portion of valbig from rows  nrowst
c                             and  cols  ncolst
c  the patch can also be described by the corners (xlp,ybp) by (xrp,ytp).
c  vals are needed at time time, and level level,
c
c  first fill with  values obtainable from the level level
c  grids. if any left unfilled, then enlarge remaining rectangle of
c  unfilled values by 1 (for later linear interp), and recusively
c  obtain the remaining values from  coarser levels.
c
c :::::::::::::::::::::::::::::::::::::::;:::::::::::::::::::::::;

      implicit double precision (a-h,o-z)

      include  "call.i"

      logical   set
      dimension valbig(mitot,mjtot,nvar), aux(mitot,mjtot,naux)

      iadflag(i,j) =  locuse + i-1+(j-1)*nrowp
c
c We begin by filling values for grids at level level. If all values can be
c filled in this way, we return;

        nrowp  = ihi - ilo + 1
        ncolp  = jhi - jlo + 1
        locuse = igetsp(nrowp*ncolp)
        hxf    = hxposs(level)
        hyf    = hyposs(level)
        xlp    = xlower + ilo*hxf
        xrp    = xlower + (ihi+1)*hxf
        ybp    = ylower + jlo*hyf
        ytp    = ylower + (jhi+1)*hyf

        call intfil
     &  (valbig,mitot,mjtot,time,locuse,nrowst,ncolst,
     &   ilo,ihi,jlo,jhi,level,nvar,naux)
c
c  only call bc2amr for coarse patches (where bc2amr sets the whole
c  patch. otherwise, orig. caller of filpatch3 will take care of this
c
        if (nrowp .eq. mitot .and. ncolp .eq. mjtot) then
           call bc2amr(valbig,aux,mitot,mjtot,nvar,naux,
     1                 hxf,hyf,level,time,
     2                 xlp,xrp,ybp,ytp,
     3                 xlower,ylower,xupper,yupper,
     4                 xperdom,yperdom)
        endif


c Trimbd returns set = true if all of the entries are filled (=1.).
c set = false, otherwise. If set = true, then no other levels are
c are required to interpolate, and we return.
c
c Note that the used array is filled entirely in intfil, i.e. the
c marking done there also takes c into account the points filled by
c the boundary conditions. bc2amr will be called later, after all 4
c boundary pieces filled.

        call trimbd(alloc(locuse),nrowp,ncolp,set,il,ir,jb,jt)

        if (set) then
           call reclam(locuse,nrowp*ncolp)
           return
        else if (level .eq. 1) then
           write(outunit,*)" error in filpatch3 - level 1 not set"
           write(outunit,900) nrowst,ncolst
           write(*,*)" error in filpatch3 - level 1 not set"
           write(*,900) nrowst,ncolst
900        format("starting at row: ",i4," cols ",i4)
           stop
        endif

c set = false. we will have to interpolate some values from coarser
c levels. We begin by initializing the level level arrays, so that we can use
c purely recursive formulation for interpolating.


        write(outunit,*)" should not need recursive call here"
        write(outunit,*)" 3 filpatches sufficient for properly",
     .                  " nested grids"
        write(*,*)" should not need recursive call here"
        write(*,*)" 3 filpatches sufficient for properly",
     .                  " nested grids"
        stop

        end
c
c --------------------------------------------------------------
c
      subroutine prefilp(level,nvar,
     1                   valbig,aux,naux,time,mitot,mjtot,
     1                   nrowst,ncolst,
     3                   ilo,ihi,jlo,jhi)

c
      implicit double precision (a-h,o-z)

      include  "call.i"

      dimension valbig(mitot,mjtot,nvar), aux(mitot,mjtot,nvar)
      dimension ist(3), iend(3), jst(3), jend(3), ishift(3), jshift(3)

c
c  :::::::::::::: PREFILP :::::::::::::::::::::::::::::::::::::::::::
c     For periodic boundary conditions more work needed to fill the
c     piece of the boundary. This routine was
c     called because the patch sticks out of the domain,
c     and has periodic bc.s preprocess the patch before calling
c     filpatch to shift the patch periodically back into the domain.
c
c     Inputs to this routine:
c     xl, xr, yb, yt = the location in physical space of
c     corners of a patch.
c     ilo,ihi,jlo,jhi = the location in index space of this patch.
c
c     Outputs from this routine:
c     The values around the border of the grid are inserted
c     directly into the enlarged valbig array for this piece.
c
c :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

c
c     # will divide patch into 9 possibilities (some empty): 
c       x sticks out left, x interior, x sticks out right
c       same for y. for example, the max. would be
c       i from (ilo,-1), (0,iregsz(level)-1), (iregsz(level),ihi)
        
        ist(1) = ilo
        ist(2) = 0
        ist(3) = iregsz(level)
        iend(1) = -1
        iend(2) = iregsz(level)-1
        iend(3) = ihi
        jst(1) = jlo
        jst(2) = 0
        jst(3) = jregsz(level)
        jend(1) = -1
        jend(2) = jregsz(level)-1
        jend(3) = jhi
        ishift(1) = iregsz(level)
        ishift(2) = 0
        ishift(3) = -iregsz(level)
        jshift(1) = jregsz(level)
        jshift(2) = 0
        jshift(3) = -jregsz(level)

       do 20 i = 1, 3
          i1 = max(ilo,  ist(i))
          i2 = min(ihi, iend(i))
       do 10 j = 1, 3
          j1 = max(jlo,  jst(j))
          j2 = min(jhi, jend(j))


          if ((i1 .le. i2) .and. (j1 .le. j2)) then
            iputst = (i1 - ilo) + nrowst
            jputst = (j1 - jlo) + ncolst
            call filpatch(level,nvar,valbig,aux,naux,time,mitot,mjtot,
     1                    iputst,jputst,
     2                    i1+ishift(i),i2+ishift(i),
     3                    j1+jshift(j),j2+jshift(j))
          endif

 10    continue
 20    continue
      
     


      return
      end
c
c --------------------------------------------------------------
c
      subroutine prefil2(level,nvar,
     1                   valbig,aux,naux,time,mitot,mjtot,
     1                   nrowst,ncolst,
     3                   ilo,ihi,jlo,jhi)

c
      implicit double precision (a-h,o-z)

      include  "call.i"

      dimension valbig(mitot,mjtot,nvar), aux(mitot,mjtot,naux)
      dimension ist(3), iend(3), jst(3), jend(3), ishift(3), jshift(3)

c
c  :::::::::::::: PREFILP :::::::::::::::::::::::::::::::::::::::::::
c     For periodic boundary conditions more work needed to fill the
c     piece of the boundary. This routine was
c     called because the patch sticks out of the domain,
c     and has periodic bc.s preprocess the patch before calling
c     filpatch to shift the patch periodically back into the domain.
c
c     Inputs to this routine:
c     xl, xr, yb, yt = the location in physical space of
c     corners of a patch.
c     ilo,ihi,jlo,jhi = the location in index space of this patch.
c
c     Outputs from this routine:
c     The values around the border of the grid are inserted
c     directly into the enlarged valbig array for this piece.
c
c :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

c
c     # will divide patch into 9 possibilities (some empty): 
c       x sticks out left, x interior, x sticks out right
c       same for y. for example, the max. would be
c       i from (ilo,-1), (0,iregsz(level)-1), (iregsz(level),ihi)
        
        ist(1) = ilo
        ist(2) = 0
        ist(3) = iregsz(level)
        iend(1) = -1
        iend(2) = iregsz(level)-1
        iend(3) = ihi
        jst(1) = jlo
        jst(2) = 0
        jst(3) = jregsz(level)
        jend(1) = -1
        jend(2) = jregsz(level)-1
        jend(3) = jhi
        ishift(1) = iregsz(level)
        ishift(2) = 0
        ishift(3) = -iregsz(level)
        jshift(1) = jregsz(level)
        jshift(2) = 0
        jshift(3) = -jregsz(level)

       do 20 i = 1, 3
          i1 = max(ilo,  ist(i))
          i2 = min(ihi, iend(i))
       do 10 j = 1, 3
          j1 = max(jlo,  jst(j))
          j2 = min(jhi, jend(j))


          if ((i1 .le. i2) .and. (j1 .le. j2)) then
            iputst = (i1 - ilo) + nrowst
            jputst = (j1 - jlo) + ncolst
            call filpatch2(level,nvar,valbig,aux,naux,time,mitot,mjtot,
     1                     iputst,jputst,
     2                    i1+ishift(i),i2+ishift(i),
     3                    j1+jshift(j),j2+jshift(j))
          endif

 10    continue
 20    continue
      
     


      return
      end
c
c --------------------------------------------------------------
c
      subroutine prefil3(level,nvar,
     1                   valbig,aux,naux,time,mitot,mjtot,
     1                   nrowst,ncolst,
     3                   ilo,ihi,jlo,jhi)

c
      implicit double precision (a-h,o-z)

      include  "call.i"

      dimension valbig(mitot,mjtot,nvar), aux(mitot,mjtot,naux)
      dimension ist(3), iend(3), jst(3), jend(3), ishift(3), jshift(3)

c
c  :::::::::::::: PREFILP :::::::::::::::::::::::::::::::::::::::::::
c     For periodic boundary conditions more work needed to fill the
c     piece of the boundary. This routine was
c     called because the patch sticks out of the domain,
c     and has periodic bc.s preprocess the patch before calling
c     filpatch to shift the patch periodically back into the domain.
c
c     Inputs to this routine:
c     xl, xr, yb, yt = the location in physical space of
c     corners of a patch.
c     ilo,ihi,jlo,jhi = the location in index space of this patch.
c
c     Outputs from this routine:
c     The values around the border of the grid are inserted
c     directly into the enlarged valbig array for this piece.
c
c :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

c
c     # will divide patch into 9 possibilities (some empty): 
c       x sticks out left, x interior, x sticks out right
c       same for y. for example, the max. would be
c       i from (ilo,-1), (0,iregsz(level)-1), (iregsz(level),ihi)
        
        ist(1) = ilo
        ist(2) = 0
        ist(3) = iregsz(level)
        iend(1) = -1
        iend(2) = iregsz(level)-1
        iend(3) = ihi
        jst(1) = jlo
        jst(2) = 0
        jst(3) = jregsz(level)
        jend(1) = -1
        jend(2) = jregsz(level)-1
        jend(3) = jhi
        ishift(1) = iregsz(level)
        ishift(2) = 0
        ishift(3) = -iregsz(level)
        jshift(1) = jregsz(level)
        jshift(2) = 0
        jshift(3) = -jregsz(level)

       do 20 i = 1, 3
          i1 = max(ilo,  ist(i))
          i2 = min(ihi, iend(i))
       do 10 j = 1, 3
          j1 = max(jlo,  jst(j))
          j2 = min(jhi, jend(j))


          if ((i1 .le. i2) .and. (j1 .le. j2)) then
            iputst = (i1 - ilo) + nrowst
            jputst = (j1 - jlo) + ncolst
            call filpatch3(level,nvar,valbig,aux,naux,time,mitot,mjtot,
     1                     iputst,jputst,
     2                    i1+ishift(i),i2+ishift(i),
     3                    j1+jshift(j),j2+jshift(j))
          endif

 10    continue
 20    continue
      
     


      return
      end
c
c -----------------------------------------------------------
c
      subroutine flglvl(nvar,naux,lcheck,nxypts,index,lbase,ldom2,npts)
c
      implicit double precision (a-h,o-z)

      include  "call.i"

c
c :::::::::::::::::::: FLGLVL :::::::::::::::::::::::::::::::::
c
c flglvl = controls the error estimation/flagging bad pts. for
c          an entire level of grids.  returns pointer into alloc
c          where the (x,y) coordinations of the flagged pts. are.
c input parameters:
c           lcheck = level to be flagged
c output parameters:
c           nxypts = no. of flagged pts. total
c           index  = starting index in alloc of the flagged pts.
c                    (which occupy 2*nxypts locations).
c
c ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c
c
      nxypts = 0
c
c   reserve space for entire domain worth of flagged points at
c   level lcheck. bits would be better, but integer will do
c   dom2 - holds domain flags
c   dom  - holds flagged pts.
c   dom3 - scratch
c
      isize = iregsz(lcheck)
      jsize = jregsz(lcheck)
      ldom  = igetsp((isize+2)*(jsize+2))
c    
c   prepare domain in ldom2 (so can use ldom as scratch array before 
c   putting in the flags)
c
      idim = iregsz(lbase)
      jdim = jregsz(lbase)
      call domprep(alloc(ldom2),lbase,idim,jdim)
      call domshrink(alloc(ldom2),alloc(ldom),idim,jdim)

      do 6 lev = lbase+1, lcheck
         mult = intrat(lev-1)
         call domup(alloc(ldom2),alloc(ldom),idim,jdim,
     1              mult*idim,mult*jdim,mult)
         idim = mult*idim
         jdim = mult*jdim
         call domshrink(alloc(ldom2),alloc(ldom),idim,jdim)
 6    continue
c     # finish by transferring from iflags to iflags2
      call domcopy(alloc(ldom2),alloc(ldom),isize,jsize)
c
      numbad = 0
      call errest(nvar,naux,numbad,lcheck,alloc(ldom),isize,jsize)
      nxypts = nxypts + numbad
c
c  colate flagged pts into flagged points array
c
      if (nxypts .gt. 0) then
          index = igetsp(2*nxypts)
          call colate(alloc(index),nxypts,lcheck,
     1                alloc(ldom),alloc(ldom2),isize,jsize,npts)
      else 
         npts = nxypts
      endif

      call reclam(ldom,  (isize+2)*(jsize+2)) 

      return
      end
c
c -------------------------------------------------------
c
      subroutine fluxad(xfluxm,xfluxp,yfluxm,yfluxp,
     1                  svdflx,mptr,mitot,mjtot,
     2                   nvar,lenbc,lratio,ng,dtf,dx,dy)
c

      implicit double precision (a-h,o-z)

      include  "call.i"

c :::::::::::::::::::: FLUXAD ::::::::::::::::::::::::::::::::::
c  save fine grid fluxes  at the border of the grid, for fixing
c  up the adjacent coarse cells. at each edge of the grid, only
c  save the plus or minus fluxes, as necessary. For ex., on
c  left edge of fine grid, it is the minus xfluxes that modify the
c  coarse cell.
c :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

      dimension xfluxm(mitot,mjtot,nvar), yfluxm(mitot,mjtot,nvar)
      dimension xfluxp(mitot,mjtot,nvar), yfluxp(mitot,mjtot,nvar)
      dimension svdflx(nvar,lenbc)
 
      nx  = mitot-2*ng
      ny  = mjtot-2*ng
      nyc = ny/lratio
      nxc = nx/lratio
 
c ::::: left side saved first
      lind = 0

      do 100 j=1,nyc
         lind = lind + 1
         jfine = (j-1)*lratio + ng
         do 110 ivar = 1, nvar
            do 120 l=1,lratio
               svdflx(ivar,lind) = svdflx(ivar,lind) +
     1                             xfluxm(ng+1,jfine+l,ivar)*dtf*dy
c              write(dbugunit,900)lind,xfluxm(1,jfine+l,ivar),
c     .                           xfluxp(1,jfine+l,ivar)
 900           format(' lind ', i4,' m & p ',2e15.7)
120         continue
110      continue
100   continue
 
c ::::: top side
      do 200 i=1,nxc
         lind = lind + 1
         ifine = (i-1)*lratio + ng
         do 210 ivar = 1, nvar
            do 220 l=1,lratio
               svdflx(ivar,lind) = svdflx(ivar,lind) + 
     1                     yfluxp(ifine+l,mjtot-ng+1,ivar)*dtf*dx
c              write(dbugunit,900)lind,yfluxm(ifine+l,mjtot-ng+1,
c    .                            ivar),yfluxp(ifine+l,mjtot-ng+1,ivar)
220         continue
210      continue
200   continue
 
c ::::: right side
      do 300 j=1,nyc
         lind = lind + 1
         jfine = (j-1)*lratio + ng
         do 310 ivar = 1, nvar
            do 320 l=1,lratio
               svdflx(ivar,lind) = svdflx(ivar,lind) + 
     1                     xfluxp(mitot-ng+1,jfine+l,ivar)*dtf*dy
c              write(dbugunit,900)lind,xfluxm(mitot-ng+1,jfine+l,
c                                 ivar),xfluxp(mitot-ng+1,jfine+l,ivar)
320         continue
310      continue
300   continue
 
c ::::: bottom side
      do 400 i=1,nxc
         lind = lind + 1
         ifine = (i-1)*lratio + ng
         do 410 ivar = 1, nvar
            do 420 l=1,lratio
               svdflx(ivar,lind) = svdflx(ivar,lind) +
     1                             yfluxm(ifine+l,ng+1,ivar)*dtf*dx
c              write(dbugunit,900)lind,yfluxm(ifine+l,ng+1,ivar),
c     .                      yfluxp(ifine+l,ng+1,ivar)
420         continue
410      continue
400   continue
 
      return
      end
c
c ----------------------------------------------------------
c
      subroutine fluxsv(mptr,xfluxm,xfluxp,yfluxm,yfluxp,listbc,
     1                  ndimx,ndimy,nvar,maxsp,dtc,hx,hy)
c
      implicit double precision (a-h,o-z)

      include  "call.i"

      dimension xfluxp(ndimx,ndimy,nvar), yfluxp(ndimx,ndimy,nvar)
      dimension xfluxm(ndimx,ndimy,nvar), yfluxm(ndimx,ndimy,nvar)
      dimension listbc(5,maxsp)
c
c :::::::::::::::::::: FLUXSV :::::::::::::::::::::::::
c
c  coarse grids should save their fluxes in cells adjacent to
c  their nested fine grids, for later conservation fixing.
c  listbc holds info for where to save which fluxes.
c  xflux holds 'f' fluxes, yflux holds 'g' fluxes.
c
c :::::::::::::::::::::::::::::;:::::::::::::::::::::::
 
 
      ispot   = 1
      level   = node(nestlevel,mptr)

 10      if (listbc(1,ispot).eq.0) go to 99          
c
         mkid     = listbc(4,ispot)
         intopl   = listbc(5,ispot)
         nx       = node(ndihi,mkid) - node(ndilo,mkid) + 1
         ny       = node(ndjhi,mkid) - node(ndjlo,mkid) + 1
         lenbckid = 2*(nx+ny)/intrat(level)
         kidlst   = node(ffluxptr,mkid)
         i        = listbc(1,ispot)
         j        = listbc(2,ispot)
         inlist   = kidlst + nvar*(intopl-1) - 1

         if (listbc(3,ispot) .eq. 1) then
c           ::::: Cell i,j is on right side of a fine grid
            do 100 ivar = 1, nvar
               alloc(inlist + ivar) = -xfluxp(i,j,ivar)*dtc*hy
100         continue
         endif

         if (listbc(3,ispot) .eq. 2) then
c           ::::: Cell i,j on bottom side of fine grid
            do 200 ivar = 1, nvar
               alloc(inlist + ivar) = -yfluxm(i,j+1,ivar)*dtc*hx
200         continue
         endif

         if (listbc(3,ispot) .eq. 3) then
c           ::::: Cell i,j on left side of fine grid
            do 300 ivar = 1, nvar
               alloc(inlist + ivar) = -xfluxm(i+1,j,ivar)*dtc*hy
300         continue
         endif

         if (listbc(3,ispot) .eq. 4) then
c           ::::: Cell i,j on top side of fine grid
            do 400 ivar = 1, nvar
               alloc(inlist + ivar) = -yfluxp(i,j,ivar)*dtc*hx
400         continue
         endif
c
      ispot = ispot + 1
      if (ispot .gt. maxsp) go to 99
      go to 10
c
 99   return
      end
c
c -------------------------------------------------------------
c
      subroutine ginit(msave, first, nvar, naux)
c
      implicit double precision (a-h,o-z)
      logical first

      include  "call.i"

c ::::::::::::::::::::::::::::: GINIT ::::::::::::::::::::::::
c
c  initializes soln on all grids at 'level'  by calling qinit
c  if first = true, (first call to init), then allocate the
c  soln storage area too, else was already allocated.
c
c :::::::::::::::::::::::::::::::::::::::;::::::::::::::::::::

      if (msave .eq. 0) go to 99

      level = node(nestlevel,msave)
      hx    = hxposs(level)
      hy    = hyposs(level)
      mptr  = msave

 10       nx      = node(ndihi,mptr) - node(ndilo,mptr) + 1
          ny      = node(ndjhi,mptr) - node(ndjlo,mptr) + 1
          mitot   = nx + 2*nghost
          mjtot   = ny + 2*nghost
          corn1   = rnode(cornxlo,mptr)
          corn2   = rnode(cornylo,mptr)
          if(.not. (first)) go to 20
              loc                 = igetsp(mitot*mjtot*nvar)
              node(store1,mptr)   = loc
              if (naux .gt. 0) then
                locaux              = igetsp(mitot*mjtot*naux)
                maxmx = mitot - 2*nghost
                mx = maxmx
                maxmy = mjtot - 2*nghost
                my = maxmy
                call setaux(maxmx,maxmy,nghost,mx,my,corn1,corn2,hx,hy,
     &                    naux,alloc(locaux))
              else 
                locaux = 1
              endif
              node(storeaux,mptr) = locaux
              if (level .lt. mxnest) then
                loc2              = igetsp(mitot*mjtot*nvar)
                node(store2,mptr) = loc2
              endif
              rnode(timemult, mptr) = 0.0
              go to 30
 20       continue
c
c  if 2nd time through, put initial values in store2 so finer grids
c  can be advanced with interpolation of their boundary values.
c  new time soln should still be in location store1.
c
          loc     = node(store2,mptr)
          locaux  = node(storeaux,mptr)
c
   30     continue
          maxmx = mitot - 2*nghost
          mx = maxmx
          maxmy = mjtot - 2*nghost
          my = maxmy
          call qinit(maxmx,maxmy,nvar,nghost,mx,my,corn1,corn2,hx,hy,
     &               alloc(loc),naux,alloc(locaux))

c         call qinit(alloc(loc),nvar,mitot,mjtot,
c    1               corn1-nghost*hx,corn2-nghost*hy,hx,hy,
c    2               alloc(locaux),naux)
c
          mptr  = node(levelptr, mptr)
      if (mptr .ne. 0) go to 10
c
c
 99   return
      end
c
c ---------------------------------------------------------
c
      subroutine grdfit (lbase,lcheck,nvar,naux,cut,time)
c
      implicit double precision (a-h,o-z)

      include  "call.i"
c
      dimension  corner(nsize,maxcl)
      integer    numptc(maxcl), prvptr
      logical    fit, nestck, cout
      data       cout/.false./
c
c ::::::::::::::::::::: GRDFIT :::::::::::::::::::::::::::::::::;
c  grdfit called by setgrd and regrid to actually fit the new grids
c         on each level. lcheck is the level being error estimated
c         so that lcheck+1 will be the level of the new grids.
c ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::;
c
      isize = iregsz(lcheck)
      jsize = jregsz(lcheck)
      ldom2 = igetsp((isize+2)*(jsize+2))

c     ## flag all grids at given level based on error ests.
c     ## npts is number of points actually colated - some
c     ## flagged points turned off due to proper nesting requirement.
c     ## (storage based on nptmax calculation however).

      call flglvl (nvar, naux, lcheck, nptmax, index, lbase, ldom2,npts)
      if (npts .eq. 0) go to 99
c
      levnew    = lcheck + 1
      hxfine    = hxposs(levnew)
      hyfine    = hyposs(levnew)
      lratio    = intrat(lcheck)
c
c     ## call smart_bisect grid gen. to make the clusters
c        till each cluster ok. needs scratch space.
c
       idim = iregsz(lcheck)
       jdim = jregsz(lcheck)
       lociscr = igetsp(idim+jdim)
       locjscr = lociscr + idim
       call smartbis(alloc(index),npts,cut,numptc,nclust,lbase,
     2               corner,alloc(lociscr),alloc(locjscr),idim,jdim)
       call reclam(lociscr,idim+jdim)

       if (gprint) then
          write(outunit,103) nclust
          write(outunit,104) (icl, numptc(icl),icl=1,nclust)
 103      format(' ',i4,' clusters after bisect')
 104      format('         cluster ',i5,' has points: ',i6)
       endif
c
c     ##  for each cluster, fit the actual grid, set up some data structures
c
 50   ibase   =  0
      icl     =  1
      prvptr  =  null
c
 70   mnew      = nodget(dummy)
 75   call  moment(node(1,mnew),alloc(index+2*ibase),numptc(icl),usage)

      if (gprint) write(outunit,100) icl,mnew,usage,numptc(icl)
100   format('         cluster ',i5,' new rect.',i5,
     1       ' usage ',e12.5,' with ',i5,' pts.')

      node(ndilo,mnew) = node(ndilo,mnew)*lratio
      node(ndjlo,mnew) = node(ndjlo,mnew)*lratio
      node(ndihi,mnew) = (node(ndihi,mnew)+1)*lratio - 1
      node(ndjhi,mnew) = (node(ndjhi,mnew)+1)*lratio - 1
      rnode(cornxlo,mnew)  = node(ndilo,mnew)*hxfine + xlower
      rnode(cornylo,mnew)  = node(ndjlo,mnew)*hyfine + ylower
      rnode(cornxhi,mnew)  = (node(ndihi,mnew)+1)*hxfine + xlower
      rnode(cornyhi,mnew)  = (node(ndjhi,mnew)+1)*hyfine + ylower
      node(nestlevel,mnew)     = levnew
      rnode(timemult,mnew)   = time
c
c     ##  if new grid doesn't fit in base grid, nestck bisect it
c     ##  and returns 2 clusters where there used to be 1.
c
c 2/28/02 : Added naux to argument list; needed by call to outtre in nestck
      fit = nestck(mnew,lbase,alloc(index+2*ibase),numptc(icl),numptc,
     1             icl,nclust,alloc(ldom2),isize,jsize,nvar, naux)
      if (.not. fit) go to 75
c
c     ##  grid accepted. put in list.
      if (newstl(levnew) .eq. null) then
          newstl(levnew)  = mnew
      else
          node(levelptr,prvptr) = mnew
      endif
      prvptr = mnew

c     ##  on to next cluster
      ibase  = ibase + numptc(icl)
      icl = icl + 1
      if (icl .le. nclust) go to 70

c
      if (cout) then
         call drawrg(time,lcheck,newstl(levnew),
     1               nclust,numptc,npts,alloc(index))
      endif

c    ##  clean up. for all grids check final size.

      call birect(newstl(levnew))
      call reclam(index, 2*nptmax)
c
 99   call reclam(ldom2, (isize+2)*(jsize+2))

      return
      end
c
c ------------------------------------------------------
c
       subroutine intfil(val,mi,mj,time,locuse,nrowst,ncolst,
     2                   ilo,ihi,jlo,jhi,level,nvar,naux)
c
c ::::::::::::::::::::::: INTFIL ::::::::::::::::::::::::::::::::;
c  INTFIL: interpolates values for a patch at the specified level and
c  location, using values from grids at LEVEL and coarser, if nec.
c
c  take the intersection of a grid patch with corners at ilo,ihi,jlo,jhi
c  and all grids mptr at LEVEL.  If there is a non-null intersection
c  copy the solution vaues from mptr (at TIME) into VAL array.
c  assumes patch at same level so do straight copy, not skipping
c  every intrat or doing any interpolation here,
c  assume called in correct order of levels, so that when copying
c  is ok to overwrite.
c
c  N.B.: there are no dummy points around patch, since
c        this is not an official "grid" but a "patch".
c
c  used array marks when point filled. filpatch checks if points left over
c  after intersections at specified level.
c :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::;
c
      implicit double precision (a-h,o-z)

      include  "call.i"
c
      dimension   val(mi,mj,nvar)
      logical     tinterp
c
      iadd(i,j,ivar)   = loc    + i - 1 + mitot*((ivar-1)*mjtot+j-1)
      iadnew(i,j,ivar) = locnew + i - 1 + mitot*((ivar-1)*mjtot+j-1)
      iadold(i,j,ivar) = locold + i - 1 + mitot*((ivar-1)*mjtot+j-1)
      iaduse(i,j)      = locuse + i - 1 + nrow*(j-1)
c
      dt     = possk(level)
      teps   = dt / 10.

      nrow = ihi - ilo + 1
      ncol = jhi - jlo + 1
      ntot   = nrow *ncol
      do 5 i = 1, ntot
5        alloc(locuse+i-1) = 0.0

      mptr   = lstart(level)
 10   if (mptr .eq. 0) go to 105
c
c     :::  check if grid mptr and patch intersect
c
      imlo = node(ndilo, mptr)
      jmlo = node(ndjlo, mptr)
      imhi = node(ndihi, mptr)
      jmhi = node(ndjhi, mptr)

      nx     = node(ndihi,mptr) - node(ndilo,mptr) + 1
      ny     = node(ndjhi,mptr) - node(ndjlo,mptr) + 1
      mitot = nx + 2*nghost
      mjtot = ny + 2*nghost

      ixlo = max(imlo,ilo)
      ixhi = min(imhi,ihi)
      jxlo = max(jmlo,jlo)
      jxhi = min(jmhi,jhi)
c
      if (.not.(ixlo .le. ixhi .and. jxlo .le. jxhi)) go to 90
c
c  :::  grids intersect. figure out what time to use.
c  :::  alphai = 1 for new time; 0 for old time
c
      alphac = (rnode(timemult,mptr) - time)/dt
      alphai = 1.d0-alphac

      if ((alphai .lt. -teps) .or. (alphai .gt. 1.d0+teps)) then
          write(outunit,900) time, mptr, level
          write(*,900) time, mptr, level
 900      format(' time wanted ',e15.7,' not available from grid ',i4,
     1           'level',i4)
          write(outunit,901) ilo,ihi,jlo,jhi,mptr,level,time,
     .                 rnode(timemult,mptr),alphai,teps
          write(*,901) ilo,ihi,jlo,jhi,mptr,level,time,
     .                 rnode(timemult,mptr),alphai,teps
          call outtre(mstart,.false.,nvar,naux)
          stop
      endif
c
      tinterp = .false.
      if (dabs(alphai - 1.d0) .lt. teps) then
          loc = node(store1,mptr)
      else if (dabs(alphai) .lt. teps) then
          loc = node(store2,mptr)
          if (level .eq. mxnest) then
             write(outunit,901) ilo,ihi,jlo,jhi,mptr,level,time,
     .                          rnode(timemult,mptr),alphai,teps
             write(*,901) ilo,ihi,jlo,jhi,mptr,level,time,
     .                          rnode(timemult,mptr),alphai,teps
             stop
           endif
      else
          locold  = node(store2,mptr)
          locnew  = node(store1,mptr)
          tinterp = .true.
          if (level .eq. mxnest) then
             write(outunit,901) ilo,ihi,jlo,jhi,mptr,level,time,
     .                    rnode(timemult,mptr),alphai,teps
             write(*,901) ilo,ihi,jlo,jhi,mptr,level,time,
     .                    rnode(timemult,mptr),alphai,teps
             stop
          endif
      endif
 901  format(' trying to interpolate from previous time values ',/,
     .       ' for a patch with corners ilo,ihi,jlo,jhi:'
     .       ,/,2x,4i10,/,
     .       ' from source grid ',i4,' at level ',i4,/,
     .       ' time wanted ',e15.7,' source time is ',e15.7,/,
     .       ' alphai, teps ',2e15.7)
c
      if (.not. tinterp) then
c     ::: no time interp. copy the solution values
         do 45 ivar = 1, nvar
         do 35 j = jxlo, jxhi
         do 20 i = ixlo, ixhi
             val(i-ilo+nrowst,j-jlo+ncolst,ivar) =
     1            alloc(iadd(i-imlo+nghost+1,j-jmlo+nghost+1, ivar))
             alloc(iaduse(i-ilo+1,j-jlo+1)) = 1.d0
 20      continue
 35      continue
 45      continue
      else
c     ::: linear interpolation in time
         do 85 ivar = 1, nvar
         do 75 j = jxlo, jxhi
         do 65 i = ixlo, ixhi
           val(i-ilo+nrowst,j-jlo+ncolst,ivar) =
     1      alloc(iadnew(i-imlo+nghost+1,j-jmlo+nghost+1,ivar))*alphai +
     2      alloc(iadold(i-imlo+nghost+1,j-jmlo+nghost+1,ivar))*alphac
            alloc(iaduse(i-ilo+1,j-jlo+1)) = 1.d0
 65      continue
 75      continue
 85      continue
      endif
c
 90   mptr = node(levelptr, mptr)
      go to 10
c
 105  continue

c  set used array points which intersect boundary to be equal to 1;
c  they will be set elsewhere

      if (jhi .ge. jregsz(level)) then
        do 1000 j = max(jregsz(level),jlo), jhi
        do 1000 i = ilo, ihi
           alloc(iaduse(i-ilo+1,j-jlo+1)) = 1.d0
1000    continue
      endif

      if (jlo .lt. 0) then
        ncolend = ncolst + ncol - 1
        do 1200 j = jlo, min(-1,ncolend)
        do 1200 i = ilo, ihi
            alloc(iaduse(i-ilo+1,j-jlo+1)) = 1.d0
1200    continue
      endif

      if (ilo .lt. 0) then
        nrowend = nrowst + nrow - 1
        do 1400 i = ilo, min(-1,nrowend)
        do 1400 j = jlo, jhi
           alloc(iaduse(i-ilo+1,j-jlo+1)) = 1.d0
1400    continue
      endif

      if (ihi .ge. iregsz(level)) then
        do 1600 i = max(iregsz(level),ilo), ihi
        do 1600 j = jlo, jhi
           alloc(iaduse(i-ilo+1,j-jlo+1)) = 1.d0
1600    continue
      endif
c
      return
      end
c
c ----------------------------------------------------------
c
      subroutine moment (intrect,badpts,npt,usage)
c
      implicit double precision (a-h,o-z)

      include "call.i"

      dimension     intrect(nsize),badpts(2,npt)
c
c :::::::::::::::::::::::: MOMENT ::::::::::::::::::::::::::::::::::
c  moment = compute enclosing rectangle around flagged points.
c  save some info., even tho. usage might be low and rect. scrapped.

c input parameters:
c     badpts      = x,y coords of flagged badpts grouped into clusters
c                   are in the first two rows
c     npt         = num. of badpts. in the cluster.
c
c output parameters:
c     usage       = ratio of flagged to unflagged badpts. in new grid
c                   measures goodness of fit and clustering
c    intrect( )    = stores some info. for grid created herein.
c                   sometimes rect = rnode, sometimes = temp. array.
c                   sometimes intrect = node.
c                   depending on calling prog. (grdfit or expand)
c
c
c :::::::::::::::::::::::: MOMENT ::::::::::::::::::::::::::::::::::
c
      rn = float(npt)
c
c compute length of enclosing rectangles to include all flagged badpts.
c
      emx1 = badpts(1,1)
      emn1 = emx1
      emx2 = badpts(2,1)
      emn2 = emx2
      do 80 ipt = 1, npt
          if (badpts(1,ipt) .gt. emx1) emx1 = badpts(1,ipt)
          if (badpts(1,ipt) .lt. emn1) emn1 = badpts(1,ipt)
          if (badpts(2,ipt) .gt. emx2) emx2 = badpts(2,ipt)
          if (badpts(2,ipt) .lt. emn2) emn2 = badpts(2,ipt)
 80   continue
c
c from length of the sides determine rect. corners.
c transform to cell numbers (subtract .5)
c
      intrect(ndilo) = nint(emn1 - .5)
      intrect(ndjlo) = nint(emn2 - .5)
      intrect(ndihi) = nint(emx1 - .5)
      intrect(ndjhi) = nint(emx2 - .5)
c
c compute usage
c
      iside1 = intrect(ndihi) - intrect(ndilo) + 1
      iside2 = intrect(ndjhi) - intrect(ndjlo) + 1
      gpall  = iside1 * iside2
      usage  = rn / gpall
c
      return
      end
c
c ---------------------------------------------------------
c
      logical function nestck(mnew,lbase,badpts,npts,numptc,icl,
     1                        nclust,domflags,isize,jsize,
     &      nvar,naux)
c
      implicit double precision (a-h,o-z)
      dimension  badpts(2,npts)
      integer    domflags(0:isize+1,0:jsize+1)

      include  "call.i"

      integer   numptc(maxcl)
c
c ::::::::::::::::::::::: NESTCK :::::::::::::::::::::::::::::::::::
c
c nestck - check that the potential grid mnew is completely
c          contained in the (coarser) finest grid which stays
c          fixed, at level lbase. projec algo. will guarantee
c          containment in all finer grids twixt them.
c          if grid not contained in some coarse grid,  then
c          bisect in long direction.
c          EVENTUALLY this has to work.
c
c input parameter:
c   mnew    - grid descriptor of potential grid
c   lbase   - level which stays fixed during regridding
c   badpts  - only the flagged pts. in this cluster (# icl)
c   domflags - the lbase domain (expanded up to level lcheck
c              which new grid must fit into
c :::::::::::::::::::::::::::::::::;::::::::::::::::::::::::::::::::
c
       nestck = .true.
c
c      # for convex coarsest grid at level 1, nothing to check
       if (lbase .eq. 1)  go to 99

       levnew = node(nestlevel,mnew)
       lratio  = intrat(levnew-1)
c
c  ### use grid indices coarsened by 1 level in checking
c  ### remember to offset by 1 since 1st grid cell is 0,0
c
       do 10 i = node(ndilo,mnew)/lratio+1, node(ndihi,mnew)/lratio+1
       do 10 j = node(ndjlo,mnew)/lratio+1, node(ndjhi,mnew)/lratio+1
          if (domflags(i,j) .eq. 0) go to 50
 10    continue
c
c      if made it here, then mnew is properly nested
       go to 99
c
c  ### grid not properly nested. bisect in long direction, and return
c  ### two clusters instead of 1.
c
 50    if (npts .gt. 1) go to 55
           write(outunit,101) levnew
           write(*,101)       levnew
 101       format(' nestck: 1 pt. cluster at level ',i5,' still not',
     1       ' nested',/,'          pt. too close to boundary')
           write(outunit,104) badpts(1,npts),badpts(2,npts)
           write(*,104)       badpts(1,npts),badpts(2,npts)
 104       format(' non-nested flagged pt. at: ',2e15.7)
           call outtre(mstart, .false.,nvar,naux)
           call outmsh(mnew, .false.,nvar,naux)
           stop

 55    if (nclust .lt. maxcl) go to 60
           write(outunit,102) maxcl
           write(*,102)       maxcl
 102       format(' too many clusters: > ',i5,' (from nestck) ')
           stop

 60   if (nprint) write(outunit,103) icl, npts
 103  format(' bisecting cluster ',i5,' with ',i5,' pts. in nestck')
      if (rnode(cornxhi,mnew)-rnode(cornxlo,mnew) .gt.
     1    rnode(cornyhi,mnew) - rnode(cornylo,mnew)) then
           rmid  = (rnode(cornxhi,mnew) + rnode(cornxlo,mnew) ) / 2.
           rmid  = (node(ndihi,mnew) + node(ndilo,mnew) + 1 ) / 2.
           rmid  = rmid / lratio
           idir = 1
       else
           rmid  = (rnode(cornyhi,mnew) + rnode(cornylo,mnew) ) / 2.
           rmid  = (node(ndjhi,mnew) + node(ndjlo,mnew) + 1) / 2.
           rmid  = rmid / lratio
           idir = 2
       endif
c
       ipt  = 1
       ntop = npts

 90    if (badpts(idir,ipt) .lt. rmid) go to 100
c
c  ### swap with a point in top half not yet tested. keep smaller
c  ### half of rect. in bottom half
c
       temp           = badpts(1,ipt)
       badpts(1,ipt)  = badpts(1,ntop)
       badpts(1,ntop) = temp
       temp           = badpts(2,ipt)
       badpts(2,ipt)  = badpts(2,ntop)
       badpts(2,ntop) = temp
       ntop           = ntop - 1
       if (ipt .le. ntop) go to 90
       go to 110
 100   ipt = ipt +1
       if (ipt .le. ntop) go to 90
c
c  ### ntop points to top of 1st cluster (= no. of points in 1st cluster)
c
 110   numptc(icl)     = npts - ntop
       do 120 i        = icl, nclust
          nmove           = nclust + icl - i
 120      numptc(nmove+1) = numptc(nmove)
       numptc(icl)     = ntop
       nclust          = nclust + 1
       nestck          = .false.
c
 99    return
       end
c
c ----------------------------------------------------------
c
      subroutine prepc(level,nvar)
c
      implicit double precision (a-h,o-z)

      include  "call.i"
c
c :::::::::::::::::::: PREPC ::::::::::::::::::::::::::::::::::::::
c
c this routine called because regridding just changed the fine grids.
c modify coarse grid boundary lists to store fluxes in appropriate
c fine grids lists.
c assume new fine grids have node(cfluxptr) initialized to point to null
c
c  first compute max. possible number of list cells. allocate
c  initially so that one pass through is enough.
c
c ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c
      maxsp  = 0
      mkid   = lstart(level+1)
 10   if (mkid .eq. 0) go to 20
         ikeep  = (node(ndihi,mkid)-node(ndilo,mkid)+1)/intrat(level)
         jkeep  = (node(ndjhi,mkid)-node(ndjlo,mkid)+1)/intrat(level)
         maxsp  = maxsp + 2*(ikeep+jkeep)
      mkid = node(levelptr,mkid)
      go to 10
 20   listsp(level) = maxsp
      if (maxsp .eq. 0) go to 99
c
      hxpar   = hxposs(level)
      hypar   = hyposs(level)
      hxkid   = hxposs(level+1)
      hykid   = hyposs(level+1)
      imax    = iregsz(level) - 1
      jmax    = jregsz(level) - 1

      mpar = lstart(level)
 30   if (mpar .eq. 0) go to 99
c
       ispot   = 0
       ilo     = node(ndilo,mpar)
       jlo     = node(ndjlo,mpar)
       ihi     = node(ndihi,mpar)
       jhi     = node(ndjhi,mpar)
       locbc   = igetsp(5*maxsp)
c      #  initialize list space to 0 (0 terminator indicates end of bc list)
       do 35 i = 1,5*maxsp
 35      alloc(locbc+i-1) = 0.d0
       node(cfluxptr,mpar) = locbc
c
       mkid = lstart(level+1)
 40    if (mkid .eq. 0) go to 60

          iclo = node(ndilo,mkid)/intrat(level)
          jclo = node(ndjlo,mkid)/intrat(level)
          ichi = node(ndihi,mkid)/intrat(level)
          jchi = node(ndjhi,mkid)/intrat(level)

          iplo = max(ilo,iclo)
          jplo = max(jlo,jclo)
          iphi = min(ihi,ichi)
          jphi = min(jhi,jchi)

c   regular intersections
          if (iplo .le. iphi+1 .and. jplo .le. jphi+1) 
     1          call setuse(alloc(locbc),maxsp,ispot,mkid,intrat(level),
     2          ilo,ihi,jlo,jhi,iclo,ichi,jclo,jchi,nghost)

c   for fine grids touching periodic boundary on right
          if  (xperdom .and. ilo .eq. 0 .and. ichi .eq. imax) 
     1        call setuse(alloc(locbc),maxsp,ispot,mkid,intrat(level),
     2          ilo,ihi,jlo,jhi,iclo-iregsz(level),ichi-iregsz(level),
     3          jclo,jchi,nghost)

c   for fine grids touching periodic boundary on left
          if  (xperdom .and. iclo .eq. 0 .and. ihi .eq. imax) 
     1        call setuse(alloc(locbc),maxsp,ispot,mkid,intrat(level),
     2          ilo,ihi,jlo,jhi,iclo+iregsz(level),ichi+iregsz(level),
     3          jclo,jchi,nghost)

c   for fine grids touching periodic boundary on top
          if  (yperdom .and. jlo .eq. 0 .and. jchi .eq. jmax) 
     1          call setuse(alloc(locbc),maxsp,ispot,mkid,intrat(level),
     2          ilo,ihi,jlo,jhi,iclo,ichi,
     3          jclo-jregsz(level),jchi-jregsz(level),nghost)

c   for fine grids touching periodic boundary on bottom
          if  (yperdom .and. jclo .eq. 0 .and. jhi .eq. jmax) 
     1        call setuse(alloc(locbc),maxsp,ispot,mkid,intrat(level),
     2          ilo,ihi,jlo,jhi,iclo,ichi,
     3          jclo+jregsz(level),jchi+jregsz(level),nghost)

 50     mkid = node(levelptr,mkid)
        go to 40
c
c  done with subgrid cycle. if no cells would need fixing, all done
c  else cycle through again to set up list with info. for bc processing
c
 60     continue
c
c  for now, leave unused space allocated to the grid. alternative is
c  to return (maxsp-ispot) amount starting at loc. node(cfluxptr,mpar)+ispot.
c
       mpar = node(levelptr,mpar)
       go to 30
c
 99    return
       end
c
c ----------------------------------------------------------
c
      subroutine prepf(level,nvar,naux)
c
      implicit double precision (a-h,o-z)

      include  "call.i"
c
c ::::::::::::::::::::::::::: PREPF :::::::::::::::::::::::::::::
c
c prepare new fine grids to save fluxes after each integration step
c for future conservative fixing of coarse grids
c save all boundary fluxes of fine grid (even if phys. bndry.) -
c but only save space for every intrat. (remember - 4 fluxes).
c
c :::::::::::::::::::::::::::::::::::;:::::::::::::::::::::::::::
c
      mkid = lstart(level)
 10   if (mkid .eq. 0) go to 99
          nx    = node(ndihi,mkid)-node(ndilo,mkid) + 1
          ny    = node(ndjhi,mkid)-node(ndjlo,mkid) + 1
          ikeep = nx/intrat(level-1)
          jkeep = ny/intrat(level-1)
          lenbc = 2*(ikeep+jkeep)
c
c         get twice the storage, one for plus or minus fluxes, the
c         other for coarse solution for wave fixing. also for aux vars.
c
          node(ffluxptr,mkid) = igetsp(2*nvar*lenbc+naux*lenbc)
          ist   = node(ffluxptr,mkid)

          do 20 i = 1, 2*nvar*lenbc + naux*lenbc
 20          alloc(ist+i-1) = 0.d0
          mkid = node(levelptr,mkid)
          go to 10

 99    return
       end
c
c ---------------------------------------------------------
c
      subroutine projec(level,numpro,iflags,isize,jsize,lrat2)
c
      implicit double precision (a-h,o-z)

      include  "call.i"

      dimension iflags(0:isize+1,0:jsize+1)
c
c  ::::::::::::::::::::::: PROJEC ::::::::::::::::::::::::::::::
c  for all newly created fine grids, project area onto a coarser
c  grid 2 levels down. Used to recreate grids 1 level down, and
c  insure proper level nesting.
c
c  on entry, all coarse grids have already had error estimated, so
c  add bad flags.   count number of 'added' flags only.
c
c input parameters:
c    level = project all fine subgrids onto grids at this level.
c output parameters:
c  numpro = number of additional flagged pts. at 'level'.
c           (initialized to 0 in flglvl)
c local variables:
c     iflags  = holds coarser domain flagged points - receives projection
c     mkid    = grid doing the projecting
c  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c
      hx      = hxposs(level)
      hy      = hyposs(level)
      hxmid   = hxposs(level+1)
      hymid   = hyposs(level+1)
      levpro  =  level + 2
c
      mkid = newstl(levpro)
 10   if (mkid .eq. 0) go to 90
       ilo = node(ndilo,mkid) 
       jlo = node(ndjlo,mkid)
       ihi = node(ndihi,mkid)
       jhi = node(ndjhi,mkid)
c
c  project entire region of fine grids into iflags array.
c  possibly take care of buffering.
c  adjust since grid descriptor (integer indices)  is 0 based, 
c  iflags indexing is 1 based. 
c
      ist  = ilo/lrat2 
      jst  = jlo/lrat2
      iend = ihi/lrat2 
      jend = jhi/lrat2
      if (ibuff .eq. 0) then
c     ## ensure proper nesting here, since buffering step won't follow
        if (ist*lrat2 .eq. ilo) ist = ist-1
        if (jst*lrat2 .eq. jlo) jst = jst-1
        if ((iend+1)*lrat2 .eq. ihi+1) iend = iend+1
        if ((jend+1)*lrat2 .eq. jhi+1) jend = jend+1
      endif
c
       do 60 j = jst+1, jend+1
       do 60 i = ist+1, iend+1
           if (iflags(i,j) .eq. goodpt) then
               iflags(i,j) = badpro
               numpro      = numpro + 1
               if (pprint) write(outunit,101) i,j,mkid
101            format(' pt.',2i5,' of grid ',i5,' projected' )
           endif
 60    continue
c
c  done with gridpt. loop for grid mkid.
c
 80     mkid = node(levelptr, mkid)
        go to 10
c
 90   if (numpro .eq. 0) go to 95
      write(outunit,102) numpro,level
 102  format(i7,' more pts. projected to level ',i5)
c
 95   if (pprint) then
         write(outunit,103) level
 103     format(/,'  from projec: flagged pts. at level ',i4,':')
         do 110 jj = 1, jsize
            j        = jsize + 1 - jj
            write(outunit,104) (iflags(i,j),i=1,isize)
104         format(80i1)
 110     continue
      endif
c
 99   return
      end
c
c --------------------------------------------------------------------
c
       subroutine signs(badpts,npts,iscr,jscr,idim,jdim,ist,iend,
     &                  ilo,ihi,jlo,jhi)
c
       implicit double precision (a-h,o-z)
       dimension badpts(2,npts)
       dimension iscr(idim), jscr(jdim)
c
c :::::::::::::::::::::::::::: SIGNS ::::::::::::::::::::::::::::::
c  compute signatures = number of flagged cells in each row/column.
c  also return first and last nonzero row/column, so don't have
c  to waste time over entire region.
c :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c
       ilo= 1
       ihi= idim
       jlo= 1
       jhi= jdim
       do 10 i = 1, idim
 10       iscr(i) = 0
       do 15 j = 1, jdim
 15       jscr(j) = 0
c
c count all flagged points in a given row/column in one pass through
c the points, i.e. a bin count
c
       do 20 ipt = ist, iend
          iloc  =  badpts(1,ipt)+1.1
          jloc  =  badpts(2,ipt)+1.1
          iscr(iloc) = iscr(iloc)+1
          jscr(jloc) = jscr(jloc)+1
 20    continue
c
       do 30 ipt = 1, idim
         if (iscr(ipt) .ne. 0) then
            ilo = ipt
            go to 40
         endif
 30    continue
 40    do 50 ipt = 1, idim
          if (iscr(idim+1-ipt) .ne. 0) then
             ihi = idim+1-ipt
             go to 60
          endif
 50    continue

 60    do 70 ipt = 1, jdim
          if (jscr(ipt) .ne. 0) then
             jlo = ipt
             go to 80
          endif
 70    continue
 80    do 90 ipt = 1, jdim
          if (jscr(jdim+1-ipt) .ne. 0) then
             jhi = jdim+1-ipt
             go to 99
          endif
 90    continue

 99    return
       end
c
c -----------------------------------------------------------
c
      subroutine findcut(icl,iscr,jscr,idim,jdim,index,iside,
     1                   ilo,ihi,jlo,jhi)
c
c ::::::::::::::::::::: FINDCUT ::::::::::::::::::::::::::::;
c   find best place to split the 2D array of flagged points
c   either split at a hole, or use signatures to find
c   zero crossing of laplacian.
c ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::;
c
      implicit double precision (a-h,o-z)

      include "call.i"

      dimension iscr(idim), jscr(jdim)

c Modified 6/02:
c Include call.i to get def's of horizontal/vertical.
c      integer   horizontal, vertical
c      parameter(horizontal = 1)
c      parameter(vertical = 2)

      parameter(ithres = 2)
      parameter(minoff = 4)
c
c  look for holes first in horizontal then vertical direction
c
       do 10 i = ilo, ihi
          if (iscr(i) .eq. 0) then
             index = i
             iside = horizontal
             return
          endif
 10    continue

       do 20 j = jlo, jhi
          if (jscr(j) .eq. 0) then
              index = j
              iside = vertical
              return
          endif
 20    continue

c
c  no holes - find 2nd derivative of signatures for best cut.
c  overwrite signature arrays. don't make cuts less than minoff
c  from boundary
c
      ipre = iscr(ilo)
      do 50 i = ilo+1, ihi-1
         icur = iscr(i)
         iscr(i) = iscr(i+1)-2*icur+ipre
         ipre = icur
 50   continue

      locmaxi = 0
      indexi  = 0
      imid    = (ilo + ihi) / 2
      do 60 i = ilo+minoff, ihi-minoff+1
           itemp1 = iscr(i-1)
           itemp2 = iscr(i)
           locdif = iabs(itemp1-itemp2)
           if (itemp1*itemp2.lt.0) then
                if (locdif .gt. locmaxi) then
                 locmaxi = locdif
                 indexi = i
                else if (locdif .eq. locmaxi) then
                    if (iabs(i-imid).lt.iabs(indexi-imid)) indexi = i
                endif
           endif
 60   continue


       jpre   = jscr(jlo)
       do 130 j = jlo+1, jhi-1
           jcur = jscr(j)
           jscr(j) = jscr(j+1)-2*jcur+jpre
           jpre = jcur
 130   continue

       locmaxj = 0
       indexj  = 0
       jmid    = (jlo + jhi) / 2
       do 160 j = jlo+minoff, jhi-minoff+1
           jtemp1 = jscr(j-1)
           jtemp2 = jscr(j)
           locdif = iabs(jtemp1-jtemp2)
           if (jtemp1*jtemp2.lt.0) then
               if (locdif .gt. locmaxj) then
                  locmaxj = locdif
                  indexj = j
                else if (locdif .eq. locmaxj) then
                       if (iabs(j-jmid).lt.iabs(indexj-jmid)) indexj = j
               endif
           endif
 160   continue
c
c      ::::: choose max dif for splitting
c
 220   if (locmaxi .gt. locmaxj) then
            index  = indexi
            iside  = horizontal
            locmax = locmaxi
       else if (locmaxi .lt. locmaxj) then
            index  = indexj
            iside  = vertical
            locmax = locmaxj
        else if (iabs(indexi-imid).lt.iabs(indexj-jmid)) then
                 index  = indexi
                 iside  = horizontal
                 locmax = locmaxi
             else
               index  = indexj
               iside  = vertical
               locmax = locmaxj
       endif

c      ::::: if inflection pt. not over the threshold, signal
c      ::::: with index 0 (out of range)
       if (locmax .lt. ithres) index = 0

       return
       end
c
c ---------------------------------------------------------
c
      subroutine smartbis(badpts,npts,cutoff,numptc,nclust,
     1                    lbase,intcorn,iscr,jscr,idim,jdim)
c
      implicit double precision (a-h,o-z)

      include "call.i"

      dimension     badpts(2,npts),intcorn(nsize,maxcl)
      dimension     iscr(idim), jscr(jdim)
      integer       nclust, numptc(maxcl)
      parameter     (usemin=.4)
c
c :::::::::::::::::::::::::::: SMARTBIS :::::::::::::::::::::::::;
c smart bisect rectangles until cutoff reached for each.
c replaced old bisection routine that cut all grids in half.
c now look for good place to do the cut, based on holes or signatures.
c
c :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::;
c
c     ## initially all points in 1 cluster
      nclust      = 1
      numptc(1)   = npts

      if (gprint) write(outunit,100) nclust
 100  format(' starting smart bisection with ',i5,' clusters')
c
      icl         = 1
      ist         = 1
      iend        = numptc(icl)
c
 10   call moment(intcorn(1,icl),badpts(1,ist),numptc(icl),usenew)
      if (gprint) write(outunit,101) icl,numptc(icl),usenew
 101  format('   testing cluster ',i4,' with ',i5,' pts. use ',e12.4)
c
      if (usenew .lt. cutoff) go to 20
c
c  this cluster ok - on to next
c
      if (.not. gprint) go to 15
         write(outunit,102) icl,numptc(icl),usenew
 102     format('     accepting smart bisected cluster',i4,' with ',i5,
     1          ' pts. use = ',e10.3)
 15   icl   = icl + 1
      if (icl .gt. nclust) go to 200
      ist   = iend + 1
      iend  = ist + numptc(icl) - 1
      go to 10
c
c  smart bisect rectangle (and its cluster) in best location
c
 20   if (nclust .lt. maxcl) go to 25
          write(outunit,900) maxcl
          write(*      ,900) maxcl
 900      format('  too many clusters:  > ',i5)
          stop
 25   continue
c
c smart bisection computes signatures, finds best cut and splits there
c
      call signs(badpts,npts,iscr,jscr,idim,jdim,
     &           ist,iend,ilo,ihi,jlo,jhi)
      call findcut(icl,iscr,jscr,idim,jdim,index,iside,
     &             ilo,ihi,jlo,jhi)
      if (index .eq. 0) then

c         if (usenew .gt. usemin) then
c           icl = icl + 1
c           if (icl .gt. nclust) go to 200
c           ist = iend + 1
c           iend = ist + numptc(icl) - 1
c           go to 10
c         else
c c         bisect in long direction
c           if (ihi-ilo .gt. jhi-jlo) then
c              iside = horizontal
c              index = (ilo + ihi)/2
c           else
c              iside = vertical
c              index = (jlo + jhi)/2
c           endif
c          endif

c 2/28/02 : 3d version uses this branch only;  no 'if' statement.
         icl = icl + 1
         if (icl .gt. nclust) go to 200
         ist = iend + 1
         iend = ist + numptc(icl) - 1
         go to 10
      endif
c
      if (iside .eq. vertical) then
c        fmid = (index-.5)*hy
         fmid = (index-.5)
         idir = 2
      else
         fmid = (index-.5)
         idir = 1
      endif
c
      itop = ist - 1
      ibot = iend + 1
      i    = ist
 50   if (badpts(idir,i) .lt. fmid) go to 60
c
c  point in top half. let it stay, increment counter
c
        itop = itop + 1
        if (itop+1 .ge. ibot) go to 80
             i = i + 1
             go to 50
c
c  point in bottom half. switch with a bottom point that's not yet
c  checked, and increment bot. pointer
c
 60    ibot           = ibot - 1
       temp           = badpts(1,ibot)
       badpts(1,ibot) = badpts(1,i)
       badpts(1,i)    = temp
       temp           = badpts(2,ibot)
       badpts(2,ibot) = badpts(2,i)
       badpts(2,i)    = temp
       if (itop+1 .lt. ibot) go to 50
c
c done smartbisecting icl'th clusters. adjust counts, repeat bisect stage .
c
 80   numptc(icl) = itop - ist + 1
      ibump       = icl + 1
c
c  bump down remaining clusters to make room for the new half of one.
c
      if (ibump .gt. nclust) go to 120
      do 90 ico         = ibump, nclust
      nmove             = nclust - ico + ibump
 90   numptc(nmove + 1) = numptc(nmove)

 120  numptc(ibump)     = iend - ibot + 1
      nclust            = nclust + 1
      iend              = itop
c
c     other half of the cluster has been inserted into cluster list.
c     icl remains the same - need to redo it.
      go to 10
c
c  done: there are nclust clusters.
c
 200  continue
c
      return
      end
c
c -------------------------------------------------------------
c
      subroutine putnod (mptr)
c
      implicit double precision (a-h,o-z)

      include  "call.i"

c :::::::::::::::::::::::::::::: PUTNOD :::::::::::::::::::::;
c
c  return mptr node to the linked list kept in node array.
c
c ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::;
c
      node(nextfree, mptr) = ndfree
      ndfree               = mptr
c
      return
      end
c
c ----------------------------------------------------------
c
      subroutine putsp(lbase,level,nvar,naux)
c
      implicit double precision (a-h,o-z)

      include  "call.i"
c
c ::::::::::::::::::::::::::::::: PUTSP :::::::::::::::::::::::::
c
c reclaim list space in nodes cfluxptr and ffluxptr for grids at level
c
c first compute max. space allocated in node cfluxptr.
c
c :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c
      if (level .eq. lfine) go to 30
c
      mptr  = lstart(level)
 20      call reclam(node(cfluxptr,mptr), 5*listsp(level))
         node(cfluxptr,mptr) = 0
      mptr  = node(levelptr,mptr)
      if (mptr .ne. 0) go to 20
c
 30    if (level .eq. lbase) go to 99
      mptr = lstart(level)
 40       nx    = node(ndihi,mptr) - node(ndilo,mptr) + 1
          ny    = node(ndjhi,mptr) - node(ndjlo,mptr) + 1
          ikeep = nx/intrat(level-1)
          jkeep = ny/intrat(level-1)
          lenbc = 2*(ikeep+jkeep)
c         twice perimeter since saving plus or minus fluxes 
c         plus coarse solution storage
          call reclam(node(ffluxptr,mptr), 2*nvar*lenbc+naux*lenbc)
          mptr  = node(levelptr,mptr)
          if (mptr .ne. 0) go to 40
c
 99    return
       end
c
c -----------------------------------------------------------
c
      subroutine regrid  (nvar,lbase,cut,naux)
c
      implicit double precision (a-h,o-z)

      include  "call.i"
c
c :::::::::::::::::::::::::::: REGRID :::::::::::::::::::::::::::::::

c  regrid = flag points on each grid with a level > = lbase.
c  cluster them, and fit new subgrids around the clusters.
c  the lbase grids stay fixed during regridding operation.
c  when a parent grid has its error estimated, add its kid grid
c  information to the error grid before clustering. (project)
c  order of grid examination - all grids at the same level, then
c  do the next coarser level.
c
c input parameters:
c     lbase  = highest level that stays fixed during regridding
c     cutoff = criteria for measuring goodness of rect. fit.

c local variables:
c     lcheck = the level being examined.
c     lfnew  = finest grid to be. will replace lfine.

c global
c    mstart  = start of very coarsest grids.
c :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c
      lcheck    = min0(lfine,mxnest-1)
      lfnew     = lbase
      do 10 i   = 1, mxnest
 10     newstl(i) = 0
      time      = rnode(timemult, lstart(lbase))
c
 20   if (lcheck .lt. lbase) go to 50
          call grdfit(lbase,lcheck,nvar,naux,cut,time)
          if (newstl(lcheck+1) .eq. 0) go to 40
          lfnew = max0(lcheck + 1,lfnew)
 40       continue
          lcheck = lcheck - 1
c
      go to 20
 50   continue
c
c  end of level loop
c
c  remaining tasks left in regridding:
c
c  interpolate storage for the new grids.  the starting pointers
c  for each level are in newstl. also reclaim some space before new
c  allocations.
      call gfixup(lbase, lfnew, nvar, naux)
c
c  merge data structures (newstl and lstart )
c  finish storage allocation, reclaim space, etc. set up boundary
c  flux conservation arrays
c
      do 60 level = lbase, lfine-1
        call prepf(level+1,nvar,naux)
        call prepc(level,nvar)
 60   continue
c
      return
      end
c
c ---------------------------------------------------------
c
      subroutine restrt(nsteps,time,nvar)
c
      implicit double precision (a-h,o-z)
      logical   ee

      include  "call.i"

      dimension intrt1(maxlv)
c
c :::::::::::::::::::::::::::: RESTRT ::::::::::::::::::::::::::::::::
c read back in the check point files written by subr. check.
c
c some input variables might have changed, and also the
c alloc array could have been written with a smaller size at checkpoint
c ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c
      read(rstunit) lenmax,lendim,isize,(alloc(i),i=1,lendim)
      read(rstunit) hxposs,hyposs,possk,icheck
      read(rstunit) lfree,lenf
      read(rstunit) rnode,node,lstart,newstl,listsp,tl,
     1        ibuf,mstart,ndfree,lfine,iorder,mxnold,
     2        intrt1,iregsz,jregsz,kcheck1,nsteps,time,matlabu
      read(rstunit) evol, rvol, rvoll, lentot

      write(outunit,100) nsteps,time
      write(6,100) nsteps,time
 100  format(/,' RESTARTING the calculation after ',i5,' steps',
     1        /,'  (time = ',e15.7,')')
c
c adjust free list of storage in case size has changed.
c
      idif = memsize - isize
      if (idif .gt. 0) then
          lfree(lenf,1) = isize + 2
          call reclam(isize+1,idif)
      else if (idif .lt. 0) then
            write(outunit,900) isize, memsize
            write(*,900)       isize, memsize
 900        format(' size of alloc not allowed to shrink with ',/,
     .             ' restart old size ',i7,' current size  ',i7)
            stop
      endif
c
c adjust storage in case mxnest has changed - only allow it to increase,
c and only at non-multiples of error estimation on old mxnest.
c
       if (mxnest .eq. mxnold) go to 99

       if (mxnest .lt. mxnold) then
         if (lfine .lt. mxnest) then
             go to 99
         else
             write(outunit,901) mxnold, mxnest
             write(*,      901) mxnold, mxnest
901          format(' only allow mxnest to increase: ',/,
     &            '  old mxnest ',i4, ' new mxnest ',i4)
             stop
         endif
       endif

c      see if simple enough situation to allow changing mxnest
        ee = .false.
        do 10 level = 1, mxnold
           if (icheck(level) .ge. kcheck) then
              ee = .true.
              kmust = icheck(level)
           endif
10      continue
        if (ee) then
           write(outunit,902) mxnold, mxnest, kmust
           write(*      ,902) mxnold, mxnest, kmust
902        format(/,' only allow changes in mxnest (from ',
     &                i4,' to ',i4,')',/,
     &            ' when not time to error estimate: ',/,
     &            ' please run a few more steps before changing ',/,
     &            ' so that # of steps not greater then kcheck',/,
     &            ' or make kcheck > ',i4 )
             stop
        else
c          #  add second storage location to previous mxnest level
           mptr = lstart(mxnold)
15         if (mptr .eq. 0) go to 25
              mitot = node(ndihi,mptr)-node(ndilo,mptr)+1+2*nghost
              mjtot = node(ndjhi,mptr)-node(ndjlo,mptr)+1+2*nghost
              node(store2,mptr) = igetsp(mitot*mjtot*nvar)
              mptr = node(levelptr,mptr)
              go to 15
25         continue
        endif
c
c          # add new info. to spatial and counting arrays
 99        level = lfine + 1
           rr = float(intrat(lfine))
35         if (level .gt. mxnest) go to 45
             hxposs(level) = hxposs(level-1) / rr
             hyposs(level) = hyposs(level-1) / rr
             possk (level) = possk (level-1) / rr
             iregsz(level) = iregsz(level-1) * rr
             jregsz(level) = jregsz(level-1) * rr
             rr            = intrat(level)
             level         = level + 1
             go to 35
45         continue
c
c
      return
      end
c
c -----------------------------------------------------------------
c
      subroutine setgrd (nvar,cut,naux)
c
      implicit double precision (a-h,o-z)

      include  "call.i"

      logical  vtime
      data     vtime/.false./
c     # may as well not bother to calculate time step for error est.
c
c :::::::::::::::::::::::::::: SETGRD :::::::::::::::::::::::::::::::;
c  set up the entire tree/grid structure.  only at this time t = 0
c  can we take advantage of initialization routines.
c  remember that regridding/error estimation needs to have two
c  time steps of soln. values.
c ::::::::::::::::::::::::::::::::::::::;::::::::::::::::::::::::::::;
c
      if (mxnest .eq. 1) go to 99
c
      levnew =  2
      time   = 0.0d0
c
 10   if (levnew .gt. mxnest) go to 30
          levold = levnew - 1
          if (lstart(levold) .eq. 0) go to 30
          lbase  = levold
          lfnew  = lbase
c
c  set up level to be flagged. need a solution t=0,and t=dt.
c  error estimation makes next one at t=2*dt for Richardson.
c
         call advanc(levold,nvar,dtlev,vtime,naux)
         evol = evol + rvol
         rvol = 0.d0
 
c        don't count it in real integration stats
         do 20 level=1,mxnest
 20         rvoll(level) = 0.d0
c
c  flag, cluster, and make new grids
c
         call grdfit(lbase,levold,nvar,naux,cut,time)
         if (newstl(levnew) .ne. 0) lfnew = levnew
c
c  init new level. after each iteration. fix the data structure
c  also reinitalize coarser grids so fine grids can be advanced
c  and interpolate correctly for their bndry vals from coarser grids.
c
         call ginit(newstl(levnew),.true., nvar, naux)
         lstart(levnew) = newstl(levnew)
         lfine = lfnew
         call ginit(lstart(levold),.false., nvar, naux)
c
         levnew = levnew + 1
      go to 10
 30   continue
c
c  switch location of old and new storage for soln. vals, 
c  and reset time to 0.0
c
      if (mxnest .eq. 1) go to 99
c
      lev = 1
 40   if ((lev .eq. mxnest) .or. (lev .gt. lfine))  go to 60
        mptr = lstart(lev)
 50        itemp                = node(store1,mptr)
           node(store1,mptr)    = node(store2,mptr)
           node(store2,mptr)    = itemp
           rnode(timemult,mptr) = 0.0d0
           mptr                 = node(levelptr,mptr)
           if (mptr .ne. 0) go to 50
       lev = lev + 1
       go to 40
 60   continue
c
c initial updating so can do conservation check. can do before
c bndry flux arrays set, since don't need them for this
c
      do 65 level = 1, lfine-1
         call update(lfine-level,nvar)
 65   continue
c
c set up boundary flux conservation arrays
c
      do 70 level = 1, lfine-1
         call prepf(level+1,nvar,naux)
         call prepc(level,nvar)
 70   continue
c
 99   continue
c
      return
      end
c
c ------------------------------------------------------------------
c
       subroutine setuse(listbc,maxsp,ispot,mkid,
     1                   lratio,ilo,ihi,jlo,jhi,
     2                   iclo,ichi,jclo,jchi,nghost)
c
c ::::::::::::::::::::::::: SETUSE ::::::::::::::::::::::::::::::::
c
c set up boundary list for coarse grid, to be used by fluxsv. 
c loop around boundary of c fine grids to do this.  each entry has
c     i, j, side #, fine grid #, loc in fine grid list for fluxes.
c  for example, side 1 of fine grid fixes side 3 of coarse grid,
c  so coarse grid list will store the # 3.
c  wrt coarse grid, the sides are:
c              2
c           1     3            that is, right edge of a coarse cell = 3
c              4                         top  edge of a coarse cell = 2
c
c  # lkid is the index into the fine grid's saved fluxes.
c  # the fine grid will save all its fluxes all around its
c  # perimeter. lkid tells where the coarse grid should
c  # taking them from.
c
c :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::;
c
      implicit double precision (a-h,o-z)
      dimension listbc(5,maxsp)

      ibc = ispot
      ist  = iclo - 1
      iend = ichi + 1
      jst  = jclo - 1
      jend = jchi + 1
c
c  left side
c
       if (ist .lt. ilo) go to 20
       lkid     = max(jlo,jclo) - jclo + 1
       do 10 j  = max(jlo,jclo), min(jhi,jchi)
          ispot              = ispot + 1
          listbc(1,ispot)    = ist-ilo+nghost+1
          listbc(2,ispot)    = j-jlo+nghost+1
          listbc(3,ispot)    = 3
          listbc(4,ispot)    = mkid
          listbc(5,ispot)    = lkid
          lkid               = lkid + 1
 10    continue
c
c   top side
c
 20    if (jend .gt. jhi) go to 40
       lkid       = (jchi-jclo+1) + max(ilo,iclo)-iclo + 1
       do 30 i    = max(ilo,iclo), min(ihi,ichi)
          ispot              = ispot + 1
          listbc(1,ispot)    = i-ilo+nghost+1
          listbc(2,ispot)    = jend-jlo+nghost+1
          listbc(3,ispot)    = 4
          listbc(4,ispot)    = mkid
          listbc(5,ispot)    = lkid
          lkid               = lkid + 1
 30    continue
c
c  right side (numbered from bottom to top, so not continuous)
c
 40    if (iend .gt. ihi) go to 60
       lkid     = (ichi-iclo+1)+(jchi-jclo+1)
     .               + max(jlo,jclo) - jclo + 1
       do 50 j  = max(jlo,jclo), min(jhi,jchi)
          ispot              = ispot + 1
          listbc(1,ispot)    = iend-ilo+nghost+1
          listbc(2,ispot)    = j-jlo+nghost+1
          listbc(3,ispot)    = 1
          listbc(4,ispot)    = mkid
          listbc(5,ispot)    = lkid
          lkid   = lkid + 1
 50    continue
c
c  bottom side (numbered left to right, so not continuous)
c
 60    if (jst .lt. jlo) go to 80
       lkid   =  2*(jchi-jclo+1) + (ichi-iclo+1) + max(ilo,iclo)-iclo+1
       do 70 i          = max(ilo,iclo), min(ihi,ichi)
          ispot              = ispot + 1
          listbc(1,ispot)    = i-ilo+nghost+1
          listbc(2,ispot)    = jst-jlo+nghost+1
          listbc(3,ispot)    = 2
          listbc(4,ispot)    = mkid
          listbc(5,ispot)    = lkid
          lkid   = lkid + 1
 70    continue
c
 80    continue
       return
       end
c
c --------------------------------------------------------------
c
      subroutine stst1
c
      implicit double precision (a-h,o-z)

      include  "call.i"

c
c :::::::::::::::::::::::::::::: STST1 ::::::::::::::::::::::::::::::::
c    intialize a few variables needed before calling user set up
c    routine domain.
c    the spatial and temporal stepsizes are set. the node array
c    is kept as a linked list of free nodes.  "ndfree" points to the
c    head of the list, i.e. first free node.  use first row of each
c    col to hold this pointer, set by the macro "nextfree".
c    the free space list, managed in lfree, will have first and
c    last positions filled with an allocation of zero words,
c    to avoid boundary cases.
c ::::::::::::::::::::::::::::::::::::::;::::::::::::::::::::::::::::::
c
      ndfree = 1
      do 10 i   = 1, maxgr
         node(nextfree,i) = i+1
 10   continue
c
c the last free node will have a null pointer
 
      node(nextfree, maxgr) = null
c
      lfine = 1
      do 20 i  = 1, memsize
        alloc(i) = 0.0d0
 20   continue
c
c  initialize linked list of alloc storage as well.
c  first and last locations are dummy placeholders of zero words
c  of allocation each, to avoid boundary cases.
c
      do  40 i  = 1, lfdim
        lfree(i,1) = 0
        lfree(i,2) = 0
 40   continue
      lfree(3,1) =memsize + 2
      lfree(2,1) = 1
      lfree(2,2) =memsize
      lenf       = 3
c
c after kcheck integrations of parent grid, move its refinements.
c finest level grid never needs to have its finer subgrids moved.
c
      do 60 i   = 1, maxlv
         lstart(i) = 0
 60      icheck(i) = 0
c
c finish initializing spatial and counting arrays
c
      level      = 2
      rr         = float(intrat(1))
 70   if (level .gt. mxnest) go to 80
          hxposs(level) = hxposs(level-1) / rr
          hyposs(level) = hyposs(level-1) / rr
          rr            =  intrat(level)
          level         = level + 1
      go to 70
 80   continue

      return
      end
c
c  -------------------------------------------------------------
c
      subroutine tick(nvar,iout,nstart,nstop,cut,vtime,time,ichkpt,
     &                naux,nout,tout)
c
      implicit double precision (a-h,o-z)

      include  "call.i"

      logical    vtime, dumpout
      dimension dtnew(maxlv), ntogo(maxlv), tlevel(maxlv)
      dimension tout(nout)

c
c :::::::::::::::::::::::::::: TICK :::::::::::::::::::::::::::::
c  main driver routine.  controls:
c        integration  of all grids.
c        error estimation / regridding
c        output counting
c        updating of fine to coarse grids

c  parameters:
c     nstop   = # of coarse grid time steps to be taken
c     iout    = output interval every 'iout' coarse time steps
c               (if 0, not used - set to inf.)
c     vtime   = true for variable timestep, calculated each coarse step
c
c  integration strategy is to advance a fine grid until it catches
c  up to the coarse grid. this strategy is applied recursively.
c  coarse grid goes first.
c
c  nsteps: used to count how number steps left for a level to be
c          integrated before it catches up with the next coarser level.
c  ncycle: counts number of coarse grid steps = # cycles.
c
c  icheck: counts the number of steps (incrementing by 1
c          each step) to keep track of when that level should 
c          have its error estimated and finer levels should be regridded.
c ::::::::::::::::::::::::::::::::::::;::::::::::::::::::::::::::
c

      ncycle         = nstart
      if ( iout .eq. 0) iout  = iinfinity
      if (ichkpt .eq. 0) ichkpt = iinfinity
      if (nout .gt. 0) then
         tfinal = tout(nout)
c        if this is a restart, make sure output times start after restart time
         if (nstart .gt. 0) then
            do ii = 1, nout
              if (tout(ii) .gt. time) then
                nextout = ii
                go to 2
              endif
            end do
         else
            nextout   = 1
         endif
      else
         tfinal  = rinfinity
         nextout = 1     ! keeps it out of the way
      endif
 2    tlevel(1)      = time
      do 5 i       = 2, mxnest
       tlevel(i) = tlevel(1)
 5     continue
c
c  ------ start of coarse grid integration loop. ------------------
c
 20   if (ncycle .ge. nstop .or. time .ge. tfinal) goto 999

      if (nextout  .le. nout) then
         outtime       = tout(nextout)
      else
         outtime       = rinfinity
      endif

      if (time + possk(1) .ge. outtime) then
c        ## adjust time step  to hit outtime exactly, and make output
         possk(1) = outtime - time
         do 12 i = 2, mxnest
 12         possk(i) = possk(i-1) / intrat(i-1)
         nextout = nextout + 1
        dumpout = .true.
      else
        dumpout = .false.
      endif
c  take output stuff from here - put it back at end.
c
          level        = 1
          ntogo(level) = 1
          do 10 i = 1, maxlv
 10          dtnew(i)  = rinfinity
c
c     ------------- regridding  time?  ---------
c
c check if either
c   (i)  this level should have its error estimated before being advanced
c   (ii) this level needs to provide boundary values for either of
c        next 2 finer levels to have their error estimated.
c        this only affects two grid levels higher, occurs because
c        previous time step needs boundary vals for giant step.
c  no error estimation on finest possible grid level
c
 60       continue
          if (icheck(level) .ge. kcheck) then
               lbase = level
          else if (level+1 .ge. mxnest) then
               go to 90
          else if (icheck(level+1) .ge. kcheck) then
               lbase = level+1
          else if (level+2 .ge. mxnest) then
               go to 90
          else if (icheck(level+2) .ge. kcheck) then
               lbase = level+2
          else
               go to 90
          endif
          if (lbase .eq. mxnest .or. lbase .gt. lfine) go to 70
c
c regrid level 'lbase+1' up to finest level.
c level 'lbase' stays fixed.
c
          if (rprint) write(outunit,101) lbase
101       format(8h  level ,i5,32h  stays fixed during regridding )
          call regrid(nvar,lbase,cut,naux)
c         call conck(1,nvar)
c         call outtre(lstart(lbase+1),.true.,nvar,naux)
c         call valout(lbase,lfine,tlevel(lbase),nvar,naux)
c
c  maybe finest level in existence has changed. reset counters.
c
          if (rprint .and. lbase .lt. lfine) then
             call outtre(lstart(lbase+1),.false.,nvar,naux)
          endif
 70       continue
          do 80  i  = lbase, lfine
 80          icheck(i) = 0
          do 81  i  = lbase+1, lfine
 81          tlevel(i) = tlevel(lbase)
c
c  ------- done regridding --------------------
c
c integrate all grids at level 'level'.
c
 90       continue
          if (tprint) write(outunit,100)level,tlevel(level),possk(level)
100       format(' integrating grids at level ',i3,' from t =',
     &           e11.5,  '  using dt = ',e11.5)

          if (method(4).ge.level) then
              write(6,100)level,tlevel(level),possk(level)
              endif

          call advanc(level,nvar,dtlevnew,vtime,naux)

c        # to debug individual grid updates...
c        call valout(1,lfine,time,nvar,naux)
c
c done with a level of integration. update counts, decide who next.
c
          ntogo(level)  = ntogo(level) - 1
          dtnew(level)  = dmin1(dtnew(level),dtlevnew)
          tlevel(level) = tlevel(level) + possk(level)
          icheck(level) = icheck(level) + 1
c
          if (level .lt. lfine) then
             level = level + 1
c            #  check if should adjust finer grid time step to start wtih
             if (((possk(level-1) - dtnew(level-1))/dtnew(level-1)) .gt.
     .            .05) then
                dttemp = dtnew(level-1)/intrat(level-1)
                ntogo(level) = (tlevel(level-1)-tlevel(level))/dttemp+.9
              else
                ntogo(level) = intrat(level-1)
              endif
             possk(level) = possk(level-1)/ntogo(level)
             go to 60
          endif
c
 105      if (level .eq. 1) go to 110
              if (ntogo(level) .gt. 0) then
c                same level goes again. check for ok time step
 106             if ((possk(level)-dtnew(level))/dtnew(level)
     .                .gt. .05)  then
c                   adjust time steps for this and finer levels
                    ntogo(level) = ntogo(level) + 1
                    possk(level) = (tlevel(level-1)-tlevel(level))/
     .                             ntogo(level)
                    go to 106
                 endif
                 go to 60
              else
                 level = level - 1
                 call update(level,nvar)
              endif
          go to 105
c
c  --------------one complete coarse grid integration cycle done. -----
c
c      time for output?  done with the whole thing?
c
 110      continue
          time    = time   + possk(1)
          ncycle  = ncycle + 1
          call conck(1,nvar)

       if ((ncycle/ichkpt)*ichkpt.eq.ncycle) then
          call check(ncycle,time,nvar,naux)
       endif

       if ((ncycle/iout)*iout .eq. ncycle .or. dumpout) then
         call valout(1,lfine,time,nvar,naux)
         if (printout) call outtre(mstart,.true.,nvar,naux)
       endif

      if ( vtime) then
c
c         find new dt for next cycle (passed back from integration routine).
         do 115 i = 2, lfine
           ii = lfine+1-i
           dtnew(ii) = min(dtnew(ii),dtnew(ii+1)*intrat(ii))
 115     continue
         possk(1) = dtnew(1)
         do 120 i = 2, mxnest
 120       possk(i) = possk(i-1) / intrat(i-1)

      endif

      go to 20
c
999   continue

c
c  # computation is complete to final time or requested number of steps
c
       if (ncycle .ge. nstop .and. tfinal .lt. rinfinity) then
c         # warn the user that calculation finished prematurely
          write(outunit,102) nstop
          write(6,102) nstop
  102     format('*** Computation halted after nv(1) = ',i8,
     &           '  steps on coarse grid')
          endif
c
c  # final output (unless we just did it above)
c 
      if (.not. dumpout) then
         if ((ncycle/iout)*iout .ne. ncycle) then
           call valout(1,lfine,time,nvar,naux)
           if (printout) call outtre(mstart,.true.,nvar,naux)
         endif
      endif

c  # checkpoint everything for possible future restart 
c  # (unless we just did it based on ichkpt)
c
c  # don't checkpoint at all if user set ichkpt=0

      if (ichkpt .lt. iinfinity) then
         if ((ncycle/ichkpt)*ichkpt .ne. ncycle) then
           call check(ncycle,time,nvar,naux)
         endif
       endif

      return
      end
c
c ---------------------------------------------------------------
c
        subroutine trimbd(used,nrow,ncol,set,il,ir,jb,jt)
c
c :::::::::::::::::::::::: TRIMBD :::::::::::::::::::::::::::;
c  if used array is completely set (=1.) then return set=true, 
c  otherwise return false, alogn with the dimensions of the smallest 
c  rectangle containing all unset points in il,ir,jb,jt.
c ::::::::::::::::::::::::::::::::::::;::::::::::::::::::::::;
c
        implicit double precision (a-h,o-z)
        dimension  used(nrow,ncol)
        logical   set

        utot = 0.
        do 100 j = 1,ncol
        do 100 i = 1,nrow
100        utot = utot + used(i,j)
        if (utot .ge. float(nrow*ncol)) then
                set = .true.
                return
        endif
 
        set = .false.
 
        uleft = 1.
        do 200 i = 1,nrow
           do 220 j = 1,ncol
              uleft = dmin1(uleft,used(i,j))
220        continue
           il = i
           if (uleft .eq. 0.) go to 230
200     continue

230     uright = 1.
        do 300 i = 1,nrow
           do 320 j = 1,ncol
              uright = dmin1(uright,used(nrow - i + 1,j))
320        continue
           ir = nrow - i + 1
           if (uright .eq. 0.) go to 330
300     continue

330     ubot = 1.
        do 400 j = 1,ncol
           do 420 i = 1,nrow
              ubot = dmin1(ubot,used(i,j))
420        continue
           jb = j
           if (ubot .eq. 0.) go to 430
400        continue
 
430     utop = 1.
        do 500 j = 1,ncol
           do 520 i = 1,nrow
              utop = dmin1(utop,used(i,ncol - j + 1))
520        continue
           jt = ncol - j + 1
           if (utop .eq. 0.) go to 530
500     continue
 
530     return
        end
c
c -----------------------------------------------------------
c
      subroutine update (level, nvar)
c
      implicit double precision (a-h,o-z)

      include  "call.i"

      iadd(i,j,ivar)  = loc     + i - 1 + mitot*((ivar-1)*mjtot+j-1)
      iaddf(i,j,ivar) = locf    + i - 1 + mi*((ivar-1)*mj  +j-1)
      iaddfaux(i,j)   = locfaux + i - 1 + mi*((mcapa-1)*mj + (j-1))
      iaddcaux(i,j)   = loccaux + i - 1 + mitot*((mcapa-1)*mjtot+(j-1))
c
c :::::::::::::::::::::::::: UPDATE :::::::::::::::::::::::::::::::::
c update - update all grids at level 'level'.
c          this routine assumes cell centered variables.
c          the update is done from 1 level finer meshes under it.
c input parameter:
c    level  - ptr to the only level to be updated. levels coarser than
c             this will be at a diffeent time.
c :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c
      lget = level
      if (uprint) write(outunit,100) lget
100   format(19h    updating level ,i5)
c
c  grid loop for each level
c
      dt     = possk(lget)

      mptr = lstart(lget)
 20   if (mptr .eq. 0) go to 85
         loc     = node(store1,mptr)
         loccaux = node(storeaux,mptr)
         nx      = node(ndihi,mptr) - node(ndilo,mptr) + 1
         ny      = node(ndjhi,mptr) - node(ndjlo,mptr) + 1
         mitot   = nx + 2*nghost
         mjtot   = ny + 2*nghost
         ilo     = node(ndilo,mptr)
         jlo     = node(ndjlo,mptr)
         ihi     = node(ndihi,mptr)
         jhi     = node(ndjhi,mptr)
c
         if (node(cfluxptr,mptr) .eq. 0) go to 25
         locuse = igetsp(mitot*mjtot)
         call upbnd(alloc(node(cfluxptr,mptr)),alloc(loc),nvar,
     1              mitot,mjtot,listsp(lget),alloc(locuse),mptr)
         call reclam(locuse,mitot*mjtot)
c
c  loop through all intersecting fine grids as source updaters.
c
 25      mkid = lstart(lget+1)
 30        if (mkid .eq. 0) go to 80
           iclo   = node(ndilo,mkid)/intrat(lget)
           jclo   = node(ndjlo,mkid)/intrat(lget)
           ichi   = node(ndihi,mkid)/intrat(lget)
           jchi   = node(ndjhi,mkid)/intrat(lget)

           mi      = node(ndihi,mkid)-node(ndilo,mkid) + 1 + 2*nghost
           mj      = node(ndjhi,mkid)-node(ndjlo,mkid) + 1 + 2*nghost
           locf    = node(store1,mkid)
           locfaux = node(storeaux,mkid)
c
c  calculate starting and ending indices for coarse grid update, if overlap
c
         iplo = max(ilo,iclo)
         jplo = max(jlo,jclo)
         iphi = min(ihi,ichi)
         jphi = min(jhi,jchi)

         if (iplo .gt. iphi .or. jplo .gt. jphi) go to 75
c
c  calculate starting index for fine grid source pts.
c
         iff    = iplo*intrat(lget) - node(ndilo,mkid) + nghost + 1
         jff    = jplo*intrat(lget) - node(ndjlo,mkid) + nghost + 1
         totrat = intrat(lget) * intrat(lget)
 
         do 71 i = iplo-ilo+nghost+1, iphi-ilo+nghost+1
         do 70 j = jplo-jlo+nghost+1, jphi-jlo+nghost+1
           if (uprint) then
              write(outunit,101) i,j,mptr,iff,jff,mkid
 101          format(' updating pt. ',2i4,' of grid ',i3,' using ',2i4,
     1               ' of grid ',i4)
              write(outunit,102)(alloc(iadd(i,j,ivar)),ivar=1,nvar)
 102          format(' old vals: ',4e12.4)
           endif
c
c
c  update using intrat fine points in each direction
c
           do 35 ivar = 1, nvar
 35           alloc(iadd(i,j,ivar)) = 0.d0
c
           if (mcapa .eq. 0) then
               do 50 jco  = 1, intrat(lget)
               do 50 ico  = 1, intrat(lget)
               do 40 ivar = 1, nvar
                 alloc(iadd(i,j,ivar))= alloc(iadd(i,j,ivar)) + 
     1                        alloc(iaddf(iff+ico-1,jff+jco-1,ivar))
 40              continue
 50            continue
            do 60 ivar = 1, nvar
 60          alloc(iadd(i,j,ivar)) = alloc(iadd(i,j,ivar))/totrat
               
           else

               do 51 jco  = 1, intrat(lget)
               do 51 ico  = 1, intrat(lget)
               capa = alloc(iaddfaux(iff+ico-1,jff+jco-1))
               do 41 ivar = 1, nvar
                 alloc(iadd(i,j,ivar))= alloc(iadd(i,j,ivar)) + 
     1                       alloc(iaddf(iff+ico-1,jff+jco-1,ivar))*capa
 41              continue
 51            continue
            do 61 ivar = 1, nvar
 61          alloc(iadd(i,j,ivar)) = alloc(iadd(i,j,ivar))/
     1                               (totrat*alloc(iaddcaux(i,j)))
           endif
c
            if (uprint) write(outunit,103)(alloc(iadd(i,j,ivar)),
     .                                     ivar=1,nvar)
 103        format(' new vals: ',4e12.4)
c
           jff = jff + intrat(lget)
 70        continue
           iff = iff + intrat(lget)
           jff    = jplo*intrat(lget) - node(ndjlo,mkid) + nghost + 1
 71        continue
c
 75         mkid = node(levelptr,mkid)
            go to 30
c
 80         mptr = node(levelptr, mptr)
            go to 20
c
 85       continue
c
 99   return
      end
c
c ------------------------------------------------------------
c
      integer function nodget(dummy)
c
      implicit double precision (a-h,o-z)

      include  "call.i"
c
c ::::::::::::::::: NODGET ::::::::::::::::::::::::::::::::::::;
c nodget =  get first free node of the linked list kept in node
c            array. adjust pointers accordingly.
c :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::;
c
      if (ndfree .ne. null) go to 10
          write(outunit,100) maxgr
          write(*,100)       maxgr
100       format(' out of nodal space - allowed ',i5,' grids')
          stop
c
 10     nodget         = ndfree
        ndfree         = node(nextfree,ndfree)
c
c  initialize nodal block
c
        do 20 i        = 1, nsize
           node(i,nodget) = 0
 20     continue
c
        do 30 i         = 1, rsize
           rnode(i,nodget) = 0.0d0
 30     continue
c
      return
      end
c
c ------------------------------------------------------------
c
       subroutine upbnd(listbc,val,nvar,mitot,mjtot,
     1                  maxsp,iused,mptr)
 
      implicit double precision (a-h,o-z)

      include  "call.i"
 
       dimension val(mitot,mjtot,nvar),listbc(5,maxsp),
     1           iused(mitot,mjtot)

       iaddaux(i,j) = locaux + i-1 +  mitot*(j-1) 
     1                 + mitot*mjtot*(mcapa-1)
 
c
c :::::::::::::::::::::::::::: UPBND :::::::::::::::::::::::::::::
c We now correct the coarse grid with the flux differences stored
c with each of the fine grids. We use an array   iused
c to indicate whether the flux has been updated or not for that zone.
c iused(i,j) = sum from (l=1,4) i(l)*2**(l-1), where i(l) = 1 if the
c flux for the  l-th side of the (i,j)-th cell has already been
c updated, and i(l) = 0 if not.
 
c if there is a capacity fn. it needs to be included in update formula
c indicated by mcapa not zero (is index of capacity fn.)
c ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c
 
      do 10 j=1,mjtot
      do 10 i=1,mitot
         iused(i,j) = 0.
 10   continue
 
      locaux = node(storeaux,mptr)
      levc   = node(nestlevel,mptr)
      area   = hxposs(levc)*hyposs(levc)
      irc    = intrat(levc)
      if (uprint) write(outunit,*)" upbnding grid ",mptr

      do 40 ispot = 1,maxsp
         icrse = listbc(1,ispot)
         if (icrse.eq.0) go to 99
 
         jcrse = listbc(2,ispot)
         iside = listbc(3,ispot)
         norm = 2**(iside-1)
         iflag =iused(icrse,jcrse)/norm
         if (mod(iflag,2).eq.1) then
            go to 40
         endif
         mkid = listbc(4,ispot)
         nx    =  node(ndihi,mkid) - node(ndilo,mkid) + 1
         ny    =  node(ndjhi,mkid) - node(ndjlo,mkid) + 1
         lenbc = 2*(nx+ny)/irc
         kidlst = node(ffluxptr,mkid)
         lkid = listbc(5,ispot)
         if (mod(iside,4).gt.1) then
c           (iside .eq. 2 .or. iside .eq. 3)
            sgnm = -1.
         else
c           (iside .eq. 4 .or. iside .eq. 1)
            sgnm = 1.
         endif

c        ## debugging output
         if (uprint) then
           write(outunit,101) icrse,jcrse,
     .         (val(icrse,jcrse,ivar),ivar=1,nvar)
 101       format(" old ",1x,2i4,4e15.7)
         endif

         if (mcapa .gt. 0) then
c            # capacity array:  need to divide by capa in each cell.
c            # modify sgnm which is reset for each grid cell.
c            # Note capa is stored in aux(icrse,jcrse,mcapa)
             sgnm = sgnm / alloc(iaddaux(icrse,jcrse))
         endif

         do 20 ivar = 1,nvar
            val(icrse,jcrse,ivar) = val(icrse,jcrse,ivar) +
     1      sgnm*alloc(kidlst+nvar*(lkid-1)+ivar-1)/area
 20      continue
         iused(icrse,jcrse) = iused(icrse,jcrse) + norm

c        ## debugging output
         if (uprint) then
           write(outunit,102) mkid,
     .         (val(icrse,jcrse,ivar),ivar=1,nvar)
 102       format(" new ","(grid",i3,")",4e15.7)
         endif

 40   continue
c
 99   return
      end
c
c ------------------------------------------------------
c
      subroutine basic (time,lst,lend)
c
      implicit double precision (a-h,o-z)

      include  "call.i"
c
c :::::::::::::::::::::: BASIC :::::::::::::::::::::::::
c  basic = outputs basic information needed by the other graphics
c          output routines (valout,drawrg). at the given time,
c          write the entire levellist, from level 1 to lfine,
c          and the tree structure from level lst to lend.
c :::::::::::::::::::::::::::::;::::::::::::::::::::::::
c
      write(pltunit1,100) time
100   format(8h*TIME = ,f10.5)
      write(pltunit1,101) lfine, (lstart(i),i=1,lfine), nghost
101   format(10i6)
      write(pltunit1,105) xupper,yupper,xlower,ylower
105   format(4e15.8)
      write(pltunit1,102) lst, lend
102   format(2i6)
      write(pltunit1,106)(hxposs(i),i=1,lfine)
      write(pltunit1,106)(hyposs(i),i=1,lfine)
106   format(6e13.8)
c
      level = lst
 10   if (level .gt. lend) go to 99
          mptr = lstart(level)
 20       if (mptr .eq. 0) go to 30
              write(pltunit1,103) mptr, (node(i,mptr),i=1,nsize)
              write(pltunit1,104) (rnode(i,mptr),i=1,rsize)
103           format(10i7)
104           format(5e14.8)
              mptr = node(levelptr,mptr)
          go to 20
 30       level = level + 1
      go to 10
c
 99   return
      end
c
c ------------------------------------------------
c
      subroutine drawrg(time,lpar,mkid1,nclust,numptc,nxypts,
     *                  badpts)
c
      implicit double precision (a-h,o-z)

      include  "call.i"

      dimension badpts(2,nxypts), numptc(nclust)

c :::::::::::::::::::::: DRAWRG ::::::::::::::::::::::::::::::;
c
c drawrg: output existing grids at level lpar
c                flagged points at level lpar
c                clusters
c                new and old subgrids - start with grid mkid1 (not a
c                     levelptr.)
c  make sure nclust <> 0 at this point. mkid1 always exists if
c  there is at least 1 flagged pt., but there might not be any
c  old fine grids.
c ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::;
c
      if (nclust .le. 0) return
c
      call basic (time, lpar, lpar+1)
c
      write(pltunit1,100) nclust, lpar, nxypts
100   format(10h*CLUSTERS ,3i10)
      write(pltunit1,105) iregsz, jregsz
105   format(6i10)
      write(pltunit1,101) (numptc(i),i=1, nclust)
101   format(10i6)
      write(pltunit1,102) ((badpts(i,j),i=1,2),j=1,nxypts)
102   format(4e12.4)
c
c output subgrid info. - since nclust > 0, we know there is at least
c  one. then we can chain through levelptr to get rest.
c
      mkid = mkid1
 10   if (mkid .eq. 0) go to 20
          write(pltunit1,103) mkid, (node(i,mkid),i=1,nsize)
103       format(10i6)
          write(pltunit1,104) (rnode(i,mkid), i = 1,rsize)
104       format(5e12.4)
          mkid = node(levelptr, mkid)
      go to 10
 20   continue
c
      return
      end
c
c =======================================================================
      subroutine outval(val,nvar,mitot,mjtot,mptr,outgrd,naux,aux)
c =======================================================================
c
      implicit double precision (a-h,o-z)

      dimension  val(mitot,mjtot,nvar)
      dimension  aux(mitot,mjtot,naux)
      logical    outgrd

      include  "call.i"

c ::::::::::::::::::::::OUTVAL :::::::::::::::::::::::::::::::
c print solution and aux. variables to output. 
c if cell outside domain, don't print soln. value - nothing
c currently in ghost cells.
c ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c
      if (.not. outgrd) go to 99
      level = node(nestlevel,mptr)
      hx    = hxposs(level)
      hy    = hyposs(level)
      cornx = rnode(cornxlo,mptr) -  nghost*hx
      corny = rnode(cornylo,mptr) -  nghost*hy
c
      do 25 j=nghost+1,mjtot-nghost
      do 20 i=nghost+1,mitot-nghost

          x  = cornx + hx*(float(i)-.5d0)
          y  = corny + hy*(float(j)-.5d0)
          write(outunit,107) x,y,i,j,(val(i,j,ivar),ivar=1,nvar)
 107      format(2hx=,f6.3,3h y=,f5.3,3h,i=,i2,3h,j=,i2,' a=',
     *           5(e9.3,1x))
          if (naux.gt.0) write(outunit,108) (aux(i,j,iaux),iaux=1,naux)
 108      format(1x,'aux = ',7(e9.3,1x))

 20   continue
 25   continue

 99   return
      end
c
c -----------------------------------------------------
c
      subroutine valout (lst, lend, time, nvar, naux)
c
      implicit double precision (a-h,o-z)
      character*10  matname1, matname2

      include  "call.i"

      iadd(i,j,ivar) = loc + i - 1 + mitot*((ivar-1)*mjtot+j-1)
c
c ::::::::::::::::::::::::::: VALOUT ::::::::::::::::::::::::::::::::::;
c valout = graphics output of soln values for contour or surface plots.
c          can output for matlab or ncar graphics post-processing
c :::::::::::::::::::::::::::::::::::::;:::::::::::::::::::::::::::::::;
c

c     ### NCAR graphics output

      if (ncarout) then

        call basic (time, lst, lend )
c
        write(pltunit1,100)  nvar
100     format(10h*VALS     ,i10)
c
        level = lst
10      if (level .gt. lend) go to 60
            mptr = lstart(level)
20          if (mptr .eq. 0) go to 50
                nx = node(ndihi,mptr)-node(ndilo,mptr) + 1
                ny = node(ndjhi,mptr)-node(ndjlo,mptr) + 1
                mitot = nx + 2*nghost
                mjtot = ny + 2*nghost
                loc = node(store1,mptr)
                call outvar(alloc(loc),mitot,mjtot,nvar,mptr,nghost)
                mptr = node(levelptr,mptr)
            go to 20
50          continue
            level = level + 1
        go to 10
c
60    endif


c     ### MATLAB graphics output
c
c

      if (matlabout) then
c        ###  make the file names and open output files
         matname1 = 'fort.qxxxx'
         matname2 = 'fort.txxxx'
         matunit1 = 50
         matunit2 = 60
         nstp     = matlabu 
         do 55 ipos = 10, 7, -1
            idigit = mod(nstp,10)
            matname1(ipos:ipos) = char(ichar('0') + idigit)
            matname2(ipos:ipos) = char(ichar('0') + idigit)
            nstp = nstp / 10
 55      continue
         open(unit=matunit1,file=matname1,status='unknown',
     .       form='formatted')

         level = lst
         ngrids = 0
 65      if (level .gt. lfine) go to 90
            mptr = lstart(level)
 70         if (mptr .eq. 0) go to 80
              ngrids  = ngrids + 1
              nx      = node(ndihi,mptr) - node(ndilo,mptr) + 1
              ny      = node(ndjhi,mptr) - node(ndjlo,mptr) + 1
              loc     = node(store1, mptr)
              locaux  = node(storeaux,mptr)
              mitot   = nx + 2*nghost
              mjtot   = ny + 2*nghost
              write(matunit1,1001) mptr, level, nx, ny
 1001 format(i5,'                 grid_number',/,
     &       i5,'                 AMR_level',/,
     &       i5,'                 mx',/,
     &       i5,'                 my')


c  old        xcorn = rnode(cornxlo,mptr) - .5d0*hxposs(level)
c  old        ycorn = rnode(cornylo,mptr) - .5d0*hyposs(level)
              xlow = rnode(cornxlo,mptr)
              ylow = rnode(cornylo,mptr)
              write(matunit1,1002) 
     &              xlow,ylow,hxposs(level),hyposs(level)
 1002 format(e18.8,'    xlow', /,
     &       e18.8,'    ylow', /,
     &       e18.8,'    dx', /,
     &       e18.8,'    dy',/)

              do 75 j = nghost+1, mjtot-nghost
              do 75 i = nghost+1, mitot-nghost
                do ivar=1,nvar
                   if (dabs(alloc(iadd(i,j,ivar))) .lt. 1d-90) then 
                      alloc(iadd(i,j,ivar)) = 0.d0
                      endif
                 enddo
                write(matunit1,109) (alloc(iadd(i,j,ivar)), ivar=1,nvar)
 109          format(4e26.16)
 75           continue


            mptr = node(levelptr, mptr)
            go to 70
 80      level = level + 1
         go to 65

 90     continue

        open(unit=matunit2,file=matname2,status='unknown',
     .       form='formatted')
      write(matunit2,1000) time,nvar,ngrids
 1000 format(e18.8,'    time', /,
     &       i5,'                 meqn'/,
     &       i5,'                 ngrids'/,/)
c

      write(6,601) matlabu,time
  601 format('AMRCLAW: Frame ',i4,
     &       ' matlab plot files done at time t = ', d12.5,/)

      matlabu = matlabu + 1

      close(unit=matunit1)
      close(unit=matunit2)
      endif  

      return
      end
c
c ----------------------------------------------------------
c
      subroutine copysol(valbig,val,nvar,mitot,mjtot,nghost,
     1                   midub,mjdub,ngbig)
c
      implicit double precision (a-h,o-z)

      dimension  valbig(midub,mjdub,nvar), val(mitot,mjtot,nvar)
c
c copy solution into grid with different number ghsot cells
c
       do 10 ivar = 1, nvar
       do 10 j = nghost+1, mjtot-nghost
       do 10 i = nghost+1, mitot-nghost
          valbig(i-nghost+ngbig,j-nghost+ngbig,ivar) = val(i,j,ivar)
 10    continue
c
       return
       end
c
c -------------------------------------------------------------
c
      subroutine outvar(rect,mitot,mjtot,nvar,mptr,ng)
c
      implicit double precision (a-h,o-z)

      include  "call.i"

      dimension rect(mitot,mjtot,nvar)

c ::::::::::::::: OUTVAR ::::::::::::::::::::::::::::::::::
c
c  dump soln for graphics 
c
c  only output max - 1 rows and cols, since with cell centered
c  variables there is one extra cell outside the grid.
c
c :::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c
      write(pltunit1,100) mptr
 100  format('*SOLN     ',i10,' is the grid - all variables')
c
      do 20 ivar = 1, nvar
         write(pltunit1,101) ((rect(i,j,ivar),i=ng+1,mitot-ng),
     .                                 j=ng+1,mjtot-ng)
 101     format(5e12.6)
 20   continue
c
      return
      end
c
c ---------------------------------------------------------
c
      subroutine outmsh(mptr,outgrd,nvar,naux)
      implicit double precision (a-h,o-z)
      logical  outgrd

      include  "call.i"

c
c ::::::::::::::::::::: OUTMSH :::::::::::::::::::::::::::::::::::::
c
c outmsh - output the grid descriptor and optionally the values on
c          the grid (for a single grid - see "outtre" for outputing
c          a subtree)
c input parameters:
c    mptr   - ptr to grid descriptor
c    outgrd - if true, output value on grid
c special case
c     if grid has level < 1, nothing is printed. (forest pointer
c has level = 0).  this simplifies logic of outtre; also, any grid
c with level <= 0 is not properly set (the forest pointer
c is used only to provide pointers into the tree of coarsest grids)
c
c :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 
100   format(1x,47h+----------------------------------------------,
     *30h-----------------------------+)
c
      lev = node(nestlevel,mptr)
c
      write(outunit,100)
      write(outunit,101) mptr
101   format(1x,10h! grid no:,i4,62x,1h!)
      write(outunit,102) node(nestlevel,mptr),rnode(timemult,mptr),
     .             node(levelptr,mptr)
102   format(1x,1h!,11h nestlevel=,i3,12h, time mult=,f8.5,
     1       13h, level ptr =,i4,24x,1h!)
      write(outunit,103) node(store1,mptr),node(store2,mptr),
     1                   node(cfluxptr,mptr),node(ffluxptr,mptr)
 103  format(1x,'! storage locs =',2i8,'  bndry locs =',2i8,14x,1h!)
      write(outunit,104)
      write(outunit,111) rnode(cornxlo,mptr),rnode(cornyhi,mptr),
     1             rnode(cornxhi,mptr),rnode(cornyhi,mptr)
      write(outunit,111) rnode(cornxlo,mptr),rnode(cornylo,mptr),
     1             rnode(cornxhi,mptr),rnode(cornylo,mptr)
      write(outunit,112)
      write(outunit,113) node(ndilo,mptr),node(ndjhi,mptr),
     1                   node(ndihi,mptr),node(ndjhi,mptr)
      write(outunit,113) node(ndilo,mptr),node(ndjlo,mptr),
     1                   node(ndihi,mptr),node(ndjlo,mptr)
112   format(1x,23h! integer index space :,53x,1h!)
113   format(1x,2h! ,18x,2(1h(,i8,2h, ,i8,1h)),16x,1h!)
104   format(1x,23h! corners of rectangle:,53x,1h!)
111   format(1x,2h! ,18x,2(1h(,f8.5,2h, ,f8.5,1h)),16x,1h!)
      write(outunit,105) hxposs(lev),hyposs(lev),possk(lev)
105   format(1x,7h! hrow=,f9.6,7h, hcol=,f9.6,8h, ktime=,f9.6,27x,1h!)
      write(outunit,100)
c
      if (.not. outgrd) go to 99
c output the grid
      mitot   = node(ndihi,mptr) - node(ndilo,mptr) + 1 + 2*nghost
      mjtot   = node(ndjhi,mptr) - node(ndjlo,mptr) + 1 + 2*nghost
      loc     = node(store1,mptr)
      locaux  = node(storeaux,mptr)
      call outval(alloc(loc),nvar,mitot,mjtot,mptr,outgrd,
     1            naux,alloc(locaux))
 99   return
      end
c
c --------------------------------------------------------------
c
      subroutine outtre(mlev,outgrd,nvar,naux)
c
      implicit double precision (a-h,o-z)
      logical  outgrd

      include  "call.i"
c
c ::::::::::::::::::::::: OUTTRE :::::::::::::::::::::::::::::::::::
c
c outtre - output subtree
c input parameters:
c    mlev   - ptr to subtree to output i.e., start at level(mlev)
c    outgrd - if true, output the values on the grid
c tree is output from 'level' to finest level.
c
c :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c
      write (outunit,1)
1     format(1x,14hthe subtree is)
c
      level = node(nestlevel, mlev)
10    if (level .gt. lfine) go to 99
          mptr    = lstart(level)
 20       if (mptr .eq. 0) go to 30
              call outmsh(mptr,outgrd,nvar,naux)
              mptr = node(levelptr, mptr)
          go to 20
 30       continue
          level = level + 1
      go to 10
c
 99   return
      end
c
c  ----------------------------------------------------------
c
      subroutine domain (nvar,vtime,nx,ny,naux)
c
      implicit double precision (a-h,o-z)
      logical    vtime

      include  "call.i"
c
c  allocate initial coarse grid domain. set node info & initialize grid
c  initial space and time step set here too
c
      mstart = nodget(dummy)
c
c code assumes in many places that lower left corner at (0,0)
c this initial code sets the domain - assumed rectangular
c if it is too large, birect will chop it up into several rectangular
c pieces
c
      rnode(cornxlo,mstart)   = xlower
      rnode(cornylo,mstart)   = ylower
      rnode(cornyhi,mstart)   = yupper
      rnode(cornxhi,mstart)   = xupper
      node(nestlevel,mstart) = 1
      node(levelptr,mstart)  = 0
      lstart(1) = mstart

      if (((nx/2)*2 .ne. nx) .or. (ny/2)*2 .ne. ny) then 
         write(outunit,*)" must have even number of cells"
         write(*,*)      " must have even number of cells"
         stop
      endif

      node(ndilo,mstart) = 0
      node(ndjlo,mstart) = 0
      node(ndihi,mstart) = nx-1
      node(ndjhi,mstart) = ny-1

      lfine = 1
      call  birect(mstart)
      call  ginit (mstart, .true., nvar, naux)
c
c  set stable initial time step using coarse grid data
c
      if (vtime) then
         dtgrid = 1.e+20
         mptr = lstart(1)
         dx   = hxposs(1)
         dy   = hyposs(1)
         dt   = possk(1)
 60           mitot = node(ndihi,mptr)-node(ndilo,mptr) + 1 + 2*nghost
              mjtot = node(ndjhi,mptr)-node(ndjlo,mptr) + 1 + 2*nghost
              locaux = node(storeaux,mptr)
              call estdt(alloc(node(store1,mptr)),mitot,mjtot,nvar,
     1                   dx,dy,dt,nghost,alloc(locaux),naux)
              dtgrid = dmin1(dt,dtgrid)
              mptr   = node(levelptr,mptr)
            if (mptr .ne. 0) go to 60
         possk(1) = dtgrid
      endif
c
c set rest of possk array for refined timesteps
c
      iregsz(1) = nx
      jregsz(1) = ny
      do 70 level = 2, mxnest
         iregsz(level) = iregsz(level-1) * intrat(level-1)
         jregsz(level) = jregsz(level-1) * intrat(level-1)
         possk(level)  = possk(level-1)/float(intrat(level-1))
 70   continue
c
      return
      end
c
c
c
c
c     =================================================
      subroutine cellave(xlow,ylow,dx,dy,wl)
c     =================================================
      implicit double precision (a-h,o-z)
      external fss
      logical fl(5),alll,allr
      dimension x(10),y(10),xx(5),yy(5)
      common/fsscorn/ xc0,yc0,xc1,yc1
c   
c     # compute wl, fraction of cell that lies in left state.
c     # For initial data with two states ql and qr separated by a 
c     # discontinuity. The curve along which the discontinuity lies is
c     # specified by the function fdisc, which should return a value that
c     # is negative on the side where ql lies and positive on the qr side.
c
c     # xlow,ylow is the coordinate of the lower left corner of the cell.
c     # dx, dy are grid spacing in x and y.
c
      xx(1) = xlow
      xx(2) = xlow
      xx(3) = xlow+dx
      xx(4) = xlow+dx
      xx(5) = xx(1)
      yy(1) = ylow
      yy(2) = ylow+dy
      yy(3) = ylow+dy
      yy(4) = ylow
      yy(5) = yy(1)
      alll = .true.
      allr = .true.
c
      do 20 i=1,4
         fl(i) = fdisc(xx(i),yy(i)) .lt. 0.d0
         alll = alll .and. fl(i)
         allr = allr .and. (.not. fl(i))
   20    continue
      fl(5) = fl(1)
c
      if (alll) then
         wl = 1.d0
         return
         endif
      if (allr) then
         wl = 0.d0
         return
         endif
c
      iv = 0
      do 40 i=1,4
          if (fl(i)) then
               iv = iv+1
               x(iv) = xx(i)
               y(iv) = yy(i)
               endif
          if (fl(i).neqv.fl(i+1)) then
               iv = iv+1
               xc0 = xx(i)
               yc0 = yy(i)
               xc1 = xx(i+1)
               yc1 = yy(i+1)
               ss = zeroin(0.d0, 1.d0, fss, 1d-8)
c              write(27,*) 'xc,yc,ss:',xc0,yc0,xc1,yc1,ss
               x(iv) = xx(i) + ss*(xx(i+1)-xx(i))
               y(iv) = yy(i) + ss*(yy(i+1)-yy(i))
               endif
   40     continue
c
c     # compute area:
c
      if (iv.eq.0) then
         wl = 0.d0
         return
         endif
c
      x(iv+1) = x(1)
      y(iv+1) = y(1)
      area = 0.d0
      do 50 i=1,iv
         area = area + .5d0*(y(i)+y(i+1))*(x(i+1)-x(i))
c        write(27,*) '  x,y:',x(i),y(i)
   50    continue
c
      wl = area / (dx*dy)
c     write(27,*) 'area,wl:',area,wl
c
      return
      end
c
c
c
c
c
c     =================================================
      function fss(s)
c     =================================================
      implicit double precision (a-h,o-z)
      common/fsscorn/ xc0,yc0,xc1,yc1
c   
c     # compute fdisc at distance s between corners (xc0,yc0) and (xc1,yc1)
c
      x = xc0 + s*(xc1-xc0)
      y = yc0 + s*(yc1-yc0)
      fss = fdisc(x,y)
      return
      end
c
c
c
c     =================================================
      function zeroin(ax,bx,f,tol)                                         
c     =================================================
      implicit double precision (a-h,o-z)
      external f
c                                                                               
c      a zero of the function  f(x)  is computed in the interval ax,bx .        
c      (Standard routine from netlib)
c                                                                               
c  input..                                                                      
c                                                                               
c  ax     left endpoint of initial interval                                     
c  bx     right endpoint of initial interval                                    
c  f      function subprogram which evaluates f(x) for any x in                 
c         the interval  ax,bx                                                   
c  tol    desired length of the interval of uncertainty of the                  
c         final result ( .ge. 0.0)                                              
c                                                                               
c                                                                               
c  output..                                                                     
c                                                                               
c  zeroin abcissa approximating a zero of  f  in the interval ax,bx             
c                                                                               
c                                                                               
c      it is assumed  that   f(ax)   and   f(bx)   have  opposite  signs        
c  without  a  check.  zeroin  returns a zero  x  in the given interval         
c  ax,bx  to within a tolerance  4*macheps*dabs(x) + tol, where macheps          
c  is the relative machine precision.                                           
c      this function subprogram is a slightly  modified  translation  of        
c  the algol 60 procedure  zero  given in  richard brent, algorithms for        
c  minimization without derivatives, prentice - hall, inc. (1973).              
c                                                                               
c                                                                               
c                                                                               
c  compute eps, the relative machine precision                                  
c                                                                               
      eps = 1.0                                                                 
   10 eps = eps/2.0                                                             
      tol1 = 1.0 + eps                                                          
      if (tol1 .gt. 1.0) go to 10                                               
c                                                                               
c initialization                                                                
c                                                                               
      a = ax                                                                    
      b = bx                                                                    
      fa = f(a)                                                                 
      fb = f(b)                                                                 
c                                                                               
c begin step                                                                    
c                                                                               
   20 c = a                                                                     
      fc = fa                                                                   
      d = b - a                                                                 
      e = d                                                                     
   30 if (dabs(fc) .ge. dabs(fb)) go to 40                                        
      a = b                                                                     
      b = c                                                                     
      c = a                                                                     
      fa = fb                                                                   
      fb = fc                                                                   
      fc = fa                                                                   
c                                                                               
c convergence test                                                              
c                                                                               
   40 tol1 = 2.0*eps*dabs(b) + 0.5*tol                                           
      xm = .5*(c - b)                                                           
      if (dabs(xm) .le. tol1) go to 90                                           
      if (fb .eq. 0.0) go to 90                                                 
c                                                                               
c is bisection necessary                                                        
c                                                                               
      if (dabs(e) .lt. tol1) go to 70                                            
      if (dabs(fa) .le. dabs(fb)) go to 70                                        
c                                                                               
c is quadratic interpolation possible                                           
c                                                                               
      if (a .ne. c) go to 50                                                    
c                                                                               
c linear interpolation                                                          
c                                                                               
      s = fb/fa                                                                 
      p = 2.0*xm*s                                                              
      q = 1.0 - s                                                               
      go to 60                                                                  
c                                                                               
c inverse quadratic interpolation                                               
c                                                                               
   50 q = fa/fc                                                                 
      r = fb/fc                                                                 
      s = fb/fa                                                                 
      p = s*(2.0*xm*q*(q - r) - (b - a)*(r - 1.0))                              
      q = (q - 1.0)*(r - 1.0)*(s - 1.0)                                         
c                                                                               
c adjust signs                                                                  
c                                                                               
   60 if (p .gt. 0.0) q = -q                                                    
      p = dabs(p)                                                                
c                                                                               
c is interpolation acceptable                                                   
c                                                                               
      if ((2.0*p) .ge. (3.0*xm*q - dabs(tol1*q))) go to 70                       
      if (p .ge. dabs(0.5*e*q)) go to 70                                         
      e = d                                                                     
      d = p/q                                                                   
      go to 80                                                                  
c                                                                               
c bisection                                                                     
c                                                                               
   70 d = xm                                                                    
      e = d                                                                     
c                                                                               
c complete step                                                                 
c                                                                               
   80 a = b                                                                     
      fa = fb                                                                   
      if (dabs(d) .gt. tol1) b = b + d                                           
      if (dabs(d) .le. tol1) b = b + dsign(tol1, xm)                              
      fb = f(b)                                                                 
      if ((fb*(fc/dabs(fc))) .gt. 0.0) go to 20                                  
      go to 30                                                                  
c                                                                               
c done                                                                          
c                                                                               
   90 zeroin = b                                                                
      return                                                                    
      end                                                                       
c
c --------------------------------------------------------------
c
      subroutine setflags(iflags,isize,jsize,
     1                    rctold,nvar,mitot,mjtot,mptr)
c
      implicit double precision (a-h,o-z)

      include  "call.i"

      dimension rctold(mitot,mjtot,nvar)
      dimension iflags(0:isize+1,0:jsize+1)

c :::::::::::::::::::::: SETFLAGS ::::::::::::::::::::::::::::::::::
c transfer flagged arrays into 1 large array of entire domain
c makes buffering, projecting, etc. easier without searching 
c through all kinds of grids
c :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c

      ibeg = node(ndilo,mptr) - nghost
      jbeg = node(ndjlo,mptr) - nghost

      do 10 j = nghost+1, mjtot-nghost
      do 10 i = nghost+1, mitot-nghost
        iflags(ibeg+i,jbeg+j) = rctold(i,j,1)
 10   continue
c
 99   return
      end
c
c ----------------------------------------------------------
c
       subroutine shiftset(intarray,intarray2,idir,jdir,isize,jsize)

       implicit double precision (a-h, o-z)

       include "call.i"

       dimension intarray (0:isize+1,0:jsize+1), 
     1           intarray2(0:isize+1,0:jsize+1)

c :::::::::::::::::::::: CSHIFT :::::::::::::::::::::::::::::::
c shift by + or - 1 in either direction (but only 1 at a time)
c used for bit calculus for proper nesting, buffering, etc.
c similar to cshift on CM machine.
c includes periodic buffering as well.
c ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

       if (xperdom) then
          do 10 j = 0, jsize+1
             intarray(0,j) = intarray(isize,j)
             intarray(isize+1,j) = intarray(1,j)
 10       continue
       else
          do 11 j = 0, jsize+1
             intarray(0,j) = 0
             intarray(isize+1,j) = 0
 11       continue
       endif
       if (yperdom) then
          do 12 i = 0, isize+1
             intarray(i,0) = intarray(i,jsize)
             intarray(i,jsize+1) = intarray(i,1)
 12       continue
       else
          do 13 i = 0, isize+1
             intarray(i,0) = 0
             intarray(i,jsize+1) = 0
 13       continue
       endif

       if (idir .eq. 1) then
           do 22 i = 1, isize
           do 22 j = 1, jsize
              intarray2(i,j) = intarray(i+1,j)
 22        continue
       elseif (idir .eq. -1) then
           do 25 i = 1, isize
           do 25 j = 1, jsize
               intarray2(i,j) = intarray(i-1,j)
 25        continue
       elseif (jdir .eq. 1) then
           do 50 i = 1, isize
           do 50 j = 1, jsize
               intarray2(i,j) = intarray(i,j+1)
 50         continue
       elseif (jdir .eq. -1) then
           do 55 i = 1, isize
           do 55 j = 1, jsize
              intarray2(i,j) = intarray(i,j-1)
 55        continue
       endif

c   copy back

       do 60 i = 1, isize
       do 60 j = 1, jsize
         intarray(i,j) = max(intarray(i,j),intarray2(i,j))
 60    continue


       return
       end
c
c -----------------------------------------------------------
c
      subroutine conck(level, nvar)
c
      implicit double precision (a-h,o-z)

      include  "call.i"

      iadd(i,j,ivar)  = loc + i - 1 + mitot*((ivar-1)*mjtot+j-1)
      iaddaux(i,j) = locaux + i - 1 + mitot*(j-1) +
     .                        mitot*mjtot*(mcapa-1)
c
c ******************************************************************
c conck - conservation check  for specified level
c         mostly a debugging tool
c         this assumes grids don't overlap
c ******************************************************************
c
c
c  grid loop for given level
c
      hx      = hxposs(level)
      hy      = hyposs(level)
      dt      = possk(level)
      totmass = 0.d0

      mptr = lstart(level)
 20   if (mptr .eq. 0) go to 85
         loc    = node(store1,mptr)
         locaux = node(storeaux,mptr)
         nx     = node(ndihi,mptr) - node(ndilo,mptr) + 1
         ny     = node(ndjhi,mptr) - node(ndjlo,mptr) + 1
         mitot  = nx + 2*nghost
         mjtot  = ny + 2*nghost
c
         if (mcapa .eq. 0) then
           do 50 j  = nghost+1, mjtot-nghost
           do 50 i  = nghost+1, mitot-nghost
              totmass = totmass + alloc(iadd(i,j,1)) 
 50           continue
          else
c          # with capa array:
           do 60 j  = nghost+1, mjtot-nghost
           do 60 i  = nghost+1, mitot-nghost
              totmass = totmass + alloc(iadd(i,j,1))*alloc(iaddaux(i,j)) 
 60           continue
          endif
c
       mptr = node(levelptr,mptr)
       go to 20
c
 85    totmass = totmass * hx * hy
       write(outunit,*)" total mass = ", totmass
c
 99   return
      end
c
c ----------------------------------------------------
c
      subroutine domshrink(iflags2,iflags,idim,jdim)

      implicit double precision (a-h, o-z)

      include  "call.i"

      dimension  iflags2(0:idim+1,0:jdim+1)
      dimension  iflags (0:idim+1,0:jdim+1)

c
c :::::::::::::::::::::::::  DOMSHRINK ::::::::::::::::::::::::::::
c
c  shrink domain flags one cell for allowable properly nested domain
c  This is needed even for lcheck = lbase. More shrinking needed
c  for finer levels.
c
c :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

      if (dprint) then
        write(outunit,*)" from domshrink: on entry, iflags2"
        do 10 jj = 1, jdim
        j = jdim + 1 - jj
        write(outunit,100)(iflags2(i,j),i=1,idim)
 100    format(80i1)
 10     continue
      endif

      do 40 j = 1, jdim
      do 40 i = 1, idim
         iflags(i,j) = iflags2(i,j)
         if (iflags2(i  ,j  ) .le. 0 .or.
     1       iflags2(i+1,j  ) .le. 0 .or. iflags2(i-1,j  ) .le. 0 .or. 
     2       iflags2(i+1,j+1) .le. 0 .or. iflags2(i-1,j+1) .le. 0 .or. 
     3       iflags2(i  ,j-1) .le. 0 .or. iflags2(i  ,j+1) .le. 0 .or.
     4       iflags2(i+1,j-1) .le. 0 .or. iflags2(i-1,j-1) .le. 0) then
                 iflags(i,j) = 0
          endif
 40   continue
c
c if border of domain touches a physical boundary then set domain in
c ghost cell as well
c
       if (.not. xperdom) then
         do 55 j = 1, jdim
           if (iflags(1,j) .eq. 1) iflags(0,j) = 1
           if (iflags(idim,j) .eq. 1) iflags(idim+1,j) = 1
 55      continue
       endif
       if (.not. yperdom) then
         do 65 i = 1, idim
           if (iflags(i,1) .eq. 1) iflags(i,0) = 1
           if (iflags(i,jdim) .eq. 1) iflags(i,jdim+1) = 1
 65      continue
       endif

      if (dprint) then
        write(outunit,*)" from domshrink: on exit, iflags"
        do 70 jj = 1, jdim
        j = jdim + 1 - jj
        write(outunit,100)(iflags(i,j),i=1,idim)
 70     continue
      endif

      return
      end
c
c ----------------------------------------------------
c
      subroutine domprep(domflags,lbase,ibase,jbase)

      implicit double precision (a-h, o-z)

      include  "call.i"

      integer  domflags(0:ibase+1,0:jbase+1)

c
c ::::::::::::::::::::::::::: PREPDOM :::::::::::::::::::::
c 
c  prepare 2 dimensional array of domain for proper nesting
c
c :::::::::::::::::::::::::::::::::::::::::::::::::::::::


      do 10 j = 0, jbase+1
      do 10 i = 0, ibase+1
         domflags(i,j) = 0
 10   continue

      mptr = lstart(lbase)
 15   continue
      do 20 j = node(ndjlo,mptr) + 1, node(ndjhi,mptr) + 1
      do 20 i = node(ndilo,mptr) + 1, node(ndihi,mptr) + 1
         domflags(i,j) = 1
 20   continue
      mptr = node(levelptr, mptr)
      if (mptr .ne. 0) go to 15

c
c take care of periodic domains or if border of domain touches a 
c  physical boundary then set domain in ghost cell as well
c
      if (xperdom) then
         do 25 j = 0, jbase+1
           domflags(0,j)       = domflags(ibase,j)
           domflags(ibase+1,j) = domflags(1,j)
 25      continue
       else
         do 65 j = 1, jbase
           if (domflags(1,j) .eq. 1) domflags(0,j) = 1
           if (domflags(ibase,j) .eq. 1) domflags(ibase+1,j) = 1
 65      continue
      endif
      if (yperdom) then
         do 35 i = 0, ibase+1
           domflags(i,0)       = domflags(i,jbase)
           domflags(i,jbase+1) = domflags(i,1)
 35      continue
       else
         do 55 i = 1, ibase
           if (domflags(i,1) .eq. 1) domflags(i,0) = 1
           if (domflags(i,jbase) .eq. 1) domflags(i,jbase+1) = 1
 55      continue
      endif
c
c the 4 corners
c
        if (domflags(0,1)+domflags(1,0) .eq. 2) domflags(0,0)=1
        if (domflags(ibase,0)+domflags(ibase+1,1) .eq. 2) 
     .          domflags(ibase+1,0)=1
        if (domflags(ibase,jbase+1)+domflags(ibase+1,jbase) .eq. 2) 
     .          domflags(ibase+1,jbase+1)=1
        if (domflags(0,jbase)+domflags(1,jbase+1) .eq. 2) 
     .          domflags(0,jbase+1)=1

      if (dprint) then
         write(outunit,*)" from domprep: domflags at level  ", lbase
         do 40 jj = 1, jbase
         j = jbase + 1 - jj
         write(outunit,100)(domflags(i,j),i=1,ibase)
 100     format(80i1)
 40      continue
      endif

      return
      end
c
c ----------------------------------------------------
c
      subroutine domup(iflags2,iflags,ibase,jbase,isize,jsize,mult)

      implicit double precision (a-h, o-z)

      include  "call.i"

      dimension  iflags2(0:isize+1,0:jsize+1)
      dimension  iflags (0:ibase+1,0:jbase+1)

c
c ::::::::::::::::::::::::::: DOMUP :::::::::::::::::::::
c 
c  domain flags are in iflags. copy into iflags2, allowing
c  for change of level and dimension
c
c :::::::::::::::::::::::::::::::::::::::::::::::::::::::

      if (dprint) then
         write(outunit,*)" from domup: domflags (before expansion)"
         do 5 jj = 1, jbase
         j = jbase + 1 - jj
         write(outunit,100)(iflags(i,j),i=1,ibase)
 5       continue
      endif

      do 10 i = 0, isize+1
      do 10 j = 0, jsize+1
         iflags2(i,j) = 0
 10   continue

      do 20 i = 1, ibase
      do 20 j = 1, jbase
          ifine = (i-1) * mult
          jfine = (j-1) * mult
          do 25 mi = 1, mult
          do 25 mj = 1, mult
            iflags2(ifine+mi,jfine+mj) = iflags(i,j)  
 25       continue
 20       continue
c
c  take care of periodicity again or if border of domain touches a 
c  physical boundary then set domain in ghost cell as well
c
      if (xperdom) then
         do 35 j = 0, jsize+1
           iflags2(0,j)       = iflags2(isize,j)
           iflags2(isize+1,j) = iflags2(1,j)
 35      continue
       else
       do 55 j = 1, jsize
         if (iflags2(1,j) .eq. 1) iflags2(0,j) = 1
         if (iflags2(isize,j) .eq. 1) iflags2(isize+1,j) = 1
 55    continue
      endif
      if (yperdom) then
         do 45 i = 0, isize+1
           iflags2(i,0)       = iflags2(i,jsize)
           iflags2(i,jsize+1) = iflags2(i,1)
 45      continue
       else
       do 65 i = 1, isize
         if (iflags2(i,1) .eq. 1) iflags2(i,0) = 1
         if (iflags2(i,jsize) .eq. 1) iflags2(i,jsize+1) = 1
 65    continue
      endif

c
c the 4 corners
c
        if (iflags2(0,1)+iflags2(1,0) .eq. 2) iflags2(0,0)=1
        if (iflags2(isize,0)+iflags2(isize+1,1) .eq. 2)
     .          iflags2(isize+1,0)=1
        if (iflags2(isize,jsize+1)+iflags2(isize+1,jsize) .eq. 2)
     .          iflags2(isize+1,jsize+1)=1
        if (iflags2(0,jsize)+iflags2(1,jsize+1) .eq. 2)
     .          iflags2(0,jsize+1)=1


      if (dprint) then
         write(outunit,*)" from domup: domflags (after expansion)"
         do 70 jj = 1, jsize
         j = jsize + 1 - jj
         write(outunit,100)(iflags2(i,j),i=1,isize)
 100     format(80i1)
 70      continue
      endif

      return
      end
c
c ----------------------------------------------------
c
      subroutine domcopy(iflags2,iflags,isize,jsize)

      implicit double precision (a-h, o-z)

      include  "call.i"

      dimension  iflags2(0:isize+1,0:jsize+1)
      dimension  iflags (0:isize+1,0:jsize+1)

c
c ::::::::::::::::::::::::::: DOMCOPY :::::::::::::::::::::
c 
c  domain flags are in iflags. copy into iflags2.
c
c :::::::::::::::::::::::::::::::::::::::::::::::::::::::


      do 10 j = 0, jsize+1
      do 10 i = 0, isize+1
         iflags2(i,j) = iflags(i,j)
 10   continue
c
c  take care of periodicity again
c
      if (xperdom) then
         do 35 j = 0, jsize+1
           iflags2(0,j)       = iflags2(isize,j)
           iflags2(isize+1,j) = iflags2(1,j)
 35      continue
       else
         do 55 j = 1, jsize
           if (iflags2(1,j) .eq. 1) iflags2(0,j) = 1
           if (iflags2(isize,j) .eq. 1) iflags2(isize+1,j) = 1
 55      continue
      endif
      if (yperdom) then
         do 45 i = 0, isize+1
           iflags2(i,0)       = iflags2(i,jsize)
           iflags2(i,jsize+1) = iflags2(i,1)
 45      continue
       else
         do 65 i = 1, isize
           if (iflags2(i,1) .eq. 1) iflags2(i,0) = 1
           if (iflags2(i,jsize) .eq. 1) iflags2(i,jsize+1) = 1
 65      continue
      endif

      if (dprint) then
         write(outunit,*)" from domcopy: domflags "
         do 40 jj = 1, jsize
         j = jsize + 1 - jj
         write(outunit,100)(iflags2(i,j),i=1,isize)
 100     format(80i1)
 40      continue
      endif

 
      return
      end



c
c ---------------------------------------------------
c
       subroutine coarsen(valdub,midub,mjdub,valbgc,mi2tot,mj2tot,nvar)
      
       implicit double precision (a-h, o-z)

       dimension  valdub(midub, mjdub, nvar)
       dimension  valbgc(mi2tot,mj2tot,nvar)

c :::::::::::::::::::::::: COARSEN ::::::::::::::::::::::::::::::::
c coarsen = coarsen the fine grid data (with double the usual
c           number of ghost cells to prepare coarsened
c           grid for error estimation.
c :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

       do 10 ivar = 1, nvar
       do 10 j = 1, mj2tot

          jfine = 2*(j-1) + 1

          do 10 i = 1, mi2tot

             ifine = 2*(i-1) + 1
             valbgc(i,j,ivar) = (valdub(ifine,jfine,ivar) +
     &                           valdub(ifine+1,jfine,ivar)+
     &                           valdub(ifine,jfine+1,ivar) +
     &                           valdub(ifine+1,jfine+1,ivar))/4.d0
10     continue

       return
       end
c
c --------------------------------------------------------------------
c
       subroutine intcopy(val,mitot,mjtot,nvar,ilo,ihi,jlo,jhi,level,
     &                    iputst,jputst)

       implicit double precision (a-h, o-z)

       dimension val(mitot,mjtot,nvar)

       include "call.i"

       iadd(i,j,ivar) = loc + i - 1 + mi*((ivar-1)*mj+j-1)

c ::::::::::::::::::::::::::: INTCOPY :::::::::::::::::::::::::::::::
c
c    find intersecting grids at the same level. copy data from
c    old grid to val. 
c    old grid has "nghost" ghost cells - passed in nodal common block.
c    new grid has no ghost cells - indices describe entire patch.
c    iputst, jputst: where to copy values into. may not be in
c                    location corresponding to ilo,ihi,etc. if
c                    the patch has been periodically wrapped.
c    
c :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


       mptr = lstart(level)

 10    if (mptr .eq. 0) go to 99
          iglo = node(ndilo,mptr)
          ighi = node(ndihi,mptr)
          jglo = node(ndjlo,mptr)
          jghi = node(ndjhi,mptr)

c         # does it intersect?
          ixlo = max(iglo,ilo)
          ixhi = min(ighi,ihi)
          jxlo = max(jglo,jlo)
          jxhi = min(jghi,jhi)

          if (ixlo .le. ixhi .and. jxlo .le. jxhi) then
              loc  = node(store1,mptr)
              nx   = ighi - iglo + 1
              ny   = jghi - jglo + 1
              mi   = nx + 2*nghost
              mj   = ny + 2*nghost
              do 20 j    = jxlo, jxhi
              do 20 ivar = 1, nvar
              do 30 i    = ixlo, ixhi
                  val(iputst+i-ilo,jputst+j-jlo,ivar) =
     1                alloc(iadd(i-iglo+nghost+1,j-jglo+nghost+1,ivar))
 30           continue
 20           continue
          endif
          mptr = node(levelptr, mptr)
          go to 10

 99   return
      end
c
c --------------------------------------------------------------
c
      subroutine preintcopy(val,mitot,mjtot,nvar,ilo,ihi,jlo,jhi,
     1                      level)
c
      implicit double precision (a-h,o-z)

      include  "call.i"

      dimension val(mitot,mjtot,nvar)
      dimension ist(3), iend(3), jst(3), jend(3), ishift(3), jshift(3)

c
c  :::::::::::::: PREINTCOPY :::::::::::::::::::::::::::::::::::::::::::
c     For periodic boundary conditions more work needed to initialize a
c     new grid that sticks out. This routine was
c     called because the patch sticks out of the domain,
c     and has periodic bc.s preprocess the patch before calling
c     intcopy to shift the patch periodically back into the domain.
c
c     Inputs to this routine:
c     ilo,ihi,jlo,jhi = the location in index space of this patch.
c
c     Outputs from this routine:
c     The values of the grid are inserted
c     directly into the enlarged val array for this piece.
c
c :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

c
c     # will divide patch into 9 possibilities (some empty): 
c       x sticks out left, x interior, x sticks out right
c       same for y. for example, the max. would be
c       i from (ilo,-1), (0,iregsz(level)-1), (iregsz(level),ihi)
        
        ist(1) = ilo
        ist(2) = 0
        ist(3) = iregsz(level)
        iend(1) = -1
        iend(2) = iregsz(level)-1
        iend(3) = ihi
        jst(1) = jlo
        jst(2) = 0
        jst(3) = jregsz(level)
        jend(1) = -1
        jend(2) = jregsz(level)-1
        jend(3) = jhi
        ishift(1) = iregsz(level)
        ishift(2) = 0
        ishift(3) = -iregsz(level)
        jshift(1) = jregsz(level)
        jshift(2) = 0
        jshift(3) = -jregsz(level)

       do 20 i = 1, 3
          i1 = max(ilo,  ist(i))
          i2 = min(ihi, iend(i))
       do 10 j = 1, 3
          j1 = max(jlo,  jst(j))
          j2 = min(jhi, jend(j))


          if ((i1 .le. i2) .and. (j1 .le. j2)) then
            iputst = (i1 - ilo) + 1
            jputst = (j1 - jlo) + 1
            call intcopy(val,mitot,mjtot,nvar,
     2                    i1+ishift(i),i2+ishift(i),
     3                    j1+jshift(j),j2+jshift(j),level,
     4                    iputst,jputst)
          endif

 10    continue
 20    continue
      
     


      return
      end
c
c --------------------------------------------------------------------
c
       subroutine icall(val,aux,nrow,ncol,nvar,naux,
     .                  ilo,ihi,jlo,jhi,level,iputst,jputst)

       implicit double precision (a-h, o-z)

       dimension val(nrow,ncol,nvar)
       dimension aux(nrow,ncol,naux)

       include "call.i"

       iadd   (i,j,ivar) = loc    + i - 1 + mitot*((ivar-1)*mjtot+j-1)
       iaddaux(i,j,ivar) = locaux + i - 1 + mitot*((ivar-1)*mjtot+j-1)

c ::::::::::::::::::::::::::: ICALL :::::::::::::::::::::::::::::::
c
c    find intersecting grids at the same level. copy data from
c    intersecting grids to both val and aux arrays.
c
c    use larger definition of grids here - boundary data already in.
c    aux arrays also enlarged size.
c
c    iputst, jputst: where to copy values into. may not be in
c                    location corresponding to ilo,ihi,etc. if
c                    the patch has been periodically wrapped.

c    
c :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


       mptr = lstart(level)

 10    if (mptr .eq. 0) go to 99
          iglo = node(ndilo,mptr) 
          ighi = node(ndihi,mptr) 
          jglo = node(ndjlo,mptr) 
          jghi = node(ndjhi,mptr) 

c         # does it intersect?
          ixlo = max(iglo-nghost,ilo)
          ixhi = min(ighi+nghost,ihi)
          jxlo = max(jglo-nghost,jlo)
          jxhi = min(jghi+nghost,jhi)

          if (ixlo .le. ixhi .and. jxlo .le. jxhi) then
              loc  = node(store1,mptr)
              locaux = node(storeaux,mptr)
              nx   = ighi - iglo + 1
              ny   = jghi - jglo + 1
              mitot = nx + 2*nghost
              mjtot = ny + 2*nghost
              do 30 j    = jxlo, jxhi
              do 30 i    = ixlo, ixhi
              do 20 ivar = 1, nvar
                  ialloc  =  iadd(i-iglo+nghost+1,j-jglo+nghost+1,ivar)
                  val(i-ilo+iputst,j-jlo+jputst,ivar)  =  alloc(ialloc)
 20           continue
              do 25 iaux = 1, naux
                  ialloc = iaddaux(i-iglo+nghost+1,j-jglo+nghost+1,iaux)
                  aux(i-ilo+iputst,j-jlo+jputst,iaux)  =  alloc(ialloc)
 25           continue
 30           continue
          endif
          mptr = node(levelptr, mptr)
          go to 10

 99   return
      end
c
c --------------------------------------------------------------
c
      subroutine preicall(val,aux,nrow,ncol,nvar,naux,
     1                    ilo,ihi,jlo,jhi,level)
c
      implicit double precision (a-h,o-z)

      include  "call.i"

      dimension val(nrow,ncol,nvar)
      dimension aux(nrow,ncol,naux)

      dimension ist(3), iend(3), jst(3), jend(3), ishift(3), jshift(3)

c
c  :::::::::::::: PREICALL :::::::::::::::::::::::::::::::::::::::::::
c     For periodic boundary conditions more work needed to initialize a
c     new grid that sticks out. This routine was
c     called because the patch sticks out of the domain,
c     and has periodic bc.s preprocess the patch before calling
c     icall to shift the patch periodically back into the domain.
c
c     Inputs to this routine:
c     ilo,ihi,jlo,jhi = the location in index space of this patch.
c
c     Outputs from this routine:
c     The values of the grid are inserted
c     directly into the enlarged val array for this piece.
c
c :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

c
c     # will divide patch into 9 possibilities (some empty):
c       x sticks out left, x interior, x sticks out right
c       same for y. for example, the max. would be
c       i from (ilo,-1), (0,iregsz(level)-1), (iregsz(level),ihi)

        ist(1) = ilo
        ist(2) = 0
        ist(3) = iregsz(level)
        iend(1) = -1
        iend(2) = iregsz(level)-1
        iend(3) = ihi
        jst(1) = jlo
        jst(2) = 0
        jst(3) = jregsz(level)
        jend(1) = -1
        jend(2) = jregsz(level)-1
        jend(3) = jhi
        ishift(1) = iregsz(level)
        ishift(2) = 0
        ishift(3) = -iregsz(level)
        jshift(1) = jregsz(level)
        jshift(2) = 0
        jshift(3) = -jregsz(level)

       do 20 i = 1, 3
          i1 = max(ilo,  ist(i))
          i2 = min(ihi, iend(i))
       do 10 j = 1, 3
          j1 = max(jlo,  jst(j))
          j2 = min(jhi, jend(j))


          if ((i1 .le. i2) .and. (j1 .le. j2)) then
            iputst = i1 - ilo + 1
            jputst = j1 - jlo + 1
            call icall(val,aux,nrow,ncol,nvar,naux,
     1                    i1+ishift(i),i2+ishift(i),
     2                    j1+jshift(j),j2+jshift(j),level,
     3                    iputst,jputst)
          endif

 10    continue
 20    continue




      return
      end
c
c
c     ==========================================================
      subroutine step2(maxm,maxmx,maxmy,meqn,maux,mbc,mx,my,
     &                 qold,aux,dx,dy,dt,cflgrid,
     &                 fm,fp,gm,gp,
     &                 faddm,faddp,gaddm,gaddp,q1d,dtdx1d,dtdy1d,
     &                 aux1,aux2,aux3,work,mwork,rpn2,rpt2)
c     ==========================================================
c
c     # clawpack routine ...  modified for AMRCLAW
c
c     # Take one time step, updating q.
c     # On entry, qold gives
c     #    initial data for this step
c     #    and is unchanged in this version.
c    
c     # fm, fp are fluxes to left and right of single cell edge
c     # See the flux2 documentation for more information.
c
c
      implicit double precision (a-h,o-z)
      include  "call.i"


      dimension qold(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc, meqn)
      dimension   fm(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc, meqn)
      dimension   fp(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc, meqn)
      dimension   gm(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc, meqn)
      dimension   gp(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc, meqn)
      dimension  q1d(1-mbc:maxm+mbc, meqn)
      dimension faddm(1-mbc:maxm+mbc, meqn)
      dimension faddp(1-mbc:maxm+mbc, meqn)
      dimension gaddm(1-mbc:maxm+mbc, meqn, 2)
      dimension gaddp(1-mbc:maxm+mbc, meqn, 2)
      dimension aux(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc, maux)
      dimension aux1(1-mbc:maxm+mbc, maux)
      dimension aux2(1-mbc:maxm+mbc, maux)
      dimension aux3(1-mbc:maxm+mbc, maux)
      dimension dtdx1d(1-mbc:maxm+mbc)
      dimension dtdy1d(1-mbc:maxm+mbc)
      dimension work(mwork)
      common /comxyt/ dtcom,dxcom,dycom,tcom,icom,jcom
c
c     # store mesh parameters that may be needed in Riemann solver but not
c     # passed in...
      dxcom = dx
      dycom = dy
      dtcom = dt
c
c
c     # partition work array into pieces needed for local storage in
c     # flux2 routine.  Find starting index of each piece:
c
      i0wave = 1
      i0s = i0wave + (maxm+2*mbc)*meqn*mwaves
      i0amdq = i0s + (maxm+2*mbc)*mwaves
      i0apdq = i0amdq + (maxm+2*mbc)*meqn
      i0cqxx = i0apdq + (maxm+2*mbc)*meqn
      i0bmadq = i0cqxx + (maxm+2*mbc)*meqn
      i0bpadq = i0bmadq + (maxm+2*mbc)*meqn
      iused = i0bpadq + (maxm+2*mbc)*meqn - 1
c
      if (iused.gt.mwork) then
c        # This shouldn't happen due to checks in claw2
         write(outunit,*) 'not enough work space in step2'
         write(*      ,*) 'not enough work space in step2'
         stop 
         endif
c
c
      cflgrid = 0.d0
      dtdx = dt/dx
      dtdy = dt/dy
c
      do 10 m=1,meqn
         do 10 i=1-mbc,mx+mbc
            do 10 j=1-mbc,my+mbc
               fm(i,j,m) = 0.d0
               fp(i,j,m) = 0.d0
               gm(i,j,m) = 0.d0
               gp(i,j,m) = 0.d0
   10          continue
c
      if (mcapa.eq.0) then
c        # no capa array:
         do 5 i=1-mbc,maxm+mbc
            dtdx1d(i) = dtdx
            dtdy1d(i) = dtdy
    5       continue
         endif
c
c
c     # perform x-sweeps
c     ==================
c
      do 50 j = 0,my+1
c
c        # copy data along a slice into 1d arrays:
         do 20 m=1,meqn
           do 20 i = 1-mbc, mx+mbc
               q1d(i,m) = qold(i,j,m)
   20          continue
c
         if (mcapa.gt.0)  then
           do 21 i = 1-mbc, mx+mbc
               dtdx1d(i) = dtdx / aux(i,j,mcapa)
   21          continue
           endif
c
         if (maux .gt. 0)  then
             do 22 ma=1,maux
               do 22 i = 1-mbc, mx+mbc
                 aux1(i,ma) = aux(i,j-1,ma)
                 aux2(i,ma) = aux(i,j  ,ma)
                 aux3(i,ma) = aux(i,j+1,ma)
   22            continue
           endif
c
c
c        # Store the value of j along this slice in the common block
c        # comxyt in case it is needed in the Riemann solver (for
c        # variable coefficient problems)
         jcom = j  
c                  
c        # compute modifications fadd and gadd to fluxes along this slice:
         call flux2(1,maxm,meqn,maux,mbc,mx,
     &              q1d,dtdx1d,aux1,aux2,aux3,
     &              faddm,faddp,gaddm,gaddp,cfl1d,
     &              work(i0wave),work(i0s),work(i0amdq),work(i0apdq),
     &              work(i0cqxx),work(i0bmadq),work(i0bpadq),rpn2,rpt2)
         cflgrid = dmax1(cflgrid,cfl1d)
c
c        # update fluxes for use in AMR:
c
         do 25 m=1,meqn
            do 25 i=1,mx+1
               fm(i,j,m) = fm(i,j,m) + faddm(i,m)
               fp(i,j,m) = fp(i,j,m) + faddp(i,m)
               gm(i,j,m) = gm(i,j,m) + gaddm(i,m,1)
               gp(i,j,m) = gp(i,j,m) + gaddp(i,m,1)
               gm(i,j+1,m) = gm(i,j+1,m) + gaddm(i,m,2)
               gp(i,j+1,m) = gp(i,j+1,m) + gaddp(i,m,2)
   25          continue
   50    continue
c
c
c
c     # perform y sweeps
c     ==================
c
c
      do 100 i = 0, mx+1
c
c        # copy data along a slice into 1d arrays:
         do 70 m=1,meqn
           do 70 j = 1-mbc, my+mbc
               q1d(j,m) = qold(i,j,m)
   70          continue
c
         if (mcapa.gt.0)  then
           do 71 j = 1-mbc, my+mbc
               dtdy1d(j) = dtdy / aux(i,j,mcapa)
   71          continue
           endif
c
         if (maux .gt. 0)  then
             do 72 ma=1,maux
               do 72 j = 1-mbc, my+mbc
                 aux1(j,ma) = aux(i-1,j,ma)
                 aux2(j,ma) = aux(i,  j,ma)
                 aux3(j,ma) = aux(i+1,j,ma)
   72            continue
           endif
c
c
c        # Store the value of i along this slice in the common block
c        # comxyt in case it is needed in the Riemann solver (for
c        # variable coefficient problems)
         icom = i  
c                  
c        # compute modifications fadd and gadd to fluxes along this slice:
         call flux2(2,maxm,meqn,maux,mbc,my,
     &              q1d,dtdy1d,aux1,aux2,aux3,
     &              faddm,faddp,gaddm,gaddp,cfl1d,
     &              work(i0wave),work(i0s),work(i0amdq),work(i0apdq),
     &              work(i0cqxx),work(i0bmadq),work(i0bpadq),rpn2,rpt2)
c
         cflgrid = dmax1(cflgrid,cfl1d)
c
c        # 
c        # update fluxes for use in AMR:
c
         do 75 m=1,meqn
            do 75 j=1,my+1
               gm(i,j,m) = gm(i,j,m) + faddm(j,m)
               gp(i,j,m) = gp(i,j,m) + faddp(j,m)
               fm(i,j,m) = fm(i,j,m) + gaddm(j,m,1)
               fp(i,j,m) = fp(i,j,m) + gaddp(j,m,1)
               fm(i+1,j,m) = fm(i+1,j,m) + gaddm(j,m,2)
               fp(i+1,j,m) = fp(i+1,j,m) + gaddp(j,m,2)
   75          continue
  100    continue
c
c
      return
      end
c
c
c     =====================================================
      subroutine flux2(ixy,maxm,meqn,maux,mbc,mx,
     &                 q1d,dtdx1d,aux1,aux2,aux3,
     &                 faddm,faddp,gaddm,gaddp,cfl1d,wave,s,
     &                 amdq,apdq,cqxx,bmasdq,bpasdq,rpn2,rpt2)
c     =====================================================
c
c     # clawpack routine ...  modified for AMRCLAW
c
c     # Compute the modification to fluxes f and g that are generated by
c     # all interfaces along a 1D slice of the 2D grid. 
c     #    ixy = 1  if it is a slice in x
c     #          2  if it is a slice in y
c     # This value is passed into the Riemann solvers. The flux modifications
c     # go into the arrays fadd and gadd.  The notation is written assuming
c     # we are solving along a 1D slice in the x-direction.
c
c     # fadd(i,.) modifies F to the left of cell i
c     # gadd(i,.,1) modifies G below cell i
c     # gadd(i,.,2) modifies G above cell i
c
c     # The method used is specified by method(2:3):
c
c         method(2) = 1 if only first order increment waves are to be used.
c                   = 2 if second order correction terms are to be added, with
c                       a flux limiter as specified by mthlim.  
c
c         method(3) = 0 if no transverse propagation is to be applied.
c                       Increment and perhaps correction waves are propagated
c                       normal to the interface.
c                   = 1 if transverse propagation of increment waves 
c                       (but not correction waves, if any) is to be applied.
c                   = 2 if transverse propagation of correction waves is also
c                       to be included.  
c
c     Note that if mcapa>0 then the capa array comes into the second 
c     order correction terms, and is already included in dtdx1d:
c     If ixy = 1 then
c        dtdx1d(i) = dt/dx                      if mcapa= 0
c                  = dt/(dx*aux(i,jcom,mcapa))  if mcapa = 1
c     If ixy = 2 then
c        dtdx1d(j) = dt/dy                      if mcapa = 0
c                  = dt/(dy*aux(icom,j,mcapa))  if mcapa = 1
c
c     Notation:
c        The jump in q (q1d(i,:)-q1d(i-1,:))  is split by rpn2 into
c            amdq =  the left-going flux difference  A^- Delta q  
c            apdq = the right-going flux difference  A^+ Delta q  
c        Each of these is split by rpt2 into 
c            bmasdq = the down-going transverse flux difference B^- A^* Delta q
c            bpasdq =   the up-going transverse flux difference B^+ A^* Delta q
c        where A^* represents either A^- or A^+.
c
c
      implicit double precision (a-h,o-z)
      include "call.i"
      external rpn2, rpt2
      dimension    q1d(1-mbc:maxm+mbc, meqn)
      dimension   amdq(1-mbc:maxm+mbc, meqn)
      dimension   apdq(1-mbc:maxm+mbc, meqn)
      dimension bmasdq(1-mbc:maxm+mbc, meqn)
      dimension bpasdq(1-mbc:maxm+mbc, meqn)
      dimension   cqxx(1-mbc:maxm+mbc, meqn)
      dimension   faddm(1-mbc:maxm+mbc, meqn)
      dimension   faddp(1-mbc:maxm+mbc, meqn)
      dimension   gaddm(1-mbc:maxm+mbc, meqn, 2)
      dimension   gaddp(1-mbc:maxm+mbc, meqn, 2)
      dimension dtdx1d(1-mbc:maxm+mbc)
      dimension aux1(1-mbc:maxm+mbc, maux)
      dimension aux2(1-mbc:maxm+mbc, maux)
      dimension aux3(1-mbc:maxm+mbc, maux)
c
      dimension     s(1-mbc:maxm+mbc, mwaves)
      dimension  wave(1-mbc:maxm+mbc, meqn, mwaves)
c
      logical limit
      common /comxyt/ dtcom,dxcom,dycom,tcom,icom,jcom
c
      limit = .false.
      do 5 mw=1,mwaves
         if (mthlim(mw) .gt. 0) limit = .true.
   5     continue
c
c     # initialize flux increments:
c     -----------------------------
c
      do 30 jside=1,2
         do 20 m=1,meqn
            do 10 i = 1-mbc, mx+mbc
               faddm(i,m) = 0.d0
               faddp(i,m) = 0.d0
               gaddm(i,m,jside) = 0.d0
               gaddp(i,m,jside) = 0.d0
   10          continue
   20       continue
   30    continue
c
c
c     # solve Riemann problem at each interface and compute Godunov updates
c     ---------------------------------------------------------------------
c
      call rpn2(ixy,maxm,meqn,mwaves,mbc,mx,q1d,q1d,
     &          aux2,aux2,wave,s,amdq,apdq)
c
c     # Set fadd for the donor-cell upwind method (Godunov)
      do 40 i=1,mx+1
         do 40 m=1,meqn
            faddp(i,m) = faddp(i,m) - apdq(i,m)
            faddm(i,m) = faddm(i,m) + amdq(i,m)
   40       continue
c
c     # compute maximum wave speed for checking Courant number:
      cfl1d = 0.d0
      do 50 mw=1,mwaves
         do 50 i=1,mx+1
c          # if s>0 use dtdx1d(i) to compute CFL,
c          # if s<0 use dtdx1d(i-1) to compute CFL:
            cfl1d = dmax1(cfl1d, dtdx1d(i)*s(i,mw),
     &                          -dtdx1d(i-1)*s(i,mw))
   50       continue
c
      if (method(2).eq.1) go to 130
c
c     # modify F fluxes for second order q_{xx} correction terms:
c     -----------------------------------------------------------
c
c     # apply limiter to waves:
      if (limit) call limiter(maxm,meqn,mwaves,mbc,mx,wave,s,mthlim)
c
      do 120 i = 1, mx+1
c
c        # For correction terms below, need average of dtdx in cell
c        # i-1 and i.  Compute these and overwrite dtdx1d:
c
         dtdx1d(i-1) = 0.5d0 * (dtdx1d(i-1) + dtdx1d(i))
c
         do 120 m=1,meqn
            cqxx(i,m) = 0.d0
            do 119 mw=1,mwaves
c
c              # second order corrections:
               cqxx(i,m) = cqxx(i,m) + dabs(s(i,mw))
     &             * (1.d0 - dabs(s(i,mw))*dtdx1d(i-1)) * wave(i,m,mw)
c
  119          continue
            faddm(i,m) = faddm(i,m) + 0.5d0 * cqxx(i,m)
            faddp(i,m) = faddp(i,m) + 0.5d0 * cqxx(i,m)
  120       continue
c
c
  130  continue
c
       if (method(3).eq.0) go to 999   !# no transverse propagation
c
       if (method(3).eq.2) then
c         # incorporate cqxx into amdq and apdq so that it is split also.
          do 150 i = 1, mx+1
             do 150 m=1,meqn
                amdq(i,m) = amdq(i,m) + cqxx(i,m)
                apdq(i,m) = apdq(i,m) - cqxx(i,m)
  150           continue
          endif
c
c
c      # modify G fluxes for transverse propagation
c      --------------------------------------------
c
c
c     # split the left-going flux difference into down-going and up-going:
      call rpt2(ixy,maxm,meqn,mwaves,mbc,mx,
     &          q1d,q1d,aux1,aux2,aux3,
     &          1,amdq,bmasdq,bpasdq)
c
c     # modify flux below and above by B^- A^- Delta q and  B^+ A^- Delta q:
      do 160 m=1,meqn
          do 160 i = 1, mx+1
               gupdate = 0.5d0*dtdx1d(i-1) * bmasdq(i,m)
               gaddm(i-1,m,1) = gaddm(i-1,m,1) - gupdate
               gaddp(i-1,m,1) = gaddp(i-1,m,1) - gupdate
c
               gupdate = 0.5d0*dtdx1d(i-1) * bpasdq(i,m)
               gaddm(i-1,m,2) = gaddm(i-1,m,2) - gupdate
               gaddp(i-1,m,2) = gaddp(i-1,m,2) - gupdate
  160          continue
c
c     # split the right-going flux difference into down-going and up-going:
      call rpt2(ixy,maxm,meqn,mwaves,mbc,mx,
     &          q1d,q1d,aux1,aux2,aux3,
     &          2,apdq,bmasdq,bpasdq)
c
c     # modify flux below and above by B^- A^+ Delta q and  B^+ A^+ Delta q:
      do 180 m=1,meqn
          do 180 i = 1, mx+1
               gupdate = 0.5d0*dtdx1d(i-1) * bmasdq(i,m)
               gaddm(i,m,1) = gaddm(i,m,1) - gupdate
               gaddp(i,m,1) = gaddp(i,m,1) - gupdate
c
               gupdate = 0.5d0*dtdx1d(i-1) * bpasdq(i,m)
               gaddm(i,m,2) = gaddm(i,m,2) - gupdate
               gaddp(i,m,2) = gaddp(i,m,2) - gupdate
  180          continue
c
  999 continue
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
c     # given by dotl/wnorm and dotr/wnorm, where wnorm is the 2-norm
c     # of wave.
c
      implicit double precision(a-h,o-z)
      dimension mthlim(mwaves)
      dimension wave(1-mbc:maxm+mbc, meqn, mwaves)
      dimension    s(1-mbc:maxm+mbc, mwaves)
c
c
      do 50 mw=1,mwaves
         if (mthlim(mw) .eq. 0) go to 50
         dotr = 0.d0
         do 40 i = 0, mx+1
            wnorm = 0.d0
            dotl = dotr
            dotr = 0.d0
            do 20 m=1,meqn
               wnorm = wnorm + wave(i,m,mw)**2
               dotr = dotr + wave(i,m,mw)*wave(i+1,m,mw)
   20          continue
            if (i.eq.0) go to 40
            wnorm = dsqrt(wnorm)
            if (wnorm.eq.0.d0) go to 40
c
            if (s(i,mw) .gt. 0.d0) then
                wlimitr = philim(wnorm, dotl/wnorm, mthlim(mw))
              else
                wlimitr = philim(wnorm, dotr/wnorm, mthlim(mw))
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
c     # NOTE: This routine is obsolete.  Instead of using limiter.f,
c     # which calls philim.f for every wave, it is more efficient to 
c     # use inlinelimiter.f, which eliminates all these function calls
c     # to philim.  If you wish to change the limiter function and are
c     # using inlinelimiter.f, the formulas must be changed in that routine.
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
c
c ------------------------------------------------------------
c
       subroutine cstore(qc,nrow,ncol,nvar,qc1d,lenbc,naux,auxc,auxc1d)

       implicit double precision (a-h, o-z)

       dimension qc(nrow,ncol,nvar)
       dimension qc1d(lenbc,nvar)
       dimension auxc(nrow,ncol,naux)
       dimension auxc1d(lenbc,naux)
c
c      store coarse perimeter worth of solution into 1d array.
c      go around fine grid in following order
c                2
c           __________
c        1 |          | 3
c           __________
c               4
c
c  save first interior cell of enlarged grid corresponding to
c  fine grid bordering cell. note that since fine grid is smaller,
c  the cell is one in. coarse (temporary) grid has no ghost cells

c      side 1
       index = 0
       do 10 j = 2, ncol-1
         index = index + 1
         do 5 ivar = 1, nvar
 5         qc1d(index,ivar) = qc(1,j,ivar)
         do 6 iaux = 1, naux
 6         auxc1d(index,iaux) = auxc(1,j,iaux)
 10    continue

c      side 2
       do 20 i = 2, nrow-1
         index = index + 1
         do 15 ivar = 1, nvar
 15        qc1d(index,ivar) = qc(i,ncol,ivar)
         do 16 iaux = 1, naux
 16        auxc1d(index,iaux) = auxc(i,ncol,iaux)
 20    continue

c      side 3
       do 30 j = 2, ncol-1
         index = index + 1
         do 25 ivar = 1, nvar
 25        qc1d(index,ivar) = qc(nrow,j,ivar)
         do 26 iaux = 1, naux
 26        auxc1d(index,iaux) = auxc(nrow,j,iaux)
 30    continue

c      side 4
       do 40 i = 2, nrow-1
         index = index + 1
         do 35 ivar = 1, nvar
 35        qc1d(index,ivar) = qc(i,1,ivar)
         do 36 iaux = 1, naux
 36        auxc1d(index,iaux) = auxc(i,1,iaux)
 40    continue

       return
       end
c
c  ================================================================
      subroutine saveqc(level,nvar,naux)
c  ================================================================
c
      implicit double precision (a-h,o-z)

      include  "call.i"
c
c ::::::::::::::::::::::::: SAVEQC :::::::::::::::::::::::::::::::::
c prepare new fine grids to save fluxes after each integration step
c for future conservative fix-up on coarse grids.
c save all boundary fluxes of fine grid (even if on a  phys. bndry.) -
c but only save space for every intrat of them. 
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c
      levc = level - 1
      hxc  = hxposs(levc)
      hyc  = hyposs(levc)

      mkid = lstart(level)
 10   if (mkid .eq. 0) go to 99
          nx    = node(ndihi,mkid)-node(ndilo,mkid) + 1
          ny    = node(ndjhi,mkid)-node(ndjlo,mkid) + 1
          ikeep = nx/intrat(level-1)
          jkeep = ny/intrat(level-1)
          lenbc = 2*(ikeep+jkeep)
          ist   = node(ffluxptr,mkid)
          time = rnode(timemult,mkid)

c         make coarsened enlarged patch for conservative fixup
          ilo = node(ndilo,mkid)
          jlo = node(ndjlo,mkid)
          ihi = node(ndihi,mkid)
          jhi = node(ndjhi,mkid)
          iclo = ilo/intrat(level-1) - 1
          jclo = jlo/intrat(level-1) - 1
          ichi = (ihi+1)/intrat(level-1)
          jchi = (jhi+1)/intrat(level-1)
          nrow = ichi-iclo+1
          ncol = jchi-jclo+1
          xl   = rnode(cornxlo,mkid) - hxc
          yb   = rnode(cornylo,mkid) - hyc
          xr   = rnode(cornxhi,mkid) + hxc
          yt   = rnode(cornyhi,mkid) + hyc
          loctmp = igetsp(nrow*ncol*(nvar+naux))
          loctx  = loctmp + nrow*ncol*nvar
          locaux = node(storeaux,mkid)
          if (xperdom .and. yperdom) then
            call preicall(alloc(loctmp),alloc(loctx),nrow,ncol,nvar,
     .                 naux,iclo,ichi,jclo,jchi,level-1)
          else 
            call icall(alloc(loctmp),alloc(loctx),nrow,ncol,nvar,naux,
     .                   iclo,ichi,jclo,jchi,level-1,1,1)
          endif
          call bc2amr(alloc(loctmp),alloc(loctx),nrow,ncol,nvar,naux,
     .                hxc,hyc,level,time,
     .                xl,xr,yb,yt,
     .                xlower,ylower,xupper,yupper,
     .                xperdom,yperdom)
          call cstore(alloc(loctmp),nrow,ncol,nvar,
     .                alloc(ist+nvar*lenbc),lenbc,naux,alloc(loctx),
     .                alloc(ist+2*nvar*lenbc))
          call reclam(loctmp,nrow*ncol*(nvar+naux))

          mkid = node(levelptr,mkid)
          go to 10
 99    return
       end
