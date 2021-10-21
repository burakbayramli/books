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
      dimension alf(-1:maxm2)
      dimension beta(-1:maxm2)
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
c               [ alf  beta ]
c               [-beta  alf ]
c
        do i=ix1,ixm1
c
         if (imp.eq.1) then
             i1 = i-1
           else
             i1 = i
           endif
c
           alf(i) = aux3(i1,inx)
           beta(i) = aux3(i1,iny)
           h(i) = ql(i1,1)
           u(i) = (alf(i)*ql(i1,2) + beta(i)*ql(i1,3)) / h(i)
           v(i) = (-beta(i)*ql(i1,2) + alf(i)*ql(i1,3)) / h(i)
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
         delta(2) = alf(i)*asdq(i,2) + beta(i)*asdq(i,3)
         delta(3) = -beta(i)*asdq(i,2) + alf(i)*asdq(i,3)
         a1 = ((u(i)+a(i))*delta(1) - delta(2))*(0.50d0/a(i))
         a2 = -v(i)*delta(1) + delta(3)
         a3 = (-(u(i)-a(i))*delta(1) + delta(2))*(0.50d0/a(i))
c
c        # Compute the waves.
c
         wave(i,1,1) = a1
         wave(i,2,1) = alf(i)*a1*(u(i)-a(i)) - beta(i)*a1*v(i)
         wave(i,3,1) = beta(i)*a1*(u(i)-a(i)) + alf(i)*a1*v(i)
         s(i,1) = (u(i)-a(i)) * aux3(i1,ilenrat)
c
         wave(i,1,2) = 0.0d0
         wave(i,2,2) = -beta(i)*a2
         wave(i,3,2) = alf(i)*a2
         s(i,2) = u(i) * aux3(i1,ilenrat)
c
         wave(i,1,3) = a3
         wave(i,2,3) = alf(i)*a3*(u(i)+a(i)) - beta(i)*a3*v(i)
         wave(i,3,3) = beta(i)*a3*(u(i)+a(i)) + alf(i)*a3*v(i)
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
c               [ alf  beta ]
c               [-beta  alf ]
c
        do i=ix1,ixm1
c
         if (imp.eq.1) then
             i1 = i-1
           else
             i1 = i
           endif
c
           alf(i) = aux2(i1,inx)
           beta(i) = aux2(i1,iny)
           u(i) = (alf(i)*ql(i1,2) + beta(i)*ql(i1,3)) / h(i)
           v(i) = (-beta(i)*ql(i1,2) + alf(i)*ql(i1,3)) / h(i)
           enddo
c
c
c
c     # now split asdq into waves:
c
c     # find a1 thru a3, the coefficients of the 3 eigenvectors:
      do 80 i = ix1,ixm1
         delta(1) = asdq(i,1) 
         delta(2) = alf(i)*asdq(i,2) + beta(i)*asdq(i,3)
         delta(3) = -beta(i)*asdq(i,2) + alf(i)*asdq(i,3)
         a1 = ((u(i)+a(i))*delta(1) - delta(2))*(0.50d0/a(i))
         a2 = -v(i)*delta(1) + delta(3)
         a3 = (-(u(i)-a(i))*delta(1) + delta(2))*(0.50d0/a(i))
c
c        # Compute the waves.
c
         wave(i,1,1) = a1
         wave(i,2,1) = alf(i)*a1*(u(i)-a(i)) - beta(i)*a1*v(i)
         wave(i,3,1) = beta(i)*a1*(u(i)-a(i)) + alf(i)*a1*v(i)
         s(i,1) = (u(i)-a(i)) * aux2(i1,ilenrat)
c
         wave(i,1,2) = 0.0d0
         wave(i,2,2) = -beta(i)*a2
         wave(i,3,2) = alf(i)*a2
         s(i,2) = u(i) * aux2(i1,ilenrat)
c
         wave(i,1,3) = a3
         wave(i,2,3) = alf(i)*a3*(u(i)+a(i)) - beta(i)*a3*v(i)
         wave(i,3,3) = beta(i)*a3*(u(i)+a(i)) + alf(i)*a3*v(i)
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
