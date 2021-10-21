c
c
c     =====================================================
      subroutine rpt2(ixy,maxm,meqn,mwaves,mbc,mx,
     &                  ql,qr,aux1,aux2,aux3,
     &			imp,asdq,bmasdq,bpasdq)
c     =====================================================
      implicit double precision (a-h,o-z)
c
c     # Riemann solver in the transverse direction for the Euler 
c     # equations  on a curvilinear grid.
c
c     # Split asdq (= A^* \Delta q, where * = + or -)
c     # into down-going flux difference bmasdq (= B^- A^* \Delta q)
c     #    and up-going flux difference bpasdq (= B^+ A^* \Delta q)
c    
c     # Use the same idea as in rpn2 but now rotate into the direction 
c     # normal to the cell edge above or below this cell.
c
c     # Uses Roe averages 
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
      parameter (maxm2 = 502)  !# assumes at most 500x500 grid with mbc=2
      dimension delta(4)
      dimension u2v2(-1:maxm2),
     &       u(-1:maxm2),v(-1:maxm2),enth(-1:maxm2),a(-1:maxm2),
     &       g1a2(-1:maxm2),euv(-1:maxm2) 
      dimension ax(-1:maxm2)
      dimension ay(-1:maxm2)
      dimension wave(-1:maxm2, 4, 3)
      dimension    s(-1:maxm2, 3)
      common /param/ gamma, gamma1
c
      if (-1.gt.1-mbc .or. maxm2 .lt. maxm+mbc) then
	 write(6,*) 'need to increase maxm2 in rpt2'
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
c        # since states and grid orientation are different on each side.
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
	   pres = gamma1*(ql(i1,4)  - 0.5d0*(ql(i1,2)**2 +
     &            ql(i1,3)**2)/ql(i1,1))
           u(i) = (ax(i)*ql(i1,2) + ay(i)*ql(i1,3)) / ql(i1,1)
           v(i) = (-ay(i)*ql(i1,2) + ax(i)*ql(i1,3)) / ql(i1,1)
	   enth(i) = (ql(i1,4)+pres) / ql(i1,1)
	   u2v2(i) = u(i)**2 + v(i)**2
           a2 = gamma1*(enth(i) - .5d0*u2v2(i))
           a(i) = dsqrt(a2)
	   g1a2(i) = gamma1 / a2
	   euv(i) = enth(i) - u2v2(i) 
           enddo
c
c
c     # now split asdq into waves:
c
      do 20 i = ix1,ixm1
         delta(1) = asdq(i,1) 
         delta(2) = ax(i)*asdq(i,2) + ay(i)*asdq(i,3)
         delta(3) = -ay(i)*asdq(i,2) + ax(i)*asdq(i,3)
         delta(4) = asdq(i,4) 

         a3 = g1a2(i) * (euv(i)*delta(1)
     &      + u(i)*delta(2) + v(i)*delta(3) - delta(4))
         a2 = delta(3) - v(i)*delta(1)
         a4 = (delta(2) + (a(i)-u(i))*delta(1) - a(i)*a3) / (2.d0*a(i))
         a1 = delta(1) - a3 - a4

c
c        # Compute the waves.
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
c    # compute flux difference bpasdq
c    --------------------------------
c
      do 40 m=1,meqn
         do 40 i=ix1,ixm1
	    bpasdq(i,m) = 0.d0
	    do 30 mw=1,mwaves
	       bpasdq(i,m) = bpasdq(i,m) + dmax1(s(i,mw),0.d0)
     &                        *wave(i,m,mw)*aux3(i1,ilenrat)
   30          continue
   40       continue
c
c     # rotate momentum components:
      do 50 i=ix1,ixm1
	 bpasdq2 = ax(i)*bpasdq(i,2) - ay(i)*bpasdq(i,3)
	 bpasdq3 = ay(i)*bpasdq(i,2) + ax(i)*bpasdq(i,3)
	 bpasdq(i,2) = bpasdq2
	 bpasdq(i,3) = bpasdq3
   50    continue
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
	   pres = gamma1*(ql(i1,4)  - 0.5d0*(ql(i1,2)**2 +
     &            ql(i1,3)**2)/ql(i1,1))
           u(i) = (ax(i)*ql(i1,2) + ay(i)*ql(i1,3)) / ql(i1,1)
           v(i) = (-ay(i)*ql(i1,2) + ax(i)*ql(i1,3)) / ql(i1,1)
	   enth(i) = (ql(i1,4)+pres) / ql(i1,1)
	   u2v2(i) = u(i)**2 + v(i)**2
           a2 = gamma1*(enth(i) - .5d0*u2v2(i))
           a(i) = dsqrt(a2)
	   g1a2(i) = gamma1 / a2
	   euv(i) = enth(i) - u2v2(i) 
           enddo
c
c
c
c     # now split asdq into waves:
c
      do 80 i = ix1,ixm1
         delta(1) = asdq(i,1) 
         delta(2) = ax(i)*asdq(i,2) + ay(i)*asdq(i,3)
         delta(3) = -ay(i)*asdq(i,2) + ax(i)*asdq(i,3)
         delta(4) = asdq(i,4) 

         a3 = g1a2(i) * (euv(i)*delta(1)
     &      + u(i)*delta(2) + v(i)*delta(3) - delta(4))
         a2 = delta(3) - v(i)*delta(1)
         a4 = (delta(2) + (a(i)-u(i))*delta(1) - a(i)*a3) / (2.d0*a(i))
         a1 = delta(1) - a3 - a4

c
c        # Compute the waves.

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
c
   80    continue
c
c
c    # compute flux difference bmasdq
c    --------------------------------
c
      do 100 m=1,meqn
         do 100 i=ix1,ixm1
	    bmasdq(i,m) = 0.d0
	    do 90 mw=1,mwaves
	       bmasdq(i,m) = bmasdq(i,m) + dmin1(s(i,mw), 0.d0)
     &                        *wave(i,m,mw)*aux2(i1,ilenrat)
   90          continue
  100       continue
c
c     # rotate momentum components:
      do 150 i=ix1,ixm1
	 bmasdq2 = ax(i)*bmasdq(i,2) - ay(i)*bmasdq(i,3)
	 bmasdq3 = ay(i)*bmasdq(i,2) + ax(i)*bmasdq(i,3)
	 bmasdq(i,2) = bmasdq2
	 bmasdq(i,3) = bmasdq3
  150    continue
c
c
      return
      end
