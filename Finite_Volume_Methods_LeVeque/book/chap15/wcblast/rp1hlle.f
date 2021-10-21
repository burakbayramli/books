c
c
c
c =========================================================
      subroutine rp1(maxmx,meqn,mwaves,mbc,mx,ql,qr,auxl,auxr,
     &		 wave,s,amdq,apdq)
c =========================================================
c
c     # solve Riemann problem using a variant of the HLLE solver.
c
c     # The user must specify the flux function using ffcn below, and
c     # also provide a subroutine sminmax that returns estimates of
c     # min and max wave speeds.
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

      common /param/ gamma,gamma1
c
c     # local storage
c     ---------------
      parameter (max2 = 4002)  !# assumes at most 4000 grid points with mbc=2
      dimension s1(-1:max2), s2(-1:max2)
      dimension f(-1:max2, 3)
c
      if (mx+mbc .gt. max2) then
         write(6,*) '*** Error *** need to increase max2 in rp1eux'
         stop 
         endif
c
      if (mwaves .ne. 2) then
         write(6,*) '*** Error ***  Set mwaves = 2 to use rp1hlle'
         stop
         endif
c
c     # evaluate flux function at each cell average
c     # (assuming ql=qr = cell average):
      call ffcn(maxmx,max2,meqn,mbc,mx,ql,f)
c
c     # estimate largest and smallest wave speed at each interface:
      call sminmax(maxmx,max2,meqn,mbc,mx,ql,s1,s2)
c
c     # 1-wave and 2-wave are chosen to span all characteristics

      do 100 i=2-mbc,mx+mbc
         do 90 m=1,meqn
            dq = ql(i,m) - qr(i-1,m)
            df = f(i,m) - f(i-1,m)
            s(i,1) = s1(i)
            s(i,2) = s2(i)

            wave(i,m,1) = (df - s(i,2)*dq) / (s(i,1)-s(i,2))
            wave(i,m,2) = (df - s(i,1)*dq) / (s(i,2)-s(i,1))
   90       continue
 100    continue
c
c     # Compute fluctuations:
c     # amdq = SUM s*wave   over left-going waves
c     # apdq = SUM s*wave   over right-going waves
c
      do 200 m=1,meqn
         do 180 i=2-mbc, mx+mbc
            amdq(i,m) = 0.d0
            apdq(i,m) = 0.d0
            do 150 mw=1,mwaves
               if (s(i,mw) .lt. 0.d0) then
                   amdq(i,m) = amdq(i,m) + s(i,mw)*wave(i,m,mw)
                 else
                   apdq(i,m) = apdq(i,m) + s(i,mw)*wave(i,m,mw)
                 endif
  150          continue
  180       continue
  200    continue

      return
      end


c =========================================================
      subroutine ffcn(maxmx,max2,meqn,mbc,mx,q,f)
c =========================================================
      implicit double precision (a-h,o-z)
      dimension   q(1-mbc:maxmx+mbc, meqn)
      dimension   f(-1:max2, meqn)

      common /param/ gamma,gamma1

c     # user-supplied function to evaluate the flux function f(q)
c
c     # Euler equations:

      do i=1-mbc,mx+mbc
         rhou2 = q(i,2)**2. / q(i,1)
         pres = gamma1 * (q(i,3) - 0.5d0*rhou2)
         f(i,1) = q(i,2)
         f(i,2) = rhou2 + pres
         f(i,3) = (q(i,3) + pres)*q(i,2)/q(i,1)

         enddo

      return
      end

c =========================================================
      subroutine sminmax(maxmx,max2,meqn,mbc,mx,q,smin,smax)
c =========================================================
      implicit double precision (a-h,o-z)
      dimension   q(1-mbc:maxmx+mbc, meqn)
      dimension   smin(-1:max2),smax(-1:max2)
c     # local variable
      dimension   sqmin(-1:max2),sqmax(-1:max2)

c
      common /param/ gamma,gamma1


c     # user-supplied function to estimate the mininum and
c     # maximum wave speeds across interface between cells i-1 and i
c
c     # HLLE method for the Euler equations:
c     # use characteristic speeds u-c and u+c and compare to Roe averages.

      do i=1-mbc,mx+mbc
c        # compute min and max characteristic speed in i'th cell:
c
         u = q(i,2) / q(i,1)
         rhou2 = q(i,2)**2. / q(i,1)
         pres = gamma1 * (q(i,3) - 0.5d0*rhou2)
c
         if(pres.lt.0.d0) then
           write(6,*) "negative pressure in rp1", i, pres
           stop
         endif

         c = dsqrt(gamma * pres / q(i,1))
         if (i .lt. mx+mbc)  sqmin(i) = u-c
         sqmax(i) = u+c
         enddo
c

      do i=2-mbc,mx+mbc
c        # compute Roe averages across i-1/2 interface:
         rhsqrtl = dsqrt(q(i-1,1))
         rhsqrtr = dsqrt(q(i,1))
         pl = gamma1*(q(i-1,3) - 0.5d0*(q(i-1,2)**2)/q(i-1,1))
         pr = gamma1*(q(i,3) - 0.5d0*(q(i,2)**2)/q(i,1))

         rhsq2 = rhsqrtl + rhsqrtr
         uroe = (q(i-1,2)/rhsqrtl + q(i,2)/rhsqrtr) / rhsq2
         enthroe = (((q(i-1,3)+pl)/rhsqrtl
     &             + (q(i,3)+pr)/rhsqrtr)) / rhsq2
         a2 = gamma1*(enthroe - .5d0*uroe**2)

         if (a2.lt.0.d0) then
            write(6,*) '*** a2 negative in rp1'
            write(6,*) 'i =',i,'  a2 =',a2
            stop
            endif

         aroe = dsqrt(a2)
         sminroe = uroe - aroe
         smaxroe = uroe + aroe
         smin(i) = dmin1(sqmin(i-1),sminroe)
         smax(i) = dmax1(sqmax(i),smaxroe)
         enddo

      return
      end
