c
c
c =========================================================
      subroutine rp1(maxmx,meqn,mwaves,mbc,mx,ql,qr,auxl,auxr,
     &		 wave,s,amdq,apdq)
c =========================================================
c
c     # Solve Riemann problems for the 1D advection equation q_t + u(x)*q_x = 0
c     # with variable u(x) in non-conservative form (the color equation)
c
c     # The cell-centered velocity u_i in the i'th cell is stored in aux(i,1)
c
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
      dimension  auxl(1-mbc:maxmx+mbc, 1)
      dimension  auxr(1-mbc:maxmx+mbc, 1)
      dimension    s(1-mbc:maxmx+mbc, mwaves)
      dimension wave(1-mbc:maxmx+mbc, meqn, mwaves)
      dimension amdq(1-mbc:maxmx+mbc, meqn)
      dimension apdq(1-mbc:maxmx+mbc, meqn)
      common /comrp/ u

c
c
c
      do 30 i=2-mbc,mx+mbc
c
c        # Compute the wave and speed
c
         u = dmin1(auxr(i-1,1),0.d0) +  dmax1(auxl(i,1),0.d0)
c
c        # the formula below seems to work as well...
c        u = 0.5d0 * (auxr(i-1,1) +  auxl(i,1))

         wave(i,1,1) = ql(i,1) - qr(i-1,1)
         s(i,1) = u
         amdq(i,1) = dmin1(auxr(i-1,1), 0.d0) * wave(i,1,1)
         apdq(i,1) = dmax1(auxl(i,1),   0.d0) * wave(i,1,1)
   30    continue
c
      return
      end
