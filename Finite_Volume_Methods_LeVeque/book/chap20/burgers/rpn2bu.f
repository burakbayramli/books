
c
c
c     =====================================================
      subroutine rpn2(ixy,maxm,meqn,mwaves,mbc,mx,ql,qr,auxl,auxr,
     &                  wave,s,amdq,apdq)
c     =====================================================
c
c     # Riemann solver for Burgers' equation in 2d:
c     #  u_t + cos(theta)*(0.5*u^2)_x + sin(theta)*(0.5*u^2)_y = 0
c     
c     # On input, ql contains the state vector at the left edge of each cell
c     #           qr contains the state vector at the right edge of each cell
c
c     # This data is along a slice in the x-direction if ixy=1 
c     #                            or the y-direction if ixy=2.
c     # On output, wave contains the waves,
c     #            s the speeds,
c     #            amdq the  left-going flux difference  A^- \Delta q
c     #            apdq the right-going flux difference  A^+ \Delta q
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
      logical efix
      common /comrp/ theta
c
c
      if (ixy .eq. 1) then
          a = 0.5d0*dcos(theta)
        else
          a = 0.5d0*dsin(theta)
        endif
c
      efix = .true.
c
      do 10 i = 2-mbc, mx+mbc
c        # wave is jump in q, speed comes from R-H condition:
         wave(i,1,1) = ql(i,1) - qr(i-1,1)
         s(i,1) = a*(qr(i-1,1) + ql(i,1))
c
c        # compute left-going and right-going flux differences:
c        ------------------------------------------------------
c
         amdq(i,1) = dmin1(s(i,1), 0.d0) * wave(i,1,1)
         apdq(i,1) = dmax1(s(i,1), 0.d0) * wave(i,1,1)
c
         if (efix) then
c           # entropy fix for transonic rarefactions:
            if (qr(i-1,1).lt.0.d0 .and. ql(i,1).gt.0.d0) then
               amdq(i,1) = - a*qr(i-1,1)**2
               apdq(i,1) =   a*ql(i,1)**2
               endif
            endif
   10   continue
c
      return
      end
