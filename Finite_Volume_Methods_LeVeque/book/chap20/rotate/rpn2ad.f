c
c
c     =====================================================
      subroutine rpn2(ixy,maxm,meqn,mwaves,mbc,mx,ql,qr,
     &			auxl,auxr,wave,s,amdq,apdq)
c     =====================================================
c
c     # Riemann-solver for the advection equation
c     #    q_t  +  u*q_x + v*q_y = 0
c     # where u and v are a given velocity field.
c
c       -----------------------------------------------------------
c     # In advective form, with interface velocities specified by auxl.
c       -----------------------------------------------------------
c
c     # (u,v) may depend of (x,y,t), and the common block comxyt
c     # is used to determine the value of t and either x or y,
c     # depending on what direction the slice is along.
c
c     # solve Riemann problems along one slice of data.
c     # This data is along a slice in the x-direction if ixy=1
c     #                            or the y-direction if ixy=2.
c
c     # On input, ql contains the state vector at the left edge of each cell
c     #           qr contains the state vector at the right edge of each cell
c
c     # On output, wave contains the waves, s the speeds, 
c     # and amdq, apdq the left-going and right-going flux differences,
c     # respectively.  Note that in this advective form, the sum of
c     # amdq and apdq is not equal to a difference of fluxes except in the
c     # case of constant velocities.
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
      dimension apdq(1-mbc:maxm+mbc, meqn)
      dimension amdq(1-mbc:maxm+mbc, meqn)
      dimension auxl(1-mbc:maxm+mbc, *)
      dimension auxr(1-mbc:maxm+mbc, *)
c
c
c     # Set wave, speed, and flux differences:
c     ------------------------------------------
c
      do 30 i = 2-mbc, mx+mbc
         wave(i,1,1) = ql(i,1) - qr(i-1,1)
	 s(i,1) = auxl(i,ixy)
c        # The flux difference df = s*wave  all goes in the downwind direction:
	 amdq(i,1) = dmin1(auxl(i,ixy), 0.d0) * wave(i,1,1)
	 apdq(i,1) = dmax1(auxl(i,ixy), 0.d0) * wave(i,1,1)
   30    continue
c
      return
      end
