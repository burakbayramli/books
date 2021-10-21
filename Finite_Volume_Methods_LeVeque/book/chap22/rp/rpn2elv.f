c
c
c     =====================================================
      subroutine rpn2(ixy,maxm,meqn,mwaves,mbc,mx,ql,qr,
     &			auxl,auxr,wave,s,amdq,apdq)
c     =====================================================
c
c     # Riemann solver for the elasticity equations in 2d, with varying
c     # material properties rho, lambda, and mu 
c
c     # Note that although there are 5 eigenvectors, one eigenvalue
c     # is always zero and so we only need to compute 4 waves.	
c     # 
c     # solve Riemann problems along one slice of data.
c
c     # On input, ql contains the state vector at the left edge of each cell
c     #           qr contains the state vector at the right edge of each cell
c
c     # Note that the i'th Riemann problem has left state qr(i-1,:)
c     #                                    and right state ql(i,:)
c     # From the basic clawpack routines, this routine is called with ql = qr
c
c     # This data is along a slice in the x-direction if ixy=1 
c     #                            or the y-direction if ixy=2.
c
c     # Contents of ql and qr:
c     # 
c     # q(:,1) = sigma^{11} if ixy=1   or   sigma^{22} if ixy=2
c     # q(:,2) = sigma^{22} if ixy=1   or   sigma^{11} if ixy=2
c     # q(:,3) = sigma^{12} = sigma^{21}
c     # q(:,4) = u          if ixy=1   or   v          if ixy=2
c     # q(:,5) = v          if ixy=1   or   u          if ixy=2
c     # 
c     # auxl and auxr hold corresponding slice of the aux array:
c     # Here it is assumed that auxl=auxr gives the cell values
c     # for this slice.
c     # 
c     #  auxl(i,1) = rho, density
c     #  auxl(i,2) = lambda 
c     #  auxl(i,3) = mu
c     #  auxl(i,4) = cp, P-wave speed 
c     #  auxl(i,5) = cs, S-wave speed 
c
c
c     # On output, wave contains the waves,
c     #            s the speeds,
c     #            amdq the  left-going flux difference  A^- \Delta q
c     #            apdq the right-going flux difference  A^+ \Delta q
c
c     # Note that the waves are *not* in order of increasing lambda.
c     # Instead the 1- and 2-waves are the P-waves and the 3- and 4-waves
c     # are the S-waves.   (The 5th wave has speed zero and is not used.)
c
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
      dimension auxl(1-mbc:maxm+mbc, 5)
      dimension auxr(1-mbc:maxm+mbc, 5)
c
c
c     # set ku to point to  the component of the system that corresponds
c     # to velocity in the direction of this slice, kv to the orthogonal
c     # velocity.  Similarly ksig11 and ksig22 point to normal stresses.
c     # 3rd component is always shear stress sig12.
c
c
      if (ixy.eq.1) then
         ksig11 = 1
         ksig22 = 2
         ku = 4
         kv = 5
	else
         ksig11 = 2
         ksig22 = 1
         ku = 5
         kv = 4
	endif
c
c     # note that notation for u and v reflects assumption that the 
c     # Riemann problems are in the x-direction with u in the normal
c     # direciton and v in the orthogonal direcion, but with the above
c     # definitions of ku and kv the routine also works with ixy=2
c
c
c     # split the jump in q at each interface into waves
c     # The jump is split into leftgoing waves traveling at speeds -cp, -cs
c     # relative to the material properties to the left of the interface,
c     # and rightgoing waves traveling at speeds +cp, +cs
c     # relative to the material properties to the right of the interface,
c
      do 20 i = 2-mbc, mx+mbc
         dsig11 = ql(i,ksig11) - qr(i-1,ksig11)
         dsig22 = ql(i,ksig22) - qr(i-1,ksig22)
         dsig12 = ql(i,3) - qr(i-1,3)
         du = ql(i,ku) - qr(i-1,ku)
         dv = ql(i,kv) - qr(i-1,kv)

c        # material properties in cells i (on right) and i-1 (on left):

	 alamr = auxl(i,2)
         amur = auxl(i,3)
	 bulkr = alamr + 2.d0*amur
	 cpr = auxl(i,4)
	 csr = auxl(i,5)

	 alaml = auxr(i-1,2)
         amul = auxr(i-1,3)
	 bulkl = alaml + 2.d0*amul
	 cpl = auxr(i-1,4)
	 csl = auxr(i-1,5)

c        # P-wave strengths:
c
	 det = bulkl*cpr + bulkr*cpl
	 if (det.eq.0.d0) then
	    write(6,*) 'det=0 in rpn2'
	    stop 
	    endif
         a1 = (cpr*dsig11 + bulkr*du) / det
         a2 = (cpl*dsig11 - bulkl*du) / det

c        # S-wave strengths:
c
	 det = amul*csr + amur*csl
	 if (det.eq.0.d0) then
c            # no s-waves
	     a3 = 0.d0
	     a4 = 0.d0
	   else
	     a3 = (csr*dsig12 + amur*dv) / det
	     a4 = (csl*dsig12 - amul*dv) / det
	   endif
c
c        # 5th wave has velocity 0 so is not computed or propagated.
c
c
c        # Compute the waves.
c
         wave(i,ksig11,1) = a1 * bulkl
         wave(i,ksig22,1) = a1 * alaml
         wave(i,3,1)  = 0.d0
         wave(i,ku,1) = a1 * cpl
         wave(i,kv,1) = 0.d0
         s(i,1) = -cpl
c
         wave(i,ksig11,2) = a2 * bulkr
         wave(i,ksig22,2) = a2 * alamr
         wave(i,3,2)  = 0.d0
         wave(i,ku,2) = -a2 * cpr
         wave(i,kv,2) = 0.d0
         s(i,2) = cpr
c
         wave(i,ksig11,3) = 0.d0
         wave(i,ksig22,3) = 0.d0
         wave(i,3,3)  = a3*amul
         wave(i,ku,3) = 0.d0
         wave(i,kv,3) = a3*csl
         s(i,3) = -csl
c
         wave(i,ksig11,4) = 0.d0
         wave(i,ksig22,4) = 0.d0
         wave(i,3,4)  = a4*amur
         wave(i,ku,4) = 0.d0
         wave(i,kv,4) = -a4*csr
         s(i,4) = csr
c
   20    continue
c
c
c
c     # compute the leftgoing and rightgoing flux differences:
c     # Note s(i,1),s(i,3) < 0   and   s(i,2),s(i,4) > 0.
c
      do 220 m=1,meqn
	 do 220 i = 2-mbc, mx+mbc
	    amdq(i,m) = s(i,1)*wave(i,m,1) + s(i,3)*wave(i,m,3)
	    apdq(i,m) = s(i,2)*wave(i,m,2) + s(i,4)*wave(i,m,4)
  220       continue
c
      return
      end
