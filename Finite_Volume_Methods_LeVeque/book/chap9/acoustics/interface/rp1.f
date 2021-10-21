
c
c
c     =====================================================
      subroutine rp1(maxm,meqn,mwaves,mbc,mx,ql,qr,auxl,auxr,
     &			wave,s,amdq,apdq)
c     =====================================================
c
c     # Riemann solver for the acoustics equations in 1d,
c     #  variable coefficients
c
c     # On input, ql contains the state vector at the left edge of each cell
c     #           qr contains the state vector at the right edge of each cell
c
c     # On output, wave contains the waves,
c     #            s the speeds,
c     #
c     #            amdq = A^- Delta q, 
c     #            apdq = A^+ Delta q,
c     #                   the decomposition of the flux difference
c     #                       f(qr(i-1)) - f(ql(i))
c     #                   into leftgoing and rightgoing parts respectively.
c     #
c
c     # Note that the i'th Riemann problem has left state qr(i-1,:)
c     #                                    and right state ql(i,:)
c     # From the basic clawpack routines, this routine is called with ql = qr
c
c
      implicit double precision (a-h,o-z)
c
      dimension auxl(1-mbc:maxm+mbc, 2)
      dimension auxr(1-mbc:maxm+mbc, 2)
      dimension wave(1-mbc:maxm+mbc, meqn, mwaves)
      dimension    s(1-mbc:maxm+mbc, mwaves)
      dimension   ql(1-mbc:maxm+mbc, meqn)
      dimension   qr(1-mbc:maxm+mbc, meqn)
      dimension apdq(1-mbc:maxm+mbc, meqn)
      dimension amdq(1-mbc:maxm+mbc, meqn)
      common /comlim/ mylim,mrplim(2)
c
c     local arrays
c     ------------
      dimension delta(2)
c
c
c     # split the jump in q at each interface into waves
c
c     # find a1 and a2, the coefficients of the 2 eigenvectors:
      do 20 i = 2-mbc, mx+mbc
         delta(1) = ql(i,1) - qr(i-1,1)
         delta(2) = ql(i,2) - qr(i-1,2)

c        # impedances:
	 zi = auxl(i,1)*auxl(i,2)
	 zim = auxl(i-1,1)*auxl(i-1,2)

	 a1 = (-delta(1) + zi*delta(2)) / (zim + zi)
	 a2 =  (delta(1) + zim*delta(2)) / (zim + zi)
c
c        # Compute the waves.
c
         wave(i,1,1) = -a1*zim
         wave(i,2,1) = a1
         s(i,1) = -auxl(i-1,2)
c
         wave(i,1,2) = a2*zi
         wave(i,2,2) = a2
         s(i,2) = auxl(i,2)
c
   20    continue
c
c
c     # compute the leftgoing and rightgoing fluctuations:
c     # Note s(i,1) < 0   and   s(i,2) > 0.
c
      do 220 m=1,meqn
         do 220 i = 2-mbc, mx+mbc
            amdq(i,m) = s(i,1)*wave(i,m,1)
            apdq(i,m) = s(i,2)*wave(i,m,2)
  220       continue
c
      if (mylim .gt. 0) then
c
c        # apply limiter here rather than using default in limiter.f
c
         do 40 i=1,mx
c           # limit 1-wave by looking at wave to right:    
	    a1i = wave(i,2,1)
	    if (a1i .eq. 0.d0) go to 30

	    if (mylim .eq. 1) then
c               # Lax-Liu limiter:
                delta(1) = ql(i+1,1) - qr(i,1)
                delta(2) = ql(i+1,2) - qr(i,2)
	      else if (mylim .eq. 2) then
c               # Transmission-based limiter for acoutics:
                delta(1) = wave(i+1,1,1)
                delta(2) = wave(i+1,2,1)
	      endif

c           # impedances:
      	    zi = auxl(i,1)*auxl(i,2)
	    zim = auxl(i-1,1)*auxl(i-1,2)

	    a1ip1 = (-delta(1) + zi*delta(2)) / (zim + zi)
	    wlimitr = philim(a1i, a1ip1, mrplim(1))
	    wave(i,1,1) = wlimitr * wave(i,1,1)
	    wave(i,2,1) = wlimitr * wave(i,2,1)

   30       continue
c
c           # limit 2-wave by looking at wave to left:    
	    a2i = wave(i,2,2)
	    if (a2i .eq. 0.d0) go to 40

	    if (mylim .eq. 1) then
c               # Lax-Liu limiter:
                delta(1) = ql(i-1,1) - qr(i-2,1)
                delta(2) = ql(i-1,2) - qr(i-2,2)
	      else if (mylim .eq. 2) then
c               # Transmission-based limiter for acoutics:
                delta(1) = wave(i-1,1,2)
                delta(2) = wave(i-1,2,2)
	      endif

c           # impedances:
      	    zi = auxl(i,1)*auxl(i,2)
	    zim = auxl(i-1,1)*auxl(i-1,2)

	    a2im1 =  (delta(1) + zim*delta(2)) / (zim + zi)
	    wlimitr = philim(a2i, a2im1, mrplim(2))
	    wave(i,1,2) = wlimitr * wave(i,1,2)
	    wave(i,2,2) = wlimitr * wave(i,2,2)
   40       continue
         endif


      return
      end
