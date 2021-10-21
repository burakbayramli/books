c
c
c ===================================================================
      subroutine trlimit(maxmx,meqn,mwaves,mbc,mx,aux,
     &                   wave,s,mthlim)
c ===================================================================
c
c     # Transmission-based limiter for acoustics equations
c     #    See Fogarty and LeVeque paper for a description.
c
c     # Use in place of limiter.f
c
c     # Assumes 
c     #    aux(i,1) = sound speed c in i'th cell
c     #    aux(i,2) = impedance Z in i'th cell
c     #    wave(i,*,1) is the left-going wave 
c     #      = alf_i^1 [Z_{i-1} ; 1]
c     #    wave(i,*,2) is the right-going wave 
c     #      = alf_i^2 [Z_i ; 1]
c     # 
c
c     --------------------------------------------------------------------
c
      implicit double precision (a-h,o-z)
      dimension  aux(1-mbc:maxmx+mbc, *)
      dimension    s(1-mbc:maxmx+mbc, mwaves)
      dimension wave(1-mbc:maxmx+mbc, meqn, mwaves)
      dimension mthlim(mwaves)
c
c
c     # transmission-based limiter:
c     # use the fact that second component of wave is strength alpha since
c     # second component of eigenvector is 1.
c
       do 100 i=0,mx+1
c          # 1-wave at this cell and neighbor:
	   alf1i = wave(i,2,1)
	   if (alf1i .ne. 0.d0) then
	      alf1ip = wave(i+1,2,1)
c             # transmitted part of neighboring 1-wave:
	      alf1ipt = (2.d0*aux(i,1)/(aux(i-1,1)+aux(i,1))) * alf1ip
	      wlimitr = philim(alf1i, alf1ipt, mthlim(1))
	      do m=1,meqn
	         wave(i,m,1) = wlimitr * wave(i,m,1)
	         enddo
	      endif

c          # 2-wave at this cell and neighbor:
	   alf2i = wave(i,2,2)
	   if (alf2i .ne. 0.d0) then
	      alf2im = wave(i-1,2,2)
c             # transmitted part of neighboring 2-wave:
  	      alf2imt = (2.d0*aux(i-1,1)/(aux(i-1,1)+aux(i,1))) * alf2im
  	      wlimitr = philim(alf2i, alf2imt, mthlim(2))
	      do m=1,meqn
	         wave(i,m,2) = wlimitr * wave(i,m,2)
	         enddo
	      endif
  100      continue
c
      return
      end
