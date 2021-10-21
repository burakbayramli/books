c
c
c =========================================================
       subroutine qinit(maxmx,meqn,mbc,mx,xlower,dx,q,maux,aux)
c =========================================================
c
c     # Set initial conditions for q.
c
c
      implicit double precision (a-h,o-z)
      dimension q(1-mbc:maxmx+mbc, meqn)
      dimension aux(1-mbc:maxmx+mbc, *)
c
c
      ql = -1.d0
      qr =  2.d0

      do 150 i=1,mx
c        # left and right edge of i'th cell:
	 xl = xlower + (i-1)*dx
	 xr = xl+dx

	 if (xl .ge. 0.d0) then
	     q(i,1) = qr
	   else if (xr .le. 0.d0) then
	     q(i,1) = ql
	   else
c            #  xl < 0 < xr and the cell average is weighted combo of ql,qr:
	     q(i,1) = (-xl*ql + xr*qr) / dx
	   endif
  150    continue
c
      return
      end
