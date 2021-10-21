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
      common /comic/ beta
c
c
      ql = 0.13d0
      qr = 0.1d0

      do 150 i=1,mx
	 xcell = xlower + (i-0.5d0)*dx
	 if (xcell .lt. 0.d0) then
 	     q(i,1) = ql
	   else
	     q(i,1) = qr
	   endif

  150    continue
c
      return
      end
