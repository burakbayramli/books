
c
c
c =========================================================
      subroutine src1(maxmx,meqn,mbc,mx,xlower,dx,q,maux,aux,t,dt)
c =========================================================
      implicit double precision (a-h,o-z)
      dimension q(1-mbc:maxmx+mbc, meqn)
      common /comsrc/ xramp,alf
c
c     delta-function source term for on-ramp
c
      do i=1,mx
	xleft = xlower + (i-1)*dx
        if (xramp.ge.xleft .and. xramp.lt.(xleft+dx)) then
            q(i,1) = q(i,1) + alf*dt/dx
	    endif
        enddo
       
      return
      end
