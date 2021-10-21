c
c
c =========================================================
       subroutine qinit(maxmx,meqn,mbc,mx,xlower,dx,q,maux,aux)
c =========================================================
c
c     # Set initial conditions for q.
c     # shocktube with states set in setprob.f
c
c
      implicit double precision (a-h,o-z)
      dimension q(1-mbc:maxmx+mbc, meqn)
      dimension aux(1-mbc:maxmx+mbc, *)
      common /param/ gamma, gamma1
      common /comic/ rhol,rhoul,el,rhor,rhour,er
c
c
c
       sloc = 0.0d0
c
       do 150 i=1,mx
	     xcell = xlower + (i-0.5d0)*dx
             if (xcell .lt. sloc) then
                 q(i,1) = rhol
                 q(i,2) = rhoul
                 q(i,3) = el
                else
                 q(i,1) = rhor
                 q(i,2) = rhour
                 q(i,3) = er
                endif
  150        continue
c
c
      return
      end
