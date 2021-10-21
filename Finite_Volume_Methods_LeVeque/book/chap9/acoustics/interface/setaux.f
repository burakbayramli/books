c     ============================================
      subroutine setaux(maxmx,mbc,mx,xlower,dx,maux,aux)
c     ============================================
c
c     # set auxiliary arrays 
c     # variable coefficient acoustics
c     #  aux(i,1) = density rho in i'th cell
c     #  aux(i,2) = sound speed c in i'th cell
c
c     # Piecewise constant medium with single interface at x=0
c     # Density and sound speed to left and right are set in setprob.f
c
c     
      implicit double precision (a-h,o-z)
      dimension aux(1-mbc:maxmx+mbc, 2)
      common /comaux/ rhol,cl,rhor,cr

      open(unit=31,file='fort.aux',status='unknown',form='formatted')
c

       do i=1-mbc,mx+mbc
          xcell = xlower + (i-0.5d0)*dx
          if (xcell .lt. 0.0d0) then
              aux(i,1) = rhol
              aux(i,2) = cl
	    else
              aux(i,1) = rhor
              aux(i,2) = cr
            endif
	  enddo


	do i=1,mx
          write(31,701) aux(i,1), aux(i,2)
  701     format(2e16.6)
          enddo

       close(unit=31)
c
       return
       end
