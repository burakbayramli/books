c     ============================================
      subroutine setaux(maxmx,mbc,mx,xlower,dx,maux,aux)
c     ============================================
c
c     # set auxiliary arrays 
c     # variable coefficient acoustics
c     #  aux(i,1) = impedance Z in i'th cell
c     #  aux(i,2) = sound speed c in i'th cell
c
c
c     # rho value for i-1 < x < i is determined by rho(i)
c     # input from setprob.rho in setprob.f
c
c     
      implicit double precision (a-h,o-z)
      dimension aux(1-mbc:maxmx+mbc, 2)
      common /comaux/ rho(1000)

      open(unit=31,file='fort.aux',status='unknown',form='formatted')
c

       do i=1-mbc,mx+mbc
          xcell = xlower + (i-0.5d0)*dx
c         # truncate to an integer to determine which layer xcell is in:
	  ix = xcell + 1
	  if (ix.lt.1) ix = 1
	  aux(i,2) = 1.d0
 	  aux(i,1) = rho(ix)*aux(i,2)
c         # uniform for comparison:
c	  aux(i,1) = aux(i,2)
	  enddo


	do i=1,mx
          write(31,701) aux(i,1), aux(i,2)
  701     format(2e16.6)
          enddo

       close(unit=31)
c
       return
       end
