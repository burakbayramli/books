c     ============================================
      subroutine setaux(maxmx,maxmy,mbc,mx,my,xlower,ylower,dx,dy,
     &                  maux,aux)
c     ============================================
c
c     # set auxiliary arrays 
c     # variable coefficient acoustics
c     #  aux(i,j,1) = density rho in (i,j) cell
c     #  aux(i,j,2) = lambda in (i,j) cell
c     #  aux(i,j,3) = mu in (i,j) cell
c     #  aux(i,j,4) = cp in (i,j) cell
c     #  aux(i,j,5) = cs in (i,j) cell
c
c     # Piecewise constant medium
c     # Material parameters are set in setprob.f

c
c     
      implicit double precision (a-h,o-z)
      dimension aux(1-mbc:maxmx+mbc,1-mbc:maxmy+mbc, 5)
      common /comaux/ rho1,amu1,alam1,rho2,amu2,alam2
c

      do 30 j=1-mbc,my+mbc
       do 20 i=1-mbc,mx+mbc
          xl = xlower + (i-1.0d0)*dx
          yl = ylower + (j-1.0d0)*dy
          call cellave(xl,yl,dx,dy,w1)
          w2 = 1.d0 - w1

          aux(i,j,1) = w1*rho1 + w2*rho2
          aux(i,j,2) = w1*alam1 + w2*alam2
          aux(i,j,3) = w1*amu1 + w2*amu2
          bulk       = aux(i,j,2) + 2.d0*aux(i,j,3)
          aux(i,j,4) = dsqrt(bulk/aux(i,j,1))
          aux(i,j,5) = dsqrt(aux(i,j,3)/aux(i,j,1))

   20     continue
   30    continue

       return
       end
