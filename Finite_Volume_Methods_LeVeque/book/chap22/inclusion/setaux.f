c     ============================================
      subroutine setaux(maxmx,maxmy,mbc,mx,my,xlower,ylower,dx,dy,
     &                  maux,aux)
c     ============================================
c
c     # set auxiliary arrays 
c     # variable coefficient elasticity
c     #  aux(i,j,1) = density rho in (i,j) cell
c     #  aux(i,j,2) = lambda in (i,j) cell
c     #  aux(i,j,3) = mu in (i,j) cell
c     #  aux(i,j,4) = cp in (i,j) cell
c     #  aux(i,j,5) = cs in (i,j) cell
c     #  aux(i,j,6) = xdisp in (i,j) cell
c     #  aux(i,j,7) = ydisp in (i,j) cell
c
c     # Piecewise constant medium
c     # Material parameters are set in setprob.f
c
c     # Note: aux(..6) and aux(..7) are used to store the x- and y-
c     # displacements.  These are initialized to zero here and updated each
c     # time step in b4step2.  These routines were modified for Version 4.3
c     # of clawpack.  The previous version kept xdisp and ydisp in a 
c     # separate common block.
c
c     
      implicit double precision (a-h,o-z)
      dimension aux(1-mbc:maxmx+mbc,1-mbc:maxmy+mbc, 7)
      common /comaux/ rho1,amu1,alam1,rho2,amu2,alam2

c
c     # displacements:

      do i=0,mx+1
         do j=0,my+1
            aux(i,j,6) = 0.d0
            aux(i,j,7) = 0.d0
            enddo
         enddo


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
