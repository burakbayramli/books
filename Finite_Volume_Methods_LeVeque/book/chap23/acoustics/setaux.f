c     ============================================
      subroutine setaux(maxmx,maxmy,mbc,mx,my,xlower,ylower,dxc,dyc,
     &                  maux,aux)
c     ============================================
c
c
c     #    aux(i,j,1)  = ax
c     #    aux(i,j,2)  = ay   where (ax,ay) is unit normal to left face
c     #    aux(i,j,3)  = ratio of length of left face to dyc
c
c     #    aux(i,j,4)  = bx
c     #    aux(i,j,5)  = by   where (bx,by) is unit normal to bottom face
c     #    aux(i,j,6)  = ratio of length of bottom face to dxc
c
c     #    aux(i,j,7)  = ratio of cell area to dxc*dyc
c     #                  (approximately Jacobian of mapping function)
c
c     #    aux(i,j,8)  = impedance Z in cell (i,j)
c     #    aux(i,j,9)  = sound speed c in cell (i,j)
c
c     
      implicit double precision (a-h,o-z)
      dimension aux(1-mbc:maxmx+mbc,1-mbc:maxmy+mbc, 9)
      dimension xccorn(5),yccorn(5),xpcorn(5),ypcorn(5)
      common /cparam/ rho,bulk,cc,zz
c

      dx2 = dxc/2.d0
      dy2 = dyc/2.d0
c
      do 20 j=1-mbc,my+mbc
         do 20 i=1-mbc,mx+mbc
c
c           # computational points (xc,yc) are mapped to physical
c           # coordinates (xp,yp) by mapc2p:
c
c           # lower left corner:
            xccorn(1) = xlower + (i-1)*dxc
            yccorn(1) = ylower + (j-1)*dyc
            call mapc2p(xccorn(1),yccorn(1),xpcorn(1),ypcorn(1))

c           # upper left corner:
            xccorn(2) = xccorn(1)
            yccorn(2) = yccorn(1) + dyc
            call mapc2p(xccorn(2),yccorn(2),xpcorn(2),ypcorn(2))
c
c           # upper right corner:
            xccorn(3) = xccorn(1) + dxc
            yccorn(3) = yccorn(1) + dyc
            call mapc2p(xccorn(3),yccorn(3),xpcorn(3),ypcorn(3))
c
c           # lower right corner:
            xccorn(4) = xccorn(1) + dxc
            yccorn(4) = yccorn(1)
            call mapc2p(xccorn(4),yccorn(4),xpcorn(4),ypcorn(4))
c
c           # compute normals to left and bottom side:
c
            ax =  (ypcorn(2) - ypcorn(1))
            ay = -(xpcorn(2) - xpcorn(1))
            anorm = dsqrt(ax*ax + ay*ay)
            aux(i,j,1) = ax/anorm
            aux(i,j,2) = ay/anorm
            aux(i,j,3) = anorm/dyc
c
            bx = -(ypcorn(4) - ypcorn(1))
            by =  (xpcorn(4) - xpcorn(1))
            bnorm = dsqrt(bx*bx + by*by)
            aux(i,j,4) = bx/bnorm
            aux(i,j,5) = by/bnorm
            aux(i,j,6) = bnorm/dxc
c
c           # compute area of physical cell from four corners:

            xpcorn(5) = xpcorn(1)
            ypcorn(5) = ypcorn(1)
            area = 0.d0
            do ic=1,4
              area = area + 0.5d0 * (ypcorn(ic)+ypcorn(ic+1)) *
     &               (xpcorn(ic+1)-xpcorn(ic))
              enddo
            aux(i,j,7) = area / (dxc*dyc)
c
            aux(i,j,8) = zz
            aux(i,j,9) = cc

   20       continue
c
       return

       end
