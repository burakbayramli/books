c
c
c     =====================================================
      subroutine rpn2(ixy,maxm,meqn,mwaves,mbc,mx,ql,qr,
     &                  auxl,auxr,wave,s,amdq,apdq)
c     =====================================================
c
c     # Riemann solver for the 2d variable-coefficient acoustics equations
c     # on a general quadrilateral grid.
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
c     # solve Riemann problems along one slice of data.
c
c     # On input, ql contains the state vector at the left edge of each cell
c     #           qr contains the state vector at the right edge of each cell
c
c     # This data is along a slice in the x-direction if ixy=1 
c     #                            or the y-direction if ixy=2.
c     # On output, wave contains the waves, s the speeds, 
c     # and amdq, apdq the decomposition of the flux difference
c     #   f(qr(i-1)) - f(ql(i))  
c     # into leftgoing and rightgoing parts respectively.
c     # With the Roe solver we have   
c     #    amdq  =  A^- \Delta q    and    apdq  =  A^+ \Delta q
c     # where A is the Roe matrix.  An entropy fix can also be incorporated
c     # into the flux differences.
c
c     # Note that the i'th Riemann problem has left state qr(i-1,:)
c     #                                    and right state ql(i,:)
c     # From the basic clawpack routines, this routine is called with ql = qr
c
c
      implicit double precision (a-h,o-z)
c
      dimension wave(1-mbc:maxm+mbc, meqn, mwaves)
      dimension    s(1-mbc:maxm+mbc, mwaves)
      dimension   ql(1-mbc:maxm+mbc, meqn)
      dimension   qr(1-mbc:maxm+mbc, meqn)
      dimension  apdq(1-mbc:maxm+mbc, meqn)
      dimension  amdq(1-mbc:maxm+mbc, meqn)
      dimension auxl(1-mbc:maxm+mbc, 9)
      dimension auxr(1-mbc:maxm+mbc, 9)
c
c     ------------
      dimension delta(3)
c
c     # The normal vector for the face at the i'th Riemann problem
c     # is stored in the aux array
c     # in locations (1,2) if ixy=1 or (4,5) if ixy=2.  The ratio of the
c     # length of the cell side to the length of the computational cell
c     # is stored in aux(3) or aux(6) respectively.
c     # The normal vector is called (anx,any) below and the jump in 
c     # the normal velocity is deltavel.
c
c
      if (ixy.eq.1) then
          inx = 1
          iny = 2
          ilenrat = 3
        else
          inx = 4
          iny = 5
          ilenrat = 6
        endif
c
c
      do 10 i = 2-mbc, mx+mbc
         anx = auxl(i,inx)
         any = auxl(i,iny)
         delta(1) = ql(i,1) - qr(i-1,1)
         delta(2) = ql(i,2) - qr(i-1,2)
         delta(3) = ql(i,3) - qr(i-1,3)
         deltavel = anx*delta(2) + any*delta(3)
c
c        # impedance and sound speed in adjacent cells:
         zi = auxl(i,8)
         zim = auxl(i-1,8)
         ci = auxl(i,9)
         cim = auxl(i-1,9)

c        # wave strengths:
         a1 = (-delta(1) + zim*deltavel) / (zim+zi)
         a2 = (delta(1) + zi*deltavel) / (zim+zi)
c
c        # Compute the waves.
c
         wave(i,1,1) = -a1*zim
         wave(i,2,1) = a1 * anx
         wave(i,3,1) = a1 * any
         s(i,1) = -cim * auxl(i,ilenrat)
c
         wave(i,1,2) = a2*zi
         wave(i,2,2) = a2 * anx
         wave(i,3,2) = a2 * any
         s(i,2) = ci * auxl(i,ilenrat)
   10    continue        

c     # compute the leftgoing and rightgoing flux differences:
c     # Note s(i,1) < 0   and   s(i,2) > 0.
c
      do 220 m=1,meqn
         do 220 i = 2-mbc, mx+mbc
            amdq(i,m) = s(i,1)*wave(i,m,1)
            apdq(i,m) = s(i,2)*wave(i,m,2)
  220       continue
c
      return
      end
