c
c
c     =====================================================
      subroutine rpt2(ixy,maxm,meqn,mwaves,mbc,mx,ql,qr,
     &                   aux1,aux2,aux3,imp,asdq,bmasdq,bpasdq)
c     =====================================================
      implicit double precision (a-h,o-z)
c
c     # Riemann solver in the transverse direction for the 2d acoustics 
c     # equations on a quadrilateral grid.
c
c     # Split asdq (= A^* \Delta q, where * = + or -)
c     # into down-going flux difference bmasdqb (= B^- A^* \Delta q)
c     #    and up-going flux difference bpasdq (= B^+ A^* \Delta q)
c
c
      dimension     ql(1-mbc:maxm+mbc, meqn)
      dimension     qr(1-mbc:maxm+mbc, meqn)
      dimension   asdq(1-mbc:maxm+mbc, meqn)
      dimension bmasdq(1-mbc:maxm+mbc, meqn)
      dimension bpasdq(1-mbc:maxm+mbc, meqn)
      dimension   aux1(1-mbc:maxm+mbc, 9)
      dimension   aux2(1-mbc:maxm+mbc, 9)
      dimension   aux3(1-mbc:maxm+mbc, 9)
c
c
      if (ixy.eq.1) then
          ilenrat = 3
          ixtran = 4
          iytran = 5
        else
          ilenrat = 6
          ixtran = 1
          iytran = 2
        endif
c 
c
      do 20 i=2-mbc,mx+mbc

c        # impedance and sound speed in adjacent cells:
         zim = aux1(i,8)
         zi = aux2(i,8)
         zip = aux3(i,8)
         cim = aux1(i,9)
         ci = aux2(i,9)
         cip = aux3(i,9)
c
c        # pressure component of asdq:
c
         asdqp = asdq(i,1) 
c
c
c        # up-going:
c        -----------
c
         xtran = aux3(i,ixtran)
         ytran = aux3(i,iytran)
c
c        # transverse velocity component of asdq:
         asdqt = xtran*asdq(i,2) + ytran*asdq(i,3)
         b2 = (asdqp + zi*asdqt) / (zi+zip)
         bpasdqp = b2*zip
         bpasdqt = b2

c
         bpasdq(i,1) = cip * bpasdqp
         bpasdq(i,2) = cip * xtran*bpasdqt 
         bpasdq(i,3) = cip * ytran*bpasdqt 
c
c
c        # down-going:
c        -------------
c
         xtran = aux2(i,ixtran)
         ytran = aux2(i,iytran)
c
c        # transverse velocity component of asdq:
         asdqt = xtran*asdq(i,2) + ytran*asdq(i,3)
         b1 = (-asdqp + zi*asdqt) / (zim+zi)
         bmasdqp = -b1*zim
         bmasdqt = b1
c
         bmasdq(i,1) = -cim * bmasdqp
         bmasdq(i,2) = -cim * xtran*bmasdqt 
         bmasdq(i,3) = -cim * ytran*bmasdqt 

c
c        # multiply by ratio of lengths:
         do m=1,3
            bmasdq(i,m) = bmasdq(i,m)*aux2(i,9-ilenrat)
            bpasdq(i,m) = bpasdq(i,m)*aux3(i,9-ilenrat)
            enddo
   20    continue
      return
      end
