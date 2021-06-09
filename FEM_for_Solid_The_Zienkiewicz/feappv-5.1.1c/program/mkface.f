!$Id:$
      subroutine mkface(iblend,lblend)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Form faces for 3-d Block

!      Inputs:

!      Outputs:
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer   iblend(*),lblend(20,*), lface(4,6)
      integer   i,j

      save

      data  lface/  1, 2, 3, 4,   5, 6, 7, 8,   1, 4, 8, 5,
     &              2, 3, 7, 6,   1, 5, 6, 2,   4, 8, 7, 3/

!     Assign generation increments for faces

      lblend(1,1) = iblend(1)
      lblend(2,1) = iblend(2)
      lblend(3,1) = 1

      lblend(1,2) = iblend(1)
      lblend(2,2) = iblend(2)
      lblend(3,2) = 1

      lblend(1,3) = iblend(2)
      lblend(2,3) = iblend(3)
      lblend(3,3) = 1

      lblend(1,4) = iblend(2)
      lblend(2,4) = iblend(3)
      lblend(3,4) = 1

      lblend(1,5) = iblend(3)
      lblend(2,5) = iblend(1)
      lblend(3,5) = 1

      lblend(1,6) = iblend(3)
      lblend(2,6) = iblend(1)
      lblend(3,6) = 1

!     Assign side numbers to faces

      do i = 1,6
        do j = 1,4
          lblend(j+10,i) = iblend(10+lface(j,i))
        end do
      end do

      end subroutine mkface

      subroutine pblend3x(x1,nr1,ns1, x2,nr2,ns2, x3,nr3,ns3,
     &                    x4,nr4,ns4, x5,nr5,ns5, x6,nr6,ns6,
     &                    nty,x,ndm,iblend, nf)

      implicit  none

      integer   nr1,ns1, nr2,ns2, nr3,ns3, nr4,ns4, nr5,ns5, nr6,ns6
      integer   ndm,nf
      integer   iblend(*),nty(*)
      real*8    x1(3,0:nr1,0:ns1), x2(3,0:nr2,0:ns2), x3(3,0:nr3,0:ns3)
      real*8    x4(3,0:nr4,0:ns4), x5(3,0:nr5,0:ns5), x6(3,0:nr6,0:ns6)
      real*8    xl(3,8),sh(2,3),x(ndm,*)

      integer   nr,ns,nt,j
      real*8    r,s,t, dr,ds,dt

      save

      dr = 2.d0/dble(iblend(3))
      ds = 2.d0/dble(iblend(1))
      dt = 2.d0/dble(iblend(2))

      do j = 1,ndm
        xl(j,1) = x5(j,  0,  0)
        xl(j,2) = x5(j,nr5,  0)
        xl(j,3) = x5(j,nr5,ns5)
        xl(j,4) = x5(j,  0,ns5)
        xl(j,5) = x6(j,  0,  0)
        xl(j,6) = x6(j,nr6,  0)
        xl(j,7) = x6(j,nr6,ns6)
        xl(j,8) = x6(j,  0,ns6)
      end do

      nf = iblend(4) - 1
      t  = -1.d0
      do nt = 0,iblend(2)
        sh(1,3) = 0.5d0 - 0.5d0*t
        sh(2,3) = 0.5d0 + 0.5d0*t
        s = -1.d0
        do ns = 0,iblend(1)
          sh(1,2) = 0.5d0 - 0.5d0*s
          sh(2,2) = 0.5d0 + 0.5d0*s
          r = -1.d0
          do nr = 0,iblend(3)
            sh(1,1) = 0.5d0 - 0.5d0*r
            sh(2,1) = 0.5d0 + 0.5d0*r
            nf      = nf + 1
            nty(nf) = 0
            do j = 1,ndm
              x(j,nf) =(x1(j,ns,nt)*sh(1,1) + x2(j,ns,nt)*sh(2,1)
     &                + x3(j,nt,nr)*sh(1,2) + x4(j,nt,nr)*sh(2,2)
     &                + x5(j,nr,ns)*sh(1,3) + x6(j,nr,ns)*sh(2,3))*0.5d0
            end do
            call xiso3d(r,s,t,xl,x(1,nf))
            r  = r  + dr
          end do ! nr
          s = s + ds
        end do ! ns
        t = t + dt
      end do ! nt

      end subroutine pblend3x

      subroutine xiso3d(xi1,xi2,xi3,xl,xx)

      implicit  none

      real*8    xi1,xi2,xi3, shp(8),xl(3,8), xx(3)

      integer   i,j
      real*8    sh1m,sh2m,sh3m,sh1p,sh2p,sh3p

      save

!     Constant parameters for 8-node shape functions

      sh1m = 0.25d0 - 0.25d0*xi1
      sh1p = 0.25d0 + 0.25d0*xi1
      sh2m = 0.50d0 - 0.50d0*xi2
      sh2p = 0.50d0 + 0.50d0*xi2
      sh3m = 0.50d0 - 0.50d0*xi3
      sh3p = 0.50d0 + 0.50d0*xi3

!     Form shape functions

      shp(1) = sh1m*sh2m*sh3m
      shp(2) = sh1p*sh2m*sh3m
      shp(3) = sh1p*sh2p*sh3m
      shp(4) = sh1m*sh2p*sh3m
      shp(5) = sh1m*sh2m*sh3p
      shp(6) = sh1p*sh2m*sh3p
      shp(7) = sh1p*sh2p*sh3p
      shp(8) = sh1m*sh2p*sh3p

!     Subtract 2 x isoparametric interpolation coordinate

      do j = 1,3
        do i = 1,8
          xx(j) = xx(j) - shp(i)*xl(j,i)
        end do
      end do

      end subroutine xiso3d

      subroutine mkside(n,iface,is,isd)

      implicit none

      include  'iofile.h'

      integer   n,isd,is(isd,*),iface(4)
      integer   i,j,k, i1,j1, ie,je

      save

!     Check first side for correct direction

      i1 = abs(iface(1))
      j1 = abs(iface(2))
      if(is(1,i1).eq.2) then
        do k = 3,isd
          if(is(k,i1).ne.0) ie = k
        end do
      else
        ie = 3
      endif
      if(is(1,j1).eq.2) then
        do k = 3,isd
          if(is(k,j1).ne.0) je = k
        end do
      else
        je = 3
      endif
      if(is(ie,i1).eq.is(2,j1)     .or. is(ie,i1).eq.is(je,j1)) then
        iface(1) =  i1
      elseif(is(2,i1).eq.is(2,j1) .or. is(2,i1).eq.is(je,j1)) then
        iface(1) = -i1
      else
        write(iow,2000) n,1,2
      endif

!     Check remaining directions

      do i = 1,3
        j  = i + 1
        i1 = abs(iface(i))
        j1 = abs(iface(j))
        if(is(1,i1).eq.2) then
          do k = 3,isd
            if(is(k,i1).ne.0) ie = k
          end do
        else
          ie = 3
        endif
        if(is(1,j1).eq.2) then
          do k = 3,isd
            if(is(k,j1).ne.0) je = k
          end do
        else
          je = 3
        endif
        if(iface(i).gt.0) then
          if(is(ie,i1).eq.is(2,j1)) then
            iface(j) =  j1
          elseif(is(ie,i1).eq.is(je,j1)) then
            iface(j) = -j1
          else
            write(iow,2000) n,i,j
          endif
        else
          if(is(2,i1).eq.is(2,j1)) then
            iface(j) =  j1
          elseif(is(2,i1).eq.is(je,j1)) then
            iface(j) = -j1
          else
            write(iow,2000) n,i,j
          endif
        endif
      end do

!     Formats

2000  format(' *ERROR* Face:',i2,' No match between sides',i2,
     &       ' and',i2)

      end subroutine mkside

      subroutine psregn(ix,nen,nen1,ne,nf,nreg,prt,prth)

      implicit   none

      include   'iofile.h'

      character (len=5) :: etype, pelabl

      logical        :: prt,prth
      integer        :: nen,nen1,ne,nf,nreg, i,in,j,ma
      integer        :: ix(nen1,*)

      save

!     Set region indicators

      do i = ne,nf
        ix(nen1-1,i) = nreg
      end do ! i

!     Output lists if wanted

      if(prt.and.ne.gt.0) then
        do in = ne,nf,50
          call prtitl(prth)
          write(iow,2003) (i,i=1,nen)
          if(ior.lt.0) then
            write(  *,2003) (i,i=1,nen)
          endif
          j = min(nf,in+49)
          do i = in,j
            ma = ix(nen1,i)
            etype = pelabl(ix(nen+7,i))
            write(iow,2004) i,ma,nreg,etype,(ix(j,i),j=1,nen)
            if(ior.lt.0) then
              write(*,2004) i,ma,nreg,etype,(ix(j,i),j=1,nen)
            endif
          end do ! i
        end do ! in
      endif

2003  format('   E l e m e n t   C o n n e c t i o n s'//
     &   '   Elmt Mat Reg  Type',7(i3,' node'):/(21x,7(i3,' node')))

2004  format(i7,2i4,1x,a5,7i8:/(21x,7i8))

      end subroutine psregn
