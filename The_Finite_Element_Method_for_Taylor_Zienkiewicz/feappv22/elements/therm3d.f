c$Id:$
      subroutine therm3d(d,ul,xl,ix,s,p,ndf,ndm,nst,isw)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c     Three dimensional Linear Thermal Element

c-----[--.----+----.----+----.-----------------------------------------]

c        This is a three dimensional element which can analyze
c        general geometries.  Set control parameters as
c        follows:

c           ndm - set to 3     (x,y or r,z-coords)
c           ndf - set > or = 1 (nodal temperatures)
c           nel - set > or = 8

c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'bdata.h'
      include  'cdata.h'
      include  'eldata.h'
      include  'eltran.h'
      include  'fdata.h'
      include  'iofile.h'
      include  'mdata.h'
      include  'pmod2d.h'
      include  'prstrs.h'
      include  'comblk.h'

      integer   ndf,ndm,nst,isw
      integer   i,j, i1,j1, l,lint, tdof
      real*8    xsj, a0,a1,a2,a3,shj,tdot,lfac,cfac

      integer   ix(*)
      real*8    d(*),ul(ndf,nen,*),xl(ndm,*),s(nst,*),p(*)
      real*8    xx(3),gradt(3),flux(3),dd(3,3),shp(4,8),sg(4,8),sg0(3)
      real*8    sv(5,4)

      save

      data      sg0 /0.0d0, 0.0d0, 0.0d0/

c     Set mass factors

      if(d(7).ge.0.0d0) then
        cfac = d(7)
        lfac = 1.d0 - cfac
      else
        cfac = 0.0d0
        lfac = 0.0d0
      endif

c     Input material properties

      if(isw.eq.1) then

        if(ior.lt.0) write(*,2000)
        write(iow,2000)
        call inmate(d,tdof,nen,6)

c       Delete unused parameters

        do i = 2,ndf
          ix(i) = 0
        end do

c       Set to preclude sloping boundary transformations

        ea(1,-iel) = 0
        ea(2,-iel) = 0

c       Set plot sequence

        if(nen.eq.4) then
          call pltet4(iel)
        else
          call plbrk8(iel)
        endif

c     Check of mesh if desired (chec)

      elseif(isw.eq.2) then

        call ckbrk8(n,ix,xl,ndm,nel,shp)

c     Compute conductivity (stiffness) matrix

      elseif(isw.eq.3 .or. isw.eq.6) then

        if(nel.eq.4) then
          l = 2
          call tint3d(l,lint,sv)
        else
          l = d(5)
          call int3d(l,lint,sg)
        endif

        do l = 1,lint

          if(nel.eq.4) then
            call tetshp(sv(1,l),xl,ndm,xsj,shp)
            xsj = xsj*sv(5,l)
          else
            call shp3d(sg(1,l),xsj,shp,xl,ndm)
            xsj = xsj*sg(4,l)
          endif

c         Compute flux

          call thfx3d(d,xl,ul, xx,shp,gradt,flux,dd, ndm,ndf,nel)

c         Compute thermal rate

          tdot = 0.0d0
          do j = 1,nel
            tdot = tdot + shp(4,j)*ul(1,j,4)
          end do

          j1 = 1
          do j = 1,nel

            a1 = (dd(1,1)*shp(1,j) + dd(1,2)*shp(2,j)
     &         +  dd(1,3)*shp(3,j))*xsj
            a2 = (dd(2,1)*shp(1,j) + dd(2,2)*shp(2,j)
     &         +  dd(2,3)*shp(3,j))*xsj
            a3 = (dd(3,1)*shp(1,j) + dd(3,2)*shp(2,j)
     &         +  dd(3,3)*shp(3,j))*xsj

            a0 = d(4)*d(64)*shp(4,j)*xsj

c           Compute residual

            p(j1) = p(j1) - a1*gradt(1) - a2*gradt(2) - a3*gradt(3)
     &                    - a0*(cfac*tdot + lfac*ul(1,j,4))

c           Compute tangent

            a0 = a0*ctan(2)
            a1 = a1*ctan(1)
            a2 = a2*ctan(1)
            a3 = a3*ctan(1)

c           Lumped rate terms

            s(j1,j1) = s(j1,j1) + a0*lfac

            i1 = 1
            do i = 1,nel
              s(i1,j1) = s(i1,j1) + a1*shp(1,i) + a2*shp(2,i)
     &                            + a3*shp(3,i) + a0*shp(4,i)*cfac
              i1 = i1 + ndf
            end do
            j1 = j1 + ndf
          end do
        end do

c     Output heat flux

      elseif(isw.eq.4) then

        if(nel.eq.4) then
          sv(1,1) = 0.25d0
          sv(2,1) = 0.25d0
          sv(3,1) = 0.25d0
          sv(4,1) = 0.25d0
          call tetshp(sv,xl,ndm,xsj,shp)
        else
          call shp3d(sg0,xsj,shp,xl,ndm)
        endif

c       Compute flux and gradients

        call thfx3d(d,xl,ul, xx,shp,gradt,flux,dd, ndm,ndf,nel)

        mct = mct - 1
        if(mct.le.0) then
          write(iow,2002) o,head
          if(ior.lt.0 .and. pfr) then
            write(*,2002) o,head
          endif
          mct = 50
        endif
        write(iow,2003) n,ma,xx,flux,gradt
        if(ior.lt.0 .and. pfr) then
          write(*,2003) n,ma,xx,flux,gradt
        endif

c     Compute heat capacity (mass) matrix

      elseif(isw.eq.5) then

        if(nel.eq.4) then
          l = 2
          call tint3d(l,lint,sv)
        else
          l = d(5)
          call int3d(l,lint,sg)
        endif
        do l = 1,lint
          if(nel.eq.4) then
            call tetshp(sv(1,l),xl,ndm,xsj,shp)
            xsj = xsj*sv(5,l)
          else
            call shp3d(sg(1,l),xsj,shp,xl,ndm)
            xsj = xsj*sg(4,l)
          endif
          j1 = 1
          do j = 1,nel
            shj = d(4)*d(64)*shp(4,j)*xsj

c           Lumped capacity (lmas)

            p(j1) = p(j1) + shj
            i1 = 1

c           Consistent capacity (cmas)

            do i = 1,nel
              s(i1,j1) = s(i1,j1) + shj*shp(4,i)
              i1 = i1 + ndf
            end do
            j1 = j1 + ndf
          end do
        end do

c     Compute surface flux loading (not implemented)

c     elseif(isw.eq.7) then

c     Compute nodal heat flux for output/plots

      elseif(isw.eq.8) then

        call thcn3d(ix,d,xl,ul,s,shp,hr(nph),hr(nph+numnp),hr(ner),
     &              erav,ndf,ndm,nel,numnp)
      endif

c     Formats

2000  format(5x,'F o u r i e r   H e a t   C o n d u c t i o n')

2002  format(a1,20a4//5x,'Element Fluxes'//' Elmt Matl 1-coord  2-coord'
     1            ,'  3-coord      1-flux      2-flux      3-flux'/
     2         37x,'      1-grad      2-grad      3-grad')

2003  format(2i5,0p,3f9.3,1p,3e12.3/37x,1p,3e12.3)

      end

      subroutine thcn3d(ix,d,xl,ul,s,shp,dt,st,ser,erav,
     &                  ndf,ndm,nel,numnp)

      implicit  none

      include  'iodata.h'

      integer   ndf,ndm,nel,numnp
      integer   i,j,l,ll,lint
      real*8    xsj,xg,erav

      integer   ix(*)
      real*8    d(*)
      real*8    dt(numnp),st(numnp,*),ser(*),xl(ndm,*),shp(4,8)
      real*8    xx(3),gradt(3),flux(3),dd(3,3),ul(ndf,*),s(nel,*)
      real*8    sg(4,8),sv(5,4)

      save

c     Lumped and consistent projection routine

      if(nel.eq.4) then
        l = 2
        call tint3d(l,lint,sv)
      else
        l = d(5)
        l = max(2,l)
        call int3d(l,lint,sg)
      endif
      do i = 1,nel
        do j = 1,nel
          s(j,i) = 0.0d0
        end do ! j
      end do ! i
      do l = 1,lint
        if(nel.eq.4) then
          call tetshp(sv(1,l),xl,ndm,xsj,shp)
          xsj = xsj*sv(5,l)
        else
          call shp3d(sg(1,l),xsj,shp,xl,ndm)
          xsj = xsj*sg(4,l)
        endif

        call thfx3d(d,xl,ul, xx,shp,gradt,flux,dd, ndm,ndf,nel)

c       Compute consistent projection matrix

        do i = 1,nel
          xg     = shp(4,i)*xsj
          do j = 1,nel
            s(i,j) = s(i,j) + xg*shp(4,j)
          end do
        end do

c       Compute lumped projection and assemble stress integrals

        do j = 1,nel
          ll = abs(ix(j))
          if(ll.gt.0) then
            xg       = xsj*shp(4,j)
            dt(ll)   = dt(ll) + xg
            st(ll,7) = st(ll,7) + flux(1)*xg
            st(ll,8) = st(ll,8) + flux(2)*xg
            st(ll,9) = st(ll,9) + flux(3)*xg
            ser(ll)  = ser(ll)  + erav*xg
          endif
        end do
      end do

      end

      subroutine thfx3d(d,xl,ul, xx,shp,gradt,flux,dd, ndm,ndf,nel)

c     Compute thermal gradient and flux

      implicit  none

      integer   ndm,ndf,nel, i
      real*8    psi,cs,sn,c2,s2
      real*8    d(*),xl(ndm,*),ul(ndf,*), shp(4,*)
      real*8    xx(3),gradt(3),flux(3),dd(3,3)

      save

      gradt(1) = 0.0d0
      gradt(2) = 0.0d0
      gradt(3) = 0.0d0
      xx(1)    = 0.0d0
      xx(2)    = 0.0d0
      xx(3)    = 0.0d0
      do i = 1,nel
        gradt(1) = gradt(1) + shp(1,i)*ul(1,i)
        gradt(2) = gradt(2) + shp(2,i)*ul(1,i)
        gradt(3) = gradt(3) + shp(3,i)*ul(1,i)
        xx(1)    = xx(1) + shp(4,i)*xl(1,i)
        xx(2)    = xx(2) + shp(4,i)*xl(2,i)
        xx(3)    = xx(3) + shp(4,i)*xl(3,i)
      end do

c     Compute thermal flux

      psi = d(31)
      cs  = cos(psi)
      sn  = sin(psi)
      c2  = cs*cs
      s2  = sn*sn
      cs  = cs*sn

      dd(1,1) = c2*d(61) + s2*d(62)
      dd(1,2) = cs*(d(61) - d(62))
      dd(1,3) = 0.0d0

      dd(2,1) = dd(1,2)
      dd(2,2) = s2*d(61) + c2*d(62)
      dd(2,3) = 0.0d0

      dd(3,1) = 0.0d0
      dd(3,2) = 0.0d0
      dd(3,3) = d(63)

      flux(1) = -dd(1,1)*gradt(1) - dd(1,2)*gradt(2)
      flux(2) = -dd(2,1)*gradt(1) - dd(2,2)*gradt(2)
      flux(3) = -dd(3,3)*gradt(3)

      end
