c$Id:$
      subroutine therm2d(d,ul,xl,ix,s,p,ndf,ndm,nst,isw)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c     Two dimensional (plane/axisymmetric) Linear Thermal Element

c     N.B.  Surface flux loading may be specified using: ELMT08

c-----[--.----+----.----+----.-----------------------------------------]

c        This is a two dimensional element which can analyze plane
c        or axisymmetric geometries.  Set control parameters as
c        follows:

c           ndm - set to 2     (x,y or r,z-coords)
c           ndf - set > or = 1 (nodal temperatures)
c           nel - set > or = 4

c                    A eta
c             4      |      3
c              o-----|-----o
c              |     |     |
c              |     |     |
c              |     +-----|----> xi
c              |           |
c              |           |
c              o-----------o
c             1             2

c               Node numbering
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

      logical   quad
      integer   ndf,ndm,nst,isw
      integer   i,j, i1,j1, l,lint, tdof
      real*8    xx,yy, xsj, a1,a2,a3,shj,tdot,cfac,lfac

      integer   ix(*)
      real*8    d(*),ul(ndf,nen,*),xl(ndm,*),s(nst,*),p(*)
      real*8    gradt(2),flux(2),dd(2,2),shp(3,9),sg(3,9),el(4,7)

      save

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

        if(nen.eq.3) then
          call pltri3(iel)
        elseif(nen.eq.6 .or. nen.eq.7) then
          call pltri6(iel)
        else
          call plqud4(iel)
        endif


c     Check of mesh if desired (chec)

      elseif(isw.eq.2) then

        if(nel.eq.3 .or. nel.eq.6 .or. nel.eq.7) then
          call cktris(ix,xl,shp,ndm)
        else
          call ckisop(ix,xl,shp,ndm)
        endif

c     Compute conductivity (stiffness) matrix

      elseif(isw.eq.3 .or. isw.eq.6) then

        if(nel.eq.6 .or. nel.eq.7) then
          l    =  7
          quad = .false.
          call tint2d(l,lint,el)
        else
          l    =  d(5)
          quad = .true.
          if(l*l.ne.lint) call int2d(l,lint,sg)
        endif

        do l = 1,lint

          if(quad) then
            call shp2d(sg(1,l),xl,shp,xsj,ndm,nel,ix,.false.)
            xsj = xsj*sg(3,l)
          else
            call trishp(el(1,l),xl,ndm,nel-4,xsj,shp)
            xsj = xsj*el(4,l)
          endif

c         Compute flux

          call thfx2d(d,xl,ul, xx,yy,shp,gradt,flux,dd, ndm,ndf,nel)

c         Compute thermal rate

          tdot = 0.0d0
          do j = 1,nel
            tdot = tdot + shp(3,j)*ul(1,j,4)
          end do

          if(stype.eq.3) then
            xsj = xsj*xx
          endif

          j1 = 1
          do j = 1,nel

            a1 = (dd(1,1)*shp(1,j) + dd(1,2)*shp(2,j))*xsj
            a2 = (dd(2,1)*shp(1,j) + dd(2,2)*shp(2,j))*xsj
            a3 = d(4)*d(64)*shp(3,j)*xsj

c           Compute residual

            p(j1) = p(j1) - a1*gradt(1) - a2*gradt(2)
     &                    - a3*(cfac*tdot + lfac*ul(1,j,4))

c           Compute tangent

            a1 = a1*ctan(1)
            a2 = a2*ctan(1)
            a3 = a3*ctan(2)

c           Lumped rate terms

            s(j1,j1) = s(j1,j1) + a3*lfac

c           Consistent rate and conductivity terms

            i1 = 1
            do i = 1,nel
              s(i1,j1) = s(i1,j1) + a1*shp(1,i) + a2*shp(2,i)
     &                            + a3*shp(3,i)*cfac
              i1 = i1 + ndf
            end do
            j1 = j1 + ndf
          end do
        end do

c     Output heat flux

      elseif(isw.eq.4) then

        if(nel.eq.6 .or. nel.eq.7) then
          l    =  7
          quad = .false.
          call tint2d(l,lint,el)
        else
          l    =  d(5)
          quad = .true.
          if(l*l.ne.lint) call int2d(l,lint,sg)
        endif

        do l=1,lint

        if(quad) then
          call shp2d(sg(1,l),xl,shp,xsj,ndm,nel,ix,.false.)
        else
          call trishp(el(1,l),xl,ndm,nel-4,xsj,shp)
        endif

c       Compute flux and gradients

        call thfx2d(d,xl,ul, xx,yy,shp,gradt,flux,dd, ndm,ndf,nel)

        mct = mct - 1
        if(mct.le.0) then
          write(iow,2002) o,head
          if(ior.lt.0 .and. pfr) then
            write(*,2002) o,head
          endif
          mct = 50
        endif
        write(iow,2003) n,ma,xx,yy,flux,gradt
        if(ior.lt.0 .and. pfr) then
          write(*,2003) n,ma,xx,yy,flux,gradt
        endif

        end do ! l

c     Compute heat capacity (mass) matrix

      elseif(isw.eq.5) then
        if(nel.eq.6 .or. nel.eq.7) then
          l    =  7
          quad = .false.
          call tint2d(l,lint,el)
        else
          l    =  d(5)
          quad = .true.
          if(l*l.ne.lint) call int2d(l,lint,sg)
        endif
        do l = 1,lint
          if(quad) then
            call shp2d(sg(1,l),xl,shp,xsj,ndm,nel,ix,.false.)
            xsj = xsj*sg(3,l)
          else
            call trishp(el(1,l),xl,ndm,nel-4,xsj,shp)
            xsj = xsj*el(4,l)
          endif
          if(stype.eq.3) then
            xx = 0.
              do i = 1,nel
              xx = xx + shp(3,i)*xl(1,i)
            end do
            xsj = xsj*xx
          endif
          j1 = 1
          do j = 1,nel
            shj = d(4)*d(64)*shp(3,j)*xsj

c           Lumped capacity (lmas)

            p(j1) = p(j1) + shj
            i1 = 1

c           Consistent (interpolated ) capacity (mass)

            s(i1,i1) = s(i1,j1) + shj*lfac
            do i = 1,nel
              s(i1,j1) = s(i1,j1) + shj*shp(3,i)*cfac
              i1 = i1 + ndf
            end do
            j1 = j1 + ndf
          end do
        end do

c     Compute surface flux loading (not implemented)

c     elseif(isw.eq.7) then

c     Compute nodal heat flux for output/plots

      elseif(isw.eq.8) then

        call thcn2d(ix,d,xl,ul,s,shp,hr(nph),hr(nph+numnp),hr(ner),
     &              erav,ndf,ndm,nel,numnp)

      endif

c     Formats

2000  format(5x,'F o u r i e r   H e a t   C o n d u c t i o n')

2002  format(a1,20a4//5x,'element flux'//' elmt matl 1-coord  2-coord'
     1            ,'      1-flux      2-flux      1-grad      2-grad')

2003  format(2i5,2f9.3,4e12.3)

      end

      subroutine thcn2d(ix,d,xl,ul,s,shp,dt,st,ser,erav,
     &                  ndf,ndm,nel,numnp)

      implicit  none

      include  'iodata.h'

      logical   quad
      integer   ndf,ndm,nel,numnp
      integer   i,j,l,ll,lint
      real*8    xx,yy,xsj,xg,erav

      integer   ix(*)
      real*8    d(*)
      real*8    dt(numnp),st(numnp,*),ser(*),xl(ndm,*),shp(3,9)
      real*8    gradt(2),flux(2),dd(2,2),ul(ndf,*),s(nel,*)
      real*8    sg(3,9),el(4,7)

      save

c     Lumped and consistent projection routine

      if(nel.eq.6 .or. nel.eq.7) then
        l    =  6
        quad = .false.
        call tint2d(l,lint,el)
      else
        l    =  d(5)
        l    =  max(2,l)
        quad = .true.
        if(l*l.ne.lint) call int2d(l,lint,sg)
      endif
      do i = l,nel
        do j = 1,nel
          s(j,i) = 0.0d0
        end do ! j
      end do ! i
      do l = 1,lint
        if(quad) then
          call shp2d(sg(1,l),xl,shp,xsj,ndm,nel,ix,.false.)
          xsj = xsj*sg(3,l)
        else
          call trishp(el(1,l),xl,ndm,nel-4,xsj,shp)
          xsj = xsj*el(4,l)
        endif

        call thfx2d(d,xl,ul, xx,yy,shp,gradt,flux,dd, ndm,ndf,nel)

c       Compute consistent projection matrix

        do i = 1,nel
          xg     = shp(3,i)*xsj
          do j = 1,nel
            s(i,j) = s(i,j) + xg*shp(3,j)
          end do
        end do

c       Compute lumped projection and assemble stress integrals

        do j = 1,nel
          ll = ix(j)
          if(ll.gt.0) then
            xg     = xsj*shp(3,j)
            dt(ll) = dt(ll) + xg
            st(ll,7) = st(ll,7) + flux(1)*xg
            st(ll,8) = st(ll,8) + flux(2)*xg
            ser(ll)  = ser(ll)  + erav*xg
          endif
        end do
      end do

      end

      subroutine thfx2d(d,xl,ul, xx,yy,shp,gradt,flux,dd, ndm,ndf,nel)

c     Compute thermal gradient and flux

      implicit  none

      integer   ndm,ndf,nel, i
      real*8    xx,yy,psi,cs,sn,c2,s2
      real*8    d(*),xl(ndm,*),ul(ndf,*), shp(3,*)
      real*8    gradt(2),flux(2),dd(2,2)

      save

      gradt(1) = 0.0d0
      gradt(2) = 0.0d0
      xx       = 0.0d0
      yy       = 0.0d0
      do i = 1,nel
        gradt(1) = gradt(1) + shp(1,i)*ul(1,i)
        gradt(2) = gradt(2) + shp(2,i)*ul(1,i)
        xx       = xx + shp(3,i)*xl(1,i)
        yy       = yy + shp(3,i)*xl(2,i)
      end do

c     Compute thermal flux

      psi = d(31)
      cs  = cos(psi)
      sn  = sin(psi)
      c2  = cs*cs
      s2  = sn*sn
      cs  = cs*sn

      dd(1,1) = c2*d(61) + s2*d(62)
      dd(2,2) = s2*d(61) + c2*d(62)
      dd(1,2) = cs*(d(61) - d(62))
      dd(2,1) = dd(1,2)

      flux(1) = -dd(1,1)*gradt(1) - dd(1,2)*gradt(2)
      flux(2) = -dd(2,1)*gradt(1) - dd(2,2)*gradt(2)

      end
