!$Id:$
      subroutine therm2d(d,ul,xl,ix,s,p,ndf,ndm,nst,isw)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!     Two dimensional (plane/axisymmetric) Linear Thermal Element

!-----[--.----+----.----+----.-----------------------------------------]
!        This is a two dimensional element which can analyze plane
!        or axisymmetric geometries.  Set control parameters as
!        follows:

!           ndm - set to 2     (x,y or r,z-coords)
!           ndf - set > or = 1 (nodal temperatures)
!           nel - set > or = 4

!                    A eta
!             4      |      3
!              o-----|-----o
!              |     |     |
!              |     |     |
!              |     +-----|----> xi
!              |           |
!              |           |
!              o-----------o
!             1             2

!               Node numbering
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'bdata.h'
      include  'cdata.h'
      include  'eldata.h'
      include  'eltran.h'
      include  'fdata.h'
      include  'hdata.h'
      include  'iofile.h'
      include  'mdata.h'
      include  'pmod2d.h'
      include  'prstrs.h'
      include  'qudshp.h'
      include  'comblk.h'

      integer       :: ndf,ndm,nst,isw
      integer       :: i,j, i1,j1, l, tdof, nhv, nn
      real (kind=8) :: a1,a2,a3,shj,tdot,cfac,lfac, temp,rhoc

      integer       :: ix(*)
      real (kind=8) :: d(*),ul(ndf,nen,*),xl(ndm,*),s(nst,*),p(*)
      real (kind=8) :: xx(2),gradt(3),flux(3,64),dd(3,3)

      save

!     Set mass factors

      if(d(7).ge.0.0d0) then
        cfac = d(7)
        lfac = 1.d0 - cfac
      else
        cfac = 0.0d0
        lfac = 0.0d0
      endif

!     Input material properties

      if(isw.eq.1) then

        if(ior.lt.0) write(*,2000)
        write(iow,2000)
        call inmate(d,tdof,nen,6)

!       Delete unused parameters

        do i = 2,ndf
          ix(i) = 0
        end do

!       Set to preclude sloping boundary transformations

        ea(1,-iel) = 0
        ea(2,-iel) = 0

!       Set plot sequence

        pstyp = 2

!     Check of mesh if desired (chec)

      elseif(isw.eq.2) then

        if(nel.eq.3 .or. nel.eq.6 .or. nel.eq.7) then
          call cktris(ix,xl,shp2,ndm)
        else
          call ckisop(ix,xl,shp2,ndm)
        endif

!     Compute conductivity (stiffness) matrix

      elseif(isw.eq.3 .or. isw.eq.6 .or. isw.eq.4 .or. isw.eq.8) then

        call quadr2d(d,.false.)

        nhv = nint(d(15))
        nn  = 0
        do l = 1,lint

          call interp2d(l, xl,ix, ndm,nel, .false.)

!         Compute gradient and temperature

          call thfx2d(xl,ul, xx,shp2(1,1,l),temp,gradt,ndm,ndf,nel)

!         Compute thermal flux and conductivity

          call modltd(d, temp,gradt,hr(nh1+nn),hr(nh1+nn),nhv,
     &                dd,flux,rhoc, isw)
          nn = nn + nhv

          if(isw.eq.3 .or. isw.eq.6) then

!           Compute thermal rate

            tdot = 0.0d0
            do j = 1,nel
              tdot = tdot + shp2(3,j,l)*ul(1,j,4)
            end do

            if(stype.eq.3) then
              jac(l) = jac(l)*xx(1)
            endif

            j1 = 1
            do j = 1,nel

              a1 = (dd(1,1)*shp2(1,j,l) + dd(1,2)*shp2(2,j,l))*jac(l)
              a2 = (dd(2,1)*shp2(1,j,l) + dd(2,2)*shp2(2,j,l))*jac(l)
              a3 = rhoc*shp2(3,j,l)*jac(l)

!             Compute residual

              p(j1) = p(j1) - a1*gradt(1) - a2*gradt(2)
     &                      - a3*(cfac*tdot + lfac*ul(1,j,4))

!             Compute tangent

              a1 = a1*ctan(1)
              a2 = a2*ctan(1)
              a3 = a3*ctan(2)

!             Lumped rate terms

              s(j1,j1) = s(j1,j1) + a3*lfac

!             Consistent rate and conductivity terms

              i1 = 1
              do i = 1,nel
                s(i1,j1) = s(i1,j1) + a1*shp2(1,i,l) + a2*shp2(2,i,l)
     &                              + a3*shp2(3,i,l)*cfac
                i1 = i1 + ndf
              end do
              j1 = j1 + ndf
            end do

!         Output heat flux

          elseif(isw.eq.4) then
            mct = mct - 1
            if(mct.le.0) then
              write(iow,2002) o,head
              if(ior.lt.0 .and. pfr) then
                write(*,2002) o,head
              endif
              mct = 50
            endif
            write(iow,2003) n_el,ma,xx,flux(1:2,l),gradt
            if(ior.lt.0 .and. pfr) then
              write(*,2003) n_el,ma,xx,flux(1:2,l),gradt
            endif
          endif

        end do ! l

!       Compute nodal heat flux for output/plots

        if(isw.eq.8) then
          call tlcn2d(flux,p,s,nel)
        endif

!     Compute heat capacity (mass) matrix

      elseif(isw.eq.5) then

        call quadr2d(d,.false.)

        do l=1,lint
          call interp2d(l, xl,ix, ndm,nel, .true.)
          if(stype.eq.3) then
            xx(1) = 0.
              do i = 1,nel
              xx(1) = xx(1) + shp2(3,i,l)*xl(1,i)
            end do
            jac(l) = jac(l)*xx(1)
          endif
          j1 = 1
          do j = 1,nel
            shj = d(4)*d(64)*shp2(3,j,l)*jac(l)

!           Lumped capacity (lmas)

            p(j1) = p(j1) + shj
            i1 = 1

!           Consistent (interpolated ) capacity (mass)

            s(i1,i1) = s(i1,j1) + shj*lfac
            do i = 1,nel
              s(i1,j1) = s(i1,j1) + shj*shp2(3,i,l)*cfac
              i1 = i1 + ndf
            end do
            j1 = j1 + ndf
          end do
        end do

      endif

!     Formats

2000  format(5x,'F o u r i e r   H e a t   C o n d u c t i o n')

2002  format(a1,20a4//5x,'element flux'//' elmt matl 1-coord  2-coord'
     1            ,'      1-flux      2-flux      1-grad      2-grad')

2003  format(2i5,2f9.3,4e12.3)

      end subroutine therm2d
