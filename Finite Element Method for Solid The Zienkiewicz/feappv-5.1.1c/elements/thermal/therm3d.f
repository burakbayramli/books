!$Id:$
      subroutine therm3d(d,ul,xl,ix,s,p,ndf,ndm,nst,isw)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!     Three dimensional Linear Thermal Element

!-----[--.----+----.----+----.-----------------------------------------]

!        This is a three dimensional element which can analyze
!        general geometries.  Set control parameters as
!        follows:

!           ndm - set to 3     (x,y or r,z-coords)
!           ndf - set > or = 1 (nodal temperatures)
!           nel - set > or = 8
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
      integer       :: i,j, i1,j1, l, tdof, nhv,nn
      real (kind=8) :: a0,a1,a2,a3,shj,temp,tdot,lfac,cfac, rhoc

      integer       :: ix(*)
      real (kind=8) :: d(*),ul(ndf,nen,*),xl(ndm,*),s(nst,*),p(*)
      real (kind=8) :: xx(3),gradt(3),flux(3,8),dd(3,3)

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

        pstyp = 3

!     Check of mesh if desired (chec)

      elseif(isw.eq.2) then

        call ckbrk8(n_el,ix,xl,ndm,nel,shp3)

!     Compute conductivity (stiffness) matrix

      elseif(isw.eq.3 .or. isw.eq.6 .or. isw.eq.4 .or. isw.eq.8) then

        call quadr3d(d,.true.)

        nhv = nint(d(15))
        nn  = 0
        do l = 1,lint

          call interp3d(l, xl, ndm,nel)

!         Compute flux

          call thfx3d(xl,ul, xx,shp3(1,1,l),temp,gradt,ndm,ndf,nel)

!         Compute thermal flux and conductivity

          call modltd(d, temp,gradt,hr(nh1+nn),hr(nh1+nn),nhv,
     &                dd,flux,rhoc, isw)
          nn = nn + nhv

          if(isw.eq.3 .or. isw.eq.6) then

!           Compute thermal rate

            tdot = 0.0d0
            do j = 1,nel
              tdot = tdot + shp3(4,j,l)*ul(1,j,4)
            end do

            j1 = 1
            do j = 1,nel

              a1 = (dd(1,1)*shp3(1,j,l) + dd(1,2)*shp3(2,j,l)
     &           +  dd(1,3)*shp3(3,j,l))*jac(l)
              a2 = (dd(2,1)*shp3(1,j,l) + dd(2,2)*shp3(2,j,l)
     &           +  dd(2,3)*shp3(3,j,l))*jac(l)
              a3 = (dd(3,1)*shp3(1,j,l) + dd(3,2)*shp3(2,j,l)
     &           +  dd(3,3)*shp3(3,j,l))*jac(l)

              a0 = rhoc*shp3(4,j,l)*jac(l)

!             Compute residual

              p(j1) = p(j1) - a1*gradt(1) - a2*gradt(2) - a3*gradt(3)
     &                      - a0*(cfac*tdot + lfac*ul(1,j,4))

!             Compute tangent

              a0 = a0*ctan(2)
              a1 = a1*ctan(1)
              a2 = a2*ctan(1)
              a3 = a3*ctan(1)

!             Lumped rate terms

              s(j1,j1) = s(j1,j1) + a0*lfac

              i1 = 1
              do i = 1,nel
                s(i1,j1) = s(i1,j1) + a1*shp3(1,i,l)
     &                              + a2*shp3(2,i,l)
     &                              + a3*shp3(3,i,l)
     &                              + a0*shp3(4,i,l)*cfac
                i1 = i1 + ndf
              end do ! i
              j1 = j1 + ndf
            end do ! j

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
            write(iow,2003) n_el,ma,xx,flux(1,l),gradt
            if(ior.lt.0 .and. pfr) then
              write(*,2003) n_el,ma,xx,flux(1,l),gradt
            endif
          endif
        end do ! l

!       Compute nodal heat flux for output/plots

        if(isw.eq.8) then
          call tlcn3d(flux,p,s,p(nen+1),nel)
        endif

!     Compute heat capacity (mass) matrix

      elseif(isw.eq.5) then

        call quadr3d(d,.true.)

        do l = 1,lint

          call interp3d(l, xl, ndm,nel)

          j1 = 1
          do j = 1,nel
            shj = d(4)*d(64)*shp3(4,j,l)*jac(l)

!           Lumped capacity (lmas)

            p(j1) = p(j1) + shj
            i1 = 1

!           Consistent capacity (cmas)

            do i = 1,nel
              s(i1,j1) = s(i1,j1) + shj*shp3(4,i,l)
              i1 = i1 + ndf
            end do
            j1 = j1 + ndf
          end do
        end do

!     Compute surface flux loading (not implemented)

!     elseif(isw.eq.7) then

      endif

!     Formats

2000  format(5x,'F o u r i e r   H e a t   C o n d u c t i o n')

2002  format(a1,20a4//5x,'Element Fluxes'//' Elmt Matl 1-coord  2-coord'
     1            ,'  3-coord      1-flux      2-flux      3-flux'/
     2         37x,'      1-grad      2-grad      3-grad')

2003  format(2i5,0p,3f9.3,1p,3e12.3/37x,1p,3e12.3)

      end subroutine therm3d

      subroutine thfx3d(xl,ul, xx,shp,temp,gradt, ndm,ndf,nel)

!     Compute thermal gradient and flux

      implicit  none

      integer   ndm,ndf,nel, i
      real*8    temp
      real*8    xl(ndm,*),ul(ndf,*), shp(4,*), xx(3),gradt(3)

      save

      gradt(:) = 0.0d0
      xx(:)    = 0.0d0
      do i = 1,nel
        gradt(:) = gradt(:) + shp(1:3,i)*ul(1,i)
        temp     = temp     + shp(4,i)*ul(1,i)
        xx(:)    = xx(:) + shp(4,i)*xl(1:3,i)
      end do ! i

      end subroutine thfx3d
