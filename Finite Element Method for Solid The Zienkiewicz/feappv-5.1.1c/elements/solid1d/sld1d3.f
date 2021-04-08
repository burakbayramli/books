!$Id:$
      subroutine sld1d3(d,ul,xl,tl,s,p,ndf,ndm,nst,isw)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:  1-d enhanced strain element for FEAP

!      Output records:
!      Prints in element: sig-11, sig-22, sig-33
!                         eps-11, eps-22, eps-33
!                         N.B. These are by default principal values

!      Prints at nodes:   1=sig-11, 2=sig-22, 3=sig-33
!                         psig-1  , psig-2    (computed by FEAP)

!      History Variable Storage (relative to nh1 or nh2)

!      Start           Description             Variable  Length
!      hr(0)           Enhanced displacement      ui(*,1)   1
!      hr(3)           Stress history at point-1    -      nhv
!      hr(3+  nhv)     Stress history at point-2    -      nhv

!      Total number of words / element is 1 + 2*nhv
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'bdata.h'
      include  'cdata.h'
      include  'eldata.h'
      include  'elplot.h'
      include  'eltran.h'
      include  'hdata.h'
      include  'incshp.h'
      include  'iofile.h'
      include  'pmod2d.h'
      include  'prstrs.h'
      include  'qudshp.h'
      include  'rdata.h'
      include  'comblk.h'

      logical       :: noconv
      integer       :: ndm,ndf,nst,isw
      real (kind=8) :: a11,a21,a31,ta,tol1,tolu
      real (kind=8) :: cfac,lfac,sfac,dmas,lms,jacc ,rr
      integer       :: i,j,l,nhv,ni,nn,nenit,i1,j1, istrt
      real (kind=8) :: d(*),xl(ndm,*),ul(ndf,nen,*),tl(*)
      real (kind=8) :: s(nst,*),p(ndf,*)
      real (kind=8) :: sig(9,4),eps(9,3),epsv(4),epsl(3,4)
      real (kind=8) :: gg(4),hg,hh,bb,dui,dd(6,6,5,4)
      real (kind=8) :: shpr(4,4),sigl(3),aa(3,3,4),r0(4)

      save

      data      ni /1/, eps / 27*0.0d0 /

!     Recover enhanced modes (saved in last iteration)

      nhv   = nint(d(15))
      istrt = nint(d(84))
      ui(1,1)   = hr(nh2)
      ui(1,2)   = 0.0d0

!     Compute quadrature and weights

      call quadr1d(d)

!     Initialize history variables only

      if(isw.eq.14) then
        nn = ni
        do l = 1,lint
          call stra1d(d,xl,ul,tl,shp1(1,1,l),ndf,ndm,nel,rr,ta,eps)
          call modlsd(l,d,ta,eps,hr(nn+nh1),hr(nn+nh2),nhv,istrt,
     &                dd(1,1,1,l),sig(1,l),isw)
          nn = nn + nhv
        end do ! l
        return
      endif ! isw.eq.14

!     Compute shape functions

      do l = 1,lint

        call interp1d(l, xl, ndm,nel,.false.)

!       Axisymmetry & sphere

        if(stype.eq.3 .or. stype.eq.9) then
          r0(l) = 0.0d0
          do j = 1,nel
            r0(l) = r0(l) + shp1(2,j,l)*xl(1,j)
          end do ! j
          jac(l) = jac(l)*r0(l)
          if(stype.eq.9) then
            jac(l) = jac(l)*r0(l)
          endif
          do j = 1,nel
            shpr(j,l) = shp1(2,j,l)/r0(l)
          end do ! j

!       Plane

        else
          do j = 1,nel
            shpr(j,l) = 0.0d0
          end do ! j
        endif
      end do ! l

!     Compute enhanced modes by local iteration

      tolu   =  1.d-03*tol*rnmax/dble(numel)
      nenit  =  0
      noconv = .true.
      do while(noconv)

        bb = 0.0d0
        hh = 0.0d0

!       Set initial counter for history terms in stress/strain

        nn = ni
        do l = 1,lint

!         Compute enhanced strain shape functions

          call shpi1d(sg1(1,l),xl,ndm)

!         Compute stress and strain at point

          call stra1d(d,xl,ul,tl,shp1(1,1,l),ndf,ndm,nel,rr,ta,eps)

          call modlsd(l,d,ta,eps,hr(nn+nh1),hr(nn+nh2),nhv,istrt,
     &                dd(1,1,1,l),sig(1,l),isw)

!         Scale moduli and stresses

          do i = 1,3
            do j = 1,3
              aa(j,i,l) = dd(j,i,1,l)*jac(l)*ctan(1)
            end do ! j
            sigl(i) = sig(i,l)*jac(l)
          end do ! i

!         Store time history plot data for element

          i = 12*(l-1)
          do j = 1,3
            tt(j+i)   = sig(j,l)
            tt(j+i+6) = eps(j,1)
          end do ! j

!         Enhanced residual computations

          if(stype.eq.3) then
            shpi(3,1) = shpi(2,1)/r0(l)
            shpi(2,1) = 0.0d0
          elseif(stype.eq.9) then
            shpi(2,1) = shpi(2,1)/r0(l)
            shpi(3,1) = shpi(2,1)
          else
            shpi(2,1) = 0.0d0
            shpi(3,1) = 0.0d0
          endif
          bb  = bb - sigl(1)*shpi(1,1)
     &             - sigl(2)*shpi(2,1)
     &             - sigl(3)*shpi(3,1)

!         Stiffness computations

          a11 = aa(1,1,l)*shpi(1,1)
     &        + aa(1,2,l)*shpi(2,1)
     &        + aa(1,3,l)*shpi(3,1)
          a21 = aa(2,1,l)*shpi(1,1)
     &        + aa(2,2,l)*shpi(2,1)
     &        + aa(2,3,l)*shpi(3,1)
          a31 = aa(3,1,l)*shpi(1,1)
     &        + aa(3,2,l)*shpi(2,1)
     &        + aa(3,3,l)*shpi(3,1)
          hh  = hh + shpi(1,1)*a11 + shpi(2,1)*a21 + shpi(3,1)*a31

          nn = nn + nhv
        end do ! l

!       Invert the hh
        hh = 1.d0/hh

!       Compute incremental enhanced displacements enhanced modes

        dui     = hh*bb
        ui(1,1) = ui(1,1) + dui
        ui(1,2) = ui(1,2) + dui

!       Check convergence

        tol1 = abs(bb*dui)

        if(tol1.le.tolu .and. nenit.ge.1) then
          noconv = .false.
        endif
        nenit = nenit + 1
        if(nenit.ge.3 .or. tolu.eq.0.0d0) then
          noconv = .false.
        endif
      end do ! while

!     Save enhanced modes

      hr(nh2) = ui(1,1)

!     Set initial counter for history terms in stress/strain

      if(isw.eq.3. or. isw.eq.6) then

!       Time integration order set to static or dynamic

        if((d(7).ge.0.0 .or. d(183).ne.0.0d0) .and. shflg) then
          cfac = d(7)
          lfac = 1.0d0 - cfac
        else
          cfac = 0.0d0
          lfac = 0.0d0
        endif

        do i = 1,nel
          gg(i) = 0.0d0
        end do ! i

        nn = ni
        do l = 1,lint

          call shpi1d(sg1(1,l),xl,ndm)

          if(stype.eq.3 .or. stype.eq.9) then
            shpi(3,1) = shpi(2,1)/r0(l)
            if(stype.eq.9) then
              shpi(2,1) = shpi(3,1)
            else
              shpi(2,1) = 0.0d0
            endif
            jacc      = jac(l)/r0(l)
          else
            shpi(2,1) = 0.0d0
            shpi(3,1) = 0.0d0
            jacc      = 0.0d0
          endif

!         Rayleigh damping effects

          if(d(78).ne.0.0d0) then
            call rays1d(d,shp1(1,1,l),sig(1,l),dd(1,1,5,l),ul(1,1,4),
     &                  xl,ndf,ndm,nel)
            sfac = d(78)*ctan(2)
          else
            sfac = 0.0d0
          endif

!         Residual computations

          call resid1d(cfac,lfac,jac(l),jacc,shp1(1,1,l),sig(1,l),d,
     &                 ul(1,1,4),ul(1,1,5), p,ndf)

!         Stiffness computations

          if(isw.eq.3) then

            dmas = d(4)*(ctan(3) + d(77)*ctan(2))*jac(l)

            do j = 1,3
              do i = 1,3
                aa(i,j,l) = aa(i,j,l) + dd(i,j,5,l)*sfac
              end do ! i
            end do ! j

            j1 = 1
            do j = 1,nel

!             Compute d * b matrix = a

              if(stype.eq.9) then             ! Spherical
                a11 = aa(1,1,l)*shp1(1,j,l)
     &              + aa(1,2,l)*shpr(j,l)
     &              + aa(1,3,l)*shpr(j,l)
                a21 = aa(2,1,l)*shp1(1,j,l)
     &              + aa(2,2,l)*shpr(j,l)
     &              + aa(2,3,l)*shpr(j,l)
                a31 = aa(3,1,l)*shp1(1,j,l)
     &              + aa(3,2,l)*shpr(j,l)
     &              + aa(3,3,l)*shpr(j,l)
              else                           ! Plane/Axisymmetric
                a11 = aa(1,1,l)*shp1(1,j,l)
     &              + aa(1,3,l)*shpr(j,l)
                a21 = 0.0d0
                a31 = aa(3,1,l)*shp1(1,j,l)
     &              + aa(3,3,l)*shpr(j,l)
              endif

!             Lumped mass effects

              lms          = shp1(2,j,l)*dmas
              s(j1  ,j1  ) = s(j1  ,j1  ) + lms*lfac

              i1 = 1
              do i = 1,nel
                s(i1  ,j1  ) = s(i1  ,j1  ) + shp1(1,i,l)*a11
     &                                      +   shpr(i,l)*a21
     &                                      +   shpr(i,l)*a31
     &                                      + shp1(2,i,l)*cfac*lms
                i1 = i1 + ndf
              end do ! i

!             Enhanced coupling term

              gg(j) = gg(j) + shpi(1,1)*a11
     &                      + shpi(2,1)*a21
     &                      + shpi(3,1)*a31

              j1      = j1 + ndf
            end do ! j
          endif
          nn = nn + nhv
        end do ! l

!       Eliminate enhanced modes

        if(isw.eq.3) then

!         Construct static condensation

          j1 = 1
          do j = 1,nel
            hg     = hh*gg(j)
            p(1,j) = p(1,j) - hg*bb
            i1     = 1
            do i = 1,nel
              s(i1,j1) = s(i1,j1) - gg(i)*hg
              i1       = i1 + ndf
            end do ! i
            j1 = j1 + ndf
          end do ! j

        endif

!     Compute and output element variables

      elseif(isw.eq.4 .or. isw.eq.8 .or. isw.eq.16 .or. isw.eq.25) then

        call quadr1d(d)

!       Set initial counter for history terms in stress/strain

        nn = ni
        do l = 1,lint
          call interp1d(l, xl, ndm,nel,.false.)
          call shpi1d(sg1(1,l),xl,ndm)

!         Compute stress and strain at point

          call stra1d(d,xl,ul,tl,shp1(1,1,l),ndf,ndm,nel,rr,ta,eps)

          epsv(l) = eps(1,1) + eps(2,1) + eps(3,1)

          call modlsd(l,d,ta,eps,hr(nn+nh1),hr(nn+nh2),nhv,istrt,
     &                dd,sig(1,l),isw)

!         Compute principal stress values

          if(isw.eq.4) then
            mct = mct - 2
            if(mct.le.0) then
              write(iow,2001) o,head
              if(ior.lt.0) write(*,2001) o,head
              mct = 50
            endif
            write(iow,2002) n_el,ma,(sig(i,l),i=1,3),
     &                           rr,(eps(i,1),i=1,3)
            if(ior.lt.0) then
              write(*,2002) n_el,ma,(sig(i,l),i=1,3),
     &                           rr,(eps(i,1),i=1,3)
            endif
          elseif(isw.eq.8) then
            epsl(1:3,l) = eps(1:3,1)
          endif
          nn = nn + nhv
        end do ! l

!       Plot stress values

        if(isw.eq.8) then
          call slcn1d(sig,epsl,shp1,jac,p,s,lint,nel,9)
        endif

      endif

!     Formats

2001  format(a1,20a4//5x,'Enhanced Strain Model Element Solutions'//
     &   '    Elmt Mat   11-stress   22-stress   33-stress'/
     &   '     1-coord   11-strain   22-strain   33-strain')
2002  format(i8,i4,1p,3e12.4/1p,4e12.4)

      end subroutine sld1d3
