!$Id:$
      subroutine sld2d3(d,ul,xl,ix,tl,s,p,ndf,ndm,nst,isw)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:  Plane/axisymmetric enhanced strain element for FEAPpv

!      Output records:
!      Prints in element: sig-11, sig-22, sig-33, sig-12, sig-1 sig-2
!                         eps-11, eps-22, eps-33, eps-12

!      Prints at nodes:   1=sig-11, 2=sig-22, 3=sig-33, 4=sig-12
!                         psig-1  , psig-2    (computed by FEAPpv)

!      History Variable Storage (relative to nh1 or nh2)

!      Start           Description             Variable  Length
!      hr(0)           Enhanced displacement      ui(*,1)   5
!      hr(5)           Stress history at point-1    -      nhv
!      hr(5+  nhv)     Stress history at point-2    -      nhv
!      hr(5+2*nhv)     Stress history at point-3    -      nhv
!      hr(5+3*nhv)     Stress history at point-4    -      nhv

!      Total number of words / element is 5 + 4*nhv
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
      include  'prld1.h'
      include  'prstrs.h'
      include  'qudshp.h'
      include  'rdata.h'
      include  'comblk.h'

      logical       :: noconv
      integer       :: ndm,ndf,nst,isw, ix(*)
      real (kind=8) :: ta,tol1,tolu
      real (kind=8) :: cfac,lfac,sfac,dmas,lms,cms,jac0 ,rr,zz
      integer       :: i,j,l,nhv,ni,nn,nenit,i1,j1,i2,j2,ii,jj,istrt
      real (kind=8) :: d(*),xl(ndm,*),ul(ndf,nen,*),tl(*),s(nst,*),p(*)
      real (kind=8) :: sig(10,9),eps(9,3)
      real (kind=8) :: gg(5,8),hh(5,5),bb(5),hg(5,8),dui(5),dd(6,6,5,9)
      real (kind=8) :: ss(8,8),shpr(9,9),sigl(6),aa(6,6,9)
      real (kind=8) :: bd(6,2),r0(9)

      save

      data      ni /5/, eps / 27*0.0d0 /

!     Data inputs

      if( isw.eq.1 ) then

        return

      endif

!     Recover enhanced modes (saved in last iteration)

      nhv   = nint(d(15))
      istrt = nint(d(84))
      do i = 1,5
        ui(i,1)   = hr(nh2-1+i)
        ui(i,2)   = 0.0d0
      end do ! i

!     Compute quadrature and weights

      l = nint(d(5))
      call int2d(l,lint,sg2)

!     Initialize history variables only

      if(isw.eq.14) then
        nn = ni
        do l = 1,lint
          call modlsd(l,d,ta,eps,hr(nn+nh1),hr(nn+nh2),nhv,istrt,
     &                dd(1,1,1,l),sig(1,l), isw)
          nn = nn + nhv
        end do ! l
        return
      endif ! isw.eq.14

!     Compute shape functions

      do l = 1,lint
        call shp2d(sg2(1,l),xl,shp2(1,1,l),jac(l),ndm,nel,ix,.false.)

!       Axisymmetry

        if(stype.eq.3) then
          r0(l)   = shp2(3,1,l)*xl(1,1) + shp2(3,2,l)*xl(1,2)
     &            + shp2(3,3,l)*xl(1,3) + shp2(3,4,l)*xl(1,4)
          dvol(l) = jac(l)*sg2(3,l)*r0(l)
          do j = 1,nel
            shpr(j,l) = shp2(3,j,l)/r0(l)
          end do ! j

!       Plane

        else
          dvol(l) = jac(l)*sg2(3,l)
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

        do j = 1,5
          bb(j) = 0.0d0
          do i = 1,5
            hh(i,j) = 0.0d0
          end do ! i
        end do ! j

!       Set initial counter for history terms in stress/strain

        nn = ni
        do l = 1,lint

!         Compute enhanced strain shape functions

          call shpi2d(sg2(1,l),jac(l),xl,ndm)

!         Compute strain at point

          call strn2d(d,xl,ul,tl,shp2(1,1,l),ndf,ndm,nel,rr,zz,ta,eps)

!         Compute stress and tangent moduli

          call modlsd(l,d,ta,eps,hr(nn+nh1),hr(nn+nh2),nhv,istrt,
     &                dd(1,1,1,l),sig(1,l), isw)

!         Scale moduli and stresses

          do i = 1,4
            do j = 1,4
              aa(j,i,l) = dd(j,i,1,l)*dvol(l)*ctan(1)
            end do ! j
            sigl(i) = sig(i,l)*dvol(l)
          end do ! i

!         Store time history plot data for element

          i = 6*(l-1)
          do j = 1,6
            tt(j+i)   = sig(j,l)
          end do ! j

!         Enhanced residual computations

          do j = 1,2
            bb(2*j-1) = bb(2*j-1) - sigl(1)*shpi(1,j)
     &                            - sigl(4)*shpi(2,j)
            bb(2*j  ) = bb(2*j  ) - sigl(2)*shpi(2,j)
     &                            - sigl(4)*shpi(1,j)
          end do ! j

          if(stype.eq.3) then
            shpi(3,3) = shpi(3,3)/r0(l)
            bb(5)     = bb(5) - sigl(3)*shpi(3,3)
            do j = 1,2
              bd(3,1)     = aa(3,1,l)*shpi(1,j) + aa(3,4,l)*shpi(2,j)
              bd(3,2)     = aa(3,2,l)*shpi(2,j) + aa(3,4,l)*shpi(1,j)
              hh(5,2*j-1) = hh(5,2*j-1) + shpi(3,3)*bd(3,1)
              hh(5,2*j  ) = hh(5,2*j  ) + shpi(3,3)*bd(3,2)
              bd(1,1)     = aa(1,3,l)*shpi(3,3)
              bd(2,1)     = aa(2,3,l)*shpi(3,3)
              bd(4,1)     = aa(4,3,l)*shpi(3,3)
              hh(2*j-1,5) = hh(2*j-1,5) + shpi(1,j)*bd(1,1)
     &                                  + shpi(2,j)*bd(4,1)
              hh(2*j  ,5) = hh(2*j  ,5) + shpi(2,j)*bd(2,1)
     &                                  + shpi(1,j)*bd(4,1)
            end do ! j
            hh(5,5) = hh(5,5) + shpi(3,3)*aa(3,3,l)*shpi(3,3)
          else
            shpi(3,3) = 0.0d0
            hh(5,5)   = 1.0d0
          endif

!         Stiffness computations

          do j = 1,2

!           Compute d * b matrix = a

            bd(1,1) = aa(1,1,l)*shpi(1,j) + aa(1,4,l)*shpi(2,j)
            bd(2,1) = aa(2,1,l)*shpi(1,j) + aa(2,4,l)*shpi(2,j)
            bd(4,1) = aa(4,1,l)*shpi(1,j) + aa(4,4,l)*shpi(2,j)
            bd(1,2) = aa(1,2,l)*shpi(2,j) + aa(1,4,l)*shpi(1,j)
            bd(2,2) = aa(2,2,l)*shpi(2,j) + aa(2,4,l)*shpi(1,j)
            bd(4,2) = aa(4,2,l)*shpi(2,j) + aa(4,4,l)*shpi(1,j)
            do i = 1,2
              hh(2*i-1,2*j-1) = hh(2*i-1,2*j-1) + shpi(1,i)*bd(1,1)
     &                                          + shpi(2,i)*bd(4,1)
              hh(2*i-1,2*j  ) = hh(2*i-1,2*j  ) + shpi(1,i)*bd(1,2)
     &                                          + shpi(2,i)*bd(4,2)
              hh(2*i  ,2*j-1) = hh(2*i  ,2*j-1) + shpi(2,i)*bd(2,1)
     &                                          + shpi(1,i)*bd(4,1)
              hh(2*i  ,2*j  ) = hh(2*i  ,2*j  ) + shpi(2,i)*bd(2,2)
     &                                          + shpi(1,i)*bd(4,2)
            end do ! i
          end do ! j
          nn = nn + nhv
        end do ! l

        call invert(hh,5,5)

!       Compute incremental enhanced displacements enhanced modes

        do i = 1,5
          dui(i)  = hh(i,1)*bb(1) + hh(i,2)*bb(2) + hh(i,3)*bb(3)
     &            + hh(i,4)*bb(4) + hh(i,5)*bb(5)
          ui(i,1) = ui(i,1) + dui(i)
          ui(i,2) = ui(i,2) + dui(i)
        end do ! i

!       Check convergence

        tol1 = abs(bb(1)*dui(1) + bb(2)*dui(2) + bb(3)*dui(3)
     &           + bb(4)*dui(4) + bb(5)*dui(5))

        if(tol1.le.tolu .and. nenit.ge.1) then
          noconv = .false.
        endif
        nenit = nenit + 1
        if(nenit.ge.3 .or. tolu.eq.0.0d0) then
          noconv = .false.
        endif
      end do ! while

!     Save enhanced modes

      do i = 1,5
        hr(nh2-1+i) = ui(i,1)
      end do ! i

!     Set initial counter for history terms in stress/strain

      if(isw.eq.3. or. isw.eq.6) then

!       Time integration order set to static or dynamic

        if(d(7).ge.0.0) then
          cfac = d(7)
          lfac = 1.0d0 - cfac
        else
          cfac = 0.0d0
          lfac = 0.0d0
        endif

        do i = 1,8
          do j = 1,5
            gg(j,i) = 0.0d0
          end do ! j
        end do ! i

        do l = 1,lint

          call shpi2d(sg2(1,l),jac(l),xl,ndm)

          if(stype.eq.3) then
            shpi(3,3) = shpi(3,3)/r0(l)
            jac0      = jac(l)*sg2(3,l)
          else
            shpi(3,3) = 0.0d0
            jac0      = 0.0d0
          endif

!         Rayleigh damping effects

          if(d(78).ne.0.0d0) then
            call rays2d(d,shp2(1,1,l),sig(1,l),dd(1,1,5,l),ul(1,1,4),
     &                  xl,ndf,ndm,nel)
            sfac = d(78)*ctan(2)
          else
            sfac = 0.0d0
          endif

!         Residual computations

          call resid2d(cfac,lfac,dvol(l),jac0,shp2(1,1,l),eps,
     &                 sig(1,l),d,ul(1,1,4),ul(1,1,5), p,ndf,l)

!         Stiffness computations

          if(isw.eq.3) then

            dmas = d(4)*(ctan(3) + d(77)*ctan(2))*dvol(l)

            do j = 1,4
              do i = 1,4
                aa(i,j,l) = aa(i,j,l) + dd(i,j,5,l)*sfac
              end do ! i
            end do ! j

            j1 = 0
            j2 = 0
            do j = 1,nel

!             Compute d * b matrix = a

              do i = 1,4
                bd(i,1) = aa(i,1,l)*shp2(1,j,l) + aa(i,4,l)*shp2(2,j,l)
     &                  + aa(i,3,l)*shpr(j,l)
                bd(i,2) = aa(i,2,l)*shp2(2,j,l) + aa(i,4,l)*shp2(1,j,l)
              end do ! i

!             Lumped mass effects

              lms = shp2(3,j,l)*dmas
              do jj = 1,2
                s(j1+jj,j1+jj) = s(j1+jj,j1+jj) + lms*lfac
              end do ! jj

              i1 = 0
              do i = 1,nel
                cms = lms*shp2(3,i,l)*cfac
                do jj = 1,2
                  s(i1+jj,j1+jj) = s(i1+jj,j1+jj) + cms
                  s(i1+1 ,j1+jj) = s(i1+1 ,j1+jj) + shp2(1,i,l)*bd(1,jj)
     &                                            + shp2(2,i,l)*bd(4,jj)
     &                                            + shpr(i,l) *bd(3,jj)
                  s(i1+2 ,j1+jj) = s(i1+2 ,j1+jj) + shp2(2,i,l)*bd(2,jj)
     &                                            + shp2(1,i,l)*bd(4,jj)
                end do ! jj
                i1 = i1 + ndf
              end do ! i

!             Enhanced coupling array

              do jj = 1,2
                do i = 1,2
                  gg(2*i-1,j2+jj) = gg(2*i-1,j2+jj) + shpi(1,i)*bd(1,jj)
     &                                              + shpi(2,i)*bd(4,jj)

                  gg(2*i  ,j2+jj) = gg(2*i  ,j2+jj) + shpi(2,i)*bd(2,jj)
     &                                              + shpi(1,i)*bd(4,jj)
                end do ! i
                gg(5,j2+jj) = gg(5,j2+jj) + shpi(3,3)*bd(3,jj)
              end do ! jj
              j1 = j1 + ndf
              j2 = j2 + 2
            end do ! j
          endif
        end do ! l

!       Eliminate enhanced modes

        do i = 1,5
          do j = 1,8
            hg(i,j) = hh(1,i)*gg(1,j) + hh(2,i)*gg(2,j)
     &              + hh(3,i)*gg(3,j) + hh(4,i)*gg(4,j)
     &              + hh(5,i)*gg(5,j)
          end do ! j
        end do ! i

        if(isw.eq.3) then
          do j = 1,8
            do i = 1,8
              ss(i,j) = gg(1,i)*hg(1,j) + gg(2,i)*hg(2,j)
     &                + gg(3,i)*hg(3,j) + gg(4,i)*hg(4,j)
     &                + gg(5,i)*hg(5,j)
            end do ! i
          end do ! j

!         Construct static condensation

          j1 = 0
          j2 = 0
          do j = 1,4
            i1 = 0
            i2 = 0
            do i = 1,4
              do jj = 1,2
                do ii = 1,2
                  s(i1+ii,j1+jj) = s(i1+ii,j1+jj) - ss(i2+ii,j2+jj)
                end do ! ii
              end do ! jj
              i1 = i1 + ndf
              i2 = i2 + 2
            end do ! i
            j1 = j1 + ndf
            j2 = j2 + 2
          end do ! j

!         Compute reduced residual

          j2 = 0
          do j = 1,4
            do jj = 1,2
              p(sa(j)+jj) = p(sa(j)+jj) - hg(1,j2+jj)*bb(1)
     &                                  - hg(2,j2+jj)*bb(2)
     &                                  - hg(3,j2+jj)*bb(3)
     &                                  - hg(4,j2+jj)*bb(4)
     &                                  - hg(5,j2+jj)*bb(5)
            end do ! jj
            j2     = j2 + 2
          end do ! j
        endif

!       Multiply by thickness if not unity

        if((isw.eq.3 .or. isw.eq.6) .and. d(14).ne.1.d0) then

          do j = 1,nst
            do i = 1,nst
              s(i,j) = s(i,j)*d(14)
            end do ! i
          end do ! j
          do j = 1,nel
            do i = 1,2
              p(sa(j)+i) = p(sa(j)+i)*d(14)
            end do ! i
          end do ! j

        endif

!     Compute and output element variables

      elseif(isw.eq.4 .or. isw.eq.8) then

        l = nint(d(5))
        if(l*l.ne.lint) call int2d(l,lint,sg2)

!       Set initial counter for history terms in stress/strain

        nn = ni
        do l = 1,lint
          call shp2d(sg2(1,l),xl,shp2(1,1,l),jac(l),ndm,nel,ix,.false.)
          call shpi2d(sg2(1,l),jac(l),xl,ndm)

!         Compute stress and strain at point

          call strn2d(d,xl,ul,tl,shp2(1,1,l),ndf,ndm,nel,rr,zz,ta,eps)

          call modlsd(l,d,ta,eps,hr(nn+nh1),hr(nn+nh2),nhv,istrt,
     &                dd,sig(1,l), isw)

!         Compute principal stress values

          if(isw.eq.4) then
            mct = mct - 4
            call pstr2d(sig(1,l),sig(5,l))
            if(mct.le.0) then
              write(iow,2001) o,head
              if(ior.lt.0) write(*,2001) o,head
              mct = 50
            endif
            write(iow,2002) n_el,ma,rr,zz,(sig(i,l),i=7,9),
     &                     (sig(i,l),i=1,4),(eps(i,1),i=1,4)
            if(ior.lt.0) then
              write(*,2002) n_el,ma,rr,zz,(sig(i,l),i=7,9),
     &                     (sig(i,l),i=1,4),(eps(i,1),i=1,4)
            endif
          endif
          nn = nn + nhv
        end do ! l

!       Project stress values to nodes

        if(isw.eq.8) then

          call slcn2d(sig,p,s,p(nen+1),nel,10)

        endif

      endif

!     Formats

2001  format(a1,20a4//5x,'Element Stresses'//'     Elmt Mat',
     &    4x,'1-coord    2-coord   1-stress   2-stress      Angle'/
     &   15x,'11-stress  22-stress  33-stress  12-stress',
     &   15x,'11-strain  22-strain  33-strain  12-strain'/39(' -'))
2002  format(i9,i4,0p,2f11.3,1p,3e11.3/13x,1p,4e11.3/13x,1p,4e11.3/)

      end subroutine sld2d3
