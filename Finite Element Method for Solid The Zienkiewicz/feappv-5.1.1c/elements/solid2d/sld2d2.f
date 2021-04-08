!$Id:$
      subroutine sld2d2(d,ul,xl,ix,tl,s,p,ndf,ndm,nst,isw)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Purpose: Plane/axisymmetric linear element routine - B-bar formulation
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'bdata.h'
      include  'cdata.h'
      include  'eltran.h'
      include  'eldata.h'
      include  'elm2d.h'
      include  'elplot.h'
      include  'hdata.h'
      include  'tdata.h'
      include  'iofile.h'
      include  'pmod2d.h'
      include  'prld1.h'
      include  'prstrs.h'
      include  'qudshp.h'
      include  'strnum.h'
      include  'comblk.h'

      logical       :: flg

      integer       :: ndm,ndf,nst,isw
      integer       :: i,j,ii,i1,jj,j1,k,l,nhv,nn, istrt
      real (kind=8) :: type,xr0,xr1,xz0,b1,b2,dv,dl,ddm,rr,zz,w11,ta
      real (kind=8) :: cfac,lfac

      integer       :: ix(*)
      real (kind=8) :: d(*),ul(ndf,nen,*),xl(ndm,*),tl(*),s(nst,*),p(*)
      real (kind=8) :: eps(9,3),aa(6,6,5),dd(6,6),sigv(9,9),sig(7)
      real (kind=8) :: bbd(4,2),vl(2),al(2),ac(2)
      real (kind=8) :: epsd(6),bbar(4,2,4)

      real (kind=8) :: dot

      save

      data      eps / 27*0.0d0 /

      nhv   = nint(d(15))
      istrt = nint(d(84))
      type = max(0,stype-2)

!     No action for isw = 1

      if(isw.eq.1) then

!     Compute residual and tangent

      elseif(isw.eq.3 .or. isw.eq.6 .or. isw.eq. 14) then

        flg  = isw .eq. 3
        nn   = 0
        call quadr2d(d,.true.)

        call gvec2d(xl,ul,type,ndm,ndf)

        do l = 1,lint

          call interp2d(l, xl,ix, ndm,nel, .false.)

!         Compute displacement gradient and incr. displ. gradient

          call strn2m(d,xr1,shp2(1,1,l),xl,ul,tl,type,xr0,xz0,
     &                ndm,ndf,nel,nen,ta,eps)

!         Compute Cauchy stresses and spatial tangent tensor at t-n+1

          call modlsd(l,d,ta,eps,hr(nh1+nn),hr(nh2+nn),nhv,istrt,
     &                aa,sig, isw)

          if(isw.eq.3 .or. isw.eq.6) then

            jac(l) = jac(l)*xr1

!           Integration order set to static

            if(d(7).lt.0.0d0) then
              cfac = 0.0d0
              lfac = 0.0d0
            else
              cfac = d(7)
              lfac = 1.d0 - cfac
            endif

!           Store time history plot data for element

            i = 6*(l-1)
            do j = 1,6
              tt(j+i) = sig(j)
            end do ! j

!           Compute b-bar matrix

            do i = 1,nel
              call bmat2d(type,xr0,shp2(1,i,l),g(1,i),bbar(1,1,i))
            end do

!           Proportional body forces

            if(int(d(74)).gt.0) then
              b1 = d(11) + prldv(int(d(74)))*d(71)
            else
              b1 = d(11)*dm
            endif
            if(int(d(75)).gt.0) then
              b2 = d(12) + prldv(int(d(75)))*d(72)
            else
              b2 = d(12)*dm
            endif

!           Rayleigh damping

            if(d(77).ne.0.0d0) then
              do k = 1,2
                vl(k) = 0.0d0
                do j = 1,nel
                  vl(k) = vl(k) + shp2(3,j,l)*ul(k,j,4)
                end do
              end do
              vl(1)   = cfac*vl(1)
              vl(2)   = cfac*vl(2)

!             Compute mass damping residual

              do i = 1,nel
                w11    = shp2(3,i,l)*jac(l)*d(77)*d(4)
                p(sa(i)+1) = p(sa(i)+1) - (vl(1) + lfac*ul(1,i,4))*w11
                p(sa(i)+2) = p(sa(i)+2) - (vl(2) + lfac*ul(2,i,4))*w11
              end do ! i
            endif

            if(d(78).ne.0.0d0) then
              do i = 1,4
                epsd(i) = 0.0d0
                do j = 1,nel
                  epsd(i) = epsd(i) + bbar(i,1,j)*ul(1,j,4)
     &                              + bbar(i,2,j)*ul(2,j,4)
                end do ! j
              end do ! i
              do i = 1,4
                epsd(i) = epsd(i)*d(78)
                do j = 1,4
                  sig(j) = sig(j) + aa(j,i,1)*epsd(i)
                end do ! j
              end do ! i
            endif

!           Multiply tangent moduli and stress by volume element

            dv  = jac(l)*(ctan(1) + d(78)*ctan(2))
            do i = 1,4
              sig(i) = sig(i)*jac(l)
              do j = 1,4
                dd(i,j) = aa(i,j,1)*dv
              end do
            end do

!           Compute accelerations

            do k = 1,2
              al(k) = 0.0d0
              do j = 1,nel
                al(k) = al(k) + shp2(3,j,l)*ul(k,j,5)
              end do
            end do
            al(1)   = cfac*al(1)
            al(2)   = cfac*al(2)

!           Loop over rows

            dv = (ctan(3) + d(77)*ctan(2))*jac(l)*d(4)*cfac
            dl = (ctan(3) + d(77)*ctan(2))*jac(l)*d(4)*lfac

            i1 = 0
            do i = 1,nel

!           Compute body forces and inertial loading

              ac(1)   = d(4)*(al(1) + lfac*ul(1,i,5))
              ac(2)   = d(4)*(al(2) + lfac*ul(2,i,5))

              p(sa(i)+1) = p(sa(i)+1) + (b1 - ac(1))*shp2(3,i,l)*jac(l)
              p(sa(i)+2) = p(sa(i)+2) + (b2 - ac(2))*shp2(3,i,l)*jac(l)

!             Compute internal stress divergence term

              do ii = 1,2
                p(sa(i)+ii) = p(sa(i)+ii) - dot(bbar(1,ii,i),sig(1),4)
              end do

!             Compute bbar-t * aa * dvol

              if(flg) then
                do ii = 1,2
                  do jj = 1,4
                    bbd(jj,ii) = dot(bbar(1,ii,i),dd(1,jj),4)
                  end do
                end do

!               Compute tangent stiffness

                w11 = shp2(3,i,l)*dl
                do ii = 1,2
                  s(i1+ii,i1+ii) = s(i1+ii,i1+ii) + w11
                end do

                w11 = shp2(3,i,l)*dv
                j1 = 0
                do j  = 1,nel
                  do ii = 1,2
                    do jj = 1,2
                      s(i1+ii,j1+jj) = s(i1+ii,j1+jj)
     &                               + dot(bbd(1,ii),bbar(1,jj,j),4)
                    end do
                    s(i1+ii,j1+ii) = s(i1+ii,j1+ii) + w11*shp2(3,j,l)
                    end do
                  j1 = j1 + ndf
                end do
              end if
              i1 = i1 + ndf
            end do
          endif
          nn = nn + nhv
        end do

!       Form lower part by symmetry

        if(flg) then
          do i = 1,nst
            do j = i,nst
              s(j,i) = s(i,j)
            end do
          end do
        endif

!     Output stresses

      elseif(isw.eq.4 .or.isw.eq.8) then

        call quadr2d(d,.true.)
        nn   =  0

!       Compute element stresses

        call gvec2d(xl,ul,type,ndm,ndf)
        ddm = 0.0d0
        rr = 0.0d0
        zz = 0.0d0
        do i = 1,5
          sig(i) = 0.0d0
          do j = 1,4
            sigv(i,j) = 0.0d0
          end do
        end do
        do l = 1,lint

          call interp2d(l, xl,ix, ndm,nel, .false.)

!         Compute displacement gradient and incr. displ. gradient

          call strn2m(d,xr1,shp2(1,1,l),xl,ul,tl,type,xr0,xz0,
     &                ndm,ndf,nel,nen,ta,eps)

!         Compute Cauchy stresses and spatial tangent tensor at t-n+1

          call modlsd(l,d,ta,eps,hr(nn+nh1),hr(nn+nh2),nhv,istrt,
     &                aa,sigv(1,l), isw)
          ddm = ddm + sigv(7,l)*0.25
          rr = rr + xr0*0.25
          zz = zz + xz0*0.25

          if(d(14).gt.0.0d0 .and. d(14).ne.1.d0) then
            do ii = 1,4
              sigv(ii,l) = sigv(ii,l)/d(14)
            end do
          endif

!         Move stresses for printing

          sig(1) = sig(1) + 0.25d0*sigv(1,l)
          sig(2) = sig(2) + 0.25d0*sigv(2,l)
          sig(3) = sig(3) + 0.25d0*sigv(3,l)
          sig(4) = sig(4) + 0.25d0*sigv(4,l)
          nn = nn + nhv
        end do

        if(isw.eq.8) then

!         Stress computations for nodes

          call slcn2d(sigv,p,s,p(nen+1),nel,9)

        else

!         Output stresses

          call pstr2d(sig,sig(5))

          mct = mct - 2
          if(mct.le.0) then
            write(iow,2001) o,head
            if(ior.lt.0) then
              write(*,2001) o,head
            endif
            mct = 50
          end if
          write(iow,2002) n_el,ma,(sig(ii),ii=1,6),rr,zz,ddm,sig(7)
          if(ior.lt.0) then
            write(*,2002) n_el,ma,(sig(ii),ii=1,6),rr,zz,ddm,sig(7)
          endif

        endif

      endif

!     Formats for input-output

2001  format(a1,20a4//'  element stresses'//'  elmt  matl  11-stress'
     &,'  22-stress  33-stress  12-stress   1-stress   2-stress'/
     & '  1-coord  2-coord  yield ? ',34x,'angle')

2002  format(2i6,6e11.3/2f9.3,f10.4,31x,f8.2/1x)

      end subroutine sld2d2
