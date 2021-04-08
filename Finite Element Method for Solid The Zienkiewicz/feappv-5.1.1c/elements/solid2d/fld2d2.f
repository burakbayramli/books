!$Id:$
      subroutine fld2d2(d,ul,xl,ix,s,r,ndf,ndm,nst,isw)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: TWO DIMENSIONAL FINITE DEFORMATION ELEMENT
!         ***  Mixed Model, Operator Split Technique  ***
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'augdat.h'
      include  'bdata.h'
      include  'cdata.h'
      include  'eldata.h'
      include  'elengy.h'
      include  'elplot.h'
      include  'eltran.h'
      include  'hdata.h'
      include  'iofile.h'
      include  'pmod2d.h'
      include  'prstrs.h'
      include  'qudshp.h'
      include  'comblk.h'

      integer       :: ndf,ndm,nst, isw
      integer       :: i,i1, j,jj,j1, l, nhi,nhv,nn, istrt

      real (kind=8) :: d1,     bdb,    epp
      real (kind=8) :: third,  thlog,  ta,     qfact, xlamd, ha
      real (kind=8) :: dsigtr, dpress, mpress, dmass, dmshp, dtheta

      integer       :: ix(*)
      real (kind=8) :: d(*),       ul(ndf,nen,*),  xl(ndm,*),   s(nst,*)
      real (kind=8) :: df(9,9),    fi(9,2,9),      finv(9,9)
      real (kind=8) :: xxm(3),     xu(2,9),        ru(2,9),     r(*)
      real (kind=8) :: bbd(2,7),   bei(6),         ad(7,7,5,9), dd(7,7)
      real (kind=8) :: detf(2,9)
      real (kind=8) :: sigm(9),    sigl(9,9),      bpra(3),shpbar(2,9,9)
      real (kind=8) :: acc(2),     theta(2,9),     hh(3,3)
      real (kind=8) :: hsig(3)

      save

      data    third / 0.3333333333333333d0 /
      data    nhi   / 2 /

!     TEMPORARY SET OF TEMPERATURE

      data    ta    / 0.0d0 /

!     No action for isw = 1
      if(isw.eq.1) then

!     Augmented Lagrangian update for nested iteration
      elseif(isw.eq.10) then

        d1      = augf*d(185)
        hr(nh2) = hr(nh2) + d1*hr(nh2+1)

!     Compute tangent stiffness and residual force vector
      elseif(isw.eq.3 .or. isw.eq.4 .or. isw.eq. 6 .or.
     &       isw.eq.8 .or. isw.eq.14) then

        estore = 0.0d0

!       Compute current geometry
        do j = 1,nel
          do i = 1,2
            xu(i,j) = xl(i,j) + ul(i,j,1)
          end do ! i
        end do ! j

        call quadr2d(d,.true.)

!       Get shape functions and derivatives in geometry at time t_n+1
        do l = 1,lint
          call interp2d(l, xl,ix, ndm,nel, .false.)
        end do ! l

!       Set number of history terms / quadradure point
        nhv   = nint(d(15))
        istrt = nint(d(84))

!       MECHANICAL ELEMENT
        if(isw.eq.3 .or. isw.eq.6 .or. isw.eq.14) then

!         Compute f, df and det(fei) at conf t-n+1
          call kine2m(shp2,ul,fi,df,finv,ndf,nel,nen,detf,lint)

!         Mixed model for volumetric response
          call bbar2m(sg2,shp2,jac,detf,lint,nel,hh,theta,shpbar)

!         Compute mixed model deformation gradient
          call fbar2m(fi,detf,theta,lint)

!         Compute Cauchy stresses and spatial tangent tensor at t-n+1
          nn = nhi
          do l = 1,lint

            xlamd = hr(nh2)
            call modlfd(l,d,fi(1,1,l),df(1,l),theta(1,l),ta,
     &                  hr(nn+nh1),hr(nn+nh2),nhv,istrt,ad(1,1,1,l),
     &                  sigl(1,l),bei,xlamd,ha,.true.,isw)
            nn = nn + nhv
          end do ! l

!         Set augmented funciton
          hr(nh2+1) = ha

!         Compute mixed pressure
          if(isw.eq.3 .or. isw.eq.6) then

            if(nel.eq.4) then

              press(1) = 0.0d0
              do l = 1,lint

!               Modify volume element and integrate pressure
!               over reference volume
                press(1) = press(1) + third*(sigl(1,l) + sigl(2,l)
     &                              + sigl(3,l))*jac(l)
                dvol(l)  = jac(l) * theta(1,l)

              end do ! l

!             Divide pressure by reference volume
              press(1) = press(1) * hh(1,1)
              do l = 2,lint
                press(l) = press(1)
              end do ! l

            elseif(nel.eq.9) then

              sigm(1:3) = 0.0d0
              do l = 1,lint

!               Modify volume element and integrate pressure
!               over reference volume
                mpress   = third*(sigl(1,l) + sigl(2,l)
     &                          + sigl(3,l))*jac(l)
                sigm(1) = sigm(1) + mpress
                sigm(2) = sigm(2) + mpress*sg2(1,l)
                sigm(3) = sigm(3) + mpress*sg2(2,l)
                dvol(l) = jac(l) * theta(1,l)

              end do ! l

!             Divide pressure by reference volume
              do i = 1,3
                hsig(i) = hh(i,1)*sigm(1)
     &                  + hh(i,2)*sigm(2)
     &                  + hh(i,3)*sigm(3)
              end do ! i
              do l = 1,lint
                press(l) = hsig(1) + hsig(2)*sg2(1,l) + hsig(3)*sg2(2,l)
              end do ! l

            endif

            do l = 1,lint

!             Compute mixed stress and multiply by volume element
              dsigtr  = press(l)*detf(1,l)/theta(1,l)
     &                - (sigl(1,l)+sigl(2,l)+sigl(3,l))*third
              sigm(1) =  sigl(1,l) + dsigtr
              sigm(2) =  sigl(2,l) + dsigtr
              sigm(3) =  sigl(3,l) + dsigtr
              sigm(4) =  sigl(4,l)

!             Store time history plot data for element
              i = 6*(l-1)
              do j = 1,4
                tt(j+i) = sigm(j)
                sigm(j) = sigm(j)*dvol(l)
              end do ! j

!             Compute acceleration
              if(d(7).ge.0.0d0) then
                dmass = d(4)*jac(l)
              else
                dmass = 0.0d0
              endif
              do i = 1,2
                acc(i) = 0.0d0
                do j = 1,nel
                  acc(i) = acc(i) + shp2(3,j,l)*ul(i,j,5)
                end do ! j
                acc(i) = acc(i)*dmass
              end do ! i
              dmass = ctan(3)*dmass

!             Compute residual
              do j = 1,nel

                ru(1,j) = shp2(1,j,l)*sigm(1) + shp2(2,j,l)*sigm(4)
                ru(2,j) = shp2(1,j,l)*sigm(4) + shp2(2,j,l)*sigm(2)

                r(sa(j)+1)  = r(sa(j)+1) - ru(1,j) - shp2(3,j,l)*acc(1)
                r(sa(j)+2)  = r(sa(j)+2) - ru(2,j) - shp2(3,j,l)*acc(2)

              end do ! j

!             Compute mixed tangent stiffness matrix
              if(isw.eq.3) then

!               Part 1: Geometric tangent matrix
                if(gflag) then
                  i1 = 0
                  do i = 1,nel
                    j1 = 0
                    do j = 1,nel
                      bdb = shp2(1,i,l)*ru(1,j) + shp2(2,i,l)*ru(2,j)
                      do jj = 1,2
                        s(i1+jj,j1+jj) = s(i1+jj,j1+jj) + bdb
                      end do ! jj
                      j1 = j1 + ndf
                    end do ! j
                    i1 = i1 + ndf
                  end do ! i
                endif ! gflag

!               Part 2: Material tangent matrix

!               Modify tangent moduli for stress factors
                mpress = press(l)*detf(1,l)/theta(1,l)
                dpress = third*(sigl(1,l) + sigl(2,l) + sigl(3,l))

                call dmatdx(ad(1,1,1,l),sigl(1,l),dpress,mpress)

!               Multiply tangent moduli by volume element
                d1 = dvol(l)*ctan(1)
                do i = 1,7
                  do j = 1,7
                    dd(i,j) = ad(i,j,1,l)*d1
                  end do ! j
                end do ! i

!               Compute row terms
                i1 = 0
                do i = 1,nel

!                 Compute bmat-t * dd * dvol
                  do jj = 1,7

                    bbd(1,jj) =  shp2(1,i,l)*dd(1,jj)
     &                        +  shp2(2,i,l)*dd(4,jj)
     &                        + shpbar(1,i,l)*dd(7,jj)

                    bbd(2,jj) =  shp2(2,i,l)*dd(2,jj)
     &                        +  shp2(1,i,l)*dd(4,jj)
     &                        + shpbar(2,i,l)*dd(7,jj)
                  end do ! jj

                  dmshp = shp2(3,i,l)*dmass

                  j1 = 0
                  do j = 1,nel

!                   Inertial tangent
                    do jj = 1,2
                      s(i1+jj,j1+jj) = s(i1+jj,j1+jj)+dmshp*shp2(3,j,l)
                    end do ! jj

!                   Compute mechanics part of tangent stiffness
                    do jj = 1,2
                      s(i1+jj,j1+1) = s(i1+jj,j1+1)
     &                              + bbd(jj,1)*shp2(1,j,l)
     &                              + bbd(jj,4)*shp2(2,j,l)
     &                              + bbd(jj,7)*shpbar(1,j,l)

                      s(i1+jj,j1+2) = s(i1+jj,j1+2)
     &                              + bbd(jj,2)*shp2(2,j,l)
     &                              + bbd(jj,4)*shp2(1,j,l)
     &                              + bbd(jj,7)*shpbar(2,j,l)
                    end do ! jj

                    j1 = j1 + ndf
                  end do ! j
                  i1 = i1 + ndf
                end do ! i
              endif ! isw = 3
            end do ! l

          endif ! isw = 3 or 6

!       Output stresses.

        elseif(isw.eq.4 .or. isw.eq.8) then

          do i = 1,9
            sigm(i) = 0.0d0
          end do ! i
          do i = 1,3
            bpra(i) = 0.0d0
            xxm(i)  = 0.0d0
          end do ! i
          epp    = 0.0d0
          dtheta = 0.0d0
          qfact  = 1.d0/dble(lint)

!         Compute f, df and det(fei) at conf t-n+1

          call kine2m(shp2,ul,fi,df,finv,ndf,nel,nen,detf,lint)

          call bbar2m(sg2,shp2,dvol,detf,lint,nel,hh,theta,shpbar)

          call fbar2m(fi,detf,theta,lint)

!         Second loop over Gauss points

          nn  = nhi
          do l = 1,lint

!           Compute Cauchy stresses and spatial tangent tensor at t-n+1
            xlamd = hr(nh2)
            call modlfd(l,d,fi(1,1,l),df(1,l),theta(1,l),ta,
     &                  hr(nn+nh1),hr(nn+nh2),nhv,istrt,ad,sigl(1,l),
     &                  bei,xlamd,ha,.true.,isw)

!           Compute principal stretches
            call pstr3d(bei, bpr)

!           Average stresses and stretches for printing
            do i = 1,2
              bpra(i) = bpra(i) + 0.5d0*qfact*dlog(bpr(i))
              xxm(i)  = xxm(i)  + qfact *xu(i,l)
            end do ! i
            bpra(3) = bpra(3) + 0.5d0*qfact*dlog(bpr(3))
            do i = 1,4
              sigm(i) = sigm(i) + qfact*sigl(i,l)
            end do ! i
            epp      = epp      + qfact*sigl( 9,l)
            dtheta   = dtheta   + qfact*theta(1,l)
            nn = nn + nhv
          end do ! l

!         Output stresses
          if (isw .eq. 4) then

            call pstr2d(sigm,sigm(7))

            mct = mct - 2
            if(mct.le.0) then
              write(iow,2001) o,head
              if(ior.lt.0) write(*,2001) o,head
              mct = 50
            endif

!           Compute potential damage variable
            thlog = log(abs(dtheta))
            write(iow,2002) n_el,ma,(sigm(i),i=1,9),bpra,xxm,thlog,epp
            if(ior.lt.0) then
              write(*,2002) n_el,ma,(sigm(i),i=1,9),bpra,xxm,thlog,epp
            endif
          else

!           Project stress values to nodes
            call slcn2d(sigl,r,s,r(nen+1),nel,9)

          endif
        endif ! isw = 4 or 8

      endif ! isw = 3 or 4 or 6 or 8 or 14

!     Formats

2001  format(a1,20a4//5x,'Element Stresses'//'  Elmt  Matl',
     &   '  11-Stress  22-Stress  33-Stress  12-Stress  23-Stress',
     &   '  13-Stress'/12x,'   1-Stress   2-Stress      Angle',
     &   '  log(lam1)  log(lam2)  log(lam3)'/12x,'    1-Coord',
     &   '    2-Coord    3-Coord    log-J     eff-ep')

2002  format(2i6,1p6e11.3/12x,1p6e11.3/12x,0p3f11.5,1p2e11.3/1x)

      end subroutine fld2d2
