!$Id:$
      subroutine fld3d1(d,ul,xl,s,r,ndf,ndm,nst,isw)

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: 3-D finite deformation displacement element
!      Remark: This a completely standard mechanical element
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'bdata.h'
      include  'cdata.h'
      include  'eldata.h'
      include  'elengy.h'
      include  'elplot.h'
      include  'eltran.h'
      include  'hdata.h'
      include  'iofile.h'
      include  'ndata.h'
      include  'plast3f.h'
      include  'pmod2d.h'
      include  'prstrs.h'
      include  'rdata.h'
      include  'comblk.h'

      logical       :: tetfl
      integer       :: ndf,ndm,nst,isw, i,ii,i1, j,jj,j1,  k
      integer       :: l,lint, nhv,nn,istrt
      real (kind=8) :: bdb, ta
      real (kind=8) :: qfact, xlamd, ha
      real (kind=8) :: d(*),ul(ndf,nen,*),xl(ndm,*),s(nst,*),r(ndf,*)
      real (kind=8) :: sg(4,9),sv(5,16),xsj(9),xx(3)
      real (kind=8) :: f(9,2,9),finv(3,3,9),df(3,3,9),detfi(2,9)
      real (kind=8) :: shp(4,8,9),be(6), body(3)
      real (kind=8) :: aa(6,6,5),dd(6,6,9), xu(3,9)
      real (kind=8) :: sigv(13),sigl(6,9), sigp(6,9)
      real (kind=8) :: bbd(3,6),r1(3,9)
      real (kind=8) :: dvol(9),dvol0(9),weng(9),xr(3,9),ur(3,9)

      save

!     MATERIAL DATA

      if(isw.eq.1) then

        ta    = 0.0d0

!     N.B. Augmenting not allowed in displacement model

      elseif(isw.eq.10) then

!     COMPUTE TANGENT STIFFNESS/RESIDUAL FORCE AND OUTPUTS

      else

!       Get quadrature information

        if(nel.eq.4) then
          tetfl = .true.
          l     =  1
          call tint3d (l,lint,sv)
        else
          tetfl = .false.
          l     = nint(d(5))
          call int3d(l,lint,sg)
        endif

!       Compute shape functions

        do l = 1,lint
          if(tetfl) then
            call tetshp(sv(1,l),xl,ndm,xsj(l),shp(1,1,l))
            dvol0(l) = xsj(l)*sv(5,l)
          else
            call shp3d(sg(1,l),xsj(l),shp(1,1,l),xl,ndm)
            dvol0(l) = xsj(l)*sg(4,l)
          endif
        end do ! l

!       Compute coordinates

        do l = 1,lint
          do i = 1,3
            xr(i,l) = 0.0d0
            ur(i,l) = 0.0d0
            do j = 1,nel
              xr(i,l) = xr(i,l) + shp(4,j,l)*xl(i,j)
              ur(i,l) = ur(i,l) + shp(4,j,l)*ul(i,j,1)
            end do ! j
            ur(i,l) = ur(i,l) + xr(i,l)
          end do ! i
        end do ! l

!       Set history type and size

        nhv   = nint(d(15))
        istrt = nint(d(84))

!       Initialize history variables

        if(isw.eq.14) then

          do l = 1,lint
            f(:,1,l)    = 0.0d0
            f(:,2,l)    = 0.0d0
            finv(:,:,l) = 0.0d0
            detfi(1,l)  = 1.d0
            detfi(2,l)  = 1.d0
            do i = 1,9,4
              f(i,1,l)    = 1.0d0
              f(i,2,l)    = 1.0d0
            end do ! i
            finv(1,1,l) = 1.0d0
            finv(2,2,l) = 1.0d0
            finv(3,3,l) = 1.0d0
          end do ! l

        else

!         Compute deformation gradient, inverse and determinant.

          do l = 1,lint
            call kine3df(shp(1,1,l),ul,f(1,1,l),finv(1,1,l),df(1,1,l),
     &                   detfi(1,l),ndf,nel,nen)
            dvol(l) = dvol0(l)*detfi(1,l)
          end do ! l

        endif

!       Compute Cauchy stresses and spatial tangent tensor at t-n+1

        nn  = 0
        do l = 1,lint

          xlamd  = 0.0d0
          estore = 0.0d0
          call modlfd(l,d,f(1,1,l),df(1,1,l),detfi(1,l),ta,
     &                hr(nh1+nn),hr(nh2+nn),nhv,istrt,aa,sigv,be,
     &                xlamd,ha,.false.,isw)
          weng(l) = estore

!         Multiply tangent moduli and stresses by volume element.
!         Store time history plot data for element

          k = 6*(l-1)
          do i = 1,6
            tt(i+k)   = sigv(i)
            sigp(i,l) = sigv(i)
            sigl(i,l) = sigv(i)*dvol(l)
            do j = 1,6
              dd(j,i,l) = aa(j,i,1)*dvol(l)*ctan(1)
            end do ! j
          end do ! i
          nn = nn + nhv

        end do ! l

!       STIFFNESS AND RESIDUAL

        if(isw.eq.3 .or. isw.eq.6) then

!         Compute body forces values

          call sbodyf(d, body)

!         Compute inertia effects: shflg = .true. for eigen shifts

          if(ctan(3).ne.0.0 .or. shflg) then
            call iner3d(d,xl,ul(1,1,4),ul(1,1,5),s,r, nel,ndf,ndm,nst)
          endif ! ctan(3) test

          do l = 1,lint

!           COMPUTE STRESS DIVERGENCE TERM

!           Compatible internal force.

            do i = 1,nel
              r1(1,i) = shp(1,i,l)*sigl(1,l)
     &                + shp(2,i,l)*sigl(4,l)
     &                + shp(3,i,l)*sigl(6,l)
              r1(2,i) = shp(1,i,l)*sigl(4,l)
     &                + shp(2,i,l)*sigl(2,l)
     &                + shp(3,i,l)*sigl(5,l)
              r1(3,i) = shp(1,i,l)*sigl(6,l)
     &                + shp(2,i,l)*sigl(5,l)
     &                + shp(3,i,l)*sigl(3,l)

              do j = 1,3
                r(j,i)  = r(j,i) + shp(4,i,l)*body(j)*dvol0(l)
     &                  - r1(j,i)
              end do ! j
            end do ! i

!           COMPUTE K11

            if(isw.eq.3) then

              i1 = 0
              do i = 1,nel

!               PART 1. - geometric tangenta

                j1 = 0
                if(gflag) then
                  do j = 1,nel

!                   Accumulate geometric factor with consistent mass

                    bdb = (r1(1,i)*shp(1,j,l)
     &                  +  r1(2,i)*shp(2,j,l)
     &                  +  r1(3,i)*shp(3,j,l))*ctan(1)

                    do jj = 1,3
                      s(i1+jj,j1+jj) = s(i1+jj,j1+jj) + bdb
                    end do ! jj
                    j1 = j1 + ndf
                  end do ! j
                endif

!               PART 2. - tangent modulus part (based upon aa-array)

                do jj = 1,6
                  bbd(1,jj) = shp(1,i,l)*dd(1,jj,l)
     &                      + shp(2,i,l)*dd(4,jj,l)
     &                      + shp(3,i,l)*dd(6,jj,l)
                  bbd(2,jj) = shp(1,i,l)*dd(4,jj,l)
     &                      + shp(2,i,l)*dd(2,jj,l)
     &                      + shp(3,i,l)*dd(5,jj,l)
                  bbd(3,jj) = shp(1,i,l)*dd(6,jj,l)
     &                      + shp(2,i,l)*dd(5,jj,l)
     &                      + shp(3,i,l)*dd(3,jj,l)
                end do ! jj

!               Compute tangent stiffness

                j1 = 0
                do j  = 1,nel
                  s(i1+1,j1+1) = s(i1+1,j1+1)
     &                          + bbd(1,1)*shp(1,j,l)
     &                          + bbd(1,4)*shp(2,j,l)
     &                          + bbd(1,6)*shp(3,j,l)
                  s(i1+1,j1+2) = s(i1+1,j1+2)
     &                          + bbd(1,4)*shp(1,j,l)
     &                          + bbd(1,2)*shp(2,j,l)
     &                          + bbd(1,5)*shp(3,j,l)
                  s(i1+1,j1+3) = s(i1+1,j1+3)
     &                          + bbd(1,6)*shp(1,j,l)
     &                          + bbd(1,5)*shp(2,j,l)
     &                          + bbd(1,3)*shp(3,j,l)
                  s(i1+2,j1+1) = s(i1+2,j1+1)
     &                          + bbd(2,1)*shp(1,j,l)
     &                          + bbd(2,4)*shp(2,j,l)
     &                          + bbd(2,6)*shp(3,j,l)
                  s(i1+2,j1+2) = s(i1+2,j1+2)
     &                          + bbd(2,4)*shp(1,j,l)
     &                          + bbd(2,2)*shp(2,j,l)
     &                          + bbd(2,5)*shp(3,j,l)
                  s(i1+2,j1+3) = s(i1+2,j1+3)
     &                          + bbd(2,6)*shp(1,j,l)
     &                          + bbd(2,5)*shp(2,j,l)
     &                          + bbd(2,3)*shp(3,j,l)
                  s(i1+3,j1+1) = s(i1+3,j1+1)
     &                          + bbd(3,1)*shp(1,j,l)
     &                          + bbd(3,4)*shp(2,j,l)
     &                          + bbd(3,6)*shp(3,j,l)
                  s(i1+3,j1+2) = s(i1+3,j1+2)
     &                          + bbd(3,4)*shp(1,j,l)
     &                          + bbd(3,2)*shp(2,j,l)
     &                          + bbd(3,5)*shp(3,j,l)
                  s(i1+3,j1+3) = s(i1+3,j1+3)
     &                          + bbd(3,6)*shp(1,j,l)
     &                          + bbd(3,5)*shp(2,j,l)
     &                          + bbd(3,3)*shp(3,j,l)
                  j1 = j1 + ndf
                end do ! j
                i1 = i1 + ndf
              end  do ! i

            endif

          end do ! l

!       OUTPUT STRESSES

        elseif(isw.eq.4 .or. isw.eq.8 .or. isw.eq.16) then

          do i = 1,3
            xx(i) = 0.0d0
          end do ! i
          do i = 1,13
            sigv(i) = 0.0d0
          end do ! i
          qfact = 1.d0/dble(lint)

          do l = 1,lint

            do j = 1,3
              do i=1,nel
                xx(j)   = xx(j) + qfact*shp(4,i,l)*xl(j,i)
              end do ! i
            end do ! j

!           Move stresses and jacobian for printing

            do i = 1,6
              sigv(i) = sigv(i) + qfact*sigp(i,l)
            end do ! i

          end do ! l

!         OUTPUT STRESSES

          if (isw .eq. 4) then

            call pstr3d(sigv,sigv(7))

            mct = mct - 2
            if(mct.le.0) then
              write(iow,2001) o,head
              if(ior.lt.0) write(*,2001) o,head
              mct = 50
            endif

            write(iow,2002) n_el,(sigv(ii),ii=1,6),
     &                        ma,(sigv(ii),ii=7,9),xx
            if(ior.lt.0) then
              write(*,2002) n_el,(sigv(ii),ii=1,6),
     &                        ma,(sigv(ii),ii=7,9),xx
            end if

!         PROJECT STRESSES ONTO THE NODES FOR PLOTTING

          elseif(isw.eq.8) then

!           Compute current geometry

            do i = 1,ndm
              do j = 1,nel
                xu(i,j) = xl(i,j) + ul(i,j,1)
              end do ! j
            end do ! i

            call slcn3d(sigp, r,s, nel,8)

          endif
        endif
      endif

!     FORMAT STATEMENTS

2001  format(a1,20a4//5x,'Element Stresses'//
     &       '   Elem.   11-Stress   22-Stress   33-Stress   12-Stress',
     &   '   23-Stress   13-Stress'/
     &       '   Matl.    1-Stress    2-Stress    3-Stress',
     &   '     1-Coord     2-Coord     3-Coord ')

2002  format(i8,1p,6e12.4/i8,1p,6e12.4/1x)

      end subroutine fld3d1
