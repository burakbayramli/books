!$Id:$
      subroutine sld2d2n(d,ul,xl,ix,tl,s,r,ndf,ndm,nst,isw)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Purpose: Two-dimensional mixed u-p-theta small deformation element

!      Inputs:
!         d(*)  - Element parameters
!         ul(ndf,*) - Current nodal solution parameters
!         xl(ndm,*) - Nodal coordinates
!         ix(*)     - Global nodal connections
!         tl(*)     - Nodal temp vector
!         ndf       - Degree of freedoms/node
!         ndm       - Mesh coordinate dimension
!         nst       - Element array dimension
!         isw       - Solution option switch

!      Outputs:
!         s(nst,*)  - Element array
!         r(*)      - Element vector
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'augdat.h'
      include  'bdata.h'
      include  'cdata.h'
      include  'debugs.h'
      include  'eldata.h'
      include  'elengy.h'
      include  'elplot.h'
      include  'eltran.h'
      include  'fdata.h'
      include  'hdata.h'
      include  'iofile.h'
      include  'pmod2d.h'
      include  'prstrs.h'
      include  'ptdat6.h'
      include  'p_int.h'
      include  'qudshp.h'
      include  'rdata.h'
      include  'comblk.h'

      integer   ndf,ndm,nst,isw,i,i1,j,jj,j1,l, nq
      integer   nhv,istrt,nn, ix(*)
      real*8    epp,dv,dl,d1,type
      real*8    dsigtr, dmass, dmshp, dtheta, cfac,lfac, fac
      real*8    d(*),       ul(ndf,nen,*),  xl(ndm,*),   tl(*), s(nst,*)
      real*8    r(*),       xx(2,64)
      real*8    bbd(3,7),   aa(6,6,5,64),   dd(7,7)
      real*8    sigm(9),    sigl(16,64),    bpra(3)
      real*8    al(3),      ac(3),          x0(2)
      real*8    theta(3,64),pbar(64),       eps(9,3,64)
      real*8    irad(64),   ta(64)
      real*8    bdy(3),     epsm(9),        eps3(64)

!     Arrays for history recovery

      real*8    ttn(6),ttl(6),dtl(6), rth(6),rhh(6)
      real*8    hh(6,6),gg(6,3,16),gh(6,3,16)

      save

!     TEMPORARY SET OF TEMPERATURE

      data    ta      / 64*0.0d0 /

!     Data inputs

      if( isw.eq.1 ) then

!       Check problem dimension

        if(ndf.lt.3) then
          write(iow,4001) ndf
          call plstop(.true.)
        endif

!       Set storage for theta history

        if(nen.le.4) then
          nh1 = nh1 +  1
          nh3 = nh3 + 15
        elseif(nen.le.9) then
          nh1 = nh1 +  3
          nh3 = nh3 + 99
        else
          nh1 = nh1 +  6
          nh3 = nh3 + 336
        endif

!     Augmented Lagrangian update for nested iteration

      elseif(isw.eq.10) then

        call quadr2d(d,.true.)

        d1    = augf*d(185)
        fp(1) = nh2 - 1
        fp(2) = fp(1) + npm
        do i = 1,npm
          hr(fp(1)+i) = hr(fp(1)+i) + d1*hr(fp(2)+i)
        end do ! i

!     Compute tangent stiffness and residual force vector

      elseif(isw.eq. 3 .or. isw.eq. 4 .or. isw.eq. 6 .or.
     &       isw.eq. 8 .or. isw.eq.14) then

!       Recover volumetric terms and update theta

        if(isw.lt.14) then

          if(nel.le.4) then
            nq = 1
          elseif(nel.le.9) then
            nq = 3
          else
            nq = 6
          endif

          do i = 0,nq-1
            ttn(i+1) = hr(nh1+i)
            ttl(i+1) = hr(nh2+i)
            rth(i+1) = hr(nh3+i)
          end do ! i
          i1 = nq
          do i = 1,nq
            do j = 1,nel
              gh(i,1,j) = hr(nh3+i1  )
              gh(i,2,j) = hr(nh3+i1+1)
              gh(i,3,j) = hr(nh3+i1+2)
              i1        = i1 + 3
            end do ! j
          end do ! i

!         Update theta by last incremental displacements

          do i = 1,nq
            dtl(i) = rth(i)
            do j = 1,nel
              dtl(i) = dtl(i) - gh(i,1,j)*ul(1,j,3)
     &                        - gh(i,2,j)*ul(2,j,3)
     &                        - gh(i,3,j)*ul(3,j,3)
            end do ! j
            ttl(i) = ttl(i) + dtl(i)
          end do ! i

!         Zero arrays for computations
          do i = 1,nq
            rth(i) = 0.0d0
          end do ! i
          do i = 1,nq
            do j = 1,nq
              hh(i,j) = 0.0d0
            end do ! j
            do j = 1,nel
              gg(i,1,j) = 0.0d0
              gg(i,2,j) = 0.0d0
              gg(i,3,j) = 0.0d0
            end do ! j
          end do ! i

        endif

!       Integration order set to static

        if(d(7).ge.0.0) then
          cfac = d(7)
          lfac = 1.d0 - cfac
        else
          cfac = 0.0d0
          lfac = 0.0d0
        endif

!       Proportional body forces

        call sbodyf(d, bdy)

        estore = 0.0d0

!       Set number of history terms / quadradure point

        type   = max(0,stype - 2)
        nhv    = nint(d(15))
        istrt  = nint(d(84))

!       Center estimate

        if(nq.gt.1) then
          do i = 1,2
            x0(i) = 0.0d0
            do j = 1,nel
              x0(i) = x0(i) + xl(i,j)
            end do ! j
            x0(i) = x0(i)/dble(nel)
          end do ! i
        endif

!       Set element quadrature order

        call quadr2d(d,.true.)

!       MECHANICAL ELEMENT

        nn = 0
        do l = 1,lint

!         Shape functions and derivatives

          call interp2d(l, xl,ix, ndm,nel, .false.)

!         Compute coordinates

          xx(1,l) = 0.0d0
          xx(2,l) = 0.0d0
          do i = 1,nel
            xx(1,l) = xx(1,l) + shp2(3,i,l)*xl(1,i)
            xx(2,l) = xx(2,l) + shp2(3,i,l)*xl(2,i)
          end do ! i

!         Set the pressure functions

          phi(1,l) = 1.d0
          if(nq.gt.1) then
            phi(2,l) = xx(1,l) - x0(1)
            phi(3,l) = xx(2,l) - x0(2)
            phi(4,l) = phi(2,l)**2
            phi(5,l) = phi(2,l)*phi(3,l)
            phi(6,l) = phi(3,l)**2
          endif

!         Compute mixed volumetric strain and pressure from nodal values

          if(stype.lt.3) then
            irad(l) = 0.0d0
          else
            jac(l)  = jac(l)*xx(1,l)
            irad(l) = 1.d0/xx(1,l)
          endif

!         Mixed volume effect and temperature projection

          do i = 1,3
            theta(i,l) = 0.0d0
          end do ! i
          do i = 1,nq
            theta(1,l) = theta(1,l)  + phi(i,l) * ttl(i)
            theta(2,l) = theta(2,l)  + phi(i,l) * ttn(i)
          end do ! i
          eps3(l)  =  0.0d0
          press(l) =  0.0d0

          ta(l)    = -d(9)

          do i = 1,nel
            fac        = shp2(1,i,l) + shp2(3,i,l) * irad(l)
            eps3(l)    = eps3(l)     + fac        *ul(1,i,1)
     &                               + shp2(2,i,l)*ul(2,i,1)

            press(l)   = press(l)    + shpm(i,l) * ul(3,i,1)

            ta(l)      = ta(l)       + shp2(3,i,l) * tl(i)
          end do ! i

!         Compute strains and stresses at quadrature points

          call strn2n(shp2(1,1,l),xl,ul,theta(1,l),irad(l),
     &                ndm,ndf,nel,nen,eps(1,1,l))

          call modlsd(l,d,ta(l),eps(1,1,l),hr(nn+nh1),hr(nn+nh2),nhv,
     &                istrt,aa(1,1,1,l),sigl(1,l),isw)

!         Volumetric stress from constitution

          pbar(l) = (sigl(1,l) + sigl(2,l) + sigl(3,l))/3.0d0

          nn = nn + nhv
        end do ! l

!       Compute mixed stress

        do l = 1,lint
          dsigtr    =  press(l)  - pbar(l)
          sigl(1,l) =  sigl(1,l) + dsigtr
          sigl(2,l) =  sigl(2,l) + dsigtr
          sigl(3,l) =  sigl(3,l) + dsigtr
        end do ! l

!       Tangent and residual computations

        if(isw.eq.3 .or. isw.eq.6 .or. isw.eq.14) then

!         Compute mixed pressure

          if(isw.eq.3 .or. isw.eq.6) then


            do l = 1,lint

!             Store time history plot data for element

              i = 6*(l-1)
              do j = 1,6
                tt(j+i) = sigl(j,l)
                sigm(j) = sigl(j,l)*jac(l)
              end do ! j

!             Compute acceleration

              dmass = d(4)*jac(l)
              do i = 1,2
                al(i) = 0.0d0
                do j = 1,nel
                  al(i) = al(i) + shp2(3,j,l)*ul(i,j,5)
                end do ! j
                al(i) = al(i)*cfac
              end do ! i

!             Compute residual

              do j = 1,nel

                do i = 1,2
                  ac(i)  = d(4)*(al(i) + lfac*ul(i,j,5))
                end do ! i

!               Displacement variation residual

                r(sa(j)+1) = r(sa(j)+1)
     &                     + (bdy(1) - ac(1))*shp2(3,j,l)*jac(l)
     &                     - shp2(1,j,l)*sigm(1)
     &                     - shp2(2,j,l)*sigm(4)
     &                     - shp2(3,j,l)*sigm(3)*irad(l)

                r(sa(j)+2) = r(sa(j)+2)
     &                     + (bdy(2) - ac(2))*shp2(3,j,l)*jac(l)
     &                     - shp2(1,j,l)*sigm(4)
     &                     - shp2(2,j,l)*sigm(2)

!               Pressure variation residual

                r(sa(j)+3) = r(sa(j)+3)
     &                     + shpm(j,l)*(theta(1,l)-eps3(l))*jac(l)

              end do ! j

!             Volumetric variation residual

              do j = 1,nq
                rth(j) = rth(j) + phi(j,l)*(press(l) - pbar(l))*jac(l)
              end do ! j

!             Multiply tangent moduli by volume element

              call dmatmx( aa(1,1,1,l), dd )
              d1 = jac(l)*ctan(1)
              do i = 1,7
                do j = 1,7
                  dd(i,j) = dd(i,j)*d1
                end do ! j
              end do ! i

!             Compute mixed tangent stiffness matrix

              if(isw.eq.3) then

!               Mass factors

                dv = ctan(3)*cfac
                dl = ctan(3)*lfac

!               Compute row terms

                i1 = 0
                do i = 1,nel

!                 Compute bmat-t * dd * dvol

                  do jj = 1,7

                    bbd(1,jj) =  shp2(1,i,l)*dd(1,jj)
     &                        +  shp2(2,i,l)*dd(4,jj)
     &                        +  shp2(3,i,l)*dd(3,jj)*irad(l)

                    bbd(2,jj) =  shp2(2,i,l)*dd(2,jj)
     &                        +  shp2(1,i,l)*dd(4,jj)

                  end do ! jj

!                 Compute tangent stiffness

                  fac = shp2(3,i,l)*dl
                  do jj = 1,2
                    s(i1+jj,i1+jj) = s(i1+jj,i1+jj) + fac
                  end do ! jj

                  dmshp = shp2(3,i,l)*dv

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
     &                              + bbd(jj,3)*shp2(3,j,l)*irad(l)

                      s(i1+jj,j1+2) = s(i1+jj,j1+2)
     &                              + bbd(jj,2)*shp2(2,j,l)
     &                              + bbd(jj,4)*shp2(1,j,l)
                    end do ! jj

!                   Pressure part

                    s(i1+3,j1+1) = s(i1+3,j1+1)
     &                           + shpm(i,l)*(shp2(1,j,l)
     &                           + shp2(3,j,l)*irad(l))*jac(l)

                    s(i1+1,j1+3) = s(i1+1,j1+3)
     &                           + shpm(j,l)*(shp2(1,i,l)
     &                           + shp2(3,i,l)*irad(l))*jac(l)

                    s(i1+3,j1+2) = s(i1+3,j1+2)
     &                           + shpm(i,l)*shp2(2,j,l)*jac(l)

                    s(i1+2,j1+3) = s(i1+2,j1+3)
     &                           + shpm(j,l)*shp2(2,i,l)*jac(l)

                    j1 = j1 + ndf
                  end do ! j

!                 Volumetric tangent terms

                  do j = 1,nq
                    gg(j,1,i) = gg(j,1,i) + (shp2(1,i,l)*dd(1,7)
     &                           +  shp2(3,i,l)*dd(3,7)*irad(l)
     &                           +  shp2(2,i,l)*dd(4,7)) * phi(j,l)

                    gg(j,2,i) = gg(j,2,i) + (shp2(2,i,l)*dd(2,7)
     &                           +  shp2(1,i,l)*dd(4,7)) * phi(j,l)

                    gg(j,3,i) = gg(j,3,i) - shpm(i,l)*phi(j,l)*jac(l)
                  end do ! j
                  i1 = i1 + ndf
                end do ! i

              endif ! isw = 3

!             Volumetric diagonal block

              do i = 1,nq
                do j = 1,nq
                  hh(i,j) = hh(i,j) + phi(i,l)*dd(7,7)*phi(j,l)
                end do ! j
              end do ! i

            end do ! l

!           Condense stiffness & save arrays to recover theta parameters

!           Invert hh

            call invert(hh,nq,6)

!           Multiply inverse of hh into rth

            do i = 1,nq
              rhh(i)    = 0.0d0
              do j = 1,nq
                rhh(i) = rhh(i) + hh(i,j)*rth(j)
              end do ! j
            end do ! i

!           Reduce residual with theta term

            do j = 1,nel
              do l = 1,nq
                r(sa(j)+1) = r(sa(j)+1) - gg(l,1,j)*rhh(l)
                r(sa(j)+2) = r(sa(j)+2) - gg(l,2,j)*rhh(l)
                r(sa(j)+3) = r(sa(j)+3) - gg(l,3,j)*rhh(l)
              end do ! l
            end do ! j

!           Save solution arrays

            do i = 0,nq-1
              hr(nh2+i) = ttl(i+1)     ! Current theta parameters
              hr(nh3+i) = rhh(i+1)     ! Current theta residual
            end do ! i

!           Condense stiffness

            if(isw.eq.3) then

!             Multiply inverse of hh into gg

              do j = 1,nel
                do i = 1,nq
                  gh(i,1,j) = 0.0d0
                  gh(i,2,j) = 0.0d0
                  gh(i,3,j) = 0.0d0
                end do ! i
                do i = 1,nq
                  do l = 1,nq
                    gh(i,1,j) = gh(i,1,j) + hh(i,l)*gg(l,1,j)
                    gh(i,2,j) = gh(i,2,j) + hh(i,l)*gg(l,2,j)
                    gh(i,3,j) = gh(i,3,j) + hh(i,l)*gg(l,3,j)
                  end do ! l
                end do ! i
              end do ! j

!             Reduce stiffness matrix

              j1 = 0
              do j = 1,nel
                i1 = 0
                do i = 1,nel
                  do l = 1,nq
                    s(i1+1,j1+1) = s(i1+1,j1+1) - gg(l,1,i)*gh(l,1,j)
                    s(i1+1,j1+2) = s(i1+1,j1+2) - gg(l,1,i)*gh(l,2,j)
                    s(i1+1,j1+3) = s(i1+1,j1+3) - gg(l,1,i)*gh(l,3,j)
                    s(i1+2,j1+1) = s(i1+2,j1+1) - gg(l,2,i)*gh(l,1,j)
                    s(i1+2,j1+2) = s(i1+2,j1+2) - gg(l,2,i)*gh(l,2,j)
                    s(i1+2,j1+3) = s(i1+2,j1+3) - gg(l,2,i)*gh(l,3,j)
                    s(i1+3,j1+1) = s(i1+3,j1+1) - gg(l,3,i)*gh(l,1,j)
                    s(i1+3,j1+2) = s(i1+3,j1+2) - gg(l,3,i)*gh(l,2,j)
                    s(i1+3,j1+3) = s(i1+3,j1+3) - gg(l,3,i)*gh(l,3,j)
                  end do ! l
                  i1 = i1 + ndf
                end do ! i
                j1 = j1 + ndf
              end do ! j

!             Save arrays

              i1 = nq
              do i = 1,nq
                do j = 1,nel
                  hr(nh3+i1  ) = gh(i,1,j)
                  hr(nh3+i1+1) = gh(i,2,j)
                  hr(nh3+i1+2) = gh(i,3,j)
                  i1       = i1 + 3
                end do ! j
              end do ! i
            endif  ! isw.eq.3

!           Multiply by thickness if not unity

            if(d(14).ne.1.d0) then

              do j = 1,nst
                do i = 1,nst
                  s(i,j) = s(i,j)*d(14)
                end do ! i
              end do ! j
              do j = 1,nel
                do i = 1,ndf
                  r(sa(j)+i) = r(sa(j)+i)*d(14)
                end do ! i
              end do ! j

            endif ! d(14) .ne. 1

          endif  ! isw.eq.3 .or isw.eq.6

!       Output stresses.

        elseif(isw.eq.4 .or. isw.eq.8) then

          do i = 1,9
            sigm(i) = 0.0d0
          end do ! i
          do i = 1,3
            bpra(i) = 0.0d0
          end do ! i
          epp    = 0.0d0
          dtheta = 0.0d0

!         Output stresses

          if (isw .eq. 4) then

            do l = 1,lint
              do i = 1,3
                sigm(i  ) = sigl(i  ,l)
                sigm(i+3) = sigl(i+3,l)
                epsm(i  ) = eps (i  ,1,l)
                epsm(i+3) = eps (i+3,1,l)*0.5d0
              end do ! i

              mct = mct - 4
              call pstr2d(sigm,sigm(7))
              call pstr2d(epsm,epsm(7))
              if(mct.le.0) then
                write(iow,2003) o,head
                if(ior.lt.0) write(*,2003) o,head
                mct = 50
              endif
              write(iow,2004) n_el,ma,xx(1,l),xx(2,l),
     &                       (sigm(i),i=7,9),(epsm(i),i=7,9),
     &                       (sigm(i),i=1,4),(eps(i,1,l),i=1,4)
              if(ior.lt.0) then
                write(*,2004) n_el,ma,xx(1,l),xx(2,l),
     &                       (sigm(i),i=7,9),(epsm(i),i=7,9),
     &                       (sigm(i),i=1,4),(eps(i,1,l),i=1,4)
              endif
            end do ! l

!         Project stresses onto nodes

          else
            call slcn2d(sigl,r,s,r(nen+1),nel,16)
          endif

        endif ! isw = 4 or 8

      endif ! isw = 3 or 4 or 6 or 8 or 14 or 16

!     Formats

2003  format(a1,20a4//5x,'Element Stresses and Strains'//
     &      5x,'Elmt Mat    1-coord    2-coord'/
     &   15x,' 1-stress   2-stress      Angle',
     &    2x,' 1-strain   2-strain      Angle'/
     &   15x,'11-stress  22-stress  33-stress  12-stress'/
     &   15x,'11-strain  22-strain  33-strain  12-strain'/39(' -'))
2004  format(i9,i4,0p,2f11.3/13x,2(1p,2e11.3,0p,f11.2)/(13x,1p,4e11.3))

4001  format(' ** ERROR **: Mixed U-P element: NDF must be 3 or more.'/
     &       '    INPUT = ',i4)

      end
