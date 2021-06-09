!$Id:$
      subroutine sld1d1(d,ul,xl,tl,s,p,ndf,ndm,nst,isw)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Plane and axisymmetric linear elastic element routine

!      Inputs:
!         d(*)  - Element parameters
!         ul(ndf,*) - Current nodal solution parameters
!         xl(ndm,*) - Nodal coordinates
!         ndf       - Degree of freedoms/node
!         ndm       - Mesh coordinate dimension
!         nst       - Element array dimension
!         isw       - Solution option switch

!      Outputs:
!         s(nst,*)  - Element array
!         p(ndf,*)  - Element vector
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'bdata.h'
      include  'cdata.h'
      include  'eldata.h'
      include  'elplot.h'
      include  'eltran.h'
      include  'fdata.h'
      include  'hdata.h'
      include  'iofile.h'
      include  'pmod2d.h'
      include  'prstrs.h'
      include  'ptdat6.h'
      include  'qudshp.h'
      include  'strnum.h'
      include  'rdata.h'
      include  'comblk.h'

      integer       :: ndf,ndm,nst,isw,j,k,l,ii,j1,k1,nhv,nn,istrt
      real (kind=8) :: aj1,aj2,aj0,xx,lfac,cfac,sfac,jacc,dv,ta
      real (kind=8) :: bd11,bd12,bd13, bt1
      real (kind=8) :: d(*),ul(ndf,nen,*),xl(ndm,*),tl(*)
      real (kind=8) :: s(nst,*),p(ndf,*)
      real (kind=8) :: sig(10,10),eps(9,3),epsv(10),epsl(3,10)
      real (kind=8) :: dd(6,6,5),shpr(4)

      save

      data      eps     / 27*0.0d0 /

!     Compute stress-divergence vector (p) and stiffness matrix (s)

      nhv   = nint(d(15))
      istrt = nint(d(84))

      if(isw.eq.3 .or. isw.eq.6 .or. isw.eq.13 .or. isw.eq.14) then

!       Integration order set to static

        if((d(7).ge.0.0 .or. d(183).ne.0.0d0) .and. shflg) then
          cfac = d(7)
          lfac = 1.d0 - cfac
        else
          cfac = 0.0d0
          lfac = 0.0d0
        endif

!       Compute Gauss quadrature points and weights

        call quadr1d(d)

!       Zero shpr matrix

        do j = 1,nel
          shpr(j) = 0.0d0
        end do ! j

!       Numerical integration loop

        nn = 0
        do l = 1,lint
          call interp1d(l, xl, ndm,nel,.false.)

!         Compute stresses and strains

          call stra1d(d,xl,ul,tl,shp1(1,1,l),ndf,ndm,nel, xx,ta,eps)
          call modlsd(l,d,ta,eps,hr(nh1+nn),hr(nh2+nn),nhv,istrt,
     &                dd,sig(1,l),isw)

          if(isw.eq.3 .or. isw.eq.6) then

!           Multiply jacobian by radius for axisymmetry

            if(stype.eq.3) then
              jacc   = jac(l)
              jac(l) = jac(l)*xx
              do j = 1,nel
                shpr(j) = shp1(2,j,l)/xx
              end do ! j
            elseif(stype.eq.9) then
              jacc   = jac(l)*xx
              jac(l) = jac(l)*xx*xx
              do j = 1,nel
                shpr(j) = shp1(2,j,l)/xx
              end do ! j
            else
              jacc = 0.0d0
            end if

!           Store time history plot data for element

            k = 10*(l-1)
            do j = 1,3
              tt(j+k) = sig(j,l)
            end do ! j
            k = k + 6
            do j = 1,3
              tt(j+k) = eps(j,1)
            end do ! j

!           Rayleigh damping effects

            dv = d(4)*(ctan(3) + d(77)*ctan(2))*jac(l)

            if(d(78).ne.0.0d0) then
              call rays1d(d,shp1(1,1,l),sig(1,l),dd(1,1,5),ul(1,1,4),xl,
     &                    ndf,ndm,nel)
              sfac = d(78)*ctan(2)
            else
              sfac = 0.0d0
            endif

!           Compute gravity, thermal, inertia, and stress contributions

            call resid1d(cfac,lfac,jac(l),jacc,shp1(1,1,l),sig(1,l),d,
     &                   ul(1,1,4),ul(1,1,5),p,ndf)

!           Tangent stiffness computation

            if(isw.eq.3) then

!             Modify tangent for stiffness rayleigh damping

              do j = 1,3
                do k = 1,3
                  dd(k,j,1) = dd(k,j,1)*ctan(1) + dd(k,j,5)*sfac
                end do ! k
!               Thermo-mechanical coupling
                dd(k,1,2) = dd(k,1,2)*ctan(1)
              end do ! j

              j1 = 1

              do j = 1,nel

                aj1 = shp1(1,j,l)*jac(l)
                aj2 = shp1(2,j,l)*jacc

!               Compute B_trans * D * j * w

                bd11 = aj1*dd(1,1,1) + aj2*dd(3,1,1)
                bd12 = 0.0d0
                bd13 = aj1*dd(1,3,1) + aj2*dd(3,3,1)

!               Compute B_trans * D * alpha * j * w

                bt1  = aj1*dd(1,1,2) + aj2*dd(3,1,2)

                if(stype.eq.9) then  ! Spherical symmetry
                  bd11 = bd11 + aj2*dd(2,1,1)
                  bd12 = aj1*dd(1,2,1) + aj2*(dd(2,2,1) + dd(3,2,1))
                  bd13 = bd13 + aj2*dd(2,3,1)
                  bt1  = bt1  + aj2*dd(2,1,2)
                endif

!               Compute lumped mass matrix

                aj0          = shp1(2,j,l)*dv
                s(j1  ,j1  ) = s(j1  ,j1  ) + aj0*lfac

!               Loop over columns (symmetry noted)

                k1 = 1
                do k = 1,nel
                  s(j1  ,k1  ) = s(j1  ,k1  ) + bd11*shp1(1,k,l)
     &                                        + bd12*shpr(k)
     &                                        + bd13*shpr(k)
     &                                        + cfac*aj0*shp1(2,k,l)
                  k1 = k1 + ndf
                end do ! k

                j1 = j1 + ndf
              end do ! j

            end if
          end if
          nn = nn + nhv
        end do ! l

!     Output of element quantities

      elseif(isw.eq.4 .or. isw.eq.8) then

        call quadr1d(d)

!       Compute element stresses, strains, and forces

        nn = 0
        do l = 1,lint

!         Compute element shape functions

          call interp1d(l, xl, ndm,nel,.false.)

!         Compute strains and coordinates

          call stra1d(d,xl,ul,tl,shp1(1,1,l),ndf,ndm,nel,
     &                xx,ta,eps)
          call modlsd(l,d,ta,eps,hr(nh1+nn),hr(nh2+nn),nhv,istrt,
     &                dd,sig(1,l),isw)

!         Compute volumetric strain

          epsv(l) = eps(1,1) + eps(2,1) + eps(3,1)

          if(isw.eq.4) then

!           Output stresses and strains

            mct = mct - 2
            if(mct.le.0) then
            write(iow,2001) o,head
              if(ior.lt.0 .and. pfr) then
                write(*,2001) o,head
              endif
              mct = 50
            endif
            write(iow,2002)  n_el,ma,(sig(ii,l),ii=1,3),
     &                            xx,(eps(ii,1),ii=1,3)
            if(ior.lt.0 .and. pfr) then
              write(*,2002)  n_el,ma,(sig(ii,l),ii=1,3),
     &                            xx,(eps(ii,1),ii=1,3)
            endif
          elseif(isw.eq.8) then
            do ii = 1,3
              epsl(ii,l) = eps(ii,1)
            end do ! ii
          endif
          nn = nn + nhv
        end do ! l

!       Compute nodal stress values

        if(isw.eq.8) then
          call slcn1d(sig,epsl,shp1,jac,p,s, lint,nel,10)
        endif

      endif

!     Formats for input-output

2001  format(a1,20a4//5x,'Displacement Model Element Solutions'//
     &   '    Elmt Mat   11-stress   22-stress   33-stress'/
     &   '     1-coord   11-strain   22-strain   33-strain')
2002  format(i8,i4,1p,3e12.4/1p,4e12.4)

      end subroutine sld1d1
