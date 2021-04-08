!$Id:$
      subroutine sld2d1(d,ul,xl,ix,tl,s,p,ndf,ndm,nst,isw)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Purpose: Plane and axisymmetric linear displacement routine
!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'bdata.h'
      include  'cdata.h'
      include  'eldata.h'
      include  'eltran.h'
      include  'fdata.h'
      include  'hdata.h'
      include  'iofile.h'
      include  'oelmt.h'
      include  'pmod2d.h'
      include  'prstrs.h'
      include  'qudshp.h'
      include  'strnum.h'
      include  'comblk.h'

      integer       :: ndf,ndm,nst,isw
      integer       :: j,k,l,ii,j1,k1,nhv,nn,istrt

      real (kind=8) :: jac0,dv,ta
      real (kind=8) :: aj1,aj2,aj3,aj0,xx,yy,lfac,cfac,sfac
      real (kind=8) :: bd11,bd21,bd12,bd22,bd13,bd23,bd14,bd24

      integer       :: ix(*)
      real (kind=8) :: d(*),ul(ndf,nen,*),xl(ndm,*),tl(*)
      real (kind=8) :: sig(9,64),eps(9,3),s(nst,nst),p(*)
      real (kind=8) :: dd(6,6,5),mass(64,64),shpr(64)

      save

      data      eps/ 27*0.0d0 /

!     Compute stress-divergence vector (p) and stiffness matrix (s)
      nhv   = nint(d(15))
      istrt = nint(d(84))

!     No action for isw = 1
      if(isw.eq.1) then

      elseif(isw.eq.3  .or. isw.eq.6 .or. isw.eq.14) then

!       Integration order set to static
        if(d(7).lt.0.0d0) then
          cfac = 0.0d0
          lfac = 0.0d0
        else
          cfac = d(7)
          lfac = 1.d0 - cfac
        endif

!       Compute gauss quadrature points and weights
        call quadr2d(d,.true.)

!       Zero mass matrix
        mass(:,:) = 0.0d0
        shpr(:)   = 0.0d0

!       Numerical integration loop
        nn = 0
        do l = 1,lint

          call interp2d(l, xl,ix, ndm,nel, .false.)

!         Compute stresses and strains
          call strn2d(d,xl,ul,tl,shp2(1,1,l),ndf,ndm,nel,
     &                xx,yy,ta,eps)
          call modlsd(l,d,ta,eps,hr(nh1+nn),hr(nh2+nn),nhv,istrt,
     &                dd,sig(1,l),isw)

          if(isw.eq.3 .or. isw.eq.6) then

!           Multiply jacobian by radius for axisymmetry
            if(stype.eq.3) then
              jac0   = jac(l)
              jac(l) = jac(l)*xx
              do j = 1,nel
                shpr(j) = shp2(3,j,l)/xx
              end do ! j
            else
              jac0 = 0.0d0
            end if

!           Set element parameters for multiscale
            v_avg  = v_avg + jac(l)
            v_rho  = v_rho + jac(l)*d(4)
            sig_33 = sig_33 + jac(l)*sig(3,l)

!           Rayleigh Damping
            dv = d(4)*(ctan(3) + d(77)*ctan(2))*jac(l)

            if(d(78).ne.0.0d0) then
              call rays2d(d,shp2(1,1,l),sig(1,l),dd(1,1,5),ul(1,1,4),xl,
     &                    ndf,ndm,nel)
              sfac = d(78)*ctan(2)
            else
              sfac = 0.0d0
            endif

!           Compute gravity, thermal, inertia, and stress contributions
            call resid2d(cfac,lfac,jac(l),jac0,shp2(1,1,l),eps,sig(1,l),
     &                   d,ul(1,1,4),ul(1,1,5),p,ndf,l)

!           Loop over rows
            if(isw.eq.3) then

!             Modify tangent for stiffness rayleigh damping
              do j = 1,4
                do k = 1,4
                  dd(k,j,1) = dd(k,j,1)*ctan(1) + dd(k,j,5)*sfac
                end do
              end do

              j1     = 1
              do j = 1,nel
                aj1 = shp2(1,j,l)*jac(l)
                aj2 = shp2(2,j,l)*jac(l)
                aj3 = shp2(3,j,l)*jac0

!               Compute B_trans * D * j * w
                bd11 = aj1*dd(1,1,1) + aj3*dd(3,1,1) + aj2*dd(4,1,1)
                bd12 = aj1*dd(1,2,1) + aj3*dd(3,2,1) + aj2*dd(4,2,1)
                bd13 = aj1*dd(1,3,1) + aj3*dd(3,3,1) + aj2*dd(4,3,1)
                bd14 = aj1*dd(1,4,1) + aj3*dd(3,4,1) + aj2*dd(4,4,1)

                bd21 = aj2*dd(2,1,1) + aj1*dd(4,1,1)
                bd22 = aj2*dd(2,2,1) + aj1*dd(4,2,1)
                bd23 = aj2*dd(2,3,1) + aj1*dd(4,3,1)
                bd24 = aj2*dd(2,4,1) + aj1*dd(4,4,1)

!               Compute lumped mass matrix
                aj0       = shp2(3,j,l)*dv
                mass(j,j) = mass(j,j) + aj0*lfac

!               Loop over columns (symmetry noted)
                k1 = 1
                do k = 1,nel
                  s(j1  ,k1  ) = s(j1  ,k1  ) + bd11*shp2(1,k,l)
     &                                        + bd14*shp2(2,k,l)
     &                                        + bd13*shpr(k)

                  s(j1  ,k1+1) = s(j1  ,k1+1) + bd12*shp2(2,k,l)
     &                                        + bd14*shp2(1,k,l)

                  s(j1+1,k1  ) = s(j1+1,k1  ) + bd21*shp2(1,k,l)
     &                                        + bd24*shp2(2,k,l)
     &                                        + bd23*shpr(k)

                  s(j1+1,k1+1) = s(j1+1,k1+1) + bd22*shp2(2,k,l)
     &                                        + bd24*shp2(1,k,l)

!                 Compute consistent mass matrix
                  mass(j,k)    = mass(j,k)    + cfac*aj0*shp2(3,k,l)
                  k1 = k1 + ndf
                end do ! k
                j1 = j1 + ndf
              end do ! j
            end if
          end if
          nn = nn + nhv
        end do ! l

        if(isw.eq.3) then

!         Assemble mass matrix into tangent
          j1 = 0
          do j = 1,ndf*nel,ndf
            j1 = j1 + 1
            k1 = 0
            do k = 1,ndf*nel,ndf
              k1  = k1 + 1
              s(j  ,k  ) = s(j  ,k  ) + mass(j1,k1)
              s(j+1,k+1) = s(j+1,k+1) + mass(j1,k1)
            end do ! k
          end do ! j
        endif

!     Output of element quantities
      elseif(isw.eq.4 .or. isw.eq.8) then

        call quadr2d(d,.true.)

!       Compute element stresses, strains, and forces
        nn = 0
        do l = 1,lint

!         Compute element shape functions
          call interp2d(l, xl,ix, ndm,nel, .false.)

!         Compute strains and coordinates
          call strn2d(d,xl,ul,tl,shp2(1,1,l),ndf,ndm,nel,
     &                xx,yy,ta,eps)
          call modlsd(l,d,ta,eps,hr(nh1+nn),hr(nh2+nn),nhv,istrt,
     &                dd,sig(1,l),isw)

          if(d(14).gt.0.0d0 .and. d(14).ne.1.d0) then
            do ii = 1,4
              sig(ii,l) = sig(ii,l)/d(14)
            end do
          endif

          if(isw.eq.4) then

            call pstr2d(sig(1,l),sig(7,l))

!           Output stresses and strains
            mct = mct - 2
            if(mct.le.0) then
            write(iow,2001) o,head
              if(ior.lt.0 .and. pfr) then
                write(*,2001) o,head
              endif
              mct = 50
            endif
            write(iow,2002) n_el,ma,sig(9,l),sig(1:4,l),sig(7,l),
     &                        xx,yy,eps(1:4,1),sig(8,l)
            if(ior.lt.0 .and. pfr) then
              write(*,2002) n_el,ma,sig(9,l),sig(1:4,l),sig(7,l),
     &                        xx,yy,eps(1:4,1),sig(8,l)
            endif
          endif
          nn = nn + nhv
        end do ! l

!       Compute nodal stress values
        if(isw.eq.8) then

!         Project stress values to nodes
          call slcn2d(sig,p,s,p(nen+1),nel,9)

        endif
      endif

!     Formats

2001  format(a1,20a4//5x,'Element Stresses'//'    Elmt Mat Angle',
     &   '   11-stress   22-stress   33-stress   12-stress',
     &   '    1-stress'/'  1-coord  2-coord   11-strain',
     &   '   22-strain   33-strain   12-strain    2-stress')
2002  format(i8,i4,0p,f6.1,1p,5e12.3/0p,2f9.3,1p,5e12.3/1x)

      end subroutine sld2d1
