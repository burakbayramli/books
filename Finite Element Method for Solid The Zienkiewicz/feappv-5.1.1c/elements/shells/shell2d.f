!$Id:$
      subroutine shell2d(d,ul,xl,ix,s,r,ndf,ndm,nst,isw)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--+---------+---------+---------+---------+---------+---------+-]
!     Shell elements: 2-D Axisymmetric
!         (a) Finite motions with trigonometric functions
!         (a) Large displacement  with small rotations

!     Three integration points are used.

!     Arguments:
!        d(*)      - specified parameter array
!        ul(ndf,*) - local nodal solution values
!        xl(ndm,*) - local nodal coordinate values
!        ix(*)     - node numbers
!        s(nst,nst) - finite element array (stiffness, mass, geometric
!                                           stiffness)
!        r(nst)     - finite element array (residual, lumped mass)
!        ndf        - number of degree of freedoms at node ( > or = 3 )
!        ndm        - spatial dimension of element         (      = 2 )
!        nst        - size of finite element arrays        ( > or = 6 )
!        isw        - solution option
!                   = 1: Input values and store in d(*) array
!                   = 2: Check mesh coordinate and connection inputs
!                        for errors
!                   = 3: Compute element residual (p) and stiffness (s)
!                   = 4: Output element results
!                   = 5: Compute mass (p,s) or geometric stiffness array (s)
!                   = 6: Compute element residual (p)
!                   = 8: Compute nodal projections
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit  none

      include  'cdata.h'
      include  'eldata.h'
      include  'hdata.h'
      include  'iofile.h'
      include  'mdata.h'
      include  'pmod2d.h'
      include  'strnum.h'

      include  'comblk.h'

      integer       :: ndf,ndm,nst,isw, i, tdof

      integer       :: ix(*)
      real (kind=8) :: d(*),ul(ndf,nen,*),xl(ndm,*), s(nst,*),r(ndf,*)

      save

!     Go to correct array processor

      if(isw.eq.0 .and. ior.lt.0) then
        write(*,*) '   Shell: 2-d Axisymmetric Model'

!     Input material properties

      elseif(isw.eq.1) then
        write(iow,2000)
        if(ior.lt.0) write(*,2000)
        call inmate(d,tdof,ndf*2,4)

!       Set plot sequence

        pstyp = 1

!       Deactivate dof in element for dof > 3

        do i = 4,ndf
          ix(i) = 0
        end do

      else

        dtype = nint(d(18))

        if(nel.eq.2) then
          if(dtype.gt.0) then
            call shl2dn(d,ul,xl,s,r,ndf,ndm,nst,isw)
          elseif(dtype.lt.0) then
            call shl2df(d,ul,xl,s,r,ndf,ndm,nst,isw)
          else
            call shl2dn(d,ul,xl,s,r,ndf,ndm,nst,isw)
          endif
        else
          write(iow,3000) n_el
          if(ior.lt.0) write(*,3000) n_el
          call plstop(.true.)
        endif
      endif

!     Formats for input-output

2000  format(/5x,'E l a s t i c   S h e l l   E l e m e n t'/)

3000  format('*ERROR* Shell Element',i8,' has more than 2-nodes')

      end subroutine shell2d

      subroutine trsh2d(cosphi,sinphi,rr,ss, r,s, ndf,nst)

      implicit   none

      real (kind=8) :: cosphi,sinphi
      integer       :: ndf,nst, i,i1,i2, j,j1,j2, a

      real (kind=8) :: rr(3,*), ss(6,*), r(ndf,*), s(nst,*)
      real (kind=8) :: bd(3,3)

!     Transform to global coordinates

      i1 = 0
      i2 = 0
      do i = 1,2
        r(1,i) = cosphi*rr(1,i) - sinphi*rr(2,i)
        r(2,i) = sinphi*rr(1,i) + cosphi*rr(2,i)
        r(3,i) = rr(3,i)
        j1 = 0
        j2 = 0
        do j = 1,2
          do a = 1,3
            bd(1,a) = cosphi*ss(i1+1,j1+a) - sinphi*ss(i1+2,j1+a)
            bd(2,a) = sinphi*ss(i1+1,j1+a) + cosphi*ss(i1+2,j1+a)
            bd(3,a) = ss(i1+3,j1+a)
          end do ! a
          do a = 1,3
            s(i2+a,j2+1) = bd(a,1)*cosphi - bd(a,2)*sinphi
            s(i2+a,j2+2) = bd(a,1)*sinphi + bd(a,2)*cosphi
            s(i2+a,j2+3) = bd(a,3)
          end do ! a
          j1 = j1 + 3
          j2 = j2 + ndf
        end do ! j
        i1 = i1 + 3
        i2 = i2 + ndf
      end do ! i

      end subroutine trsh2d

      subroutine shl2df(d,ul,xl,s,r,ndf,ndm,nst,isw)

      implicit   none

      include   'cdata.h'
      include   'eltran.h'
      include   'evdata.h'

      integer       :: ndf,ndm,nst,isw
      integer       :: a,b, i,j, i1,j1

      real (kind=8) :: G1,G2,G3, B11,B12,Gh, D11,D12, r1, rh1,rh3, dvol
      real (kind=8) :: ur,du,dw,dk, ra,r2, za, cs,sn, hh, cosphi,sinphi
      real (kind=8) :: lam,gam,E_11,E_22,K_11,K_22
      real (kind=8) :: N_11,N_22,S_13,M_11,M_22

      real (kind=8) :: d(*),ul(ndf,nen,*),xl(ndm,*)
      real (kind=8) :: s(nst,*),r(ndf,*), ss(6,6),rr(3,2)

      real (kind=8) :: D_t(5,5), bb(5,3,2), bd(3,5), uu(3,2), shp(2,2)
      real (kind=8) :: aa(3,2)

      save

      if(isw.eq.1) then
      else

!       Shell properties

        B11 = d(1)*d(14)/(1.d0 - d(2)*d(2))   ! Et/(1.d0 - nu*nu)
        B12 = d(2)*B11
        Gh  = d(1)*d(14)/(1.d0+d(2))*0.5*d(10) ! kappa * Gt
        D11 = d(14)**2/12.d0*B11              ! D
        D12 = d(2)*D11

!       Shape functions for one point quadrature

        hh       =   sqrt((xl(1,2) - xl(1,1))**2
     +                  + (xl(2,2) - xl(2,1))**2)

        shp(1,1) = - 1.0d0/hh
        shp(2,1) =   0.5d0

        shp(1,2) =   1.0d0/hh
        shp(2,2) =   0.5d0

!       Compute local displacements and accelerations at nodes

        cosphi   =  (xl(1,2) - xl(1,1))/hh
        sinphi   =  (xl(2,2) - xl(2,1))/hh

        do i = 1,2
          uu(1,i) =   cosphi*ul(1,i,1) + sinphi*ul(2,i,1)
          uu(2,i) = - sinphi*ul(1,i,1) + cosphi*ul(2,i,1)
          uu(3,i) =   ul(3,i,1)
          aa(1,i) =   cosphi*ul(1,i,5) + sinphi*ul(2,i,5)
          aa(2,i) = - sinphi*ul(1,i,5) + cosphi*ul(2,i,5)
          aa(3,i) =   ul(3,i,5)
        end do

!       Compute local derivatives

        ra = (xl(1,1) + xl(1,2))*0.5d0
        za = (xl(2,1) + xl(2,2))*0.5d0
        r2 = ra*ra
        ur = (ul(1,1,1) + ul(1,2,1))*0.5d0
        du = (uu(1,2) - uu(1,1))/hh
        dw = (uu(2,2) - uu(2,1))/hh
        dk = (uu(3,2) - uu(3,1))/hh
        cs = cos(0.5d0*(uu(3,1) + uu(3,2)))
        sn = sin(0.5d0*(uu(3,1) + uu(3,2)))

        lam =  (1.d0 + du)*cs - dw*sn
        gam =  (1.d0 + du)*sn + dw*cs

!       Compute strains

        E_11 = du + 0.5*(du*du + dw*dw)
        E_22 = ur/ra + 0.5d0*(ur/ra)**2
        K_11 = lam*dk
        K_22 = (1.d0 + ur/ra)*sn/ra

!       Set the volume of the element

        dvol = hh*ra

!       Compute the force resultants

        N_11 =  (B11*E_11 + B12*E_22)*dvol
        N_22 =  (B12*E_11 + B11*E_22)*dvol
        S_13 =   Gh*gam *dvol
        M_11 =  (D11*K_11 + D12*K_22)*dvol
        M_22 =  (D12*K_11 + D11*K_22)*dvol

!       Compute stiffness factors

        G1  =  S_13*cs  - M_11*dk*sn
        G2  = -S_13*sn  - M_11*dk*cs
        G3  = -S_13*gam - M_11*dk*lam

        if(isw.eq.3 .or. isw.eq.6) then

!         Elastic 'tangent moduli'

          do i = 1,5
            do j = 1,5
              D_t(j,i) = 0.0d0
            end do ! j
          end do ! i
          D_t(1,1) = B11*dvol
          D_t(1,2) = B12*dvol
          D_t(2,1) = B12*dvol
          D_t(2,2) = B11*dvol
          D_t(3,3) = Gh*dvol
          D_t(4,4) = D11*dvol
          D_t(4,5) = D12*dvol
          D_t(5,4) = D12*dvol
          D_t(5,5) = D11*dvol

!         Strain-displacement matrix

          do i = 1,2
            bb(1,1,i) = (1.0d0 + du)*shp(1,i)
            bb(1,2,i) =          dw *shp(1,i)
            bb(1,3,i) =  0.0d0

            bb(2,1,i) = (1.d0 + ur/ra)*cosphi/ra*shp(2,i)
            bb(2,2,i) =-(1.d0 + ur/ra)*sinphi/ra*shp(2,i)
            bb(2,3,i) =  0.0d0

            bb(3,1,i) =  sn * shp(1,i)
            bb(3,2,i) =  cs * shp(1,i)
            bb(3,3,i) =  lam* shp(2,i)

            bb(4,1,i) =  dk * cs * shp(1,i)
            bb(4,2,i) = -dk * sn * shp(1,i)
            bb(4,3,i) = -gam* dk * shp(2,i) + lam*shp(1,i)

            bb(5,1,i) =  sn*cosphi/r2*shp(2,i)
            bb(5,2,i) = -sn*sinphi/r2*shp(2,i)
            bb(5,3,i) = (1.d0 + ur/ra)*cs/ra*shp(2,i)
          end do

!         Compute residual and tangent

          do a = 1,3
            rr(a,1) = 0.0d0
            rr(a,2) = 0.0d0
          end do ! a

          i1 = 0
          do i = 1,2

!           Compute the residual term

            do a = 1,3
              rr(a,i) = rr(a,i) - bb(1,a,i)*N_11
     &                          - bb(2,a,i)*N_22
     &                          - bb(3,a,i)*S_13
     &                          - bb(4,a,i)*M_11
     &                          - bb(5,a,i)*M_22
              do b = 1,5
                bd(a,b) = bb(1,a,i)*D_t(1,b)
     &                  + bb(2,a,i)*D_t(2,b)
     &                  + bb(3,a,i)*D_t(3,b)
     &                  + bb(4,a,i)*D_t(4,b)
     &                  + bb(5,a,i)*D_t(5,b)
              end do ! b
            end do ! a

            j1 = 0
            do j = 1,2
              do a = 1,3
                do b = 1,3
                  ss(i1+a,j1+b) = bd(a,1)*bb(1,b,j)
     &                          + bd(a,2)*bb(2,b,j)
     &                          + bd(a,3)*bb(3,b,j)
     &                          + bd(a,4)*bb(4,b,j)
     &                          + bd(a,5)*bb(5,b,j)
                end do ! b
              end do ! a
              ss(i1+1,j1+1) = ss(i1+1,j1+1) + shp(1,i)*N_11*shp(1,j)
     &                      + shp(2,i)*N_22/r2*cosphi*cosphi*shp(2,j)
              ss(i1+1,j1+2) = ss(i1+1,j1+2)
     &                      - shp(2,i)*N_22/r2*cosphi*sinphi*shp(2,j)
              ss(i1+1,j1+3) = ss(i1+1,j1+3) + shp(1,i)*M_11*cs*shp(1,j)
     &                      - shp(2,i)*M_22*cs/r2*cosphi*shp(2,j)
     &                      + shp(1,i)*G1*shp(2,j)
              ss(i1+2,j1+1) = ss(i1+2,j1+1)
     &                      - shp(2,i)*N_22/r2*cosphi*sinphi*shp(2,j)
              ss(i1+2,j1+2) = ss(i1+2,j1+2) + shp(1,i)*N_11*shp(1,j)
     &                      + shp(2,i)*N_22/r2*sinphi*sinphi*shp(2,j)
              ss(i1+2,j1+3) = ss(i1+2,j1+3) - shp(1,i)*M_11*sn*shp(1,j)
     &                      + shp(2,i)*M_22*cs/r2*sinphi*shp(2,j)
     &                      + shp(1,i)*G2*shp(2,j)
              ss(i1+3,j1+1) = ss(i1+3,j1+1) + shp(1,i)*M_11*cs*shp(1,j)
     &                      - shp(2,i)*M_22*cs/r2*cosphi*shp(2,j)
     &                      + shp(2,i)*G1*shp(1,j)
              ss(i1+3,j1+2) = ss(i1+3,j1+2) - shp(1,i)*M_11*sn*shp(1,j)
     &                      + shp(2,i)*M_22*cs/r2*sinphi*shp(2,j)
     &                      + shp(2,i)*G2*shp(1,j)
              ss(i1+3,j1+3) = ss(i1+3,j1+3) + shp(2,i)*G3*shp(2,j)
     &                      - shp(1,i)*M_11*gam*shp(2,j)
     &                      - shp(2,i)*M_11*gam*shp(1,j)
     &                      - shp(2,i)*M_22*(1.d0+ur/ra)*sn/ra*shp(2,j)
              j1 = j1 + 3
            end do ! j
            i1 = i1 + 3
          end do ! i

          do j = 1,6
            do i = 1,6
              ss(i,j) = ss(i,j)*ctan(1)
            end do ! i
          end do ! j

!         Inertial terms

          call shms2d(d,xl,r1,r2,hh,rh1,rh3,shp,ctan(3),rr,ss,ndm,
     &               .false.)

!         Inertia residual

          do j = 1,2
            rr(1,j) = rr(1,j) - (shp(i,1)*r1*aa(1,1)
     &                        +  shp(i,2)*r2*aa(1,2))*rh1
            rr(2,j) = rr(2,j) - (shp(i,1)*r1*aa(2,1)
     &                        +  shp(i,2)*r2*aa(2,2))*rh1
     &                        - (shp(i,1)*r1 + shp(i,2)*r2)*d(8)*hh
            rr(3,j) = rr(3,j) - (shp(i,1)*r1*aa(3,1)
     &                        +  shp(i,2)*r2*aa(3,2))*rh3
          end do

!         Transform to global coordinates

          call trsh2d(cosphi,sinphi,rr,ss, r,s, ndf,nst)

!       Stress outputs

        elseif(isw.eq.4) then

          call shst2d(ra,za,E_11,E_22, gam,K_11,K_22,
     &                      N_11,N_22,S_13,M_11,M_22)

!       Mass or Geometric stiffness

        elseif(isw.eq.5) then

!         Form a mass matrix

          if(imtyp.eq.1) then

            call shms2d(d,xl,r1,r2,hh,rh1,rh3,shp,1.0d0,rr,ss,ndm,
     &                 .true.)

!         Form a Geometric stiffness matrix

          else
            i1 = 0
            do i = 1,2
              j1 = 0
              do j = 1,2
                ss(i1+1,j1+1) =-shp(1,i)*N_11*shp(1,j)
     &                        + shp(2,i)*N_22/r2*cosphi*cosphi*shp(2,j)
                ss(i1+1,j1+2) = ss(i1+1,j1+2)
     &                        - shp(2,i)*N_22/r2*cosphi*sinphi*shp(2,j)
                ss(i1+1,j1+3) =-shp(1,i)*M_11*cs*shp(1,j)
     &                        - shp(2,i)*M_22*cs/r2*cosphi*shp(2,j)
     &                        - shp(1,i)*G1*shp(2,j)

                ss(i1+2,j1+1) = ss(i1+2,j1+1)
     &                        - shp(2,i)*N_22/r2*cosphi*sinphi*shp(2,j)
                ss(i1+2,j1+2) =-shp(1,i)*N_11*shp(1,j)
     &                        + shp(2,i)*N_22/r2*sinphi*sinphi*shp(2,j)
                ss(i1+2,j1+3) = shp(1,i)*M_11*sn*shp(1,j)
     &                        + shp(2,i)*M_22*cs/r2*sinphi*shp(2,j)
     &                        - shp(1,i)*G2*shp(2,j)

                ss(i1+3,j1+1) =-shp(1,i)*M_11*cs*shp(1,j)
     &                        - shp(2,i)*M_22*cs/r2*cosphi*shp(2,j)
     &                        - shp(2,i)*G1*shp(1,j)
                ss(i1+3,j1+2) = shp(1,i)*M_11*sn*shp(1,j)
     &                        + shp(2,i)*M_22*cs/r2*sinphi*shp(2,j)
     &                        - shp(2,i)*G2*shp(1,j)
                ss(i1+3,j1+3) =-shp(2,i)*G3*shp(2,j)
     &                        + shp(1,i)*M_11*gam*shp(2,j)
     &                        + shp(2,i)*M_11*gam*shp(1,j)
     &                        - shp(2,i)*M_22*(1.d0+ur/ra)*sn/ra
     &                        * shp(2,j)
                j1 = j1 + 3
              end do ! j
              i1 = i1 + 3
            end do ! i
          endif

!         Transform to global coordinates

          call trsh2d(cosphi,sinphi,rr,ss, r,s, ndf,nst)

        endif

      endif

      end subroutine shl2df

      subroutine shst2d(ra,za,E_11,E_22,E_13,K_11,K_22,
     &                        N_11,N_22,S_13,M_11,M_22)

      implicit   none

      include   'bdata.h'
      include   'eldata.h'
      include   'iofile.h'

      real (kind=8) ::  ra,za
      real (kind=8) ::  E_11,E_22,E_13,K_11,K_22
      real (kind=8) ::  N_11,N_22,S_13,M_11,M_22

!     Output Stress and Strains for Whell

      mct = mct -2
      if(mct.le.0) then
        write(iow,2001) o,head
        if(ior.lt.0) then
          write (*,2001) o,head
        endif
        mct = 50
      endif
      write(iow,2002) n_el,ma,ra,za,N_11,N_22,S_13,M_11,M_22,
     &                           E_11,E_22,E_13,K_11,K_22
      if(ior.lt.0) then
        write(*,2002) n_el,ma,ra,za,N_11,N_22,S_13,M_11,M_22,
     &                           E_11,E_22,E_13,K_11,K_22
      endif

!     Formats for output

2001  format(a1,20a4//5x,'Element Resultants'//
     & '  Element Material     R-Coord     Z-Coord'/
     & 4x,'11-Force    22-Force    13-Shear   11-Moment   22-Moment'/
     & 4x,'11-Strain   22-Strain   13-Strain  11-Curvtr   22-Curvtr'/1x)

2002  format(2i9,1p,2e12.3/(12x,1p,5e12.3))

      end subroutine shst2d

      subroutine shms2d(d,xl,r1,r2,hh,rh1,rh3,shp,ctan3,rr,ss,ndm,lump)

      implicit   none

      logical       :: lump
      integer       :: ndm, i,j, i1,j1
      real (kind=8) :: r1,r2, hh, rh1,rh3, ctan3, sq3, G1
      real (kind=8) :: d(*), xl(ndm,2),shp(2,2), rr(3,2), ss(6,6)

!     Intertia tangent matrix (Mass)

      sq3      = 0.5d0/sqrt(3.d0)

      shp(1,1) = 0.5d0 + sq3
      shp(2,1) = 0.5d0 - sq3
      shp(1,2) = shp(2,1)
      shp(2,2) = shp(1,1)
      r1       = shp(1,1)*xl(1,1) + shp(2,1)*xl(1,2)
      r2       = shp(1,2)*xl(1,1) + shp(2,2)*xl(1,2)
      rh1      = hh*d(4)*d(14)
      rh3      = rh1*d(14)**2/12.d0

      j1 = 1
      do j = 1,2
        i1 = 1
        do i = 1,2
          G1            = (shp(i,1)*shp(j,1)*r1
     &                  +  shp(i,2)*shp(j,2)*r2)*rh1*ctan3
          ss(i1  ,j1  ) =  ss(i1  ,j1  ) + G1
          ss(i1+1,j1+1) =  ss(i1+1,j1+1) + G1
          ss(i1+2,j1+2) =  ss(i1+2,j1+2) + (shp(i,1)*shp(j,1)*r1
     &                  +  shp(i,2)*shp(j,2)*r2)*rh3*ctan3
          i1 = 4
        end do ! i
        j1 = 4
      end do ! j

!     Lumped mass matrix (Row sum)

      if(lump) then
        do j = 1,3
          rr(j,1) = ss(j  ,j  ) + ss(j  ,j+3)
          rr(j,2) = ss(j+3,j  ) + ss(j+3,j+3)
        end do ! j
      endif

      end subroutine shms2d

      subroutine shl2dn(d,ul,xl,s,r,ndf,ndm,nst,isw)

      implicit   none

      include   'cdata.h'
      include   'eltran.h'
      include   'evdata.h'

      integer       :: ndf,ndm,nst,isw
      integer       :: a,b, i,j, i1,j1

      real (kind=8) :: B11,B12,Gh, D11,D12, r1, rh1,rh3, dvol
      real (kind=8) :: ur,du,dw,dk, ra,r2, za, th, hh, cosphi,sinphi
      real (kind=8) :: gam,E_11,E_22,K_11,K_22, N_11,N_22,S_13,M_11,M_22

      real (kind=8) :: d(*),ul(ndf,nen,*),xl(ndm,*)
      real (kind=8) :: s(nst,*),r(ndf,*), ss(6,6),rr(3,2)

      real (kind=8) :: D_t(5,5), bb(5,3,2), bd(3,5), uu(3,2), shp(2,2)
      real (kind=8) :: aa(3,2)

      if(isw.eq.1) then
      else

!       Plate properties

        B11 = d(1)*d(14)/(1.d0 - d(2)*d(2))   ! Et/(1.d0 - nu*nu)
        B12 = d(2)*B11
        Gh  = d(1)*d(14)/(1.d0+d(2))*0.5*d(10) ! kappa * Gt
        D11 = d(14)**2/12.d0*B11              ! D
        D12 = d(2)*D11

!       Shape functions for one point quadrature

        hh       =   sqrt((xl(1,2) - xl(1,1))**2
     +                  + (xl(2,2) - xl(2,1))**2)

        shp(1,1) = - 1.0d0/hh
        shp(2,1) =   0.5d0

        shp(1,2) =   1.0d0/hh
        shp(2,2) =   0.5d0

!       Compute local displacements at nodes

        cosphi   =  (xl(1,2) - xl(1,1))/hh
        sinphi   =  (xl(2,2) - xl(2,1))/hh

        do i = 1,2
          uu(1,i) =   cosphi*ul(1,i,1) + sinphi*ul(2,i,1)
          uu(2,i) = - sinphi*ul(1,i,1) + cosphi*ul(2,i,1)
          uu(3,i) =   ul(3,i,1)
          aa(1,i) =   cosphi*ul(1,i,5) + sinphi*ul(2,i,5)
          aa(2,i) = - sinphi*ul(1,i,5) + cosphi*ul(2,i,5)
          aa(3,i) =   ul(3,i,5)
        end do

!       Compute local derivatives

        ra = (xl(1,1) + xl(1,2))*0.5d0
        za = (xl(2,1) + xl(2,2))*0.5d0
        r2 = ra*ra
        ur = (ul(1,1,1) + ul(1,2,1))*0.5d0
        du = (uu(1,2) - uu(1,1))/hh
        dw = (uu(2,2) - uu(2,1))/hh
        dk = (uu(3,2) - uu(3,1))/hh
        th = (uu(3,1) + uu(3,2))*0.5d0

!       Compute strains

        E_11 = du + 0.5*(du*du + dw*dw)
        E_22 = ur/ra + 0.5d0*(ur/ra)**2
        K_11 = dk
        K_22 = th/ra
        gam  = dw + th

!       Set the volume of the element

        dvol = hh*ra

!       Compute the force resultants

        N_11 =  (B11*E_11 + B12*E_22)*dvol
        N_22 =  (B12*E_11 + B11*E_22)*dvol
        S_13 =   Gh*gam *dvol
        M_11 =  (D11*K_11 + D12*K_22)*dvol
        M_22 =  (D12*K_11 + D11*K_22)*dvol

        if(isw.eq.3 .or. isw.eq.6) then

!         Elastic 'tangent moduli'

          D_t(:,:) = 0.0d0
          D_t(1,1) = B11*dvol
          D_t(1,2) = B12*dvol
          D_t(2,1) = B12*dvol
          D_t(2,2) = B11*dvol
          D_t(3,3) = Gh*dvol
          D_t(4,4) = D11*dvol
          D_t(4,5) = D12*dvol
          D_t(5,4) = D12*dvol
          D_t(5,5) = D11*dvol

!         Strain-displacement matrix

          do i = 1,2
            bb(1,1,i) = (1.0d0 + du)*shp(1,i)
            bb(1,2,i) =          dw *shp(1,i)
            bb(1,3,i) =  0.0d0

            bb(2,1,i) = (1.d0 + ur/ra)*cosphi/ra*shp(2,i)
            bb(2,2,i) =-(1.d0 + ur/ra)*sinphi/ra*shp(2,i)
            bb(2,3,i) =  0.0d0

            bb(3,1,i) =  0.0d0
            bb(3,2,i) =  shp(1,i)
            bb(3,3,i) =  shp(2,i)

            bb(4,1,i) =  0.0d0
            bb(4,2,i) =  0.0d0
            bb(4,3,i) =  shp(1,i)

            bb(5,1,i) =  0.0d0
            bb(5,2,i) =  0.0d0
            bb(5,3,i) =  shp(2,i)/ra
          end do

!         Compute residual and tangent
          rr(:,:) = 0.0d0
          ss(:,:) = 0.0d0

          i1 = 0
          do i = 1,2

!           Compute the residual term
            do a = 1,3
              rr(a,i) = rr(a,i) - bb(1,a,i)*N_11
     &                          - bb(2,a,i)*N_22
     &                          - bb(3,a,i)*S_13
     &                          - bb(4,a,i)*M_11
     &                          - bb(5,a,i)*M_22
              do b = 1,5
                bd(a,b) = bb(1,a,i)*D_t(1,b)
     &                  + bb(2,a,i)*D_t(2,b)
     &                  + bb(3,a,i)*D_t(3,b)
     &                  + bb(4,a,i)*D_t(4,b)
     &                  + bb(5,a,i)*D_t(5,b)
              end do ! b
            end do ! a

            j1 = 0
            do j = 1,2
              do a = 1,3
                do b = 1,3
                  ss(i1+a,j1+b) = bd(a,1)*bb(1,b,j)
     &                          + bd(a,2)*bb(2,b,j)
     &                          + bd(a,3)*bb(3,b,j)
     &                          + bd(a,4)*bb(4,b,j)
     &                          + bd(a,5)*bb(5,b,j)
                end do ! b
              end do ! a
              ss(i1+1,j1+1) = ss(i1+1,j1+1) + shp(1,i)*N_11*shp(1,j)
     &                      + shp(2,i)*N_22/r2*cosphi*cosphi*shp(2,j)
              ss(i1+1,j1+2) = ss(i1+1,j1+2)
     &                      - shp(2,i)*N_22/r2*cosphi*sinphi*shp(2,j)
              ss(i1+2,j1+1) = ss(i1+2,j1+1)
     &                      - shp(2,i)*N_22/r2*cosphi*sinphi*shp(2,j)
              ss(i1+2,j1+2) = ss(i1+2,j1+2) + shp(1,i)*N_11*shp(1,j)
     &                      + shp(2,i)*N_22/r2*sinphi*sinphi*shp(2,j)
              j1 = j1 + 3
            end do ! j
            i1 = i1 + 3
          end do ! i

          ss(:,:) = ss(:,:)*ctan(1)

!         Inertial terms

          call shms2d(d,xl,r1,r2,hh,rh1,rh3,shp,ctan(3),rr,ss,ndm,
     &               .false.)

!         Inertia residual and pressure load (on 2-residual only)

          do j = 1,2
            rr(1,j) = rr(1,j) - (shp(i,1)*r1*aa(1,1)
     &                        +  shp(i,2)*r2*aa(1,2))*rh1
            rr(2,j) = rr(2,j) - (shp(i,1)*r1*aa(2,1)
     &                        +  shp(i,2)*r2*aa(2,2))*rh1
     &                        - (shp(i,1)*r1 + shp(i,2)*r2)*d(8)*hh
            rr(3,j) = rr(3,j) - (shp(i,1)*r1*aa(3,1)
     &                        +  shp(i,2)*r2*aa(3,2))*rh3
          end do

!         Transform to global coordinates

          call trsh2d(cosphi,sinphi,rr,ss, r,s, ndf,nst)

!       Stress outputs

        elseif(isw.eq.4) then

          call shst2d(ra,za,E_11,E_22, gam,K_11,K_22,
     &                      N_11,N_22,S_13,M_11,M_22)

!       Mass or Geometric stiffness

        elseif(isw.eq.5) then

!         Form a mass matrix

          if(imtyp.eq.1) then

            call shms2d(d,xl,r1,r2,hh,rh1,rh3,shp,1.0d0,rr,ss,ndm,
     &                 .true.)

!         Form a Geometric stiffness matrix

          else

            ss(:,:) = 0.0d0
            i1 = 0
            do i = 1,2
              j1 = 0
              do j = 1,2
                ss(i1+1,j1+1) = -shp(1,i)*N_11*shp(1,j)
     &                        +  shp(2,i)*N_22/r2*cosphi*cosphi*shp(2,j)
                ss(i1+1,j1+2) =  ss(i1+1,j1+2)
     &                        -  shp(2,i)*N_22/r2*cosphi*sinphi*shp(2,j)
                ss(i1+2,j1+1) =  ss(i1+2,j1+1)
     &                        -  shp(2,i)*N_22/r2*cosphi*sinphi*shp(2,j)
                ss(i1+2,j1+2) = -shp(1,i)*N_11*shp(1,j)
     &                        +  shp(2,i)*N_22/r2*sinphi*sinphi*shp(2,j)
                j1 = j1 + 3
              end do ! j
              i1 = i1 + 3
            end do ! i

          endif

!         Transform to global coordinates

          call trsh2d(cosphi,sinphi,rr,ss, r,s, ndf,nst)

        endif

      endif

      end subroutine shl2dn
