!$Id:$
      subroutine vem_pmat2d(xl, nel)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Purpose: 2-D VEM: Virtual Element Method shape function basis

!     Remarks:  The shape function of node 'a' is computed from:
!                 N(a,ll)   = mm(i,ll) * P0mat(i,a)      ; i = 1,nk
!               Cartesian derivatives are computed from:
!                 N,1(a,ll) = mm(i,ll) * Pdmat(i,1,a)    ; i = 1,nkm1
!                 N,2(a,ll) = mm(i,ll) * Pdmat(i,2,a)    ; i = 1,nkm1

!     Reference:
!            1.  "The Hitchhiker's Guide to the Virtual Element Method",
!                L. Beirao de Veiga, F. Brezzi, L.D. Marini & A. Russo,
!                Mathematical Models and Methods in Applied Sciences,
!                Vol 24, No. 8, (2014) 1541--1573.

!            2.  "Arbitrary order 2D Virtual Elements for Polygonal
!                Meshes: Part I, Elastic Problem", E. Artioli,
!                L. Beirao de Veiga, C. Lovadina & E. Sacco,
!                Computational Mechanics, (to appear)

!     Inputs:
!       k_order       - Order of VEM
!       xl(ndm,nen)   - Element nodal coordinates
!       xc(ndm)       - Centroidal coordinate
!       vol           - Volume of VEM
!       hVm1          - Scaling dimension

!     Outputs:
!       P0mat(10,*)  - Shape function projection matrix
!       Pdmat(6,2,*) - Shape derivative projection matrix
!       Dmat(15,*)   - Shape derivative projection matrix
!       xp(2,*)      - Coordinates of GP
!       dvol(*)      - Differential volume at GP
!       mm(10,*)     - Pascal polynomials at GP
!       ltot         - Total number of GP for VEM
!-----[--.----+----.----+----.-----------------------------------------]
      implicit none

      include 'debugs.h'
      include 'iofile.h'
      include 'qudshp.h'                   ! sa(125)
      include 'sdata.h'                    ! ndm
      include 'vem_data.h'                 ! VEM data arrays

      integer          ::  nel             ! No. nodes
      real    (kind=8) ::  xl(ndm,*)       ! Local nodal coordinates

!     Local variables

      integer          ::  i, j, l         ! Loop counter
      integer          ::  ii, jj          ! Loop counter
      integer          ::  ll              ! GP counter
      integer          ::  ne              ! Number edges
      integer          ::  nm              ! Number Pascal functions
      integer          ::  c               ! Mid-edge node
      real    (kind=8) ::  vold            ! Differential volume  @ GP.
      real    (kind=8) ::  volr            ! Reciprocal volume
      real    (kind=8) ::  md(2,10,64)     ! Pascal derivatives at GP
      real    (kind=8) ::  xx(2)           ! Cartesian coordinate @ GP.
      real    (kind=8) ::  nx(2)           ! Normal vector at edge
      real    (kind=8) ::  tx(2)           ! Normal vector at edge
      real    (kind=8) ::  dx1(2),dx2(2)   ! Diff. vectors # GP.
      real    (kind=8) ::  dxn(2)          ! Relative x at vertex nd. i
      real    (kind=8) ::  dxm(2)          ! Relative x at vertex nd. j
      real    (kind=8) ::  dx4(2)          ! Relative x at mid-edge nd
      real    (kind=8) ::  p(0:2,10)       ! 0 = function
                                           ! 1 = 1-deriv.
                                           ! 2 = 2-deriv.
      real    (kind=8) ::  fac(2)          ! Temporary factor
      real    (kind=8) ::  Gmtr(6,10)      ! Element integrals
      real    (kind=8) ::  Ginv(6,6)       ! Element integrals inverse
      real    (kind=8) ::  Hmtr(10,10)     ! Element integrals
      real    (kind=8) ::  Bmtr(10,65)     ! Element integrals
      real    (kind=8) ::  Psmtr(6,2,65)   ! Element integrals

      save

!     Set number of functions
!                                  k_order  !  1  !  2  !  3
!     -------------------------------------------------------
      nk   = (k_order + 1)*(k_order + 2)/2  !  3  !  6  ! 10
      nkm1 = (k_order    )*(k_order + 1)/2  !  1  !  3  !  6
      nkm2 = (k_order - 1)*(k_order    )/2  !  0  !  1  !  3
!     -------------------------------------------------------

!     Reciprocal volume

      volr = 1.0d0/vol

!     Perform base computation for various order edges

      select case (k_order)

!       Linear edges

        case (1)

!         Build G- and B-matrix

          Pdmat(:,:,1:nel) = 0.0d0
          Plmat(:,:,1:nel) = 0.0d0
          do i = 1,nel  ! Number of edges
            j      = mod(i,nel) + 1

!           Pdmat

            tx(:) = xl(:,j) - xl(:,i)              ! h * tangent

            nx(1) = 0.5d0*(xl(2,j) - xl(2,i))*volr ! h^{-1} included
            nx(2) = 0.5d0*(xl(1,i) - xl(1,j))*volr

            Pdmat(1,1,i) = Pdmat(1,1,i) + nx(1)
            Pdmat(1,2,i) = Pdmat(1,2,i) + nx(2)

            Pdmat(1,1,j) = Pdmat(1,1,j) + nx(1)
            Pdmat(1,2,j) = Pdmat(1,2,j) + nx(2)

!           D matrix for k_order = 1 (size = nk)

            xx(:)     =  (xl(:,i) - xc(:))*hVm1
            Dmat(i,1) = 1.0d0
            Dmat(i,2) = xx(1)
            Dmat(i,3) = xx(2)

            Plmat(1,1,j) = Plmat(1,1,j) + nx(1)*tx(1)/6.d0 ! has V^-1
            Plmat(2,1,j) = Plmat(2,1,j) + nx(2)*tx(1)/6.d0 !
            Plmat(1,2,j) = Plmat(1,2,j) + nx(1)*tx(2)/6.d0
            Plmat(2,2,j) = Plmat(2,2,j) + nx(2)*tx(2)/6.d0

            Plmat(1,1,i) = Plmat(1,1,i) - nx(1)*tx(1)/6.d0
            Plmat(2,1,i) = Plmat(2,1,i) - nx(2)*tx(1)/6.d0
            Plmat(1,2,i) = Plmat(1,2,i) - nx(1)*tx(2)/6.d0
            Plmat(2,2,i) = Plmat(2,2,i) - nx(2)*tx(2)/6.d0

          end do ! i

!         Form terms for P0mat projector

          Hmtr(:,:)        = 0.0d0
          Bmtr(:,:)        = 0.0d0

          ll            = 0
          Bmtr(1,1:nel) = 1.0d0/dble(nel)
          do i = 1,nel  ! Number of edges
            j      = mod(i,nel) + 1

!           Volume element

            dx1(:) = xl(:,i) - xc(:)
            dx2(:) = xl(:,j) - xc(:)
            vold   = (dx1(1)*dx2(2) - dx1(2)*dx2(1))*0.5d0

!           First row of H_mat:

            call vem_pascal(k_order,xl(1,i),xc, hVm1, p, nm)

            Hmtr(1,1:3) = Hmtr(1,1:3) + p(0,1:3)/dble(nel)

!           Quadrature over the element

            do l = 1,lint2

!             Increment counter

              ll = ll + 1

              dvol(ll) = vold*el2(4,l)

!             Solution point

              xx(:) = xl(:,i)*el2(1,l)
     &              + xl(:,j)*el2(2,l)
     &              + xc(:)  *el2(3,l)

              xp(:,ll) = xx

              call vem_pascal(k_order,xx,xc, hVm1, p, nm)

              mm(1:nm,ll)   = p(0,1:nm)
              md(:,1:nm,ll) = p(1:2,1:nm)

!             H matrix - Rows 2-nk

              do jj = 2,3
                fac(:) = p(1:2,jj)*dvol(ll)
                do ii = 2,3
                  Hmtr(ii,jj) = Hmtr(ii,jj)
     &                        + p(1,ii)*fac(1) + p(2,ii)*fac(2)
                end do ! ii
              end do ! jj

!             Bmtrrix

              do ii = 1, nel
!               N.B. nkm1 = 1
                fac(:) = Pdmat(1,:,ii)*dvol(ll) ! Monomial = 1.0
                do jj = 2,3
                  Bmtr(jj,ii) = Bmtr(jj,ii) + p(1,jj)*fac(1)
     &                                      + p(2,jj)*fac(2)
                end do ! ii
              end do ! jj

            end do ! loop l

          end do ! i

          ltot = ll

!         Invert H

          call invert(Hmtr, 3, 10)

!         Compute P0mat = G^{-1} * B

          do ii = 1,nel
            P0mat(:,ii) = Hmtr(:,1)*Bmtr(1,ii)
     &                  + Hmtr(:,2)*Bmtr(2,ii)
     &                  + Hmtr(:,3)*Bmtr(3,ii)
          end do ! ii

        case (2)

!         Set interior coordinate to centroid

          xl(:,nel) = xc(:)

!         Loop over edges: This part common to all first deriv forms.

          Gmtr(:,:)    = 0.0d0
          Gmtr(1,1)    = vol
          Psmtr(:,:,:) = 0.0d0

          ll           = 0
          ne           = nel/2
          do i = 1,ne
            j      = mod(i,ne) + 1
            c      = ne + i        ! Mid-edge node number

            dxn(:) = xl(:,i) - xc(:)
            dxm(:) = xl(:,j) - xc(:)
            dx4(:) = xl(:,c)*2.0d0 - xl(:,i) - xl(:,j)

!           Area integral

            do l = 1,lintv(i)

              ll = ll + 1

!             Differential volume element

              dx1(:)   = dxn(:) + 2.d0*elv(2,ll)*dx4(:)
              dx2(:)   = dxm(:) + 2.d0*elv(1,ll)*dx4(:)
              dvol(ll) = (dx1(1)*dx2(2) - dx1(2)*dx2(1))*elv(4,ll)*0.5d0

!             Solution point

              xx(:)  = xl(:,i)*elv(1,ll)
     &               + xl(:,j)*elv(2,ll)
     &               + xc(:)  *elv(3,ll)
     &               + 2.0d0*elv(1,ll)*elv(2,ll)*dx4(:)

              xp(:,ll) = xx(:)

              call vem_pascal(k_order,xx,xc, hVm1, p, nm)

              mm(  1:nm,ll) = p(0,1:nm)
              md(:,1:nm,ll) = p(1:2,1:nm)

!             G-matrix

              Gmtr(2,2) = Gmtr(2,2) + p(0,2)*p(0,2)*dvol(ll)
              Gmtr(2,3) = Gmtr(2,3) + p(0,2)*p(0,3)*dvol(ll)
              Gmtr(3,3) = Gmtr(3,3) + p(0,3)*p(0,3)*dvol(ll)

            end do ! l

!           Edge integrals

            do l = 1,lint1

              call shp1dn(sg1(1,l),shp1,3)

              xx(:) = (xl(:,i)*shp1(2,1,1) + xl(:,j)*shp1(2,2,1)
     &              +  xl(:,c)*shp1(2,3,1) - xc(:))*hVm1
              dx1(:) = (xl(:,i)*shp1(1,1,1) + xl(:,j)*shp1(1,2,1)
     &               +  xl(:,c)*shp1(1,3,1))*sg1(2,l)
              nx(1)  =  dx1(2)   ! nx * ds
              nx(2)  = -dx1(1)   ! ny * ds

              Psmtr(1,:,i) = Psmtr(1,:,i) + nx(:)*shp1(2,1,1)
              Psmtr(2,:,i) = Psmtr(2,:,i) + nx(:)*shp1(2,1,1)*xx(1)
              Psmtr(3,:,i) = Psmtr(3,:,i) + nx(:)*shp1(2,1,1)*xx(2)

              Psmtr(1,:,j) = Psmtr(1,:,j) + nx(:)*shp1(2,2,1)
              Psmtr(2,:,j) = Psmtr(2,:,j) + nx(:)*shp1(2,2,1)*xx(1)
              Psmtr(3,:,j) = Psmtr(3,:,j) + nx(:)*shp1(2,2,1)*xx(2)

              Psmtr(1,:,c) = Psmtr(1,:,c) + nx(:)*shp1(2,3,1)
              Psmtr(2,:,c) = Psmtr(2,:,c) + nx(:)*shp1(2,3,1)*xx(1)
              Psmtr(3,:,c) = Psmtr(3,:,c) + nx(:)*shp1(2,3,1)*xx(2)

            end do ! l loop

          end do ! i loop

!         Internal value

          Psmtr(2,1,nel) = Psmtr(2,1,nel) - vol*hVm1
          Psmtr(3,2,nel) = Psmtr(3,2,nel) - vol*hVm1

          Gmtr(3,2) = Gmtr(2,3)
          Ginv(:,:) = Gmtr(:,1:6)
          call invert(Ginv,3,6)

          Pdmat(:,:,1:nel) = 0.0d0
          do i = 1,nel
            do j = 1,3
              Pdmat(:,1,i) = Pdmat(:,1,i) + Ginv(:,j)*Psmtr(j,1,i)
              Pdmat(:,2,i) = Pdmat(:,2,i) + Ginv(:,j)*Psmtr(j,2,i)
            end do ! j
          end do ! i loop

!         D matrix for stabilizing

          Dmat(:,1:nk) = 0.0d0
          do i = 1,nel-1  ! Number of external nodes

            xx(:)     = (xl(:,i) - xc(:))*hVm1
            Dmat(i,1) = 1.0d0
            Dmat(i,2) = xx(1)
            Dmat(i,3) = xx(2)
            Dmat(i,4) = xx(1)**2
            Dmat(i,5) = xx(1)*xx(2)
            Dmat(i,6) = xx(2)**2

          end do ! i

!         Moment stabilizing part

          Dmat(nel,1) = 1.0d0
          Dmat(nel,4) = Gmtr(2,2)*volr ! Same as int_v p(0,1)*p(0,4)*dV
          Dmat(nel,5) = Gmtr(2,3)*volr ! Same as int_v p(0,1)*p(0,5)*dV
          Dmat(nel,6) = Gmtr(3,3)*volr ! Same as int_v p(0,1)*p(0,6)*dV

!         Loop over edges: For P0mat projector

          Hmtr(:,:)    = 0.0d0
          Bmtr(:,:)    = 0.0d0
          Bmtr(1,nel)  = 1.0d0
          ll           = 0
          do i = 1,ne

!           Area integral

            do l = 1,lintv(i)

              ll = ll + 1

!             H-matrix - Row  1

              fac(1) = dvol(ll)*volr
              do ii = 1,nk
                Hmtr(1,ii) = Hmtr(1,ii) + mm(ii,ll)*fac(1)
              end do ! ii

!             H-matrix - Rows 2-nk

              do jj = 2,nk
                fac(:) = md(1:2,jj,ll)*dvol(ll)
                do ii = 2,nk
                  Hmtr(ii,jj) = Hmtr(ii,jj)
     &                        + md(1,ii,ll)*fac(1)
     &                        + md(2,ii,ll)*fac(2)
                end do ! ii
              end do ! jj

!             Bmtrrix

              do ii = 1, nel
                fac(:) = Pdmat(1,:,ii)
                do jj = 2,nkm1
                  fac(:) = fac(:) + mm(jj,ll)*Pdmat(jj,:,ii)
                end do ! jj
                fac(:) = fac(:)*dvol(ll)
                do jj = 2,nk
                  Bmtr(jj,ii) = Bmtr(jj,ii) + md(1,jj,ll)*fac(1)
     &                                      + md(2,jj,ll)*fac(2)
                end do ! ii
              end do ! jj

            end do ! l

          end do ! i loop

!         Save total number of GP

          ltot = ll

!         Invert Hmtr

          call invert(Hmtr,6,10)

!         Compute P0mat = H^{-1} * B

          do ii = 1,nel
            P0mat(:,ii) = 0.0d0
            do jj = 1,nk
              P0mat(:,ii) = P0mat(:,ii) + Hmtr(:,jj)*Bmtr(jj,ii)
            end do ! jj
          end do ! ii

        case default

          write(*,'(a,i3)') ' --> ERROR Note coded for',k_order

      end select

      if(debug) write(*,*) ' LTOT =',ltot,' NEL =',nel

      end subroutine vem_pmat2d
