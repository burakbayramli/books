!$Id:$
      subroutine plate2d(d,ul,xl,ix,s,p,ndf,ndm,nst,isw)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!**********************************************************************
!     Triangular plate: 3 dofs per node (w, theta-x, theta-y)
!                       2 bubble modes for rotations
!                       2 shear parameters

!     Mixed approach for shear stiffness.
!        Step 1: Condensation of shear terms
!        Step 2: Condensation of bubble terms

!     Three integration points are used.

!     Arguments:
!        d(*)      - specified parameter array
!        ul(ndf,*) - local nodal solution values
!        xl(ndm,*) - local nodal coordinate values
!        ix(*)     - node numbers
!        s(nst,nst) - finite element array (stiffness, mass, geometric
!                                           stiffness)
!        p(nst)     - finite element array (residual, lumped mass)
!        ndf        - number of degree of freedoms at node ( > or = 3 )
!        ndm        - spatial dimension of element         ( > or = 2 )
!        nst        - size of finite element arrays        ( > or = 9 )
!        isw        - solution option
!                   = 1: Input values and store in d(*) array
!                   = 2: Check mesh coordinate and connection inputs
!                        for errors
!                   = 3: Compute element residual (p) and stiffness (s)
!                   = 4: Output element results
!                   = 5: Compute mass (p,s) or geometric stiffness array (s)
!                   = 6: Compute element residual (p)
!                   = 8: Compute nodal projections

!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!     Input parameters set as follows:

!         ndm = 2 (x,y cartesian coordinates at nodes)
!         ndf = 3 (w,theta-x,theta-y, at nodes)
!         nen = 3 nodes (counterclockwise around element)
!                  or
!         nen = 4 nodes (counterclockwise around element)
!**********************************************************************
      implicit  none

      include  'cdata.h'
      include  'eldata.h'
      include  'hdata.h'
      include  'iofile.h'
      include  'mdata.h'
      include  'strnum.h'

      include  'comblk.h'

      integer       :: ndf,ndm,nst,isw, i, tdof

      integer       :: ix(*)
      real (kind=8) :: d(*),ul(ndf,nen,*),xl(ndm,*),s(nst,*),p(ndf,*)

      save

!     Go to correct array processor

      if(isw.eq.0 .and. ior.lt.0) then
        write(*,*) '   Plate2d: 2-d Plate Linear Elastic (3 or 4 node)'

!     Input material properties

      elseif(isw.eq.1) then
        write(iow,2000)
        if(ior.lt.0) write(*,2000)
        call inmate(d,tdof,ndf*4,4)

!       Set plot sequence

        pstyp = 2

!       Set rotation parameters: theta-x = 2; theta-y = 3

        ea(1,-iel) = 2
        ea(2,-iel) = 3

        istv = 20

!       Deactivate dof in element for dof > 3

        do i = 4,ndf
          ix(i) = 0
        end do

      else

        if(nel.eq.3) then
          call plate2t(d,ul,xl,ix,s,p,ndf,ndm,nst,isw)
        elseif(nel.eq.4) then
          call plate2q(d,ul,xl,ix,s,p,ndf,ndm,nst,isw)
        else
          write(iow,3000) n_el
          if(ior.lt.0) write(*,3000) n_el
          call plstop(.true.)
        endif
      endif

!     Formats for input-output

2000  format(/5x,'E l a s t i c   P l a t e   E l e m e n t'/)

3000  format('*ERROR* Plate Element',i8,' has more than 4-nodes')
      end subroutine plate2d

      subroutine plate2q(d,ul,xl,ix,s,p,ndf,ndm,nst,isw)

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:
!             Quadrilateral plate: 3 dofs per node (w, theta_x, theta_y)
!                                  4 internal bubble for rotational fields

!             Mixed approach for shear stiffness.
!             Step 1: Condensation of bubble terms
!             Step 2: Condensation of shear terms
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'bdata.h'
      include  'cdata.h'
      include  'eldata.h'
      include  'eltran.h'
      include  'iofile.h'
      include  'mdata.h'
      include  'prstrs.h'
      include  'comblk.h'

      integer       :: ndm, ndf, nst, isw
      integer       :: i, ii, i1, j, jj, j1, k, l, lint
      integer       :: ix(*)

      real (kind=8) :: thk, thk3, q0, xx, yy , xsj, ctan1
      real (kind=8) :: jac(2,2)   , jac0(2,2)   , jinv(2,2) , td(12)

      real (kind=8) :: k_tt(12,12), k_wt(12,12)
      real (kind=8) :: k_st(4,12) , k_bt(4,12)  , k_bb(4,4)
      real (kind=8) :: k_bs(4)    , k_ss(4,4)   , k_sw(4,12)

      real (kind=8) :: bt_b(3,12) , bt_b1(12,3) , bb_b(3,4) , bb_b1(4,3)
      real (kind=8) :: bw_s(2,12) , bwt_s(2,12) , ns(2,4)   , ns1(4,2)

      real (kind=8) :: app1(4,12) , app2(4,12)  , bpp1(4)   , bpp2(4)
      real (kind=8) :: s_hat(4)   , b_hat(4)    , shear(2)

      real (kind=8) :: d(*),ul(ndf,nen,*),xl(ndm,*), s(nst,*),p(ndf,*)
      real (kind=8) :: shp(3,4), shpn(3), shpm(3,4), sg(3,25)
      real (kind=8) :: eps(3), sig(6), dd(3,3), ds(2,2), alphg(3)
      real (kind=8) :: co(4),si(4)

      save

!     Go to correct array processor

      go to(1,2,3,3,5,3,1,3), isw

!     Return

1     return

!     Check element for errors

2     call ckisop(ix,xl,bt_b,ndm)

      return

!     Compute stiffness

!     Zero all matrices

3     do i = 1,12
        do j = 1,4
          k_bt(j,i) = 0.0d0
          k_st(j,i) = 0.0d0
          k_sw(j,i) = 0.0d0
        end do ! j
        do j = 1,12
          k_tt(j,i) = 0.0d0
        end do ! j
      end do ! i
      do i = 1,4
        do j = 1,4
          k_bb(j,i) = 0.0d0
          k_ss(j,i) = 0.0d0
        end do ! j
        k_bs(i) = 0.0d0
      end do ! i

!     Compute location Gauss points, weights and geometry

      l   = 3
      call int2d (l,lint,sg)
      call geompq(xl,ndm,co,si,jac0)

!     Compute integrals ( LOOP ON GAUSS POINTS )

      do l = 1, lint

        call shpspq(sg(1,l),xl,shp,shpn,shpm,xsj,jac,jinv,ndm)
        call bmatpq(sg(1,l),xl,shp,shpn,shpm,si,co,jac0,jinv,xsj,
     &              bw_s,bt_b,bb_b,bwt_s,ns,ndm)
        xsj = xsj*sg(3,l)

!       Define material constants x jacobian x weight

        call dmatpl(d,d(31),dd,ds,alphg)

!       Bending stiffness

        thk  = d(14)
        thk3 = thk**3/12.d0*xsj
        do j = 1,3
          do i = 1,3
            dd(i,j) = dd(i,j)*thk3
          end do
        end do

!       Shear compliance

        thk3    =  xsj/((ds(1,1)*ds(2,2) - ds(1,2)*ds(2,1))*thk*d(10))
        bpp1(1) =  ds(2,2)*thk3
        ds(2,2) =  ds(1,1)*thk3
        ds(1,2) = -ds(1,2)*thk3
        ds(2,1) = -ds(2,1)*thk3
        ds(1,1) =  bpp1(1)

        do i = 1,12

!         Construct  "bt_b1 = bt_b * Db"

          bt_b1(i,1) = bt_b(1,i)*dd(1,1)
     &               + bt_b(2,i)*dd(2,1)
     &               + bt_b(3,i)*dd(3,1)
          bt_b1(i,2) = bt_b(1,i)*dd(1,2)
     &               + bt_b(2,i)*dd(2,2)
     &               + bt_b(3,i)*dd(3,2)
          bt_b1(i,3) = bt_b(1,i)*dd(1,3)
     &               + bt_b(2,i)*dd(2,3)
     &               + bt_b(3,i)*dd(3,3)

        end do

        do i = 1,4

!         Construct  "bb_b1 = bb_b * Db"

          bb_b1(i,1) = bb_b(1,i)*dd(1,1)
     &               + bb_b(2,i)*dd(2,1)
     &               + bb_b(3,i)*dd(3,1)
          bb_b1(i,2) = bb_b(1,i)*dd(1,2)
     &               + bb_b(2,i)*dd(2,2)
     &               + bb_b(3,i)*dd(3,2)
          bb_b1(i,3) = bb_b(1,i)*dd(1,3)
     &               + bb_b(2,i)*dd(2,3)
     &               + bb_b(3,i)*dd(3,3)

!         Construct   (N_s)^T * Ds^(-1)

          ns1(i,1)   = ns(1,i) * ds(1,1)
     &               + ns(2,i) * ds(2,1)
          ns1(i,2)   = ns(1,i) * ds(1,2)
     &               + ns(2,i) * ds(2,2)

        end do

!       Construct stiffness matrices with dimension equal to NST

        do i = 1,12
          do j = 1,i
            k_tt(j,i) = k_tt(j,i) + bt_b1(i,1)*bt_b(1,j)
     &                            + bt_b1(i,2)*bt_b(2,j)
     &                            + bt_b1(i,3)*bt_b(3,j)
          end do

          do j = 1,4
            k_sw(j,i) = k_sw(j,i) + bw_s(1,i) *ns(1,j)*xsj
     &                            + bw_s(2,i) *ns(2,j)*xsj
            k_st(j,i) = k_st(j,i) + bwt_s(1,i)*ns(1,j)*xsj
     &                            + bwt_s(2,i)*ns(2,j)*xsj
            k_bt(j,i) = k_bt(j,i) + bt_b1(i,1)*bb_b(1,j)
     &                            + bt_b1(i,2)*bb_b(2,j)
     &                            + bt_b1(i,3)*bb_b(3,j)
          end do

        end do

!       Construct stiffness matrices

        do i = 1, 4
          do j = 1,i

!           Bubble modes

            k_bb(i,j) = k_bb(i,j) + bb_b1(i,1)*bb_b(1,j)
     &                            + bb_b1(i,2)*bb_b(2,j)
     &                            + bb_b1(i,3)*bb_b(3,j)

!           Shear modes

            k_ss(i,j) = k_ss(i,j) - ns1(i,1)*ns(1,j)
     &                            - ns1(i,2)*ns(2,j)
          end do
        end do

!       Consistent load

        q0 = dm * d(8) * xsj
        do i = 1,4
          k       = mod(i+2 ,4) + 1
          p(1,i) = p(1,i) + q0*shp(3,i)
          p(2,i) = p(2,i) + q0*(shpm(3,i)*co(i) - shpm(3,k)*co(k))
          p(3,i) = p(3,i) + q0*(shpm(3,i)*si(i) - shpm(3,k)*si(k))
        end do

      end do

!     END LOOP ON GAUSS POINTS

!     Generate specific (diagonal) form for K_sb

      k_bs(1) = 16.0d0/9.0d0*(jac0(1,1)*jac0(2,2)-jac0(1,2)*jac0(2,1))
      k_bs(2) = k_bs(1)
      k_bs(3) = k_bs(1)*0.2d0
      k_bs(4) = k_bs(3)

!     Make symmetric parts

      do i = 2,4
        do j = 1,i-1
          k_bb(j,i) = k_bb(i,j)
        end do
      end do

!     Condense stiffness matrices: First (bubble modes)

      call invert(k_bb,4,4)

      do j = 1,4
        do i = 1,j
          k_ss(i,j) = k_ss(i,j) - k_bs(i)*k_bb(i,j)*k_bs(j)
          k_ss(j,i) = k_ss(i,j)
        end do
      end do

      do j = 1,12
        do i = 1,4
          app1(i,j) = k_bb(i,1)*k_bt(1,j) + k_bb(i,2)*k_bt(2,j)
     &              + k_bb(i,3)*k_bt(3,j) + k_bb(i,4)*k_bt(4,j)
        end do
      end do

      do j = 1,12
        do i = 1,4
          k_st(i,j) = k_st(i,j) - k_bs(i)*app1(i,j)
        end do
      end do

      do j = 1,12
        do i = 1,j
          k_tt(i,j) = k_tt(i,j) - k_bt(1,i)*app1(1,j)
     &                          - k_bt(2,i)*app1(2,j)
     &                          - k_bt(3,i)*app1(3,j)
     &                          - k_bt(4,i)*app1(4,j)
        end do
      end do

!     Condense stiffness matrices: Second (shear modes)

      call invert(k_ss,4,4)

      do j = 1,12
        do i = 1,4
          app1(i,j) = k_ss(i,1)*k_sw(1,j) + k_ss(i,2)*k_sw(2,j)
     &              + k_ss(i,3)*k_sw(3,j) + k_ss(i,4)*k_sw(4,j)
          app2(i,j) = k_ss(i,1)*k_st(1,j) + k_ss(i,2)*k_st(2,j)
     &              + k_ss(i,3)*k_st(3,j) + k_ss(i,4)*k_st(4,j)
        end do
      end do

      do j = 1,12
        do i = 1,j
          k_tt(i,j) = k_tt(i,j)
     &              - k_sw(1,i)*app1(1,j) - k_sw(2,i)*app1(2,j)
     &              - k_sw(3,i)*app1(3,j) - k_sw(4,i)*app1(4,j)
     &              - k_st(1,i)*app2(1,j) - k_st(2,i)*app2(2,j)
     &              - k_st(3,i)*app2(3,j) - k_st(4,i)*app2(4,j)
        end do

        do i = 1,12
          k_wt(i,j) = k_sw(1,i)*app2(1,j) + k_sw(2,i)*app2(2,j)
     &              + k_sw(3,i)*app2(3,j) + k_sw(4,i)*app2(4,j)
        end do
      end do

!     Form residual and assemble stiffness

      if (isw.eq.3 .or.isw.eq.6) then

        ctan1 = ctan(1) + d(78)*ctan(2)

!       Accumulate stiffness parts

        do j = 1,12
          do i = 1,j
            k_tt(i,j) = k_tt(i,j) - k_wt(i,j) - k_wt(j,i)
            k_tt(j,i) = k_tt(i,j)
          end do
        end do

!       Assemble element array and residual

        ii = 0
        i1 = 0
        do i = 1,4
          jj = 0
          j1 = 0
          do j = 1,4
            do k = 1,3
              do l = 1,3
!               Compute residual
                p(k,i) = p(k,i) - k_tt(ii+k,jj+l)*(ul(l,j,1)
     &                          + d(78)*ul(l,j,4))
!               Assemble element tangent matrix
                s(i1+k,j1+l) = k_tt(ii+k,jj+l)*ctan1
              end do
            end do
            jj = jj + 3
            j1 = j1 + ndf
          end do
          ii = ii + 3
          i1 = i1 + ndf
        end do

!     Recover stress modes

      elseif ((isw.eq.4).or.(isw.eq.8)) then

!       Set local displacement order

        ii = 0
        do i=1,4
          do j = 1,3
            td(ii+j) = ul(j,i,1)
          end do
          ii = ii + ndf
        end do

!       Multiply stiffness order

        do i=1,4
          s_hat(i) = 0.0d0
          b_hat(i) = 0.0d0
          bpp1(i)  = 0.0d0
          bpp2(i)  = 0.0d0
        end do

        do j=1,12
          do i=1,4
            bpp1(i) = bpp1(i) + (k_sw(i,j) + k_st(i,j))*td(j)
            bpp2(i) = bpp2(i) +  k_bt(i,j)             *td(j)
          end do
        end do

        do j=1,4
          do i=1,4
            s_hat(i) = s_hat(i) - k_ss(i,j)*bpp1(j)
          end do
        end do

        do j=1,4
          bpp2(j) = bpp2(j) + k_bs(j)*s_hat(j)
          do i=1,4
            b_hat(i) = b_hat(i) - k_bb(i,j)*bpp2(j)
          end do
        end do

!       Compute curvatures and moments

        if(isw.eq.4) then

!         Compute Gauss points, weights and geometry

          l   = 1
          call int2d (l,lint,sg)
          call geompq(xl,ndm,co,si,jac0)

          do l=1,lint

            call shpspq(sg(1,l),xl,shp,shpn,shpm,xsj,
     &                  jac,jinv,ndm)
            call bmatpq(sg(1,l),xl,shp,shpn,shpm,si,co,jac0,jinv,
     &                  xsj,bw_s,bt_b,bb_b,bwt_s,ns,ndm)

            call dmatpl(d,d(31),dd,ds,alphg)

            thk3 = d(14)**3/12.d0
            do j = 1,3
              do i = 1,3
                dd(i,j) = dd(i,j)*thk3
              end do
            end do

            xx = shp(3,1)*xl(1,1) + shp(3,2)*xl(1,2)
     &         + shp(3,3)*xl(1,3) + shp(3,4)*xl(1,4)
            yy = shp(3,1)*xl(2,1) + shp(3,2)*xl(2,2)
     &         + shp(3,3)*xl(2,3) + shp(3,4)*xl(2,4)

            call strepq(dd,bt_b,bb_b,b_hat,ns,s_hat,ul,ndf,
     &                  eps,sig,shear)

!           Output moments and curvatures

            mct = mct -2
            if(mct.le.0) then
              write(iow,2001) o,head
              if(ior.lt.0) write (*,2001) o,head
              mct = 50
            endif
            write(iow,2002) n_el,ma,(sig(j),j=1,5),xx,yy,eps,sig(6),
     &                      shear
            if(ior.lt.0) then
              write(*,2002) n_el,ma,(sig(j),j=1,5),xx,yy,eps,sig(6),
     &                      shear
            endif

          end do

!       Compute nodal stress values

        else
          call stcnpq(ix,d,xl,ul,s,shp,hr(nph),hr(nph+numnp),
     &                s_hat,b_hat,ndf,ndm,nel,numnp)
        end if

      endif

!     Compute consistent and lumped mass matrix

5     continue
      return

!     Formats for input-output

2001  format(a1,20a4//5x,'Element Moments'//'  Element Material',
     &   3x,'11-Moment',3x,'22-Moment',3x,'12-Moment',4x,
     &   '1-Moment',4x,'2-Moment'/2x,'1-Coord',2x,'2-Coord',3x,
     &   '11-Strain',3x,'22-Strain',3x,'12-Strain',12x,'Angle'/
     &   21x,'Shear_x  ',3x,'Shear_y')

2002  format(2i9,1p,5e12.3/0p,2f9.3,1p,3e12.3,0p,f18.2/18x,1p,2e12.3/1x)

      end subroutine plate2q

      subroutine stcnpq(ix,d,xl,ul,s,shp,dt,st,s_hat,b_hat,
     &                  ndf,ndm,nel,numnp)

!     Lumped and consistent projection routine

      implicit  none

      include  'iofile.h'

      integer       :: ndf, ndm, nel, numnp
      integer       :: i, j, l, ll, lint
      real (kind=8) :: thk, xsj, xg

      integer       :: ix(*)
      real (kind=8) :: dt(numnp),st(numnp,*),xl(ndm,*),shp(3,4),d(*)
      real (kind=8) :: ul(ndf,*),s(nel,*),sg(3,25)
      real (kind=8) :: shpn(3), shpm(3,4), jac0(2,2),jac(2,2), jinv(2,2)
      real (kind=8) :: eps(3), sig(6), dd(3,3), ds(2,2), alphg(3)
      real (kind=8) :: co(4),si(4)

      real (kind=8) :: bw_s(2,12), bt_b(3,12), bb_b(3,4), bwt_s(2,12)
      real (kind=8) :: ns(2,4), s_hat(4), b_hat(4), shear(2)

      save

!     Zero all matrices

      do i = 1,nel
        do j = 1,nel
          s(j,i) = 0.0d0
        end do ! j
      end do ! i
      do i = 1,12
        do j = 1,3
          bt_b(j,i) = 0.0d0
        end do ! j
      end do ! i

!     Compute Gauss points, weights and geometry

      l   = 3
      call int2d (l,lint,sg)
      call geompq(xl,ndm,co,si,jac0)

      do l=1,lint

        call shpspq(sg(1,l),xl,shp,shpn,shpm,xsj,jac,jinv,ndm)
        call bmatpq(sg(1,l),xl,shp,shpn,shpm,si,co,jac0,jinv,xsj,
     &              bw_s,bt_b,bb_b,bwt_s,ns,ndm)

        xsj = xsj*sg(3,l)

        call dmatpl(d,d(31),dd,ds,alphg)

        thk = d(14)**3/12.d0
        do j = 1,3
          do i = 1,3
            dd(i,j) = dd(i,j)*thk
          end do
        end do

        call strepq(dd,bt_b,bb_b,b_hat,ns,s_hat,ul,ndf,eps,sig,shear)

!       Compute consistent projection matrix

        do i = 1,nel
          xg   = shp(3,i)*xsj
          do j = 1,nel
            s(i,j) = s(i,j) + xg*shp(3,j)
          end do
        end do

!       Compute lumped projection and assemble stress integrals

        do j = 1,nel
          ll = abs(ix(j))
          if(ll.gt.0) then
            xg       = xsj*shp(3,j)
            dt(ll)   = dt(ll)     +        xg
            st(ll,1) = st(ll,1)   + sig(1)*xg
            st(ll,2) = st(ll,2)   + sig(2)*xg
            st(ll,4) = st(ll,4)   + sig(3)*xg
            st(ll,5) = st(ll,5)   + shear(1)*xg
            st(ll,6) = st(ll,6)   + shear(2)*xg
          endif
        end do

      end do

      end subroutine stcnpq

      subroutine shpspq(xi,xl,shp,shpn,shpm,xsj,jac,jinv,ndm)

!     Shape functions with linked edges

      implicit  none

      include  'eldata.h'

      integer       :: i, j, k, ndm

      real (kind=8) :: xl(ndm,4),shp(3,4),shpn(3),shpm(3,4)
      real (kind=8) :: jac(2,2),jinv(2,2)
      real (kind=8) :: xi(2),xi1p,xi1m,eta1m,eta1p,xi2,eta2,xsj,xsjinv

      save

!     Set up parameters

      xi1p  = 1.0d0 + xi(1)
      xi1m  = 1.0d0 - xi(1)
      eta1p = 1.0d0 + xi(2)
      eta1m = 1.0d0 - xi(2)
      xi2   = xi1p  * xi1m
      eta2  = eta1p * eta1m

!     Natural coordinate shape functions

      shp(3,1) =  0.25d0*xi1m*eta1m
      shp(3,2) =  0.25d0*xi1p*eta1m
      shp(3,3) =  0.25d0*xi1p*eta1p
      shp(3,4) =  0.25d0*xi1m*eta1p

!     Natural coordinate derivatives for mid-surface

      shp(1,1) = -0.25d0*eta1m
      shp(1,2) = -shp(1,1)
      shp(1,3) =  0.25d0*eta1p
      shp(1,4) = -shp(1,3)

      shp(2,1) = -0.25d0*xi1m
      shp(2,2) = -0.25d0*xi1p
      shp(2,3) = -shp(2,2)
      shp(2,4) = -shp(2,1)

!     Construct jacobian-transpose and its inverse

      do i = 1,2
        do j = 1,2
          jac(i,j) = 0.0d0
          do k = 1,nel
            jac(i,j) = jac(i,j) + xl(j,k)*shp(i,k)
          end do
        end do
      end do

      xsj = jac(1,1)*jac(2,2)-jac(1,2)*jac(2,1)

      if(xsj.lt.0.0d0) then
        write(*,*) 'NEGATIVE JACOBIAN: Element = ',n_el
        xsj = -xsj
      elseif(xsj.eq.0.0d0) then
        write(*,*) 'ZERO JACOBIAN: Element = ',n_el
        call plstop(.true.)
      endif

      xsjinv    = 1.d0/xsj
      jinv(1,1) = jac(2,2)*xsjinv
      jinv(2,2) = jac(1,1)*xsjinv
      jinv(1,2) =-jac(1,2)*xsjinv
      jinv(2,1) =-jac(2,1)*xsjinv

!     Shape function 'N' and derivatives

      shpn(1)   = - 2.0d0*xi(1)*eta2
      shpn(2)   = - 2.0d0*xi(2)*xi2
      shpn(3)   =   xi2 * eta2

!     Shape function 'M' and derivatives (0.125=1/8; 0.0625=1/16)

      shpm(3,1) =   xi2*eta1m * 0.0625d0
      shpm(3,2) =   xi1p*eta2 * 0.0625d0
      shpm(3,3) =   xi2*eta1p * 0.0625d0
      shpm(3,4) =   xi1m*eta2 * 0.0625d0

      shpm(1,1) = - xi(1) * eta1m * 0.125d0
      shpm(1,2) =   eta2* 0.0625d0
      shpm(1,3) = - xi(1) * eta1p * 0.125d0
      shpm(1,4) = - eta2* 0.0625d0

      shpm(2,1) = - xi2* 0.0625d0
      shpm(2,2) = - xi(2) * xi1p * 0.125d0
      shpm(2,3) =   xi2* 0.0625d0
      shpm(2,4) = - xi(2) * xi1m * 0.125d0

      end subroutine shpspq

      subroutine geompq(xl,ndm,co,si,jac0)

!     Compute tangential directions
!     ATT.: co() and si() and cosine and sine times side length

      implicit  none

      integer       :: ndm, i, j
      real (kind=8) :: xl(ndm,*), co(4), si(4), jac0(2,2)

      save

!     Compute side length * angles

      do i = 1,4
        j     =   mod(i,4) + 1
        co(i) = - xl(2,i)  + xl(2,j)
        si(i) =   xl(1,i)  - xl(1,j)
      end do

!     Compute element center jacobian

      jac0(1,1) = 0.25d0*(-xl(1,1) + xl(1,2) + xl(1,3) - xl(1,4))
      jac0(2,1) = 0.25d0*(-xl(1,1) - xl(1,2) + xl(1,3) + xl(1,4))
      jac0(1,2) = 0.25d0*(-xl(2,1) + xl(2,2) + xl(2,3) - xl(2,4))
      jac0(2,2) = 0.25d0*(-xl(2,1) - xl(2,2) + xl(2,3) + xl(2,4))

      end subroutine geompq

      subroutine bmatpq(xi,xl,shp,shpn,shpm,si,co,jac0,jinv,xsj,
     &                  bw_s,bt_b,bb_b,bwt_s,ns,ndm)

!     Compute all B-strain-type matrices for mixed formulation

      implicit  none

      integer       :: ndm

      real (kind=8) :: xi(2)
      real (kind=8) :: xl(ndm,*), shp(3,4), shpn(3), shpm(3,4)
      real (kind=8) :: si(4), co(4)
      real (kind=8) :: jac0(2,2), jinv(2,2), xsj, xsjinv, xsjinv2
      real (kind=8) :: bw_s(2,3,4), bt_b(3,3,4), bb_b(3,*), bwt_s(2,3,4)
      real (kind=8) :: ns(2,*)

      integer       :: i, c

      real (kind=8) :: b1(4), b2(4)
      real (kind=8) :: f1(4),f1c(4),f1s(4), f2(4),f2c(4),f2s(4)
      real (kind=8) :: aa, bb, dj1, dj2
      real (kind=8) :: Nj, Nj_xi, Nj_eta, Nj_x, Nj_y
      real (kind=8) :: xiNj_xi, xiNj_eta, etaNj_xi, etaNj_eta
      real (kind=8) :: N_x, N_y, xiNj_x, xiNj_y, etaNj_x, etaNj_y
      real (kind=8) :: ax, ay, bx, by, cx, cy, dx, dy

      save

!     Construct parameters for B-strain-type matrices

!     Parameters for linear shape functions and for rotational

!     Contribution to transverse displacement

      do i = 1,4
        b1(i)   = jinv(1,1)*shp(1,i)  + jinv(1,2)*shp(2,i)
        b2(i)   = jinv(2,1)*shp(1,i)  + jinv(2,2)*shp(2,i)
        f1(i)   = jinv(1,1)*shpm(1,i) + jinv(1,2)*shpm(2,i)
        f1c(i)  = f1(i)*co(i)
        f1s(i)  = f1(i)*si(i)
        f2(i)   = jinv(2,1)*shpm(1,i) + jinv(2,2)*shpm(2,i)
        f2c(i)  = f2(i)*co(i)
        f2s(i)  = f2(i)*si(i)
      end do

!     Parameters for bubble functions

      dj1       = (xl(1,1) - xl(1,2) + xl(1,3) - xl(1,4))*0.25d0
      dj2       = (xl(2,1) - xl(2,2) + xl(2,3) - xl(2,4))*0.25d0
      aa        = jac0(1,1)*dj2 - jac0(1,2)*dj1
      bb        = jac0(2,2)*dj1 - jac0(2,1)*dj2

      xsjinv    = 1.0d0 / xsj
      xsjinv2   = xsjinv * xsjinv

      N_x       = jinv(1,1)*shpn(1) + jinv(1,2)*shpn(2)
      N_y       = jinv(2,1)*shpn(1) + jinv(2,2)*shpn(2)

      Nj        = shpn(3) * xsjinv
      Nj_xi     = xsjinv2 * ( shpn(1)*xsj - shpn(3)*aa)
      Nj_eta    = xsjinv2 * ( shpn(2)*xsj - shpn(3)*bb)
      Nj_x      = jinv(1,1)*Nj_xi + jinv(1,2)*Nj_eta
      Nj_y      = jinv(2,1)*Nj_xi + jinv(2,2)*Nj_eta

      xiNj_xi   = Nj + xi(1)*Nj_xi
      xiNj_eta  =      xi(1)*Nj_eta
      etaNj_xi  =      xi(2)*Nj_xi
      etaNj_eta = Nj + xi(2)*Nj_eta

      xiNj_x    = jinv(1,1)*xiNj_xi + jinv(1,2)*xiNj_eta
      xiNj_y    = jinv(2,1)*xiNj_xi + jinv(2,2)*xiNj_eta
      etaNj_x   = jinv(1,1)*etaNj_xi + jinv(1,2)*etaNj_eta
      etaNj_y   = jinv(2,1)*etaNj_xi + jinv(2,2)*etaNj_eta

      ax        =  etaNj_x*jac0(2,1)
      ay        =  etaNj_y*jac0(2,1)
      bx        = -xiNj_x*jac0(1,1)
      by        = -xiNj_y*jac0(1,1)

      cx        =  etaNj_x*jac0(2,2)
      cy        =  etaNj_y*jac0(2,2)
      dx        = -xiNj_x*jac0(1,2)
      dy        = -xiNj_y*jac0(1,2)

!     Construct B-strain-type matrices

      do i = 1,4
        bw_s(1,1,i)  =   b1(i)                            !  B_w_s
        bw_s(2,1,i)  =   b2(i)

        bt_b(1,3,i)  =   b1(i)                            !  B_t_b
        bt_b(2,2,i)  = - b2(i)
        bt_b(3,2,i)  = - b1(i)
        bt_b(3,3,i)  =   b2(i)

        c            =   mod(i+2,4) + 1                   !  B_wt_s
        bwt_s(1,2,i) =           f1c(i) - f1c(c)
        bwt_s(1,3,i) =  shp(3,i)+f1s(i) - f1s(c)
        bwt_s(2,2,i) = -shp(3,i)+f2c(i) - f2c(c)
        bwt_s(2,3,i) =           f2s(i) - f2s(c)

      end do

      bb_b(1,1) =   jac0(2,2) * Nj_x                     !    B_b_b
      bb_b(1,2) = - jac0(1,2) * Nj_x
      bb_b(2,1) = - jac0(2,1) * Nj_y
      bb_b(2,2) =   jac0(1,1) * Nj_y
      bb_b(3,1) = - jac0(2,1) * Nj_x + jac0(2,2) * Nj_y
      bb_b(3,2) =   jac0(1,1) * Nj_x - jac0(1,2) * Nj_y

      bb_b(1,3) =   cx
      bb_b(1,4) =   dx
      bb_b(2,3) = - ay
      bb_b(2,4) = - by
      bb_b(3,3) = - ax + cy
      bb_b(3,4) = - bx + dy

!     Construct N_s

      ns(1,1)   = jac0(1,1)
      ns(1,2)   = jac0(2,1)
      ns(2,1)   = jac0(1,2)
      ns(2,2)   = jac0(2,2)
      ns(1,3)   = jac0(1,1)*xi(2)
      ns(1,4)   = jac0(2,1)*xi(1)
      ns(2,3)   = jac0(1,2)*xi(2)
      ns(2,4)   = jac0(2,2)*xi(1)

      end subroutine bmatpq

      subroutine strepq(dd,bt_b,bb_b,b_hat,ns,s_hat,ul,ndf,
     &                  eps,sig,shear)

!     Compute curvatures [eps(1)=eps_x,eps(2)=eps_y,eps(3)=gamma_xy]
!             stresses   [sig(1)=mom_x,sig(2)=mom_y,sig(3)=mom_xy]
!                         shear(1)=q_x,shear(2)=q_y]
      implicit  none

      integer       :: i,j,ndf
      real (kind=8) :: dd(3,3)    , eps(3)   , sig(6)   , shear(2)
      real (kind=8) :: bt_b(3,3,4), bb_b(3,4), ns(2,4)
      real (kind=8) :: b_hat(4)   , s_hat(4) , ul(ndf,4)

      save

!     Initialize

      eps(1)   = 0.0d0
      eps(2)   = 0.0d0
      eps(3)   = 0.0d0
      shear(1) = 0.0d0
      shear(2) = 0.0d0

!     Compute bending strains

      do i=1,4
        do j = 1,3
          eps(1) = eps(1) + bt_b(1,j,i)*ul(j,i)
          eps(2) = eps(2) + bt_b(2,j,i)*ul(j,i)
          eps(3) = eps(3) + bt_b(3,j,i)*ul(j,i)
        end do
      end do
      do i=1,4
        eps(1) = eps(1) + bb_b(1,i)*b_hat(i)
        eps(2) = eps(2) + bb_b(2,i)*b_hat(i)
        eps(3) = eps(3) + bb_b(3,i)*b_hat(i)
      end do

!     Compute moments

      sig(1) = dd(1,1)*eps(1) + dd(1,2)*eps(2) + dd(1,3)*eps(3)
      sig(2) = dd(2,1)*eps(1) + dd(2,2)*eps(2) + dd(2,3)*eps(3)
      sig(3) = dd(3,1)*eps(1) + dd(3,2)*eps(2) + dd(3,3)*eps(3)

      call pstr2d(sig,sig(4))

!     Compute shears

      do i=1,4
        shear(1) = shear(1) + ns(1,i)*s_hat(i)
        shear(2) = shear(2) + ns(2,i)*s_hat(i)
      end do

      end subroutine strepq

      subroutine plate2t(d,ul,xl,ix,s,p,ndf,ndm,nst,isw)

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:
!             Triangular plate: 3 dofs per node (w, theta-x, theta-y)
!                               2 bubble modes for rotations
!                               2 shear parameters

!      Mixed approach for shear stiffness.
!             Step 1: Condensation of bubble terms
!             Step 2: Condensation of shear terms

!      Three integration points are used.
!-----[--.----+----.----+----.-----------------------------------------]

!      Input parameters set as follows:

!             ndm = 2 (x,y cartesian coordinates at nodes)
!             ndf = 3 (w,theta-x,theta-y, at nodes)
!             nen = 3 nodes (counterclockwise around element)
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'bdata.h'
      include  'cdata.h'
      include  'eldata.h'
      include  'eltran.h'
      include  'iofile.h'
      include  'mdata.h'
      include  'prstrs.h'
      include  'strnum.h'
      include  'comblk.h'

      integer       :: ndf,ndm,nst,isw
      integer       :: i,j,k,l, lint, i1,j1, i2,j2

      real (kind=8) :: den, area,ar3,ar24, third, xx,yy, thk,thk3, psi
      real (kind=8) :: ctan1,ctan3

      integer       :: ix(*)
      real (kind=8) :: d(*),ul(ndf,nen,*),xl(ndm,*),s(nst,*),p(ndf,*)
      real (kind=8) :: b(3),c(3),co(3),si(3),el(4,3),ss(9,9)
      real (kind=8) :: bb(2,3,3), bbd(2,3,4), bs(3,2,3), hh(4,4),gamm(2)
      real (kind=8) :: dbg(3,3), dsg(2,2)
      real (kind=8) :: dd(3,3), dv(3), uhat(2), shear(2)
      real (kind=8) :: alphm(3), epsc(3),eps(3,3),sigc(6),sig(3,3)

      save

      data      third / 0.3333333333333333d0 /

!     Go to correct array processor

      go to (1,2,3,3,5,3,7,3,3,7,3), isw

1     return

!     Check element for errors

2     call cktris(ix,xl,bbd,ndm)
      return

!     Compute stiffness and internal force quantities

!     Compute geometric factors

3     call geompt(xl,ndm,co,si,b,c,area)

!     Moduli multiplied by thickness factors

      ctan1 = ctan(1) + ctan(2)*d(78)
      ctan3 = ctan(3) + ctan(2)*d(77)

      psi   = d(31)

      call dmatpl(d,psi,dbg,dsg,alphm)

      thk  = d(14)
      thk3 = thk**3/12.d0
      ar3  = thk3*area
      do j = 1,3
        do i = 1,3
          dd(i,j) = dbg(i,j)*ar3
        end do
      end do

!     Compute Bs-bar matrix

      den = 0.5d0*third*area
      do i=1,3
        j  = mod(i,3) + 1
        k  = mod(j,3) + 1

        bs(1,1,i) =  b(i) * area
        bs(2,1,i) = (b(j)*co(j) - b(k)*co(k)) * den
        bs(3,1,i) = (b(j)*si(j) - b(k)*si(k) + 2.0d0) * den
        bs(1,2,i) =  c(i) * area
        bs(2,2,i) = (c(j)*co(j) - c(k)*co(k) - 2.0d0) * den
        bs(3,2,i) = (c(j)*si(j) - c(k)*si(k)) * den

      end do

!     Numerical integration for bubble

      hh(3,3) = 0.0d0
      hh(3,4) = 0.0d0
      hh(4,4) = 0.0d0

      ar3     = 5.0625d0*third

      do l = 1,3  ! { begin integration

!     Multiply by bending stiffness * quadrature weight

        do j = 1,3
          bbd(1,j,4) =  (c(l)*dd(2,j) + b(l)*dd(3,j))*ar3
          bbd(2,j,4) = -(b(l)*dd(1,j) + c(l)*dd(3,j))*ar3
        end do

!     Bubble bending stiffness

        hh(3,3) = hh(3,3) + bbd(1,2,4)*c(l) + bbd(1,3,4)*b(l)
        hh(3,4) = hh(3,4) - bbd(1,1,4)*b(l) - bbd(1,3,4)*c(l)
        hh(4,4) = hh(4,4) - bbd(2,1,4)*b(l) - bbd(2,3,4)*c(l)

      end do  ! end integration }

!     Bubble shear mode and symmetric parts

      hh(1,4) =  0.5d0*area
      hh(2,3) = -hh(1,4)
      hh(3,2) =  hh(2,3)
      hh(4,1) =  hh(1,4)
      hh(4,3) =  hh(3,4)

!     Compute shear compliances * area

      den = area/((dsg(1,1)*dsg(2,2) - dsg(1,2)*dsg(2,1))*thk*d(10))

      hh(1,1) = -dsg(2,2)*den
      hh(1,2) =  dsg(1,2)*den
      hh(2,1) =  dsg(2,1)*den
      hh(2,2) = -dsg(1,1)*den

      if ( isw.eq.3 .or. isw.eq.6 ) then

!       Compute bending stiffness

        ar3 =  d(8)*area*third
        ar24 = ar3*0.125d0

        do i = 1,3
          j          = mod(i,3) + 1
          k          = mod(j,3) + 1

          bb(1,1,i) =  0.0d0
          bb(1,2,i) = -c(i)
          bb(1,3,i) = -b(i)
          bb(2,1,i) =  b(i)
          bb(2,2,i) =  0.0d0
          bb(2,3,i) =  c(i)

          p(1,i)    =  ar3
          p(2,i)    =  ar24*(co(k)-co(j))
          p(3,i)    =  ar24*(si(k)-si(j))

        end do

!       Strain-displacement times moduli

        do i = 1,3
          do j = 1,3
            bbd(1,j,i) = bb(1,2,i)*dd(2,j) + bb(1,3,i)*dd(3,j)
            bbd(2,j,i) = bb(2,1,i)*dd(1,j) + bb(2,3,i)*dd(3,j)
          end do
        end do

!       Bending stiffness (constant part)

        j1 = 2
        do j=1,3
          i1 = 2
          do i=1,j
            s(i1  ,j1  ) = bbd(1,2,i)*bb(1,2,j) + bbd(1,3,i)*bb(1,3,j)
            s(i1+1,j1  ) = bbd(2,2,i)*bb(1,2,j) + bbd(2,3,i)*bb(1,3,j)
            s(i1  ,j1+1) = bbd(1,1,i)*bb(2,1,j) + bbd(1,3,i)*bb(2,3,j)
            s(i1+1,j1+1) = bbd(2,1,i)*bb(2,1,j) + bbd(2,3,i)*bb(2,3,j)

!           Make symmetric part

            s(j1  ,i1  ) = s(i1  ,j1  )
            s(j1  ,i1+1) = s(i1+1,j1  )
            s(j1+1,i1  ) = s(i1  ,j1+1)
            s(j1+1,i1+1) = s(i1+1,j1+1)
            i1 = i1 + ndf
          end do
          j1 = j1 + ndf
        end do

!       Static condensation and load vector

        call sconpt(hh,bs,s,ndf,nst,1)

!       Compute stress residual

        do i = 1,nst
          j1 = 0
          do j = 1,3
            do k = 1,ndf
              p(i,1)    = p(i,1) - s(i,j1+k)*(ul(k,j,1)
     &                                + d(78)*ul(k,j,4))
              s(i,j1+k) = s(i,j1+k)*ctan1
            end do
            j1 = j1 + ndf
          end do
        end do

!       Add inertial parts if necessary

        if(ctan3.ne.0.0d0) then

          call masspl(d,xl,ndm,3,9, hh,ss)

          i1 = 0
          i2 = 0
          do i = 1,3

            j1 = 0
            j2 = 0
            do j = 1,3

              do k = 1,3
                do l = 1,3
                  p(k,i)       = p(k,i) - ss(i2+k,j2+l)*(ul(l,j,5)
     &                                           + d(77)*ul(l,j,4))
                  s(i1+k,j1+l) = s(i1+k,j1+l) + ctan3*ss(i2+k,j2+l)
                end do
              end do

              j1 = j1 + ndf
              j2 = j2 + 3
            end do

            i1 = i1 + ndf
            i2 = i2 + 3
          end do

        endif

      else if ((isw.eq.4).or.(isw.eq.8).or.(isw.eq.11)) then

!       Compute curvatures and moments

!       Get quadrature

        l = -3
        call tint2d(l,lint,el)

!       Recover bubble and shear parameters

        call sconpt(hh,bs,s,ndf,nst,0)

        gamm(1) = 0.0d0
        gamm(2) = 0.0d0
        do i = 1,3
          gamm(1) = gamm(1) - bs(1,1,i)*ul(1,i,1)
     &                      - bs(2,1,i)*ul(2,i,1)
     &                      - bs(3,1,i)*ul(3,i,1)
          gamm(2) = gamm(2) - bs(1,2,i)*ul(1,i,1)
     &                      - bs(2,2,i)*ul(2,i,1)
     &                      - bs(3,2,i)*ul(3,i,1)
        end do

!       Compute shear stress/strain and bubble displacement

        shear(1) =  hh(1,1)*gamm(1) + hh(1,2)*gamm(2)
        shear(2) =  hh(2,1)*gamm(1) + hh(2,2)*gamm(2)

        dv(1)    = -hh(3,2)*shear(2)
        dv(2)    = -hh(4,1)*shear(1)

        uhat(1)  =  hh(3,3)*dv(1) + hh(3,4)*dv(2)
        uhat(2)  =  hh(4,3)*dv(1) + hh(4,4)*dv(2)

!       Get elastic material properties

        psi   = d(31)

        call dmatpl(d,psi,dbg,dsg,alphm)

        thk   = d(14)
        thk3  = thk**3/12.d0
        do i = 1,3 ! {
          do j = 1,3 ! {
            dd(i,j) = dbg(i,j)*thk3
          end do ! j   }
        end do ! i   }

!       Constant part of bending/shear strains

        epsc(1) =   b(1)*ul(3,1,1)
     &            + b(2)*ul(3,2,1)
     &            + b(3)*ul(3,3,1)
        epsc(2) = - c(1)*ul(2,1,1)
     &            - c(2)*ul(2,2,1)
     &            - c(3)*ul(2,3,1)
        epsc(3) = - b(1)*ul(2,1,1)
     &            - b(2)*ul(2,2,1)
     &            - b(3)*ul(2,3,1)
     &            + c(1)*ul(3,1,1)
     &            + c(2)*ul(3,2,1)
     &            + c(3)*ul(3,3,1)

        den = 1.d0/((dsg(1,1)*dsg(2,2) - dsg(1,2)*dsg(2,1))*thk*d(10))

        gamm(1) = ( dsg(2,2)*shear(1) - dsg(1,2)*shear(2))*den
        gamm(2) = (-dsg(1,2)*shear(1) + dsg(1,1)*shear(2))*den

        if(isw.eq.4) then

          xx    = (xl(1,1) + xl(1,2) + xl(1,3))/3.d0
          yy    = (xl(2,1) + xl(2,2) + xl(2,3))/3.d0

          sigc(1) = dd(1,1)*epsc(1) + dd(1,2)*epsc(2) + dd(1,3)*epsc(3)
          sigc(2) = dd(2,1)*epsc(1) + dd(2,2)*epsc(2) + dd(2,3)*epsc(3)
          sigc(3) = dd(3,1)*epsc(1) + dd(3,2)*epsc(2) + dd(3,3)*epsc(3)

          call pstr2d(sigc(1),sigc(4))

!         Output moments and curvatures

          mct = mct -2
          if(mct.le.0) then
            write(iow,2001) o,head
            if(ior.lt.0) then
              write (*,2001) o,head
            endif
            mct = 50
          endif
          write(iow,2002) n_el,ma,(sigc(j),j=1,5),xx,yy,epsc,sigc(6),
     &                    shear
          if(ior.lt.0) then
            write(*,2002) n_el,ma,(sigc(j),j=1,5),xx,yy,epsc,sigc(6),
     &                    shear
          endif

        elseif(isw.eq.8 .or.isw.eq.11) then

          do l = 1, lint

!           Area weight for projection

            dv(l) = area

!           Bubble mode bending functions

            eps(1,l) = epsc(1) - 2.25d0*b(l)*uhat(2)
            eps(2,l) = epsc(2) + 2.25d0*c(l)*uhat(1)
            eps(3,l) = epsc(3) + 2.25d0*(b(l)*uhat(1) - c(l)*uhat(2))

!           Compute moments

            sig(1,l) = dd(1,1)*eps(1,l) + dd(1,2)*eps(2,l)
     &               + dd(1,3)*eps(3,l)
            sig(2,l) = dd(2,1)*eps(1,l) + dd(2,2)*eps(2,l)
     &               + dd(2,3)*eps(3,l)
            sig(3,l) = dd(3,1)*eps(1,l) + dd(3,2)*eps(2,l)
     &               + dd(3,3)*eps(3,l)

          end do

!         Compute nodal stress values

          if(isw.eq.8) then

            call stcnpt(ix,dv,el,lint,sig,eps,shear,gamm,
     &                  hr(nph),hr(nph+numnp),hr(ner),erav,numnp)
          end if

        end if

      end if

      return

!     Compute consistent and lumped mass matrix

5     call masspl(d,xl,ndm,ndf,nst, p,s)
      return

!     Compute surface tractions

7     continue
      return

!     Formats for input-output

2001  format(a1,20a4//5x,'Element Moments'//'  Element Material',
     1   3x,'11-Moment',3x,'22-Moment',3x,'12-Moment',4x,
     2   '1-Moment',4x,'2-Moment'/2x,'1-Coord',2x,'2-Coord',3x,
     3   '11-Strain',3x,'22-Strain',3x,'12-Strain',12x,'Angle'/
     4   21x,'Shear-x  ',3x,'Shear-y')

2002  format(2i9,1p,5e12.3/0p,2f9.3,1p,3e12.3,0p,f18.2/18x,1p,2e12.3/1x)

      end subroutine plate2t

      subroutine stcnpt(ix,dv,el,lint,sig,eps,shear,gamm,dt,st,ser,
     &                  errav,numnp)

!     Lumped projection routine

      implicit  none

      integer       :: l, j, ll, lint, numnp
      integer       :: ix(*)
      real (kind=8) :: xg, errav
      real (kind=8) :: dt(numnp),st(numnp,*),ser(*),sig(3,*),eps(3,*)
      real (kind=8) :: shear(2), gamm(2), el(3,*),dv(*)

      save

      do l = 1,lint
        do j = 1,3
          ll = abs(ix(j))

          if(ll.gt.0) then
            xg     = dv(l)*el(j,l)
            dt(ll) = dt(ll) + xg
            st(ll,1) = st(ll,1)   + sig(1,l)*xg
            st(ll,2) = st(ll,2)   + sig(2,l)*xg
            st(ll,4) = st(ll,4)   + sig(3,l)*xg
            st(ll,5) = st(ll,5)   + shear(1)*xg
            st(ll,6) = st(ll,6)   + shear(2)*xg
            st(ll,7) = st(ll,7)   + eps(1,l)*xg
            st(ll,8) = st(ll,8)   + eps(2,l)*xg
            st(ll,10) = st(ll,10) + eps(3,l)*xg
            st(ll,11) = st(ll,11) + gamm(1)*xg
            st(ll,12) = st(ll,12) + gamm(2)*xg

            ser(ll)   = ser(ll)   + errav*xg

          endif

        end do
      end do

      end subroutine stcnpt

      subroutine geompt(xl,ndm,co,si,b,c,area)

!     Compute geometric quantities and shape function derivatives
!     Linear shape functions ==> constant derivatives [b(i) and c(i)]

      implicit  none

      integer       :: i, ndm

      real (kind=8) :: det,area
      real (kind=8) :: b(3),c(3),co(3),si(3)
      real (kind=8) :: xl(ndm,*)

      save

      b(1)  =  xl(2,2) - xl(2,3)
      b(2)  =  xl(2,3) - xl(2,1)
      b(3)  =  xl(2,1) - xl(2,2)

      c(1)  =  xl(1,3) - xl(1,2)
      c(2)  =  xl(1,1) - xl(1,3)
      c(3)  =  xl(1,2) - xl(1,1)

      det  = c(2)*b(1) - c(1)*b(2)
      area = 1.d0/det

      do i=1,3
        co(i) = -b(i)
        si(i) = -c(i)
        b(i)  =  b(i)*area
        c(i)  =  c(i)*area
      end do

      area = det*0.5d0

      end subroutine geompt

      subroutine sconpt(hh,bs,s,ndf,nst,isw)

!     Static condensation of internal dofs

      implicit  none

      include  'eltran.h'

      integer       :: isw, i, j, ii, jj, i1, j1, ndf,nst
      real (kind=8) :: hh(4,4),bs(3,2,3), s(nst,nst), tem(2), det

      save

!     Solve bubble and shear modes

      det     = 1.d0/(hh(3,3)*hh(4,4) - hh(3,4)*hh(4,3))
      tem(1)  =  hh(4,4)*det
      hh(3,4) = -hh(3,4)*det
      hh(4,3) = -hh(4,3)*det
      hh(4,4) =  hh(3,3)*det
      hh(3,3) =  tem(1)

      hh(1,1) = hh(1,1) - hh(1,4)*hh(4,4)*hh(4,1)
      hh(2,1) = hh(2,1) - hh(2,3)*hh(3,4)*hh(4,1)
      hh(1,2) = hh(1,2) - hh(1,4)*hh(4,3)*hh(3,2)
      hh(2,2) = hh(2,2) - hh(2,3)*hh(3,3)*hh(3,2)

      det     =  1.0d0/(hh(1,1)*hh(2,2) - hh(1,2)*hh(2,1))
      tem(1)  =  hh(2,2)*det
      hh(1,2) = -hh(1,2)*det
      hh(2,1) = -hh(2,1)*det
      hh(2,2) =  hh(1,1)*det
      hh(1,1) =  tem(1)

!     Condense stiffness matrix

      if(isw.eq.1) then

        j1 = 0
        do jj = 1,3
          do j = 1,3

            tem(1) = hh(1,1)*bs(j,1,jj) + hh(1,2)*bs(j,2,jj)
            tem(2) = hh(2,1)*bs(j,1,jj) + hh(2,2)*bs(j,2,jj)

            i1 = 0
            do ii = 1,3
              do i = 1,3
                s(i+i1,j+j1) = s(i+i1,j+j1) - bs(i,1,ii)*tem(1)
     &                                      - bs(i,2,ii)*tem(2)
              end do
            i1 = i1 + ndf
            end do
          end do
          j1 = j1 + ndf
        end do

      end if

      end subroutine sconpt

      subroutine dmatpl(d,psi,dmg,dsg,alphg)

!     Rotation of material arrays from principal to local element directions


!     Inputs: d        - Array with material properties
!             psi      - Angle from y1-axis (local) to 1-axis (principal)
!     Output: dmg(3,3) - Plane stress modulus matrix
!             dsg(2,2) - Transverse shear modulus matrix
!             alphg(3) - global thermal strain/temperature

!     Variables used in subroutine

!             qm(3,3)  - Transformation matrix for plane stresses

!             sig_glob = qm * sig_princ

!             dml(3,3) - Local (orthotropic ) plane modulus matrix
!             dmlqj(3) - intermediate matrix for triple product
!             alphl(3) - local  thermal strain/temperature

!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      integer       :: i, j

      real (kind=8) :: psi, si, co, s2, c2, cs

      real (kind=8) :: d(*)
      real (kind=8) :: dml(3,3), dmg(3,3), dsg(2,2), qm(3,3), dmlqj(3)
      real (kind=8) :: alphg(3)

      save

!     Set up constants for transformation

      si = sin(psi)
      co = cos(psi)
      s2 = si*si
      c2 = co*co
      cs = co*si

!     Transformation matrix for plane stress

      qm(1,1) =  c2
      qm(1,2) =  s2
      qm(1,3) =  cs
      qm(2,1) =  s2
      qm(2,2) =  c2
      qm(2,3) = -cs
      qm(3,1) = -2.d0 * cs
      qm(3,2) =  2.d0 * cs
      qm(3,3) =  c2 - s2

!     Set up local (orthotropic) plane stress matrix

      dml(1,1) = d(21)
      dml(2,2) = d(22)

      dml(1,2) = d(24)
      dml(2,1) = d(24)

      dml(3,3) = d(27)

!     Convert plane stress local to global matrix

      do j = 1,3 ! {

        dmlqj(1) = dml(1,1)*qm(1,j) + dml(1,2)*qm(2,j)
        dmlqj(2) = dml(2,1)*qm(1,j) + dml(2,2)*qm(2,j)
        dmlqj(3) = dml(3,3)*qm(3,j)

        do i = 1,3 ! {
          dmg(i,j) = qm(1,i)*dmlqj(1) + qm(2,i)*dmlqj(2)
     +             + qm(3,i)*dmlqj(3)
        end do ! i   }

      end do ! j   }

!     Set up global shear matrix

      dsg(1,1) = c2 * d(29) + s2 * d(28)
      dsg(2,2) = s2 * d(29) + c2 * d(28)
      dsg(1,2) = cs * ( d(29) - d(28) )
      dsg(2,1) = dsg(1,2)

!     Convert to global thermal stiffness vector

      alphg(1) = c2 * d(47) + s2 * d(48)
      alphg(2) = s2 * d(47) + c2 * d(48)
      alphg(3) = cs * ( d(47) - d(48) )

      end subroutine dmatpl

      subroutine masspl(d,xl,ndm,ndf,nst, p,s)

      implicit  none

      integer       :: ndm,ndf,nst, i,i1, j,k, l,lint
      real (kind=8) :: d(*),xl(ndm,*), p(ndf,*),s(nst,nst)
      real (kind=8) :: co(3),si(3),b(3),c(3),el(4,3),an(24)
      real (kind=8) :: area, ar3,ar24, ccm,clm, den, third

      save

      data      third /0.3333333333333333d0/

!     Compute consistent and lumped mass matrix

      l = -3
      call tint2d(l,lint,el)

!     Compute geometric factors

      call geompt(xl,ndm,co,si,b,c,area)

      ar3 = d(4)*d(14)*area
      ccm = d(7)*ar3
      clm = ar3 - ccm

!     Lumped mass matrix part

      i1 = 1
      do i=1,3
        p(1,i)   = ar3*third
        s(i1,i1) = s(i1,i1) + clm*third
        i1       = i1 + ndf
      end do

!     Consistent mass matrix part

      do l=1,lint

        i1 = 1
        do i=1,3
          j        = mod(i,3) + 1
          k        = mod(j,3) + 1
          an(i1  ) = el(i,l)
          an(i1+1) = el(i,l)*(el(j,l)*(xl(2,j) - xl(2,i))
     &                      - el(k,l)*(xl(2,i) - xl(2,k)))*0.5d0
          an(i1+2) = el(i,l)*(el(j,l)*(xl(1,i) - xl(1,j))
     &                      - el(k,l)*(xl(1,k) - xl(1,i)))*0.5d0
          i1 = i1 + ndf
        end do

        den = ccm*el(4,l)
        do j=1,3*ndf
          ar24 = an(j)*den
          do k=1,j
            s(j,k) = s(j,k) + ar24*an(k)
            s(k,j) = s(j,k)
          end do
        end do
      end do

      end subroutine masspl
