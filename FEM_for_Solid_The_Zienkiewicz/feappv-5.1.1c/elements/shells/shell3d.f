!$Id:$
      subroutine shell3d(d,ul,xl,ix,s,p,ndf,ndm,nst,isw)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:  Quadrilateral shell element for feap

!     Input parameters set as follows:

!       Small deformation
!         ndm = 3 (x,y,z cartesian coordinates at nodes)
!         ndf = 6 (u-x,u-y,u-z,r-x,r-y,r-z at nodes)
!         nen = 4 nodes (counterclockwise around element)

!       Note: 1-direction bisects diagonals between 2-3 element and
!             2-direction bisects diagonals between 3-4 element nodes.
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      implicit  none

      include  'eldata.h'
      include  'iofile.h'
      include  'hdata.h'
      include  'mdata.h'
      include  'strnum.h'

      include  'comblk.h'

      integer       :: ndm,ndf,nst,isw, tdof, i
      integer       :: ix(*)

      real (kind=8) :: d(*),xl(ndm,*),ul(ndf,*),s(nst,*),p(nst)

      save

!     Input material properties

      if(isw.eq.1) then

        if(ior.lt.0) write(*,2000)
        write(iow,2000)
        call inmate(d,tdof, 24  ,5)

!       Deactivate dof in element for dof > 6

        do i = 7,ndf
          ix(i) = 0
        end do

!       History for through thickness integrations

        if(nint(d(102)).gt.1) then
          nh1 = nh1*nint(d(102))
        endif

!       Construct rotation parameters: u-x = 1; u-y = 2 (same as defaults)

        ea(1,-iel) = 1
        ea(2,-iel) = 2

!       Construct rotation parameters: theta-x = 4; theta-y = 5

        er(1,-iel) = 4
        er(2,-iel) = 5

!       Set plot sequence

        pstyp = 2

!       Set maximum number of stress plots

        istv = 24

!     Remaining options

      else

!       Set zero state

!       Small deformation

        if(d(18).gt.0.0d0) then
          if(nint(d(102)).gt.1) then
            call shl3di(d,ul,xl,ix,s,p,ndf,ndm,nst,isw)
          else
            call shl3ds(d,ul,xl,ix,s,p,ndf,ndm,nst,isw)
          endif

!       Finite deformation

        else
          write(*,*) '  *ERROR* No finite deformation shell'
        endif

      endif

!     Format

2000  format(5x,'T h r e e   D i m e n s i o n a l   S h e l l',
     &          '   E l e m e n t')

      end subroutine shell3d

      subroutine shl3di(d,ul,xl,ix,s,r,ndf,ndm,nst,isw)

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:  Quadrilateral shell element for feap
!                incorporating membrane with normal drilling dof
!                modified to remove effects of constant strains and
!                including deep shell curvature corrections to the
!                discrete kirchhoff quadrilateral plate bending element
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!.... Input parameters set as follows:

!         ndm = 3 (x,y,z cartesian coordinates at nodes)
!         ndf = 6 (u-x,u-y,u-z,r-x,r-y,r-z at nodes)
!         nen = 4 nodes (counterclockwise around element)

!       Note: 1-direction bisects diagonals between 2-3 element and
!             2-direction bisects diagonals between 3-4 element nodes.
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      implicit  none

      include  'bdata.h'
      include  'cdata.h'
      include  'eldata.h'
      include  'elplot.h'
      include  'eltran.h'
      include  'evdata.h'
      include  'iofile.h'
      include  'prld1.h'
      include  'prstrs.h'
      include  'shlc16.h'
      include  'shld16.h'
      include  'shpf16.h'
      include  'sstr16.h'
      include  'comblk.h'

      integer       :: ndm,ndf,nst,isw
      integer       :: i,j,k,l,lint,ll,lt,ialph
      integer       :: i1, j1, ii,jj, nn

      real (kind=8) :: a11,a13, a23,a33, xx,yy,zz, hh
      real (kind=8) :: dv, dv1,dv2, pen,ggi,ggv,tc, xsj, thk
      real (kind=8) :: xyn, x1n,x2n,y1n,y2n, ctan1,ctan3
      real (kind=8) :: shp1i,shp2i,shp3i
      real (kind=8) :: shp13, shp23

      integer       :: ix(*)

      real (kind=8) :: d(*),xl(ndm,*),ul(ndf,nen,*),s(nst,*),r(ndf,*)
      real (kind=8) :: dvl(9),eps(6),sig(6)
      real (kind=8) :: yl(3,4),vl(6,4,3),tr(3,3),bl(3),bg(3)
      real (kind=8) :: gshp1(3,4),gshp2(3,4),gg(6,4),dd(6,6,5)
      real (kind=8) :: bmat(4,6,4), dbmat(6,4)

      save

      data      ialph /2/

!     Transfer to correct processor

      go to (1,2,3,3,3,3,1,3), isw

1     return

!     Check element for errors

2     call tran3d(xl,yl,tr,ndm)
      call ckisop(ix,yl,shp,3)
      return

!     Compute element tangent array

!     Compute transformation and midsurface coords

3     call tran3d(xl,yl,tr,ndm)

!     Compute local coordinates

      do i = 1,4 ! {
        do j = 1,3 ! {
          vl(j  ,i,1) = 0.0d0
          vl(j+3,i,1) = 0.0d0
          do k = 1,3
            vl(j  ,i,1) = vl(j  ,i,1) + tr(j,k)*ul(k  ,i,1)
            vl(j+3,i,1) = vl(j+3,i,1) + tr(j,k)*ul(k+3,i,1)
          end do ! k  }
        end do ! j  }
      end do ! i  }

!     Get quadratrure data

      l   = nint(d(5))
      lt  = nint(d(102))
      call int2d (l ,lint,sg)
      call int1dl(lt,sgt)


!     Test for triangular element

      if( ix(1) .eq. ix(2)  .or. ix(2) .eq. ix(3) .or.
     &    ix(3) .eq. ix(4)  .or. ix(4) .eq. ix(1) ) then

        qdflg = .false.
        call pzero( bm, 162)
        call pzero(shp1,12*lint)
        call pzero(shp2,12*lint)
        shp13 = 0.0d0
        shp23 = 0.0d0
        a13   = 0.0d0
        a23   = 0.0d0
        a33   = 0.0d0
        x1n   = 0.0d0
        y1n   = 0.0d0
        x2n   = 0.0d0
        y2n   = 0.0d0
        xyn   = 0.0d0
        call jtri3d(yl,xsjt)
      else
        qdflg = .true.
        pen   = d(60)
        call jacq3d(yl)
      endif

      if(yl(3,1).eq.0.0d0) then
        ii1 = 3
        ii2 = 5
      else
        ii1 = 1
        ii2 = 6
      endif

!     Construct integrals of drilling shape functions

      call pzero(gshp1,12)
      call pzero(gshp2,12)
      call pzero(gg   ,24)

      dv = 0.0d0
      do l = 1,lint ! {

!       Form shape functions and their integrals

        if(qdflg) then
          call rshp3d(sg(1,l),yl,shp(1,1,l),shp1(1,1,l),
     &                shp2(1,1,l),xsj,3)
          dvl(l) = xsj*sg(3,l)
          do j = 1,4 ! {
            do i = 1,3 ! {
              gshp1(i,j) = gshp1(i,j) + shp1(i,j,l)*dvl(l)
              gshp2(i,j) = gshp2(i,j) + shp2(i,j,l)*dvl(l)
            end do ! i  }
          end do ! j  }
        else
          call shp2d (sg(1,l),yl,shp(1,1,l),xsj,3,nel,ix,.false.)
          dvl(l) = xsj*sg(3,l)
        endif
        dv     = dv + dvl(l)
      end do ! l  }

      if(isw.eq.5) go to 5

!     Compute thickness for element

      thk  = d(14)

      if(isw.eq.4) go to 4

!     Construct modified drilling shape functions

      if(qdflg) then
        do j = 1,4 ! {
          do i = 1,3 ! {
            dv1 = gshp1(i,j)/dv
            dv2 = gshp2(i,j)/dv
            do l = 1,lint ! {
              shp1(i,j,l) = shp1(i,j,l) - dv1
              shp2(i,j,l) = shp2(i,j,l) - dv2
            end do ! l  }
          end do ! j  }
        end do ! i  }
      endif

      if(isw.eq.8) go to 8

!     Compute element load vectors

      bl(1) = 0.0d0
      bl(2) = 0.0d0

!     Set body loading factors

      if(int(d(74)).gt.0) then
        bg(1) = d(11) + prldv(int(d(74)))*d(71)
      else
        bg(1) = d(11)*dm
      endif

      if(int(d(75)).gt.0) then
        bg(2) = d(12) + prldv(int(d(75)))*d(72)
      else
        bg(2) = d(12)*dm
      endif

      if(int(d(76)).gt.0) then
        bl(3) = d( 8)
        bg(3) = d(13) + prldv(int(d(76)))*d(73)
      else
        bl(3) = d( 8)*dm
        bg(3) = d(13)*dm
      endif

!     Multiply body loads by thickness

      do i = 1,3 ! {
        bg(i) = bg(i)*thk
      end do ! i  }

!     Transform body loads to local system

      do i = 1,3 ! {
        do j = 1,3 ! {
          bl(i) = bl(i) + tr(i,j)*bg(j)
        end do ! j  }
      end do ! i  }

      tc = 1.0d0

!     Transform displacements

      do i = 1,4
        vl(1,i,1) = vl(1,i,1) - yl(3,i)*vl(5,i,1)
        vl(2,i,1) = vl(2,i,1) + yl(3,i)*vl(4,i,1)
      end do

!     Transient factors

      ctan1 = ctan(1) + d(78)*ctan(2)
      ctan3 = ctan(3) + d(77)*ctan(2)

!     Compute local velocity and acceleration

      do i = 1,4 ! {
        do j = 1,3 ! {
          vl(j  ,i,2) = 0.0d0
          vl(j+3,i,2) = 0.0d0
          vl(j  ,i,3) = 0.0d0
          vl(j+3,i,3) = 0.0d0
          do k = 1,3
            vl(j  ,i,2) = vl(j  ,i,2) + tr(j,k)*ul(k  ,i,4)
            vl(j+3,i,2) = vl(j+3,i,2) + tr(j,k)*ul(k+3,i,4)
            vl(j  ,i,3) = vl(j  ,i,3) + tr(j,k)*ul(k  ,i,5)
            vl(j+3,i,3) = vl(j+3,i,3) + tr(j,k)*ul(k+3,i,5)
          end do ! k  }
        end do ! j  }
      end do ! i  }

!     Compute parts

      nn = 0
      do l = 1,lint ! {

!       Compute loading term

        do i = 1,4
          shp3i  = shp(3,i,l)
          if(qdflg) then
            shp13       =  shp1(3,i,l)
            shp23       =  shp2(3,i,l)
          end if

          r(1,i) = r(1,i) + shp3i*bl(1)*dvl(l)
          r(2,i) = r(2,i) + shp3i*bl(2)*dvl(l)
          r(3,i) = r(3,i) + shp3i*bl(3)*dvl(l)
          r(4,i) = r(4,i) + shp13*bl(3)*dvl(l)*tc
          r(5,i) = r(5,i) + shp23*bl(3)*dvl(l)*tc
          r(6,i) = r(6,i) -(shp13*bl(1) + shp23*bl(2))*dvl(l)*tc
        end do ! i

!       Membrane and bending stiffness part

        dv1 = 0.5d0*thk*dvl(l)*ctan1

        do ll = 1,lt

          hh = 0.5d0*thk*sgt(1,ll)
          call str3di(d,yl,vl,3,nel,l, xx,yy,zz,hh,eps,sig,dd,nn,isw)
          nn = nn + nint(d(15))

!         Store time history plot data for element

          i = 8*(l-1)
          do j = 1,4
            tt(j+i  ) = sig(j)
            tt(j+i+3) = eps(j)
          end do ! j

!         Multiply stress and tangent by volume and quadratrue weight

          dv2  = dv1*sgt(2,ll)
          do i = 1,4
            sig(i) = sig(i)*dv2
            do j = 1,4
              dd(j,i,1) = dd(j,i,1)*dv2
            end do ! j
          end do ! i

!         Recover previously computed shape functions

          do i = 1,4 ! {
            shp1i       =  shp(1,i,l)
            shp2i       =  shp(2,i,l)

            bmat(1,1,i) =  bm(1,1,i)*hh + shp1i
            bmat(2,1,i) =  bm(2,1,i)*hh
            bmat(4,1,i) =  bm(3,1,i)*hh + shp2i
            bmat(1,2,i) =  bm(1,2,i)*hh
            bmat(2,2,i) =  bm(2,2,i)*hh + shp2i
            bmat(4,2,i) =  bm(3,2,i)*hh + shp1i
            bmat(1,3,i) =  bm(1,3,i)*hh
            bmat(2,3,i) =  bm(2,3,i)*hh
            bmat(4,3,i) =  bm(3,3,i)*hh
            bmat(1,4,i) =  bm(1,4,i)*hh
            bmat(2,4,i) =  bm(2,4,i)*hh
            bmat(4,4,i) =  bm(3,4,i)*hh
            bmat(1,5,i) =  bm(1,5,i)*hh
            bmat(2,5,i) =  bm(2,5,i)*hh
            bmat(4,5,i) =  bm(3,5,i)*hh

            if(qdflg) then
              bmat(1,6,i) = bm(1,6,i) - shp1(1,i,l)
              bmat(2,6,i) = bm(2,6,i) - shp2(2,i,l)
              bmat(4,6,i) = bm(3,6,i) - shp1(2,i,l) - shp2(1,i,l)
            endif

!           Compute stress divergence terms

            do ii = 1,6
              r(ii,i) = r(ii,i) - bmat(1,ii,i)*sig(1)
     &                          - bmat(2,ii,i)*sig(2)
     &                          - bmat(4,ii,i)*sig(4)
            end do ! ii
          end do ! i

!         Form stiffness

          i1 = 0
          do i = 1,4 ! {

!           Form stress-displacement matrix (Bi-trans * D)

            do ii = 1,6
              dbmat(ii,1) = bmat(1,ii,i)*dd(1,1,1)
     &                    + bmat(2,ii,i)*dd(2,1,1)
     &                    + bmat(4,ii,i)*dd(4,1,1)

              dbmat(ii,2) = bmat(1,ii,i)*dd(1,2,1)
     &                    + bmat(2,ii,i)*dd(2,2,1)
     &                    + bmat(4,ii,i)*dd(4,2,1)

              dbmat(ii,4) = bmat(1,ii,i)*dd(1,4,1)
     &                    + bmat(2,ii,i)*dd(2,4,1)
     &                    + bmat(4,ii,i)*dd(4,4,1)

            end do ! ii

!           Loop on columns

            j1 = i1
            do j = i,4 ! {

!             Compute stiffness contribution

              do jj = 1,6 ! {
                do ii = 1,6 ! {
                  s(ii+i1,jj+j1) = s(ii+i1,jj+j1)
     +                           + dbmat(ii,1)*bmat(1,jj,j)
     +                           + dbmat(ii,2)*bmat(2,jj,j)
     +                           + dbmat(ii,4)*bmat(4,jj,j)
                end do ! ii  }
              end do ! jj  }
              j1 = j1 + ndf
            end do ! j  }
            i1 = i1 + ndf
          end do ! i }

        end do ! ll

!       Compute Hughes/Brezzi rotation matrix

        if(pen.gt.0.0d0 .and. qdflg) then
          do j = 1,4 ! {
            gg(1,j) = gg(1,j) - shp(2,j,l)*dvl(l)
            gg(2,j) = gg(2,j) + shp(1,j,l)*dvl(l)
            gg(6,j) = gg(6,j) - 2.d0*shp(3,j,l)*dvl(l)
            if(ialph.gt.1) then
              gg(6,j) = gg(6,j) + (shp1(2,j,l) - shp2(1,j,l))*dvl(l)
            endif
          end do ! j  }
        endif
      end do ! l  }

!     Perform rank one update for Huges/Brezzi term

      if(pen.gt.0.0d0 .and. qdflg) then

!       Compute H/B strain

        xx = 0.0d0
        do i = 1,4 ! {
          do j = 1,6 ! {
            xx = xx + gg(j,i)*vl(j,i,1)
          end do ! j  }
        end do ! i  }

!       Compute H/B residual and tangent

        ggv = pen*d(27)*thk*ctan(1)/dv

        do i = 1,4
          ggi = ggv*xx
          do j = 1,6
            r(j,i) = r(j,i) - ggi*gg(j,i)
          end do ! j
        end do ! i

        do i = 1,nst ! {
          ggi = ggv*gg(i,1)
          do j = i,nst ! {
            s(i,j) = s(i,j) + ggi*gg(j,1)
          end do ! j  }
        end do ! i  }
      endif

!     Correct residual and tangent matrix for element warpage

      i1 = 1
      if(yl(3,1).ne.0.0d0) then
        do i = 1,4 ! {
          r(4,i) = r(4,i) + yl(3,i)*r(2,i)
          r(5,i) = r(5,i) - yl(3,i)*r(1,i)
          j1 = i1
          do j = i,4 ! {
            call proj3d(s(i1,j1),yl(3,i),yl(3,j),nst)
            j1 = j1 + ndf
          end do ! j  }
          i1 = i1 + ndf
        end do ! i  }
      endif

!     Rotate to global frame

      call rots3d(s,r,tr,nst,ndf)

      return

!     Compute and output element variables

4     nn = 0
      do l = 1,lint ! {

!       Form shape functions

        call rshp3d(sg(1,l),yl,shp,shp1,shp2,xsj,3)

!       Modify rotational shape functions

        do j = 1,4
          do i = 1,3
            shp1(i,j,1) = shp1(i,j,1) - gshp1(i,j)/dv
            shp2(i,j,1) = shp2(i,j,1) - gshp2(i,j)/dv
          end do ! i  }
        end do ! j  }

        do ll = 1,lt
          hh  = 0.5d0*thk*sgt(1,ll)
          call str3di(d,xl,vl,ndm,nel,1, xx,yy,zz,hh,eps,sig,dd,nn,isw)
          nn  = nn + nint(d(15))
          mct = mct - 3
          if(mct.le.0) then
            write(iow,2002) o,head
            if(ior.lt.0) then
            write(*,2002) o,head
            endif
            mct = 50
          end if
          xx = xx + tr(1,3)*hh
          yy = yy + tr(2,3)*hh
          zz = zz + tr(3,3)*hh
          write(iow,2003) n_el,xx,sig,ma,yy,eps,zz
          if(ior.lt.0) then
            write(*,2003) n_el,xx,sig,ma,yy,eps,zz
          endif
        end do ! ll
      end do ! l  }
      return

!     Compute element mass or geometric stifness arrays

5     if(imtyp.eq.1) then

!       Compute mass

        do l = 1,lint ! {
          dv1 = dvl(l)*d(4)*d(14)
          i1  = 0
          do j = 1,4 ! {
            do i = 1,3 ! {
              r(i,j)       = r(i,j) + shp(3,j,l)*dv1
              s(i1+i,i1+i) = r(i,j)
            end do ! i }
            i1 = i1 + ndf
          end do ! j }
        end do ! l }

      elseif(imtyp.eq.2) then

!       Compute geometric stiffness

        do i = 1,4 ! {
          do j = 1,3 ! {
            vl(j  ,i,1) = 0.0d0
            vl(j+3,i,1) = 0.0d0
            do k = 1,3 ! {
              vl(j  ,i,1) = vl(j  ,i,1) + tr(j,k)*ul(k  ,i,1)
              vl(j+3,i,1) = vl(j+3,i,1) + tr(j,k)*ul(k+3,i,1)
            end do ! k  }
          end do ! j  }
        end do ! i  }

        nn = 0
        do l = 1,lint ! {

!         Modify rotational shape functions

          do j = 1,4 ! {
            do i = 1,3 ! {
              shp1(i,j,l) = shp1(i,j,l) - gshp1(i,j)/dv
              shp2(i,j,l) = shp2(i,j,l) - gshp2(i,j)/dv
            end do ! i  }
          end do ! j  }

          hh = 0.0d0
          call str3di(d,xl,vl,ndm,nel,l, xx,yy,zz,hh,eps,sig,dd,nn,isw)
          nn = nn + nint(d(15))

          do i = 1,4
            sig(i) = sig(i)*thk
          end do ! i

          i1 = 0
          do i = 1,4 !{
            j1 = 0
            dv1 = (shp(1,i,l)*sig(1) + shp(2,i,l)*sig(4))*dvl(l)
            dv2 = (shp(1,i,l)*sig(4) + shp(2,i,l)*sig(2))*dvl(l)
            do j = 1,4 !{
              a11 = dv1*shp(1,j,l) + dv2*shp(2,j,l)
              do k = 1,3 !{
                s(i1+k,j1+k) = s(i1+k,j1+k) - a11
              end do ! k  }
              j1 = j1 + ndf
            end do ! j  }
            i1 = i1 + ndf
          end do ! i  }

        end do ! l  }

        i1 = 1
        if(yl(3,1).ne.0.0d0) then
          do i = 1,4 !{
            j1 = i1
            do j = i,4 !{
              call proj3d(s(i1,j1),yl(3,i),yl(3,j),nst)
              j1 = j1 + ndf
            end do ! j  }
            i1 = i1 + ndf
          end do ! i  }
        endif
        call rots3d(s,r,tr,nst,ndf)
      endif

      return

!     Compute nodal output quantities

8     call stcn3si(ix,d,yl,ul,tr,hr(nph),hr(nph+numnp),dvl,thk,
     &             ndf,nel,numnp,isw)
      return

!     Formats

2002  format(a1,20a4//5x,'S h e l l   S t r e s s e s'//
     & ' elmt x-coord  xx-stress  yy-stress  zz-stress  xy-stress',
     & ' matl y-coord  xx-strain  yy-strain  zz-strain  xy-strain',
     & '      z-coord'/38(' -'))

2003  format(/i5,0p,1f8.3,1p,5e11.3,0p,1f8.2/i5,0p,1f8.3,1p,5e11.3,
     &       0p,1f8.2/5x,0p,1f8.3,1p,6e11.3/(13x,1p,6e11.3))

      end subroutine shl3di

      subroutine stcn3si(ix,d,yl,ul,tr,dt,st,dvl,thk,
     &                   ndf,nel,numnp,isw)

      implicit  none

      include  'shpf16.h'
      include  'shlc16.h'
      include  'sstr16.h'

      integer       :: ndf,nel,numnp, i,j,l, ii, ll,lt, nn,isw
      real (kind=8) :: xsji, xx,yy,zz, hh, dh, thk

      integer       :: ix(*)

      real (kind=8) :: dt(numnp),st(numnp,*),yl(3,*),tr(3,3),d(*)
      real (kind=8) :: vl(6,4),dvl(4),ul(ndf,*)
      real (kind=8) :: eps(6),sig(6),momt(6),norm(6),dd(6,6,5),eps0(6)

      save

!     Compute membrane and bending stresses for projection.

      do i = 1,4 ! {
        do j = 1,3 ! {
          vl(j  ,i) = 0.0d0
          vl(j+3,i) = 0.0d0
          do l = 1,3 ! {
            vl(j  ,i) = vl(j  ,i) + tr(j,l)*ul(l  ,i)
            vl(j+3,i) = vl(j+3,i) + tr(j,l)*ul(l+3,i)
          end do ! l  }
        end do ! j  }
      end do ! i  }

      lt = nint(d(102))
      nn = 0
      do l = 1,4 ! {
        do j = 1,6
          norm(j) = 0.0d0
          momt(j) = 0.0d0
        end do ! j
        do ll = 1,lt
          hh = 0.5d0*thk*sgt(1,ll)
          dh = 0.5d0*thk*sgt(2,ll)
          call str3di(d,yl,vl,3,nel,l, xx,yy,zz,hh,eps,sig,dd,nn,isw)
          norm(1) = norm(1) + sig(1)*dh
          norm(2) = norm(2) + sig(2)*dh
          norm(3) = norm(3) + sig(4)*dh
          momt(1) = momt(1) + sig(1)*hh*dh
          momt(2) = momt(2) + sig(2)*hh*dh
          momt(3) = momt(3) + sig(4)*hh*dh

          if(ll.eq.1) then
            do j = 1,4 ! {
              xsji = dvl(l)*shp(3,j,l)
              ii = ix(j)
              if(ii.gt.0) then
                st(ii,20) = st(ii,20) + sig(1)*xsji
                st(ii,21) = st(ii,21) + sig(2)*xsji
                st(ii,22) = st(ii,22) + sig(4)*xsji
              endif
            end do ! j
            do j = 1,3
              eps0(j) = eps(j)
            end do

          elseif(ll.eq.lt) then

            do j = 1,4 ! {
              xsji = dvl(l)*shp(3,j,l)
              ii = ix(j)
              if(ii.gt.0) then
                st(ii,17) = st(ii,17) + sig(1)*xsji
                st(ii,18) = st(ii,18) + sig(2)*xsji
                st(ii,19) = st(ii,19) + sig(4)*xsji
              endif
            end do ! j
            do j = 1,3
              eps0(j+3) = (eps(j) - eps0(j))/thk
              eps0(j  ) = (eps(j) + eps0(j))*0.5d0
            end do

          endif

          nn = nn + nint(d(15))
        end do ! ll
        do j = 1,4 ! {
          xsji = dvl(l)*shp(3,j,l)
          ii = ix(j)
          if(ii.gt.0) then
            dt(ii)    = dt(ii)    + xsji

            st(ii,1)  = st(ii,1)  + norm(1)*xsji
            st(ii,2)  = st(ii,2)  + norm(2)*xsji
            st(ii,3)  = st(ii,3)  + norm(3)*xsji
            st(ii,4)  = st(ii,4)  + norm(4)*xsji
            st(ii,5)  = st(ii,5)  + norm(5)*xsji

            st(ii,6)  = st(ii,6)  + momt(1)*xsji
            st(ii,7)  = st(ii,7)  + momt(2)*xsji
            st(ii,8)  = st(ii,8)  + momt(3)*xsji
            st(ii,9)  = st(ii,9)  + momt(4)*xsji
            st(ii,10) = st(ii,10) + momt(5)*xsji

            st(ii,11) = st(ii,11) + eps0(1)*xsji
            st(ii,12) = st(ii,12) + eps0(2)*xsji
            st(ii,13) = st(ii,13) + eps0(3)*xsji
            st(ii,14) = st(ii,14) + eps0(4)*xsji
            st(ii,15) = st(ii,15) + eps0(5)*xsji
            st(ii,16) = st(ii,16) + eps0(6)*xsji

          end if
        end do ! j  }
      end do ! l  }

      end subroutine stcn3si

      subroutine str3di(d,xl,vl,ndm,nel,l, xx,yy,zz,hh, eps,sig,dd,
     &                  nn,isw)

      implicit  none

      include  'hdata.h'
      include  'shlc16.h'
      include  'shld16.h'
      include  'shpf16.h'
      include  'sstr16.h'

      include  'comblk.h'

      integer       :: ndm,nel,l, i,j, ii, nn,isw, nhv, istrt

      real (kind=8) :: xx,yy,zz, hh, ta

      real (kind=8) :: d(*), xl(ndm,*),vl(6,*), eps0(6),eps(6),sig(6)
      real (kind=8) :: dd(6,6,5)

      save

!     Point processed

      ii = 0

!     Compute membrane and bending strains

      if(qdflg) then
        call dktq3d(shp(1,5,l),shp(1,1,l))
      else
        call dktb3d(sg(1,l),xsjt)
      endif

      do i = 1,6 ! {
        eps0(i) = 0.0d0
        eps (i) = 0.0d0
      end do ! i  }
      xx = 0.0d0
      yy = 0.0d0
      zz = 0.0d0
      do j = 1,nel ! {
        xx = xx + shp(3,j,l)*xl(1,j)
        yy = yy + shp(3,j,l)*xl(2,j)
        zz = zz + shp(3,j,l)*xl(3,j)
        eps0(1) = eps0(1) + shp(1,j,l)*vl(1,j)
     &                    - shp1(1,j,l)*vl(6,j)
        eps0(2) = eps0(2) + shp(2,j,l)*vl(2,j)
     &                    - shp2(2,j,l)*vl(6,j)
        eps0(3) = eps0(3) + shp(1,j,l)*vl(2,j) + shp(2,j,l)*vl(1,j)
     &                    - (shp1(2,j,l) + shp2(1,j,l))*vl(6,j)
        do i = ii1,ii2 ! {
          eps0(4) = eps0(4) + bm(1,i,j)*vl(i,j)
          eps0(5) = eps0(5) + bm(2,i,j)*vl(i,j)
          eps0(6) = eps0(6) + bm(3,i,j)*vl(i,j)
        end do ! i  }
      end do ! j  }

!     Compute strain at current elevation

      eps(1) = eps0(1) + hh*eps0(4)
      eps(2) = eps0(2) + hh*eps0(5)
      eps(4) = eps0(3) + hh*eps0(6)

      ta     = 0.0d0

!     Compute stress at point

      nhv   = nint(d(15))
      istrt = nint(d(84))
      call modlsd(ii,d,ta,eps,hr(nh1+nn),hr(nh2+nn),nhv,istrt,dd,sig,
     &            isw)

      end subroutine str3di

      subroutine shl3ds(d,ul,xl,ix,s,r,ndf,ndm,nst,isw)

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:  Quadrilateral shell element for feap
!                incorporating membrane with normal drilling dof
!                modified to remove effects of constant strains and
!                including deep shell curvature corrections to the
!                discrete kirchhoff quadrilateral plate bending element
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!.... Input parameters set as follows:

!         ndm = 3 (x,y,z cartesian coordinates at nodes)
!         ndf = 6 (u-x,u-y,u-z,r-x,r-y,r-z at nodes)
!         nen = 4 nodes (counterclockwise around element)

!       Note: 1-direction bisects diagonals between 2-3 element and
!             2-direction bisects diagonals between 3-4 element nodes.
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      implicit  none

      include  'bdata.h'
      include  'cdata.h'
      include  'eldata.h'
      include  'elplot.h'
      include  'eltran.h'
      include  'evdata.h'
      include  'iofile.h'
      include  'prld1.h'
      include  'prstrs.h'
      include  'shlc16.h'
      include  'shld16.h'
      include  'shpf16.h'
      include  'sstr16.h'
      include  'comblk.h'

      integer       :: ndm,ndf,nst,isw
      integer       :: i,j,k,l,lint,ialph, i1, j1, ii,jj,kk

      real (kind=8) :: a11,a12,a13, a21,a22,a23, a31,a32,a33, xx,yy,zz
      real (kind=8) :: dv, dv1,dv2,dvm, pen,ggi,ggv,tc, xsj, thk,thk3
      real (kind=8) :: xyn, x1n,x2n,y1n,y2n, xn,yn, ctan1,ctan3
      real (kind=8) :: shp1i,shp2i,shp3i
      real (kind=8) :: shp11,shp12,shp13, shp21,shp22,shp23

      integer       :: ix(*)

      real (kind=8) :: d(*),xl(ndm,*),ul(ndf,nen,*),s(nst,*),r(ndf,*)
      real (kind=8) :: norm(6),dvl(9),momt(6),eps(6)
      real (kind=8) :: yl(3,4),vl(6,4,3),tr(3,3),bl(3),bg(3)
      real (kind=8) :: btd(3,6),gshp1(3,4),gshp2(3,4),gg(24),dd(6,6)
!     real (kind=8) :: b0(4,2,4),bf(4,3,4), ddb0(4,2), ddbf(4,3)

      save

      data      ialph /2/

!     Transfer to correct processor

      go to (1,2,3,3,3,3,1,3), isw

1     return

!     Check element for errors

2     call tran3d(xl,yl,tr,ndm)
      call ckisop(ix,yl,shp,3)
      return

!     Compute element tangent array

!     Compute transformation and midsurface coords

3     call tran3d(xl,yl,tr,ndm)

!     Compute local coordinates

      do i = 1,4 ! {
        do j = 1,3 ! {
          vl(j  ,i,1) = 0.0d0
          vl(j+3,i,1) = 0.0d0
          do k = 1,3
            vl(j  ,i,1) = vl(j  ,i,1) + tr(j,k)*ul(k  ,i,1)
            vl(j+3,i,1) = vl(j+3,i,1) + tr(j,k)*ul(k+3,i,1)
          end do ! k  }
        end do ! j  }
      end do ! i  }

!     Get quadratrure data

      l = nint(d(5))
      if(l*l.ne.lint) call int2d(l ,lint,sg)

!     Test for triangular element

      if( ix(1) .eq. ix(2)  .or. ix(2) .eq. ix(3) .or.
     &    ix(3) .eq. ix(4)  .or. ix(4) .eq. ix(1) ) then

        qdflg = .false.
        call pzero(  bm, 162)
        shp11 = 0.0d0
        shp12 = 0.0d0
        shp13 = 0.0d0
        shp21 = 0.0d0
        shp22 = 0.0d0
        shp23 = 0.0d0
        a13   = 0.0d0
        a23   = 0.0d0
        a33   = 0.0d0
        x1n   = 0.0d0
        y1n   = 0.0d0
        x2n   = 0.0d0
        y2n   = 0.0d0
        xyn   = 0.0d0
        call jtri3d(yl,xsjt)
      else
        qdflg = .true.
        pen   = d(60)
        call jacq3d(yl)
      endif

!     if(qdflg .and. yl(3,1).eq.0.0d0) then
      if(yl(3,1).eq.0.0d0) then
        ii1 = 3
        ii2 = 5
      else
        ii1 = 1
        ii2 = 6
      endif

!     Construct integrals of drilling shape functions

      call pzero(gshp1,12)
      call pzero(gshp2,12)
      call pzero(gg   ,24)

      dv = 0.0d0
      do l = 1,lint ! {

!       Form shape functions and their integrals

        if(qdflg) then
          call rshp3d(sg(1,l),yl,shp(1,1,l),shp1(1,1,l),
     &                shp2(1,1,l),xsj,3)
          dvl(l) = xsj*sg(3,l)
          do j = 1,4 ! {
            do i = 1,3 ! {
              gshp1(i,j) = gshp1(i,j) + shp1(i,j,l)*dvl(l)
              gshp2(i,j) = gshp2(i,j) + shp2(i,j,l)*dvl(l)
            end do ! i  }
          end do ! j  }
        else
          call shp2d (sg(1,l),yl,shp(1,1,l),xsj,3,nel,ix,.false.)
          dvl(l) = xsj*sg(3,l)
        endif
        dv     = dv + dvl(l)
      end do ! l  }

      if(isw.eq.5) go to 5

!     Compute thickness for element

      thk  = d(14)
      thk3 = thk**3/12.d0

      if(isw.eq.4) go to 4

!     Construct modified drilling shape functions

      if(qdflg) then
        do j = 1,4 ! {
          do i = 1,3 ! {
            dv1 = gshp1(i,j)/dv
            dv2 = gshp2(i,j)/dv
            do l = 1,lint ! {
              shp1(i,j,l) = shp1(i,j,l) - dv1
              shp2(i,j,l) = shp2(i,j,l) - dv2
            end do ! l  }
          end do ! i  }
        end do ! j  }
      endif

      if(isw.eq.8) go to 8

!     Compute element load vectors

      bl(1) = 0.0d0
      bl(2) = 0.0d0

!     Set body loading factors

      if(int(d(74)).gt.0) then
        bg(1) = d(11) + prldv(int(d(74)))*d(71)
      else
        bg(1) = d(11)*dm
      endif

      if(int(d(75)).gt.0) then
        bg(2) = d(12) + prldv(int(d(75)))*d(72)
      else
        bg(2) = d(12)*dm
      endif

      if(int(d(76)).gt.0) then
        bl(3) = d( 8)
        bg(3) = d(13) + prldv(int(d(76)))*d(73)
      else
        bl(3) = d( 8)*dm
        bg(3) = d(13)*dm
      endif

      do i = 1,3 ! {
        do j = 1,3 ! {
          bl(i) = bl(i) + tr(i,j)*bg(j)
        end do ! j  }
      end do ! i  }

      tc = 1.0d0

!     Transform displacements

      do i = 1,4
        vl(1,i,1) = vl(1,i,1) - yl(3,i)*vl(5,i,1)
        vl(2,i,1) = vl(2,i,1) + yl(3,i)*vl(4,i,1)
      end do

!     Transient factors

      ctan1 = ctan(1) + d(78)*ctan(2)
      ctan3 = ctan(3) + d(77)*ctan(2)

!     Compute local velocity and acceleration

      do i = 1,4 ! {
        do j = 1,3 ! {
          vl(j  ,i,2) = 0.0d0
          vl(j+3,i,2) = 0.0d0
          vl(j  ,i,3) = 0.0d0
          vl(j+3,i,3) = 0.0d0
          do k = 1,3
            vl(j  ,i,2) = vl(j  ,i,2) + tr(j,k)*ul(k  ,i,4)
            vl(j+3,i,2) = vl(j+3,i,2) + tr(j,k)*ul(k+3,i,4)
            vl(j  ,i,3) = vl(j  ,i,3) + tr(j,k)*ul(k  ,i,5)
            vl(j+3,i,3) = vl(j+3,i,3) + tr(j,k)*ul(k+3,i,5)
          end do ! k  }
        end do ! j  }
      end do ! i  }

!     Compute membrane/load parts

      do l = 1,lint ! {

!       Membrane and bending stiffness part

        dv1 = thk *dvl(l)*ctan1
        dv2 = thk3*dvl(l)*ctan1

        call stre3d(d,yl,vl,3,nel,l, xx,yy,zz,eps,norm,momt,dd)

!       Store time history plot data for element

        i = 6*(l-1)
        do j = 1,3
          tt(j+i  ) = norm(j)
          tt(j+i+3) = momt(j)
        end do ! j

!       Add Rayleigh damping stress terms

        if(d(78).ne.0.0d0) then
          call rayl3d(d,vl(1,1,2),nel,l, norm,momt,dd)
        endif

!       Recover previously computed shape functions

        i1 = 0
        do i = 1,4 ! {
          shp1i = shp(1,i,l)
          shp2i = shp(2,i,l)
          shp3i = shp(3,i,l)
          if(qdflg) then
            shp11 = shp1(1,i,l)
            shp12 = shp1(2,i,l)
            shp13 = shp1(3,i,l)
            shp21 = shp2(1,i,l)
            shp22 = shp2(2,i,l)
            shp23 = shp2(3,i,l)
            a13   = -(dd(1,1)*shp11 + dd(1,2)*shp22
     &              + dd(1,4)*(shp12+shp21))*dv1
            a23   = -(dd(2,1)*shp11 + dd(2,2)*shp22
     &              + dd(2,4)*(shp12+shp21))*dv1
            a33   = -(dd(4,1)*shp11 + dd(4,2)*shp22
     &              + dd(4,4)*(shp12+shp21))*dv1
          endif

!         Compute loading term

          r(1,i) = r(1,i) + shp3i*bl(1)*dvl(l)
          r(2,i) = r(2,i) + shp3i*bl(2)*dvl(l)
          r(3,i) = r(3,i) + shp3i*bl(3)*dvl(l)
          r(4,i) = r(4,i) + shp13*bl(3)*dvl(l)*tc
          r(5,i) = r(5,i) + shp23*bl(3)*dvl(l)*tc
          r(6,i) = r(6,i) -(shp13*bl(1) + shp23*bl(2))*dvl(l)*tc

!         Compute stress divergence terms

          r(1,i) = r(1,i) - (shp1i*norm(1) + shp2i*norm(3))*dvl(l)
          r(2,i) = r(2,i) - (shp2i*norm(2) + shp1i*norm(3))*dvl(l)
          r(6,i) = r(6,i) + (shp11*norm(1) + shp22*norm(2)
     &                    + (shp12 + shp21)*norm(3))*dvl(l)

          do ii = ii1,ii2 ! {
            r(ii,i) = r(ii,i) - (bm(1,ii,i)*momt(1)
     &                        +  bm(2,ii,i)*momt(2)
     &                        +  bm(3,ii,i)*momt(3))*dvl(l)
          end do ! ii  }

!         Form stress-displacement matrix (Bi-trans * D)

          a11 =  (dd(1,1)*shp1i + dd(1,4)*shp2i)*dv1
          a12 =  (dd(1,2)*shp2i + dd(1,4)*shp1i)*dv1
          a21 =  (dd(2,1)*shp1i + dd(2,4)*shp2i)*dv1
          a22 =  (dd(2,2)*shp2i + dd(2,4)*shp1i)*dv1
          a31 =  (dd(4,1)*shp1i + dd(4,4)*shp2i)*dv1
          a32 =  (dd(4,2)*shp2i + dd(4,4)*shp1i)*dv1

!         Form plate stress-displacement matrix

          do ii = ii1,ii2 ! {
            btd(1,ii) = (dd(1,1)*bm(1,ii,i)
     &                +  dd(1,2)*bm(2,ii,i)
     &                +  dd(1,4)*bm(3,ii,i))*dv2
            btd(2,ii) = (dd(2,1)*bm(1,ii,i)
     &                +  dd(2,2)*bm(2,ii,i)
     &                +  dd(2,4)*bm(3,ii,i))*dv2
            btd(3,ii) = (dd(4,1)*bm(1,ii,i)
     &                +  dd(4,2)*bm(2,ii,i)
     &                +  dd(4,4)*bm(3,ii,i))*dv2
          end do ! ii  }

!         Inertial parts

          dvm = dvl(l)*d(4)*d(14)*shp3i
          do ii = 1,3 ! {
            r(ii,i)        = r(ii,i)        - dvm*( vl(ii,i,3)
     &                                      + d(77)*vl(ii,i,2))
            s(i1+ii,i1+ii) = s(i1+ii,i1+ii) + dvm*ctan3
          end do ! i }

!         Loop on columns

          j1 = i1
          do j = i,4 ! {
            xn = shp(1,j,l)
            yn = shp(2,j,l)
            if(qdflg) then
              x1n = - shp1(1,j,l)
              y2n = - shp2(2,j,l)
              x2n = - shp1(2,j,l)
              y1n = - shp2(1,j,l)
              xyn =   x2n + y1n
            endif

!           Compute membrane part

            s(i1+1,j1+1) = s(i1+1,j1+1) + (a11*xn + a31*yn)
            s(i1+2,j1+1) = s(i1+2,j1+1) + (a12*xn + a32*yn)
            s(i1+1,j1+2) = s(i1+1,j1+2) + (a21*yn + a31*xn)
            s(i1+2,j1+2) = s(i1+2,j1+2) + (a22*yn + a32*xn)
            s(i1+6,j1+1) = s(i1+6,j1+1) + a13*xn  + a33*yn
            s(i1+6,j1+2) = s(i1+6,j1+2) + a23*yn  + a33*xn
            s(i1+1,j1+6) = s(i1+1,j1+6) + a11*x1n + a21*y2n + a31*xyn
            s(i1+2,j1+6) = s(i1+2,j1+6) + a12*x1n + a22*y2n + a32*xyn
            s(i1+6,j1+6) = s(i1+6,j1+6) + a13*x1n + a23*y2n + a33*xyn

!           Compute bending part

            do ii = ii1,ii2 ! {
              do jj = ii1,ii2 ! {
                do kk = 1,3 ! {
                  s(i1+ii,j1+jj) = s(i1+ii,j1+jj)
     &                           + btd(kk,ii)*bm(kk,jj,j)
                end do ! kk  }
              end do ! jj  }
            end do ! ii  }
            j1 = j1 + ndf
          end do ! j  }
          i1 = i1 + ndf
        end do ! i }

!       Compute Hughes/Brezzi rotation matrix

        if(ialph.ne.0 .and. qdflg) then
          j1 = 0
          do j = 1,4 ! {
            gg(j1+1) = gg(j1+1) - shp(2,j,l)*dvl(l)
            gg(j1+2) = gg(j1+2) + shp(1,j,l)*dvl(l)
            gg(j1+6) = gg(j1+6) - 2.d0*shp(3,j,l)*dvl(l)
            if(ialph.gt.1) then
              gg(j1+6) = gg(j1+6) + (shp1(2,j,l) - shp2(1,j,l))*dvl(l)
            endif
            j1 = j1 + ndf
          end do ! j  }
        endif
      end do ! l  }

!     Perform rank one update for Huges/Brezzi term

      if(ialph.gt.0 .and. qdflg) then

!       Compute H/B strain

        xx = 0.0d0
        j1 = 0
        do i = 1,4 ! {
          do j = 1,6 ! {
            xx = xx + gg(j1+j)*vl(j,i,1)
          end do ! j  }
          j1 = j1 + ndf
        end do ! i  }

!       Compute H/B residual and tangent

        ggv = pen*d(27)*thk*ctan1/dv

        do i = 1,nst ! {
          ggi = ggv*gg(i)
          r(i,1) = r(i,1) - ggi*xx
          do j = 1,nst ! {
            s(i,j) = s(i,j) + ggi*gg(j)
          end do ! j  }
        end do ! i  }
      endif

!     Correct residual and tangent matrix for element warpage

      i1 = 1
      if(yl(3,1).ne.0.0d0) then
        do i = 1,4 ! {
          r(4,i) = r(4,i) + yl(3,i)*r(2,i)
          r(5,i) = r(5,i) - yl(3,i)*r(1,i)
          j1 = i1
          do j = i,4 ! {
            call proj3d(s(i1,j1),yl(3,i),yl(3,j),nst)
            j1 = j1 + ndf
          end do ! j  }
          i1 = i1 + ndf
        end do ! i  }
      endif

!     Rotate to global frame

      call rots3d(s,r,tr,nst,ndf)

      return

!     Compute and output element variables

4     l = nint(d(6))
      if(l*l.ne.lint) call int2d(l,lint,sg)

      do l = 1,lint ! {

!       Form shape functions

        if(qdflg) then
          call rshp3d(sg(1,l),yl,shp,shp1,shp2,xsj,3)

!         Modify rotational shape functions

          do j = 1,4
            do i = 1,3
              shp1(i,j,1) = shp1(i,j,1) - gshp1(i,j)/dv
              shp2(i,j,1) = shp2(i,j,1) - gshp2(i,j)/dv
            end do ! i  }
          end do ! j  }
        else
          call shp2d (sg(1,l),yl,shp,xsj,3,nel,ix,.false.)
        endif
        call stre3d(d,xl,vl,ndm,nel,1, xx,yy,zz,eps,norm,momt,dd)
        mct = mct - 3
        if(mct.le.0) then
          write(iow,2002) o,head
          if(ior.lt.0) then
            write(*,2002) o,head
          endif
          mct = 50
        end if
        write(iow,2003) n_el,xx,norm,ma,yy,momt,zz,eps,sigt,sigb
        if(ior.lt.0) then
          write(*,2003) n_el,xx,norm,ma,yy,momt,zz,eps,sigt,sigb
        endif
      end do ! l  }
      return

!     Compute element mass or geometric stifness arrays

5     if(imtyp.eq.1) then

!       Compute mass

        do l = 1,lint ! {
          dv1 = dvl(l)*d(4)*d(14)
          i1  = 0
          do j = 1,4 ! {
            do i = 1,3 ! {
              r(i,j)       = r(i,j) + shp(3,j,l)*dv1
              s(i1+i,i1+i) = r(i,j)
            end do ! i }
            i1 = i1 + ndf
          end do ! j }
        end do ! l }

      elseif(imtyp.eq.2) then

!       Compute geometric stiffness

        do i = 1,4 ! {
          do j = 1,3 ! {
            vl(j  ,i,1) = 0.0d0
            vl(j+3,i,1) = 0.0d0
            do k = 1,3 ! {
              vl(j  ,i,1) = vl(j  ,i,1) + tr(j,k)*ul(k  ,i,1)
              vl(j+3,i,1) = vl(j+3,i,1) + tr(j,k)*ul(k+3,i,1)
            end do ! k  }
          end do ! j  }
        end do ! i  }

        do l = 1,lint ! {

!         Modify rotational shape functions

          do j = 1,4 ! {
            do i = 1,3 ! {
              shp1(i,j,l) = shp1(i,j,l) - gshp1(i,j)/dv
              shp2(i,j,l) = shp2(i,j,l) - gshp2(i,j)/dv
            end do ! i  }
          end do ! j  }

          call stre3d(d,xl,vl,ndm,nel,l, xx,yy,zz,eps,norm,momt,dd)

          i1 = 0
          do i = 1,4 !{
            j1 = 0
            dv1 = (shp(1,i,l)*norm(1) + shp(2,i,l)*norm(3))*dvl(l)
            dv2 = (shp(1,i,l)*norm(3) + shp(2,i,l)*norm(2))*dvl(l)
            do j = 1,4 !{
              a11 = dv1*shp(1,j,l) + dv2*shp(2,j,l)
              do k = 1,3 !{
                s(i1+k,j1+k) = s(i1+k,j1+k) - a11
              end do ! k  }
              j1 = j1 + ndf
            end do ! j  }
            i1 = i1 + ndf
          end do ! i  }

        end do ! l  }

        i1 = 1
        if(yl(3,1).ne.0.0d0) then
          do i = 1,4 !{
            j1 = i1
            do j = i,4 !{
              call proj3d(s(i1,j1),yl(3,i),yl(3,j),nst)
              j1 = j1 + ndf
            end do ! j  }
            i1 = i1 + ndf
          end do ! i  }
        endif
        call rots3d(s,r,tr,nst,ndf)
      endif

      return

!     Compute nodal output quantities

8     call stcn3sh(ix,d,yl,ul,tr,hr(nph),hr(nph+numnp),dvl,
     &             ndf,nel,numnp)
      return

!     Formats

2002  format(a1,20a4//5x,'S h e l l   S t r e s s e s'//
     & ' elmt x-coord  xx-stress  yy-stress  xy-stress   1-stress',
     & '   2-stress   angle'/' matl y-coord  xx-moment  yy-moment',
     & '  xy-moment   1-moment   2-moment   angle'/
     & '      z-coord  xx-strain  yy-strain  xy-strain  xx-curvtr',
     & '  yy-curvtr  xy-curvtr'/13x,'  xx-sig(t)  yy-sig(t)  xy-sig(t)',
     & '   1-sig(t)   2-sig(t)   angle'/13x,'  xx-sig(b)  yy-sig(b)',
     & '  xy-sig(b)   1-sig(b)   2-sig(b)   angle'/38(' -'))

2003  format(/i5,0p,1f8.3,1p,5e11.3,0p,1f8.2/i5,0p,1f8.3,1p,5e11.3,
     &       0p,1f8.2/5x,0p,1f8.3,1p,6e11.3/(13x,1p,6e11.3))

      end subroutine shl3ds

      subroutine dktb3d(sg,xsjt)

      implicit  none

      include  'shld16.h'
      include  'shle16.h'

      integer       :: i,j,k
      real (kind=8) :: sg(2),el(3),shn(2,3),bd(3),cd(3),xsjt,shm1,shm2
      real (kind=8) :: a1(3),a2(3),b1(3),b2(3),c1(3),c2(3),d1(3),d2(3)
      real (kind=8) :: e1(3),e2(3)

      save

!     Compute area coordinates for point

      el(1) = 0.25d0*(1.d0 - sg(1))*(1.d0 - sg(2))
      el(2) = 0.25d0*(1.d0 + sg(1))*(1.d0 - sg(2))
      el(3) = 0.50d0*(1.d0 + sg(2))

!     Form shape function terms for strains

      do i = 1,3 ! {
        bd(i) = b(i)/xsjt
        cd(i) = c(i)/xsjt
      end do ! i  }
      do i = 1,3 ! {
        j        = mod(i,3) + 1
        k        = mod(j,3) + 1
        shn(1,i) = bd(i)*(4.0d0*el(i) - 1.0)
        shn(2,i) = cd(i)*(4.0d0*el(i) - 1.0)
        shm1     =(bd(j)*el(k) + bd(k)*el(j))*4.d0
        shm2     =(cd(j)*el(k) + cd(k)*el(j))*4.d0
        a1(i)    = aa(i)*shm1
        a2(i)    = aa(i)*shm2
        b1(i)    = bb(i)*shm1
        b2(i)    = bb(i)*shm2
        c1(i)    = cc(i)*shm1
        c2(i)    = cc(i)*shm2
        d1(i)    = dd(i)*shm1
        d2(i)    = dd(i)*shm2
        e1(i)    = ee(i)*shm1
        e2(i)    = ee(i)*shm2
      end do ! i  }

!     Plate strain displacement matrix

      do i = 1,3 ! {
        j = mod(i,3) + 1
        k = mod(j,3) + 1
        bm(1,3,i) = a1(k) - a1(j)
        bm(1,4,i) = b1(k) + b1(j)
        bm(1,5,i) = c1(k) + c1(j) - shn(1,i)
        bm(2,3,i) = d2(k) - d2(j)
        bm(2,4,i) =-e2(k) - e2(j) + shn(2,i)
        bm(2,5,i) =-b2(k) - b2(j)
        bm(3,3,i) = a2(k) - a2(j) + d1(k) - d1(j)
        bm(3,4,i) =-e1(k) - e1(j) + shn(1,i) - bm(2,5,i)
        bm(3,5,i) = c2(k) + c2(j) - shn(2,i) - bm(1,4,i)
      end do ! i  }

      end subroutine dktb3d

      subroutine dktq3d(shm,shn)

      implicit  none

      include  'shld16.h'
      include  'shle16.h'

      integer       :: i,j

      real (kind=8) :: shm(3,4),shn(3,4)

      save

!     Form strain-displacement array for DKQ element

      do i = 1,4 ! {
        j = mod(i+2,4) + 1
        bm(1,3,i) = aa(i)*shm(1,i) - aa(j)*shm(1,j)
        bm(1,4,i) = bb(i)*shm(1,i) + bb(j)*shm(1,j)
        bm(1,5,i) = cc(i)*shm(1,i) + cc(j)*shm(1,j) - shn(1,i)
        bm(2,3,i) = dd(i)*shm(2,i) - dd(j)*shm(2,j)
        bm(2,4,i) =-ee(i)*shm(2,i) - ee(j)*shm(2,j) + shn(2,i)
        bm(2,5,i) =-bb(i)*shm(2,i) - bb(j)*shm(2,j)
        bm(3,3,i) = aa(i)*shm(2,i) - aa(j)*shm(2,j)
     &            + dd(i)*shm(1,i) - dd(j)*shm(1,j)
        bm(3,4,i) =-ee(i)*shm(1,i) - ee(j)*shm(1,j) + shn(1,i)
     &            - bm(2,5,i)
        bm(3,5,i) = cc(i)*shm(2,i) + cc(j)*shm(2,j) - shn(2,i)
     &            - bm(1,4,i)
      end do ! i  }

      end subroutine dktq3d

      subroutine hshp3d(shp,shp1,shp2)

      implicit  none

      include  'shle16.h'

      integer       :: i,j,l
      real (kind=8) :: shp(3,4),shp1(3,4),shp2(3,4),shx(4),shy(4)

      save

!     Form shape functions

      do l = 1,3 ! {

        do i = 1,4 ! {
          shx(i) = shp(l,i)*c(i)
          shy(i) = shp(l,i)*b(i)
        end do ! i  }

        j = 4
        do i = 1,4 ! {
          shp1(l,i) = shy(i) - shy(j)
          shp2(l,i) = shx(i) - shx(j)
          j = i
        end do ! i  }

      end do ! l  }

      end subroutine hshp3d

      subroutine jacq3d(xl)

      implicit  none

      include  'shld16.h'
      include  'shle16.h'

      integer       :: i,k
      real (kind=8) :: cxx,cyy,cxy
      real (kind=8) :: dz, ad, a4, b2,c2, sql, x31,y31, x42,y42
      real (kind=8) :: xl(3,*)

      save

!     Form geometric constants for DKQ element

      cxx = 0.0d0
      cyy = 0.0d0
      cxy = 0.0d0
      do i = 1,4 ! {
        k = mod(i,4) + 1
        b(i) = xl(2,k) - xl(2,i)
        c(i) = xl(1,i) - xl(1,k)
        dz   = xl(3,k) - xl(3,i)
        b2 = b(i)*b(i)
        c2 = c(i)*c(i)
        cxx = cxx + dz*b2/(b2 + c2)
        cyy = cyy + dz*c2/(b2 + c2)
        cxy = cxy + (dz+dz)*b(i)*c(i)/(b2 + c2)
        sql = (b2 + c2)/0.75d0
        aa(i) = (c(i)+c(i))/sql
        bb(i) = b(i)*c(i)/sql
        cc(i) = c2/sql
        dd(i) =-(b(i)+b(i))/sql
        ee(i) = b2/sql
        b(i)  = b(i)*0.125d0
        c(i)  = c(i)*0.125d0
      end do ! i  }

      if(xl(3,1).ne.0.0d0) then
        x31 = xl(1,3) - xl(1,1)
        y31 = xl(2,3) - xl(2,1)
        x42 = xl(1,4) - xl(1,2)
        y42 = xl(2,4) - xl(2,2)
        a4  = (x31*y42 - x42*y31)*2.0d0
        ad  = a4*a4*0.25d0
        x31 = x31/ad
        y31 = y31/ad
        x42 = x42/ad
        y42 = y42/ad
        bm(1,1,1) = -cxx*x42
        bm(2,1,1) = -cyy*x42
        bm(3,1,1) = -cxy*x42
        bm(1,2,1) = -cxx*y42
        bm(2,2,1) = -cyy*y42
        bm(3,2,1) = -cxy*y42
        bm(1,6,1) = -cxx/a4
        bm(2,6,1) = -cyy/a4
        bm(3,6,1) = -cxy/a4
        bm(1,1,2) =  cxx*x31
        bm(2,1,2) =  cyy*x31
        bm(3,1,2) =  cxy*x31
        bm(1,2,2) =  cxx*y31
        bm(2,2,2) =  cyy*y31
        bm(3,2,2) =  cxy*y31
        bm(1,6,2) = -cxx/a4
        bm(2,6,2) = -cyy/a4
        bm(3,6,2) = -cxy/a4
        do i = 1,3 ! {
          bm(i,1,3) = -bm(i,1,1)
          bm(i,2,3) = -bm(i,2,1)
          bm(i,6,3) =  bm(i,6,1)
          bm(i,1,4) = -bm(i,1,2)
          bm(i,2,4) = -bm(i,2,2)
          bm(i,6,4) =  bm(i,6,2)
        end do ! i  }
      endif

      end subroutine jacq3d

      subroutine jtri3d(xl,xsjt)

      implicit  none

      include  'shld16.h'
      include  'shle16.h'

      integer       :: i,j,k
      real (kind=8) :: xl(3,*), xsjt,sql,cs,bs

      save

!     Form terms for jacobian of a triangle

      do i = 1,3 ! {
        j = mod(i,3) + 1
        k = mod(j,3) + 1
        b(i) = xl(2,k) - xl(2,j)
        c(i) = xl(1,j) - xl(1,k)
        sql = (b(i)+b(i))*(b(i)+b(i)) + (c(i)+c(i))*(c(i)+c(i))
        cs = c(i)/sql
        bs = b(i)/sql
        aa(i) = 6.d0*cs
        bb(i) = 3.d0*bs*c(i)
        cc(i) = c(i)*cs - b(i)*(bs+bs)
        dd(i) =-6.d0*bs
        ee(i) = b(i)*bs - c(i)*(cs+cs)
      end do ! i  }
      b(4)  = 0.0d0
      c(4)  = 0.0d0
      aa(4) = 0.0d0
      bb(4) = 0.0d0
      cc(4) = 0.0d0
      dd(4) = 0.0d0
      ee(4) = 0.0d0

!     Store jacobian

      xsjt  = b(1)*c(2) - b(2)*c(1)

      end subroutine jtri3d

      subroutine proj3d(s,zi,zj,nst)

      implicit  none

      integer       :: i, nst
      real (kind=8) :: zi, zj
      real (kind=8) :: s(nst,*)

!     Modify stiffness for offset projections

!     Postmultiply by transformation

      do i = 1,6 ! {
        s(i,4) =  zj*s(i,2) + s(i,4)
        s(i,5) = -zj*s(i,1) + s(i,5)
      end do ! i  }

!     Premultiply using modified terms from postmultiplication

      do i = 1,6 ! {
        s(4,i) =  zi*s(2,i) + s(4,i)
        s(5,i) = -zi*s(1,i) + s(5,i)
      end do ! i  }

      end subroutine proj3d

      subroutine rots3d(s,p,t,nst,ndf)

!     Transform loads and stiffness to global coords.

      implicit  none

      integer       :: nst,ndf, i,i0,i1, ir,ii, j,j0,j1, jc,jj

      real (kind=8) :: s(nst,nst),p(nst),t(3,3),a(3,3),b(6)

      real (kind=8) :: dot

      i0 = 0
      do ir = 1,4 ! {
        do ii = 1,3 ! {
          b(ii  ) = dot(t(1,ii),p(i0+1),3)
          b(ii+3) = dot(t(1,ii),p(i0+4),3)
        end do ! ii }
        do ii = 1,6 ! {
          p(i0+ii) = b(ii)
        end do ! ii }
        j0 = i0
        do jc = ir,4 ! {
          i1 = i0
          do i = 1,2 ! {
            j1 = j0
            do j = 1,2 ! {
              do ii = 1,3 ! {
              do jj = 1,3 ! {
                a(jj,ii) = dot(t(1,ii),s(i1+1,jj+j1),3)
              end do ! jj }
              end do ! ii }
              do jj = 1,3 ! {
              do ii = 1,3 ! {
                s(ii+i1,jj+j1) = dot(a(1,ii),t(1,jj),3)
              end do ! ii }
              end do ! jj }
              j1 = j1 + 3
            end do ! j }
            i1 = i1 + 3
          end do ! i }

!         Compute symmetric block

          if(ir.ne.jc) then
            do i = 1,6 ! {
              do j = 1,6 ! {
                s(j0+j,i0+i) = s(i0+i,j0+j)
              end do ! j  }
            end do ! i  }
          endif
          j0 = j0 + ndf
        end do ! jc  }
        i0 = i0 + ndf
      end do ! ir  }

      end subroutine rots3d

      subroutine rshp3d(sg,x,shp,shp1,shp2,xsj,ndm)

!     Shape function routine for two dimensional elements

      implicit  none

      integer       :: ndm, i

      real (kind=8) :: s2,t2, tp, xsj, xsj1

      real (kind=8) :: sg(2),shp(3,8)
      real (kind=8) :: x(ndm,*),sx(2,2),shp1(3,4),shp2(3,4)

!     Form 4-node quadrilateral shape functions

      shp(1,2) = 0.25d0*(1.d0-sg(2))
      shp(1,3) = 0.25d0*(1.d0+sg(2))
      shp(1,1) = -shp(1,2)
      shp(1,4) = -shp(1,3)
      shp(2,4) = 0.25d0*(1.d0-sg(1))
      shp(2,3) = 0.25d0*(1.d0+sg(1))
      shp(2,2) = -shp(2,3)
      shp(2,1) = -shp(2,4)
      shp(3,1) = shp(1,2)*(1.d0-sg(1))
      shp(3,2) = shp(1,2)*(1.d0+sg(1))
      shp(3,3) = shp(1,3)*(1.d0+sg(1))
      shp(3,4) = shp(1,3)*(1.d0-sg(1))
      s2 = (1.d0-sg(1)*sg(1))*0.5d0
      t2 = (1.d0-sg(2)*sg(2))*0.5d0
      shp(3,5) =  s2*(1.d0-sg(2))
      shp(3,6) =  t2*(1.d0+sg(1))
      shp(3,7) =  s2*(1.d0+sg(2))
      shp(3,8) =  t2*(1.d0-sg(1))
      shp(1,5) = -sg(1)*(1.d0-sg(2))
      shp(1,6) =  t2
      shp(1,7) = -sg(1)*(1.d0+sg(2))
      shp(1,8) = -t2
      shp(2,5) = -s2
      shp(2,6) = -sg(2)*(1.d0+sg(1))
      shp(2,7) =  s2
      shp(2,8) = -sg(2)*(1.d0-sg(1))

!     Construct jacobian and its inverse

      sx(2,2) =  x(1,1)*shp(1,1)+x(1,2)*shp(1,2)+x(1,3)*shp(1,3)
     &          +x(1,4)*shp(1,4)
      sx(1,2) =  x(1,1)*shp(2,1)+x(1,2)*shp(2,2)+x(1,3)*shp(2,3)
     &          +x(1,4)*shp(2,4)
      sx(2,1) =  x(2,1)*shp(1,1)+x(2,2)*shp(1,2)+x(2,3)*shp(1,3)
     &          +x(2,4)*shp(1,4)
      sx(1,1) =  x(2,1)*shp(2,1)+x(2,2)*shp(2,2)+x(2,3)*shp(2,3)
     &          +x(2,4)*shp(2,4)
      xsj = sx(1,1)*sx(2,2)-sx(1,2)*sx(2,1)
      xsj1 = xsj
      if(xsj.eq.0.0d0) xsj1 = 1.0d0
      sx(2,2) = sx(2,2)/xsj1
      sx(1,1) = sx(1,1)/xsj1
      sx(1,2) =-sx(1,2)/xsj1
      sx(2,1) =-sx(2,1)/xsj1

!     Form global derivatives

      do i = 1,8 ! {
        tp        = shp(1,i)*sx(1,1)+shp(2,i)*sx(2,1)
        shp(2,i)  = shp(1,i)*sx(1,2)+shp(2,i)*sx(2,2)
        shp(1,i) = tp
      end do ! i  }

!     Form rotational and 5-th shape functions

      call hshp3d(shp(1,5),shp1,shp2)

      end subroutine rshp3d

      subroutine stcn3sh(ix,d,yl,ul,tr,dt,st,dvl,ndf,nel,numnp)

      implicit  none

      include  'shpf16.h'
      include  'sstr16.h'

      integer       :: ndf,nel,numnp, i,j,k, ii
      real (kind=8) :: xsji, xx,yy,zz

      integer       :: ix(*)

      real (kind=8) :: dt(numnp),st(numnp,*)
      real (kind=8) :: yl(3,*),tr(3,3),d(*),ul(ndf,*)
      real (kind=8) :: vl(6,4),eps(6),momt(6),norm(6),dd(6,6),dvl(4)

      save

!     Compute membrane and bending stresses for projection.

      do i = 1,4 ! {
        do j = 1,3 ! {
          vl(j  ,i) = 0.0d0
          vl(j+3,i) = 0.0d0
          do k = 1,3 ! {
            vl(j  ,i) = vl(j  ,i) + tr(j,k)*ul(k  ,i)
            vl(j+3,i) = vl(j+3,i) + tr(j,k)*ul(k+3,i)
          end do ! k  }
        end do ! j  }
      end do ! i  }

      do k = 1,4 ! {
        call stre3d(d,yl,vl,3,nel,k, xx,yy,zz,eps,norm,momt,dd)
        do j = 1,4 ! {
          xsji = dvl(k)*shp(3,j,k)
          ii = ix(j)
          if(ii.gt.0) then
            dt(ii)    = dt(ii)    + xsji

            st(ii,1)  = st(ii,1)  + norm(1)*xsji
            st(ii,2)  = st(ii,2)  + norm(2)*xsji
            st(ii,3)  = st(ii,3)  + norm(3)*xsji
            st(ii,4)  = st(ii,4)  + norm(4)*xsji
            st(ii,5)  = st(ii,5)  + norm(5)*xsji

            st(ii,6)  = st(ii,6)  + momt(1)*xsji
            st(ii,7)  = st(ii,7)  + momt(2)*xsji
            st(ii,8)  = st(ii,8)  + momt(3)*xsji
            st(ii,9)  = st(ii,9)  + momt(4)*xsji
            st(ii,10) = st(ii,10) + momt(5)*xsji

            st(ii,11) = st(ii,11) + eps(1)*xsji
            st(ii,12) = st(ii,12) + eps(2)*xsji
            st(ii,13) = st(ii,13) + eps(3)*xsji
            st(ii,14) = st(ii,14) + eps(4)*xsji
            st(ii,15) = st(ii,15) + eps(5)*xsji
            st(ii,16) = st(ii,16) + eps(6)*xsji

            st(ii,17) = st(ii,17) + sigt(1)*xsji
            st(ii,18) = st(ii,18) + sigt(2)*xsji
            st(ii,19) = st(ii,19) + sigt(3)*xsji

            st(ii,20) = st(ii,20) + sigb(1)*xsji
            st(ii,21) = st(ii,21) + sigb(2)*xsji
            st(ii,22) = st(ii,22) + sigb(3)*xsji

            st(ii,23) = st(ii,23) + max(sigb(4),sigt(4))*xsji
            st(ii,24) = st(ii,24) + min(sigb(5),sigt(5))*xsji

          end if
        end do ! j  }
      end do ! k  }

      end subroutine stcn3sh

      subroutine stre3d(d,xl,vl,ndm,nel,l, xx,yy,zz,eps,norm,momt,dd)

      implicit  none

      include  'shlc16.h'
      include  'shld16.h'

      integer       :: ndm,nel,l, i,j

      real (kind=8) :: xx,yy,zz,thk,thk3

      real (kind=8) :: d(*), xl(ndm,*),vl(6,*)
      real (kind=8) :: eps(6),norm(6),momt(6),temp(4)
      real (kind=8) :: dd(6,6),alp(6)

      include   'shpf16.h'
      include   'sstr16.h'

      save

!     Compute membrane and bending strains

      if(qdflg) then
        call dktq3d(shp(1,5,l),shp(1,1,l))
      else
        call dktb3d(sg(1,l),xsjt)
      endif

      do i = 1,6 ! {
        eps(i) = 0.0d0
      end do ! i  }
      xx = 0.0d0
      yy = 0.0d0
      zz = 0.0d0
      do j = 1,nel ! {
        xx = xx + shp(3,j,l)*xl(1,j)
        yy = yy + shp(3,j,l)*xl(2,j)
        zz = zz + shp(3,j,l)*xl(3,j)
        eps(1) = eps(1) + shp(1,j,l)*vl(1,j)
     &                  - shp1(1,j,l)*vl(6,j)
        eps(2) = eps(2) + shp(2,j,l)*vl(2,j)
     &                  - shp2(2,j,l)*vl(6,j)
        eps(3) = eps(3) + shp(1,j,l)*vl(2,j) + shp(2,j,l)*vl(1,j)
     &                  - (shp1(2,j,l) + shp2(1,j,l))*vl(6,j)
        do i = ii1,ii2 ! {
          eps(4) = eps(4) + bm(1,i,j)*vl(i,j)
          eps(5) = eps(5) + bm(2,i,j)*vl(i,j)
          eps(6) = eps(6) + bm(3,i,j)*vl(i,j)
        end do ! i  }
      end do ! j  }

      call dmat2d(d,d(31),dd,alp)

      thk     = d(14)
      thk3    = thk**3/12.d0

      norm(1) = (dd(1,1)*eps(1) + dd(1,2)*eps(2) + dd(1,4)*eps(3))*thk
      norm(2) = (dd(2,1)*eps(1) + dd(2,2)*eps(2) + dd(2,4)*eps(3))*thk
      norm(3) = (dd(4,1)*eps(1) + dd(4,2)*eps(2) + dd(4,4)*eps(3))*thk
      temp(1) = norm(1)
      temp(2) = norm(2)
      temp(4) = norm(3)

      call pstr2d(temp,norm(4))

      momt(1) = (dd(1,1)*eps(4) + dd(1,2)*eps(5) + dd(1,4)*eps(6))*thk3
      momt(2) = (dd(2,1)*eps(4) + dd(2,2)*eps(5) + dd(2,4)*eps(6))*thk3
      momt(3) = (dd(4,1)*eps(4) + dd(4,2)*eps(5) + dd(4,4)*eps(6))*thk3
      temp(1) = momt(1)
      temp(2) = momt(2)
      temp(4) = momt(3)

      call pstr2d(temp,momt(4))

!     Compute surface stresses from bending and in-plane forces

      thk = 1.d0/thk
      do i = 1,3 ! {
        sigt(i) = (norm(i) + 6.d0*momt(i)*thk)*thk
        sigb(i) = (norm(i) - 6.d0*momt(i)*thk)*thk
      end do ! i   }

      temp(1) = sigt(1)
      temp(2) = sigt(2)
      temp(4) = sigt(3)

      call pstr2d(temp,sigt(4))

      temp(1) = sigb(1)
      temp(2) = sigb(2)
      temp(4) = sigb(3)

      call pstr2d(temp,sigb(4))

      end subroutine stre3d

      subroutine rayl3d(d,vl,nel,l, norm,momt,dd)

!     Purpose: Rayleigh damping stresses

      implicit  none

      include  'shlc16.h'
      include  'shld16.h'

      integer       :: nel,l, i,j

      real (kind=8) :: thk,thk3

      real (kind=8) :: d(*), vl(6,*), eps(6),norm(6),momt(6), dd(6,6)

      include   'shpf16.h'
      include   'sstr16.h'

      save

!     Compute membrane and bending strains

      if(qdflg) then
        call dktq3d(shp(1,5,l),shp(1,1,l))
      else
        call dktb3d(sg(1,l),xsjt)
      endif

      do i = 1,6 ! {
        eps(i) = 0.0d0
      end do ! i  }
      do j = 1,nel ! {
        eps(1) = eps(1) + shp(1,j,l)*vl(1,j)
     &                  - shp1(1,j,l)*vl(6,j)
        eps(2) = eps(2) + shp(2,j,l)*vl(2,j)
     &                  - shp2(2,j,l)*vl(6,j)
        eps(3) = eps(3) + shp(1,j,l)*vl(2,j) + shp(2,j,l)*vl(1,j)
     &                  - (shp1(2,j,l) + shp2(1,j,l))*vl(6,j)
        do i = ii1,ii2 ! {
          eps(4) = eps(4) + bm(1,i,j)*vl(i,j)
          eps(5) = eps(5) + bm(2,i,j)*vl(i,j)
          eps(6) = eps(6) + bm(3,i,j)*vl(i,j)
        end do ! i  }
      end do ! j  }

      thk     = d(14)*d(78)
      thk3    = thk**3/12.d0

      norm(1) = norm(1) + (dd(1,1)*eps(1)
     &                  +  dd(1,2)*eps(2)
     &                  +  dd(1,4)*eps(3))*thk
      norm(2) = norm(1) + (dd(2,1)*eps(1)
     &                  +  dd(2,2)*eps(2)
     &                  +  dd(2,4)*eps(3))*thk
      norm(3) = norm(1) + (dd(4,1)*eps(1)
     &                  +  dd(4,2)*eps(2)
     &                  +  dd(4,4)*eps(3))*thk

      momt(1) = momt(1) + (dd(1,1)*eps(4)
     &                  +  dd(1,2)*eps(5)
     &                  +  dd(1,4)*eps(6))*thk3
      momt(2) = momt(2) + (dd(2,1)*eps(4)
     &                  +  dd(2,2)*eps(5)
     &                  +  dd(2,4)*eps(6))*thk3
      momt(3) = momt(3) + (dd(4,1)*eps(4)
     &                  +  dd(4,2)*eps(5)
     &                  +  dd(4,4)*eps(6))*thk3

      end subroutine rayl3d

      subroutine tran3d(xl,yl,t,ndm)

!     Compute transformation array and surface coords.

      implicit  none

      integer       :: ndm, i,j,k

      real (kind=8) :: v1, v2, htol, dl1,dl2
      real (kind=8) :: x0(3),xl(ndm,*),yl(3,4),t(3,3)

!     Compute inplane direction cosines (bisect diagonals)

      do i = 1,3 ! {
        t(1,i) = xl(i,3) - xl(i,1)
        t(2,i) = xl(i,2) - xl(i,4)
      end do ! i  }
      dl1 = sqrt(t(1,1)**2 + t(1,2)**2 + t(1,3)**2)
      dl2 = sqrt(t(2,1)**2 + t(2,2)**2 + t(2,3)**2)
      do i = 1,3 ! {
        v1 = t(1,i)/dl1
        v2 = t(2,i)/dl2
        t(1,i) = v1 + v2
        t(2,i) = v1 - v2
      end do ! i  }
      dl1 = sqrt(t(1,1)**2 + t(1,2)**2 + t(1,3)**2)
      dl2 = sqrt(t(2,1)**2 + t(2,2)**2 + t(2,3)**2)
      do i = 1,3 ! {
        t(1,i) = t(1,i)/dl1
        t(2,i) = t(2,i)/dl2

!       Compute center (0,0) displacement

        x0(i) = 0.25*(xl(i,1) + xl(i,2) + xl(i,3) + xl(i,4))
      end do ! i  }

!     Compute normal to surface

      t(3,1) = t(1,2)*t(2,3) - t(2,2)*t(1,3)
      t(3,2) = t(1,3)*t(2,1) - t(2,3)*t(1,1)
      t(3,3) = t(1,1)*t(2,2) - t(2,1)*t(1,2)

!     Compute projected middle surface coordinates

      do i = 1,4 ! {
      do j = 1,3 ! {
        yl(j,i) = 0.0d0
        do k = 1,3 ! {
          yl(j,i) = yl(j,i) + t(j,k)*(xl(k,i) - x0(k))
        end do ! k  }
      end do ! j  }
      end do ! i  }

!     Set offset coordinates to zero if small compared to plan size

      htol =  0.0d0
      do i = 1,4 ! {
        htol = max(htol,abs(yl(1,i)),abs(yl(2,i)))
      end do ! i  }
      htol = htol*1.d-7
      do i = 1,4 ! {
        if(abs(yl(3,i)) .le. htol) yl(3,i) = 0.0d0
      end do ! i  }

      end subroutine tran3d
