!$Id:$
      subroutine membr3d(d,ul,xl,ix,s,p,ndf,ndm,nst,isw)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:  Quadrilateral membrane element for feap

!     Input parameters set as follows:

!       Small deformation
!         ndm = 3 (x,y,z cartesian coordinates at nodes)
!         ndf = 3 (u-x,u-y,u-z at nodes)
!         nen = 4 nodes (counterclockwise around element)

!       Note: 1-direction bisects diagonals between 2-3 element and
!             2-direction bisects diagonals between 3-4 element nodes.
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      implicit  none

      include  'eldata.h'
      include  'hdata.h'
      include  'iofile.h'
      include  'mdata.h'

      include  'comblk.h'

      integer       :: ndm,ndf,nst,isw, tdof, i
      integer       :: ix(*)

      real (kind=8) :: d(*),xl(ndm,*),ul(ndf,*),s(nst,*),p(nst)

      save

!     Input material properties

      if(isw.eq.1) then

        if(ior.lt.0) write(*,2000)
        write(iow,2000)
        call inmate(d,tdof, 0 ,5)

!       Deactivate dof in element for dof > 3

        do i = 4,ndf
          ix(i) = 0
        end do

!       Set plot sequence

        pstyp = 2

!     Remaining options

      else

!       Small deformation

        if(d(18).gt.0.0d0) then
          call mem3ds(d,ul,xl,ix,s,p,ndf,ndm,nst,isw)

!       Finite deformation

        else
          write(*,*) '  *ERROR* No finite deformation membrane'
        endif

      endif

!     Format

2000  format(5x,'T h r e e   D i m e n s i o n a l   M e m b r a n e',
     &          '   E l e m e n t')

      end subroutine membr3d

      subroutine mem3ds(d,ul,xl,ix,s,p,ndf,ndm,nst,isw)

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:  Quadrilateral membrane element for feap

!.... Input parameters set as follows:

!         ndm = 3 (x,y,z cartesian coordinates at nodes)
!         ndf = 3 (u-x,u-y,u-z at nodes)
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
      integer       :: i,j,k,l,lint, i1,j1

      real (kind=8) :: a11,a12, a21,a22, a31,a32, xx,yy,zz
      real (kind=8) :: dv, dv1,dv2, pen, xsj, thk
      real (kind=8) :: xn,yn, shp1i,shp2i,shp3i

      integer       :: ix(*)

      real (kind=8) :: d(*),xl(ndm,*),ul(ndf,*),s(nst,*),p(nst)
      real (kind=8) :: norm(6),dvl(9),eps(6)
      real (kind=8) :: yl(3,4),vl(6,4),tr(3,3),bl(3),bg(3), dd(6,6)

      save

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
          vl(j  ,i) = 0.0d0
          do k = 1,3
            vl(j  ,i) = vl(j  ,i) + tr(j,k)*ul(k,i)
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
        call jtri3d(yl,xsjt)
      else
        qdflg = .true.
        pen   = d(60)
        call jacq3d(yl)
      endif

      dv = 0.0d0
      do l = 1,lint ! {

!       Form shape functions and their integrals

        call rshp3d(sg(1,l),yl,shp(1,1,l),shp1(1,1,l),
     &              shp2(1,1,l),xsj,3)

        dvl(l) = xsj*sg(3,l)
        dv     = dv + dvl(l)

      end do ! l  }

      if(isw.eq.5) go to 5

!     Compute thickness for element

      thk  = d(14)

      if(isw.eq.4) go to 4
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

!     Compute membrane/load parts

      do l = 1,lint ! {

        dv1 = thk *dvl(l)*ctan(1)

        call stre3m(d,yl,vl,3,nel,l, xx,yy,zz,eps,norm,dd)

!       Store time history plot data for element

        i = 3*(l-1)
        do j = 1,3
          tt(j+i  ) = norm(j)
        end do ! j

!       Recover previously computed shape functions

        i1 = 1
        do i = 1,4 ! {
          shp1i = shp(1,i,l)
          shp2i = shp(2,i,l)
          shp3i = shp(3,i,l)

!         Compute loading term

          p(i1  ) = p(i1  ) + shp3i*bl(1)*dvl(l)
          p(i1+1) = p(i1+1) + shp3i*bl(2)*dvl(l)
          p(i1+2) = p(i1+2) + shp3i*bl(3)*dvl(l)

!         Compute stress divergence terms

          p(i1  ) = p(i1  ) - (shp1i*norm(1) + shp2i*norm(3))*dvl(l)

          p(i1+1) = p(i1+1) - (shp2i*norm(2) + shp1i*norm(3))*dvl(l)

!         Form stress-displacement matrix (Bi-trans * D)

          a11 = (dd(1,1)*shp1i + dd(1,4)*shp2i)*dv1
          a12 = (dd(1,2)*shp2i + dd(1,4)*shp1i)*dv1
          a21 = (dd(2,1)*shp1i + dd(2,4)*shp2i)*dv1
          a22 = (dd(2,2)*shp2i + dd(2,4)*shp1i)*dv1
          a31 = (dd(4,1)*shp1i + dd(4,4)*shp2i)*dv1
          a32 = (dd(4,2)*shp2i + dd(4,4)*shp1i)*dv1

!         Loop on columns

          j1 = i1
          do j = i,4 ! {
            xn = shp(1,j,l)
            yn = shp(2,j,l)

!           Compute membrane part

            s(i1  ,j1  ) = s(i1  ,j1  ) + (a11*xn + a31*yn)
            s(i1+1,j1  ) = s(i1+1,j1  ) + (a12*xn + a32*yn)
            s(i1  ,j1+1) = s(i1  ,j1+1) + (a21*yn + a31*xn)
            s(i1+1,j1+1) = s(i1+1,j1+1) + (a22*yn + a32*xn)

            j1 = j1 + ndf
          end do ! j  }
          i1 = i1 + ndf
        end do ! i }
      end do ! l }

!     Rotate to global frame

      call rotm3d(s,p,tr,nst,ndf)

      return

!     Compute and output element variables

4     l = nint(d(6))
      if(l*l.ne.lint) call int2d(l,lint,sg)

      do l = 1,lint ! {

!       Form shape functions

        call rshp3d(sg(1,l),yl,shp,shp1,shp2,xsj,3)

        call stre3m(d,xl,vl,ndm,nel,1, xx,yy,zz,eps,norm,dd)
        mct = mct - 3
        if(mct.le.0) then
          write(iow,2002) o,head
          if(ior.lt.0) then
            write(*,2002) o,head
          endif
          mct = 50
        end if
        write(iow,2003) n_el,xx,norm,ma,yy,zz,eps,sigt
        if(ior.lt.0) then
          write(*,2003) n_el,xx,norm,ma,yy,zz,eps,sigt
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
              p(i1+i)      = p(i1+i) + shp(3,j,l)*dv1
              s(i1+i,i1+i) = p(i1+i)
            end do ! i }
            i1 = i1 + ndf
          end do ! j }
        end do ! l }

      elseif(imtyp.eq.2) then

!       Compute geometric stiffness

        do i = 1,4 ! {
          do j = 1,3 ! {
            vl(j  ,i) = 0.0d0
            do k = 1,3 ! {
              vl(j  ,i) = vl(j  ,i) + tr(j,k)*ul(k,i)
            end do ! k  }
          end do ! j  }
        end do ! i  }

        do l = 1,lint ! {

          call stre3m(d,xl,vl,ndm,nel,l, xx,yy,zz,eps,norm,dd)

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

        call rotm3d(s,p,tr,nst,ndf)
      endif

      return

!     Compute nodal output quantities

8     call stcn3sh(ix,d,yl,ul,tr,hr(nph),hr(nph+numnp),dvl,
     &             ndf,nel,numnp)
      return

!     Formats

2002  format(a1,20a4//5x,'S h e l l   S t r e s s e s'//
     & ' elmt x-coord  xx-stress  yy-stress  xy-stress   1-stress',
     & '   2-stress   angle'/' matl y-coord  xx-strain  yy-strain',
     & '  xy-strain  xx-curvtr  yy-curvtr  xy-curvtr'/'      z-coord',
     & '  xx-sig(t)  yy-sig(t)  xy-sig(t)   1-sig(t)   2-sig(t)  angle')

2003  format(/i5,0p,1f8.3,1p,5e11.3,0p,1f8.2/
     &       /i5,0p,1f8.3,1p,6e11.3/
     &       /5x,0p,1f8.3,1p,6e11.3)

      end subroutine mem3ds

      subroutine rotm3d(s,p,t,nst,ndf)

!     Transform loads and stiffness to global coords.

      implicit  none

      integer       :: nst,ndf, i,i0, ir,ii, j,j0, jc,jj

      real (kind=8) :: s(nst,nst),p(nst),t(3,3),a(3,3),b(3)

      real (kind=8) :: dot

      i0 = 0
      do ir = 1,4 ! {
        do ii = 1,3 ! {
          b(ii  ) = dot(t(1,ii),p(i0+1),3)
        end do ! ii }
        do ii = 1,3 ! {
          p(i0+ii) = b(ii)
        end do ! ii }
        j0 = i0
        do jc = ir,4 ! {
          do ii = 1,3 ! {
            do jj = 1,3 ! {
              a(jj,ii) = dot(t(1,ii),s(i0+1,jj+j0),3)
            end do ! jj }
          end do ! ii }
          do jj = 1,3 ! {
            do ii = 1,3 ! {
              s(ii+i0,jj+j0) = dot(a(1,ii),t(1,jj),3)
            end do ! ii }
          end do ! jj }

!         Compute symmetric block

          if(ir.ne.jc) then
            do i = 1,3 ! {
              do j = 1,3 ! {
                s(j0+j,i0+i) = s(i0+i,j0+j)
              end do ! j  }
            end do ! i  }
          endif
          j0 = j0 + ndf
        end do ! jc  }
        i0 = i0 + ndf
      end do ! ir  }

      end subroutine rotm3d

      subroutine stre3m(d,xl,vl,ndm,nel,l, xx,yy,zz,eps,norm,dd)

      implicit  none

      include   'shpf16.h'
      include   'sstr16.h'

      include  'shlc16.h'
      include  'shld16.h'

      integer       :: ndm,nel,l, i,j

      real (kind=8) :: xx,yy,zz,thk

      real (kind=8) :: d(*), xl(ndm,*),vl(6,*), eps(6),norm(6),temp(4)
      real (kind=8) :: dd(6,6),alp(6)

      save

!     Compute membrane strains

      do i = 1,6 ! {
        eps(i) = 0.0
      end do ! i  }
      xx = 0.0d0
      yy = 0.0d0
      zz = 0.0d0
      do j = 1,nel ! {
        xx = xx + shp(3,j,l)*xl(1,j)
        yy = yy + shp(3,j,l)*xl(2,j)
        zz = zz + shp(3,j,l)*xl(3,j)
        eps(1) = eps(1) + shp(1,j,l)*vl(1,j)
        eps(2) = eps(2) + shp(2,j,l)*vl(2,j)
        eps(3) = eps(3) + shp(1,j,l)*vl(2,j) + shp(2,j,l)*vl(1,j)
      end do ! j  }

      call dmat2d(d,d(31),dd,alp)

      thk     = d(14)

!     Compute surface stresses

      sigt(1) = dd(1,1)*eps(1) + dd(1,2)*eps(2) + dd(1,4)*eps(3)
      sigt(2) = dd(2,1)*eps(1) + dd(2,2)*eps(2) + dd(2,4)*eps(3)
      sigt(3) = dd(4,1)*eps(1) + dd(4,2)*eps(2) + dd(4,4)*eps(3)
      temp(1) = sigt(1)
      temp(2) = sigt(2)
      temp(4) = sigt(3)

      call pstr2d(temp,sigt(4))

!     Compute in-plane loading

      do i = 1,6 ! {
        norm(i) = sigt(i)*thk
      end do ! i   }

      end subroutine stre3m
