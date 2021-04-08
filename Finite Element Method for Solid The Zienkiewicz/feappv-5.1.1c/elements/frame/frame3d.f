!$Id:$
      subroutine frame3d(d,ul,xl,ix,s,p,ndf,ndm,nst,isw)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!     Three dimensional frame element

!     Control data:
!         ndm - 3 (x,y,z)
!         ndf - 6 (u,v,w, theta_x,theta_y,theta_z)
!         nen - 3 or more (see below)

!      Beam end nodes 1 and 2
!      Plane defined by nodes 1, 2, 3 contains z-axis
!                            (perpendicular to x-axis)
!      Vector products: e_1 = (x_2 - x_1)/|x_2 - x_1|
!                       e_2 = (x_2 - x_1) x ( x_3 - x_1)
!                       e_3 =  e_2 x e_1


!                  z (e_3)         x (e_1)
!                3 o - - - - - - /
!                  |           o 2
!                  |         /
!                  |       /
!                  |     /
!                  |   /
!                  | /
!                  / ------------- y (e_2)
!              1 o

!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'cdata.h'
      include  'eldata.h'
      include  'hdata.h'
      include  'iofile.h'
      include  'pmod2d.h'

      include  'comblk.h'

      integer       :: ndf,ndm,nst,isw, tdof, i

      integer       :: ix(*)
      real (kind=8) :: d(*),xl(ndm,*),ul(ndf,nen,*),s(nst,*),p(nst)

      save

!     Output descriptor

      if(isw.eq.0 .and. ior.lt.0) then

        write(*,*) '   Frame3d: 3-d Frame Elastic'
        return

      else

        if(isw.eq.1) then
          write(iow,2000)
          if(ior.lt.0) write(*,2000)
          call inmate(d,tdof,12,3)

          pstyp = 1

          if(ndf.lt.6.or.ndm.ne.3) then
            write(iow,2001) ndf,ndm
            call plstop(.true.)
          end if

!         Deactivate dof in element for dof > 6

          do i = 7,ndf
            ix(i) = 0
          end do

!         Check for reference node definition

          if(int(d(96)).lt.1 .or. int(d(96)).gt.2) then
            write(iow,2002) int(d(96))
            call plstop(.true.)
          endif

        endif

        if(d(18).gt.0.0d0) then
          call frams3d(d,ul,xl,ix,s,p,ndf,ndm,nst,isw)
        else
          write(*,*) ' *ERROR* No 3-d finite deformation frame'
          call plstop(.true.)
        endif

      endif

!     Format statements

2000  format(5x,'T h r e e   D i m e n s i o n a l   F r a m e'/)

2001  format(/5x,'Error in FRAME3D. Any of following values is wrong:'
     &       /6x,'ndf(should be 6) =',i3/6x,'ndm (should be 3) =',i3)

2002  format(/5x,'Error in FRAME3D. No reference vector definition for'
     &          ,' axes: lref =',i3)

      end subroutine frame3d

      subroutine frams3d(d,ul,xl,ix,s,r,ndf,ndm,nst,isw)

!-----[--.----+----.----+----.-----------------------------------------]
!     Small Deformation Three dimensional frame element

!     Control data:
!         ndm - 3 (x,y,z)
!         ndf - 6 (u,v,w, theta_x,theta_y,theta_z)
!         nen - 3 or more (see below)

!      Beam end nodes 1 and 2
!      Plane defined by nodes 1, 2, 3 contains z-axis
!                            (perpendicular to x-axis)
!      Vector products: e_1 = (x_2 - x_1)/|x_2 - x_1|
!                       e_2 = (x_2 - x_1) x ( x_3 - x_1)
!                       e_3 =  e_2 x e_1


!                  z (e_3)         x (e_1)
!                3 o - - - - - - /
!                  |           o 2
!                  |         /
!                  |       /
!                  |     /
!                  |   /
!                  | /
!                  / ------------- y (e_2)
!              1 o
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'bdata.h'
      include  'cdata.h'
      include  'eldata.h'
      include  'eltran.h'
      include  'iofile.h'
      include  'prld1.h'
      include  'prstrs.h'
      include  'refnd.h'

      include  'comblk.h'

      integer       :: ndf,ndm,nst,isw, i,i1,i2, j,j1,j2, k,l
      real (kind=8) :: le,dl,dl2,dl3,dn,ctan1,ctan3

      integer       :: ix(*)
      real (kind=8) :: d(*),xl(ndm,*),ul(ndf,nen,*),s(nst,*),r(ndf,*)
      real (kind=8) :: b(6),t(3,3),fi(6,2),sm(12,12),pm(12)

      save

      data      b / 6*0.0d0 /

!     Transfer to correct processor

      if((isw.ge.3 .and. isw.le.6) .or. isw.eq.8) then

!       Compute direction cosine terms and member length

        lref    = nint(d(96))
        refx(1) = d(97)
        refx(2) = d(98)
        refx(3) = d(99)

        t(1,1) = xl(1,2) - xl(1,1)
        t(1,2) = xl(2,2) - xl(2,1)
        t(1,3) = xl(3,2) - xl(3,1)
        le  = sqrt(t(1,1)*t(1,1)+t(1,2)*t(1,2)+t(1,3)*t(1,3))
        dl  = 1.0d0/le
        dl2 = dl*dl
        dl3 = dl*dl2
        t(1,1) = t(1,1)*dl
        t(1,2) = t(1,2)*dl
        t(1,3) = t(1,3)*dl
        if(nel.eq.2) then
          if    (lref.eq.1) then
            t(3,1) = refx(1) - xl(1,1)
            t(3,2) = refx(2) - xl(2,1)
            t(3,3) = refx(3) - xl(3,1)
          elseif(lref.eq.2) then
            t(3,1) = refx(1)
            t(3,2) = refx(2)
            t(3,3) = refx(3)
          else
            write(iow,3000)
            if(ior.lt.0) then
              write(*,3000)
            endif
            call plstop(.true.)
          endif
        else
          t(3,1) = xl(1,3) - xl(1,1)
          t(3,2) = xl(2,3) - xl(2,1)
          t(3,3) = xl(3,3) - xl(3,1)
        endif

        if(isw.eq.2) return

        t(2,1) = (t(3,2)*t(1,3) - t(3,3)*t(1,2))
        t(2,2) = (t(3,3)*t(1,1) - t(3,1)*t(1,3))
        t(2,3) = (t(3,1)*t(1,2) - t(3,2)*t(1,1))
        dn  = 1.0d0/sqrt(t(2,1)*t(2,1)+t(2,2)*t(2,2)+t(2,3)*t(2,3))
        t(2,1) = t(2,1)*dn
        t(2,2) = t(2,2)*dn
        t(2,3) = t(2,3)*dn
        t(3,1) = t(1,2)*t(2,3) - t(1,3)*t(2,2)
        t(3,2) = t(1,3)*t(2,1) - t(1,1)*t(2,3)
        t(3,3) = t(1,1)*t(2,2) - t(1,2)*t(2,1)

!       Compute stiffness/residual & output stress

        if(isw.ne.5) then
!         Compute axial stiffness terms

          i2       =  ndf + 1
          s( 1, 1) =  d(21)*d(32)*dl
          s(i2, 1) = -s(1,1)
          s( 1,i2) = -s(1,1)
          s(i2,i2) =  s(1,1)

!         Compute torsional stiffness terms

          i2       =  ndf + 4
          s( 4, 4) =  d(21)*d(36)*dl/(1.d0 + d(2))*0.5d0
          s(i2, 4) = -s(4,4)
          s( 4,i2) = -s(4,4)
          s(i2,i2) =  s(4,4)

!         Compute bending stiffness terms for z-displacements

          i1       = ndf + 3
          i2       = ndf + 5

          s( 3, 3) =  12.0d0*d(21)*d(33)*dl3
          s( 3,i1) = -s(3,3)
          s(i1, 3) = -s(3,3)
          s(i1,i1) =  s(3,3)

          s( 5,i2) = 2.d0*d(21)*d(33)*dl
          s( 5, 5) =  s(5,i2) + s(5,i2)
          s(i2, 5) =  s(5,i2)
          s(i2,i2) =  s(5,5)

          s( 3, 5) = -6.d0*d(21)*d(33)*dl2
          s( 5, 3) =  s(3,5)
          s( 3,i2) =  s(3,5)
          s(i2, 3) =  s(3,i2)
          s( 5,i1) = -s(3,5)
          s(i1, 5) =  s(5,i1)
          s(i1,i2) = -s(3,5)
          s(i2,i1) =  s(i1,i2)

!         Compute bending stiffness terms for y-displacement

          i1       = ndf + 2
          i2       = ndf + 6

          s( 2, 2) =  12.d0*d(21)*d(34)*dl3
          s( 2,i1) = -s(2,2)
          s(i1, 2) = -s(2,2)
          s(i1,i1) =  s(2,2)

          s( 6,i2) = 2.d0*d(21)*d(34)*dl
          s( 6, 6) =  s(6,i2) + s(6,i2)
          s(i2, 6) =  s(6,i2)
          s(i2,i2) =  s(6,6)

          s( 2, 6) =  6.d0*d(21)*d(34)*dl2
          s( 6, 2) =  s(2,6)
          s( 2,i2) =  s(2,6)
          s(i2, 2) =  s(2,i2)
          s( 6,i1) = -s(2,6)
          s(i1, 6) =  s(6,i1)
          s(i1,i2) = -s(2,6)
          s(i2,i1) =  s(i1,i2)

!         Compute bending stiffness terms for yz-displacement

          if(d(35).ne.0.0d0) then

            i1       = ndf + 3
            i2       = ndf + 5
            j1       = ndf + 2
            j2       = ndf + 6

            s( 2, 3) =  12.d0*d(21)*d(35)*dl3
            s( 3, 2) =  s(2,3)

            s( 2,i1) = -s(2,3)
            s(i1, 2) = -s(2,3)

            s(j1, 3) = -s(2,3)
            s( 3,j1) = -s(2,3)

            s(j1,i1) =  s(2,3)
            s(i1,j1) =  s(2,3)

            s( 6,i2) = 2.d0*d(21)*d(35)*dl
            s(i2, 6) =  s(6,i2)

            s( 6, 5) =  s(6,i2) + s(6,i2)
            s( 5, 6) =  s(6,5)

            s(j2, 5) =  s(6,i2)
            s( 5,j2) =  s(6,i2)

            s(j2,i2) =  s(6,5)
            s(i2,j2) =  s(6,5)

            s( 2, 5) =  6.d0*d(21)*d(35)*dl2
            s( 5, 2) =  s(2,5)

            s( 6, 3) =  s(2,5)
            s( 3, 6) =  s(2,5)

            s( 2,i2) =  s(2,5)
            s(i2, 2) =  s(2,5)

            s(j2, 3) =  s(2,i2)
            s( 3,j2) =  s(2,i2)

            s( 6,i1) = -s(2,5)
            s(i1, 6) = -s(2,5)

            s(j1, 5) =  s(6,i1)
            s( 5,j1) =  s(6,i1)

            s(j1,i2) = -s(2,5)
            s(i2,j1) =  s(j1,i2)

            s(i1,j2) = -s(2,5)
            s(j2,i1) =  s(j1,i2)

          endif

!         Transform to global coordinate displacements

          if(isw.eq.3 .or. isw.eq.6) then
            call tran13(s,t,nst,ndf)

!           Compute element residual

            do i = 1,6
              do j = 1,6
                r(i,1) = r(i,1)
     &             - s(i    ,j    )*(ul(j,1,1) + d(78)*ul(j,1,4))
     &             - s(i    ,j+ndf)*(ul(j,2,1) + d(78)*ul(j,2,4))
                r(i,2) = r(i,2)
     &             - s(i+ndf,j    )*(ul(j,1,1) + d(78)*ul(j,1,4))
     &             - s(i+ndf,j+ndf)*(ul(j,2,1) + d(78)*ul(j,2,4))
              end do ! j
            end do ! i

!           Modify tangent for transient factors

            ctan1 = ctan(1) + d(78)*ctan(2)
            do j = 1,nst
              do i = 1,nst
                s(i,j) = s(i,j)*ctan1
              end do ! i
            end do ! j

!           Inertial and body force effects

            dn = d(4)*d(32)*le*0.5d0

!           Set body loading factors

            if(int(d(74)).gt.0) then
              b(1) = 0.5*le*(d(11) + prldv(int(d(74)))*d(71))
            else
              b(1) = 0.5*le*d(11)*dm
            endif
            if(int(d(75)).gt.0) then
              b(2) = 0.5*le*(d(12) + prldv(int(d(75)))*d(72))
            else
              b(2) = 0.5*le*d(12)*dm
            endif
            if(int(d(76)).gt.0) then
              b(3) = 0.5*le*(d(13) + prldv(int(d(76)))*d(73))
            else
              b(3) = 0.5*le*d(13)*dm
            endif

!           Lumped mass computation

            ctan3 = ctan(3) + d(77)*ctan(2)
            if(d(7).eq.0.0d0) then

              do i = 1,3
                r(i,1)         = r(i,1)
     &                         - dn*(ul(i,1,5) + d(77)*ul(i,1,4))
     &                         + b(i)
                r(i,2)         = r(i,2)
     &                         - dn*(ul(i,2,5) + d(77)*ul(i,2,4))
     &                         + b(i)
                s(i    ,i    ) = s(i    ,i    ) + ctan3*dn
                s(i+ndf,i+ndf) = s(i+ndf,i+ndf) + ctan3*dn
              end do

!           Consistent mass computation

            else
              do i = 1,12
                do j = 1,12
                  sm(j,i) = 0.0d0
                end do ! i
                pm(i) = 0.0d0
              end do ! i
              call massf3(sm,pm,d(7),d,le,12,ndm,6)
              call tran13(sm,t,12,6)
              i1 = 0
              i2 = 0
              do i = 1,2
                do k = 1,6
                  r(k,i) = r(k,i) + b(k)
                  j1 = 0
                  j2 = 0
                  do j = 1,2
                    do l = 1,6
                      r(k,i)  = r(k,i)
     &                    - sm(k+i2,l+j2)*(ul(l,j,5) + d(77)*ul(l,j,4))
                      s(k+i1,l+j1) = s(k+i1,l+j1) + sm(k+i2,l+j2)*ctan3
                    end do
                    j1 = j1 + ndf
                    j2 = j2 + 6
                  end do
                end do
                i1 = i1 + ndf
                i2 = i2 + 6
              end do

            endif

          endif

!         Output member forces

          if(isw.eq.4 .or. isw.eq.8) then

!           Transform displacements

            do i = 1,ndf
              ul(i,1,2) = ul(i,1,1)
              ul(i,2,2) = ul(i,2,1)
            end do
            do i = 1,3
              ul(i  ,1,1) = t(i,1)*ul(1,1,2)
     &                    + t(i,2)*ul(2,1,2)
     &                    + t(i,3)*ul(3,1,2)
              ul(i+3,1,1) = t(i,1)*ul(4,1,2)
     &                    + t(i,2)*ul(5,1,2)
     &                    + t(i,3)*ul(6,1,2)
              ul(i  ,2,1) = t(i,1)*ul(1,2,2)
     &                    + t(i,2)*ul(2,2,2)
     &                    + t(i,3)*ul(3,2,2)
              ul(i+3,2,1) = t(i,1)*ul(4,2,2)
     &                    + t(i,2)*ul(5,2,2)
     &                    + t(i,3)*ul(6,2,2)
            end do ! i

!           Compute member force if necessary

            do i = 1,6
              r(i,1) = 0.0d0
              r(i,2) = 0.0d0
              do j = 1,6
                r(i,1) = r(i,1) - s(i    ,j    )*ul(j,1,1)
     &                          - s(i    ,j+ndf)*ul(j,2,1)
                r(i,2) = r(i,2) - s(i+ndf,j    )*ul(j,1,1)
     &                          - s(i+ndf,j+ndf)*ul(j,2,1)
              end do ! j
            end do ! i

            do i = 1,6
              fi(i,1) =  r(i,1)
              fi(i,2) = -r(i,2)
            end do

            if(isw.eq.4) then
              mct = mct - 1
              if(mct.le.0) then
                write(iow,2001) o,head
                if(ior.lt.0) then
                  write(*,2001) o,head
                endif
                mct = 50
              endif

              write(iow,2002) n_el,ma,(xl(i,1),i=1,3),(fi(i,1),i=1,6),
     &                             (xl(i,2),i=1,3),(fi(i,2),i=1,6)
              if(ior.lt.0) then
                write(*,2002) n_el,ma,(xl(i,1),i=1,3),(fi(i,1),i=1,6),
     &                             (xl(i,2),i=1,3),(fi(i,2),i=1,6)
              endif

!           Project stress resultants to nodes

            else

              call frcn3d(ix,fi,hr(nph),hr(nph+numnp),numnp)

            endif

          endif

!       Compute lumped  mass matrix

        elseif(isw.eq.5) then

          if(d(7).eq.0.0d0) then
            dn = d(4)*d(32)*le*0.5d0
            do i = 1,3
              r(i,1)         = dn
              r(i,2)         = dn
              s(i    ,i    ) = dn
              s(i+ndf,i+ndf) = dn
            end do

!         Compute consistent  mass matrix

          else
            call massf3(s,r,d(7),d,le,nst,ndm,ndf)
            call tran13(s,t,nst,ndf)
          endif
        endif ! isw.eq.5

      endif

!     Format statements

2001  format(a1,20a4//5x,'3-D Frame Element Forces'//
     &   '    Elmt  Mat     x-Coor     y-Coor     z-Coor'/
     & 7x,'I-end:      Force    1-Shear    2-Shear   1-Torque',
     &     '   1-Moment   2-Moment'/
     &   '                  x-Coor     y-Coor     z-Coor'/
     & 7x,'J-end:      Force    1-Shear    2-Shear   1-Torque',
     &     '   1-Moment   2-Moment'/1x,78('-'))

2002  format(i8,i5,1p,3e11.3/13x,1p,6e11.3/
     +       13x,  1p,3e11.3/13x,1p,6e11.3/1x)

3000  format('    *ERROR*  No Reference point for element')

      end subroutine frams3d

      subroutine tran13(s,t,nst,ndf)

      implicit  none

      integer       :: nst,ndf, i,j,j1,j2,k, nsiz

      real (kind=8) :: s(nst,nst),ss(6),t(3,3)

      save

!     Transform if not identity

      if(t(1,1)+t(2,2)+t(3,3).lt.2.999999d+00) then

!     Postmultiply local stiffness by transformation array

        j1 = 0
        nsiz = ndf + ndf
        do k = 1,2
          do i = 1,nsiz
            do j = 1,6
              ss(j) = s(i,j+j1)
            end do
            j2 = j1 + 3
            do j = 1,3
              s(i,j+j1) = ss(1)*t(1,j) + ss(2)*t(2,j) + ss(3)*t(3,j)
              s(i,j+j2) = ss(4)*t(1,j) + ss(5)*t(2,j) + ss(6)*t(3,j)
            end do
          end do
          j1 = j1 + ndf
        end do

!     Premultiply result by transpose of transformation array

        j1 = 0
        do k = 1,2
          do i = 1,nsiz
            do j = 1,6
              ss(j) = s(j+j1,i)
            end do
            j2 = j1 + 3
            do j = 1,3
              s(j+j1,i) = t(1,j)*ss(1) + t(2,j)*ss(2) + t(3,j)*ss(3)
              s(j+j2,i) = t(1,j)*ss(4) + t(2,j)*ss(5) + t(3,j)*ss(6)
            end do
          end do
          j1 = j1 + ndf
        end do

      endif

      end subroutine tran13

      subroutine frcn3d(ix,fi,dt,st,numnp)

      implicit  none

      integer       :: numnp
      integer       :: i,j,ll

      integer       :: ix(*)
      real (kind=8) :: dt(numnp),st(numnp,*),fi(6,*)

      save

      do i = 1,2

        ll = ix(i)
        if(ll.gt.0) then

          dt(ll) = dt(ll) + 1.d0

!         Stress projections

          do j = 1,6
            st(ll,j) = st(ll,j) + fi(j,i)
          end do
        endif
      end do

      end subroutine frcn3d

      subroutine massf3(s,r,cfac,d,le,nst,ndm,ndf)

!     Frame mass matrix

      implicit  none

      integer       :: nst,ndm,ndf,i,j,k,l, i1,j1, ll
      real (kind=8) :: cfac,lfac,ra,le,lr,t,dv,s1,s2,s3
      real (kind=8) :: d(*), r(ndf,*),s(nst,nst),sg(2,4),bb(2,2),db(2,2)
      real (kind=8) :: nt(2,6,2),nr(3,6,2),ntak(2,6),nrik(3,6),in(3,3)

      save

!     Set inertial properties

      ra       = d(4)*d(32)
      in(1,2)  = 0.0d0
      in(1,3)  = 0.0d0
      in(2,1)  = 0.0d0
      in(3,1)  = 0.0d0

!     Lumped mass matrix

      lr       = 1.d0/le
      t        = 0.5d0*ra*le
      do i = 1,ndm
        r(i,1) = t
        r(i,2) = t
      end do
      do i = 1,6
        do j = 1,2
          nt(1,i,j) = 0.0d0
          nt(2,i,j) = 0.0d0
          nr(1,i,j) = 0.0d0
          nr(2,i,j) = 0.0d0
          nr(3,i,j) = 0.0d0
        end do
      end do

!     Consistent mass matrix

      s(1    ,1    ) = 0.6666666666666667d0*t
      s(1    ,ndf+1) = 0.3333333333333333d0*t
      s(ndf+1,1    ) = s(1,ndf+1)
      s(ndf+1,ndf+1) = s(1,1)

      call int1d(4,sg)

      do ll = 1,4

        dv     = t*sg(2,ll)
        in(1,1)  = d(4)*d(36)*0.5d0*le*sg(2,ll)
        in(2,2)  = d(4)*d(33)*0.5d0*le*sg(2,ll)
        in(2,3)  = d(4)*d(35)*0.5d0*le*sg(2,ll)
        in(3,2)  = d(4)*d(35)*0.5d0*le*sg(2,ll)
        in(3,3)  = d(4)*d(34)*0.5d0*le*sg(2,ll)

        s1     = 0.5d0 + 0.5d0*sg(1,ll)
        s2     = s1*s1
        s3     = s1*s2

        bb(1,2)  = 3.d0*s2 - s3 - s3
        bb(2,2)  = le*(s3 - s2)
        bb(1,1)  = 1.d0 - bb(1,2)
        bb(2,1)  = le*(s1 - s2) + bb(2,2)

        db(1,2)  = 6.d0*(s1 - s2)*lr
        db(2,2)  = 3.d0*s2 - 2.d0*s1
        db(1,1)  = -db(1,2)
        db(2,1)  = 1.d0 -2.d0*s1 + db(2,2)

        nr(1,4,1) = 1.d0 - s1
        nr(1,4,2) = s1
        do k = 1,2

          nt(1,2,k) =  bb(1,k)
          nt(1,6,k) =  bb(2,k)
          nt(2,3,k) =  bb(1,k)
          nt(2,5,k) = -bb(2,k)

          nr(2,3,k) =  db(1,k)
          nr(2,5,k) = -db(2,k)
          nr(3,2,k) = -db(1,k)
          nr(3,6,k) = -db(2,k)

        end do
        i1 = 0
        do k = 1,2
          ntak(1,2) = dv*nt(1,2,k)
          ntak(1,6) = dv*nt(1,6,k)
          ntak(2,3) = dv*nt(2,3,k)
          ntak(2,5) = dv*nt(2,5,k)
          do i = 1,6
            do j = 1,3
              nrik(j,i) = nr(1,i,k)*in(1,j)
     &                  + nr(2,i,k)*in(2,j)
     &                  + nr(3,i,k)*in(3,j)
            end do ! j
          end do ! i

          j1 = 0
          do l = 1,2

            do i = 1,6
              do j = 1,6
                s(i+i1,j+j1) = s(i+i1,j+j1)
     &                       + ntak(1,i)*nt(1,j,l)
     &                       + ntak(2,i)*nt(2,j,l)
     &                       + nrik(1,i)*nr(1,j,l)
     &                       + nrik(2,i)*nr(2,j,l)
     &                       + nrik(3,i)*nr(3,j,l)
              end do ! j
            end do ! i
            j1 = j1 + ndf
          end do ! l
          i1 = i1 + ndf
        end do ! k
      end do ! ll

!     Interpolate mass between lumped and consistent
!     Consistent Mass: cfac = 1.0 ; Lumped Mass: cfac = 0.0

      lfac = 1.d0 - cfac
      do i = 1,nst
        do j = 1,nst
          s(i,j) = cfac*s(i,j)
        end do
      end do
      do i = 1,ndm
        s(i    ,i    ) = s(i    ,i    ) + lfac*r(i,1)
        s(i+ndf,i+ndf) = s(i+ndf,i+ndf) + lfac*r(i,2)
      end do

      end subroutine massf3
