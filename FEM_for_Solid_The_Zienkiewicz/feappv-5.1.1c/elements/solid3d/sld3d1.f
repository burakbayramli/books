!$Id:$
      subroutine sld3d1(d,ul,xl,tl,s,r,ndf,ndm,nst,isw)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: 3-D linear elastic displacment element for feap

!     Output records:

!     Prints in element: sig-11, sig-22, sig-33, sig-12, sig-23, sig-31
!                        eps-11, eps-22, eps-33, eps-12, eps-23, eps-31
!                         1-sig,  2-sig,  3-sig

!     Prints at nodes:   1=sig-11, 2=sig-22, 3=sig-33,
!                        4=sig-12  5=sig-23, 6=sig-31
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'bdata.h'
      include  'cdata.h'
      include  'debugs.h'
      include  'elcoor.h'
      include  'eldata.h'
      include  'elplot.h'
      include  'eltran.h'
      include  'hdata.h'
      include  'iofile.h'
      include  'prstrs.h'
      include  'ptdat6.h'
      include  'qudshp.h'
      include  'rdata.h'

      include  'comblk.h'

      integer       :: i,j,l,nn,i1,j1,ndf,ndm,nst,isw,nhv,istrt
      integer       :: tdof
      real (kind=8) :: sig(10,125),eps(9,3),dd(6,6,5),th(125)
      real (kind=8) :: d(*),xl(ndm,*),ul(ndf,nen,*),tl(*),s(nst,*),r(*)
      real (kind=8) :: psig(3),epp(6),peps(3)
      real (kind=8) :: xn, yn, zn, ta, lfac, cfac
      real (kind=8) :: a11, a12, a13, a14, a15, a16
      real (kind=8) :: a21, a22, a23, a24, a25, a26
      real (kind=8) :: a31, a32, a33, a34, a35, a36

      save

!     Input modifications

      if(isw.eq.1) then
        return
      endif

!     Set nodal temperatures: Can be specified or computed

      if(isw.gt.1) then
        tdof = nint(d(19))
        if(tdof.le.0) then
          do i = 1,nel ! {
            th(i) = tl(i)
          end do ! i     }
        else
          do i = 1,nel ! {
            th(i) = ul(tdof,i,1)
          end do ! i     }
        endif
      endif

!     Compute element tangent array

      nhv   = nint(d(15))
      istrt = nint(d(84))

!     Get quadrature information

      call quadr3d(d,.true.)

!     Compute shape functions

      do l = 1,lint
        call interp3d(l, xl, ndm,nel)
      end do ! l

!     Compute the residual and tangent arrays or energy

      if(isw.eq.3 .or. isw.eq.6 .or. isw.eq.14) then

!       Compute intertial effects

        if(ctan(3).ne.0.0) then
          cfac = d(7)
          lfac = 1.0d0 - cfac
        else
          cfac = 0.0d0
          lfac = 0.0d0
        endif

!       Compute residual and tangent arrays

        nn = 0
        do l = 1,lint

!         Compute strain at point

          call strn3d(d,ul,th,shp3(1,1,l),ndf,nel, eps,ta)

!         Compute stress at point

          call modlsd(l,d,ta,eps,hr(nh1+nn),hr(nh2+nn),nhv,istrt,
     &                dd,sig(1,l),isw)

!         Residual and tangent computation

          if(isw.eq.3 .or. isw.eq.6) then

!           Add stiffness part of Rayleigh damping to stress

            if(d(78).ne.0.0d0) then
              call rays3d(d,shp3(1,1,l),shp3(1,1,l),sig(1,l),dd,
     &                    ul(1,1,4),ndf,nel,.false.)
            endif

!           Form residual

            call resid3d(jac(l),shp3(1,1,l),sig(1,l),d,
     &                   ul(1,1,4),ul(1,1,5),r,ndf, l)

!           Stiffness computations

            if(isw.eq.3) then

!             Modify tangent for stiffness Rayleigh damping
              dd(:,:,1) = dd(:,:,1)*(ctan(1) + d(78)*ctan(2))

              i1 = 1
              do i = 1,nel

!               Compute d * b matrix = a

                xn  = shp3(1,i,l)*jac(l)
                yn  = shp3(2,i,l)*jac(l)
                zn  = shp3(3,i,l)*jac(l)

                a11 = xn*dd(1,1,1) + yn*dd(4,1,1) + zn*dd(6,1,1)
                a12 = xn*dd(1,2,1) + yn*dd(4,2,1) + zn*dd(6,2,1)
                a13 = xn*dd(1,3,1) + yn*dd(4,3,1) + zn*dd(6,3,1)
                a14 = xn*dd(1,4,1) + yn*dd(4,4,1) + zn*dd(6,4,1)
                a15 = xn*dd(1,5,1) + yn*dd(4,5,1) + zn*dd(6,5,1)
                a16 = xn*dd(1,6,1) + yn*dd(4,6,1) + zn*dd(6,6,1)

                a21 = xn*dd(4,1,1) + yn*dd(2,1,1) + zn*dd(5,1,1)
                a22 = xn*dd(4,2,1) + yn*dd(2,2,1) + zn*dd(5,2,1)
                a23 = xn*dd(4,3,1) + yn*dd(2,3,1) + zn*dd(5,3,1)
                a24 = xn*dd(4,4,1) + yn*dd(2,4,1) + zn*dd(5,4,1)
                a25 = xn*dd(4,5,1) + yn*dd(2,5,1) + zn*dd(5,5,1)
                a26 = xn*dd(4,6,1) + yn*dd(2,6,1) + zn*dd(5,6,1)

                a31 = xn*dd(6,1,1) + yn*dd(5,1,1) + zn*dd(3,1,1)
                a32 = xn*dd(6,2,1) + yn*dd(5,2,1) + zn*dd(3,2,1)
                a33 = xn*dd(6,3,1) + yn*dd(5,3,1) + zn*dd(3,3,1)
                a34 = xn*dd(6,4,1) + yn*dd(5,4,1) + zn*dd(3,4,1)
                a35 = xn*dd(6,5,1) + yn*dd(5,5,1) + zn*dd(3,5,1)
                a36 = xn*dd(6,6,1) + yn*dd(5,6,1) + zn*dd(3,6,1)

                j1 = 1
                do j = 1,nel

!                 Compute stiffness matrix

                  xn = shp3(1,j,l)
                  yn = shp3(2,j,l)
                  zn = shp3(3,j,l)

                  s(i1  ,j1  ) = s(i1  ,j1  ) + a11*xn + a14*yn + a16*zn
                  s(i1  ,j1+1) = s(i1  ,j1+1) + a14*xn + a12*yn + a15*zn
                  s(i1  ,j1+2) = s(i1  ,j1+2) + a16*xn + a15*yn + a13*zn

                  s(i1+1,j1  ) = s(i1+1,j1  ) + a21*xn + a24*yn + a26*zn
                  s(i1+1,j1+1) = s(i1+1,j1+1) + a24*xn + a22*yn + a25*zn
                  s(i1+1,j1+2) = s(i1+1,j1+2) + a26*xn + a25*yn + a23*zn

                  s(i1+2,j1  ) = s(i1+2,j1  ) + a31*xn + a34*yn + a36*zn
                  s(i1+2,j1+1) = s(i1+2,j1+1) + a34*xn + a32*yn + a35*zn
                  s(i1+2,j1+2) = s(i1+2,j1+2) + a36*xn + a35*yn + a33*zn

                  j1 = j1 + ndf
                end do ! j
                i1 = i1 + ndf
              end do ! i
            endif

          endif ! isw
          nn = nn + nhv
        end do ! l

!       Compute intertial effects

        if(ctan(3).ne.0.0) then
          call iner3d(d,xl,ul(1,1,4),ul(1,1,5),s,r,
     &                nel,ndf,ndm,nst)
        endif

!     Compute and output element variables

      elseif(isw.eq.4 .or. isw.eq.8) then

!       Set initial counter for history terms in stress/strain

        nn = 0
        do l = 1,lint

!         Compute strain at point

          call strn3d(d,ul,th,shp3(1,1,l),ndf,nel, eps,ta)

!         Compute stress at point

          call modlsd(l,d,ta,eps,hr(nh1+nn),hr(nh2+nn),nhv,istrt,
     &                dd,sig(1,l),isw)

!         Output values

          if(isw.eq.4) then

!           Compute coordinates

            xn = 0.0
            yn = 0.0
            zn = 0.0
            do j = 1,nel
              xn = xn + shp3(4,j,l)*xl(1,j)
              yn = yn + shp3(4,j,l)*xl(2,j)
              zn = zn + shp3(4,j,l)*xl(3,j)
            end do ! j

!           Compute principal stress values

            do i = 1,3
              epp(i  ) = eps(i  ,1)
              epp(i+3) = eps(i+3,1)*0.5d0
            end do ! i
            call pstr3d(sig(1,l),psig)
            call pstr3d(epp     ,peps)

            mct = mct - 5
            if(mct.le.0) then
              write(iow,2010) o,head
              if(ior.lt.0) write(*,2010) o,head
              mct = 50
            endif
            write(iow,2011) n_el,ma,xn,yn,zn,
     &                      (sig(i,l),i=1,6),(epp(i),i=1,6),
     &                      (psig(i),i=1,3),(peps(i),i=1,3)
            if(ior.lt.0) then
              write(*,2011) n_el,ma,xn,yn,zn,
     &                      (sig(i,l),i=1,6),(epp(i),i=1,6),
     &                      (psig(i),i=1,3),(peps(i),i=1,3)
            endif

          endif
          nn = nn + nhv
        end do ! l

!       Plot stress values

        if(isw.eq.8) then
          call slcn3d(sig,r,s,nel,10)
        endif
      endif

!     Formats

2010  format(a1,20a4//5x,'Element Stresses'//'     Elmt Mat',
     &   '    1-coord    2-coord    3-coord'/12x,
     &   '  11-stress  22-stress  33-stress  12-stress',
     &   '  23-stress  31-stress'/12x,
     &   '  11-strain  22-strain  33-strain  12-strain',
     &   '  23-strain  31-strain'/12x,
     &   '   1-stress   2-stress   3-stress',
     &   '   1-strain   2-strain   3-strain',/39(' -'))

2011  format(/i9,i4,1p,3e11.3/(12x,1p,6e11.3))

      end subroutine sld3d1
