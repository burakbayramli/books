!$Id:$
      subroutine sld3d1(d,ul,xl,ix,tl,s,p,ndf,ndm,nst,isw)

!_____________________________________________________________________c
!     3-D linear elastic displacment element for feap

!     Output records:

!     Prints in element: sig-11, sig-22, sig-33, sig-12, sig-23, sig-31
!                        eps-11, eps-22, eps-33, eps-12, eps-23, eps-31

!     Prints at nodes:   1=sig-11, 2=sig-22, 3=sig-33,
!                        4=sig-12  5=sig-23, 6=sig-31

!_____________________________________________________________________c

      implicit  none

      include  'bdata.h'
      include  'cdata.h'
      include  'eldata.h'
      include  'eltran.h'
      include  'hdata.h'
      include  'iofile.h'
      include  'prld1.h'
      include  'prstrs.h'
      include  'rdata.h'
      include  'comblk.h'

      integer   i,j,l,nn, i1,j1
      integer   ndf,ndm,nst,isw
      integer   lint,nhv,tdof, istrt

      real*8    dv, dvm, xn, yn, zn, ta, rr, lfac, cfac, am, mass
      real*8    a11, a12, a13, a21, a22, a23, a31, a32, a33
      real*8    a41, a42, a43, a51, a52, a53, a61, a62, a63
      real*8    b1 , b2 , b3
      real*8    shp(4,8,8),sg(4,8),sig(8),eps(9,3),dd(6,6,5), xsj(8)
      real*8    th(8),sv(5,4)

      integer   ix(*)
      real*8    d(*),xl(ndm,*),ul(ndf,nen,*),tl(*),s(nst,*),p(nst)

      save

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

!     Transfer to correct processor

      go to (1,1,3,3,1,3,1,3,1,1,1,3), isw

!     Return

1     return

!     Compute element tangent array

3     l = 2

!     Set number of history terms

      nhv   = nint(d(15))
      istrt = nint(d(84))

      if(mod(isw,3).eq.0) then

!       Set body loading factors

        if(int(d(74)).gt.0) then
          b1 = d(11) + prldv(int(d(74)))*d(71)
        else
          b1 = d(11)*dm
        endif
        if(int(d(75)).gt.0) then
          b2 = d(12) + prldv(int(d(75)))*d(72)
        else
          b2 = d(12)*dm
        endif
        if(int(d(76)).gt.0) then
          b3 = d(13) + prldv(int(d(76)))*d(73)
        else
          b3 = d(13)*dm
        endif

!       Set mass factors

        rr   = d(4)
        if(d(7).ge.0.0d0) then
          cfac = d(7)
          lfac = 1.d0 - cfac
        else
          cfac = 0.0d0
          lfac = 0.0d0
        endif

!       Get quadrature information

        if(nel.eq.4) then
          call tint3d(l,lint,sv)
        else
          call int3d(l,lint,sg)
        endif

        nn = 0
        do l = 1,lint
          if(nel.eq.4) then
            call tetshp(sv(1,l),xl,ndm,xsj(l),shp(1,1,l))
            dv = xsj(l)*sv(5,l)
          else
            call shp3d(sg(1,l),xsj(l),shp(1,1,l),xl,ndm)
            dv = xsj(l)*sg(4,l)
          endif

!         Compute strain at point

          call strn3d(d,ul,th,shp(1,1,l),ndf,nel, eps,ta)

!         Compute stress at point

          call modlsd(l,d,ta,eps,hr(nh1+nn),hr(nh2+nn),nhv,istrt,
     &                dd,sig, isw)

!         Residual computations

          if(isw.eq.3 .or.isw.eq.6) then

!           Add stiffness part of Rayleigh damping to stress

            if(d(78).ne.0.0d0) then
              call rays3d(d,shp(1,1,l),shp(1,1,l),sig,dd,ul(1,1,4),
     &                    ndf,nel,.false.)
            endif

!           Form residual

            call resid3d(dv,shp(1,1,l),sig,d,
     &                   ul(1,1,4),ul(1,1,5),p,ndf,l)
          endif

!         Stiffness computations

          if(isw.eq.3) then

            dvm   = rr*(ctan(3) + d(77)*ctan(2))*dv
            dv    =    (ctan(1) + d(78)*ctan(2))*dv

            j1 = 1
            do j = 1,nel

!             Compute d * b matrix = a

              xn  = shp(1,j,l)*dv
              yn  = shp(2,j,l)*dv
              zn  = shp(3,j,l)*dv
              a11 = dd(1,1,1)*xn + dd(1,4,1)*yn + dd(1,6,1)*zn
              a21 = dd(2,1,1)*xn + dd(2,4,1)*yn + dd(2,6,1)*zn
              a31 = dd(3,1,1)*xn + dd(3,4,1)*yn + dd(3,6,1)*zn
              a41 = dd(4,1,1)*xn + dd(4,4,1)*yn + dd(4,6,1)*zn
              a51 = dd(5,1,1)*xn + dd(5,4,1)*yn + dd(5,6,1)*zn
              a61 = dd(6,1,1)*xn + dd(6,4,1)*yn + dd(6,6,1)*zn
              a12 = dd(1,2,1)*yn + dd(1,4,1)*xn + dd(1,5,1)*zn
              a22 = dd(2,2,1)*yn + dd(2,4,1)*xn + dd(2,5,1)*zn
              a32 = dd(3,2,1)*yn + dd(3,4,1)*xn + dd(3,5,1)*zn
              a42 = dd(4,2,1)*yn + dd(4,4,1)*xn + dd(4,5,1)*zn
              a52 = dd(5,2,1)*yn + dd(5,4,1)*xn + dd(5,5,1)*zn
              a62 = dd(6,2,1)*yn + dd(6,4,1)*xn + dd(6,5,1)*zn
              a13 = dd(1,3,1)*zn + dd(1,5,1)*yn + dd(1,6,1)*xn
              a23 = dd(2,3,1)*zn + dd(2,5,1)*yn + dd(2,6,1)*xn
              a33 = dd(3,3,1)*zn + dd(3,5,1)*yn + dd(3,6,1)*xn
              a43 = dd(4,3,1)*zn + dd(4,5,1)*yn + dd(4,6,1)*xn
              a53 = dd(5,3,1)*zn + dd(5,5,1)*yn + dd(5,6,1)*xn
              a63 = dd(6,3,1)*zn + dd(6,5,1)*yn + dd(6,6,1)*xn

!             Add diagonal mass effects

              am           = shp(4,j,l)*dvm
              s(j1  ,j1  ) = s(j1  ,j1  ) + am*lfac
              s(j1+1,j1+1) = s(j1+1,j1+1) + am*lfac
              s(j1+2,j1+2) = s(j1+2,j1+2) + am*lfac

              if(isw.eq.3) then
                i1 = 1
                do i = 1,j

!                 Compute consistent mass matrix

                  xn   = shp(1,i,l)
                  yn   = shp(2,i,l)
                  zn   = shp(3,i,l)
                  mass = shp(4,i,l)*am*cfac

                  s(i1  ,j1  ) = s(i1  ,j1  ) + xn*a11 + yn*a41 + zn*a61
     &                                        + mass
                  s(i1  ,j1+1) = s(i1  ,j1+1) + xn*a12 + yn*a42 + zn*a62
                  s(i1  ,j1+2) = s(i1  ,j1+2) + xn*a13 + yn*a43 + zn*a63
                  s(i1+1,j1  ) = s(i1+1,j1  ) + yn*a21 + xn*a41 + zn*a51
                  s(i1+1,j1+1) = s(i1+1,j1+1) + yn*a22 + xn*a42 + zn*a52
     &                                        + mass
                  s(i1+1,j1+2) = s(i1+1,j1+2) + yn*a23 + xn*a43 + zn*a53
                  s(i1+2,j1  ) = s(i1+2,j1  ) + zn*a31 + yn*a51 + xn*a61
                  s(i1+2,j1+1) = s(i1+2,j1+1) + zn*a32 + yn*a52 + xn*a62
                  s(i1+2,j1+2) = s(i1+2,j1+2) + zn*a33 + yn*a53 + xn*a63
     &                                        + mass
                  i1 = i1 + ndf
                end do
              endif
              j1 = j1 + ndf
            end do
          endif
          nn = nn + nhv
        end do

!       Construct symmetric part

        if(isw.eq.3) then
          do i = 2,ndf*nel
            do j = 1,i
              s(i,j) = s(j,i)
            end do
          end do
        endif
      endif

!     Compute and output element variables

      if(isw.eq.4) then

        if(nel.eq.4) then
          call tint3d(l,lint,sv)
        else
          call int3d(l,lint,sg)
        endif

!       Set initial counter for history terms in stress/strain

        nn = 0
        do l = 1,lint
          if(nel.eq.4) then
            call tetshp(sv(1,l),xl,ndm,xsj(l),shp)
          else
            call shp3d(sg(1,l),xsj(l),shp,xl,ndm)
          endif

!         Compute strain at point

          call strn3d(d,ul,th,shp,ndf,nel, eps,ta)

!         Compute stress at point

          call modlsd(l,d,ta,eps,hr(nh1+nn),hr(nh2+nn),nhv,istrt,
     &                dd,sig, isw)

!         Compute coordinates

          xn = 0.0
          yn = 0.0
          zn = 0.0
          do j = 1,nel
            xn = xn + shp(4,j,1)*xl(1,j)
            yn = yn + shp(4,j,1)*xl(2,j)
            zn = zn + shp(4,j,1)*xl(3,j)
          end do

!         Compute principal stress values

          mct = mct - 3
          if(mct.le.0) then
            write(iow,2010) o,head
            if(ior.lt.0) write(*,2010) o,head
            mct = 50
          endif
          write(iow,2011) n_el,xn,(sig(i),i=1,6),ma,yn,(eps(i,1),i=1,6)
          if(ior.lt.0) then
            write(*,2011) n_el,xn,(sig(i),i=1,6),ma,yn,(eps(i,1),i=1,6)
          end if
          nn = nn + nhv
        end do

!     Plot stress values

      elseif(isw.eq.8) then
        call stcn3d(ix(1),d,xl,th,ul,shp,hr(nph),hr(nph+numnp),
     &              ndf,ndm,nel,numnp,nhv,istrt)
      endif
      return

!     Formats

2010  format(a1,20a4//5x,'Element Stresses'//' Elmt 1-coord',
     &    2x,'11-stress  22-stress  33-stress  12-stress',
     &    2x,'23-stress  31-stress'/' matl 2-coord  11-strain',
     &    2x,'22-strain  33-strain  12-strain  23-strain',
     &    2x,'31-strain'/39(' -'))

2011  format(i4,0p1f9.3,1p6e11.3/i4,0p1f9.3,1p6e11.3/)

      end
