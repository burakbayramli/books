!$Id:$
      subroutine convec3d(d,ul,xl,ix,s,r,ndf,ndm,nst,isw)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!     Three dimensional thermal surface element

!     N.B. Surface loading for solutions using THERMAL (3d) Element

!-----[--.----+----.----+----.-----------------------------------------]
!     1. Parameters input by this routine when isw = 1 (i.e., mate)

!        Record 1. (H,T_0,qn,nn)

!           H   - Surface parameter
!           T_o - Equilibrium temperature
!           qn  - Normal flux to boundary
!           nn  - Exponent to convection/radiation b.c

!                 flux = qn + H*(T^n - T_o^n)
!                 nn = 1 - convection b.c.
!                 nn = 4 - Stefan-Boltzman radiation b.c.

!-----[--.----+----.----+----.-----------------------------------------]

!     2. Control parameters

!        This is a two dimensional element which can analyze plane
!        or axisymmetric geometries.  Set control parameters as
!        follows:

!           ndm - set to 3     (x,y,z-coords)
!           ndf - set > or = 1 (nodal temperatures)
!           nel - set > or = 4

!....  OUTPUT variables

!        r(1,nel)     Contribution to residual

!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'eldata.h'
      include  'eltran.h'
      include  'iofile.h'
      include  'prstrs.h'

      character (len=15) :: tx

      logical       :: errck, tinput, pcomp

      integer       :: ndm,ndf,nst, isw
      integer       :: i,j, i1,j1, l, lint, nn
      real (kind=8) :: da, xsj, hh, qq, tt, uu

      integer       :: ix(*)
      real (kind=8) :: d(*), xl(ndm,*),ul(ndf,*), r(ndf,*), s(nst,*)
      real (kind=8) :: shp2(3,9), xi(3,9)

      save

!     Output element type

      if(isw.eq.0) then
        if(ior.lt.0) then
          write(*,*) '    3-d Thermal Thermal Flux'
        endif

!     Input material properties

      elseif(isw.eq.1) then

    1   if(ior.lt.0) write(*,3000)
        errck = tinput(tx,1,d(11),4)
        if(errck) go to 1

        if    (pcomp(tx,'surf',4)) then
           d(1) = d(11)
           d(2) = d(12)
        elseif(pcomp(tx,'flux',4)) then
           d(3) = d(11)
        elseif(pcomp(tx,'expo',4)) then
           nn  = max(1,int(d(11)))
        elseif(pcomp(tx,'    ',4)) then
           go to 11
        endif
        go to 1

!       Set final parameters

   11   nn  = max(1,nn)
        d(4)= nn
        if(ior.lt.0) then
          write(*,2000) d(1),d(2),d(3),nn
        end if
        write(iow,2000) d(1),d(2),d(3),nn

        do i = 2,ndf
          ix(i) = 0
        end do

!       No plot of convection mesh

        pstyp = 0

!     Compute conductivity (stiffness) matrix

      elseif(isw.eq.3 .or. isw.eq.6) then

!       Set quadrature to 2-point

        if(nel.le.4) then
          l = 2
        else
          l = 3
        endif

        call int2d(l,lint,xi)

        nn    = nint(d(4))

!       Loop over quadrature points

        do l = 1,lint ! {

!         Compute geometric factors

          call shp2d (xi(1,l),xl,shp2,xsj,ndm,nel,ix,.true.)
          uu = 0.0d0
          do i = 1,nel ! {
            uu = uu + shp2(3,i)*ul(1,i)
          end do ! i   }

          da = xsj*xi(3,l)

!         Thermal properties and loads for flux on face point

          if(nn.eq.1) then
            qq = ( d(3) + d(1)*( uu - d(2) ) )*da
            tt = d(1)*da*ctan(1)
          else
            qq = ( d(3) + d(1)*( uu**nn - d(2)**nn ) )*da
            tt = d(1)*dble(nn)*uu**(nn-1)*da*ctan(1)
          endif

          i1 = 1
          do i = 1,nel ! {
            r(1,i) = r(1,i) - qq*shp2(3,i)

!           Compute stiffness for surface convections

            hh = tt*shp2(3,i)
            j1 = 1
            do j = 1,nel ! {
              s(i1,j1) = s(i1,j1) + hh*shp2(3,j)
              j1 = j1 + ndf
            end do ! j   }
            i1 = i1 + ndf
          end do ! i   }

        end do ! l   }

      endif

!     Formats

!-----[--.----+----.----+----.-----------------------------------------]

2000  format(5x,'Three Dimensional Heat Conduction Boundary Element'//
     &      10x,'Surface Parameter ',1p,e12.5/
     &      10x,'Equilibrium Temp. ',1p,e12.5/
     &      10x,'Boundary flux     ',1p,e12.5/
     &      10x,'Temperature exp. n',i7)

3000  format(' Input: SURFace , H, T_0,
     &                FLUX    , q_n,
     &           or   EXPOnent, n'/
     &       '  1>',$)

      end subroutine convec3d
