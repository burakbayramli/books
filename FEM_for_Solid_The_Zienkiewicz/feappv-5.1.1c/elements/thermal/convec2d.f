!$Id:$
      subroutine convec2d(d,ul,xl,ix,s,r,ndf,ndm,nst,isw)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!     Two dimensional (plane/axisymmetric) thermal surface element

!     N.B. Surface loading for solutions using THERMAL (2d) Element

!-----[--.----+----.----+----.-----------------------------------------]
!     1. Parameters input by this routine when isw = 1 (i.e., mate)

!        Record 1. (H,T_0,kat)

!           H   - Surface parameter
!           T_o - Equilibrium temperature
!           qn  - Normal flux to boundary
!           nn  - Exponent to convection/radiation b.c

!                 flux = qn + H*(T^n - T_o^n)
!                 nn = 1 - convection b.c.
!                 nn = 4 - Stefan-Boltzman radiation b.c.

!           kat - geometry type
!                 1 = plane
!                 2 = geometry
!           N.B. If kat is not = 2 it is set to 1.
!-----[--.----+----.----+----.-----------------------------------------]

!     2. Control parameters

!        This is a two dimensional element which can analyze plane
!        or axisymmetric geometries.  Set control parameters as
!        follows:

!           ndm - set to 2     (x,y or r,z-coords)
!           ndf - set > or = 1 (nodal temperatures)
!           nel - set > or = 2

!....  OUTPUT variables

!        r(1,nel)     Contribution to residual

!....  PARAMATER set up:

!         kat  =  1  (for plane analysis, constant flux)

!              =  2  (for axisymmetric analysis, constant flux)

!         nel  =  2  (for 2-node edge of bi-linear element)

!                                                         ^
!                           ^^^                n (normal) |
!                           |||  + q_n                    |
!                           |||                   <-------+
!         Nodes are:     o----------------o           t (tangent)
!                        2                1
!                        |  interior      |
!                        |     of mesh    |

!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'eldata.h'
      include  'eltran.h'
      include  'iofile.h'

      character (len=15) :: tx
      character (len=6)  :: wlab(2)

      logical       :: errck, tinput, pcomp

      integer       :: ndm,ndf,nst, isw
      integer       :: i,j, i1,j1, kat, l, lint, nn
      real (kind=8) :: da, dx, dy, hh, rr, qq, tt, uu

      integer       :: ix(*)
      real (kind=8) :: d(*), xl(ndm,*),ul(ndf,*), r(ndf,*), s(nst,*)
      real (kind=8) :: shp1(2,2), xi(2),wg(2)

      save

      data wlab/'Plane ','Axisym'/

!     Output element type

      if(isw.eq.0) then
        if(ior.lt.0) then
          write(*,*) '   2-d Plane/Axisym Surface Thermal Flux'
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
        kat = nint(d(5))
        if(kat.ne.2) kat = 1
        d(5) = kat
        if(ior.lt.0) then
          write(*,2000) d(1),d(2),d(3),nn,wlab(kat)
        end if
        write(iow,2000) d(1),d(2),d(3),nn,wlab(kat)

!       Set quadrature to 2-point

        lint  =  2
        xi(1) =  1.0d0/sqrt(3.0d0)
        xi(2) = -xi(1)
        wg(1) =  1.0d0
        wg(2) =  1.0d0

        do i = 2,ndf
          ix(i) = 0
        end do

!       Bloc k plotting

        pstyp = 0

!     Compute conductivity (stiffness) matrix

      elseif(isw.eq.3 .or. isw.eq.6) then

        nn    = nint(d(4))
        kat   = nint(d(5))

!       Loop over quadrature points

        do l = 1,lint ! {

!         Compute geometric factors

          call shap1d (xi(l), nel, shp1)
          dx = 0.0d0
          dy = 0.0d0
          rr = 0.0d0
          uu = 0.0d0
          do i = 1,nel ! {
            dx = dx + shp1(1,i)*xl(1,i)
            dy = dy + shp1(1,i)*xl(2,i)
            rr = rr + shp1(2,i)*xl(1,i)
            uu = uu + shp1(2,i)*ul(1,i)
          end do ! i   }

!         Compute surface area ( no radius effect yet)

          da = sqrt(dx**2 + dy**2)*wg(l)

!         Axisymmetric problems: Modify terms for radius

          if(kat.eq.2) then
            da = da*rr
          endif

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
            r(1,i) = r(1,i) - qq*shp1(2,i)

!           Compute tangent matrix for surface convections

            hh = tt*shp1(2,i)
            j1 = 1
            do j = 1,nel ! {
              s(i1,j1) = s(i1,j1) + hh*shp1(2,j)
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
     &      10x,'Temperature exp. n',i7/
     &      10x,a,' Analysis')

3000  format(' Input: SURFace , H, T_0,
     &                FLUX    , q_n,
     &           or   EXPOnent, n'/
     &       '  1>',$)

      end subroutine convec2d
