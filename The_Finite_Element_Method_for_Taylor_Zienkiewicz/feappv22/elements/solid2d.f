c$Id:$
      subroutine solid2d(d,ul,xl,ix,tl,s,p,ndf,ndm,nst,isw)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c     Plane and axisymmetric linear thermo-elastic element routine

c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'cdata.h'
      include  'eldata.h'
      include  'evdata.h'
      include  'hdata.h'
      include  'iofile.h'
      include  'pmod2d.h'
      include  'strnum.h'
      include  'comblk.h'

      integer   ndf,ndm,nst,isw
      integer   i

      integer   ix(*)
      real*8    d(*),ul(ndf,16),xl(ndm,*),tl(*),s(nst,nst),p(nst)
      real*8    shp(3,9)

      save

c     Extract type data

      stype = d(16)
      etype = d(17)
      dtype = d(18)
      hflag = d(30).eq.1.0d0

c     Input material properties

      if(isw.eq.1) then
        write(iow,2001)
        if(ior.lt.0) write(*,2001)
        call inmate(d,i,2*nen,1)
        if(etype.eq.2) then
          nh1 = nh1 + 2
        endif

c       Deactivate dof in element for dof > 2

        do i = 3,ndf
          ix(i) = 0
        end do ! i

c       Set plot sequence for triangles

        if(nen.eq.3) then
          call pltri3(iel)
        elseif(nen.eq.6 .or. nen.eq.7) then
          call pltri6(iel)
        endif

c       Set number of projected stresses

        istv = 9

c     Check element for errors in input data

      elseif(isw.eq.2) then
        if(nel.eq.3. .or. nel.eq.6 .or. nel.eq.7) then
          call cktris(ix,xl,shp,ndm)
        else
          call ckisop(ix,xl,shp,ndm)
        endif

c     Compute residuals and tangents for parts

      elseif(isw.eq.3 .or. isw.eq.4 .or. isw.eq.6 .or. isw.eq.8) then

c       Displacement Model

        if(etype.eq.1) then

          if(dtype.gt.0) then
            call sld2d1(d,ul,xl,ix,tl,s,p,ndf,ndm,nst,isw)
          else
            call fld2d1(d,ul,xl,ix,s,p,ndf,ndm,nst,isw)
          endif

c       Mixed Model (B-Bar)

        elseif(etype.eq.2) then

          if(dtype.gt.0) then
            call sld2d2(d,ul,xl,ix,tl,s,p,ndf,ndm,nst,isw)
          else
            call fld2d2(d,ul,xl,ix,s,p,ndf,ndm,nst,isw)
          endif

c       Enhanced Strain Model

        elseif(etype.eq.3) then

          if(dtype.gt.0) then
            call sld2d3(d,ul,xl,ix,tl,s,p,ndf,ndm,nst,isw)
          else
c           No routine supplied
          endif

        endif

c     Compute mass or geometric stiffness matrix

      elseif(isw.eq.5) then

        if(imtyp.eq.1) then
          call mass2d(d,xl,ix,s,p,ndf,ndm,nst)
        else
c         put call to geometric stiffness routine here
        endif

      endif

c     Formats for input-output

2001  format(
     & /5x,'T w o   D i m e n s i o n a l   S o l i d   E l e m e n t'/)

      end

      subroutine bbar2m(sg,shp,dvol,xji,lint,nel,hh,theta,shpbar)

c_____________________________________________________________________c
c     Purpose: Compute mixed formulation for the volumetric response

c     Inputs:
c        sg(3,*)       - Quadrature points and weights at gauss points
c        shp(3,9,*)    - Shape functions and derivatives at gauss points
c        vol(*)        - Volume elements at gauss points at t_n+1
c        xji(2,*)      - Jacobian determinant at gauss points
c        lint          - Number of quadrature points
c        nel           - Number of nodes on element (should be 8)

c     Outputs:
c        hh(3,3)       - Reference configuration shape integrals (inverse)
c        theta(2,*)    - Mixed jacobian determinant for element
c        shpbar(2,9,*) - Mixed derivatives of shape functions.
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      integer   lint,  nel,  i,  j,  l
      real*8    voln, h0, h1, h2

      real*8    sg(3,*),    shp(3,9,*), dvol(*), xji(2,*)
      real*8    theta(2,*), shpbar(2,9,*)
      real*8    gg(3,2,9),  hh(3,3),    ji(3,2), hj(3,2), hg(3,2,9)
      real*8    phi(3)

      save

c     4-Node Element

      if(nel.eq.4) then

        do j = 1,nel
          shpbar(1,j,1) = 0.0d0
          shpbar(2,j,1) = 0.0d0
        end do ! j
        hh(1,1) = 0.d0
        h1      = 0.d0
        h2      = 0.d0

        do l = 1,lint

c         H-array and D-array

          voln    = dvol(l) / xji(1,l)
          hh(1,1) = hh(1,1) + voln
          h1      = h1      + dvol(l)
          h2      = h2      + voln*xji(2,l)

c         G-array

          do j = 1,nel
            do i = 1,2
              shpbar(i,j,1) = shpbar(i,j,1) + shp(i,j,l) * dvol(l)
            end do
          end do
        end do

c       Modify shpbar for B-bar type computations

        h0 = 1.d0/h1

        do j = 1,nel
          do i = 1,2
            shpbar(i,j,1) = shpbar(i,j,1)*h0
          end do
        end do

c       Average Jacobian

        hh(1,1)    = 1.d0 / hh(1,1)
        theta(1,1) = h1   * hh(1,1)
        theta(2,1) = h2   * hh(1,1)

        do l = 2,lint
          theta(1,l) = theta(1,1)
          theta(2,l) = theta(2,1)
          do j = 1,nel
            shpbar(1,j,l) = shpbar(1,j,1)
            shpbar(2,j,l) = shpbar(2,j,1)
          end do ! j
        end do ! l

c     9-Node element

      elseif(nel.eq.9) then

        do i = 1,3
          do j = 1,nel
            gg(i,1,j) = 0.0d0
            gg(i,2,j) = 0.0d0
          end do ! j
          hh(i,1) = 0.0d0
          hh(i,2) = 0.0d0
          hh(i,3) = 0.0d0
          ji(i,1) = 0.0d0
          ji(i,2) = 0.0d0
        end do ! i

c       Quadrature loop

        phi(1) = 1.d0
        do l = 1,lint
          phi(2) = sg(1,l)
          phi(3) = sg(2,l)
          do j = 1,3

            h1 = phi(j)*dvol(l)
            h0 = h1/xji(1,l)
            h2 = h1*xji(2,l)

c           Ji-array

            ji(j,1) = ji(j,1) + h1
            ji(j,2) = ji(j,2) + h1*xji(2,l)/xji(1,l)

c           H-array

            do i = 1,3
              hh(i,j)    = hh(i,j)    + phi(i)*h0
            end do ! i

c           G-array

            do i = 1,nel
              gg(j,1,i) = gg(j,1,i) + shp(1,i,l)*h1
              gg(j,2,i) = gg(j,2,i) + shp(2,i,l)*h1
            end do ! i
          end do ! j

        end do ! l

c       Invert H-array

        call invert(hh,3,3)

        do j = 1,2
          do i = 1,3
            hj(i,j) = hh(i,1)*ji(1,j)
     &              + hh(i,2)*ji(2,j)
     &              + hh(i,3)*ji(3,j)
          end do ! i
        end do ! j

        do j = 1,nel
          do i = 1,3
            hg(i,1,j) = hh(i,1)*gg(1,1,j)
     &                + hh(i,2)*gg(2,1,j)
     &                + hh(i,3)*gg(3,1,j)
            hg(i,2,j) = hh(i,1)*gg(1,2,j)
     &                + hh(i,2)*gg(2,2,j)
     &                + hh(i,3)*gg(3,2,j)
          end do ! i
        end do ! j

        do l = 1,lint
          theta(1,l) = hj(1,1) + sg(1,l)*hj(2,1) + sg(2,l)*hj(3,1)
          theta(2,l) = hj(1,2) + sg(1,l)*hj(2,2) + sg(2,l)*hj(3,2)
          h0         = 1.d0/theta(1,l)
          do j = 1,nel
            shpbar(1,j,l) = h0*(    hg(1,1,j)
     &                    + sg(1,l)*hg(2,1,j)
     &                    + sg(2,l)*hg(3,1,j))
            shpbar(2,j,l) = h0*(    hg(1,2,j)
     &                    + sg(1,l)*hg(2,2,j)
     &                    + sg(2,l)*hg(3,2,j))
          end do ! j
        end do ! l

      endif

      end

      subroutine bmat2d(c,r,shp,g,bbar)

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Sets B-bar matrix for 2-d problems

c      Inputs:
c         c         - Constant for plane = 0; for axisymm = 1
c         r         - Radius for axisymmetrix (= 1 for plane)
c         shp(3)    - Shape function and derivatives
c         g(2)      - b-bar integrals

c      Outputs:
c         bbar(4,2) - B-bar matrix for a node
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      real*8    c,r,bb1,bb2,sh3
      real*8    shp(3),g(2),bbar(4,2)

      save

c     Mixed modification to form B-bar

      sh3 = c*shp(3)/r
      bb1 = (g(1) - shp(1) - sh3)*0.3333333333333333d0
      bb2 = (g(2) - shp(2)      )*0.3333333333333333d0

c     B-bar matrix for plane and axisymmetric problems

      bbar(1,1) = bb1 + shp(1)
      bbar(2,1) = bb1
      bbar(3,1) = bb1 + sh3
      bbar(4,1) = shp(2)
      bbar(1,2) = bb2
      bbar(2,2) = bb2 + shp(2)
      bbar(3,2) = bb2
      bbar(4,2) = shp(1)

      end

      subroutine fbar2m(f,xji,theta,lint)

c_____________________________________________________________________c
c     Purpose: Form F-bar and left Cauchy-Green tensors

c     Inputs:
c        f(3,3)      - Deformation gradient
c        xji(2,*)    - Determinant of deformation gradient (J)
c        theta(2,*)  - Mixed determinant of deformation gradient
c        lint        - Number of quadrature points

c     Outputs:
c        f(3,3)      - Mixed deformation gradient
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      integer   lint , i , l

      real*8    ration, ratio1
      real*8    f(9,2,*), xji(2,*),theta(2,*)

      save

c     Compute mixed deformation gradient

      do l = 1,lint
        ratio1 = (theta(1,l)/xji(1,l))**0.3333333333333333d0
        ration = (theta(2,l)/xji(2,l))**0.3333333333333333d0
        do i = 1,9
          f(i,1,l) = ratio1*f(i,1,l)
          f(i,2,l) = ration*f(i,2,l)
        end do
      end do

      end

      subroutine fld2d1(d,ul,xl,ix,s,r,ndf,ndm,nst,isw)

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose:  2-D Finite Deformation Elasticity Routine
c                Remark: This is a standard displacement model

c      Inputs:
c         d(*)      - Material set parameters
c         ul(ndf,*) - Nodal solution parameters for element
c         xl(ndm,*) - Nodal coordinates for element
c         ix(*)     - Element nodal connection list
c         ndf       - Number dof/node
c         ndm       - Spatial dimension of mesh
c         nst       - Dimension of element arrays
c         isw       - Switch to control action

c      Outputs:
c         s(nst,*)  - Element matrix
c         p(nst)    - Element vector
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'bdata.h'
      include  'cdata.h'
      include  'eldata.h'
      include  'elengy.h'
      include  'elplot.h'
      include  'eltran.h'
      include  'hdata.h'
      include  'iofile.h'
      include  'pmod2d.h'
      include  'prstrs.h'
      include  'ptdat6.h'
      include  'comblk.h'

      integer   ndf,ndm,nst,isw
      integer   i,i1, j,jj,j1, l,lint, nn,nhv, istrt

      real*8    bdb,bd3,dl,dc,di,dvol0,dvol,dmas0, xsj0
      real*8    cfac,lfac,xx1,xx2,xx3,yy1, tempi, xlamd, ha, ta

      integer   ix(*)
      real*8    d(*),ul(ndf,nen,*),xl(ndm,*),s(nst,*)
      real*8    r(ndf,*),r1(3,9),al(2),ac(2),vl(2)
      real*8    sg(3,16),bbd(4,2),bb(6),shpr(9)
      real*8    df(3,3,9),f(9,2,9),finv(3,3,9),detf(2,9),xsj(9)
      real*8    ds(6,6,5),dd(6,6),sigv(9),shps(3,9,9),sigl(9,9)

      save

      data      lint / 0 /
      data      xlamd,ha /  2*0.0d0 /
      data      f    /162*0.0d0 /
      data      finv / 81*0.0d0/

c     TEMPORARY SET OF TEMPERATURE VALUE

      data      ta   /    0.0d0/

c     Set quadrature and go to process isw

      l = d(5)
      if(lint.ne.l*l) call int2d(l,lint,sg)

c     COMPUTE TANGENT STIFFNESS AND RESIDUAL FORCE VECTOR

c     Compute shape functions and derivatives in reference configuration

      do l = 1,lint
        call shp2d(sg(1,l),xl,shps(1,1,l),xsj(l),ndm,nel,ix,.false.)
      end do

c     Compute deformation gradient and determinant; transform shape
c     functions to current configuration.

      do l = 1,lint
        call kine2d(shps(1,1,l),xl,ul,f(1,1,l),finv(1,1,l),df(1,1,l),
     &              detf(1,l),ndm,ndf,nel,nen)
      end do

c     Integration order set to static

      if(d(7).lt.0.0d0) then
        cfac = 0.0d0
        lfac = 0.0d0
      else
        cfac = d(7)
        lfac = 1.0d0 - cfac
      endif

      nhv   = nint(d(15))
      istrt = nint(d(84))

      if(mod(isw,4).eq.0) go to 4

c     LOOP OVER GAUSS POINTS

      nn = 0
      do l = 1,lint

c       Set reference coordinates

        xx1 = 0.0d0
        xx2 = 0.0d0
        do i = 1,nel
          xx1 = xx1 + shps(3,i,l)*xl(1,i)
          xx2 = xx2 + shps(3,i,l)*xl(2,i)
        end do

c       Check for axisymmetry

        if(stype.eq.3) then
          xsj0  = xsj(l)*sg(3,l)
          dvol0 = xsj0*xx1
          do i = 1,nel
            shpr(i) = shps(3,i,l)/xx1
          end do ! i
        else
          xsj0  = 0.0d0
          dvol0 = xsj(l)*sg(3,l)
          do i = 1,nel
            shpr(i) = 0.0d0
          end do ! i
        end if
        dvol  = dvol0*detf(1,l)
        dmas0 = dvol0*d(4)

c       Compute Cauchy stresses and spatial tangent tensor

        call modlfd(d,f(1,1,l),finv(1,1,l),df(1,1,l),detf(1,l),ta,
     &             hr(nn+nh1),hr(nn+nh2),nhv,istrt,ds,sigv,bb,
     &             xlamd,ha,.false.,isw)

        if(isw.eq.13) then

          epl(8) = epl(8) + estore*dvol0

c         Compute velocity at point

          vl(1) = shps(3,1,l)*ul(1,1,4) + shps(3,2,l)*ul(1,2,4)
     &          + shps(3,3,l)*ul(1,3,4) + shps(3,4,l)*ul(1,4,4)

          vl(2) = shps(3,1,l)*ul(2,1,4) + shps(3,2,l)*ul(2,2,4)
     &          + shps(3,3,l)*ul(2,3,4) + shps(3,4,l)*ul(2,4,4)

          tempi = 0.0d0
          do i = 1,nel
            tempi = tempi
     &            + (ul(1,i,4)**2 + ul(2,i,4)**2)*shps(3,i,l)
          end do ! i

c         Accumulate kinetic energy

          epl(7) = epl(7) + 0.5d0*(lfac*tempi
     &                    + cfac*(vl(1)**2 + vl(2)**2))*dmas0

        elseif(isw.ne.14) then

c       Store stress values for tplot

        j1 = 6*(l-1)
        do j = 1,4
          tt(j+j1) = sigv(j)
        end do

c       Multiply tangent moduli and stresses by volume element.

        do i = 1,4
          sigv(i) = sigv(i)*dvol
          do j = 1,4
            dd(i,j) = ds(i,j,1)*dvol*ctan(1)
          end do
        end do

c       Compute accelerations

        al(1) = cfac*(shps(3,1,l)*ul(1,1,5) + shps(3,2,l)*ul(1,2,5)
     &              + shps(3,3,l)*ul(1,3,5) + shps(3,4,l)*ul(1,4,5))

        al(2) = cfac*(shps(3,1,l)*ul(2,1,5) + shps(3,2,l)*ul(2,2,5)
     &              + shps(3,3,l)*ul(2,3,5) + shps(3,4,l)*ul(2,4,5))

c       COMPUTE STRESS DIVERGENCE AND INERTIA TERMS

        xx1 = xx1*d(65)**2
        xx2 = xx2*d(65)**2

        do i = 1,nel

c         Compute inertial and body load effects

          ac(1)   = (al(1) + lfac*ul(1,i,5))*dmas0
     &            -  d(11)*dvol0 - xx1*dmas0
          ac(2)   = (al(2) + lfac*ul(2,i,5))*dmas0
     &            -  d(12)*dvol0 - xx2*dmas0

c         Stress divergence term (used in geometric stiffness)

          r1(1,i) = shps(1,i,l)*sigv(1) + shps(2,i,l)*sigv(4)
          r1(2,i) = shps(1,i,l)*sigv(4) + shps(2,i,l)*sigv(2)

c         Element residual

          r(1,i) = r(1,i) - r1(1,i) - ac(1)*shps(3,i,l)
     &                    - shpr(i)*sigv(3)
          r(2,i) = r(2,i) - r1(2,i) - ac(2)*shps(3,i,l)

        end do

c       COMPUTE K (s(nst,nst) = K)

        if(isw.eq.3) then

c         PART 1. - Geometric and inertial part.

          dc  = cfac*ctan(3)*dmas0
          dl  = lfac*ctan(3)*dmas0
          i1  = 0
          do i = 1,nel

            do jj = 1,2
              s(i1+jj,i1+jj) = s(i1+jj,i1+jj) + shps(3,i,l)*dl
            end do

            di  = dc*shps(3,i,l)

c           Include geometric stiffness

            if(gflag) then
              bd3 = shpr(i)*sigv(3)*ctan(1)
              j1  = 0
              do j = 1,i
                bdb          = (r1(1,i)*shps(1,j,l)
     &                       +  r1(2,i)*shps(2,j,l))*ctan(1)
     &                       +       di*shps(3,j,l)
                s(i1+1,j1+1) = s(i1+1,j1+1) + bdb + bd3*shpr(j)
                s(i1+2,j1+2) = s(i1+2,j1+2) + bdb
                j1 = j1 + ndf
              end do

c           Include inertia only

            else
              j1  = 0
              do j = 1,i
                bdb          = di*shps(3,j,l)
                s(i1+1,j1+1) = s(i1+1,j1+1) + bdb
                s(i1+2,j1+2) = s(i1+2,j1+2) + bdb
                j1 = j1 + ndf
              end do
            endif

            i1 = i1 + ndf
          end do

c         PART 2. - Tangent modulus part (based upon dd-array)

          i1 = 0
          do i  = 1,nel

c           Compute bmat-t * dd * dvol

            do jj = 1,4

              bbd(jj,1) = shps(1,i,l)*dd(1,jj)
     &                  + shpr(  i  )*dd(3,jj)
     &                  + shps(2,i,l)*dd(4,jj)

              bbd(jj,2) = shps(1,i,l)*dd(4,jj)
     &                  + shps(2,i,l)*dd(2,jj)

            end do ! jj

c           Compute tangent stiffness

            j1 = 0
            do j  = 1,i

              s(i1+1,j1+1) = s(i1+1,j1+1) + bbd(1,1)*shps(1,j,l)
     &                                    + bbd(3,1)*shpr(  j  )
     &                                    + bbd(4,1)*shps(2,j,l)

              s(i1+2,j1+1) = s(i1+2,j1+1) + bbd(1,2)*shps(1,j,l)
     &                                    + bbd(3,2)*shpr(  j  )
     &                                    + bbd(4,2)*shps(2,j,l)

              s(i1+1,j1+2) = s(i1+1,j1+2) + bbd(4,1)*shps(1,j,l)
     &                                    + bbd(2,1)*shps(2,j,l)

              s(i1+2,j1+2) = s(i1+2,j1+2) + bbd(4,2)*shps(1,j,l)
     &                                    + bbd(2,2)*shps(2,j,l)

              j1 = j1 + ndf
            end do

            i1 = i1 + ndf
          end  do

        endif ! end of tangent

        endif ! end of isw options

        nn = nn + nhv

      end do

      if(isw.eq.3) then

c       Compute upper part by symmetry

        do j = 1,nst
          do i = 1,j
            s(i,j) = s(j,i)
          end do
        end do

      endif

      return

c     OUTPUT STRESSES

   4  xx1  = 0.d0
      xx2  = 0.d0
      xx3  = 0.d0
      do i = 1,6
        sigv(i) = 0.0d0
      end do ! i

c     LOOP OVER GAUSS POINTS

      nn = 0

      do l = 1,lint

c       Compute Cauchy stresses and spatial tangent tensor at t-n+1

        call modlfd(d,f(1,1,l),finv(1,1,l),df(1,1,l),detf(1,l),ta,
     &             hr(nn+nh1),hr(nn+nh2),nhv,istrt,ds,sigl(1,l),bb,
     &             xlamd,ha,.false.,isw)

        yy1 = 1.d0/dble(nel)
        do i=1,nel
          tempi = yy1 * shps(3,i,l)
          xx1   = xx1 + tempi*xl(1,i)
          xx2   = xx2 + tempi*xl(2,i)
        end do

c       Compute average stresses and jacobian for printing

        do i = 1,4
          sigv(i) = sigv(i) + yy1 * sigl(i,l)
        end do

        nn = nn + nhv

      end do

c     OUTPUT STRESSES

      if(isw.eq.4) then
        mct = mct - 2
        if(mct.le.0) then
          write(iow,2001) o,head
          if(ior.lt.0) write(*,2001) o,head
          mct = 50
        endif

        write(iow,2002) n,ma,(sigv(jj),jj=1,6),xx1,xx2,xx3
        if(ior.lt.0) then
          write(*,2002) n,ma,(sigv(jj),jj=1,6),xx1,xx2,xx3
        end if

      elseif(isw.eq.8) then

c       Project stress values to nodes

        call stcn2d(ix,sigl,shps,xsj,hr(nph),hr(nph+numnp),hr(ner),
     &              erav,lint,nel,9,numnp)

      end if

c     Format statements

2001  format(a1,20a4//5x,'Element Stresses'//'  Elmt  Matl',
     1   '  11-stress  22-stress  33-stress  12-stress',
     2   '  23-stress  13-stress'/16x,'1-coord    2-coord    3-coord ')

2002  format(2i6,1p,6e11.3/12x,1p,6e11.3/12x,0p,3f11.5)

      end

      subroutine fld2d2(d,ul,xl,ix,s,r,ndf,ndm,nst,isw)

c_____________________________________________________________________c

c           TWO DIMENSIONAL FINITE DEFORMATION ELEMENT

c         ***  Mixed Model, Operator Split Technique  ***
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'augdat.h'
      include  'bdata.h'
      include  'cdata.h'
      include  'eldata.h'
      include  'elengy.h'
      include  'elplot.h'
      include  'eltran.h'
      include  'hdata.h'
      include  'iofile.h'
      include  'pmod2d.h'
      include  'prstrs.h'
      include  'comblk.h'

      integer   ndf,ndm,nst, isw
      integer   i,i1, j,jj,j1, l,lint, nhi,nhv,nn, istrt

      real*8    augfp,  d1,     bdb,    epp
      real*8    third,  thlog,  xsj,    ta,    qfact
      real*8    dsigtr, dpress, mpress, dmass, dmshp, dtheta

      integer   ix(*)
      real*8    d(*),       ul(ndf,nen,*),  xl(ndm,*),   s(nst,*)
      real*8    sg(3,9),    df(9,9),        fi(9,2,9),   finv(9,9)
      real*8    xxm(3),     xu(2,9),        ru(2,9),     r(ndf,*)
      real*8    bbd(2,7),   bei(6),         ad(7,7,5,9), dd(7,7)
      real*8    shp(3,9,9), dvol(9),        dvl0(9),     xji(2,9)
      real*8    sigm(9),    sigl(9,9),      bpra(3),     shpbar(2,9,9)
      real*8    acc(2),     theta(2,9),     hh(3,3)
      real*8    press(9),   hsig(3)

      save

      data    third / 0.3333333333333333d0 /
      data    nhi   / 2 /

c     TEMPORARY SET OF TEMPERATURE

      data    ta    / 0.0d0 /

c     Augmented Lagrangian update for nested iteration

      if(isw.eq.10) then

        d1      = augfp*d(21)
        hr(nh2) = hr(nh2) + d1*hr(nh2+1)

c     Compute tangent stiffness and residual force vector

      elseif(isw.eq.3 .or. isw.eq.4 .or. isw.eq. 6 .or.
     &       isw.eq.8 .or. isw.eq.14) then

        augfp  = augf
        estore = 0.0d0

c       Compute current geometry

        do j = 1,nel
          do i = 1,2
            xu(i,j) = xl(i,j) + ul(i,j,1)
          end do ! i
        end do ! j

        if(nel.le.4) then
          l = 2
        else
          l = 3
        endif
        call int2d(l,lint,sg)

c       Get shape functions and derivatives in geometry at time t_n+1

        do l = 1,lint
          call shp2d(sg(1,l),xu,shp(1,1,l),xsj,ndm,nel,ix,.false.)
          dvol(l) = xsj*sg(3,l)
        end do ! l

c       Set number of history terms / quadradure point

        nhv   = nint(d(15))
        istrt = nint(d(84))

c       MECHANICAL ELEMENT

        if(isw.eq.3 .or. isw.eq.6 .or. isw.eq.14) then

c         Compute f, finv, df and det(fei) at conf t-n+1

          call kine2m(shp,ul,fi,finv,df,ndf,nel,nen,xji,lint)

c         Mixed model for volumetric response

          call bbar2m(sg,shp,dvol,xji,lint,nel,hh,theta,shpbar)

c         Compute mixed model deformation gradient

          call fbar2m(fi,xji,theta,lint)

c         Compute Cauchy stresses and spatial tangent tensor at t-n+1

          nn = nhi
          do l = 1,lint

            call modlfd(d,fi(1,1,l),finv(1,l),df(1,l),theta(1,l),ta,
     &                  hr(nn+nh1),hr(nn+nh2),nhv,istrt,ad(1,1,1,l),
     &                  sigl(1,l),bei,hr(nh2),hr(nh2+1),.true.,isw)
            nn = nn + nhv
          end do ! l

c         Compute mixed pressure

          if(isw.eq.3 .or. isw.eq.6) then

            if(nel.eq.4) then

              press(1) = 0.0d0
              do l = 1,lint

c               Modify volume element and integrate pressure
c               over reference volume

                dvl0(l)  = dvol(l) / xji(1,l)
                press(1) = press(1) + third*(sigl(1,l) + sigl(2,l)
     &                              + sigl(3,l))*dvl0(l)
                dvol(l)  = dvl0(l) * theta(1,l)

              end do ! l

c             Divide pressure by reference volume

              press(1) = press(1) * hh(1,1)
              do l = 2,lint
                press(l) = press(1)
              end do ! l

            elseif(nel.eq.9) then

              sigm(1) = 0.0d0
              sigm(2) = 0.0d0
              sigm(3) = 0.0d0
              do l = 1,lint

c               Modify volume element and integrate pressure
c               over reference volume

                dvl0(l)  = dvol(l) / xji(1,l)
                mpress   = third*(sigl(1,l) + sigl(2,l)
     &                          + sigl(3,l))*dvl0(l)
                sigm(1) = sigm(1) + mpress
                sigm(2) = sigm(2) + mpress*sg(1,l)
                sigm(3) = sigm(3) + mpress*sg(2,l)
                dvol(l) = dvl0(l) * theta(1,l)

              end do ! l

c             Divide pressure by reference volume

              do i = 1,3
                hsig(i) = hh(i,1)*sigm(1)
     &                  + hh(i,2)*sigm(2)
     &                  + hh(i,3)*sigm(3)
              end do ! i
              do l = 1,lint
                press(l) = hsig(1) + hsig(2)*sg(1,l) + hsig(3)*sg(2,l)
              end do ! l

            endif

            do l = 1,lint

c             Compute mixed stress and multiply by volume element

              dsigtr  = press(l)*xji(1,l)/theta(1,l)
     &                - (sigl(1,l)+sigl(2,l)+sigl(3,l))*third
              sigm(1) =  sigl(1,l) + dsigtr
              sigm(2) =  sigl(2,l) + dsigtr
              sigm(3) =  sigl(3,l) + dsigtr
              sigm(4) =  sigl(4,l)

c             Store time history plot data for element

              i = 6*(l-1)
              do j = 1,4
                tt(j+i) = sigm(j)
                sigm(j) = sigm(j)*dvol(l)
              end do ! j

c             Compute acceleration

              if(d(7).ge.0.0d0) then
                dmass = d(4)*dvl0(l)
              else
                dmass = 0.0d0
              endif
              do i = 1,2
                acc(i) = 0.0d0
                do j = 1,nel
                  acc(i) = acc(i) + shp(3,j,l)*ul(i,j,5)
                end do ! j
                acc(i) = acc(i)*dmass
              end do ! i
              dmass = ctan(3)*dmass

c             Compute residual

              do j = 1,nel

                ru(1,j) = shp(1,j,l)*sigm(1) + shp(2,j,l)*sigm(4)
                ru(2,j) = shp(1,j,l)*sigm(4) + shp(2,j,l)*sigm(2)

                r(1,j)  = r(1,j) - ru(1,j) - shp(3,j,l)*acc(1)
                r(2,j)  = r(2,j) - ru(2,j) - shp(3,j,l)*acc(2)

              end do ! j

c             Compute mixed tangent stiffness matrix

              if(isw.eq.3) then

c               Part 1: Geometric tangent matrix

                if(gflag) then
                  i1 = 0
                  do i = 1,nel
                    j1 = 0
                    do j = 1,nel
                      bdb = shp(1,i,l)*ru(1,j) + shp(2,i,l)*ru(2,j)
                      do jj = 1,2
                        s(i1+jj,j1+jj) = s(i1+jj,j1+jj) + bdb
                      end do ! jj
                      j1 = j1 + ndf
                    end do ! j
                    i1 = i1 + ndf
                  end do ! i
                endif ! gflag

c               Part 2: Material tangent matrix

c               Modify tangent moduli for stress factors

                mpress = press(l)*xji(1,l)/theta(1,l)
                dpress = third*(sigl(1,l) + sigl(2,l) + sigl(3,l))

                call dmatdx(ad(1,1,1,l),sigl(1,l),dpress,mpress)

c               Multiply tangent moduli by volume element

                d1 = dvol(l)*ctan(1)
                do i = 1,7
                  do j = 1,7
                    dd(i,j) = ad(i,j,1,l)*d1
                  end do ! j
                end do ! i

c               Compute row terms

                i1 = 0
                do i = 1,nel

c                 Compute bmat-t * dd * dvol

                  do jj = 1,7

                    bbd(1,jj) =    shp(1,i,l)*dd(1,jj)
     &                        +    shp(2,i,l)*dd(4,jj)
     &                        + shpbar(1,i,l)*dd(7,jj)

                    bbd(2,jj) =    shp(2,i,l)*dd(2,jj)
     &                        +    shp(1,i,l)*dd(4,jj)
     &                        + shpbar(2,i,l)*dd(7,jj)
                  end do ! jj

                  dmshp = shp(3,i,l)*dmass

                  j1 = 0
                  do j = 1,i

c                   Inertial tangent

                    do jj = 1,2
                      s(i1+jj,j1+jj) = s(i1+jj,j1+jj) + dmshp*shp(3,j,l)
                    end do ! jj

c                   Compute mechanics part of tangent stiffness

                    do jj = 1,2
                      s(i1+jj,j1+1) = s(i1+jj,j1+1)
     &                              + bbd(jj,1)*shp(1,j,l)
     &                              + bbd(jj,4)*shp(2,j,l)
     &                              + bbd(jj,7)*shpbar(1,j,l)

                      s(i1+jj,j1+2) = s(i1+jj,j1+2)
     &                              + bbd(jj,2)*shp(2,j,l)
     &                              + bbd(jj,4)*shp(1,j,l)
     &                              + bbd(jj,7)*shpbar(2,j,l)
                    end do ! jj

                    j1 = j1 + ndf
                  end do ! j
                  i1 = i1 + ndf
                end do ! i
              endif ! isw = 3
            end do ! l

c           Compute lower part by symmetry

            if(isw .eq. 3) then

              do i = 1,nst
                do j = 1,i
                  s(j,i) = s(i,j)
                end do ! j
              end do ! i
            endif

          endif ! isw = 3 or 6

c       Output stresses.

        elseif(isw.eq.4 .or. isw.eq.8) then

          do i = 1,9
            sigm(i) = 0.0d0
          end do ! i
          do i = 1,3
            bpra(i) = 0.0d0
            xxm(i)  = 0.0d0
          end do ! i
          epp    = 0.0d0
          dtheta = 0.0d0
          qfact  = 1.d0/dble(lint)

c         Compute f, finv, df and det(fei) at conf t-n+1

          call kine2m(shp,ul,fi,finv,df,ndf,nel,nen,xji,lint)

          call bbar2m(sg,shp,dvol,xji,lint,nel,hh,theta,shpbar)

          call fbar2m(fi,xji,theta,lint)

c         Second loop over Gauss points

          nn  = nhi
          do l = 1,lint

c           Compute Cauchy stresses and spatial tangent tensor at t-n+1

            call modlfd(d,fi(1,1,l),finv(1,l),df(1,l),theta(1,l),ta,
     &                  hr(nn+nh1),hr(nn+nh2),nhv,istrt,ad,sigl(1,l),
     &                  bei,hr(nh2),hr(nh2+1),.true.,isw)

c           Compute principal stretches

            call pstr3d(bei, bpr)

c           Average stresses and stretches for printing

            do i = 1,2
              bpra(i) = bpra(i) + 0.5d0*qfact*dlog(bpr(i))
              xxm(i)  = xxm(i)  + qfact *xu(i,l)
            end do ! i
            bpra(3) = bpra(3) + 0.5d0*qfact*dlog(bpr(3))
            do i = 1,4
              sigm(i) = sigm(i) + qfact*sigl(i,l)
            end do ! i
            epp      = epp      + qfact*sigl( 9,l)
            dtheta   = dtheta   + qfact*theta(1,l)
            nn = nn + nhv
          end do ! l

c         Output stresses

          if (isw .eq. 4) then

            call pstr2d(sigm,sigm(7))

            mct = mct - 2
            if(mct.le.0) then
              write(iow,2001) o,head
              if(ior.lt.0) write(*,2001) o,head
              mct = 50
            endif

c           Compute potential damage variable

            thlog = log(abs(dtheta))
            write(iow,2002) n,ma,(sigm(i),i=1,9),bpra,xxm,thlog,epp
            if(ior.lt.0) then
              write(*,2002) n,ma,(sigm(i),i=1,9),bpra,xxm,thlog,epp
            endif
          else

c           Project stresses onto nodes

            call stcn2d(ix,sigl,shp,dvol,hr(nph),hr(nph+numnp),hr(ner),
     &                  erav,lint,nel,9,numnp)
          endif
        endif ! isw = 4 or 8

      endif ! isw = 3 or 4 or 6 or 8 or 14

c     Formats

2001  format(a1,20a4//5x,'Element Stresses'//'  Elmt  Matl',
     &   '  11-Stress  22-Stress  33-Stress  12-Stress  23-Stress',
     &   '  13-Stress'/12x,'   1-Stress   2-Stress      Angle',
     &   '  log(lam1)  log(lam2)  log(lam3)'/12x,'    1-Coord',
     &   '    2-Coord    3-Coord    log-J     eff-ep')

2002  format(2i6,1p6e11.3/12x,1p6e11.3/12x,0p3f11.5,1p2e11.3/1x)

      end

      subroutine gvec2d(xl,ul,c,ndm,ndf)

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Compute integrals for mixed B-bar 4 node element

c      Inputs:
c         xl(ndm,*)  - Nodal coordinates for element
c         ul(ndf,*)  - Nodal solution for element
c         c          - Plane strain/axisymmetric indicator
c         ndm        - Spatial dimension of mesh
c         ndf        - Number dof/node

c      Outputs:
c         none       - Output is through common /elm2d/
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'elm2d.h'

      integer   i,n,ndm,ndf
      real*8    c,reta,rpsi,rpxe,zeta,zpsi,zpxe
      real*8    crpsi,creta,crpxe,crsum,xj0,xj1,xj2,xj0c,vol
      real*8    gpsi1,geta1,gpxe1,gpsi2,geta2,gpxe2
      real*8    xl(ndm,*),ul(ndf,*),cr(4)

      save

c     Set up cr array (r for axisymmetric, 1's for plane)

      do i = 1,4
        cr(i) = 1.0d0
        if(c.ne.0.0d0) cr(i) = xl(1,i)
      end do

c     Set up sums

      reta = - xl(1,1) - xl(1,2) + xl(1,3) + xl(1,4)
      rpsi = - xl(1,1) + xl(1,2) + xl(1,3) - xl(1,4)
      rpxe =   xl(1,1) - xl(1,2) + xl(1,3) - xl(1,4)
      zeta = - xl(2,1) - xl(2,2) + xl(2,3) + xl(2,4)
      zpsi = - xl(2,1) + xl(2,2) + xl(2,3) - xl(2,4)
      zpxe =   xl(2,1) - xl(2,2) + xl(2,3) - xl(2,4)
      crpsi = - cr(1) + cr(2) + cr(3) - cr(4)
      creta = - cr(1) - cr(2) + cr(3) + cr(4)
      crpxe = + cr(1) - cr(2) + cr(3) - cr(4)
      crsum = + cr(1) + cr(2) + cr(3) + cr(4)

c     Compute jacobian constants

      xj0 = (rpsi*zeta - reta*zpsi)
      xj1 = (rpsi*zpxe - rpxe*zpsi)
      xj2 = (rpxe*zeta - reta*zpxe)
      vol = xj0*crsum + (xj1*crpsi + xj2*creta)/3.0

c     Modify terms to form volumetric matrix

      xj0c = xj0*c/vol
      crsum = crsum/vol
      crpsi = crpsi/vol
      creta = creta/vol
      crpxe = crpxe/vol

c     Form g-vector constants

      gpsi1 = zeta*crsum + (zpxe*crpsi + xj1*c/vol)/3.0
      geta1 =-zpsi*crsum - (zpxe*creta - xj2*c/vol)/3.0
      gpxe1 = (zeta*creta - zpsi*crpsi)/3.0
      gpsi2 =-reta*crsum - (rpxe*crpsi)/3.0
      geta2 = rpsi*crsum + (rpxe*creta)/3.0
      gpxe2 = (rpsi*crpsi - reta*creta)/3.0

c     Form g-vector for each shape function

      g(1,1) = - gpsi1 - geta1 + gpxe1 + xj0c
      g(1,2) = + gpsi1 - geta1 - gpxe1 + xj0c
      g(1,3) = + gpsi1 + geta1 + gpxe1 + xj0c
      g(1,4) = - gpsi1 + geta1 - gpxe1 + xj0c
      g(2,1) = - gpsi2 - geta2 + gpxe2
      g(2,2) = + gpsi2 - geta2 - gpxe2
      g(2,3) = + gpsi2 + geta2 + gpxe2
      g(2,4) = - gpsi2 + geta2 - gpxe2

c     Compute trace of strain (or its increment)

      trep = 0.0d0
      do n = 1,4
        trep = trep + g(1,n)*ul(1,n) + g(2,n)*ul(2,n)
      end do

      end

      subroutine kine2d (shps,xl,ul,f,fi,df,detf,ndm,ndf,nel,nen)

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Compute kinematic quantities for finite deformations

c      Inputs:
c         shps(3,nel) - Reference configuration shape functions
c         xl(ndm,nel) - Nodal reference coordinates
c         ul(ndf,nel) - Nodal displacements
c         ndm         - Number mesh dimensions
c         ndf         - Number dof/node
c         nel         - Number nodes/element
c         nen         - Maximum number nodes/element

c      Outputs:
c         f(3,3,2)    - deformation gradient
c         fi(3,3)     - inverse deformation gradient
c         df(3,3)     - incremental deformation gradient
c         detf(2)     - determinant of deformation gradient
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'pmod2d.h'

      integer   ndm,ndf,nel,nen, i,j,k
      real*8    detfi,temp,xx1

      real*8    shps(3,*),xl(ndm,*),ul(ndf,nen,*)
      real*8    df(3,3),f(3,3,*),fi(3,3),detf(*)

      save

c     Deformation gradient at t_n+1 : F_n+1 = I + GRAD u_n+1

      do i = 1,2
        do j = 1,2
          f(i,j,1)  = 0.0d0
          df(i,j) = 0.0d0
          do k = 1,nel
            f(i,j,1) = f(i,j,1) + ul(i,k,1)*shps(j,k)
            df(i,j ) = df(i,j ) + ul(i,k,2)*shps(j,k)
          end do ! k
        end do ! j
        f(i,i,1) = f(i,i,1) + 1.0d0
      end do ! i

c     Deformation gradient at t_n: F_n

      f(1,1,2)  = f(1,1,1) - df(1,1)
      f(2,1,2)  = f(2,1,1) - df(2,1)
      f(1,2,2)  = f(1,2,1) - df(1,2)
      f(2,2,2)  = f(2,2,1) - df(2,2)

      f(1,3,1)  = 0.0d0
      f(3,1,1)  = 0.0d0

      f(2,3,1)  = 0.0d0
      f(3,2,1)  = 0.0d0

      f(1,3,2)  = 0.0d0
      f(3,1,2)  = 0.0d0

      f(2,3,2)  = 0.0d0
      f(3,2,2)  = 0.0d0

      df(1,3)   = 0.0d0
      df(3,1)   = 0.0d0

      df(2,3)   = 0.0d0
      df(3,2)   = 0.0d0

      if(stype.eq.3) then
        f(3,3,1) = 0.0d0
        xx1      = 0.0d0
        df(3,3)  = 0.0d0
        do k = 1,nel
          xx1      = xx1      + xl(1,k  )*shps(3,k)
          f(3,3,1) = f(3,3,1) + ul(1,k,1)*shps(3,k)
          df(3,3)  = df(3,3)  + ul(1,k,2)*shps(3,k)
        end do
        f(3,3,1) = 1.d0 + f(3,3,1)/xx1
        df(3,3)  = df(3,3)/xx1
        f(3,3,2) = f(3,3,1) - df(3,3)
      else
        f(3,3,1) = 1.0d0
        f(3,3,2) = 1.0d0
        df(3,3)  = 0.0d0
      endif

c     Invert F

      detf(1) = f(1,1,1)*f(2,2,1) - f(1,2,1)*f(2,1,1)
      detf(2) = f(1,1,2)*f(2,2,2) - f(1,2,2)*f(2,1,2)

      detfi   =  1.d0/detf(1)
      fi(1,1) =  f(2,2,1)*detfi
      fi(1,2) = -f(1,2,1)*detfi
      fi(1,3) =  0.0d0
      fi(2,1) = -f(2,1,1)*detfi
      fi(2,2) =  f(1,1,1)*detfi
      fi(2,3) =  0.0d0
      fi(3,1) =  0.0d0
      fi(3,2) =  0.0d0
      fi(3,3) =  1.0d0/f(3,3,1)

c     Determinants

      detf(1) = detf(1)*f(3,3,1)
      detf(2) = detf(2)*f(3,3,2)

c     Transform shape functions to current configuration

      do k = 1,nel
        temp      = fi(1,1)*shps(1,k) + fi(2,1)*shps(2,k)
        shps(2,k) = fi(1,2)*shps(1,k) + fi(2,2)*shps(2,k)
        shps(1,k) = temp
      end do

      end

      subroutine kine2m(shp,ul,f,finv,df,ndf,nel,nen,detf,lint)

c_____________________________________________________________________c
c     Purpose: Compute deformation gradient and its inverse at tn+1

c     Inputs:
c        shp(3,9,*)  - Shape functions and derivatives at gauss points
c        ul(ndf,*)   - Nodal solution parameters
c        ul(2,*)     - Nodal stress free reference displacements
c        ndf         - Number dof/node
c        nel         - Number nodes/element
c        nen         - Maximum number nodes/element
c        lint        - Number of quadrature points

c     Outputs:
c        f(9,*)      - Deformation gradient at gauss points
c        finv(9,*)   - Inverse deformation gradient at points
c        df(9,*)     - Incremental deformation gradient at points
c        detf(*)     - Determinant of deformation gradient at points
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      integer   ndf, nel,nen, lint, k, l

      real*8    shp(3,9,*),ul(ndf,nen,*)
      real*8    df(9,*),f(9,2,*),finv(9,*),detf(2,*),dfi(9)

      save

c     Compute deformation gradient at t-n+1

c        F**-1 = I - grad u

      do l = 1,lint
        do k = 1,8
          finv(k,l) = 0.0d0
        end do ! k
        finv(9,l) = 1.0d0
      end do ! l

      do k = 1,nel
        do l = 1,lint
          finv(1,l) = finv(1,l) - ul(1,k,1)*shp(1,k,l)
          finv(2,l) = finv(2,l) - ul(2,k,1)*shp(1,k,l)
          finv(4,l) = finv(4,l) - ul(1,k,1)*shp(2,k,l)
          finv(5,l) = finv(5,l) - ul(2,k,1)*shp(2,k,l)
        end do ! l
      end do ! k

      do l = 1,lint
        finv(1,l) = finv(1,l) + 1.0d0
        finv(5,l) = finv(5,l) + 1.0d0
      end do ! l

c     F = ( F^-1)^-1

      do l = 1,lint
        detf(1,l) = 1.d0/(finv(1,l)*finv(5,l)-finv(2,l)*finv(4,l))
      end do ! l

      do l = 1,lint
        f(1,1,l) = finv(5,l)*detf(1,l)
        f(2,1,l) =-finv(2,l)*detf(1,l)
        f(3,1,l) = 0.0d0
        f(4,1,l) =-finv(4,l)*detf(1,l)
        f(5,1,l) = finv(1,l)*detf(1,l)
        f(6,1,l) = 0.0d0
        f(7,1,l) = 0.0d0
        f(8,1,l) = 0.0d0
        f(9,1,l) =  1.0d0
      end do ! l

c     Compute incremental deformation gradient

      do l = 1,lint
        dfi(1) = 0.0d0
        dfi(2) = 0.0d0
        dfi(4) = 0.0d0
        dfi(5) = 0.0d0
        do k = 1,nel
          dfi(1)   = dfi(1)   + ul(1,k,2)*shp(1,k,l)
          dfi(2)   = dfi(2)   + ul(2,k,2)*shp(1,k,l)
          dfi(4)   = dfi(4)   + ul(1,k,2)*shp(2,k,l)
          dfi(5)   = dfi(5)   + ul(2,k,2)*shp(2,k,l)
        end do
        df(1,l) = dfi(1)*f(1,1,l) + dfi(4)*f(2,1,l)
        df(2,l) = dfi(2)*f(1,1,l) + dfi(5)*f(2,1,l)
        df(3,l) = 0.0d0
        df(4,l) = dfi(1)*f(4,1,l) + dfi(4)*f(5,1,l)
        df(5,l) = dfi(2)*f(4,1,l) + dfi(5)*f(5,1,l)
        df(6,l) = 0.0d0
        df(7,l) = 0.0d0
        df(8,l) = 0.0d0
        df(9,l) = 0.0d0

c       Compute deformation gradient F_n

        f(1,2,l) = f(1,1,l) - df(1,l)
        f(2,2,l) = f(2,1,l) - df(2,l)
        f(3,2,l) = 0.0d0
        f(4,2,l) = f(4,1,l) - df(4,l)
        f(5,2,l) = f(5,1,l) - df(5,l)
        f(6,2,l) = 0.0d0
        f(7,2,l) = 0.0d0
        f(8,2,l) = 0.0d0
        f(9,2,l) = 1.0d0

        detf(2,l) = f(1,2,l)*f(5,2,l) - f(2,2,l)*f(4,2,l)

      end do ! l

      end

      subroutine mass2d(d,xl,ix,s,p,ndf,ndm,nst)

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Compute mass matrix for plane and axisymmetric problems

c      Inputs:
c         d(*)      - Material set parameters
c         xl(ndm,*) - Nodal coordinates for element
c         ix(*)     - Element nodal connections
c         ndf       - Number dof/node
c         ndm       - Spatial dimension of mesh
c         nst       - Size of element arrays

c      Outputs:
c         s(nst,*)  - Consistent or interpolated mass
c         p(nst)    - Diagonal (lumped) mass
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'eldata.h'
      include  'pmod2d.h'

      logical   quad
      integer   ndf,ndm,nst
      integer   j,k,l,j1,k1,lint

      real*8    xsj,dv, aj1,xx,lfac,cfac

      integer   ix(*)
      real*8    d(*),xl(ndm,*),s(nst,nst),p(nst)
      real*8    shp(3,9),sg(3,16),el(4,7)

      save

c     Compute mass matrix

      if(nel.eq.6 .or. nel.eq.7) then
        l    =  7
        quad = .false.
        call tint2d(l,lint,el)
      else
        l    = d(5)
        quad = .true.
        if(l*l.ne.lint) call int2d(l,lint,sg)
      endif
      cfac = d(7)
      lfac = 1.d0 - cfac

      do l = 1,lint

c       Compute shape functions

        if(quad) then
          call shp2d(sg(1,l),xl,shp,xsj,ndm,nel,ix,.false.)
          dv = sg(3,l)*abs(xsj)*d(4)
        else
          call trishp(el(1,l),xl,ndm,nel-4,xsj,shp)
          dv = el(4,l)*abs(xsj)*d(4)
        endif
        if(stype.eq.3) then
          xx = 0.0d0
          do j = 1,nel
            xx = xx + shp(3,j)*xl(1,j)
          end do ! j
          dv = dv*xx
        end if

c       For each node j compute db = rho*shape*dv

        j1 = 1
        do j = 1,nel
          aj1 = shp(3,j)*dv

c         Compute a lumped mass

          p(j1)    = p(j1) + aj1
          s(j1,j1) = s(j1,j1) + aj1*lfac
          aj1      = aj1*cfac

c         For each node k compute mass matrix (upper triangular part)

          k1 = j1
          do k = j,nel
            s(j1,k1) = s(j1,k1) + shp(3,k)*aj1
            k1 = k1 + ndf
          end do
          j1 = j1 + ndf
        end do
      end do

c     Compute missing parts and lower part by symmetries

      do j = 1,ndf*nel,ndf
        p(j+1) = p(j)
        do k = j,ndf*nel,ndf
          s(j+1,k+1) = s(j,k)
          s(k  ,j  ) = s(j,k)
          s(k+1,j+1) = s(j,k)
        end do
      end do

      end

      subroutine rays2d(d,shp,sig,dd,vl,xl,ndf,ndm,nel)

c-----[--.----+----.----+----.-----------------------------------------]
c     Purpose: Stiffness proportional Rayleigh damping residual

      implicit  none

      include  'pmod2d.h'

      integer   ndf,ndm,nel, i,j
      real*8    xx

      real*8    d(*),shp(3,*),sig(*),dd(6,6),vl(ndf,*),xl(ndm,*)
      real*8    eps(6)

      do j = 1,6
        eps(j) = 0.0d0
      end do
      xx = 0.0d0
      do j = 1,nel
        xx     = xx     + shp(1,j)*xl(1,j)
        eps(1) = eps(1) + shp(1,j)*vl(1,j)
        eps(2) = eps(2) + shp(2,j)*vl(2,j)
        eps(3) = eps(3) + shp(3,j)*vl(1,j)
        eps(4) = eps(4) + shp(2,j)*vl(1,j) + shp(1,j)*vl(2,j)
      end do ! j

c     Set 3-strain (thickness/hoop)

      if(stype.eq.3) then
        eps(3) = eps(3)/xx
      else
        eps(3) = 0.0d0
      endif

      do j = 1,4
        eps(j) = eps(j)*d(78)
      end do ! j

c     compute stress modification due to damping

      do j = 1,4
        do i = 1,4
          sig(i) = sig(i) + dd(i,j)*eps(j)
        end do ! i
      end do ! j

      end

      subroutine resid2d(cfac,lfac,xsj,xsj0,shp,eps,sig,d,vl,al,p,
     &                   ndf,l)

c     Plane and axisymmetric residual routine

      implicit  none

      include  'eldata.h'
      include  'elplot.h'
      include  'eltran.h'
      include  'prld1.h'

      integer   ndf,l, j,k
      real*8    xsj,xsj0

      real*8    b1,b2,rr
      real*8    aj1,aj2,aj3,aj0,lfac,cfac

      real*8    d(*),vl(ndf,*),al(ndf,*),p(ndf,*)
      real*8    shp(3,*),eps(*),sig(*),vc(2),ac(2)

      save

c     Compute stress-divergence vector (p)

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

      rr   = d(4)

c     Store time history plot data for element

      k = 10*(l-1)
      do j = 1,6
        tt(j+k) = sig(j)
      end do ! j
      k = k + 6
      do j = 1,4
        tt(j+k) = eps(j)
      end do ! j

c     Compute accelerations

      ac(1) = 0.0d0
      ac(2) = 0.0d0
      do j = 1,nel
        ac(1) = ac(1) + shp(3,j)*al(1,j)
        ac(2) = ac(2) + shp(3,j)*al(2,j)
      end do ! j
      ac(1)   = rr*ac(1)*cfac
      ac(2)   = rr*ac(2)*cfac

c     For Rayleigh Mass Damping: Compute velocity

      if(d(77).ne.0.0d0) then
        vc(1) = 0.0d0
        vc(2) = 0.0d0
        do j = 1,nel
          vc(1) = vc(1) + shp(3,j)*vl(1,j)
          vc(2) = vc(2) + shp(3,j)*vl(2,j)
        end do ! j
        vc(1)   = rr*vc(1)*cfac*d(77)
        vc(2)   = rr*vc(2)*cfac*d(77)

        aj0 = lfac*d(77)*rr
        do j = 1,nel
          p(1,j) = p(1,j) - (vc(1) + aj0*vl(1,j))*shp(3,j)*xsj
          p(2,j) = p(2,j) - (vc(2) + aj0*vl(2,j))*shp(3,j)*xsj
        end do ! j

      endif

c     Loop over rows

      do j = 1,nel
        aj1 = shp(1,j)*xsj
        aj2 = shp(2,j)*xsj
        aj3 = shp(3,j)*xsj0
        aj0 = lfac*rr

c       Compute gravity, thermal, inertia, and stress contributions

        p(1,j) = p(1,j) + (b1 - ac(1) - aj0*al(1,j))*shp(3,j)*xsj
     &                  - aj1*sig(1) - aj2*sig(4) - aj3*sig(3)
        p(2,j) = p(2,j) + (b2 - ac(2) - aj0*al(2,j))*shp(3,j)*xsj
     &                  - aj1*sig(4) - aj2*sig(2)

      end do ! j

      end

      subroutine shpi2d(sg,xsj,xl,ndm)

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Enhanced mode shaped functions

c      Inputs:

c      Outputs:
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'incshp.h'

      integer   ndm
      real*8    xsj, sg(2),xl(ndm,*)

      save

c     Compute enhanced strain 'incompatible' shape functions

      shpi(1,1) = -sg(1)*(-xl(2,1) - xl(2,2) + xl(2,3) + xl(2,4))/xsj
      shpi(2,1) =  sg(1)*(-xl(1,1) - xl(1,2) + xl(1,3) + xl(1,4))/xsj
      shpi(3,1) =  0.0d0
      shpi(1,2) =  sg(2)*(-xl(2,1) + xl(2,2) + xl(2,3) - xl(2,4))/xsj
      shpi(2,2) = -sg(2)*(-xl(1,1) + xl(1,2) + xl(1,3) - xl(1,4))/xsj
      shpi(3,2) =  0.0d0
      shpi(1,3) =  0.0d0
      shpi(2,3) =  0.0d0
      shpi(3,3) =  sg(1)*sg(2)

      end

      subroutine sld2d1(d,ul,xl,ix,tl,s,p,ndf,ndm,nst,isw)

c     Plane and axisymmetric linear elastic element routine

      implicit  none

      include  'bdata.h'
      include  'cdata.h'
      include  'eldata.h'
      include  'eltran.h'
      include  'fdata.h'
      include  'hdata.h'
      include  'iofile.h'
      include  'pmod2d.h'
      include  'prstrs.h'
      include  'strnum.h'
      include  'comblk.h'

      logical   quad
      integer   ndf,ndm,nst,isw
      integer   j,k,l,ii,j1,k1,lint,nhv,nn,istrt

      real*8    xsj0,dv,ta
      real*8    aj1,aj2,aj3,aj0,xx,yy,lfac,cfac,sfac
      real*8    bd11,bd21,bd12,bd22,bd13,bd23,bd14,bd24

      integer   ix(*)
      real*8    d(*),ul(ndf,nen,*),xl(ndm,*),tl(*),s(nst,nst),p(ndf,*)
      real*8    shp(3,9,9),sg(3,16),sig(9,9),eps(9,3)
      real*8    xsj(9),dd(6,6,5),mass(9,9),shpr(9),el(4,7)

      save

      data      eps/ 27*0.0d0 /

c     Compute stress-divergence vector (p) and stiffness matrix (s)

      nhv   = nint(d(15))
      istrt = nint(d(84))

      if(isw.eq.3  .or. isw.eq.6 .or. isw.eq.14) then

c       Integration order set to static

        if(d(7).lt.0.0d0) then
          cfac = 0.0d0
          lfac = 0.0d0
        else
          cfac = d(7)
          lfac = 1.d0 - cfac
        endif

c       Compute gauss quadrature points and weights

        if(nel.eq.6 .or. nel.eq.7 ) then
          l    = 7
          quad = .false.
          call tint2d(l,lint,el)
        else
          l    = d(5)
          quad = .true.
          if(l*l.ne.lint) call int2d(l,lint,sg)
        endif

c       Zero mass matrix

        do j = 1,9
          do k = 1,nel
            mass(k,j) = 0.0d0
          end do ! k
          shpr(j) = 0.0d0
        end do ! j

c       Numerical integration loop

        nn = 0
        do l = 1,lint
          if(quad) then
            call shp2d(sg(1,l),xl,shp,xsj(l),ndm,nel,ix,.false.)
            xsj(l) = xsj(l)*sg(3,l)
          else
            call trishp(el(1,l),xl,ndm,nel-4,xsj(l),shp)
            xsj(l) = xsj(l)*el(4,l)
          endif

c         Compute stresses and strains

          call strn2d(d,xl,ul,tl,shp,ndf,ndm,nel,
     &                xx,yy,ta,eps)
          call modlsd(d,ta,eps,hr(nh1+nn),hr(nh2+nn),nhv,istrt,
     &                dd,sig(1,l),isw)

          if(isw.eq.3 .or. isw.eq.6) then

c           Multiply jacobian by radius for axisymmetry

            if(stype.eq.3) then
              xsj0   = xsj(l)
              xsj(l) = xsj(l)*xx
              do j = 1,nel
                shpr(j) = shp(3,j,1)/xx
              end do ! j
            else
              xsj0 = 0.0d0
            end if

c           Rayleigh Damping

            dv = d(4)*(ctan(3) + d(77)*ctan(2))*xsj(l)

            if(d(78).ne.0.0d0) then
              call rays2d(d,shp,sig(1,l),dd(1,1,5),ul(1,1,4),xl,
     &                    ndf,ndm,nel)
              sfac = d(78)*ctan(2)
            else
              sfac = 0.0d0
            endif

c           Compute gravity, thermal, inertia, and stress contributions

            call resid2d(cfac,lfac,xsj(l),xsj0,shp,eps,sig(1,l),d,
     &                   ul(1,1,4),ul(1,1,5),p,ndf,l)

c           Loop over rows

            if(isw.eq.3) then

c             Modify tangent for stiffness rayleigh damping

              do j = 1,4
                do k = 1,4
                  dd(k,j,1) = dd(k,j,1)*ctan(1) + dd(k,j,5)*sfac
                end do
              end do

              j1     = 1

              do j = 1,nel

                aj1 = shp(1,j,1)*xsj(l)
                aj2 = shp(2,j,1)*xsj(l)
                aj3 = shp(3,j,1)*xsj0

c               Compute B_trans * D * j * w

                bd11 = aj1*dd(1,1,1) + aj3*dd(3,1,1) + aj2*dd(4,1,1)
                bd12 = aj1*dd(1,2,1) + aj3*dd(3,2,1) + aj2*dd(4,2,1)
                bd13 = aj1*dd(1,3,1) + aj3*dd(3,3,1) + aj2*dd(4,3,1)
                bd14 = aj1*dd(1,4,1) + aj3*dd(3,4,1) + aj2*dd(4,4,1)

                bd21 = aj2*dd(2,1,1) + aj1*dd(4,1,1)
                bd22 = aj2*dd(2,2,1) + aj1*dd(4,2,1)
                bd23 = aj2*dd(2,3,1) + aj1*dd(4,3,1)
                bd24 = aj2*dd(2,4,1) + aj1*dd(4,4,1)

c               Compute lumped mass matrix

                aj0       = shp(3,j,1)*dv
                mass(j,j) = mass(j,j) + aj0*lfac

c               Loop over columns (symmetry noted)

                k1 = j1
                do k = j,nel
                  s(j1  ,k1  ) = s(j1  ,k1  ) + bd11*shp(1,k,1)
     &                                        + bd14*shp(2,k,1)
     &                                        + bd13*shpr(k)

                  s(j1  ,k1+1) = s(j1  ,k1+1) + bd12*shp(2,k,1)
     &                                        + bd14*shp(1,k,1)

                  s(j1+1,k1  ) = s(j1+1,k1  ) + bd21*shp(1,k,1)
     &                                        + bd24*shp(2,k,1)
     &                                        + bd23*shpr(k)

                  s(j1+1,k1+1) = s(j1+1,k1+1) + bd22*shp(2,k,1)
     &                                        + bd24*shp(1,k,1)

c                 Compute consistent mass matrix

                  mass(j,k)    = mass(j,k)    + cfac*aj0*shp(3,k,1)
                  k1 = k1 + ndf
                end do ! k
                j1 = j1 + ndf
              end do ! j
            end if
          end if
          nn = nn + nhv
        end do ! l

c       Form lower part by symmetry

        if(isw.eq.3) then
          do j = 1,ndf*nel
            do k = j+1,ndf*nel
              s(k,j) = s(j,k)
            end do ! k
          end do ! j

c         Assemble mass matrix into tangent

          do j = 1,nel
            do k = j+1,nel
              mass(k,j) = mass(j,k)
            end do ! k
          end do ! j
          j1 = 0
          do j = 1,ndf*nel,ndf
            j1 = j1 + 1
            k1 = 0
            do k = 1,ndf*nel,ndf
              k1  = k1 + 1
              s(j  ,k  ) = s(j  ,k  ) + mass(j1,k1)
              s(j+1,k+1) = s(j+1,k+1) + mass(j1,k1)
            end do ! k
          end do ! j
        endif

c     Output of element quantities

      elseif(isw.eq.4 .or. isw.eq.8) then

        if(nel.eq.6 .or. nel.eq.7) then
          l    =  6
          quad = .false.
          call tint2d(l,lint,el)
        else
          if(isw.eq.4) then
            l = d(6)
          else
            l = d(5)
          endif
          quad = .true.
          if(l*l.ne.lint) call int2d(l,lint,sg)
        endif

c       Compute element stresses, strains, and forces

        nn = 0
        do l = 1,lint

c         Compute element shape functions

          if(quad) then
            call shp2d(sg(1,l),xl,shp(1,1,l),xsj(l),ndm,nel,ix,.false.)
            xsj(l) = xsj(l)*sg(3,l)
          else
            call trishp(el(1,l),xl,ndm,nel-4,xsj(l),shp(1,1,l))
            xsj(l) = xsj(l)*el(4,l)
          endif

c         Compute strains and coordinates

          call strn2d(d,xl,ul,tl,shp(1,1,l),ndf,ndm,nel,
     &                xx,yy,ta,eps)
          call modlsd(d,ta,eps,hr(nh1+nn),hr(nh2+nn),nhv,istrt,
     &                dd,sig(1,l),isw)

          if(d(14).gt.0.0d0 .and. d(14).ne.1.d0) then
            do ii = 1,4
              sig(ii,l) = sig(ii,l)/d(14)
            end do
          endif

          if(isw.eq.4) then

            call pstr2d(sig(1,l),sig(7,l))

c           Output stresses and strains

            mct = mct - 2
            if(mct.le.0) then
            write(iow,2001) o,head
              if(ior.lt.0 .and. pfr) then
                write(*,2001) o,head
              endif
              mct = 50
            endif
            write(iow,2002)  n,ma,sig(9,l),(sig(ii,l),ii=1,4),sig(7,l),
     &                             xx,yy,(eps(ii,1),ii=1,4),sig(8,l)
            if(ior.lt.0 .and. pfr) then
              write(*,2002)  n,ma,sig(9,l),(sig(ii,l),ii=1,4),sig(7,l),
     &                             xx,yy,(eps(ii,1),ii=1,4),sig(8,l)
            endif
          endif
          nn = nn + nhv
        end do ! l

c       Compute nodal stress values

        if(isw.eq.8) then

          call stcn2d(ix,sig,shp,xsj,hr(nph),hr(nph+numnp),hr(ner),
     &                erav,lint,nel,9,numnp)

        endif
      endif

c     Formats for input-output

2001  format(a1,20a4//5x,'Element Stresses'//'    Elmt Mat Angle',
     &   '   11-stress   22-stress   33-stress   12-stress',
     &   '    1-stress'/'  1-coord  2-coord   11-strain',
     &   '   22-strain   33-strain   12-strain    2-stress')
2002  format(i8,i4,0p,f6.1,1p,5e12.3/0p,2f9.3,1p,5e12.3/1x)

      end

      subroutine sld2d2(d,ul,xl,ix,tl,s,p,ndf,ndm,nst,isw)

c-----[--.----+----.----+----.-----------------------------------------]
c     Plane/axisymmetric linear element routine - B-bar formulation

      implicit  none

      include  'bdata.h'
      include  'cdata.h'
      include  'eltran.h'
      include  'eldata.h'
      include  'elm2d.h'
      include  'elplot.h'
      include  'hdata.h'
      include  'tdata.h'
      include  'iofile.h'
      include  'pmod2d.h'
      include  'prld1.h'
      include  'prstrs.h'
      include  'strnum.h'
      include  'comblk.h'

      logical   flg

      integer   ndm,ndf,nst,isw
      integer   i,j,ii,i1,jj,j1,k,l,lint,nhv,nn, istrt
      real*8    type,xr0,xr1,xz0,b1,b2,dv,dl,ddm,rr,zz,w11,ta
      real*8    cfac,lfac

      integer   ix(*)
      real*8    d(*),ul(ndf,nen,*),xl(ndm,*),tl(*),s(nst,*),p(ndf,*)
      real*8    eps(9,3),aa(6,6,5),dd(6,6),sigv(9,9),sig(7),bbar(4,2,4)
      real*8    bbd(4,2),vl(2),al(2),ac(2),shp(3,9,9),sg(3,9),xsj(9)
      real*8    epsd(6)

      real*8    dot

      save

      data      eps / 27*0.0d0 /

      nhv   = nint(d(15))
      istrt = nint(d(84))
      type = max(0,stype-2)
      if(isw.eq.3 .or. isw.eq.6 .or. isw.eq. 14) then

        flg  = isw .eq. 3
        nn   = 0
        l    = d(5)
        call int2d(l,lint,sg)

        call gvec2d(xl,ul,type,ndm,ndf)

        do l = 1,lint

          call shp2d(sg(1,l),xl,shp,xsj,ndm,nel,ix,.false.)

c         Compute displacement gradient and incr. displ. gradient

          call strn2m(d,xr1,shp,xl,ul,tl,type,xr0,xz0,
     &                ndm,ndf,nel,nen,ta,eps)

c         Compute Cauchy stresses and spatial tangent tensor at t-n+1

          call modlsd(d,ta,eps,hr(nn+nh1),hr(nn+nh2),nhv,istrt,
     &                aa,sig,isw)

          if(isw.eq.3 .or. isw.eq.6) then

            xsj(1) = xsj(1)*sg(3,l)*xr1

c           Integration order set to static

            if(d(7).lt.0.0d0) then
              cfac = 0.0d0
              lfac = 0.0d0
            else
              cfac = d(7)
              lfac = 1.d0 - cfac
            endif

c           Store time history plot data for element

            i = 6*(l-1)
            do j = 1,6
              tt(j+i) = sig(j)
            end do ! j

c           Compute b-bar matrix

            do i = 1,nel
              call bmat2d(type,xr0,shp(1,i,1),g(1,i),bbar(1,1,i))
            end do

c           Proportional body forces

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

c           Rayleigh damping

            if(d(77).ne.0.0d0) then
              do k = 1,2
                vl(k) = 0.0d0
                do j = 1,nel
                  vl(k) = vl(k) + shp(3,j,1)*ul(k,j,4)
                end do
              end do
              vl(1)   = cfac*vl(1)
              vl(2)   = cfac*vl(2)

c             Compute mass damping residual

              do i = 1,nel
                w11    = shp(3,i,1)*xsj(1)*d(77)*d(4)
                p(1,i) = p(1,i) - (vl(1) + lfac*ul(1,i,4))*w11
                p(2,i) = p(2,i) - (vl(2) + lfac*ul(2,i,4))*w11
              end do ! i
            endif

            if(d(78).ne.0.0d0) then
              do i = 1,4
                epsd(i) = 0.0d0
                do j = 1,nel
                  epsd(i) = epsd(i) + bbar(i,1,j)*ul(1,j,4)
     &                              + bbar(i,2,j)*ul(2,j,4)
                end do ! j
              end do ! i
              do i = 1,4
                epsd(i) = epsd(i)*d(78)
                do j = 1,4
                  sig(j) = sig(j) + aa(j,i,1)*epsd(i)
                end do ! j
              end do ! i
            endif

c           Multiply tangent moduli and stress by volume element

            dv  = xsj(1)*(ctan(1) + d(78)*ctan(2))
            do i = 1,4
              sig(i) = sig(i)*xsj(1)
              do j = 1,4
                dd(i,j) = aa(i,j,1)*dv
              end do
            end do

c           Compute accelerations

            do k = 1,2
              al(k) = 0.0d0
              do j = 1,nel
                al(k) = al(k) + shp(3,j,1)*ul(k,j,5)
              end do
            end do
            al(1)   = cfac*al(1)
            al(2)   = cfac*al(2)

c           Loop over rows

            dv = (ctan(3) + d(77)*ctan(2))*xsj(1)*d(4)*cfac
            dl = (ctan(3) + d(77)*ctan(2))*xsj(1)*d(4)*lfac

            i1 = 0
            do i = 1,nel

c           Compute body forces and inertial loading

              ac(1)   = d(4)*(al(1) + lfac*ul(1,i,5))
              ac(2)   = d(4)*(al(2) + lfac*ul(2,i,5))

              p(1,i) = p(1,i) + (b1 - ac(1))*shp(3,i,1)*xsj(1)
              p(2,i) = p(2,i) + (b2 - ac(2))*shp(3,i,1)*xsj(1)

c             Compute internal stress divergence term

              do ii = 1,2
                p(ii,i) = p(ii,i) - dot(bbar(1,ii,i),sig(1),4)
              end do

c             Compute bbar-t * aa * dvol

              if(flg) then
                do ii = 1,2
                  do jj = 1,4
                    bbd(jj,ii) = dot(bbar(1,ii,i),dd(1,jj),4)
                  end do
                end do

c               Compute tangent stiffness

                w11 = shp(3,i,1)*dl
                do ii = 1,2
                  s(i1+ii,i1+ii) = s(i1+ii,i1+ii) + w11
                end do

                w11 = shp(3,i,1)*dv
                j1 = i1
                do j  = i,nel
                  do ii = 1,2
                    do jj = 1,2
                      s(i1+ii,j1+jj) = s(i1+ii,j1+jj)
     &                               + dot(bbd(1,ii),bbar(1,jj,j),4)
                    end do
                    s(i1+ii,j1+ii) = s(i1+ii,j1+ii) + w11*shp(3,j,1)
                    end do
                  j1 = j1 + ndf
                end do
              end if
              i1 = i1 + ndf
            end do
          endif
          nn = nn + nhv
        end do

c       Form lower part by symmetry

        if(flg) then
          do i = 1,nst
            do j = i,nst
              s(j,i) = s(i,j)
            end do
          end do
        endif

c     Output stresses

      elseif(isw.eq.4 .or.isw.eq.8) then

        l = d(5)
        call int2d(l,lint,sg)
        nn   =  0

c       Compute element stresses

        call gvec2d(xl,ul,type,ndm,ndf)
        ddm = 0.0d0
        rr = 0.0d0
        zz = 0.0d0
        do i = 1,5
          sig(i) = 0.0d0
          do j = 1,4
            sigv(i,j) = 0.0d0
          end do
        end do
        do l = 1,lint

          call shp2d(sg(1,l),xl,shp(1,1,l),xsj(l),ndm,nel,ix,.false.)

c         Compute displacement gradient and incr. displ. gradient

          call strn2m(d,xr1,shp(1,1,l),xl,ul,tl,type,xr0,xz0,
     &                ndm,ndf,nel,nen,ta,eps)

c         Compute Cauchy stresses and spatial tangent tensor at t-n+1

          call modlsd(d,ta,eps,hr(nn+nh1),hr(nn+nh2),nhv,istrt,
     &                aa,sigv(1,l),isw)
          ddm = ddm + sigv(7,l)*0.25
          rr = rr + xr0*0.25
          zz = zz + xz0*0.25

          if(d(14).gt.0.0d0 .and. d(14).ne.1.d0) then
            do ii = 1,4
              sigv(ii,l) = sigv(ii,l)/d(14)
            end do
          endif

c         Move stresses for printing

          sig(1) = sig(1) + 0.25d0*sigv(1,l)
          sig(2) = sig(2) + 0.25d0*sigv(2,l)
          sig(3) = sig(3) + 0.25d0*sigv(3,l)
          sig(4) = sig(4) + 0.25d0*sigv(4,l)
          nn = nn + nhv
        end do

        if(isw.eq.8) then

c         Stress computations for nodes

          call stcn2d(ix,sigv,shp,xsj,hr(nph),hr(nph+numnp),hr(ner),
     &                erav,lint,nel,9,numnp)

        else

c         Output stresses

          call pstr2d(sig,sig(5))

          mct = mct - 2
          if(mct.le.0) then
            write(iow,2001) o,head
            if(ior.lt.0) then
              write(*,2001) o,head
            endif
            mct = 50
          end if
          write(iow,2002) n,ma,(sig(ii),ii=1,6),rr,zz,ddm,sig(7)
          if(ior.lt.0) then
            write(*,2002) n,ma,(sig(ii),ii=1,6),rr,zz,ddm,sig(7)
          endif

        endif

      endif

c     Formats for input-output

2001  format(a1,20a4//'  element stresses'//'  elmt  matl  11-stress'
     &,'  22-stress  33-stress  12-stress   1-stress   2-stress'/
     & '  1-coord  2-coord  yield ? ',34x,'angle')

2002  format(2i6,6e11.3/2f9.3,f10.4,31x,f8.2/1x)

      end

      subroutine sld2d3(d,ul,xl,ix,tl,s,p,ndf,ndm,nst,isw)

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose:  Plane/axisymmetric enhanced strain element for FEAPpv

c      Output records:
c      Prints in element: sig-11, sig-22, sig-33, sig-12, sig-1 sig-2
c                         eps-11, eps-22, eps-33, eps-12

c      Prints at nodes:   1=sig-11, 2=sig-22, 3=sig-33, 4=sig-12
c                         psig-1  , psig-2    (computed by FEAPpv)

c      History Variable Storage (relative to nh1 or nh2)

c      Start           Description             Variable  Length
c      hr(0)           Enhanced displacement      ui(*,1)   5
c      hr(5)           Stress history at point-1    -      nhv
c      hr(5+  nhv)     Stress history at point-2    -      nhv
c      hr(5+2*nhv)     Stress history at point-3    -      nhv
c      hr(5+3*nhv)     Stress history at point-4    -      nhv

c      Total number of words / element is 5 + 4*nhv
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'bdata.h'
      include  'cdata.h'
      include  'eldata.h'
      include  'elplot.h'
      include  'eltran.h'
      include  'hdata.h'
      include  'incshp.h'
      include  'iofile.h'
      include  'pmod2d.h'
      include  'prld1.h'
      include  'prstrs.h'
      include  'rdata.h'
      include  'comblk.h'

      logical   noconv
      integer   ndm,ndf,nst,isw, ix(*)
      real*8    ta,tol1,tolu
      real*8    cfac,lfac,sfac,dmas,lms,cms,xsj0 ,rr,zz
      integer   i,j,l,lint,nhv,nu1,nu2,ni,nn,nenit,i1,j1,i2,j2,ii,jj
      integer   istrt
      real*8    d(*),xl(ndm,*),ul(ndf,nen,*),tl(*),s(nst,*),p(ndf,*)
      real*8    shp(3,10,9),sg(3,16),sig(10,9),eps(9,3)
      real*8    gg(5,8),hh(5,5),bb(5),hg(5,8),dui(5),dd(6,6,5,9)
      real*8    ss(8,8),shpr(9,9),xsj(9),sigl(6),aa(6,6,9)
      real*8    bd(6,2),dvol(9),r0(9)

      save

      data      ni /5/, eps / 27*0.0d0 /

c     Data inputs

      if( isw.eq.1 ) then

        return

      endif

c     Recover enhanced modes (saved in last iteration)

      nhv   = nint(d(15))
      istrt = nint(d(84))
      nu1   = nh1 - 1
      nu2   = nh2 - 1
      do i = 1,5
        ui(i,1)   = hr(nu2+i)
        ui(i,2)   = 0.0d0
      end do ! i

c     Compute quadrature and weights

      l = nint(d(5))
      call int2d(l,lint,sg)

c     Initialize history variables only

      if(isw.eq.14) then
        nn = ni
        do l = 1,lint
          call modlsd(d,ta,eps,hr(nn+nh1),hr(nn+nh2),nhv,istrt,
     &                dd(1,1,1,l),sig(1,l),isw)
          nn = nn + nhv
        end do ! l
        return
      endif ! isw.eq.14

c     Compute shape functions

      do l = 1,lint
        call shp2d(sg(1,l),xl,shp(1,1,l),xsj(l),ndm,nel,ix,.false.)

c       Axisymmetry

        if(stype.eq.3) then
          r0(l)   = shp(3,1,l)*xl(1,1) + shp(3,2,l)*xl(1,2)
     &            + shp(3,3,l)*xl(1,3) + shp(3,4,l)*xl(1,4)
          dvol(l) = xsj(l)*sg(3,l)*r0(l)
          do j = 1,nel
            shpr(j,l) = shp(3,j,l)/r0(l)
          end do ! j

c       Plane

        else
          dvol(l) = xsj(l)*sg(3,l)
          do j = 1,nel
            shpr(j,l) = 0.0d0
          end do ! j
        endif
      end do ! l

c     Compute enhanced modes by local iteration

      tolu   =  1.d-03*tol*rnmax/dble(numel)
      nenit  =  0
      noconv = .true.
      do while(noconv)

        do j = 1,5
          bb(j) = 0.0d0
          do i = 1,5
            hh(i,j) = 0.0d0
          end do ! i
        end do ! j

c       Set initial counter for history terms in stress/strain

        nn = ni
        do l = 1,lint

c         Compute enhanced strain shape functions

          call shpi2d(sg(1,l),xsj(l),xl,ndm)

c         Compute strain at point

          call strn2d(d,xl,ul,tl,shp(1,1,l),ndf,ndm,nel,rr,zz,ta,eps)

c         Compute stress and tangent moduli

          call modlsd(d,ta,eps,hr(nn+nh1),hr(nn+nh2),nhv,istrt,
     &                dd(1,1,1,l),sig(1,l),isw)

c         Scale moduli and stresses

          do i = 1,4
            do j = 1,4
              aa(j,i,l) = dd(j,i,1,l)*dvol(l)*ctan(1)
            end do ! j
            sigl(i) = sig(i,l)*dvol(l)
          end do ! i

c         Store time history plot data for element

          i = 6*(l-1)
          do j = 1,6
            tt(j+i)   = sig(j,l)
          end do ! j

c         Enhanced residual computations

          do j = 1,2
            bb(2*j-1) = bb(2*j-1) - sigl(1)*shpi(1,j)
     &                            - sigl(4)*shpi(2,j)
            bb(2*j  ) = bb(2*j  ) - sigl(2)*shpi(2,j)
     &                            - sigl(4)*shpi(1,j)
          end do ! j

          if(stype.eq.3) then
            shpi(3,3) = shpi(3,3)/r0(l)
            bb(5)     = bb(5) - sigl(3)*shpi(3,3)
            do j = 1,2
              bd(3,1)     = aa(3,1,l)*shpi(1,j) + aa(3,4,l)*shpi(2,j)
              bd(3,2)     = aa(3,2,l)*shpi(2,j) + aa(3,4,l)*shpi(1,j)
              hh(5,2*j-1) = hh(5,2*j-1) + shpi(3,3)*bd(3,1)
              hh(5,2*j  ) = hh(5,2*j  ) + shpi(3,3)*bd(3,2)
              bd(1,1)     = aa(1,3,l)*shpi(3,3)
              bd(2,1)     = aa(2,3,l)*shpi(3,3)
              bd(4,1)     = aa(4,3,l)*shpi(3,3)
              hh(2*j-1,5) = hh(2*j-1,5) + shpi(1,j)*bd(1,1)
     &                                  + shpi(2,j)*bd(4,1)
              hh(2*j  ,5) = hh(2*j  ,5) + shpi(2,j)*bd(2,1)
     &                                  + shpi(1,j)*bd(4,1)
            end do ! j
            hh(5,5) = hh(5,5) + shpi(3,3)*aa(3,3,l)*shpi(3,3)
          else
            shpi(3,3) = 0.0d0
            hh(5,5)   = 1.0d0
          endif

c         Stiffness computations

          do j = 1,2

c           Compute d * b matrix = a

            bd(1,1) = aa(1,1,l)*shpi(1,j) + aa(1,4,l)*shpi(2,j)
            bd(2,1) = aa(2,1,l)*shpi(1,j) + aa(2,4,l)*shpi(2,j)
            bd(4,1) = aa(4,1,l)*shpi(1,j) + aa(4,4,l)*shpi(2,j)
            bd(1,2) = aa(1,2,l)*shpi(2,j) + aa(1,4,l)*shpi(1,j)
            bd(2,2) = aa(2,2,l)*shpi(2,j) + aa(2,4,l)*shpi(1,j)
            bd(4,2) = aa(4,2,l)*shpi(2,j) + aa(4,4,l)*shpi(1,j)
            do i = 1,2
              hh(2*i-1,2*j-1) = hh(2*i-1,2*j-1) + shpi(1,i)*bd(1,1)
     &                                          + shpi(2,i)*bd(4,1)
              hh(2*i-1,2*j  ) = hh(2*i-1,2*j  ) + shpi(1,i)*bd(1,2)
     &                                          + shpi(2,i)*bd(4,2)
              hh(2*i  ,2*j-1) = hh(2*i  ,2*j-1) + shpi(2,i)*bd(2,1)
     &                                          + shpi(1,i)*bd(4,1)
              hh(2*i  ,2*j  ) = hh(2*i  ,2*j  ) + shpi(2,i)*bd(2,2)
     &                                          + shpi(1,i)*bd(4,2)
            end do ! i
          end do ! j
          nn = nn + nhv
        end do ! l

        call invert(hh,5,5)

c       Compute incremental enhanced displacements enhanced modes

        do i = 1,5
          dui(i)  = hh(i,1)*bb(1) + hh(i,2)*bb(2) + hh(i,3)*bb(3)
     &            + hh(i,4)*bb(4) + hh(i,5)*bb(5)
          ui(i,1) = ui(i,1) + dui(i)
          ui(i,2) = ui(i,2) + dui(i)
        end do ! i

c       Check convergence

        tol1 = abs(bb(1)*dui(1) + bb(2)*dui(2) + bb(3)*dui(3)
     &           + bb(4)*dui(4) + bb(5)*dui(5))

        if(tol1.le.tolu .and. nenit.ge.1) then
          noconv = .false.
        endif
        nenit = nenit + 1
        if(nenit.ge.3 .or. tolu.eq.0.0d0) then
          noconv = .false.
        endif
      end do ! while

c     Save enhanced modes

      do i = 1,5
        hr(nu2+i) = ui(i,1)
      end do ! i

c     Set initial counter for history terms in stress/strain

      if(isw.eq.3. or. isw.eq.6) then

c       Time integration order set to static or dynamic

        if(d(7).ge.0.0) then
          cfac = d(7)
          lfac = 1.0d0 - cfac
        else
          cfac = 0.0d0
          lfac = 0.0d0
        endif

        do i = 1,8
          do j = 1,5
            gg(j,i) = 0.0d0
          end do ! j
        end do ! i

        do l = 1,lint

          call shpi2d(sg(1,l),xsj(l),xl,ndm)

          if(stype.eq.3) then
            shpi(3,3) = shpi(3,3)/r0(l)
            xsj0      = xsj(l)*sg(3,l)
          else
            shpi(3,3) = 0.0d0
            xsj0      = 0.0d0
          endif

c         Rayleigh damping effects

          if(d(78).ne.0.0d0) then
            call rays2d(d,shp(1,1,l),sig(1,l),dd(1,1,5,l),ul(1,1,4),
     &                  xl,ndf,ndm,nel)
            sfac = d(78)*ctan(2)
          else
            sfac = 0.0d0
          endif

c         Residual computations

          call resid2d(cfac,lfac,dvol(l),xsj0,shp(1,1,l),eps,
     &                 sig(1,l),d,ul(1,1,4),ul(1,1,5), p,ndf,l)

c         Stiffness computations

          if(isw.eq.3) then

            dmas = d(4)*(ctan(3) + d(77)*ctan(2))*dvol(l)

            do j = 1,4
              do i = 1,4
                aa(i,j,l) = aa(i,j,l) + dd(i,j,5,l)*sfac
              end do ! i
            end do ! j

            j1 = 0
            j2 = 0
            do j = 1,nel

c             Compute d * b matrix = a

              do i = 1,4
                bd(i,1) = aa(i,1,l)*shp(1,j,l) + aa(i,4,l)*shp(2,j,l)
     &                  + aa(i,3,l)*shpr(j,l)
                bd(i,2) = aa(i,2,l)*shp(2,j,l) + aa(i,4,l)*shp(1,j,l)
              end do ! i

c             Lumped mass effects

              lms = shp(3,j,l)*dmas
              do jj = 1,2
                s(j1+jj,j1+jj) = s(j1+jj,j1+jj) + lms*lfac
              end do ! jj

              i1 = 0
              do i = 1,nel
                cms = lms*shp(3,i,l)*cfac
                do jj = 1,2
                  s(i1+jj,j1+jj) = s(i1+jj,j1+jj) + cms
                  s(i1+1 ,j1+jj) = s(i1+1 ,j1+jj) + shp(1,i,l)*bd(1,jj)
     &                                            + shp(2,i,l)*bd(4,jj)
     &                                            + shpr(i,l) *bd(3,jj)
                  s(i1+2 ,j1+jj) = s(i1+2 ,j1+jj) + shp(2,i,l)*bd(2,jj)
     &                                            + shp(1,i,l)*bd(4,jj)
                end do ! jj
                i1 = i1 + ndf
              end do ! i

c             Enhanced coupling array

              do jj = 1,2
                do i = 1,2
                  gg(2*i-1,j2+jj) = gg(2*i-1,j2+jj) + shpi(1,i)*bd(1,jj)
     &                                              + shpi(2,i)*bd(4,jj)

                  gg(2*i  ,j2+jj) = gg(2*i  ,j2+jj) + shpi(2,i)*bd(2,jj)
     &                                              + shpi(1,i)*bd(4,jj)
                end do ! i
                gg(5,j2+jj) = gg(5,j2+jj) + shpi(3,3)*bd(3,jj)
              end do ! jj
              j1 = j1 + ndf
              j2 = j2 + 2
            end do ! j
          endif
        end do ! l

c       Eliminate enhanced modes

        do i = 1,5
          do j = 1,8
            hg(i,j) = hh(1,i)*gg(1,j) + hh(2,i)*gg(2,j)
     &              + hh(3,i)*gg(3,j) + hh(4,i)*gg(4,j)
     &              + hh(5,i)*gg(5,j)
          end do ! j
        end do ! i

        if(isw.eq.3) then
          do j = 1,8
            do i = 1,8
              ss(i,j) = gg(1,i)*hg(1,j) + gg(2,i)*hg(2,j)
     &                + gg(3,i)*hg(3,j) + gg(4,i)*hg(4,j)
     &                + gg(5,i)*hg(5,j)
            end do ! i
          end do ! j

c         Construct static condensation

          j1 = 0
          j2 = 0
          do j = 1,4
            i1 = 0
            i2 = 0
            do i = 1,4
              do jj = 1,2
                do ii = 1,2
                  s(i1+ii,j1+jj) = s(i1+ii,j1+jj) - ss(i2+ii,j2+jj)
                end do ! ii
              end do ! jj
              i1 = i1 + ndf
              i2 = i2 + 2
            end do ! i
            j1 = j1 + ndf
            j2 = j2 + 2
          end do ! j

c         Compute reduced residual

          j2 = 0
          do j = 1,4
            do jj = 1,2
              p(jj,j) = p(jj,j) - hg(1,j2+jj)*bb(1) - hg(2,j2+jj)*bb(2)
     &                          - hg(3,j2+jj)*bb(3) - hg(4,j2+jj)*bb(4)
     &                          - hg(5,j2+jj)*bb(5)
            end do ! jj
            j2     = j2 + 2
          end do ! j
        endif

c       Multiply by thickness if not unity

        if((isw.eq.3 .or. isw.eq.6) .and. d(14).ne.1.d0) then

          do j = 1,nst
            do i = 1,nst
              s(i,j) = s(i,j)*d(14)
            end do ! i
          end do ! j
          do j = 1,nel
            do i = 1,2
              p(i,j) = p(i,j)*d(14)
            end do ! i
          end do ! j

        endif

c     Compute and output element variables

      elseif(isw.eq.4 .or. isw.eq.8) then

        l = d(5)
        if(l*l.ne.lint) call int2d(l,lint,sg)

c       Set initial counter for history terms in stress/strain

        nn = ni
        do l = 1,lint
          call shp2d(sg(1,l),xl,shp(1,1,l),xsj(l),ndm,nel,ix,.false.)
          call shpi2d(sg(1,l),xsj(l),xl,ndm)

c         Compute stress and strain at point

          call strn2d(d,xl,ul,tl,shp(1,1,l),ndf,ndm,nel,rr,zz,ta,eps)

          call modlsd(d,ta,eps,hr(nn+nh1),hr(nn+nh2),nhv,istrt,
     &                dd,sig(1,l),isw)

c         Compute principal stress values

          if(isw.eq.4) then
            mct = mct - 4
            call pstr2d(sig(1,l),sig(5,l))
            if(mct.le.0) then
              write(iow,2001) o,head
              if(ior.lt.0) write(*,2001) o,head
              mct = 50
            endif
            write(iow,2002) n,ma,rr,zz,(sig(i,l),i=7,9),
     &                     (sig(i,l),i=1,4),(eps(i,1),i=1,4)
            if(ior.lt.0) then
              write(*,2002) n,ma,rr,zz,(sig(i,l),i=7,9),
     &                     (sig(i,l),i=1,4),(eps(i,1),i=1,4)
            endif
          endif
          nn = nn + nhv
        end do ! l

c       Plot stress values

        if(isw.eq.8) then
          call stcn2d(ix,sig,shp,xsj,hr(nph),hr(nph+numnp),hr(ner),
     &                erav,lint,nel,10,numnp)
        endif

      endif

c     Formats

2001  format(a1,20a4//5x,'Element Stresses'//'     Elmt Mat',
     &    4x,'1-coord    2-coord   1-stress   2-stress      Angle'/
     &   15x,'11-stress  22-stress  33-stress  12-stress',
     &   15x,'11-strain  22-strain  33-strain  12-strain'/39(' -'))
2002  format(i9,i4,0p,2f11.3,1p,3e11.3/13x,1p,4e11.3/13x,1p,4e11.3/)

      end

      subroutine stcn2d(ix,sig,shp,xsj,dt,st,ser,erav,
     &                  lint,nel,nen,numnp)

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose:

c      Inputs:

c      Outputs:
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      integer   nel,nen,numnp
      integer   i,l,ll,lint
      real*8    xg, erav

      integer   ix(*)
      real*8    dt(numnp),st(numnp,*),ser(*)
      real*8    xsj(*),shp(3,nen,*),sig(nen,*)

      save

c     Lumped and consistent projection routine

      do l = 1,lint

c       Compute lumped projection and assemble stress integrals

        do i = 1,nel
          ll = ix(i)
          if(ll.gt.0) then

            xg     = shp(3,i,l)*xsj(l)
            dt(ll) = dt(ll) + xg

c           Stress projections

            st(ll,1) = st(ll,1) + sig(1,l)*xg
            st(ll,2) = st(ll,2) + sig(2,l)*xg
            st(ll,3) = st(ll,3) + sig(3,l)*xg
            st(ll,4) = st(ll,4) + sig(4,l)*xg

c           Error estimation projection

            ser(ll)  = ser(ll)  + erav*xg

          endif
        end do
      end do

      end

      subroutine strn2d(d,xl,ul,tl,shp,ndf,ndm,nel,xx,yy,ta,eps)

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose:

c      Inputs:

c      Outputs:
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'cdata.h'
      include  'incshp.h'
      include  'pmod2d.h'

      integer   ndf,ndm,nel, j
      real*8    xx,yy,ta
      real*8    d(*),xl(ndm,*),ul(ndf,nen,*),tl(*),shp(3,*)
      real*8    eps(9,3)

      save

c     Compute strains and coordinates

      do j = 1,6
        eps(j,1) = 0.0d0
        eps(j,3) = 0.0d0
      end do
      xx = 0.0d0
      yy = 0.0d0
      ta = -d(9)
      do j = 1,nel
        xx = xx + shp(3,j)*xl(1,j)
        yy = yy + shp(3,j)*xl(2,j)
        ta = ta + shp(3,j)*tl(j)
        eps(1,1) = eps(1,1) + shp(1,j)*ul(1,j,1)
        eps(2,1) = eps(2,1) + shp(2,j)*ul(2,j,1)
        eps(3,1) = eps(3,1) + shp(3,j)*ul(1,j,1)
        eps(4,1) = eps(4,1) + shp(2,j)*ul(1,j,1)
     &                      + shp(1,j)*ul(2,j,1)
        eps(1,3) = eps(1,1) + shp(1,j)*ul(1,j,2)
        eps(2,3) = eps(2,1) + shp(2,j)*ul(2,j,2)
        eps(3,3) = eps(3,1) + shp(3,j)*ul(1,j,2)
        eps(4,3) = eps(4,1) + shp(2,j)*ul(1,j,2) + shp(1,j)*ul(2,j,2)
      end do

c     Compute enhanced strains

      if(etype.eq.3) then
        do j = 1,2
          eps(1,1) = eps(1,1) + shpi(1,j)*ui(2*j-1,1)
          eps(2,1) = eps(2,1) + shpi(2,j)*ui(2*j,1)
          eps(4,1) = eps(4,1) + shpi(1,j)*ui(2*j,1)
     &                        + shpi(2,j)*ui(2*j-1,1)
          eps(1,3) = eps(1,3) + shpi(1,j)*ui(2*j-1,2)
          eps(2,3) = eps(2,3) + shpi(2,j)*ui(2*j,2)
          eps(4,3) = eps(4,3) + shpi(1,j)*ui(2*j,2)
     &                        + shpi(2,j)*ui(2*j-1,2)
        end do
      endif

c     Strain at t_n

      eps(1,2) = eps(1,1) - eps(1,3)
      eps(2,2) = eps(2,1) - eps(2,3)
      eps(3,2) = eps(3,1) - eps(3,3)
      eps(4,2) = eps(4,1) - eps(4,3)

c     Set 3-strain (thickness/hoop)

      if(stype.eq.3) then
        eps(3,1) = eps(3,1)/xx
        eps(3,2) = eps(3,2)/xx
        eps(3,3) = eps(3,3)/xx
      else
        eps(3,1) = 0.0d0
        eps(3,2) = 0.0d0
        eps(3,3) = 0.0d0
      endif

      end

      subroutine strn2m(d,xr1,shp,xl,ul,tl,type,xr0,xz0,ndm,ndf,
     &                  nel,nen,ta,ep)

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose:

c      Inputs:

c      Outputs:
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'elm2d.h'

      integer   ndm,ndf,nel,nen,k
      real*8    type,xr1,xr0,xz0,ur,ta, theta

      real*8    d(*),shp(3,*),xl(ndm,*),ul(ndf,nen),tl(*),ep(4)

      save

c     Compute strain tensor for constitutive equations

      ur    = 0.0d0
      xr0   = 0.0d0
      xz0   = 0.0d0
	  ta    = -d(9)
      ep(1) = 0.0d0
      ep(2) = 0.0d0
      ep(4) = 0.0d0
      do k = 1,nel
        xr0   = xr0   + shp(3,k)*xl(1,k)
        xz0   = xz0   + shp(3,k)*xl(2,k)
        ta    = ta    + shp(3,k)*tl(k)
        ur    = ur    + shp(3,k)*ul(1,k)
        ep(1) = ep(1) + shp(1,k)*ul(1,k)
        ep(2) = ep(2) + shp(2,k)*ul(2,k)
        ep(4) = ep(4) + shp(1,k)*ul(2,k)
     &                + shp(2,k)*ul(1,k)
      end do
      xr1   = 1.0 + type*(xr0 - 1.0)
      ep(3) = type*ur/xr1

c     Correct strains and incremental strains for mixed formulation

      theta = 0.333333333333333d0*(trep - ep(1) - ep(2) - ep(3))
      ep(1) = ep(1) + theta
      ep(2) = ep(2) + theta
      ep(3) = ep(3) + theta

      end
