c$Id:$
      subroutine solid3d(d,ul,xl,ix,tl,s,p,ndf,ndm,nst,isw)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c     Three Dimensional Solid Element Driver
c_____________________________________________________________________c

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
      integer   i,tdof

      integer   ix(*)
      real*8    d(*),ul(ndf,*),xl(ndm,*),tl(*),s(nst,nst),p(nst)
      real*8    shp(4,8),th(8)

      save

c     Extract type data

      stype = d(16)
      etype = d(17)
      dtype = d(18)
      hflag = d(30).eq.1.d0

c     Set nodal temperatures: Can be specified or computed

      if(isw.gt.1) then
        tdof = d(19)
        if(tdof.le.0) then
          do i = 1,nel ! {
            th(i) = tl(i)
          end do ! i     }
        else
          do i = 1,nel ! {
            th(i) = ul(tdof,i)
          end do ! i     }
        endif
      endif

c     Go to correct process

      go to(1,2,3,3,5,3,3,3,3,3,3,3), isw

c     Output element type

      if(isw.eq.0 .and. ior.lt.0) then
        write(*,*) '   Elmt  1: 3-d Solid Linear/Finite Defm. Element.'
      endif

      return

c     Input material properties

1     write(iow,2001)
      if(ior.lt.0) write(*,2001)
      call inmate(d,tdof,  0 ,1)
      nh1 = nh1 + 2
      if(etype.eq.3) then
        nh1 = nh1 + 12
      endif

c     Set tdof to zero if 1, 2, 3, or larger than ndf

      if(tdof.gt.ndf) then
        write(iow,3003)
        if(ior.lt.0) write(*,3003)
        tdof = 0
      elseif(tdof.ge.1 .and. tdof.le.3) then
        write(iow,3004)
        if(ior.lt.0) write(*,3004)
        tdof = 0
      endif

c     Deactivate dof in element for dof > 3

      do i = 4,ndf
        ix(i) = 0
      end do

c     If temperature dof is specified activate dof

      if(tdof.gt.0) then
        ix(tdof) = 1
      endif

c     Set plot sequence for 4-nod tet or 8-node brick

      if(nen.eq.4) then
        call pltet4(iel)
      else
        call plbrk8(iel)
      endif

      return

c     Check element for errors

2     call ckbrk8 ( n, ix, xl, ndm, nel, shp )

      return

c     Compute stress-divergence vector (p) and stiffness matrix (s)

3     if(etype.eq.1) then

c       Displacement Model

        if(dtype.gt.0) then
          call sld3d1(d,ul,xl,ix,th,s,p,ndf,ndm,nst,isw)
        endif

      endif

      return

c     Compute mass or geometric striffness matrix

5     if(imtyp.eq.1) then
        call mass3d(d,xl,s,p,ndf,ndm,nst)
      else
c       Geometric stiffness here
      endif
      return

c     Formats for input-output

2001  format(
     & /5x,'T h r e e   D i m e n s i o n a l   S o l i d',
     &     '   E l e m e n t'/)
3003  format(' *WARNING* Thermal d.o.f. > active d.o.f.s : Set to 0')
3004  format(' *WARNING* Thermal d.o.f. can not be 1 to 3: Set to 0')

      end

      subroutine mass3d(d,xl,s,p,ndf,ndm,nst)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2006: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Compute mass matrix for 3-d tet and brick elements

c      Inputs:
c         d(*)      - Material set parameters
c         xl(ndm,*) - Nodal coordinates for element
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

      real*8    xsj,dv, aj1,lfac,cfac

      real*8    d(*),xl(ndm,*),s(nst,nst),p(nst)
      real*8    shp(4,8),sg(4,8),sv(5,4)

      save

c     Compute mass matrix

      if(nel.eq.4) then
        l    =  2
        quad = .false.
        call tint3d(l,lint,sv)
      else
        l    = d(5)
        quad = .true.
        call int3d(l,lint,sg)
      endif

c     Set mass interpolation factor between consistent (1) and lumped (0)

      cfac = d(7)
      lfac = 1.d0 - cfac

      do l = 1,lint

c       Compute shape functions

        if(quad) then
          call shp3d(sg(1,l),xsj,shp,xl,ndm)
          dv = sg(4,l)*xsj*d(4)
        else
          call tetshp(sv(1,l),xl,ndm,xsj,shp)
          dv = sv(5,l)*xsj*d(4)
        endif

c       For each node j compute db = rho*shape*dv

        j1 = 1
        do j = 1,nel
          aj1 = shp(4,j)*dv

c         Compute a lumped mass

          p(j1)    = p(j1)    + aj1
          s(j1,j1) = s(j1,j1) + aj1*lfac
          aj1      = aj1*cfac

c         For each node k compute mass matrix (upper triangular part)

          k1 = 1
          do k = 1,nel
            s(j1,k1) = s(j1,k1) + shp(4,k)*aj1
            k1 = k1 + ndf
          end do
          j1 = j1 + ndf
        end do
      end do

c     Compute missing parts and lower part by symmetries

      do j = 1,ndf*nel,ndf
        p(j+1) = p(j)
        p(j+2) = p(j)
        do k = 1,ndf*nel,ndf
          s(j+1,k+1) = s(j,k)
          s(j+2,k+2) = s(j,k)
        end do
      end do

      end

      subroutine rays3d(d,shp,shpbar,sig,dr,vl,ndf,nel,mixed)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2006: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c     Purpose: Stiffness proportional Rayleigh damping residual

      implicit  none

      logical   mixed
      integer   ndf,nel, i,j
      real*8    theta,dtheta

      real*8    d(*),shp(4,*),shpbar(3,*),sig(*),dr(6,6),vl(ndf,*)
      real*8    eps(6)

c     Compute strain rate terms

      do j = 1,6
        eps(j) = 0.0d0
      end do
      do j = 1,nel
        eps(1) = eps(1) + shp(1,j)*vl(1,j)
        eps(2) = eps(2) + shp(2,j)*vl(2,j)
        eps(3) = eps(3) + shp(3,j)*vl(3,j)
        eps(4) = eps(4) + shp(2,j)*vl(1,j) + shp(1,j)*vl(2,j)
        eps(5) = eps(5) + shp(3,j)*vl(2,j) + shp(2,j)*vl(3,j)
        eps(6) = eps(6) + shp(1,j)*vl(3,j) + shp(3,j)*vl(1,j)
      end do ! j

c     Modify if mixed

      if(mixed) then

c       Mixed volume change

        theta  = 0.0d0
        do j = 1,nel
          theta  = theta  + shpbar(1,j)*vl(1,j)
     &                    + shpbar(2,j)*vl(2,j)
     &                    + shpbar(3,j)*vl(3,j)
        end do

c       Mixed strains

        dtheta = 0.3333333333333333d0*(theta - eps(1) - eps(2) - eps(3))
        eps(1) = eps(1) + dtheta
        eps(2) = eps(2) + dtheta
        eps(3) = eps(3) + dtheta

      endif

c     Compute stress modification due to Rayleigh damping

      do j = 1,6
        eps(j) = eps(j)*d(78)
        do i = 1,6
          sig(i) = sig(i) + dr(i,j)*eps(j)
        end do ! i
      end do ! j

      end

      subroutine resid3d(xsj,shp,sig,d,vl,al,p,ndf,l)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2006: Robert L. Taylor
c                               All rights reserved

c     3-D residual routine

      implicit  none

      include  'eldata.h'
      include  'elplot.h'
      include  'eltran.h'
      include  'prld1.h'

      integer   ndf,l, j,k
      real*8    xsj

      real*8    b1,b2,b3,rr
      real*8    aj1,aj2,aj3,aj0,lfac,cfac

      real*8    d(*),vl(ndf,*),al(ndf,*),p(ndf,*)
      real*8    shp(4,*),sig(*),ac(3),vc(3)

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

      if(int(d(76)).gt.0) then
        b3 = d(13) + prldv(int(d(76)))*d(73)
      else
        b3 = d(13)*dm
      endif

      rr   = d(4)
      if(d(7).ge.0.0d0) then
        cfac = d(7)
        lfac = 1.d0 - cfac
      else
        cfac = 0.0d0
        lfac = 0.0d0
      endif

c     Store time history plot data for element

      k = 6*(l-1)
      do j = 1,6
        tt(j+k) = sig(j)
      end do ! j

c     Compute accelerations

      ac(1) = 0.0d0
      ac(2) = 0.0d0
      ac(3) = 0.0d0
      do j = 1,nel
        ac(1) = ac(1) + shp(4,j)*al(1,j)
        ac(2) = ac(2) + shp(4,j)*al(2,j)
        ac(3) = ac(3) + shp(4,j)*al(3,j)
      end do ! j
      ac(1)   = rr*ac(1)*cfac
      ac(2)   = rr*ac(2)*cfac
      ac(3)   = rr*ac(3)*cfac

c     For Rayleigh Mass Damping: Compute velocity

      if(d(77).ne.0.0d0) then
        vc(1) = 0.0d0
        vc(2) = 0.0d0
        vc(3) = 0.0d0
        do j = 1,nel
          vc(1) = vc(1) + shp(4,j)*vl(1,j)
          vc(2) = vc(2) + shp(4,j)*vl(2,j)
          vc(3) = vc(3) + shp(4,j)*vl(3,j)
        end do ! j
        vc(1)   = vc(1)*cfac
        vc(2)   = vc(2)*cfac
        vc(3)   = vc(3)*cfac

        do j = 1,nel
          aj0    = shp(4,j)*xsj*rr*d(77)
          p(1,j) = p(1,j) - (vc(1) + lfac*vl(1,j))*aj0
          p(2,j) = p(2,j) - (vc(2) + lfac*vl(2,j))*aj0
          p(3,j) = p(3,j) - (vc(3) + lfac*vl(3,j))*aj0
        end do ! j

      endif

c     Loop over rows

      do j = 1,nel
        aj1 = shp(1,j)*xsj
        aj2 = shp(2,j)*xsj
        aj3 = shp(3,j)*xsj
        aj0 = lfac*rr

c       Compute gravity, thermal, inertia, and stress contributions

        p(1,j) = p(1,j) + (b1 - ac(1) - aj0*al(1,j))*shp(4,j)*xsj
     &                  - aj1*sig(1)  - aj2*sig(4)  - aj3*sig(6)
        p(2,j) = p(2,j) + (b2 - ac(2) - aj0*al(2,j))*shp(4,j)*xsj
     &                  - aj1*sig(4)  - aj2*sig(2)  - aj3*sig(5)
        p(3,j) = p(3,j) + (b3 - ac(3) - aj0*al(3,j))*shp(4,j)*xsj
     &                  - aj1*sig(6)  - aj2*sig(5)  - aj3*sig(3)

      end do ! j

      end

      subroutine sld3d1(d,ul,xl,ix,tl,s,p,ndf,ndm,nst,isw)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2006: Robert L. Taylor
c                               All rights reserved

c_____________________________________________________________________c
c     3-D linear elastic displacment element for feap

c     Output records:

c     Prints in element: sig-11, sig-22, sig-33, sig-12, sig-23, sig-31
c                        eps-11, eps-22, eps-33, eps-12, eps-23, eps-31

c     Prints at nodes:   1=sig-11, 2=sig-22, 3=sig-33,
c                        4=sig-12  5=sig-23, 6=sig-31

c_____________________________________________________________________c

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

c     Set nodal temperatures: Can be specified or computed

      if(isw.gt.1) then
        tdof = d(19)
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

c     Transfer to correct processor

      go to (1,1,3,3,1,3,1,3,1,1,1,3), isw

c     Return

1     return

c     Compute element tangent array

3     l = 2

c     Set number of history terms

      nhv   = nint(d(15))
      istrt = nint(d(84))

      if(mod(isw,3).eq.0) then

c       Set body loading factors

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

c       Set mass factors

        rr   = d(4)
        if(d(7).ge.0.0d0) then
          cfac = d(7)
          lfac = 1.d0 - cfac
        else
          cfac = 0.0d0
          lfac = 0.0d0
        endif

c       Get quadrature information

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

c         Compute strain at point

          call strn3d(d,ul,th,shp(1,1,l),ndf,nel, eps,ta)

c         Compute stress at point

          call modlsd(d,ta,eps,hr(nh1+nn),hr(nh2+nn),nhv,istrt,
     &                dd,sig,isw)

c         Residual computations

          if(isw.eq.3 .or.isw.eq.6) then

c           Add stiffness part of Rayleigh damping to stress

            if(d(78).ne.0.0d0) then
              call rays3d(d,shp(1,1,l),shp(1,1,l),sig,dd,ul(1,1,4),
     &                    ndf,nel,.false.)
            endif

c           Form residual

            call resid3d(dv,shp(1,1,l),sig,d,
     &                   ul(1,1,4),ul(1,1,5),p,ndf,l)
          endif

c         Stiffness computations

          if(isw.eq.3) then

            dvm   = rr*(ctan(3) + d(77)*ctan(2))*dv
            dv    =    (ctan(1) + d(78)*ctan(2))*dv

            j1 = 1
            do j = 1,nel

c             Compute d * b matrix = a

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

c             Add diagonal mass effects

              am           = shp(4,j,l)*dvm
              s(j1  ,j1  ) = s(j1  ,j1  ) + am*lfac
              s(j1+1,j1+1) = s(j1+1,j1+1) + am*lfac
              s(j1+2,j1+2) = s(j1+2,j1+2) + am*lfac

              if(isw.eq.3) then
                i1 = 1
                do i = 1,j

c                 Compute consistent mass matrix

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

c       Construct symmetric part

        if(isw.eq.3) then
          do i = 2,ndf*nel
            do j = 1,i
              s(i,j) = s(j,i)
            end do
          end do
        endif
      endif

c     Compute and output element variables

      if(isw.eq.4) then

        if(nel.eq.4) then
          call tint3d(l,lint,sv)
        else
          call int3d(l,lint,sg)
        endif

c       Set initial counter for history terms in stress/strain

        nn = 0
        do l = 1,lint
          call shp3d(sg(1,l),xsj,shp,xl,ndm)
          if(nel.eq.4) then
            call tetshp(sv(1,l),xl,ndm,xsj,shp)
          else
            call shp3d(sg(1,l),xsj,shp,xl,ndm)
          endif

c         Compute strain at point

          call strn3d(d,ul,th,shp,ndf,nel, eps,ta)

c         Compute stress at point

          call modlsd(d,ta,eps,hr(nh1+nn),hr(nh2+nn),nhv,istrt,
     &                dd,sig,isw)

c         Compute coordinates

          xn = 0.0
          yn = 0.0
          zn = 0.0
          do j = 1,nel
            xn = xn + shp(4,j,1)*xl(1,j)
            yn = yn + shp(4,j,1)*xl(2,j)
            zn = zn + shp(4,j,1)*xl(3,j)
          end do

c         Compute principal stress values

          mct = mct - 3
          if(mct.le.0) then
            write(iow,2010) o,head
            if(ior.lt.0) write(*,2010) o,head
            mct = 50
          endif
          write(iow,2011) n,xn,(sig(i),i=1,6),ma,yn,(eps(i,1),i=1,6)
          if(ior.lt.0) then
            write(*,2011) n,xn,(sig(i),i=1,6),ma,yn,(eps(i,1),i=1,6)
          end if
          nn = nn + nhv
        end do

c     Plot stress values

      elseif(isw.eq.8) then
        call stcn3d(ix,d,xl,th,ul,shp,hr(nph),hr(nph+numnp),
     &              ndf,ndm,nel,numnp,nhv,istrt)
      endif
      return

c     Formats

2001  format(
     & /5x,'L i n e a r   E l a s t i c   S o l i d   E l e m e n t'/)

2010  format(a1,20a4//5x,'Element Stresses'//' Elmt 1-coord',
     &    2x,'11-stress  22-stress  33-stress  12-stress',
     &    2x,'23-stress  31-stress'/' matl 2-coord  11-strain',
     &    2x,'22-strain  33-strain  12-strain  23-strain',
     &    2x,'31-strain'/39(' -'))

2011  format(i4,0p1f9.3,1p6e11.3/i4,0p1f9.3,1p6e11.3/)

3000  format(' Input: E, nu, rho, #pts/dir arrays, #pts/dir stress',
     &       ', Enhanced flag','   >',$)

3001  format(' Input: 1-force, 2-force, 3-force, alpha, T-0'/'   >',$)

3002  format(' Input: Ma=4, Sig-ref, z-ref, gamma, horiz-1 horiz-2'/
     &       '   >',$)

3003  format(' *WARNING* Thermal d.o.f. > active d.o.f.s : Set to 0')

3004  format(' *WARNING* Thermal d.o.f. can not be 1 or 2: Set to 0')

      end

      subroutine stcn3d(ix,d,xl,th,ul,shp,dt,st,
     &                     ndf,ndm,nel,numnp,nhv,istrt)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2006: Robert L. Taylor
c                               All rights reserved

c_____________________________________________________________________c

      implicit  none

      include  'hdata.h'
      include  'comblk.h'

      integer   ndf,ndm,nel,numnp,nhv,istrt
      integer   ii, l, ll, lint, nn

      real*8    xsj, xj, ta

      integer   ix(*)
      real*8    dt(numnp),st(numnp,*),xl(ndm,*),th(*),shp(4,8)
      real*8    d(*),sig(8),eps(9,3),dd(6,6,5),ul(ndf,*),sg(4,8)
      real*8    sv(5,4)

      save

c     Compute stress projections to nodes

      l    = 2
      if(nel.eq.4) then
        call tint3d(l,lint,sv)
      else
        call int3d(l,lint,sg)
      endif

c     Set initial counter for history terms in stress/strain

      nn   = 0
      do l = 1,lint
        if(nel.eq.4) then
          call tetshp(sv(1,l),xl,ndm,xsj,shp)
          xsj = xsj*sv(5,l)
        else
          call shp3d(sg(1,l),xsj,shp,xl,ndm)
          xsj = xsj*sg(4,l)
        endif

c       Compute strain at point

        call strn3d(d,ul,th,shp,ndf,nel, eps,ta)

c       Compute stress at point

        call modlsd(d,ta,eps,hr(nh1+nn),hr(nh2+nn),nhv,istrt,
     &              dd,sig, 8)

c       Compute projections: int ( sig * shp(i) * darea )

        do ii = 1,nel
          ll = abs(ix(ii))
          if(ll.ne.0) then
            xj     = xsj*shp(4,ii)
            dt(ll) = dt(ll) + xj
            st(ll,1) = st(ll,1) + sig(1)*xj
            st(ll,2) = st(ll,2) + sig(2)*xj
            st(ll,3) = st(ll,3) + sig(3)*xj
            st(ll,4) = st(ll,4) + sig(4)*xj
            st(ll,5) = st(ll,5) + sig(5)*xj
            st(ll,6) = st(ll,6) + sig(6)*xj
          endif
        end do
        nn = nn + nhv
      end do

      end

      subroutine strn3d(d,ul,th,shp,ndf,nel, eps,ta)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2006: Robert L. Taylor
c                               All rights reserved

c_____________________________________________________________________c

c     Three dimensional strain calculations

      implicit  none

      include  'cdata.h'

      integer   ndf,nel, j
      real*8    ta

      real*8    d(*),ul(ndf,nen,*),th(*),shp(4,*)
      real*8    eps(9,*)

      save

c     Compute stress and strain at point

      do j = 1,9
        eps(j,1) = 0.0d0
        eps(j,2) = 0.0d0
        eps(j,3) = 0.0d0
      end do ! j

c     Compute temperature and coordinates

      ta = -d(9)
      do j = 1,nel
        ta     = ta         + shp(4,j)*th(j)
        eps(1,1) = eps(1,1) + shp(1,j)*ul(1,j,1)
        eps(2,1) = eps(2,1) + shp(2,j)*ul(2,j,1)
        eps(3,1) = eps(3,1) + shp(3,j)*ul(3,j,1)
        eps(4,1) = eps(4,1) + shp(1,j)*ul(2,j,1)
     &                      + shp(2,j)*ul(1,j,1)
        eps(5,1) = eps(5,1) + shp(2,j)*ul(3,j,1)
     &                      + shp(3,j)*ul(2,j,1)
        eps(6,1) = eps(6,1) + shp(3,j)*ul(1,j,1)
     &                      + shp(1,j)*ul(3,j,1)
        eps(1,3) = eps(1,3) + shp(1,j)*ul(1,j,2)
        eps(2,3) = eps(2,3) + shp(2,j)*ul(2,j,2)
        eps(3,3) = eps(3,3) + shp(3,j)*ul(3,j,2)
        eps(4,3) = eps(4,3) + shp(1,j)*ul(2,j,2) + shp(2,j)*ul(1,j,2)
        eps(5,3) = eps(5,3) + shp(2,j)*ul(3,j,2) + shp(3,j)*ul(2,j,2)
        eps(6,3) = eps(6,3) + shp(3,j)*ul(1,j,2) + shp(1,j)*ul(3,j,2)
      end do
      do j = 1,6
        eps(j,2) = eps(j,1) - eps(j,3)
      end do

      end
