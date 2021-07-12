c$Id:$
      subroutine frame2d(d,ul,xl,ix,s,p,ndf,ndm,nst,isw)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]

c     Two dimensional frame element

c     Control Information:

c       ndm  - Spatial dimension of problem       = 2
c       ndf  - Number degree-of-freedoms at node >= 3
c              ( 1 = u_1 ; 2 = u_2 ; 3 = theta )
c       nen  - Two node element (nel = 2)        >= 2

c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'bdata.h'
      include  'cdata.h'
      include  'eldata.h'
      include  'eltran.h'
      include  'hdata.h'
      include  'iofile.h'
      include  'prld1.h'

      include  'comblk.h'

      integer   ndf,ndm,nst,isw,tdof,i
      real*8    cs,sn,le

      integer   ix(*)
      real*8    d(*),ul(ndf,nen,*),xl(ndm,*),s(nst,*),p(*)

      save

c     INPUT MATERIAL PARAMETERS

      if(isw.eq.1) then
        if(ior.lt.0) write(*,2000)
        write(iow,2000)
        call inmate(d,tdof,3*2,3)

c       Set plot sequence

        call pltln2(iel)

c       Check dimensions

        if(ndm.ne.2 .or. ndf.lt.3 .or. nen.lt.2) then
          write(iow,3000) ndm,ndf,nen
          call plstop()
        endif

c       Deactivate dof in element for dof > 3

        do i = 4,ndf
          ix(i) = 0
        end do

c       Check for geometric storage for finite deformation element

        if(d(18).lt.0.0d0 ) then
          if(d(79).eq.0.0d0) then
            call framf2d(d,ul,xl,ix,s,p,ndf,ndm,nst,isw)
          else
            call franf2d(d,ul,xl,ix,s,p,ndf,ndm,nst,isw)
          endif
        endif

c     CHECK ELEMENT FOR ERRORS

      elseif(isw.eq.2) then

        cs = xl(1,2) - xl(1,1)
        sn = xl(2,2) - xl(2,1)
        le = sqrt(cs*cs+sn*sn)

        if(ix(1).le.0 .or. ix(2).le.0 .or. ix(1).eq.ix(2)) then
          write(iow,4000) n,ix(1),ix(2)
          if(ior.lt.0) write(*,4000) n,ix(1),ix(2)
        endif
        if(le.le.0.0d0) then
          write(iow,4001) n
          if(ior.lt.0) write(*,4001) n
        endif

c     COMPUTE ELEMENT ARRAYS

      else

c       Small deformaion

        if(d(18).gt.0.0d0 ) then

c         Shear deformable (2-node: linear interpolations)

          if(d(79).eq.0.0d0) then
            call frams2d(d,ul,xl,ix,s,p,ndf,ndm,nst,isw)

c         Euler-Bernoulli  (2-node: cubic interpolations)

          else
            call frans2d(d,ul,xl,s,p,ndf,ndm,nst,isw)
          endif

c       Finite deformation (2-node: linear interpolations)

        else

c         Shear deformable (2-node: linear, finite displacements)

          if(d(79).eq.0.0d0) then

            call framf2d(d,ul,xl,ix,s,p,ndf,ndm,nst,isw)

c         No shear case    (2-node: cubic, 2-nd order displacements)

          else

            call franf2d(d,ul,xl,ix,s,p,ndf,ndm,nst,isw)

          endif
        endif
      endif

c     Formats

2000  format(5x,'T w o    D i m e n s i o n a l    F r a m e'/)

3000  format(/' *ERROR* Problem control values incorrect. Set as:'/
     &        '         ndm = ',i2,': (Should be 2)'/
     &        '         ndf = ',i2,': (Minimum = 3)'/
     &        '         nen = ',i2,': (Minimum = 2)'/)

4000  format(' *ERROR* Element',i7,' has unspecified node: ix =',2i7)
4001  format(' *ERROR* Element',i7,' has zero length')

      end

      subroutine frams2d(d,ul,xl,ix,s,p,ndf,ndm,nst,isw)

c     Purpose: Two dimensional small displacement frame element

      implicit  none

      include  'bdata.h'
      include  'bm2com.h'
      include  'cdata.h'
      include  'cdat1.h'
      include  'eldata.h'
      include  'eltran.h'
      include  'hdata.h'
      include  'iofile.h'
      include  'prld1.h'
      include  'prstrs.h'
      include  'ptdat6.h'
      include  'tdata.h'

      include  'comblk.h'

      integer   ndf,ndm,nst,isw
      integer   i,ii, j,jj, ll,lint, mm,nn,nhh,nhv, n1,n2, istrt
      real*8    cs,sn,a1,a2,a3,a4,le,b1,b2,dx,dva,dvi,xjac, energy
      real*8    rhoa, rhoi, ctan1, ctan3

      integer   ix(*)

      real*8    aa(4,4,2),shp(2,3),sg(3),wg(3),forc(4,2),xx(2),b(2)
      real*8    xl(ndm,*),ul(ndf,nen,*)
      real*8    d(*),p(ndf,*),s(nst,nst)

      save

c     Small deformation visco-plastic BEAM element.

c     d(21)*d(32)       = EA
c     d(37)*d(27)*d(32) = kappa*GA
c     d(21)*d(33)       = EI
c     d( 4)*d(32)       = rho*A
c     d( 4)*d(33)       = rho*I
c

c     Compute element length and direction cosines

      if(isw.ge.2) then
        cs = xl(1,nel) - xl(1,1)
        sn = xl(2,nel) - xl(2,1)
        le  = sqrt(cs*cs + sn*sn)
        cs = cs/le
        sn = sn/le

        nhv  = 0
        rhoa = d(4)*d(32)
        rhoi = d(4)*d(33)
      endif

c     Read data

      if(isw.eq.1) then

c       Increment history storage if necessary

      elseif(isw.eq.3 .or. isw.eq.6) then

        n1   = nh1
        n2   = nh2
        lint = nel - 1
        call int1d(lint,sg,wg)
        call bm2trn (ul(1,1,1),cs,sn,ndf*nel,ndf,2)
        call bm2trn (ul(1,1,2),cs,sn,ndf*nel,ndf,2)
        call bm2trn (ul(1,1,5),cs,sn,ndf*nel,ndf,2)
        call bm2trn (xl       ,cs,sn,ndm*nel,ndm,2)
        dva = 0.5d0*rhoa*le
        dvi  =0.5d0*rhoi*d(69)*le
        do ll = 1,lint
          call shp1db(sg(ll),xl,shp,ndm,nel,xjac)
          dx = wg(ll)*xjac
          call b2mods (d,ul,forc,aa,energy,shp,ndf,nen,isw)

c         Multiply moduli by solution parameter: ctan(1)

          ctan1 = ctan(1)
          do jj = 1,4
            do ii = 1,4
              aa(ii,jj,1) = aa(ii,jj,1)*ctan1
            end do ! ii
          end do ! jj

c         Mechanical tangent terms

          mm = 0
          do ii = 1,nel
            b1 = shp(1,ii)*dx
            b2 = shp(2,ii)*dx
            if(isw.eq.3) then
              nn = 0
              do jj = 1,nel
                a1 = b1*shp(1,jj)
                a2 = b1*shp(2,jj)
                a3 = b2*shp(1,jj)
                a4 = b2*shp(2,jj)
                do i = 1,3
                  s(i+mm,3+nn)   = s(i+mm,3+nn) + a2*aa(i,4,1)
                  s(3+mm,i+nn)   = s(3+mm,i+nn) + a3*aa(4,i,1)
                  do j = 1,3
                    s(i+mm,j+nn) = s(i+mm,j+nn) + a1*aa(i,j,1)
                  end do ! j
                end do ! i
                s(3+mm,3+nn) = s(3+mm,3+nn) + a4*aa(4,4,1)
                nn = nn + ndf
              end do ! jj
            endif
            do i = 1,3
              p(i,ii)   = p(i,ii)   - forc(i,1)*b1
            end do ! i
            p(3,ii)   = p(3,ii) - forc(4,1)*b2
            mm = mm + ndf
          end do ! ii
          n1 = n1 + nhv
          n2 = n2 + nhv
        end do ! ll

c       Lumped inertia contributions

        ctan3 = ctan(3) + d(77)*ctan(2)
        jj    = 0
        do ii = 1,nel
          p(1,ii)      = p(1,ii) - dva*(ul(1,ii,5)+d(77)*ul(1,ii,4))
          p(2,ii)      = p(2,ii) - dva*(ul(2,ii,5)+d(77)*ul(2,ii,4))
          p(3,ii)      = p(3,ii) - dvi*(ul(3,ii,5)+d(77)*ul(3,ii,4))
          s(jj+1,jj+1) = s(jj+1,jj+1) + dva*ctan3
          s(jj+2,jj+2) = s(jj+2,jj+2) + dva*ctan3
          s(jj+3,jj+3) = s(jj+3,jj+3) + dvi*ctan3
          jj           = jj + ndf
        end do

c       Transform stiffness and residual to global coordinates

        if(isw.eq.3) then
          call bm2trn (s,cs,sn,nst,ndf,1)
        endif
        call bm2trn ( p,cs,-sn,nst,ndf,2)

c       Set body loading factors

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

c       Add body force components

        p(1,1) = p(1,1) + b(1)
        p(1,2) = p(1,2) + b(1)
        p(2,1) = p(2,1) + b(2)
        p(2,2) = p(2,2) + b(2)

c     Output forces

      elseif(isw.eq.4 .or. isw.eq.8) then
        n1   = nh1
        n2   = nh2
        lint = nel - 1
        call int1d(lint,sg,wg)

c       Loop over quadrature points

        call bm2trn (ul(1,1,1),cs,sn,ndf*nel,ndf,2)
        call bm2trn (ul(1,1,2),cs,sn,ndf*nel,ndf,2)
        call bm2trn (ul(1,1,5),cs,sn,ndf*nel,ndf,2)
        call bm2trn (xl,cs,sn,ndm*nel,ndm,2)
        do ll = 1,lint
          call shp1db(sg(ll),xl,shp,ndm,nel,xjac)

c         Output forces

          call b2mods (d,ul,forc,aa,energy,shp,ndf,nen,isw)
          if(isw.eq.4) then
            do i = 1,ndm
              xx(i) = 0.
              do ii = 1,nel
                xx(i) = xx(i) + xl(i,ii)*shp(2,ii)
              end do ! ii
            end do ! i
            nhh = n2 - 1
            mct = mct - 3
            if (mct.le.0) then
              write(iow,2001) o,head,ttim
              if(ior.lt.0) write(*,2001) o,head,ttim
              mct = 50
            endif
            write(iow,2002) n,ma,(xx(i),i=1,2),
     &                      (strs(i,1),i=1,3),(defa(i,1),i=1,3)
            if(ior.lt.0) then
              write(*,2002) n,ma,(xx(i),i=1,2),
     &                      (strs(i,1),i=1,3),(defa(i,1),i=1,3)
            endif

c         Stress projections save

          else

            dx = wg(ll)*xjac
            do ii = 1,nel
              b1 = shp(1,ii)*dx
              do i = 1,3
                p(i,ii)   = p(i,ii)   - forc(i,1)*b1
              end do ! i
              p(3,ii)   = p(3,ii) - forc(4,1)*shp(2,ii)*dx
            end do ! ii

          endif

          n1 = n1 + nhv
          n2 = n2 + nhv
        end do ! ll

        if(isw.eq.8) then
          do i = 1,3
            p(i,2) = -p(i,2)
          end do
          call frcn2d(ix,p,ndf,hr(nph),hr(nph+numnp),numnp)
        endif

c     Compute mass array

      elseif(isw.eq.5) then

        lint = nel
        call int1d(lint,sg,wg)

c       Compute lumped mass matrix

        call bm2trn (xl,cs,sn,ndm*nel,ndm,2)
        do ll = 1,lint

c         Compute shape functions

          call shp1db(sg(ll),xl,shp,ndm,nel,xjac)

          dva  = wg(ll)*xjac*rhoa
          dvi  = wg(ll)*xjac*rhoi*d(69)

c         For each node j compute db = rho*shape*dv

          do j = 1,nel
            b1 = shp(2,j)*dva
            b2 = shp(2,j)*dvi

c           Compute a lumped mass

            p(1,j) = p(1,j) + b1
            p(2,j) = p(2,j) + b1
            p(3,j) = p(3,j) + b2
          end do ! j
        end do ! ll

c       Place in consistent mass

        jj = 0
        do j = 1,nel
          s(jj+1,jj+1) = p(1,j)
          s(jj+2,jj+2) = p(2,j)
          s(jj+3,jj+3) = p(3,j)
          jj = jj + ndf
        end do ! j

      elseif(isw.eq.12) then

        n1   = nh1
        n2   = nh2
        lint = nel - 1
        call int1d(lint,sg,wg)
        call bm2trn (ul(1,1,1),cs,sn,ndf*nel,ndf,2)
        call bm2trn (ul(1,1,2),cs,sn,ndf*nel,ndf,2)
        call bm2trn (xl       ,cs,sn,ndm*nel,ndm,2)
        do ll = 1,lint
          call shp1db(sg(ll),xl,shp,ndm,nel,xjac)
          call b2mods (d,ul,forc,aa,energy,shp,ndf,nen,isw)
        end do ! ll


c     Compute energy

      elseif(isw.eq.13) then

        n1   = nh1
        n2   = nh2
        lint = nel - 1
        call int1d(lint,sg,wg)
        call bm2trn (ul(1,1,1),cs,sn,ndf*nel,ndf,2)
        call bm2trn (ul(1,1,2),cs,sn,ndf*nel,ndf,2)
        call bm2trn (ul(1,1,4),cs,sn,ndf*nel,ndf,2)
        call bm2trn (xl       ,cs,sn,ndm*nel,ndm,2)
        dva = 0.5d0*rhoa*a1
        dvi  =0.5d0*rhoi*d(69)*a1

c       Compute internal energy

        do ll = 1,lint

c         Compute energy density from stress and deformation

          call shp1db(sg(ll),xl,shp,ndm,nel,xjac)
          dx = wg(ll)*xjac
          call b2mods (d,ul,forc,aa,energy,shp,ndf,nen,isw)

c         Accumulate energy

          epl(8) = epl(8) + 0.5d0*energy*dx

          n1 = n1 + nhv
          n2 = n2 + nhv
        end do ! ll

c       Compute kinetic energy for lumped mass

        epl(7) = epl(7) + 0.5d0*dva*(ul(1,1,4)**2 + ul(1,2,4)**2
     &                             + ul(2,1,4)**2 + ul(2,2,4)**2)
     &                  + 0.5d0*dvi*(ul(3,1,4)**2 + ul(3,2,4)**2)

c     Initialize history variables

      elseif(isw.eq.14) then

        istrt = nint(d(84))
        call modl1d(d,a1,xx,hr(nh1),hr(nh2),nint(d(15)),
     &              1,istrt,forc,aa,isw)

      endif

c     Formats

2001  format(a1,20a4/5x,'time',e13.5,5x,' element forces '//
     &  43x,'*********  FORCE / STRAIN  *********'/
     &   3x,'element  material',
     &  3x,'1-coord',3x,'2-coord',6x,'n-dir',8x,'s-dir',8x,'m-dir'/)

2002  format(2i10,0p,2f10.3,1p,3e13.4/40x,1p,3e13.4)

      end

      subroutine b2mods (d,ul,forca,aa,energy,shp,ndf,nen,isw)

      implicit  none

      include  'bm2com.h'
      include  'ddata.h'
      include  'eldata.h'
      include  'tdata.h'

      integer   ndf,nen, i,ii,isw

      real*8    d(*),ul(ndf,nen,*),cc(3,3,2)
      real*8    forca(4,2),aa(4,4,2), shp(2,3), energy

      save

c     Compute beam strains

      do i = 1,3
        defa(i,1) = 0.0d0
        defa(i,2) = 0.0d0
        do ii = 1,nel
          defa(i,1) = defa(i,1) + ul(i,ii,1)*shp(1,ii)
          defa(i,2) = defa(i,2) + ul(i,ii,4)*shp(1,ii)
        end do ! ii
      end do ! i

      do ii = 1,nel
        defa(2,1) = defa(2,1) - ul(3,ii,1)*shp(2,ii)
        defa(2,2) = defa(2,2) - ul(3,ii,4)*shp(2,ii)
      end do ! ii

c     Compute forces

      call bm2con (d,cc,strs,defa,isw)

c     Compute stored energy density

      if(isw.eq.13) then

        energy = strs(1,1)*defa(1,1)
     &         + strs(2,1)*defa(2,1)
     &         + strs(3,1)*defa(3,1)

      elseif(isw.ne.12) then

        do ii = 1,2

c         Compute first Piola-material frame

          forca(1,ii) =  strs(1,ii)
          forca(2,ii) =  strs(2,ii)
          forca(3,ii) =  strs(3,ii)
          forca(4,ii) = -forca(2,ii)

c         Compute tangent tensor

          aa(1,1,ii) = cc(1,1,ii)
          aa(1,2,ii) = cc(1,2,ii)
          aa(1,3,ii) = cc(1,3,ii)

          aa(2,1,ii) = cc(1,2,ii)
          aa(2,2,ii) = cc(2,2,ii)
          aa(2,3,ii) = cc(2,3,ii)

          aa(3,1,ii) = cc(1,3,ii)
          aa(3,2,ii) = cc(2,3,ii)
          aa(3,3,ii) = cc(3,3,ii)

          aa(1,4,ii) = -cc(1,2,ii)
          aa(2,4,ii) = -cc(2,2,ii)
          aa(3,4,ii) = -cc(2,3,ii)

          aa(4,1,ii) = aa(1,4,ii)
          aa(4,2,ii) = aa(2,4,ii)
          aa(4,3,ii) = aa(3,4,ii)

          aa(4,4,ii) = cc(2,2,ii)
        end do ! ii

      endif

      end

      subroutine framf2d(d,ul,xl,ix,s,p,ndf,ndm,nst,isw)

c     Purpose: 2-d finite deformation frame element

      implicit  none

      include  'augdat.h'
      include  'bdata.h'
      include  'bm2com.h'
      include  'cdata.h'
      include  'cdat1.h'
      include  'eldata.h'
      include  'eltran.h'
      include  'hdata.h'
      include  'iofile.h'
      include  'prld1.h'
      include  'prstrs.h'
      include  'ptdat6.h'
      include  'tdata.h'

      include  'comblk.h'

      integer   ndf,ndm,nst,isw, istrt
      integer   i,ii, j,jj,j1, ll,lint, mm,nn,nhv,nhi, n1,n2
      real*8    cs,sn,a1,a2,a3,a4,b1,b2,dx,dva,dvi,xjac, energy
      real*8    rhoa, rhoi

      integer   ix(*)

      real*8    a(4,4),shp(2,3),sg(3),wg(3),forc(4),xx(2)
      real*8    xl(ndm,*),ul(ndf,nen,*)
      real*8    d(*),p(ndf,*),s(nst,nst)

      save

c     Finite deformation visco-plastic BEAM element.

c     d(1)*d(32)                  = EA
c     d(37)*d(2)*d(32)/2/(1+d(2)) = kappa*GA
c     d(1)*d(33)                  = EI
c     d(4)*d(32)                  = rho*A
c     d(4)*d(33)                  = rho*I

      if(isw.eq.1) then
        if(nint(d(160)).eq.2) then
          nh1    = nh1 + 1 ! Augmented storage
          d(166) = 1
        else
          d(166) = 0
        endif

c     Compute element length and direction cosines

      elseif(isw.ge.2) then
        cs = xl(1,nel) - xl(1,1)
        sn = xl(2,nel) - xl(2,1)
        a1 = sqrt(cs*cs + sn*sn)
        cs = cs/a1
        sn = sn/a1

        nhv  = 2
        rhoa = d(4)*d(32)
        rhoi = d(4)*d(33)
      endif

c     Read data

      if(isw.eq.1) then

c       History storage for rotation parameters

        nh1 = nh1 + 2

      elseif(isw.eq.3 .or. isw.eq.6) then

        nhi  = nint(d(166))
        n1   = nhi + nh1
        n2   = nhi + nh2
        lint = nel - 1
        call int1d(lint,sg,wg)
        call bm2trn (ul(1,1,1),cs,sn,ndf*nel,ndf,2)
        call bm2trn (ul(1,1,2),cs,sn,ndf*nel,ndf,2)
        call bm2trn (ul(1,1,5),cs,sn,ndf*nel,ndf,2)
        call bm2trn (xl       ,cs,sn,ndm*nel,ndm,2)
        dva = 0.5d0*rhoa*a1
        dvi  =0.5d0*rhoi*d(69)*a1
        do ll = 1,lint
          call shp1db(sg(ll),xl,shp,ndm,nel,xjac)
          dx  = wg(ll)*xjac
          call b2modl (d,ul,forc,a,energy,shp,ndf,nen,isw)

c         Multiply moduli by solution parameter: ctan(1)
          do jj = 1,4
            do ii = 1,4
              a(ii,jj) = a(ii,jj)*ctan(1)
            end do ! ii
          end do ! jj

c         Compute residual and tangent

c         Mechanical and Geometric tangent terms

          mm = 0
          do ii = 1,nel
            b1 = shp(1,ii)*dx
            b2 = shp(2,ii)*dx
            if(isw.eq.3) then
              nn = 0
              do jj = 1,nel
                a1 = b1*shp(1,jj)
                a2 = b1*shp(2,jj)
                a3 = b2*shp(1,jj)
                a4 = b2*shp(2,jj)
                do i = 1,3
                  s(i+mm,3+nn)   = s(i+mm,3+nn) + a2*a(i,4)
                  s(3+mm,i+nn)   = s(3+mm,i+nn) + a3*a(4,i)
                  do j = 1,3
                    s(i+mm,j+nn) = s(i+mm,j+nn) + a1*a(i,j)
                  end do ! j
                end do ! i
                s(3+mm,3+nn) = s(3+mm,3+nn) + a4*a(4,4)
                nn = nn + ndf
              end do ! jj
            endif
            do i = 1,3
              p(i,ii)   = p(i,ii)   - forc(i)*b1
            end do ! i
            p(3,ii)   = p(3,ii) - forc(4)*b2
            mm = mm + ndf
          end do ! ii
          n1 = n1 + nhv
          n2 = n2 + nhv
        end do ! ll

c       Lumped inertia contributions

        jj = 0
        do ii = 1,nel
          p(1,ii)      = p(1,ii)      - dva*ul(1,ii,5)
          p(2,ii)      = p(2,ii)      - dva*ul(2,ii,5)
          p(3,ii)      = p(3,ii)      - dvi*ul(3,ii,5)
          s(jj+1,jj+1) = s(jj+1,jj+1) + dva*ctan(3)
          s(jj+2,jj+2) = s(jj+2,jj+2) + dva*ctan(3)
          s(jj+3,jj+3) = s(jj+3,jj+3) + dvi*ctan(3)
          jj           = jj + ndf
        end do

c       Transform stiffness and residual to global coordinates

        if(isw.eq.3) then
          call bm2trn (s,cs,sn,nst,ndf,1)
        endif
        call bm2trn ( p,cs,-sn,nst,ndf,2)

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

c       Add body forces

        if(nel.eq.2) then

          p(1,1)   = p(1,1)  + b1*xjac
          p(2,1)   = p(2,1)  + b2*xjac
          p(1,2)   = p(1,2)  + b1*xjac
          p(2,2)   = p(2,2)  + b2*xjac

        else
          do ll = 1,lint
            call shp1db(sg(ll),xl,shp,ndm,nel,xjac)
            dx = dm*wg(ll)*xjac
            do ii = 1,nel
              a1 = shp(2,ii)*dx
              p(1,ii)   = p(1,ii) + a1*b1
              p(2,ii)   = p(2,ii) + a1*b2
            end do ! ii
          end do ! ll
        endif

c     Output forces

      elseif(isw.eq.4 .or. isw.eq.8) then
        nhi  = nint(d(166))
        n1   = nhi + nh1
        n2   = nhi + nh2
        lint = nel - 1
        call int1d(lint,sg,wg)

c       Loop over quadrature points

        call bm2trn (ul(1,1,1),cs,sn,ndf*nel,ndf,2)
        call bm2trn (ul(1,1,2),cs,sn,ndf*nel,ndf,2)
        call bm2trn (ul(1,1,5),cs,sn,ndf*nel,ndf,2)
        call bm2trn (xl,cs,sn,ndm*nel,ndm,2)
        do ll = 1,lint
          call shp1db(sg(ll),xl,shp,ndm,nel,xjac)

c         Output forces

          call b2modl (d,ul,forc,a,energy,shp,ndf,nen,isw)
          if(isw.eq.4) then
            do i = 1,ndm
              xx(i) = 0.
              do ii = 1,nel
                xx(i) = xx(i) + xl(i,ii)*shp(2,ii)
              end do ! ii
            end do ! i
            mct = mct - 3
            if (mct.le.0) then
              write(iow,2001) o,head,ttim
              if(ior.lt.0) write(*,2001) o,head,ttim
              mct = 50
            endif
            write(iow,2002) n,ma,(xx(i),i=1,ndm),
     &                     (strs(i,1),i=1,3),(defa(i,1),i=1,3)
            if(ior.lt.0) then
              write(*,2002) n,ma,(xx(i),i=1,ndm),
     &                     (strs(i,1),i=1,3),(defa(i,1),i=1,3)
            endif

c         Stress projections save

          else

            dx = wg(ll)*xjac
            do ii = 1,nel
              b1 = shp(1,ii)*dx
              do i = 1,3
                p(i,ii)   = p(i,ii)   - forc(i)*b1
              end do ! i
              p(3,ii)   = p(3,ii) - forc(4)*shp(2,ii)*dx
            end do ! ii

          endif

          n1 = n1 + nhv
          n2 = n2 + nhv
        end do ! ll

        if(isw.eq.8) then
          do i = 1,3
            p(i,2) = -p(i,2)
          end do
          call frcn2d(ix,p,ndf,hr(nph),hr(nph+numnp),numnp)
        endif

c     Compute mass array

      elseif(isw.eq.5) then

        lint = nel
        call int1d(lint,sg,wg)

c       Compute lumped mass matrix

        call bm2trn (xl,cs,sn,ndm*nel,ndm,2)
        do ll = 1,lint

c         Compute shape functions

          call shp1db(sg(ll),xl,shp,ndm,nel,xjac)

          dva  = wg(ll)*xjac*rhoa
          dvi  = wg(ll)*xjac*rhoi*d(69)

c         For each node j compute db = rho*shape*dv

          j1 = 1
          do j = 1,nel
            b1 = shp(2,j)*dva
            b2 = shp(2,j)*dvi

c           Compute a lumped mass

            p(1,j) = p(1,j) + b1
            p(2,j) = p(2,j) + b1
            p(3,j) = p(3,j) + b2

            s(j1  ,j1  ) = s(j1  ,j1  ) + b1
            s(j1+1,j1+1) = s(j1+1,j1+1) + b1
            s(j1+2,j1+2) = s(j1+2,j1+2) + b2

            j1 = j1 + ndf
          end do ! j
        end do ! ll

c     Augmented update or time update of history variables

      elseif(isw.eq.10 .or. isw.eq.12) then

        nhi  = nint(d(166))
        n1   = nhi + nh1
        n2   = nhi + nh2
        lint = nel - 1
        call int1d(lint,sg,wg)
        call bm2trn (ul(1,1,1),cs,sn,ndf*nel,ndf,2)
        call bm2trn (ul(1,1,2),cs,sn,ndf*nel,ndf,2)
        call bm2trn (xl       ,cs,sn,ndm*nel,ndm,2)
        do ll = 1,lint
          call shp1db(sg(ll),xl,shp,ndm,nel,xjac)
          call b2modl (d,ul,forc,a,energy,shp,ndf,nen,isw)
        end do ! ll

c     Compute energy

      elseif(isw.eq.13) then

        nhi  = nint(d(166))
        n1   = nhi + nh1
        n2   = nhi + nh2
        lint = nel - 1
        call int1d(lint,sg,wg)
        call bm2trn (ul(1,1,1),cs,sn,ndf*nel,ndf,2)
        call bm2trn (ul(1,1,2),cs,sn,ndf*nel,ndf,2)
        call bm2trn (ul(1,1,4),cs,sn,ndf*nel,ndf,2)
        call bm2trn (xl       ,cs,sn,ndm*nel,ndm,2)
        dva = 0.5d0*rhoa*a1
        dvi  =0.5d0*rhoi*d(69)*a1

c       Compute internal energy

        do ll = 1,lint

c         Compute energy density from stress and deformation

          call shp1db(sg(ll),xl,shp,ndm,nel,xjac)
          dx = wg(ll)*xjac
          nhi = nint(d(166))
          call b2modl (d,ul,forc,a,energy,shp,ndf,nen,isw)

c         Accumulate energy

          epl(8) = epl(8) + 0.5d0*energy*dx

          n1 = n1 + nhv
          n2 = n2 + nhv
        end do ! ll

c       Compute kinetic energy for lumped mass

        epl(7) = epl(7) + 0.5d0*dva*(ul(1,1,4)**2 + ul(1,2,4)**2
     &                             + ul(2,1,4)**2 + ul(2,2,4)**2)
     &                  + 0.5d0*dvi*(ul(3,1,4)**2 + ul(3,2,4)**2)

c     Initialize history variables

      elseif(isw.eq.14) then

        nhi    = nint(d(166))
        hr(nhi+nh1) = 1.d0
        hr(nhi+nh2) = 1.d0
        nhi         = nhi + 2
        istrt = nint(d(84))
        call modl1d(d,a1,xx,hr(nh1+nhi),hr(nh2+nhi),int(d(15)),
     &              1,istrt,forc,a,isw)

      endif

c     Formats

2001  format(a1,20a4/5x,'time',e13.5,5x,' element forces '//
     &  43x,'*********  FORCE / STRAIN  *********'/
     &   3x,'element  material',
     &  3x,'1-coord',3x,'2-coord',6x,'n-dir',8x,'s-dir',8x,'m-dir'/)

2002  format(2i10,0p,2f10.3,1p,3e13.4/40x,1p,3e13.4)

      end

      subroutine b2modl (d,ul,forca,a,energy,shp,ndf,nen,isw)

      implicit  none

      include  'bm2com.h'
      include  'ddata.h'
      include  'eldata.h'
      include  'pmod2d.h'
      include  'tdata.h'

      integer   ndf,nen, i,ii,isw
      real*8    energy, psi, cn,sn, phia1,phia2

      real*8    d(*),ul(ndf,nen,*),fa(3),df(3),cc(3,3,2)
      real*8    forca(4),a(4,4),b(2,2),shp(2,3)

      save

c     Compute gradient

      do i = 1,3
        fa(i) = 0.0d0
        df(i) = 0.0d0
        do ii = 1,nel
          fa(i) = fa(i) + ul(i,ii,1)*shp(1,ii)
          df(i) = df(i) + ul(i,ii,2)*shp(1,ii)
        end do ! ii
      end do ! i
      fa(1) = fa(1) + 1.d0

c     Generalized mid-point formulation

      psi = 0.d0
      do ii = 1,nel
        psi = psi + ul(3,ii,1)*shp(2,ii)
      end do ! ii
      cn = cos(psi)
      sn = sin(psi)

c     Compute strains and forces in gaussian frame

      defa(1,1) = fa(1)*cn + fa(2)*sn - 1.d0
      defa(2,1) = fa(2)*cn - fa(1)*sn
      defa(3,1) = fa(3)

      call bm2con (d,cc,strs,defa,isw)

c     Compute stored energy density

      if(isw.eq.13) then

        energy = strs(1,1)*defa(1,1)
     &         + strs(2,1)*defa(2,1)
     &         + strs(3,1)*defa(3,1)

      elseif(isw.ne.12) then

c       Compute first Piola-material frame

        forca(1) = cn*strs(1,1) - sn*strs(2,1)
        forca(2) = sn*strs(1,1) + cn*strs(2,1)
        forca(3) = strs(3,1)
        forca(4) = fa(2)*forca(1) - fa(1)*forca(2)

c       Compute tangent elastic tensor

        b(1,1) = cn*cc(1,1,1) - sn*cc(2,1,1)
        b(1,2) = cn*cc(1,2,1) - sn*cc(2,2,1)
        b(2,1) = sn*cc(1,1,1) + cn*cc(2,1,1)
        b(2,2) = sn*cc(1,2,1) + cn*cc(2,2,1)

        a(1,1) = b(1,1)*cn    - b(1,2)*sn
        a(1,2) = b(1,1)*sn    + b(1,2)*cn
        a(1,3) = cn*cc(1,3,1) - sn*cc(2,3,1)

        a(2,1) = a(1,2)
        a(2,2) = b(2,1)*sn    + b(2,2)*cn
        a(2,3) = sn*cc(1,3,1) + cn*cc(2,3,1)

        a(3,1) = a(1,3)
        a(3,2) = a(2,3)
        a(3,3) = cc(3,3,1)

        phia1  = defa(1,1) + 1.d0
        phia2  = defa(2,1)

        a(1,4) = b(1,1)*phia2    - b(1,2)*phia1
        a(2,4) = b(2,1)*phia2    - b(2,2)*phia1
        a(3,4) = cc(1,3,1)*phia2 - cc(2,3,1)*phia1

        a(4,4) = phia2*(cc(1,1,1)*phia2 - cc(1,2,1)*phia1)
     &         + phia1*(cc(2,2,1)*phia1 - cc(1,2,1)*phia2)

        if(gflag) then
          a(1,4) = a(1,4) - forca(2)
          a(2,4) = a(2,4) + forca(1)
          a(4,4) = a(4,4) - fa(1)*forca(1) - fa(2)*forca(2)
        endif

        a(4,1) = a(1,4)
        a(4,2) = a(2,4)
        a(4,3) = a(3,4)

      endif

      end

      subroutine bm2con(d, cc,strs,def,isw)

      implicit  none

      include  'hdata.h'
      include  'comblk.h'

      integer   isw, i

      real*8    d(*), cc(3,3,2),strs(3,2),def(3,2), dl(3)

      save

c     Resultant model

      dl(1) = d(1)*d(32)
      dl(2) = d(1)*d(32)*d(37)*0.5d0/(1.d0+d(2))
      dl(3) = d(1)*d(33)

      do i = 1,9
        cc(i,1,1) = 0.0d0
        cc(i,1,2) = 0.0d0
      end do ! i

c     Elastic resultant model only

      do i = 1,3
        cc(i,i,1) = dl(i)
        cc(i,i,2) = dl(i)
        strs(i,1) = dl(i)*def(i,1)
        strs(i,2) = dl(i)*def(i,2)
      end do ! i

      if(nint(d(160)).eq.2) then ! Add augmented value
        strs(1,1) = strs(1,1) + hr(nh2)
        if(isw.eq.10) then ! Update for augmentation
          hr(nh2) = strs(1,1)
        endif
      endif

      end

      subroutine shp1db(s,xl,shp,ndm,nel,xaj)

      implicit  none

      integer   ndm,nel
      real*8    s,s2, xaj, hx1,hx2,h,h2, sh
      real*8    xl(ndm,nel), shp(2,nel)

      save

      hx1 = xl(1,nel) - xl(1,1)
      hx2 = xl(2,nel) - xl(2,1)
      h   = sqrt(hx1*hx1 + hx2*hx2)
      sh  = s*0.5

c     Linear element

      if(nel.eq.2) then

        shp(2,1) =  0.5d0 - sh
        shp(2,2) =  0.5d0 + sh
        xaj      =  h*0.5d0
        shp(1,1) = -1.0d0/h
        shp(1,2) = -shp(1,1)

c     Quadratic element

      elseif(nel.eq.3) then

        hx1      =   xl(1,nel) - xl(1,nel-1)
        hx2      =   xl(2,nel) - xl(2,nel-1)
        h2       =   sqrt(hx1*hx1 + hx2*hx2)
        s2       =   s*s*0.5d0
        shp(2,1) =   s2 - sh
        shp(2,2) =   1.0d0 - s2 - s2
        shp(2,3) =   s2 + sh
        xaj      =   h*0.5d0 + s*(h2+h2-h)
        shp(1,1) =  (s-0.5)/xaj
        shp(1,2) = -(s+s)/xaj
        shp(1,3) =  (s+0.5)/xaj

      endif

      end

      subroutine bm2trn(s,cs,sn,nst,ndf,itype)

c     Itype:

c        1  Transform matrix s(nst,nst)
c        2  Transform vector s(nst,1)

      implicit  none

      integer   nst,ndf,itype
      integer   i,j,n
      real*8    cs,sn,t,tol

      real*8    s(nst,*)

      save

      data      tol/ 1.d-12 /

      if(cs.gt.1.0d0-tol) return

      if(itype.eq.1) then

        do i = 1,nst,ndf
          j = i + 1
          do n = 1,nst
            t      = s(n,i)*cs - s(n,j)*sn
            s(n,j) = s(n,i)*sn + s(n,j)*cs
            s(n,i) = t
          end do ! n
        end do ! i
        do i = 1,nst,ndf
          j = i + 1
          do n = 1,nst
            t      = s(i,n)*cs - s(j,n)*sn
            s(j,n) = s(i,n)*sn + s(j,n)*cs
            s(i,n) = t
          end do ! n
        end do ! i

      else

        do i=1,nst,ndf
          j = i + 1
          t      =  s(i,1)*cs + s(j,1)*sn
          s(j,1) = -s(i,1)*sn + s(j,1)*cs
          s(i,1) =  t
        end do ! i

      endif

      end

      subroutine frcn2d(ix,p,ndf,dt,st,numnp)

      implicit  none

      integer   ndf,numnp
      integer   i,j,ll

      integer   ix(*)
      real*8    dt(numnp),st(numnp,*),p(ndf,*)

      save

      do i = 1,2

        ll = ix(i)
        if(ll.gt.0) then

          dt(ll) = dt(ll) + 1.d0

c         Stress projections

          do j = 1,3
            st(ll,j) = st(ll,j) + p(j,i)
          end do
        endif
      end do

      end

      subroutine frans2d(d,ul,xl,s,p,ndf,ndm,nst,isw)

c-----[--+---------+---------+---------+---------+---------+---------+-]
c     Two dimensional Euler-Bernoulli frame element
c     N.B. ELASTIC ONLY

c     Control Information:

c       ndm  - Spatial dimension of problem       = 2
c       ndf  - Number degree-of-freedoms at node >= 3
c              ( 1 = u_1 ; 2 = u_2 ; 3 = theta )
c       nen  - Two node element (nel = 2)        >= 2
c-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit none

      include   'bdata.h'
      include   'cdata.h'
      include   'eldata.h'
      include   'eltran.h'
      include   'iofile.h'
      include   'prld1.h'


      integer   i,j,ii,jj,i1,j1,i2,j2,ndf,ndm,nst,isw
      real*8    cs,sn,le,xx,yy,xn,xm,vv,eps,chi,gam,EA,EI,RA
      real*8    b1,b2

      real*8    d(*),ul(ndf,nen,*),xl(ndm,*),s(nst,*),p(*)
      real*8    sm(6,6),pm(6)

      save

c     COMPUTE ELEMENT ARRAYS

      cs = xl(1,2) - xl(1,1)
      sn = xl(2,2) - xl(2,1)
      le = sqrt(cs*cs+sn*sn)
      cs = cs/le
      sn = sn/le

c     Compute elment stiffness/residual arrays

      if(isw.eq.3 .or. isw.eq.6) then

c       Set body loading factors

        if(int(d(81)).gt.0) then
          b1 = 0.5*le*(d(11) + prldv(int(d(81)))*d(71))
        else
          b1 = 0.5*le*d(11)*dm
        endif
        if(int(d(82)).gt.0) then
          b2 = 0.5*le*(d(12) + prldv(int(d(82)))*d(72))
        else
          b2 = 0.5*le*d(12)*dm
        endif

c       Compute momentum residual and tangent matrix

        call pzero(sm,36)
        EA = d(21)*d(32)
        EI = d(21)*d(33)
        RA = d(4)*d(32)
        call beam2d(s ,EA,EI,le,cs,sn,nst,ndf)
        call massf2(sm,pm,d(7),RA,le,cs,sn,6,3)

c       Stress and mass modification to residual and stiffness

        i1 = 0
        i2 = 0
        do ii = 1,2
          p(1+i1) = p(1+i1) + b1
          p(2+i1) = p(2+i1) + b2
          do i = 1,ndf
            j1 = 0
            j2 = 0
            do jj = 1,2
              do j = 1,ndf

c               Residual

                p(i+i1)      = p(i+i1) -  s(i+i1,j+j1)*ul(j,jj,1)
     &                                 - sm(i+i2,j+j2)*ul(j,jj,5)

c               Tangent

                s(i+i1,j+j1) =  s(i+i1,j+j1)*ctan(1)
     &                       + sm(i+i2,j+j2)*ctan(3)

              end do
              j1 = j1 + ndf
              j2 = j2 + 3
            end do

c           Lumped mass modification to residual and tangent

            p(i+i1)      = p(i+i1)      - pm(i+i2)*ul(i,ii,5)

            s(i+i1,i+i1) = s(i+i1,i+i1) + pm(i+i2)*ctan(3)

          end do
          i1 = i1 + ndf
          i2 = i2 + 3
        end do

c     Compute element output quantities

      elseif(isw.eq.4) then

        xx  = 0.5d0*(xl(1,1)+xl(1,2))
        yy  = 0.5d0*(xl(2,1)+xl(2,2))
        eps = (cs*(ul(1,2,1)-ul(1,1,1))
     &       + sn*(ul(2,2,1)-ul(2,1,1)))/le
        chi = (ul(3,2,1)-ul(3,1,1))/le
        gam =-12.d0*(-sn*(ul(1,1,1)-ul(1,2,1))
     &              + cs*(ul(2,1,1)-ul(2,2,1)))/le**3
     &            - 6.d0*(ul(3,1,1)+ul(3,2,1))/le/le
        xn  = d(21)*d(32)*eps
        xm  = d(21)*d(33)*chi
        vv  = d(21)*d(33)*gam

        mct = mct - 1
        if(mct.le.0) then
          write(iow,2000) o,head
          if(ior.lt.0) write(*,2000) o,head
          mct = 50
        endif

        write(iow,2001) n,ma,xx,yy,xn,xm,vv,eps,chi,gam
        if(ior.lt.0) write(*,2001) n,ma,xx,yy,xn,xm,vv,eps,chi,gam

c     Compute element mass arrays

      elseif(isw.eq.5) then
        RA = d(4)*d(32)
        call massf2(s,p,d(7),RA,le,cs,sn,nst,ndf)
      end if

c     Formats

2000  format(a1,20a4//5x,'Beam Element Stresses'//
     &     '    Elmt  Matl     x-coord     y-coord     ',
     &     ' Force      Moment       Shear'/
     & 43x,'Strain    Curvature      Gamma')

2001  format(i8,i6,1p,5e12.4/38x,1p,3e12.4)

4000  format(' *ERROR* Element',i7,' has unspecified node: ix =',2i7)
4001  format(' *ERROR* Element',i7,' has zero length')

      end

      subroutine beam2d(s,ea,ei,le,cs,sn,nst,ndf)

c     Stiffness for frame element

      implicit   none

      integer    nst,ndf,i,j,k
      real*8     ea,ei,le,cs,sn,t

      real*8     s(nst,nst)

      i = ndf + 1
      j = ndf + 2
      k = ndf + 3

      t = ea/le
      s(1,1) = t
      s(i,i) = t
      s(1,i) =-t
      s(i,1) =-t

      t = 12.d0*ei/le**3
      s(2,2) = t
      s(j,j) = t
      s(2,j) =-t
      s(j,2) =-t
      t = (ei+ei)/le
      s(3,3) = t + t
      s(k,k) = t + t
      s(3,k) = t
      s(k,3) = t
      t = 6.d0*ei/le**2
      s(2,3) = t
      s(3,2) = t
      s(2,k) = t
      s(k,2) = t
      s(3,j) =-t
      s(j,3) =-t
      s(j,k) =-t
      s(k,j) =-t

      call rotaf2(s,cs,sn,nst,ndf)

      end

      subroutine massf2(s,p,cfac,ra,le,cs,sn,nst,ndf)

c     Frame mass matrix

      implicit   none

      integer    nst,ndf,i,j,l,ii(4)
      real*8     cfac,lfac,ra,le,cs,sn,t,dv,s1,s2,s3
      real*8     p(nst),s(nst,nst),sg(4),ag(4),bb(4)

      data       ii /2,3,5,6/

      ii(3)    = ndf + 2
      ii(4)    = ndf + 3

c     Lumped mass matrix

      t        = 0.5d0*ra*le
      p(1)     = t
      p(2)     = t
      p(ndf+1) = t
      p(ndf+2) = t

c     Consistent mass matrix

      s(1    ,1    ) = 0.6666666666666667d0*t
      s(1    ,ndf+1) = 0.3333333333333333d0*t
      s(ndf+1,ndf+1) = s(1,1)

      call int1d(4,sg,ag)

      do l = 1,4
        dv     = t*ag(l)
        s1     = 0.5d0 + 0.5d0*sg(l)
        s2     = s1*s1
        s3     = s1*s2
        bb(3)  = 3.d0*s2 - s3 - s3
        bb(4)  = le*(s3 - s2)
        bb(1)  = 1.d0 - bb(3)
        bb(2)  = le*(s1 - s2) + bb(4)
        do i = 1,4
          s1 = bb(i)*dv
          do j = i,4
            s(ii(i),ii(j)) = s(ii(i),ii(j)) + s1*bb(j)
          end do
        end do
      end do

      do i = 1,6
        do j = 1,i
          s(i,j) = s(j,i)
        end do
      end do

	lfac = 1.d0 - cfac
      do i = 1,6
        do j = 1,6
          s(i,j) = cfac*s(i,j)
        end do
        s(i,i) = s(i,i) + lfac*p(i)
      end do
	
      call rotaf2(s,cs,sn,nst,ndf)

      end

      subroutine rotaf2(s,cs,sn,nst,ndf)

c     Rotate arrays from local to global dof's

      implicit   none

      integer    nst,ndf,i,j,n
      real*8     cs,sn,t
      real*8     s(nst,nst)

c     Check angle (perform rotation if necessary)

      if(cs.lt.0.99999999d0) then

        do i = 1,nst,ndf
          j = i + 1
          do n = 1,nst
            t      = s(n,i)*cs - s(n,j)*sn
            s(n,j) = s(n,i)*sn + s(n,j)*cs
            s(n,i) = t
          end do
        end do
        do i = 1,nst,ndf
          j = i + 1
          do n = 1,nst
            t      = cs*s(i,n) - sn*s(j,n)
            s(j,n) = sn*s(i,n) + cs*s(j,n)
            s(i,n) = t
          end do
        end do

      end if

      end

      subroutine franf2d(d,ul,xl,ix,s,r,ndf,ndm,nst,isw)

c-----[--+---------+---------+---------+---------+---------+---------+-]
c     Purpose: Two dimensional Euler-Bernoulli frame element: Second
c              order theory. Includes one enhanced mode for axial and
c              bending strain match.
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'bdata.h'
      include  'bm2com.h'
      include  'cdata.h'
      include  'cdat1.h'
      include  'eldata.h'
      include  'eltran.h'
      include  'hdata.h'
      include  'iofile.h'
      include  'pmod2d.h'
      include  'prld1.h'
      include  'prstrs.h'
      include  'ptdat6.h'
      include  'tdata.h'

      include  'comblk.h'

      logical   noconv
      integer   ndf,ndm,nst,isw, istrt
      integer   i,ii,itmax, j,jj, ll,lint, mm,nn,nhv, n1,n2
      real*8    cs,sn,b1,b2,dva,dvi,xjac, energy
      real*8    len, hle
      real*8    rhoa, rhoi, ctan1, ctan3

      integer   ix(*)

      real*8    aa(4,4,2),shpw(4,2,4),shpt(4,2,4),shpu(2,2,4),dx(4)
      real*8    sg(4),wg(4), dudx(4), dwdx(4), eps(4), kap(4), gam(4)
      real*8    bmat(2,3,2),baii(4,2),forc(4,5),xx(2),b(2),nxi(3)
      real*8    xl(ndm,*),ul(ndf,nen,*)
      real*8    d(*),r(ndf,*),s(nst,nst), gen(3,2)
      real*8    duen, uen, ben, hen, EA, tol

      save

      data      itmax / 20     /
      data      tol   / 1.d-10 /

c     Second order (nonlinear) deformation BEAM element.

c     d(1)*d(32)       = EA
c     d(1)*d(33)       = EI
c     d(4)*d(32)       = rho*A
c     d(4)*d(33)       = rho*I

c     Compute element length and direction cosines

      if(isw.ge.2) then
        cs  = xl(1,nel) - xl(1,1)
        sn  = xl(2,nel) - xl(2,1)
        len = sqrt(cs*cs + sn*sn)
        cs  = cs/len
        sn  = sn/len
        hle = 0.5d0*len

        nhv  = 0
        rhoa = d(4)*d(32)
        rhoi = d(4)*d(33)
      endif

c     Read data

      if(isw.eq.1) then

c       Increment history storage if necessary

        nh1 = nh1 + 1   ! Enhanced strain parameter

c     Compute mass array

      elseif(isw.eq.5) then

        lint = nel + 1
        call int1d(lint,sg,wg)

c       Compute lumped mass matrix

        call bm2trn (xl,cs,sn,ndm*nel,ndm,2)
        do ll = 1,lint

c         Compute shape functions

          call shp1db(sg(ll),xl,shpu,ndm,nel,xjac)

          dva  = wg(ll)*xjac*rhoa
          dvi  = wg(ll)*xjac*rhoi*d(69)

c         For each node j compute db = rho*shape*dv

          do j = 1,nel
            b1 = shpu(2,j,ll)*dva
            b2 = shpu(2,j,ll)*dvi

c           Compute a lumped mass

            r(1,j) = r(1,j) + b1
            r(2,j) = r(2,j) + b1
            r(3,j) = r(3,j) + b2
          end do ! j
        end do ! ll

c       Place in consistent mass

        jj = 0
        do j = 1,nel
          s(jj+1,jj+1) = r(1,j)
          s(jj+2,jj+2) = r(2,j)
          s(jj+3,jj+3) = r(3,j)
          jj = jj + ndf
        end do ! j

      else

c       Quadrature terms

        n1   = nh1 + 1
        n2   = nh2 + 1
        lint = nel + 1
        call int1d(lint,sg,wg)

c       Transform to local coordinates

        call bm2trn (ul(1,1,1),cs,sn,ndf*nel,ndf,2)
        call bm2trn (ul(1,1,2),cs,sn,ndf*nel,ndf,2)
        call bm2trn (ul(1,1,4),cs,sn,ndf*nel,ndf,2)
        call bm2trn (ul(1,1,5),cs,sn,ndf*nel,ndf,2)
        call bm2trn (xl       ,cs,sn,ndm*nel,ndm,2)
        dva = rhoa*hle
        dvi = rhoi*hle*d(69)

        do ll = 1,lint

c         Shape functions

          call shp1dh(sg(ll),len,shpw(1,1,ll),shpt(1,1,ll))
          shpu(1,1,ll) = -1.d0/len
          shpu(1,2,ll) = -shpu(1,1,ll)
          shpu(2,1,ll) = 0.5d0 - 0.5d0*sg(ll)
          shpu(2,2,ll) = 0.5d0 + 0.5d0*sg(ll)

          dx(ll)      = wg(ll)*hle

c         Form displacement derivatives from nodal displacements

          dwdx(ll) = shpw(1,1,ll)*ul(2,1,1) + shpw(1,2,ll)*ul(2,2,1)
     &             + shpt(1,1,ll)*ul(3,1,1) + shpt(1,2,ll)*ul(3,2,1)

          dudx(ll) = 0.5d0*(ul(1,2,1) - ul(1,1,1))/hle

          eps(ll)  = dudx(ll) + 0.5d0*(dudx(ll)*dudx(ll)
     &                               + dwdx(ll)*dwdx(ll))

          kap(ll)  = shpw(2,1,ll)*ul(2,1,1) + shpw(2,2,ll)*ul(2,2,1)
     &             + shpt(2,1,ll)*ul(3,1,1) + shpt(2,2,ll)*ul(3,2,1)

          gam(ll)  = shpw(3,1,ll)*ul(2,1,1) + shpw(3,2,ll)*ul(2,2,1)
     &             + shpt(3,1,ll)*ul(3,1,1) + shpt(3,2,ll)*ul(3,2,1)
        end do ! ll

c       Enhanced strain computation

        if(etype.eq.3) then
          uen = hr(nh1)
          ii  = 0
          noconv = .true.
          do while(noconv)

            ii  = ii + 1

c           Zero enhanced terms

            ben = 0.0d0
            hen = 0.0d0

            do ll = 1,lint

              EA  = d(1)*d(32)
              hen = hen + sg(ll)*EA*sg(ll)*dx(ll)*ctan(1)
              ben = ben - sg(ll)*EA*(eps(ll) + sg(ll)*uen)*dx(ll)

            end do ! ll

            hen  = 1.d0/ hen
            duen = ben * hen
            uen  = uen + duen

            if(abs(duen).le.tol*abs(uen) .or. ben.eq.0.0d0) then
              noconv = .false.
            elseif(ii.gt.itmax) then
              noconv = .false.
              write(*,*) 'WARNING -- No convergence in FRANF2D'
              write(*,*) duen,uen,hen
            endif

          end do ! while

c         Save enhance mode parameter

          hr(nh2) = uen

        else

          hen = 0.0d0

        end if ! etype

c       Stiffness and residual computation

        if(isw.eq.3 .or. isw.eq.6) then

c         Zero enhanced coupling array

          do i = 1,3
            gen(i,1) = 0.0d0
            gen(i,2) = 0.0d0
          end do ! i

c         Final tangent form

          do ll = 1,lint

            forc(1,ll) = d(1)*d(32)*(eps(ll) + sg(ll)*uen)*dx(ll)
            forc(2,ll) = d(1)*d(33)*kap(ll)*dx(ll)
            aa(1,1,1)  = d(1)*d(32)
            aa(2,2,1)  = d(1)*d(33)
            aa(1,2,1)  = 0.0d0
            aa(2,1,1)  = 0.0d0

            ctan1 = ctan(1)*dx(ll)
            do jj = 1,4
              do ii = 1,4
                aa(ii,jj,1) = aa(ii,jj,1)*ctan1
              end do ! ii
            end do ! jj

c           Compute strain-displacement matrices for two nodes

            do i = 1,2
              bmat(1,1,i) =  shpu(1,i,ll)*(1.d0 + dudx(ll))
              bmat(2,1,i) =  0.0d0
              bmat(1,2,i) =  shpw(1,i,ll)*dwdx(ll)
              bmat(2,2,i) =  shpw(2,i,ll)
              bmat(1,3,i) =  shpt(1,i,ll)*dwdx(ll)
              bmat(2,3,i) =  shpt(2,i,ll)
            end do ! i

c           Mechanical tangent terms

            mm = 0
            do ii = 1,nel

              do i = 1,3

c               B^T * AA

                baii(i,1) = (bmat(1,i,ii)*aa(1,1,1) +
     &                       bmat(2,i,ii)*aa(2,1,1))
                baii(i,2) = (bmat(1,i,ii)*aa(1,2,1) +
     &                       bmat(2,i,ii)*aa(2,2,1))

c               Residual

                r(i,ii) = r(i,ii) - bmat(1,i,ii)*forc(1,ll)
     &                            - bmat(2,i,ii)*forc(2,ll)

c               Enhanced stiffness

                gen(i,ii) = gen(i,ii) + baii(i,1)*sg(ll)

              end do ! i

c             Tangent

              if(isw.eq.3) then

                nxi(1) = shpu(1,ii,ll)*forc(1,ll)*ctan(1)
                nxi(2) = shpw(1,ii,ll)*forc(1,ll)*ctan(1)
                nxi(3) = shpt(1,ii,ll)*forc(1,ll)*ctan(1)
                nn = 0
                do jj = 1,nel

c                 Material part

                  do j = 1,3
                    do i = 1,3
                      s(mm+i,nn+j) = s(mm+i,nn+j)
     &                             + baii(i,1)*bmat(1,j,jj)
     &                             + baii(i,2)*bmat(2,j,jj)
                    end do ! i
                  end do ! j

c                 Geometric part

                  s(mm+1,nn+1) = s(mm+1,nn+1) + nxi(1)* shpu(1,jj,ll)
                  s(mm+2,nn+2) = s(mm+2,nn+2) + nxi(2)*shpw(1,jj,ll)
                  s(mm+2,nn+3) = s(mm+2,nn+3) + nxi(2)*shpt(1,jj,ll)
                  s(mm+3,nn+2) = s(mm+3,nn+2) + nxi(3)*shpw(1,jj,ll)
                  s(mm+3,nn+3) = s(mm+3,nn+3) + nxi(3)*shpt(1,jj,ll)

                  nn = nn + ndf
                end do ! jj
              endif
              mm = mm + ndf
            end do ! ii
            n1 = n1 + nhv
            n2 = n2 + nhv
          end do ! ll

c         Lumped inertia contributions

          ctan3 = ctan(3) + d(77)*ctan(2)
          jj    = 0
          do ii = 1,nel
            r(1,ii)      = r(1,ii) - dva*(ul(1,ii,5)+d(77)*ul(1,ii,4))
            r(2,ii)      = r(2,ii) - dva*(ul(2,ii,5)+d(77)*ul(2,ii,4))
            r(3,ii)      = r(3,ii) - dvi*(ul(3,ii,5)+d(77)*ul(3,ii,4))
            s(jj+1,jj+1) = s(jj+1,jj+1) + dva*ctan3
            s(jj+2,jj+2) = s(jj+2,jj+2) + dva*ctan3
            s(jj+3,jj+3) = s(jj+3,jj+3) + dvi*ctan3
            jj           = jj + ndf
          end do

c         Transform stiffness and residual to global coordinates

          if(isw.eq.3) then

c           Static condensation

            nn = 0
            do jj = 1,nel
              do j = 1,3
                duen = gen(j,jj)*hen
                mm = 0
                do ii = 1,nel
                  do i = 1,3
                    s(i+mm,j+nn) = s(i+mm,j+nn) - gen(i,ii)*duen
                  end do ! i
                  mm = mm + ndf
                end do ! ii
              end do ! j
              nn = nn + ndf
            end do ! jj

            call bm2trn (s,cs,sn,nst,ndf,1)
          endif
          call bm2trn ( r,cs,-sn,nst,ndf,2)

c         Set body loading factors

          if(int(d(74)).gt.0) then
            b(1) = hle*(d(11) + prldv(int(d(74)))*d(71))
          else
            b(1) = hle*d(11)*dm
          endif
          if(int(d(75)).gt.0) then
            b(2) = hle*(d(12) + prldv(int(d(75)))*d(72))
          else
            b(2) = hle*d(12)*dm
          endif

c         Add body force components

          r(1,1) = r(1,1) + b(1)
          r(1,2) = r(1,2) + b(1)
          r(2,1) = r(2,1) + b(2)
          r(2,2) = r(2,2) + b(2)

c       Output forces

        elseif(isw.eq.4 .or. isw.eq.8) then

c         Member forces

          do ll = 1,lint

            defa(1,1)  = eps(ll) + sg(ll)*uen
            defa(2,1)  = gam(ll)
            defa(3,1)  = kap(ll)

            forc(1,ll) = d(1)*d(32)*defa(1,1)
            forc(2,ll) = d(1)*d(33)*defa(2,1)
            forc(3,ll) = d(1)*d(33)*defa(3,1)

c           Stress output

            if(isw.eq.4) then

              do i = 1,ndm
                xx(i) = 0.
                do ii = 1,nel
                  xx(i) = xx(i) + xl(i,ii)*shpu(2,ii,ll)
                end do ! ii
              end do ! i
              mct = mct - 3
              if (mct.le.0) then
                write(iow,2001) o,head,ttim
                if(ior.lt.0) write(*,2001) o,head,ttim
                mct = 50
              endif
              write(iow,2002) n,ma,(xx(i),i=1,2),
     &                        (forc(i,ll),i=1,3),(defa(i,1),i=1,3)
              if(ior.lt.0) then
                write(*,2002) n,ma,(xx(i),i=1,2),
     &                        (forc(i,ll),i=1,3),(defa(i,1),i=1,3)
              endif

c           Stress projections save

            else

c             Compute strain-displacement matrices for two nodes

              do i = 1,2
                bmat(1,1,i) =  shpu(1,i,ll)*(1.d0 + dudx(ll))
                bmat(2,1,i) =  0.0d0
                bmat(1,2,i) =  shpw(1,i,ll)*dwdx(ll)
                bmat(2,2,i) =  shpw(2,i,ll)
                bmat(1,3,i) =  shpt(1,i,ll)*dwdx(ll)
                bmat(2,3,i) =  shpt(2,i,ll)
              end do ! i

c             End forces

              do ii = 1,nel

                do i = 1,3
                  r(i,ii) = r(i,ii) - (bmat(1,i,ii)*forc(1,ll)
     &                              +  bmat(2,i,ii)*forc(3,ll))*dx(ll)
                end do ! i

              end do ! ii
            end if
          end do ! ll

c         Projection on end notes (uses reactions)

          if(isw.eq.8) then
            do i = 1,3
              r(i,2) = -r(i,2)
            end do
            call frcn2d(ix,r,ndf,hr(nph),hr(nph+numnp),numnp)
          endif

c       Compute energy

        elseif(isw.eq.13) then

          dva = hle*rhoa
          dvi  =hle*rhoi*d(69)

c         Compute internal energy

          n1 = nh1 + 1
          n2 = nh2 + 1
          do ll = 1,lint

c           Compute energy density from stress and deformation

            call shp1db(sg(ll),xl,shpu,ndm,nel,xjac)
            dx(ll) = wg(ll)*xjac
            call b2mods (d,ul,forc,aa,energy,shpu,ndf,nen,isw)

c           Accumulate energy

            epl(8) = epl(8) + 0.5d0*energy*dx(ll)

            n1 = n1 + nhv
            n2 = n2 + nhv
          end do ! ll

c         Compute kinetic energy for lumped mass

          epl(7) = epl(7) + 0.5d0*dva*(ul(1,1,4)**2 + ul(1,2,4)**2
     &                               + ul(2,1,4)**2 + ul(2,2,4)**2)
     &                    + 0.5d0*dvi*(ul(3,1,4)**2 + ul(3,2,4)**2)

c       Initialize history variables

        elseif(isw.eq.14) then

          istrt = nint(d(84))
          call modl1d(d,len,xx,hr(nh1),hr(nh2),nint(d(15)),
     &                1,istrt,forc,aa,isw)

        endif

      endif

c     Formats

2001  format(a1,20a4/5x,'time',e13.5,5x,' element forces '//
     &  43x,'*********  FORCE / STRAIN  *********'/
     &   3x,'element  material',
     &  3x,'1-coord',3x,'2-coord',6x,'n-dir',8x,'s-dir',8x,'m-dir'/)

2002  format(2i10,0p,2f10.3,1p,3e13.4/40x,1p,3e13.4)

      end
