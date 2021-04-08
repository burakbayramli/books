!$Id:$
      subroutine trussnd(d,ul,xl,ix,tl,s,p,ndf,ndm,nst,isw)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     1, 2 and 3 dimensional truss element routine

!     Outputs: (isw = 4)

!         xx  - Coordinates at mid-length of truss bar
!         sig - Force on truss bar
!         eps - Strain on truss bar

!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'augdat.h'
      include  'bdata.h'
      include  'cdata.h'
      include  'eldata.h'
      include  'elplot.h'
      include  'eltran.h'
      include  'hdata.h'
      include  'iofile.h'
      include  'pmod2d.h'
      include  'prld1.h'
      include  'prstrs.h'
      include  'ptdat6.h'
      include  'comblk.h'

      logical       :: nonlin
      integer       :: i, j, ii, jj, i1, j1, nhv,nhi, tdof
      integer       :: ndf, ndm, nst, isw, istrt
      real (kind=8) :: xlen,xlen0,xlen2, sig(2),eps(2), ta, body, dd(2)
      real (kind=8) :: lm,cmd,cmo,hmd,hmo,geo,forc, alphar, one3

      real (kind=8) :: db(3,2),bl(3,2),br(3,2),xx(3),th(2)

      integer       :: ix(*)
      real (kind=8) :: d(*),ul(ndf,nen,*),xl(ndm,nen),tl(*)
      real (kind=8) :: s(nst,nst),p(ndf,*)

      save

      data      one3 / 0.3333333333333333d0 /

!     Set Analysis Type

      if(nint(d(39)).eq.1) then
        dtype  =  1
        nonlin = .true.
      else
        dtype  =  nint(d(18))
        nonlin =  dtype.lt.0
      endif
      hflag  = nint(d(30)).eq.1
      nhv    = nint(d(15))

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

!     INPUT MATERIAL PROPERTIES

      if(isw.eq.1) then

        write(iow,2000)
        if(ior.lt.0) write(*,2000)

!       Input material parameters

        call inmate(d,tdof,ndm*2,2)
        if(nint(d(160)).eq.2) then
          nh1    = nh1 + 1 ! Augmented storage
          d(166) = 1
        else
          d(166) = 0
        endif

!       Set tdof to zero if 1, 2, or larger than ndf

        if(tdof.gt.ndf) then
          write(iow,3003)
          if(ior.lt.0) write(*,3003)
          tdof = 0
        elseif(tdof.gt.0 .and. tdof.le.ndm) then
          write(iow,3004)
          if(ior.lt.0) write(*,3004)
          tdof = 0
        endif

!       Deactivate dof in element for dof > ndm

        do i = ndm+1,ndf
          ix(i) = 0
        end do

!       If temperature dof is specified activate dof

        if(tdof.gt.0) then
          ix(tdof) = 1
        endif

!       Set plot sequence

        pstyp = 1

!     CHECK FOR ZERO LENGTH ELEMENTS

      elseif(isw.eq.2) then

        if(ix(1).eq.0 .or. ix(2).eq.0) then
          write(iow,4000) n_el,ix(1),ix(2)
          if(ior.lt.0) then
            write(*,4000) n_el,ix(1),ix(2)
          endif
        else
          xlen = 0.0d0
          eps(1)  = 0.0d0
          do i = 1,ndm
            eps(1)  = max(eps(1) , abs(xl(i,1)), abs(xl(i,2)))
            xlen = max(xlen, abs(xl(i,2) - xl(i,1)))
          end do
          if(xlen.eq.1.0d-10*eps(1)) then
            write(iow,4001) n_el
            if(ior.lt.0) then
              write(*,4001) n_el
            endif
          endif
        endif

!     COMPUTE ELEMENT STIFFNESS AND RESIDUAL ARRAYS

      elseif(isw.eq.3 .or. isw.eq.6) then

!       Compute element stress, strain, and tangent modulus

        call strn1d(d,xl,ul,th,ndm,ndf,nen,xlen,xlen0,xx,eps,ta,
     &              nonlin,isw)
        nhi    = nint(d(166))
        if(nint(d(160)).eq.2) then ! Set augmented value
          sig(1) = hr(nh2)
        else
          sig(1) = 0.0d0
        endif
        istrt = nint(d(84))
        call modl1d(d,ta,eps,hr(nhi+nh1),hr(nhi+nh2),nhv,
     &              1,istrt,dd,sig,isw)

!       Save stress and strain for tplot

        tt(1) = sig(1)
        tt(2) = eps(1)

!       Multiply tangent modulus by length and area

        if(dtype.lt.0) then
          dd(1) = dd(1) - sig(1)
        endif

        dd(1) = (dd(1)*ctan(1) + dd(2)*d(78)*ctan(2))*d(32)*xlen

!       Compute strain-displacement matrix

        xlen2  = 1.d0/(xlen*xlen)
        alphar = xlen2/ctan(1) - xlen2
        do i = 1,ndm

          bl(i,1) = (xl(i,1) - xl(i,2))*xlen2

!         Set non-linear terms in strain-displacement matrix

          if(nonlin) then
            bl(i,1) = bl(i,1) + (ul(i,1,1) - ul(i,2,1))*xlen2
          endif
          br(i,1) = bl(i,1)

!         Set remaining terms and multiply by elastic modulus

          bl(i,2) = -bl(i,1)
          br(i,2) = -br(i,1)

          db(i,1) =  bl(i,1)*dd(1)
          db(i,2) = -db(i,1)

        end do

!       Compute mass terms

        cmd = d(4)*d(32)*xlen0*one3
        cmo = cmd*0.5d0
        lm  = cmd*1.5d0
        hmd = d(7)*cmd + (1.d0 - d(7))*lm
        hmo = d(7)*cmo

!       Form internal, body  and inertia force vector
!       Include stiffness proportional Rayleigh damping effect in force
!       Include mass proportional Rayleigh damping in residual

        forc = (sig(1) + d(78)*sig(2))*xlen*d(32)

        do i = 1,ndm

!         Set body loading factors

          if(int(d(73+i)).gt.0) then
            body = 0.5*xlen0*(d(10+i) + prldv(int(d(73+i)))*d(70+i))
          else
            body = 0.5*xlen0*d(10+i)*dm
          endif

          p(i,1) = body   - bl(i,1)*forc
     &           - hmd*(ul(i,1,5) + d(77)*ul(i,1,4))
     &           - hmo*(ul(i,2,5) + d(77)*ul(i,2,4))

          p(i,2) = body   - bl(i,2)*forc
     &           - hmo*(ul(i,1,5) + d(77)*ul(i,1,4))
     &           - hmd*(ul(i,2,5) + d(77)*ul(i,2,4))

        end do

!       Compute stiffness terms

        if(isw.eq.3) then

          i1 = 0
          do ii = 1,2
            j1 = 0
            do jj = 1,2
              do i = 1,ndm
                do j = 1,ndm
                  s(i+i1,j+j1) = db(i,ii)*br(j,jj)
                end do
              end do
              j1 = j1 + ndf
            end do
            i1 = i1 + ndf
          end do

!         Correct for geometric and inertial tangent effects

          if(nonlin .and. gflag) then
            geo = sig(1)*d(32)/xlen*ctan(1)
          else
            geo = 0.0d0
          endif

!         Set diagonal and off-diagonal terms
!         Include mass proportional rayleigh damping

          hmd = hmd*(ctan(3) + d(77)*ctan(2)) + geo
          hmo = hmo*(ctan(3) + d(77)*ctan(2)) - geo

          do i = 1,ndm
            j      = i + ndf
            s(i,i) = s(i,i) + hmd
            s(i,j) = s(i,j) + hmo
            s(j,i) = s(j,i) + hmo
            s(j,j) = s(j,j) + hmd
          end do

        endif

!     OUTPUT STRESS AND STRAIN IN ELEMENT

      elseif(isw.eq.4 .or. isw.eq.8) then

!       Form strain and stress

        call strn1d(d,xl,ul,th,ndm,ndf,nen,xlen,xlen0,xx,eps,ta,
     &              nonlin,isw)
        nhi  = nint(d(166))
        if(nint(d(160)).eq.2) then ! Set augmented value
          sig(1) = hr(nh2)
        else
          sig(1) = 0.0d0
        endif
        istrt = nint(d(84))
        call modl1d(d,ta,eps,hr(nhi+nh1),hr(nhi+nh2),nhv,
     &              1,istrt,dd,sig,isw)

!       Form truss force: multiply stress by area

        sig(1) = d(32)*sig(1)

!       Output element force/strains

        if(isw.eq.4) then
          mct = mct - 1
          if(mct.le.0) then
            write(iow,2001) o,head
            if(ior.lt.0) write(*,2001) o,head
            mct = 50
          endif
          write(iow,2002) n_el,ma,xx,sig(1),eps(1)
          if(ior.lt.0) then
            write(*,2002) n_el,ma,xx,sig(1),eps(1)
          endif
        else
          call trcnnd(ix,sig(1),hr(nph),hr(nph+numnp))
        endif

!     COMPUTE ELEMENT MASS MATRICES

      elseif(isw.eq.5) then

        xlen0 = 0.0d0
        do i = 1,ndm
          xlen0 = xlen0 + (xl(i,2)-xl(i,1))**2
        end do

!       Compute mass matrix terms

        cmd = d(4)*d(32)*sqrt(xlen0)*one3
        lm  = 1.5d0*cmd
        cmo = 0.5d0*cmd
        hmd = d(7)*cmd + (1.d0 - d(7))*lm
        hmo = d(7)*cmo

        do i = 1,ndm

!         Higher order mass

          j      = i + ndf
          s(i,i) = hmd
          s(j,j) = hmd
          s(j,i) = hmo
          s(i,j) = hmo

!         Lumped mass

          p(i,1) = lm
          p(i,2) = p(i,1)

        end do

!     Augmented update

      elseif(isw.eq.10) then

!       Form strain and stress

        if(nint(d(160)).eq.2) then ! Set augmented value
          call strn1d(d,xl,ul,th,ndm,ndf,nen,xlen,xlen0,xx,eps,ta,
     &                nonlin,isw)
          nhi    = nint(d(166))
          sig(1) = hr(nh2)
          istrt = nint(d(84))
          call modl1d(d,ta,eps,hr(nhi+nh1),hr(nhi+nh2),nhv,
     &                1,istrt,dd,sig,isw)

          hr(nh2) = hr(nh2) + augf*(sig(1) - hr(nh2))
        endif

!     COMPUTE ELEMENT ENERGY

      elseif(isw.eq.13) then

!       Compute element stress, strain, and tangent modulus

        call strn1d(d,xl,ul,th,ndm,ndf,nen,xlen,xlen0,xx,eps,ta,
     &              nonlin,isw)
        istrt = nint(d(84))
        call modl1d(d,ta,eps,hr(nh1),hr(nh2),nhv,
     &              1,istrt,dd,sig,isw)

!       Stored energy

        epl(8) = epl(8) + 0.5d0*eps(1)*sig(1)*d(32)*xlen

!       Compute mass terms

        cmd = d(4)*d(32)*xlen0*one3
        cmo = cmd*0.5d0
        lm  = cmd*1.5d0
        hmd =(d(7)*cmd + (1.d0 - d(7))*lm)*0.5d0
        hmo = d(7)*cmo

!       Kinetic energy

        do i = 1,ndm
          epl(7) = epl(7) + hmd*(ul(i,1,4)**2 + ul(i,2,4)**2)
     &                    + hmo* ul(i,1,4)*ul(i,2,4)
        end do

      endif

!     FORMATS

2000  format(9x,'T r u s s    E l e m e n t'/1x)
2001  format(a1,20a4//9x,'Truss Element'//' Elmt Matl    ',
     & '1-coord    2-coord    3-coord',5x,'Force',9x,'Strain')
2002  format(2i5,1p3e11.3,1p2e14.5)

3003  format(' *WARNING* Thermal d.o.f. > active d.o.f.s : Set to 0')
3004  format(' *WARNING* Thermal d.o.f. can not be <= ndm: Set to 0')

4000  format(' *ERROR* Element',i7,' has nodes',2i8)
4001  format(' *ERROR* Element',i7,' has zero length')

      end subroutine trussnd

      subroutine strn1d(d,xl,ul,tl,ndm,ndf,nen,xlen,xlen0,xx,eps,ta,
     &                  nonlin,isw)

!     Compute constitutive equation

      implicit  none

      include  'iofile.h'
      include  'eltran.h'
      include  'pmod2d.h'

      logical       :: nonlin
      integer       :: i,ndm,ndf,nen, isw
      real (kind=8) :: xlen,xlen0,xlenn,xlen1,xlena,dx,du,dd, ta
      real (kind=8) :: alpha, alphar
      real (kind=8) :: xx(3),d(*),xl(ndm,*),ul(ndf,nen,*),tl(*),eps(*)

      save

!     Set integration parameter: For energy = 1.0

      if(isw.eq.13) then
        alpha = 1.d0
      else
        alpha = ctan(1)
      endif
      alphar = 1.d0/alpha - 1.d0

!     Compute length and strain terms

      xlen0  = 0.0d0
      xlenn  = 0.0d0
      xlen1  = 0.0d0
      xlena  = 0.0d0
      eps(1) = 0.0d0
      eps(2) = 0.0d0
      do i = 1,ndm
        dx    = xl(i,2)   - xl(i,1)
        du    = ul(i,2,1) - ul(i,1,1)
        dd    = ul(i,2,2) - ul(i,1,2)
        xlen0 = xlen0 +  dx**2
        xlen1 = xlen1 + (dx + du + dd*alphar)**2
        xlenn = xlenn + (dx + du - dd       )**2
        xlena = xlena + (dx + du            )**2
        eps(1)= eps(1) +  dx*du
        eps(2)= eps(2) +  dx*(ul(i,2,4) - ul(i,1,4))
        xx(i) = (xl(i,2) + xl(i,1))*0.5d0
      end do

!     Compute temperature change

      ta   = 0.5d0*(tl(1) + tl(2)) - d(9)

!     Compute strain forms

      if(dtype.lt.0) then

!       Logarithmic stretch strain and deformed length

        eps(1) = 0.5d0*log(xlena/xlen0)
        xlen0 = sqrt(xlen0)
        xlen  = sqrt(xlena)

      else

!       Green or linear strain

        if(nonlin) then
          eps(1) = 0.5d0*xlena/xlen0 - 0.5d0
        else
          eps(1) = eps(1)/xlen0
          eps(2) = eps(2)/xlen0
        endif
        xlen0 = sqrt(xlen0)
        xlen  = xlen0
      endif

      end subroutine strn1d

      subroutine trcnnd(ix,sig,dt,st)

      implicit  none

      integer       :: i,ll

      integer       :: ix(*)
      real (kind=8) :: dt(*),st(*),sig(*)

      save

      do i = 1,2

        ll = ix(i)
        if(ll.gt.0) then

          dt(ll) = dt(ll) + 1.d0

!         Stress projections

          st(ll) = st(ll) + sig(1)

        endif
      end do

      end subroutine trcnnd
