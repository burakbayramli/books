!$Id:$
      subroutine inmate(d,tdof,ndv,eltype)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Input material parameters for material models

!      Inputs:
!         ietype    - Element type indicator
!                     1: Mechanical solid
!                     2: Truss
!                     3: Frame
!                     4: Plate
!                     5: Shell/Membrane
!                     6: Thermal solid
!                     7: Three-dimensional
!                     8: Unspecified
!      Outputs:
!         d(*)      - Material set parameters
!         tdof      - Dof to specify temperature location in
!                     coupled thermo-mechanical problems
!         ndv       - Number of element history variables
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'cdata.h'
      include  'cdat1.h'
      include  'chdata.h'
      include  'eldata.h'
      include  'elplot.h'
      include  'hdata.h'
      include  'iofile.h'
      include  'pglob1.h'
      include  'pmod2d.h'
      include  'refnd.h'
      include  'refng.h'
      include  'sdata.h'
      include  'setups.h'

      character (len=15) :: text(2)
      character (len=13) :: wd(8)

      logical       :: pcomp,erflag, errck, tinput, inputs
      logical       :: cflag,eflag,fflag,iflag,sflag,tflag,uflag, oned
      logical       :: efl,flg
      integer       :: eltype,ietype,imat,ndv,nvis,tdof,i,ii,j,jj,nn
      integer       :: n1,n3,nseg,nhd,ntm,naug,tmat,umat,uprm
      real (kind=8) :: bulk,e1,e2,e3,nu12,nu23,nu31,g12,g23,g31,rad,t1
      real (kind=8) :: ev(10),dd(6,6),alp(3),d(*)

      save

      data      wd/'Plane Stress' ,'Plane Strain' ,'Axisymmetric',
     &             'Plate & Shell','Truss & Frame','Thermal',
     &             '3-Dimensional','Unspecified'   /

!     PART 1: Set default values
      tdof   = gtdof
      etype  = 1
      dtype  = gdtype
      ietype = abs(eltype)

      if(dtype.gt.0 .or. ietype.ne.1) then
        fflag  = .false.
        sflag  = .true.
      else
        fflag  = .true.
        sflag  = .false.
      endif

!     Number of stress/strain history terms/pt
      if(ndm.eq.3) then
        ntm = 6
      elseif(ndm.eq.2) then
        ntm = 4
      else
        ntm = 1
      endif

      cflag  = .false.
      eflag  = .false.
      erflag = .false.
      hflag  = .false.
      iflag  = .false.
      oned   = .false.
      plasfl = .false.
      tflag  = .false.
      uflag  = .false.
      viscfl = .false.
      if(nen.gt.4 .and. ndm.eq.2 .or.
     &   nen.gt.8 .and. ndm.eq.3 ) then
        ii = 3
      else
        ii = 2
      endif
      jj   = ii - 1
      imat = 1
      lref = 0
      naug = 0
      nvis = 0
      nseg = 0

!     Default Mass: Consistent
      d( 7) = 1.d0

!     Default augmenting on
      d(185) = 1.d0

!     Default angle in degrees
      rad  = atan(1.d0)/45.d0
      d(31)= d(31)/rad

!     Solid type
      if(ietype.eq.1 .or. ietype.eq.6) then
        stype = g2type

!     Truss/frame type
      elseif(ietype.eq.2 .or.ietype.eq.3) then
        if(ietype.eq.2) oned   = .true.
        stype = 5
        lref  = gref
        sref  = 0
        if(lref.eq.1) then
          do i = 1,ndm
            refx(i) = grefx(i)
            tref(i) = 0.0d0
          end do
        else
          do i = 1,ndm
            refx(i) = gtref(i)
            tref(i) = 0.0d0
          end do
        end if

!     Plate/shell type
      elseif(ietype.eq.4) then
        stype = 4
        ii    = 3
      elseif(ietype.eq.5) then
        stype = 4
        ii    = 2
      elseif(ietype.eq.7) then
        stype = 7
      endif

!     Rayleigh damping set
      if(ietype.ne.6) then
        d(77) = gray(1)
        d(78) = gray(2)
      endif

      d(5)   = ii
      d(6)   = jj
      d(14)  = 1.d0
      alp(1) = 0.d0

!     PART 2: Poll for legal input records: Stop on blank record
      inputs = .true.

      do while(inputs)

!       Input record
        errck = .true.
        do while(errck)
          errck = tinput(text,2,ev,9)
        end do ! while

!       Plate/Shell Thickness
        if    (ietype.ne.2.and.ietype.ne.3
     &                    .and. pcomp(text(1),'thic',4)) then
          d(14) = ev(1)
          if(ev(2).eq.0.0d0) then
            d(10) = 5.d0/6.d0
          else
            d(10) = ev(2)
          endif
          d(102) = ev(3)

!       Mass density
        elseif(pcomp(text(1),'dens',4)) then
          d(4)  = ev(1)
          if(ev(2).eq.0.0d0) then
            d(69) = 1.d0
          else
            d(69) = ev(2)
          endif

!       Damping factor
        elseif(pcomp(text(1),'damp',4)) then
          if(pcomp(text(2),'rayl',4)) then
            d(77) = ev(1)
            d(78) = ev(2)
          else
            d(70) = ev(1)
          endif

!       Mass matrix type
        elseif(pcomp(text(1),'mass',4)) then
          if(pcomp(text(2),'lump',4)) then
            d(7) = 0.0d0
          elseif(pcomp(text(2),'cons',4)) then
            d(7) = 1.0d0
          elseif(pcomp(text(2),'off',3)) then
            d(7) = -1.0d0
          else
            d(7) = ev(1)
          endif

!       Normal surface loading
        elseif((ietype.eq.4.or.ietype.eq.5)
     &                    .and. pcomp(text(1),'load',4)) then

          d(8)  = ev(1)

!       Solid material body forces, b,  and heat source term Q
        elseif(pcomp(text(1),'body',4)) then

          if(pcomp(text(2),'heat',4)) then
            d(66) = ev(1)
          else
            d(11) = ev(1)
            d(12) = ev(2)
            d(13) = ev(3)
          endif

!       Orthotropic material principal direction angle
        elseif((ietype.eq.1.or.ietype.eq.4.or.ietype.eq.5)
     &                    .and. pcomp(text(1),'angl',4)) then

          d(31) = ev(1)

!       Frame/Truss: Cross section properties
        elseif((ietype.eq.2.or.ietype.eq.3)
     &                    .and. pcomp(text(1),'cros',4)) then

          cflag = .true.

          do i = 1,7
            d(31+i) = ev(i)
          end do

          if(ev(5).eq.0.0d0) then
            d(36) = d(33) + d(34)
          endif
          if(ev(6).eq.0.0d0) then
            d(37) = 5.d0/6.d0
          endif
          if(ev(7).eq.0.0d0) then
            d(38) = 5.d0/6.d0
          endif

!       Frame: Reference node/vector set
        elseif(pcomp(text(1),'refe',4)) then

          if    (pcomp(text(2),'node',4)) then
            lref = 1
            do i = 1,ndm
              refx(i) = ev(i)
            end do
          elseif(pcomp(text(2),'vect',4)) then
            lref = 2
            do i = 1,ndm
              refx(i) = ev(i)
            end do
          elseif(pcomp(text(2),'shea',4)) then
            sref = 1
            do i = 1,ndm
              tref(i) = ev(i)
            end do
          else
            lref = 0
            write(iow,4008)
            if(ior.lt.0) then
              write(*,4008)
            end if
          endif

!       Frame: No Shear Option
        elseif(ietype.eq.3 .and. pcomp(text(1),'shea',4)) then

          if(pcomp(text(2),'off',3)) then
            d(79) = 1.0d0
          else
            d(79) = 0.0d0
          endif

!       Truss/Frame: Nonlinear flag
        elseif((ietype.eq.2.or.ietype.eq.3)
     &                    .and. pcomp(text(1),'nonl',4)) then

          if(ev(1).eq.0.0d0) then
            d(39) = 1.0d0
          else
            d(39) = 0.0d0
          endif

!       Kinematics type: Small deformation
        elseif(pcomp(text(1),'smal',4)) then

          dtype = 1
          if(ietype.eq.1) then
            sflag = .true.
            fflag = .false.
          endif

!       Kinematics type: Finite deformation
        elseif(pcomp(text(1),'fini',4)) then

          dtype = -1
          if(ietype.eq.1) then
            fflag = .true.
            sflag = .false.
          endif

!       Element type: Displacement
        elseif(pcomp(text(1),'disp',4)) then

          etype = 1

!       Element type: Mixed (B-Bar)
        elseif(pcomp(text(1),'mixe',4)) then

          etype = 2

!       Element type: Enhanced Strain
        elseif(pcomp(text(1),'enha',4)) then

          etype = 3

!       Interpolation type: Nurbs
        elseif(pcomp(text(1),'nurb',4)) then

          d(189) = 1.0d0
          do i = 1,3
            if(ev(i).gt.0.0d0) then
              d(189+i) = ev(i)
            elseif(ev(i).lt.0.0d0) then
              d(189+i) = 0.0d0
            else
              d(189+i) = 5.d0
            endif
          end do ! i

!       Interpolation type: VEM
        elseif(pcomp(text(1),'vem',3)) then

          d(189) = 8.0d0
          nh1    = nh1 + 1

!       Initial data
        elseif(pcomp(text(1),'init',4)) then

!         Constant initial stress state
          if( pcomp(text(2),'stre',4)) then
            d(160) = 1
            do i = 1,6
              d(160+i) = ev(i)
            end do ! i
          elseif( pcomp(text(2),'augm',4)) then
            d(160) = 2
          endif

!       Quadrature data
        elseif((ietype.eq.1.or.ietype.eq.5.or.ietype.eq.6)
     &              .and. pcomp(text(1),'quad',4)) then

!         Limit quadrature for built-in elements
          if(eltype.gt.0) then
            ii = max(-1,min(3,nint(ev(1))))
            jj = max( 1,min(3,nint(ev(2))))
          else
            ii = nint(ev(1))
            jj = nint(ev(2))
          endif

          d(5) = ii

          d(6) = jj

!       Temperature dof
        elseif(ietype.ne.6 .and.
     &     (pcomp(text(1),'temp',4) .or. pcomp(text(1),'volt',4))) then

          tdof = nint(ev(1))

!       Input solution type
        elseif((ietype.eq.1 .or. ietype.eq.6)  .and.
     &                     pcomp(text(1),'plan',4)) then
          if( pcomp(text(2),'stre',4)) then
            stype = 1
          else
            stype = 2
          endif

        elseif((ietype.eq.1 .or. ietype.eq.6)  .and.
     &                     pcomp(text(1),'axis',4)) then
          stype = 3

!       Penalty parameter
        elseif(pcomp(text(1),'pena',4)) then

          d(60) = ev(1)

!       Augmentation switch
        elseif(pcomp(text(1),'augm',4)) then

          if(pcomp(text(2),'off',3)) then
            d(185) = 0.0d0
          else
            d(185) = 1.0d0
          endif

!       Thermal properties
        elseif(ietype.ne.6 .and. pcomp(text(1),'ther',4)) then

          tflag = .true.

!         Orthotropic inputs
          if(pcomp(text(2),'orth',4)) then

            alp(1) = ev(1)
            alp(2) = ev(2)
            alp(3) = ev(3)
            d(9)   = ev(4)

!         Isotropic inputs
          else

            alp(1) = ev(1)
            alp(2) = ev(1)
            alp(3) = ev(1)
            d(9)   = ev(2)

          endif

!       Elastic properties
        elseif(ietype.ne.6 .and. pcomp(text(1),'elas',4)) then

          eflag  = .true.

!         Transverse isotropy inputs
          if(pcomp(text(2),'tran',4)) then

            iflag = .false.
            imat  =  2

            e1   = ev(1)
            e2   = ev(2)
            e3   = ev(1)
            nu12 = ev(3)
            nu23 = ev(3)
            nu31 = ev(4)
            g12  = ev(1)/(2.d0 + nu31 + nu31)
            g23  = ev(5)
            g31  = ev(5)

!         Orthotropic inputs
          elseif(pcomp(text(2),'orth',4)) then

            iflag = .false.
            imat  =  2

            e1   = ev(1)
            e2   = ev(2)
            e3   = ev(3)
            nu12 = ev(4)
            nu23 = ev(5)
            nu31 = ev(6)
            g12  = ev(7)
            g23  = ev(8)
            g31  = ev(9)

!         Finite Elastic Models
!         Regular compressible Neo-Hookean

          elseif(ietype.eq.1 .and. pcomp(text(2),'neoh',4)) then

            imat  =  1
            dtype = -1
            fflag = .true.
            sflag = .false.

            e1    = ev(1)
            nu12  = ev(2)

!         Isotropic inputs
          else

            imat  = 2
            if(pcomp(text(2),'stve',4).or.pcomp(text(2),'stvk',4)) then
              dtype = -1
              fflag = .true.
              sflag = .false.
            endif
            iflag = .true.

            e1    = ev(1)
            e2    = e1
            e3    = e1
            nu12  = ev(2)

            nu23  = nu12
            nu31  = nu12
            g12   = 0.5d0*e1/(1.d0 + nu12)
            g23   = g12
            g31   = g12
            bulk  = e1/(1.d0 - 2.d0*min(0.49999999d0,nu12))/3.d0

          endif
!       Solid Mechanics Representative Volume Material behavior
        elseif(ietype.ne.6 .and. (pcomp(text(1),'srve',4)   .or.
     &                            pcomp(text(1),'frve',4)   .or.
     &                            pcomp(text(1),'rve' ,3))) then

          if(ntasks.le.1) then
            write(iow,*) ' *ERROR* Use RVE material only for np > 1'
            if(rank.eq.0) then
              write(*,*) ' *ERROR* Use RVE material only for np > 1'
            endif
            call plstop(.true.)

          elseif(rank.eq.0) then

            nrve = nrve + 1
            if(nrve.gt.ntasks-1) then
              write(iow,*) ' *ERROR* Number RVEs > processors available'
              if(rank.eq.0) then
                write(*,*) ' *ERROR* Number RVEs > processors available'
              endif
              call plstop(.true.)
            endif
            prtyp(1,ma) = 2
            if(pcomp(text(1),'srve',4)) then  ! Small deformation
              d(40) = 4.0d0 ! Stres  RVE
              prtyp(2,ma) =  1
            else                              ! Finite deformation
              dtype = -1
              prtyp(2,ma) = -1
            endif
            imat = 13
            e1   = 1.0d0 ! Prevents error message
            d(1) = e1
            nh1  = nh1 + 6 + 1 ! Store stresses plus one

            if(.not.pcomp(text(2),'    ',4)) then
              i = index(xxx,',')
              if(i.eq.0) then
                i = index(xxx,' ')
              endif

              do j = i+1,i+20
                if( xxx(j:j).ne.' ' ) then
                    exit
                endif
              end do ! j
              i = index(xxx(j:j+70),' ')
              matfile(ma)   = xxx(j:j+i)
            endif

          endif

!       Thermal Representative Volume Material behavior
        elseif(ietype.eq.6 .and. pcomp(text(1),'rve' ,3)) then

          if(ntasks.le.1) then
            write(iow,*) ' *ERROR* Use RVE material only for np > 1'
            if(rank.eq.0) then
              write(*,*) ' *ERROR* Use RVE material only for np > 1'
            endif
            call plstop(.true.)

          elseif(rank.eq.0) then

            nrve = nrve + 1
            if(nrve.gt.ntasks-1) then
              write(iow,*) ' *ERROR* Number RVEs > processors available'
             if(rank.eq.0) then
                write(*,*) ' *ERROR* Number RVEs > processors available'
              endif
              call plstop(.true.)
            endif

            prtyp(1,ma) = 1
            prtyp(2,ma) = 1
            d(40)       = 5.0d0 ! Thermal RVE
            tmat = 1
            t1   = 1.0d0 ! Prevents error message
            nh1  = nh1 + 4 + 1 ! Store temperature and gradient

            if(.not.pcomp(text(2),'    ',4)) then
              i = index(xxx,',')
              if(i.eq.0) then
                i = index(xxx,' ')
              endif

              do j = i+1,i+20
                if( xxx(j:j).ne.' ' ) then
                    exit
                endif
              end do ! j
              i = index(xxx(j:j+70),' ')
              matfile(ma)   = xxx(j:j+i)
            endif
          endif

!       Viscoelastic properties
        elseif(ietype.ne.6 .and. pcomp(text(1),'visc',4)) then

          viscfl = .true.
          plasfl = .false.
          d(40)  = 2.d0  ! Constitution is now Viscoelastic
          nvis   = nvis + 1
          if(nvis.le.3) then
            d(50+2*nvis-1) = ev(1)
            d(50+2*nvis  ) = ev(2)
            d(57)          = nvis
          else
            write(iow,4009)
            call plstop(.true.)
          endif

!       Plasticity properties
        elseif(ietype.ne.6 .and. pcomp(text(1),'plas',4)) then

          plasfl = .true.
          viscfl = .false.
          d(40)  = 1.d0  ! Constitution is now Plastic

!         Mises yield/loading function
          if(pcomp(text(2),'mise',4)) then

            d(41) = ev(1)
            d(42) = ev(2)
            d(43) = ev(3)
            if(d(41).eq.0.0d0) d(41) = 1.d+20
            if(d(42).eq.0.0d0) d(42) = d(41)
            if(d(43).eq.0.0d0) d(43) = 1.d0
            d(46) = 1.d0  ! Mises flag

!         Drucker-Prager yield/loading function
          elseif(pcomp(text(2),'druc',4)) then

            d(41) = ev(1)
            d(42) = ev(2)
            if(d(42).eq.0.0d0) d(42) = d(41)
            d(46) = 2.d0  ! Drucker-Prager flag

!         Drucker-Prager yield/loading function
          elseif(pcomp(text(2),'lode',4)) then

            d(41) = ev(1)
            d(42) = ev(2)
            if(d(42).eq.0.0d0) d(42) = d(41)
            d(46) = 3.d0  ! Drucker-Prager flag

!         Hardening parameters
          elseif(pcomp(text(2),'hard',4)) then

            d(44)  = ev(1)
            d(45)  = ev(2)

!         Linear segment hardening parameters
          elseif(pcomp(text(2),'segm',4)) then

            nseg          = nseg + 1
            if(nseg.gt.6) then
              write(iow,4012)
              call plstop(.true.)
            endif
            d(130)        = nseg
            d(128+3*nseg) = ev(1)
            d(129+3*nseg) = ev(2)
            d(130+3*nseg) = ev(3)

          endif

!       Angular velocity: rad/sec
        elseif(ietype.ne.6 .and. pcomp(text(1),'omeg',4)) then

          d(65) = ev(1)

!       Fourier Heat Conduction properties
        elseif(pcomp(text(1),'four',4)) then

          hflag = .true.
          d(30) = 1.0d0 ! Heat constitution added
          tmat  = 2

!         Orthotropic inputs
          if(pcomp(text(2),'orth',4)) then

            d(61) = ev(1)
            d(62) = ev(2)
            d(63) = ev(3)
            d(64) = ev(4)

!         Isotropic inputs
          else

            d(61) = ev(1)
            d(62) = ev(1)
            d(63) = ev(1)
            d(64) = ev(2)

          endif

!         Thermal dof for mechanical solid and truss problems
          if(ietype.eq.1 .and. tdof.eq.0 .or. ietype.eq.2) then

            tdof = ndm + 1

          endif

!       Ground motion acceleration factors/proportional load numbers
        elseif(ietype.ne.6 .and. pcomp(text(1),'grou',4)) then

          do i = 1,ndm
            d(70+i) = ev(2*i-1)
            d(73+i) = ev(2*i)
          end do

!       Constitutive solution start value (0 = elastic)
        elseif(pcomp(text(1),'star',4)) then

          if(pcomp(text(2),'elas',4)) then
            d(84) = -1.0d0
          else
            d(84) =  1.0d0
          endif

!       User Material Model interface
        elseif(pcomp(text(1),'ucon',4)) then

!         Default user constitutive equation number
          umat    = 1
          uprm    = ndd - nud
          n1      = 0
          n3      = 0

          call uconst(text(2),ev,d(1), d(uprm+1),n1,n3, umat)

          d(uprm) = umat + 100

!         Deactivate standard program models
          sflag  = .false.
          fflag  = .false.
          uflag  = .true.

!         Increase number of history terms/quadrature point
          nh1    = nh1 + n1
          nh3    = nh3 + n3
          e1     = 1.0d0

!       Check end of data
        elseif(pcomp(text(1),'    ',4)) then

!         Transfer to sets and checks
          inputs = .false.

        endif

      end do ! while

!     PART 3: Set final parameters and output material state

!     Set moduli
      if(ietype.ne.6) then

!       Small deformation options
        if(sflag) then
          d(1)    = e1
          d(2)    = nu12
          d(3)    = alp(1)

          dd(1,1) =  1.d0/e1
          dd(2,2) =  1.d0/e2
          dd(3,3) =  1.d0/e3


          dd(1,2) = -dd(2,2)*nu12
          dd(2,3) = -dd(3,3)*nu23
          dd(3,1) = -dd(1,1)*nu31

!         1-Dimensional Models
          if(stype.eq.5) then
            dd(2,2) =  1.d0
            dd(3,3) =  1.d0
            dd(1,2) =  0.d0
            dd(2,3) =  0.d0
            dd(3,1) =  0.d0

!         Plane Stress Models
          elseif(stype.eq.1 .or. stype .eq.4) then

            d(90)   =  dd(3,1)
            d(91)   =  dd(2,3)

            dd(3,3) =  1.d0
            dd(2,3) =  0.d0
            dd(3,1) =  0.d0

          endif

          dd(2,1) = dd(1,2)
          dd(3,2) = dd(2,3)
          dd(1,3) = dd(3,1)

!         Mechanical modulus properties
          call invert(dd,3,6)

!         Save moduli
          d(21) = dd(1,1)
          d(22) = dd(2,2)
          d(23) = dd(3,3)
          d(24) = dd(1,2)
          d(25) = dd(2,3)
          d(26) = dd(3,1)
          d(27) = g12
          d(28) = g23
          d(29) = g31

!         Thermal properties
          d(47) = dd(1,1)*alp(1) + dd(1,2)*alp(2) + dd(1,3)*alp(3)
          d(48) = dd(2,1)*alp(1) + dd(2,2)*alp(2) + dd(2,3)*alp(3)
          d(49) = dd(3,1)*alp(1) + dd(3,2)*alp(2) + dd(3,3)*alp(3)

!         Set for plane stress problems
          if(stype.eq.1 .or. stype .eq.4) then

            d(23) = 0.0d0
            d(49) = 0.0d0

            d(92) = alp(3)

          endif

!         Output parameters for element
          if(plasfl) then
            jj   = ii
            d(6) = jj
          endif

!         Output elastic properties
          if(eflag) then
            if(stype.eq.5 .or. iflag) then
              write(iow,2000) wd(stype),e1,nu12
              if(ior.lt.0) then
                write(*,2000) wd(stype),e1,nu12
              endif
            else
              write(iow,2001) wd(stype),e1,e2,e3,nu12,nu23,nu31,
     &                        g12,g23,g31,d(31)
              if(ior.lt.0) then
                write(*,2001) wd(stype),e1,e2,e3,nu12,nu23,nu31,
     &                        g12,g23,g31,d(31)
              endif
            endif
            if(stype.ne.5 .and. stype.ne.7) then
              write(iow,2018) d(14)
              if(nint(d(102)).gt.1) write(iow,2051) nint(d(102))
            endif
            if(stype.eq.4) then
              write(iow,2021) d(8)
            endif
            if(ietype.eq.1 .or.ietype.eq.5) write(iow,2019) ii,jj
            if(ior.lt.0) then
              if(stype.ne.5 .and. stype.ne.7) then
                write(*,2018) d(14)
                if(nint(d(102)).gt.1) write(*,2051) nint(d(102))
              endif
              if(stype.eq.4) then
                write(*,2021) d(8)
              endif
              if(ietype.eq.1 .or.ietype.eq.5) write(*,2019) ii,jj
            endif
          elseif(.not.uflag .and. imat.ne.13) then
            write(iow,4000)
            if(ior.lt.0) write(*,4000)
            erflag = .true.
          endif

!         Output thermal expansions
          if(tflag) then
            write(iow,2002) alp,d(9),tdof
            if(ior.lt.0) then
              write(*,2002) alp,d(9),tdof
            endif
          endif

!         Output fourier heat conduction properties
          if(hflag) then
            write(iow,2020) wd(stype),(d(i),i=61,64)
            if(ior.lt.0) then
              write(*,2020) wd(stype),(d(i),i=61,64)
            endif
          endif

!       Finite deformation options
        elseif(fflag)then

!         Output Regular NeoHookean
          if(imat.eq.1) then

            bulk = e1/(1.d0 - nu12*2.d0)/3.d0
            g12  = e1/(1.d0 + nu12)/2.d0
            if(ior.lt.0) then
              write(*,2010) ' ',e1,nu12,bulk,g12
            endif
            write(iow,2010) ' ',e1,nu12,bulk,g12

!           Compute Lame' parameters
            d(1)  = e1
            d(2)  = nu12
            d(3)  = alp(1)

            d(21) = bulk - 2.d0/3.d0*g12
            d(22) = g12

!         St. Venant-Kirchhoff
          elseif(imat.eq.2) then

            if(iflag) then
              write(iow,2000) wd(stype),e1,nu12
              if(ior.lt.0) then
                write(*,2000) wd(stype),e1,nu12
              endif
            else
              write(iow,2001) wd(stype),e1,e2,e3,nu12,nu23,nu31,
     &                        g12,g23,g31,d(31)
              if(ior.lt.0) then
                write(*,2001) wd(stype),e1,e2,e3,nu12,nu23,nu31,
     &                        g12,g23,g31,d(31)
              endif
            endif

!           Set material parameters
            d(1)    = e1
            d(2)    = nu12
            d(3)    = alp(1)

            dd(1,1) =  1.d0/e1
            dd(2,2) =  1.d0/e2
            dd(3,3) =  1.d0/e3

            dd(1,2) = -dd(2,2)*nu12

            if(stype.eq.1) then          ! Plane stress option
              dd(2,3) = 0.0d0
              dd(3,1) = 0.0d0
            else                         ! Other options
              dd(2,3) = -dd(3,3)*nu23
              dd(3,1) = -dd(1,1)*nu31
            endif

            dd(2,1) =  dd(1,2)
            dd(3,2) =  dd(2,3)
            dd(1,3) =  dd(3,1)

!           Mechanical modulus properties
            call invert(dd,3,6)

!           Save moduli
            d(21) = dd(1,1)
            d(22) = dd(2,2)
            d(23) = dd(3,3)
            d(24) = dd(1,2)
            d(25) = dd(2,3)
            d(26) = dd(3,1)
            d(27) = g12
            d(28) = g23
            d(29) = g31

!           Thermal properties
            d(47) = dd(1,1)*alp(1) + dd(1,2)*alp(2) + dd(1,3)*alp(3)
            d(48) = dd(2,1)*alp(1) + dd(2,2)*alp(2) + dd(2,3)*alp(3)
            d(49) = dd(3,1)*alp(1) + dd(3,2)*alp(2) + dd(3,3)*alp(3)

          endif
        endif

!       Output shell/plate thickness
        if(stype.ne.5 .and. stype.ne.7) then
          write(iow,2018) d(14)
          if(nint(d(102)).gt.1) write(iow,2051) nint(d(102))
        endif
        if(stype.eq.4) then
          write(iow,2021) d(8)
        endif
        if(ior.lt.0) then
          if(stype.ne.5 .and. stype.ne.7) then
            write(*,2018) d(14)
            if(nint(d(102)).gt.1) write(*,2051) nint(d(102))
          endif
          if(stype.eq.4) then
            write(*,2021) d(8)
          endif
        endif

!       Output density and body loading
        if(ior.lt.0) then
          write(*,2029) d(4),d(11),d(12),d(13)
        endif
        write(iow,2029) d(4),d(11),d(12),d(13)

!       Output constant initial stresses
        if(nint(d(160)).eq.1) then
          if(ietype.eq.2 .or. ietype.eq.3) then
            j = 1
          else
            j = 6
          endif
          write(iow,2044) (d(160+i),i=1,j)
          if(ior.lt.0) then
            write(*,2044) (d(160+i),i=1,j)
          endif
        elseif(nint(d(160)).eq.2) then
          if(ietype.eq.2 .or. ietype.eq.3) then
            naug = 1
            write(iow,2063)
            if(ior.lt.0) then
              write(*,2063)
            endif
          endif
        endif

!       Output angular velocity
        if(d(65).ne.0.0d0) then
          if(ior.lt.0) then
            write(*,2030) d(65)
          endif
          write(iow,2030) d(65)
        endif

!       Output ground acceleration factors
        flg = .false.
        efl = .false.
        do i = 1,ndm
          if(nint(d(73+i)).gt.0 ) flg = .true.
          if(nint(d(73+i)).gt.10) then
            efl = .true.
          endif
        end do
        if(flg) then
          write(iow,2023) (d(70+i),nint(d(73+i)),i=1,ndm)
          if(ior.lt.0) then
            write(*,2023) (d(70+i),nint(d(73+i)),i=1,ndm)
          endif
        endif

        if(efl) then
          write(iow,4007)
          if(ior.lt.0) write(*,4007)
          erflag = .true.
        endif

!       Output section properties
        if(cflag) then
          if(ietype.eq.2) then
            j = 32
          else
            j = 38
          endif
          write(iow,2015) (d(i),i=32,j)
          if(ior.lt.0) then
            write(*,2015) (d(i),i=32,j)
          endif
          ev(1) = 1.d-8*max(abs(d(33)),abs(d(34)))
          ev(2) = d(33)*d(34) - d(35)*d(35)
          if(ev(2).lt.ev(1)) then
            write(iow,4013) ev(2)
            if(ior.lt.0) then
              write(*,4013) ev(2)
            endif
            erflag = .true.
          endif
        endif

!       Output plasticity parameters
        if(plasfl) then

!         Small deformation plasticity (also for one-d problems)
          if(sflag .or. oned) then

            if(nint(d(40)).eq.1) then
              write(iow,2003) (d(i),i=41,45)
              if(ior.lt.0) then
                write(*,2003) (d(i),i=41,45)
              endif
              nn  = 1
              nhd = ntm + 1  ! epp, ep(i),i=1,ntm
            endif

          endif ! fflag/sflag

!         One dimensional model history storage
          if(oned) then
            if(nseg.eq.0) then
              nh1 = nh1 + nn + 3   ! ep(1); epp; state each layer
            else
              write(iow,2041) (d(i),i=131,130+3*nseg)
              if(ior.lt.0) then
                write(*,2041) (d(i),i=131,130+3*nseg)
              endif
              nh1 = nh1 + 4   ! ep(1); alp(1); epp; state each layer
            endif

!         Multi dimensional model history storage
          else
            if(ndm.eq.3) then
              nh1 = nh1 + nhd*nn + 1
            else
              if(stype.eq.1) then
!                               ep(3), beta(3), ep; state (plane stress)
                nh1 = nh1 + nn + 7
              else
                if(fflag) then
                  nh1 = nh1 + nhd*nn + 1
                else
                  nh1 = nh1 + 5*nn + 1 ! ep(4); ep; state (plane strain)
                endif
              endif
            endif
          endif

        endif ! plasfl

!       Output visco-elastic properties
        if(viscfl) then
          write(iow,2004) (d(i),i=51,50+2*nvis)
          if(ior.lt.0) then
            write(*,2004) (d(i),i=51,50+2*nvis)
          endif
          if(fflag) then
            i = 1
          else
            i = 0
          endif
          if(oned) then
            nh1 = nh1 + i + 1 + nvis   ! xi; eps_n; hh(*) each point
          else
            if(ndm.eq.3) then
              nh1 = nh1 + i + 6 + 6*nvis !xi; eps_n(6); hh(6) each point
            else
              nh1 = nh1 + i + 4 + 4*nvis !xi; eps_n(4); hh(4) each point
            endif
          endif
        endif ! viscfl

!       Constitutive start (.ne.0 for nonclassical elastic)
        if(nint(d(84)).lt.0) then
          write(iow,2066) 'Elastic solution'
          if(ior.lt.0) then
            write(*,2066) 'Elastic solution'
          endif
        elseif(nint(d(84)).gt.0) then
          write(iow,2066) 'State at last time step'
          if(ior.lt.0) then
            write(*,2066) 'State at last time step'
          endif
        endif

!       Kinematics type
        if(ietype.ne.6) then
          if(dtype.gt.0) then
            write(iow,2005)
            if(ior.lt.0) then
              write(*,2005)
            endif
          else
            write(iow,2006)
            if(ior.lt.0) then
              write(*,2006)
            endif
          endif
        endif

!       Element type
        if(ietype.eq.1) then
          if(etype.eq.1) then
            write(iow,2012)
            if(ior.lt.0) then
              write(*,2012)
            endif
          elseif(etype.eq.2) then
            write(iow,2013)
            if(ior.lt.0) then
              write(*,2013)
            endif
          elseif(etype.eq.3) then
            write(iow,2014)
            if(ior.lt.0) then
              write(*,2014)
            endif
          endif
        elseif(ietype.eq.3) then
          if(d(79).gt.0.0d0 .or. (ndm.eq.3  .and. dtype.ge.0)) then
            write(iow,2064)
            if(ior.lt.0) then
              write(*,2064)
            endif
          else
            write(iow,2065)
            if(ior.lt.0) then
              write(*,2065)
            endif
          endif
        endif

!       Output augmentation option
        if(etype.gt.1) then
          if(d(185).gt.0.0d0) then
            write(iow,2084) 'On'
            if(ior.lt.0) then
              write(*,2084) 'On'
            endif
          else
            write(iow,2084) 'Off'
            if(ior.lt.0) then
              write(*,2084) 'Off'
            endif
          endif
        endif

!     Thermal element only
      elseif(ietype.eq.6) then

        write(iow,2020) wd(stype),d(61),d(62),d(63),d(64),d(4)
        write(iow,2019) ii,jj
        if(ior.lt.0) then
          write(*,2020) wd(stype),d(61),d(62),d(63),d(64),d(4)
          write(*,2019) ii,jj
        endif

      endif

!     Output RVE type
      if(imat.eq.13 .or. tmat.eq.1) then
        write(iow,2091) ' Hill-Mandel ',wd(stype)
        if    (prtyp(1,ma).eq.1) then
          write(iow,2092) 'Thermal ',matfile(ma)
        elseif(prtyp(1,ma).eq.2) then
          write(iow,2092) 'Stress ',matfile(ma)
        endif
      endif

!     Interpolation type
      if(nint(d(189)).eq.1) then
        write(iow,2090) 'Global NURBS',(i,nint(d(189+i)),i=1,ndm)
        if(ior.lt.0) then
          write(*,2090) 'Global NURBS',(i,nint(d(189+i)),i=1,ndm)
        endif
      endif

!     Mass type
      if(d(4).gt.0.0d0) then
        if(d(7).eq.0.0d0) then
          write(iow,2007)
          if(ior.lt.0) then
            write(*,2007)
          endif
        elseif(d(7).eq.1.0d0) then
          write(iow,2008)
          if(ior.lt.0) then
            write(*,2008)
          endif
        else
          write(iow,2009) d(7)
          if(ior.lt.0) then
            write(*,2009) d(7)
          endif
        endif
        if(ietype.eq.3 .or. ietype.eq.5.and.dtype.lt.0) then
          write(iow,2032) d(69)
          if(ior.lt.0) then
            write(*,2032) d(69)
          endif
        endif
      endif

!     Damping factor
      if(d(70).gt.0.0d0) then
        write(iow,2046) d(70)
        if(ior.lt.0) then
          write(*,2046) d(70)
        endif
      endif

!     Rayleigh Damping factors
      if(max(abs(d(77)),abs(d(78))).gt.0.0d0) then
        write(iow,2060) d(77),d(78)
        if(ior.lt.0) then
          write(*,2060) d(77),d(78)
        endif
      endif

      if(d(39).ne.0.0d0) then

        write(iow,2016)
        if(ior.lt.0) then
          write(*,2016)
        endif
      endif

!     Output penalty value
      if(d(60).ne.0.0d0) then
        write(iow,2022) d(60)
        if(ior.lt.0) then
          write(*,2022) d(60)
        endif
      endif

!     Multiply parameters by thickness
      if(ietype.eq.1) then
        d( 4) = d( 4)*d(14)
        d(11) = d(11)*d(14)
        d(12) = d(12)*d(14)
        do i = 21,30
          d(i) = d(i)*d(14)
        end do
      elseif(ietype.eq.4 .or. ietype.eq.5.and.dtype.lt.0) then
        write(iow,2017) d(10)
        if(ior.lt.0) then
          write(*,2017) d(10)
        endif
      endif

!     Output reference coordinate/vector
      if    (sref.eq.1) then

        d(93) = sref
        do i = 1,2
          d(93+i) = tref(i)
        end do

        write(iow,2031) (i,tref(i),i=1,2)
        if(ior.lt.0) then
          write(*,2031) (i,tref(i),i=1,2)
        endif

      endif

!     Output reference coordinate/vector
      if    (lref.eq.1) then

        d(96) = lref
        do i = 1,3
          d(96+i) = refx(i)
        end do

        write(iow,2025) (i,refx(i),i=1,ndm)
        if(ior.lt.0) then
          write(*,2025) (i,refx(i),i=1,ndm)
        endif

      elseif(lref.eq.2) then

        d(96) = lref
        do i = 1,3
          d(96+i) = refx(i)
        end do

        write(iow,2026) (i,refx(i),i=1,ndm)
        if(ior.lt.0) then
          write(*,2026) (i,refx(i),i=1,ndm)
        endif

      endif

!     Save types
      d(15)  = nh1 + naug
      d(16)  = stype
      d(17)  = etype
      d(18)  = dtype
      d(19)  = tdof
      d(20)  = imat
      d(31)  = d(31)*rad
      d(193) = tmat
      if(ietype.eq.1 .or. ietype.eq.5) then
        i     = int(nh1)
        if(ndm.eq.2) then
          nh1   = nh1*ii*ii
        elseif(ndm.eq.3) then
          nh1   = nh1*ii*ii*ii
        endif
        if(etype.eq.3 .and. sflag) nh1 = nh1 + 5  ! linear element
        if(etype.eq.3 .and. fflag) nh1 = nh1 + i  ! extra quadrature
      elseif(ietype.eq.4) then
        nh1   = nh1*ii
      elseif(ietype.eq.7) then
        nh1   = nh1*ii**3
      endif

!     Set history for saving element variables
      nh3  = ndv

!     Set augmenting base value
      d(185) = d(185)*d(21)

!     Check for warnings
      if(d(4).eq.0.0d0) then
        write(iow,3000)
        if(ior.lt.0) then
          write(*,3000)
        endif
      endif

!     Check for errors
      if     (ietype.eq.1) then
        if(e1.eq.0.0d0) then
          write(iow,4001)
          if(ior.lt.0) then
            write(*,4001)
          endif
        endif
      elseif (ietype.eq.2) then
        if(e1.eq.0.0d0 .or. d(32).eq.0.0d0) then
          write(iow,4002) e1,d(32)
          if(ior.lt.0) then
            write(*,4002) e1,d(32)
          endif
        endif
      elseif (ietype.eq.3) then
        if(e1.eq.0.0d0 .or. d(32).eq.0.0d0 .or. d(33).eq.0.0d0) then
          write(iow,4003) e1,d(32),d(33)
          if(ior.lt.0) then
            write(*,4003) e1,d(32),d(33)
          endif
        endif
      elseif (ietype.eq.4) then
        if(e1.eq.0.0d0 .or. d(10).eq.0.0d0 .or. d(14).eq.0.0d0) then
          write(iow,4004) e1,d(14),d(10)
          if(ior.lt.0) then
            write(*,4004) e1,d(14),d(10)
          endif
        endif
      elseif (ietype.eq.5) then
        if(e1.eq.0.0d0 .or. d(10).eq.0.0d0 .or. d(14).eq.0.0d0) then
          write(iow,4005) e1,d(14),d(10)
          if(ior.lt.0) then
            write(*,4005) e1,d(14),d(10)
          endif
        endif
      elseif (ietype.eq.6) then
        if(d(61).eq.0.0d0) then
          write(iow,4006) e1,d(61)
          if(ior.lt.0) then
            write(*,4006) e1,d(61)
          endif
        endif
      endif

!     Errors detected in input data
      if(erflag) call plstop(.true.)

!     I/O Formats

2000  format( 5x,'M e c h a n i c a l   P r o p e r t i e s'//
     & 10x,a,' Analysis'//
     & 10x,'Modulus E       ',1p,1e12.5/
     & 10x,'Poisson ratio   ',0p,1f8.5/)

2001  format( 5x,'M e c h a n i c a l   P r o p e r t i e s'//
     & 10x,a,' Analysis'//
     & 10x,'Modulus E-1     ',1p,1e12.5/
     & 10x,'Modulus E-2     ',1p,1e12.5/
     & 10x,'Modulus E-3     ',1p,1e12.5/
     & 10x,'Poisson ratio 12',0p,1f8.5 /
     & 10x,'Poisson ratio 23',0p,1f8.5 /
     & 10x,'Poisson ratio 31',0p,1f8.5 /
     & 10x,'Modulus G-12    ',1p,1e12.5/
     & 10x,'Modulus G-23    ',1p,1e12.5 /
     & 10x,'Modulus G-31    ',1p,1e12.5/
     & 10x,'Angle (psi)     ',0p,1f8.5/)

2002  format(/5x,'T h e r m a l   E x p a n s i o n s'//
     & 10x,'Th. Alpha-1',1p,1e17.5/10x,'Th. Alpha-2',1p,1e17.5/
     & 10x,'Th. Alpha-3',1p,1e17.5/10x,'T_0        ',1p,1e17.5/
     & 10x,'Th. D.O.F. ',i7/)

2003  format(/5x,'M i s e s   P l a s t i c   P a r a m e t e r s'//
     &  10x,'Yield stress short  ',1p,1e15.5/
     &  10x,'Yield stress infin. ',1p,1e15.5/
     &  10x,'Hardening exponent  ',1p,1e15.5/
     &  8x,'Linear hardening parts'/
     &  10x,'Isotropic hardening ',1p,1e15.5/
     &  10x,'Kinematic hardening ',1p,1e15.5/)

2004  format(/5x,'V i s c o e l a s t i c   P a r a m e t e r s'//
     &   (10x,'nu-i:ratio',1p,1e15.5/10x,'lam-i:time',1p,1e15.5:))

2005  format( 10x,'Formulation : Small deformation.')
2006  format( 10x,'Formulation : Finite deformation.')

2007  format( 10x,'Mass type   : Lumped.')
2008  format( 10x,'Mass type   : Consistent.')
2009  format( 10x,'Mass type   : Interpolated:',1p,1e11.4)

2010  format( 5x,'M e c h a n i c a l   P r o p e r t i e s'//
     &        4x,a,'NeoHookean Stored Energy Function '//
     &       10x,'Modulus E       ',1p,1e12.5/
     &       10x,'Poisson ratio   ',0p,1f8.5/
     &       10x,'Bulk Modulus    ',1p,1e12.5/
     &       10x,'Shear Modulus   ',1p,1e12.5/1x)

2012  format(10x,'Element type: Displacement.')
2013  format(10x,'Element type: Mixed B-Bar.')
2014  format(10x,'Element type: Enhanced Strain.')

2015  format(/5x,'C r o s s   S e c t i o n   P a r a m e t e r s'//
     &    10x,'Area      ',1p,1e15.5:/10x,'I_xx      ',1p,1e15.5/
     &    10x,'I_yy      ',1p,1e15.5 /10x,'I_xy      ',1p,1e15.5/
     &    10x,'J_zz      ',1p,1e15.5 /10x,'Kappa_x   ',1p,1e15.5/
     &    10x,'Kappa_y   ',1p,1e15.5 / )

2016  format( 10x,'Non-linear analysis')

2017  format( 10x,'Plate/Shell : Kappa ',1p,1e15.5)

2018  format(10x,'Thickness       ',1p,1e12.5)
2019  format(
     & 10x,'Quadrature: Arrays',i3    /10x,'Quadrature: Output',i3/)

2020  format(/5x,'T h e r m a l   P r o p e r t i e s'//
     & 10x,a,' Analysis'//
     & 10x,'Cond. K-1    ',1p,1e15.5/10x,'Cond. K-2    ',1p,1e15.5/
     & 10x,'Cond. K-3    ',1p,1e15.5/10x,'Specific Heat',1p,1e15.5/
     &:10x,'Density      ',1p,1e15.5)

2021  format( 10x,'Loading - q ',1p,1e16.5)

2022  format( 10x,'Penalty - k ',1p,1e16.5)

2023  format(
     &    /5x,'P r o p o r t i o n a l   B o d y   L o a d i n g s',//
     &    10x,'1-Dir. Factor',1p,1e15.5,': Proportional Load No.',i3/:
     &    10x,'2-Dir. Factor',1p,1e15.5,': Proportional Load No.',i3/:
     &    10x,'3-Dir. Factor',1p,1e15.5,': Proportional Load No.',i3/)

2025  format(/5x,'R e f e r e n c e    C o o r d i n a t e s'//
     &        (10x,'X-',i1,' = ',1p,1e12.5:))

2026  format(/5x,'R e f e r e n c e    V e c t o r'//
     &        (10x,'v-',i1,' = ',1p,1e12.5:))

2029  format(10x,'Density         ',1p,1e12.5//
     &       10x,'1-Gravity Load  ',1p,1e12.5/
     &       10x,'2-Gravity Load  ',1p,1e12.5/
     &       10x,'3-Gravity Load  ',1p,1e12.5/1x)

2030  format(/10x,'Ang. Velocity',1p,1e15.5/)

2031  format(/5x,'S h e a r   C e n t e r   C o o r d i n a t e s'//
     &        (10x,'X-',i1,' = ',1p,1e12.5:))

2032  format( 10x,'Rotational Mass Factor:',1p,1e12.5)

2041  format(/5x,'H a r d e n i n g    P a r a m e t e r s'//
     &    12x,'Strain e_p  Isotropic Yield-Y  Kinematic Hardening-H'/
     &     (1p,1e12.5,1p,1e19.5,1p,1e23.5))

2044  format(/5x,'I n i t i a l   S t r e s s   D a t a'//
     &       10x,'11-Stress         =',1p,e13.4/:
     &       10x,'22-Stress         =',1p,e13.4/
     &       10x,'33-Stress         =',1p,e13.4/
     &       10x,'12-Stress         =',1p,e13.4/
     &       10x,'23-Stress         =',1p,e13.4/
     &       10x,'31-Stress         =',1p,e13.4/)

2046  format( 10x,'Damping     ',1p,1e16.5)

2051  format( 10x,'Thickness pts',i12)

2060  format(/8x,'Rayleigh Damping Ratios'/
     &       10x,'Mass  value: a0',1p,1e14.5/
     &       10x,'Stiff value: a1',1p,1e14.5)

2063  format(/10x,'Augmented Solution for Inextensible Behavior')

2064  format( 24x,'No shear deformation')

2065  format( 24x,'Includes shear deformation')

2066  format(/5x,'C o n s t i t u t i v e   S t a r t   S t a t e'//
     &       10x,a)

2084  format( 10x,'Augmenting   : ',a)

2090  format(10x,'Interpolation: ',a/(15x,'Quadrature ',i1,' = ',i5:))

2091  format( 5x,'M e c h a n i c a l   P r o p e r t i e s'//
     &        4x,a,'Representative Volume Element Model'//
     &       10x,a,'Analysis'/1x)

2092  format(10x,a,'Response'//
     &       10x,'RVE Model Filename: ',a)

3000  format(/10x,'Material density is zero.')

4000  format(/5x,'*ERROR* No elastic properties input.'/)

4001  format(/5x,'*ERROR* Solid Element: Modulus zero.'/)

4002  format(/5x,'*ERROR* Truss Element: Modulus  = ',1p,1e12.5/
     &        5x,'                       Area     = ',1p,1e12.5/)

4003  format(/5x,'*ERROR* Frame Element: Modulus  = ',1p,1e12.5/
     &        5x,'                       Area     = ',1p,1e12.5/
     &        5x,'                       Intertia = ',1p,1e12.5/)

4004  format(/5x,'*ERROR* Plate Element: Modulus  = ',1p,1e12.5/
     &        5x,'                       Thickness= ',1p,1e12.5/
     &        5x,'                       Kappa    = ',1p,1e12.5/)

4005  format(/5x,'*ERROR* Shell Element: Modulus  = ',1p,1e12.5/
     &        5x,'                       Thickness= ',1p,1e12.5/
     &        5x,'                       Kappa    = ',1p,1e12.5/)

4006  format(/5x,'*ERROR* Thermal Element: Conductivity zero.'/)

4007  format(/5x,'*ERROR* Incorrect proportional load number'/)

4008  format(
     & /5x,'*ERROR* Incorrect reference vector or node specification.'/)

4009  format(/5x,'*ERROR* Too many viscoelastic terms: limit = 3.'/)

4012  format(/5x,'*ERROR* Too many hardening segments: limit = 6.'/)

4013  format(/5x,'*ERROR* Inertia determinant is zero or negative'/
     &       13x,'Det = I_xx*I_yy - I_xy*I_xy = ',1p,1e12.5/)

      end subroutine inmate

      subroutine dmat2d(d,psi,dmg,betag)

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:  Rotation of material arrays from principal to
!                local element directions

!      Inputs:
!         d        - Array with material properties
!         psi      - Angle from y1-axis (local) to 1-axis (principal)

!      Outputs:
!         dmg(6,6) - Plane modulus matrix
!         betag(6) - global thermal stress/temperature

!-----[--.----+----.----+----.-----------------------------------------]
!           Variables used in subroutine

!         qm(4,4)  - Transformation matrix for plane problems

!         dml(4,4) - Local (orthotropic ) plane modulus matrix
!         dmlqj(4) - intermediate matrix for triple product

!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      integer       :: i, j
      real (kind=8) :: psi, si, co, s2, c2, cs

      real (kind=8) :: d(*)
      real (kind=8) :: dml(4,4), qm(4,4), dmlqj(4)
      real (kind=8) :: dmg(6,6), betag(6)

      save

!     Assign material properties

!     No rotation
      if(psi.eq.0.0d0) then

        dmg(1,1) = d(21)
        dmg(2,2) = d(22)
        dmg(3,3) = d(23)

        dmg(1,2) = d(24)
        dmg(2,1) = dmg(1,2)

        dmg(2,3) = d(25)
        dmg(3,2) = dmg(2,3)

        dmg(3,1) = d(26)
        dmg(1,3) = dmg(3,1)

        dmg(4,4) = d(27)
        dmg(5,5) = d(28)
        dmg(6,6) = d(29)

        dmg(1,4) = 0.0d0
        dmg(4,1) = 0.0d0

        dmg(2,4) = 0.0d0
        dmg(4,2) = 0.0d0

        dmg(3,4) = 0.0d0
        dmg(4,3) = 0.0d0

        dmg(4,5) = 0.0d0
        dmg(5,4) = 0.0d0

        dmg(4,6) = 0.0d0
        dmg(6,4) = 0.0d0

        dmg(5,6) = 0.0d0
        dmg(6,5) = 0.0d0

        betag(1) = d(47)
        betag(2) = d(48)
        betag(3) = d(49)
        betag(4) = 0.0d0
        betag(5) = 0.0d0
        betag(6) = 0.0d0

!     Orthotropic material (rotations)
      else

!       Set up constants for transformation
        si = sin(psi)
        co = cos(psi)
        s2 = si*si
        c2 = co*co
        cs = co*si

!       Set up transformation matrix for plane problem
        qm(1,1) =  c2
        qm(1,2) =  s2
        qm(1,3) =  0.0d0
        qm(1,4) =  cs
        qm(2,1) =  s2
        qm(2,2) =  c2
        qm(2,3) =  0.0d0
        qm(2,4) = -cs
        qm(3,1) =  0.0d0
        qm(3,2) =  0.0d0
        qm(3,3) =  1.0d0
        qm(3,4) =  0.0d0
        qm(4,1) = -2.d0 * cs
        qm(4,2) =  2.d0 * cs
        qm(4,3) =  0.0d0
        qm(4,4) =  c2 - s2

!       Set up local (orthotropic) plane matrix
        dml(1,1) = d(21)
        dml(2,2) = d(22)
        dml(3,3) = d(23)

        dml(1,2) = d(24)
        dml(2,1) = dml(1,2)

        dml(2,3) = d(25)
        dml(3,2) = dml(2,3)

        dml(3,1) = d(26)
        dml(1,3) = dml(3,1)

        dml(1,4) = 0.0d0
        dml(4,1) = 0.0d0

        dml(2,4) = 0.0d0
        dml(4,2) = 0.0d0

        dml(3,4) = 0.0d0
        dml(4,3) = 0.0d0

        dml(4,4) = d(27)

!       Convert plane local to global matrix
        do j = 1,4 ! {

          dmlqj(1) = dml(1,1)*qm(1,j) + dml(1,2)*qm(2,j)
     &             + dml(1,3)*qm(3,j) + dml(1,4)*qm(4,j)
          dmlqj(2) = dml(2,1)*qm(1,j) + dml(2,2)*qm(2,j)
     &             + dml(2,3)*qm(3,j) + dml(2,4)*qm(4,j)
          dmlqj(3) = dml(3,1)*qm(1,j) + dml(3,2)*qm(2,j)
     &             + dml(3,3)*qm(3,j) + dml(3,4)*qm(4,j)
          dmlqj(4) = dml(4,4)*qm(4,j)

          do i = 1,4 ! {
            dmg(i,j) = qm(1,i)*dmlqj(1) + qm(2,i)*dmlqj(2)
     &               + qm(3,i)*dmlqj(3) + qm(4,i)*dmlqj(4)
          end do ! i   }

        end do ! j   }

!       Set up global shear matrix
        dmg(5,5) = c2 * d(28) + s2 * d(29)
        dmg(5,6) = cs * ( d(29) - d(28) )
        dmg(6,5) = dmg(5,6)
        dmg(6,6) = s2 * d(28) + c2 * d(29)

!       Global thermal stiffness vector
        betag(1) = c2 * d(47) + s2 * d(48)
        betag(2) = s2 * d(47) + c2 * d(48)
        betag(3) = d(49)
        betag(4) = cs * ( d(47) - d(48) )
        betag(5) = 0.0d0
        betag(6) = 0.0d0

      endif

      end subroutine dmat2d

      subroutine elas1d(d,ta, eps, sig,dd)

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:  1-D elastic model

!      Inputs:
!        d(*)  - Material property parameters
!        ta    - Thermal strain
!        eps   - Strain

!      Outputs:
!        sig   - Stress
!        dd(2) - Moduli
!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      real (kind=8) :: ta,eps, sig,dd(2)
      real (kind=8) :: d(*)

      save

!     Compute stress
      sig   = sig + d(1)*(eps - d(3)*ta)

!     Set modulus
      dd(1) = d(1)
      dd(2) = d(1)

      end subroutine elas1d

      subroutine epps2d(d,eps,epsp,alp,epn,istrt, sig,dd,dr)

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:  Elastic plastic plane stress kinematic/isotropic
!                hardening model: Return map solution

!      Inputs:
!         d(*)      - array of material constants
!         eps(*)    - total strain
!         epsp(*)   - plastic strain at t-n
!         alp(*)    - back-stress at t-n
!         epn(*)    - equivalent plastic strain at t-n and state
!         istrt     - start state: 0 = elastic

!      Outputs:
!         sig(*)    - stresses at t-n+1
!         epsp(*)   - plastic strain at t-n+1
!         alp(*)    - back-stresses at t-n+1
!         epn(*)    - equivalent plastic strain at t-n+1
!         dd(*,*)   - material tangent matrix at t-n+1
!         dr(*,*)   - rayleigh damping matrix at t-n+1
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'eldata.h'
      include  'rdata.h'

      logical       :: state
      integer       :: i,j, istrt
      real (kind=8) :: yldtr,rad2,xlam0,fact,a1,a2
      real (kind=8) :: xkprim,hprim,beta, dyld

      real (kind=8) :: eps(*),sig(*),sigtr(3),alp(*)
      real (kind=8) :: d(*),dd(6,6),dr(6,6)
      real (kind=8) :: en(4),eta(3),epsp(*),epn(2),dum(6)

      real (kind=8) :: hard2d,ylds2d,One3

      save

      data  One3 / 0.3333333333333333d0 /

!     Check state for iterations
      if(rnmax.eq.0.0d0) then  ! First iteration in step
        if(istrt.le.0) then
          state = .false.
          dyld  = 0.0d0
        else
          state = .true.
          dyld  = 1.d-08*d(41)
        endif
      else                     ! Not first iteration in step
        state = .true.
        dyld  = 0.0d0
      endif

!     Set-up elastic moduli
      call dmat2d(d,0.d0,dd,dum)
      do i = 1,4
        do j = 1,4
          dr(j,i) = dd(j,i)
        end do ! j
      end do ! i

!     Compute trial stress
      sigtr(1) = dd(1,1)*(eps(1)-epsp(1)) + dd(1,2)*(eps(2)-epsp(2))
      sigtr(2) = dd(2,1)*(eps(1)-epsp(1)) + dd(2,2)*(eps(2)-epsp(2))
      sigtr(3) = dd(4,4)*(eps(4)-epsp(3))

!     Compute yield state
      eta(1)  = sigtr(1) - alp(1)
      eta(2)  = sigtr(2) - alp(2)
      eta(3)  = sigtr(3) - alp(3)
      yldtr   = (eta(1)**2+eta(2)**2-eta(1)*eta(2))*One3 + eta(3)**2
      rad2    = hard2d(d,epn(1),xkprim,hprim)
      xlam0   = 0.0d0

!     Compute scale factor and compute elastic-plastic tangent
      if (yldtr+dyld.gt.rad2 .and.state) then

        epn(2) = 1.0d0
        dm     = ylds2d(d,sigtr,alp,eta,dd,beta,epn(1),xlam0,hprim)

!       Compute plastic strains
        a1      = (2.d0*eta(1) - eta(2))*One3
        a2      = (2.d0*eta(2) - eta(1))*One3
        epsp(1) = epsp(1) + xlam0*a1
        epsp(2) = epsp(2) + xlam0*a2
        epsp(3) = epsp(3) + xlam0*2.d0*eta(3)

!       Compute a "normal" and finish computation of tangent
        en(1) = dd(1,1)*a1 + dd(1,2)*a2
        en(2) = dd(2,1)*a1 + dd(2,2)*a2
        en(3) = 0.0d0
        en(4) = dd(4,4)*eta(3)*2.0d0
        fact  = sqrt(en(1)*a1 + en(2)*a2 + en(4)*eta(3)*2.d0 + beta)
        do i = 1,4
          en(i) = en(i)/fact
        end do
        do i = 1,4
          do j = 1,4
            dd(i,j) = dd(i,j) - en(i)*en(j)
          end do
        end do

      else
        epn(2) = 0.0d0
      endif

!     Set stresses
      sig(1) = sigtr(1)
      sig(2) = sigtr(2)
      sig(3) = 0.0d0
      sig(4) = sigtr(3)

      end subroutine epps2d

      subroutine estrsd(d,ta,eps,sig,dd,dr)

!-----[--.----+----.----+----.-----------------------------------------]
!     Linear Elastic Constitutive Model

      implicit  none

      integer       :: i,j
      real (kind=8) :: ta
      real (kind=8) :: d(*), eps(6), sig(6), dd(6,6), dr(6,6), beta(6)

      save

!     Stress:
      call dmat2d(d,d(31),dd,beta)

      do i = 1,6
        sig(i) = sig(i) - beta(i)*ta
        do j = 1,6
          sig(i)  = sig(i) + dd(i,j)*eps(j)
          dr(i,j) = dd(i,j)
        end do
      end do

!     Set plane stress case (dd(3,3) = 0.0d0)
      if(dd(3,3) .eq. 0.0d0 ) then

        eps(3) = d(90)*sig(1) + d(91)*sig(2) + d(92)*ta
        sig(3) = 0.0d0

      endif

      end subroutine estrsd

      function hard2d(d,ep,xkprim,hprim)

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Linear-saturation hardeing model for plasticity

!      Inputs:
!         d(*)     - Material parameters
!         ep       - Accumlated plastic strain value

!      Outputs:
!         xkprim   - Kinematic hardening part
!         hprim    - Isotropic hardentin part
!         hard2d   - Value of hardening parameter
!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      real (kind=8) :: hard2d

      real (kind=8) :: y0,yinf,ylin,delta,ep,eep,xk,xkprim,hprim
      real (kind=8) :: d(*)

      save

!     Extract constants
      y0     = d(41)
      yinf   = d(42)
      delta  = d(43)
      ylin   = d(44)
      hprim  = d(45)

!     Compute yield function and derivative
      eep    = exp(-delta*ep)
      xk     =  y0 + (yinf - y0)*(1.d0 - eep) + ylin*ep
      xkprim = delta*(yinf - y0)*eep          + ylin

      hard2d = xk*xk*0.3333333333333333d0

      end function hard2d

      function hvisc(x,expx)

!-----[--+---------+---------+---------+---------+---------+---------+-]
!     Purpose: Compute integration factor for viscoelastic material
!              H_visc(x) = [ 1 - exp(-x)]/x

      implicit  none

      real (kind=8) :: hvisc, x, expx

      if(x.lt.1.d-04) then

        hvisc = 1.d0 - 0.5d0*x*(1.d0 - x/3.d0*(1.d0
     &               - 0.25d0*x*(1.d0 - 0.2d0*x)))

      else

        hvisc = (1.d0 - expx)/x

      endif

      end function hvisc

      subroutine mises(d,eps,epsp,epp,istrt, sig,dd,dr)

!-----[--.----+----.----+----.-----------------------------------------]
!     Mises (J2) plasticity with isotropic and kinematic hardening

      implicit  none

      include  'rdata.h'
      include  'sdata.h'

      logical       :: conv,state
      integer       :: i, j, ntm, count, istrt
      real (kind=8) :: d(*),eps(*),epsp(*),epp(*),sig(*),dd(6,6),dr(6,6)
      real (kind=8) :: alp(6), ep(6), en(6), xi(6)
      real (kind=8) :: aa,bb,cc, press, theta, dlam, lam, xin
      real (kind=8) :: K, G, Gbar, twoG, Hi,Hi23,His23,Hk,Hk23, dyld
      real (kind=8) :: R0,Rinf,Rn,Rb, beta, expb,expl, Bbar, One3,Two3
      real (kind=8) :: tolc, s23

      real (kind=8) :: dot

      save

      data      tolc / 1.d-10 /
      data      One3 / 0.3333333333333333d0 /
      data      Two3 / 0.6666666666666667d0 /

!     Check state for iterations
      if(rnmax.eq.0.0d0) then    ! First iteration in step
        if(istrt.le.0) then
          state = .false.
          dyld  = 0.0d0
        else
          state = .true.
          dyld  = 1.0d-08*d(41)
        endif
      else                       ! Not first iteration
        state = .true.
        dyld  = 0.0d0
      endif

!     Set number stress/strain components
      if(ndm.eq.2) then
        ntm = 4
      elseif(ndm.eq.3) then
        ntm = 6
      else
        ntm = 1
      end if

!     Set parameters
      s23   = sqrt(Two3)

      G     = d(27)
      K     = d(21) - 2.d0*Two3*G

      Hi    = d(44)
      Hk    = d(45)

      R0    = s23*d(41)
      Rinf  = s23*d(42)
      beta  =     d(43)

!     Compute constants
      Hk23  = Two3*Hk
      Hi23  = Two3*Hi
      His23 =  s23*Hi

      twoG  = 2.d0*G

      expb  = exp(-beta*epp(1))
      Bbar  =  s23*beta*(Rinf - R0)*expb
      Gbar  = twoG + Hi23 + Hk23

!     Compute volumetric and deviatoric strains
      theta = (eps(1) + eps(2) + eps(3))*One3
      do i = 1,3
        ep(i)   = eps(i) - theta
      end do ! i
      do i = 4,ntm
        ep(i) = eps(i)*0.5d0
      end do ! i

      theta = theta*3.0d0

!     Compute trial values
      do i = 1,ntm
        sig(i) = sig(i) + twoG*(ep(i) - epsp(i))
        alp(i) = Hk23*epsp(i)
        xi(i)  = sig(i) - alp(i)
      end do

!     Compute trial norm of stress - back stress
      xin = sqrt(dot(xi,xi,ntm) + dot(xi(4),xi(4),ntm-3))
      Rn  = Rinf + His23*epp(1)
      Rb  = (R0  - Rinf)*expb

!     Check yield
      if(xin+dyld .gt. (Rn+Rb) .and. state) then

!       Compute consistency
        epp(2) = 1.0d0
        lam    = (xin - Rn - Rb) / (Gbar + Bbar)
        conv   = .false.
        count  =  0
        do while (.not.conv .and. count.le.25)
          expl = exp(-s23*beta*lam)
          dlam = (xin - Rn - Rb*expl - Gbar*lam)/(Gbar + Bbar*expl)
          lam  = lam + dlam
          if(abs(dlam) .lt. tolc*abs(lam) .or.
     &       abs(lam)  .lt. tolc*R0/twoG)          then
            conv = .true.
          endif
          count = count + 1
        end do

!       Warning: Not converged
        if(.not.conv) then
          write( *,*) '  *WARNING* No convergence in MISES'
          write(16,*) '  *WARNING* No convergence in MISES'
          write(16,*) '   lam ',lam
          write(16,*) '  dlam ',dlam
          write(16,*) ' count ',count
          call plstop(.true.)
        endif

!       Compute normal vector
        do i = 1,ntm
          en(i) = xi(i)/xin
        end do

        bb  = twoG*lam/xin
        aa  = twoG*(1.d0 - bb)
        bb  = twoG*(bb - twoG/(Gbar + Bbar*expl))

!       Compute plastic tangent from normal
        do i = 1,ntm
          cc = bb*en(i)
          do j = 1,ntm
            dd(i,j) = cc*en(j)
          end do
        end do

!     Set for elastic increment
      else

        epp(2) = 0.0d0
        do i = 1,ntm
          en(i) = 0.0d0
        end do

        lam = 0.0d0
        aa  = twoG

        do i = 1,ntm
          do j = 1,ntm
            dd(i,j) = 0.0d0
          end do
        end do

      endif

!     Compute deviator stress, plastic strain, and accumulated plastic strain
      do i = 1,ntm
        sig(i)  = sig(i)  - twoG*lam*en(i)
        epsp(i) = epsp(i) + lam*en(i)
      end do
      epp(1) = epp(1) + s23*lam

!     Add pressure
      press = K*theta
      do i = 1,3
        sig(i) = sig(i) + press
      end do

!     Compute tangent moduli
      cc = K - aa*One3
      bb = K - twoG*One3

      do i = 1,3
        do j = 1,3
          dd(i,j) = dd(i,j) + cc
          dr(i,j) =           bb
        end do
        dd(i  ,i  ) = dd(i  ,i  ) + aa
        dr(i  ,i  ) = dr(i  ,i  ) + twoG
      end do
      do i = 4,ntm
        dd(i,i) = dd(i,i) + 0.5d0*aa
        dr(i,i) = G
      end do

      end subroutine mises

      subroutine modl1d(d,ta,eps,hn,h1,nh,ii,istrt,dd,sig,isw)

!-----[--.----+----.----+----.-----------------------------------------]
!     Purpose: Small Deformation 1-d Constitutive Equation Driver

!     Input parameters
!          d(*)    -  up to ndd-nud-1 material parameters
!          eps(2)  -  current strain at point, and strain rate
!          hn(nh)  -  history terms at t_n
!          h1(nh)  -  history terms at t_n+1
!          nh      -  number of history terms
!          ii      -  Number of calls to routine from each element
!          istrt   -  Start for inelastic iteration: 0 = elastic
!          isw     -  element control parameter

!     Ouput parameters
!          dd(2)   -  current material tangent modulus
!          sig(2)  -  stress at point.

!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'cdat1.h'
      include  'elcount.h'
      include  'pmod2d.h'

      integer       :: i,ii,istrt,nh, umat,uprm, isw
      real (kind=8) :: ta

      real (kind=8) :: d(*),eps(*),hn(*),h1(*),dd(*),sig(*)

      save

!     Extract analysis type: 1=plane stress; 2=plane strain; 3=axi
      uprm  = ndd - nud
      umat  = int(d(uprm)) - 100

!     Set initial stress and zero tangent modulus
      dd(1)  = 0.0d0
      if(nint(d(160)).eq.1) then
        sig(1) = d(161)
      elseif(nint(d(160)).eq.2) then
      else
        sig(1) = 0.0d0
      endif

!     Program material models
      if(umat.lt.0) then

        plasfl = int(d(40)).eq.1
        viscfl = int(d(40)).eq.2

!       Move hn to h1
        do i = 1,nh
          h1(i) = hn(i)
        end do

!       P l a s t i c i t y
        if(plasfl) then

          call plas1d(d,ta,eps(1),h1,istrt, sig(1),dd)
          if(h1(3).eq.0.0d0) then
            nomats(1,2) = nomats(1,2) + 1
          else
            nomats(2,2) = nomats(2,2) + 1
          endif

!       V i s c o e l a s t i c i t y
        elseif(viscfl) then

          call visc1d(d,eps(1),h1(1),h1(2), sig(1),dd)
          nomats(1,4) = nomats(1,4) + 1

!       E l a s t i c i t y
        else

          call elas1d(d,ta,eps(1), sig(1),dd)
          nomats(1,1) = nomats(1,1) + 1

        end if

!     U s e r    M o d e l    I n t e r f a c e
      else

        call umod1d(umat,eps(1),ta,d(1),d(uprm+1),hn(1),h1(1),nh,ii,
     &              istrt,sig(1),dd(1), isw)

      end if

!     Rayleigh stress
      sig(2) = dd(2)*eps(2)

      end subroutine modl1d

      subroutine modlsd(ii,d,ta,eps,h1,h2,nh,istrt, dd,sig,isw)

!-----[--.----+----.----+----.-----------------------------------------]
!     Purpose: Small Deformation Constitutive Equation Driver

!     Input parameters
!          ii        -  Point being processed
!          d(*)      -  up to ndd-nud-1 material parameters
!          eps(9,3)  -  current strains and fluxes at point
!          h(nh)     -  history terms at point
!          nh        -  number of history terms
!          istrt     -  Start state: 0 = elastic; 1 = last iterate
!          im        -  material type
!     Ouput parameters
!          dd(6,6,5) -  current material tangent moduli
!                       Coupled problems:    | dd_1   dd_2 |
!                                            | dd_3   dd_4 |
!                       Rayleigh damping: dd_5
!          sig(6)    -  stresses at point.
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'cdat1.h'
      include  'elcount.h'
      include  'pmod2d.h'
      include  'sdata.h'

      integer       :: i,nh,ii,istrt,isw, umat,uprm
      real (kind=8) :: ta

      real (kind=8) :: d(*),eps(9,*),h1(*),h2(*)
      real (kind=8) :: dd(6,6,5),sig(*),theta(3)

      save

!     Extract analysis type: 1=plane stress; 2=plane strain; 3=axi
      stype = nint(d(16))
      uprm  = ndd-nud
      umat  = int(d(uprm)) - 100

!     Zero dd arrays
      dd(:,:,:) = 0.0d0

!     Set constant initial stresses
      if(nint(d(160)).eq.1) then
        sig(1:6) = d(161:166)
      else
        sig(1:6)  = 0.0d0
      end if

!     Program material models
      if(umat.lt.0) then

!       Set model type
        plasfl = nint(d(40)).eq.1
        viscfl = nint(d(40)).eq.2

!       P l a s t i c i t y
        if(plasfl) then

          if(nint(d(40)).eq.1) then

!           Move h1 to h2
            h2(1:nh) = h1(1:nh)

!           Plane stress plasticity
            if (stype.eq.1 .or.stype.eq.4) then

              call epps2d(d,eps,h2,h2(4),h2(7),istrt, sig,dd,dd(1,1,5))
              if(h2(8).eq.0.0d0) then
                nomats(1,2) = nomats(1,2) + 1
              else
                nomats(2,2) = nomats(2,2) + 1
              endif

!           Plane strain, axisymmetric or 3D plasticity
            else

              call mises(d,eps,h2(3),h2,istrt, sig,dd,dd(1,1,5))
              if(h2(2).eq.0.0d0) then
                nomats(1,2) = nomats(1,2) + 1
              else
                nomats(2,2) = nomats(2,2) + 1
              endif

            end if

          endif

!       Representative volume element model (two-scale solutions)
        elseif(nint(d(40)).eq.4) then

          call srvemat(d,eps,ta,h1,h2,nh, sig,dd, isw)

!       E l a s t i c i t y
        else

          call estrsd(d,ta,eps,sig,dd,dd(1,1,5))
          nomats(1,1) = nomats(1,1) + 1

        end if

!       V i s c o e l a s t i c i t y
        if(viscfl) then

!         Move h1 to h2
          h2(1:nh) = h1(1:nh)

          if(ndm.le.2) then
            i = 4
          else
            i = 6
          endif
          call viscoe(d,eps,h2(1),h2(i+1),i,sig,dd,dd(1,1,5))
          nomats(1,4) = nomats(1,4) + 1

        end if

!     U s e r    M o d e l    I n t e r f a c e
      else

!       Compute trace to pass to user module

        theta(1) = eps(1,1) + eps(2,1) + eps(3,1)
        theta(2) = eps(1,2) + eps(2,2) + eps(3,2)
        theta(3) = eps(1,3) + eps(2,3) + eps(3,3)

        call umodel(umat,eps,theta,ta,d(1),d(uprm+1),h1(1),h2(1),nh,
     &              ii,istrt,sig,dd,isw)

      end if

      end subroutine modlsd

      subroutine plas1d(d,ta, eps,h1,istrt, sig,dd)

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: 1-d plasticity constitutive equation

!      Inputs:

!      Outputs:
!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'rdata.h'

      logical       :: state
      integer       :: istrt,nseg
      real (kind=8) :: ta,eps, sig,dd(2)
      real (kind=8) :: d(*),h1(*)

      save

!     Check state for iterations (N.B. 'rnmax' zero in first iteration)
      if(rnmax.eq.0.0d0) then
        if(istrt.eq.0) then         ! First iteration in step
          state = .false.
        else
          state = .true.
        endif
      else                          ! Not first iteration in step
        state = .true.
      endif

      nseg = nint(d(130))
      if(nseg.eq.0) then
        call pllh1d(d,ta, eps,h1, sig,dd, state)
      else
        call plsh1d(d,d(131),nseg, ta, eps,h1, sig,dd, state)
      endif

      end subroutine plas1d

      subroutine pllh1d(d,ta, eps,h1, sig,dd, state)

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:  1-d linear kinematic, saturation isotropic hardening

!      Inputs:

!      Outputs:
!-----[--.----+----.----+----.-----------------------------------------]

      implicit none

      include  'iofile.h'

      logical       :: noconv, state
      integer       :: count
      real (kind=8) :: ta,eps, sig,dd(2), epp, epn, alp
      real (kind=8) :: epstr,sigtr,cc, dyld,Rs,rf,nn
      real (kind=8) :: ff,yld, etol,det,dsig,dlam,lambda
      real (kind=8) :: d(*),h1(*)

      save

      data     etol /1.d-08/

!     Extract effective plastic strain
      epn   = h1(1)
      epp   = h1(2)

!     Compute trial stress
      sigtr = sig + d(1)*(eps - epn - d(3)*ta)

!     Check yield
      alp   = d(45)*epn
      ff    = abs(sigtr-alp)
      yld   = d(42) + (d(41) - d(42))*exp(-d(43)*epp) + d(44)*epp

!     Plastic state
      if(ff.gt.yld .and. state) then

        lambda =  0.0d0
        sig    =  sigtr

        cc     =  1.d0/d(1)
        epstr  =  cc*sigtr

!       Newton solution for state
        noconv = .true.
        count  =  0

        do while(noconv)

!         Compute Newton parameters
          count =  count + 1
          dyld  = (d(42) - d(41))*exp(-d(43)*epp)
          yld   =  d(42) - dyld + d(44)*epp
          dyld  =  d(43)*dyld + d(44) + d(45)
          ff    = abs(sig - alp)
          nn    = (sig - alp)/ff
          Rs    =  epstr - cc*sig - lambda*nn
          rf    =  yld - ff
          det   =  1.d0/(dyld*cc + nn*nn)

!         Compute increments
          dsig  =  det*(dyld*Rs + nn*rf)
          dlam  =  det*(  nn*Rs - cc*rf)

!         Update variables
          lambda = lambda + dlam
          sig    = sig    + dsig

          epn    = h1(1)  + lambda*nn
          epp    = h1(2)  + lambda
          alp    = d(45)*epn

!         Check convergence
          if(abs(dlam).lt.etol*abs(lambda)) then
            if(abs(dsig).lt.etol*abs(sig))  then
              noconv = .false.
            endif
          elseif(count.gt.25) then
            if(ior.lt.0) then
              write(*,*) ' *WARNING* No convergence in PLAS1D'
            endif
            write(iow,*) ' *WARNING* No convergence in PLAS1D'
            noconv = .false.
          endif
        end do ! while noconv

!       Elasto-plastic modulus
        dd(1) = d(1)*(1.d0 - nn*nn*det)

!       Update history variables
        h1(1) = epn
        h1(2) = epp
        h1(3) = 1.d0

!     Elastic state
      else

!       Elastic modulus

        sig   = sigtr
        dd(1) = d(1)
        h1(3) = 0.d0

      endif

      dd(2)  = d(1)

      end subroutine pllh1d

      subroutine plsh1d(d,table,nseg, ta, eps,h1, sig,dd, state)

!     Plasticity: 1-k table lookup hardening/yield

      implicit  none

      include  'iofile.h'

      logical       :: noconv, state
      integer       :: nseg,count,i
      real (kind=8) :: ta,eps, sig,dd(2), epp, epn, alp
      real (kind=8) :: ee,cc, rsig,ralp,rf,nn,det,dsig,dalp,dlam,lambda
      real (kind=8) :: a11,a12,a13,a22,a23
      real (kind=8) :: ff,yld, iso, kin, etol
      real (kind=8) :: d(*),table(3,*),h1(*)

      save

      data      etol /1.d-08/

!     Extract effective plastic strain
      epn   = h1(1)
      epp   = h1(2)
      alp   = h1(4)

!     Compute trial stress
      sig = sig + d(1)*(eps - epn - d(3)*ta)

      i = 1
      call tyld1d(nseg,table, epp, yld, iso, kin, i)

      ff    = abs(sig - alp)

!     Plastic state
      if(ff.gt.yld .and. state) then

        lambda =  0.0d0
        ee     =  d(1)
        cc     =  1.d0/ee

!       Newton solution for state
        noconv = .true.
        count  =  0

        do while(noconv)

!         Compute Newton parameters
          count =  count + 1

!         Compute parameters
          call tyld1d(nseg,table, epp, yld, iso, kin, i)

          ff   =  abs(sig - alp)
          nn   = (sig - alp)/ff
          rsig =  eps - h1(1) - cc*sig - lambda*nn
          if(kin.gt.0.0d0) then
            ralp = -(alp - h1(4))/kin + lambda*nn
          else
            ralp = 0.0d0
          endif
          rf   = -ff + yld
          det  =  1.d0/(ee + iso + kin)
          a11  =  ee*(iso + kin)
          a12  =  ee*kin
          a13  =  ee*nn
          a22  =  kin*(ee + iso)
          a23  = -kin*nn

!         Compute increments
          dsig = (a11*rsig + a12*ralp + a13*rf)*det
          dalp = (a12*rsig + a22*ralp + a23*rf)*det
          dlam = (a13*rsig + a23*ralp -1.d0*rf)*det

!         Update variables
          lambda = lambda + dlam
          sig    = sig    + dsig
          alp    = alp    + dalp

          epn    = h1(1)  + lambda*nn
          epp    = h1(2)  + lambda

!         Check convergence
          if(  abs(dlam).lt.etol*abs(lambda)) then
            if(abs(dsig).lt.etol*abs(sig) .and.
     &         abs(dalp).lt.etol*abs(alp))  then
              noconv = .false.
            endif
          elseif(count.gt.25) then
            if(ior.lt.0) then
              write(*,*) ' *WARNING* No convergence in PLSH1D'
            endif
            write(iow,*) ' *WARNING* No convergence in PLSH1D'
            noconv = .false.
          endif
        end do ! while noconv

!       Elasto-plastic modulus
        dd(1) = a11*det

!       Update history variables
        h1(1) = epn
        h1(2) = epp
        h1(3) = 1.d0

!     Elastic state
      else

!       Elastic modulus
        dd(1) = d(1)
        h1(3) = 0.d0

      endif
      dd(2) = d(21)

      end subroutine plsh1d

      subroutine tyld1d(nseg,table, epp, yld, iso, kin, i)

!     Table Look-up for Yield and hardening moduli

      implicit  none

      logical       :: flag,case
      integer       :: nseg,i
      real (kind=8) :: epp, del, yld, iso,kin, table(3,*)

!     Check yield
      flag = .true.
      i    = 1
      do while (flag)
        if(epp.lt.table(1,i)) then
          flag = .false.
          case = .true.
        elseif(i.ge.nseg) then
          flag = .false.
          case = .false.
        endif
        i = i + 1
      end do

      if(case) then
        del =  1.d0/(table(1,i+1) - table(1,i))
        iso = (table(2,i+1) - table(2,i))*del
        yld = (table(2,i)*table(1,i+1) - table(2,i+1)*table(1,i))*del
     &      +  iso*epp
        kin = (table(3,i)*table(1,i+1) - table(3,i+1)*table(1,i))*del
     &      + (table(3,i+1) - table(3,i))*del *epp
      else
        yld = table(2,nseg)
        kin = table(3,nseg)
        iso = 0.0d0
      endif

      end subroutine tyld1d

      subroutine visc1d(d,eps,epsn,q, sig,ee)

!-----[--+---------+---------+---------+---------+---------+---------+-]
!     Purpose: One-dimensional viscoelastic model

!     Inputs:
!       d(*)    - Material parameters
!       epsn    - Strain value at t_n
!       eps     - Strain value at t_n+1
!       q(*)    - Viscoelastic stress components at t_n

!     Outputs
!       sig     - Stress at t_n+1
!       q(*)    - Viscoelastic stress components at t_n+1
!       ee(*)   - Tangent moduli
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit  none

      include  'tdata.h'

      integer       :: nv, n
      real (kind=8) :: efac,mu,mu_n,exp_n,dtau,dq, sig,eps,epsn
      real (kind=8) :: d(*),q(*), ee(*)

      real (kind=8) :: hvisc

!     Set initial values
      ee(1) = d(1)
      ee(2) = d(1)
      sig   = 0.0d0
      efac  = 0.0d0
      mu    = 0.0d0

!     Accumulate viscoelastic terms
      nv    = nint(d(57))
      do n = 1,nv

        mu_n  = d(2*n+49)

!       Exact integral
        dtau  = dt/d(2*n+50)
        exp_n = exp( -dtau)
        dq    = mu_n*hvisc(dtau,exp_n)
        q(n)  = exp_n*q(n) + dq*(eps - epsn)

!       Mid-point integral
!       dtau  = dt/d(2*n+50)*0.5d0
!       exp_n = exp( -dtau)
!       dq    = mu_n*exp_n
!       q(n)  = exp_n*exp_n*q(n) + dq*(eps - epsn)

!       Accumulate terms
        sig   = sig  + q(n)
        mu    = mu   + mu_n
        efac  = efac + dq

      end do

!     Set final values and add elastic component
      mu    = 1.d0 - mu
      sig   = ee(1)*(mu*eps + sig)
      ee(1) = ee(1)*(mu + efac)
      epsn  = eps

      end subroutine visc1d

      subroutine viscoe(d,eps,en,qi,ntm,sig,dd,dr)

!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'sdata.h'
      include  'tdata.h'

      integer       :: i,j, n,nv,ntm
      real (kind=8) :: G,Gg,K,Kg,Kth
      real (kind=8) :: gfac,exp_n,mu_0,mu_n,dq_n,dtau, theta

      real (kind=8) :: d(*),eps(*),en(*),qi(ntm,*)
      real (kind=8) :: sig(*),dd(6,6),dr(6,6)
      real (kind=8) :: ee(6)

      real (kind=8) :: hvisc

      save

!     Set elastic parameters for G (mu) and lambda
      G = d(1)/(2.d0*(1.d0 + d(2)))
      K = d(1)/(3.d0*(1.d0 - 2.d0*d(2)))

!     Compute volumetric strain and deviatoric components
      theta = (eps(1) + eps(2) + eps(3))*0.3333333333333333d0
      do i = 1,3
        ee(i  ) = eps(i) - theta
      end do ! i
      do i = 4,ntm
        ee(i) = eps(i)*0.5d0
      end do ! i

!     Set properties for integrating the q_i terms
      do i = 1,ntm
        sig(i) = 0.0d0
      end do ! i
      mu_0 = 0.0d0
      gfac = 0.0d0

      nv   = nint(d(57))
      do n = 1,nv
        mu_n  = d(2*n+49)
        dtau  = dt/d(2*n+50)
        exp_n = exp(-dtau)

        dq_n = mu_n * hvisc(dtau,exp_n)
        gfac = gfac + dq_n
        mu_0 = mu_0 + mu_n

!       Update history and compute viscoelastic deviatoric stress
        do i = 1,ntm
          qi(i,n) = exp_n*qi(i,n) + dq_n*(ee(i) - en(i))
          sig(i)  = sig(i) + qi(i,n)
        end do ! i
      end do ! n

!     Finish updates and save the strains
      mu_0 = 1.d0 - mu_0
      gfac = gfac + mu_0
      do i = 1,ntm
        sig(i) = 2.d0*G*(mu_0*ee(i) + sig(i))
        en(i)  = ee(i)
      end do ! i

!     Add elastic bulk term
      Kth = K*theta*3.0d0
      do i = 1,3
        sig(i) = sig(i) + Kth
      end do ! i

!     Set tangent parameters
      Gg = G*gfac
      Kg = K - 0.6666666666666666d0*Gg
      K  = K - 0.6666666666666666d0*G
      do j =1,3
        do i = 1,3
          dd(i,j) = Kg
          dr(i,j) = K
        end do ! i
        dd(j,j) = dd(j,j) + 2.d0*Gg
        dr(j,j) = dr(j,j) + 2.d0*G
      end do ! i

      do i = 4,ntm
        dd(i,i) = Gg
        dr(i,i) = G
      end do ! i

      end subroutine viscoe

      real*8 function yldgpl(ep,salm,saltrm,g,yield,beta,delta,
     &                       hiso,hkin,tt,lambda)

!-----[--.----+----.----+----.-----------------------------------------]
!     Inputs:

!          ep     - Value of plastic strain at time t(n)
!          salm   - Norm of deviator less back stress at time t(n)
!          saltrm - Norm of trial deviator less back stress
!          g      - Shear modulus
!          yield  - Initial yield
!          beta   - Increase to limit yield * sqrt(2/3)
!          delta  - Growth rate * 2/3
!          hiso   - Isotropic hardening
!          hkin   - Kinematic hardening
!          tt     - Sqrt(2/3)

!     Outputs:

!          yldgpl - yield function value for current iterate
!          lambda - consistency parameter (from constitutive model)
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'iofile.h'

      real (kind=8) :: salm,saltrm,ep,g
      real (kind=8) :: yield,beta,delta,hiso,hkin,tt, lambda
      real (kind=8) :: a1,a2,a3,aa,bb,discr,g2, one3,two3

      save

      data      one3 / 0.3333333333333333d0 /
      data      two3 / 0.6666666666666667d0 /

!     Set parameters
      g2  = g + (hiso + hkin)*one3

!     Solution of quadratic equation
      a1 = saltrm - tt * (yield + hiso*ep)
      a2 = saltrm - salm
      a3 = 2.0d0 * g - delta

      aa =   2.0d0 * g2 * a3
      bb = - a1 * a3 - 2.0d0 * a2 * g2
     &     - (delta + two3*(hkin + hiso))*beta

      discr = bb * bb - 4.0d0 * aa * a1 * a2

      if (discr.lt.0.0d0) then
        write(iow,*) ' DISCR ERROR ', discr
      end if

      lambda = -0.5d0 * (bb + sqrt(abs(discr)))/aa
      yldgpl =     tt * (yield + hiso*(ep + tt*lambda))

      end function yldgpl

      function ylds2d(d,sigtr,alp,eta,dd,beta,epn,lam0,hprim)

!-----[--.----+----.----+----.-----------------------------------------]
!     Plane stress plasticity routine for return map algorithm

      implicit  none

      include  'iofile.h'
      include  'rdata.h'

      real (kind=8) :: d(*),sigtr(3),alp(3),eta(3),psi(3),dd(6,6)

      real (kind=8) :: toli,stwo,two3,four3,psi1,psi2,twoh3,e2,e3
      real (kind=8) :: f1,f2,ff1,ff2,phit,ep,rad2,f3,phi,dphi,dlam,lam
      real (kind=8) :: d1,d2,gam1,gam2,beta,lam0,epn,hprim,xkprim

      integer       :: i,icnt

      real (kind=8) :: ylds2d,hard2d

      save

      data      toli  /1.d-8/
      data      stwo  /0.7071067811865475d0/
      data      two3  /0.6666666666666667d0/
      data      four3 /1.3333333333333333d0/

      psi(1) = stwo*( eta(1) + eta(2))
      psi(2) = stwo*(-eta(1) + eta(2))
      psi(3) = eta(3)

!     Compute scale factor
      psi1 = psi(1)*psi(1)*0.3333333333333333d0
      psi2 = psi(2)*psi(2) + psi(3)*psi(3)*2.d0
      twoh3= two3*hprim
      d1   = d(1)/(1.d0 - d(2))*0.3333333333333333d0
      d2   = d(1)/(1.d0 + d(2))
      e3   = d1 + twoh3
      e2   = d2 + twoh3

!     Newton's method for determining correct lambda
      lam  = lam0
      icnt = 0
100   f1   = 1.d0/(1.d0 + e3*lam)
      f2   = 1.d0/(1.d0 + e2*lam)
      ff1  = f1*f1*psi1
      ff2  = f2*f2*psi2
      phit = 0.50d0*(ff1  + ff2)
      ep   = epn + sqrt(four3*phit)*lam
      rad2 = hard2d(d,ep,xkprim,hprim)
      f3   = (1.d0 - lam*two3*xkprim)
      phi  = phit - rad2
      dphi = f3*(e3*ff1*f1 + e2*ff2*f2) + four3*xkprim*phit
      dlam = phi/dphi
      if(dlam.gt.0.0d0.or.abs(dlam).lt.abs(lam)) then
        lam = lam + dlam
      else
        lam = 0.50d0*abs(lam)
      endif
      icnt = icnt + 1
      if(icnt.gt.100) go to 110
      if(abs(dlam).gt.toli*abs(lam) .and. rnmax.gt.0.0d0) go to 100
      go to 120
110   write(iow,2000) dlam,lam

!     Scale psi onto yield surface using lambda
120   continue

!     Compute plasticity map dd array
      f1      = 1.d0/(1.d0 + e3*lam)
      f2      = 1.d0/(1.d0 + e2*lam)
      gam1    = 1.d0 + twoh3*lam
      gam2    = 1.d0 - two3*xkprim*lam
      d1      = 1.5d0*d1*f1*gam1
      d2      = 0.5d0*d2*f2*gam1

      dd(1,1) = d1 + d2
      dd(1,2) = d1 - d2
      dd(2,1) = d1 - d2
      dd(2,2) = d1 + d2
      dd(4,4) = d2

!     Compute stresses on yield surface
      psi(1)  = psi(1)*f1
      psi(2)  = psi(2)*f2
      psi(3)  = psi(3)*f2
      eta(1)  = stwo*(psi(1) - psi(2))
      eta(2)  = stwo*(psi(1) + psi(2))
      eta(3)  = psi(3)
      ylds2d  = (eta(1)**2 + eta(2)**2 - eta(1)*eta(2))/3.
     &        +  eta(3)**2
      rad2    = hard2d(d,ep,xkprim,hprim)
      do i = 1,3
        alp(i)  = alp(i) + lam*twoh3*eta(i)
        sigtr(i)= eta(i) + alp(i)
      end do
      beta   = four3*ylds2d*(gam1*xkprim + gam2*hprim)*gam1/gam2
      ylds2d = sqrt(ylds2d*3.d0)/d(41)
      lam0   = lam
      epn    = ep

2000  format(' * * Warning * * Failure to converge in ylds2d'/
     &       '     dlam =',1p,1e12.5,' lam =',1p,1e12.5)

      end function ylds2d

      subroutine dmatdx(dd,sigb,p_bar,p_mix)

!-----[--+---------+---------+---------+---------+---------+---------+-]
!     Purpose: Compute finite deformation mixed D arrays

!     Inputs:
!         dd(7,7)       Modified constitutive array
!         sigb(6)       Constitutive stresses
!         p_bar         Constitutive pressure
!         p_mix         Mixed pressure

!     Outputs:
!         dd(7,7)       Material tangent terms
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit  none

      integer       :: i,j
      real (kind=8) :: p_mix, p_bar, fac1, one3, two3
      real (kind=8) :: dd(7,7), sigb(6), sigb_d(6)

      data      one3 / 0.3333333333333333d0 /
      data      two3 / 0.6666666666666667d0 /

!     Compute deviator stress
      do i = 1,3
        sigb_d(i  ) = two3 *(sigb(i  ) - p_bar)
        sigb_d(i+3) = two3 * sigb(i+3)
      end do ! i

!     D_11: B_matrix part
      fac1 = p_mix - two3*p_bar
      do j = 1,3
        do i = 1,3
          dd(i,j) = dd(i,j) + fac1
        end do ! i
      end do ! j

      do j = 1,6
        do i = 1,3
          dd(i,j) = dd(i,j) - sigb_d(j)
          dd(j,i) = dd(j,i) - sigb_d(j)
        end do ! i
      end do ! j

      fac1 = p_bar - p_mix
      do j = 1,3
        dd(j  ,j  ) = dd(j  ,j  ) + fac1*2.d0
        dd(j+3,j+3) = dd(j+3,j+3) + fac1
      end do ! j

!     D_12: Coupling matrix with volume
      do j = 1,6
        dd(7,j) = dd(7,j) + sigb_d(j)
        dd(j,7) = dd(j,7) + sigb_d(j)
      end do ! j

!     D_22: Volumetric part
      dd(7,7) = dd(7,7) - one3*p_bar

      end subroutine dmatdx

      subroutine dmatmx ( aa, dd )

!-----[--.----+----.----+----.-----------------------------------------]
!     Purpose: Project 6x6 AA-matrix onto a 7x7 DD-matrix for mixed model
!              (N.B. Jacobian ratio is in dvol)

!                     | D_11   D_12 |
!                DD = |             |
!                     | D_21   D_22 |

!              where:

!                D_11  = 6 x 6 Deviatoric part of matrix
!                D_12  = 6 x 1 Coupling   part of matrix
!                D_21  = 1 x 6 Coupling   part of matrix
!                D_22  = 1 x 1 Volumetric part of matrix

!     Inputs:
!                                                      _
!        aa(6,6)   - Material tangent matrix (based on F)

!     Outputs:
!        dd(7,7)   - Mixed material tangent for material stiffness
!                    computations.
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer       :: i , j
      real (kind=8) :: third
      real (kind=8) :: aa(6,6), dd(7,7)

      save

      data      third/ 0.3333333333333333d0 /

!     Load moduli from constitution
      do i = 1,6
        do j = 1,6
          dd(i,j) = aa(i,j)
        end do
      end do

!     Compute left and right multiples with trace
      do i = 1,6
        dd(i,7) = (aa(i,1) + aa(i,2) + aa(i,3))*third
        dd(7,i) = (aa(1,i) + aa(2,i) + aa(3,i))*third
      end do

!     Convert upper 6 x 6 to a deviatoric D_11
      do i = 1,6
        do j = 1,3
          dd(i,j) = dd(i,j) - dd(i,7)
          dd(j,i) = dd(j,i) - dd(7,i)
        end do
      end do

!     Form last term, D_22
      dd(7,7) = (dd(1,7) + dd(2,7) + dd(3,7))*third

!     Final update to form D_12 and D_21
      do i = 1,3
        dd(i,7) = dd(i,7) - dd(7,7)
        dd(7,i) = dd(7,i) - dd(7,7)
        do j = 1,3
          dd(i,j) = dd(i,j) + dd(7,7)
        end do
      end do

      end subroutine dmatmx

      subroutine fengy3(d,detf,u,up,upp, ha,hp,hpp, isw)

!-----[--.----+----.----+----.-----------------------------------------]
!     Purpose: Compute pressure-entropy function

!     Inputs:
!       d(*)        - Material parameter array
!       detf        - Deformation gradient
!       isw         - Function type: 1 - K*[ 0.5*( J^2 - 1 ) - ln(J) ]
!                                    2 - K*[ 0.5*( J - 1 )^2 ]
!                                    3 - K*[ 0.5*( ln(J) )^2 ]

!     Outputs:
!       u           - Internal energy
!       up          - First derivative of internal energy
!       upp         - Second derivative of internal energy
!       ha          - Augmentation function
!       hp          - First derivative of augmentation function
!       hpp         - Second derivative of augmentation function
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'augdat.h'

      integer       :: isw
      real (kind=8) :: d1, detf, u, up, upp, ha, hp, hpp

      real (kind=8) :: d(*)

      save

!     Perform (augmented) iteration on penalty
      d1    = augf*d(21)

!     Free energy function
!     Psi   = U(J) -3*alpha*J*U'(J)*[T - T_ref]

!     up    = partial_J ( Psi )
!     upp   = [ partial_J ( J partial_J Psi ) - up ]/J
!           =   partial^2_J ( Psi )/J

!     Current volumetric functions are:

!     Model 1.) U(J) = K*0.5*(J^2-1) - K*(log J)
      if    (isw.eq.1) then

        u   = 0.5d0*d1*(0.5d0*detf**2 - 0.5d0 - log(abs(detf)))
        up  = 0.5d0*d1*( detf - 1.d0/detf    )
        upp = 0.5d0*d1*( 1.d0 + 1.d0/detf**2 )

!     Model 2.) U(J) = K*0.5*(J-1)^2
      elseif(isw.eq.2) then

        u   = d1*(detf - 1.d0)**2*0.5d0
        up  = d1*(detf - 1.d0)
        upp = d1

!     Model 3.) U(J) = K*0.5*(log J)^2
      elseif(isw.eq.3) then

!       up  = ( d1*log( detf ) - 3*K*alpha*(T - Tref) )/detf
        u   = d1*log(abs(detf))**2*0.5d0
        up  = d1*log(abs(detf)) / detf
        upp = ( d1/detf  - up )/detf

      endif

!     Augmented Lagrangian function and derivatives
!     Current augmented Lagrangian function is

!     Model 1.) h(J) = (J - 1)
      ha  = detf - 1.d0
      hp  = 1.0d0
      hpp = 0.0d0

      end subroutine fengy3

      subroutine modlfd(ii,d,f,df,detf,ta,hn,hn1,nh,istrt, dd,sig,bb,
     &                  xlamd, ha, bbar, isw)

!-----[--.----+----.----+----.-----------------------------------------]
!     Purpose: Driver for finite deformation constitutive models

!     Inputs:
!       ii           - Pointe being processed
!       d(*)         - Material parameters array
!       f(3,3)       - Deformation gradient
!       df(3,3)      - Incremental deformation gradient
!       detf         - Determinant of deformation gradient
!       ta           - Temperature at point
!       hn(*)        - History parameters at t_n
!       istrt        - Start state: 0 = elastic
!       nh           - Number history parameters/stress point
!       bbar         - Flag (true for B-bar, false for others)
!       isw          - Solution option from elements

!     Outputs
!       hn1(*)       - History parameters at t_n+1
!       dd(*,*,5)    - Expanded material moduli for mixed computation
!                      N.B. Computed from spatial tangent, aa(6,6,5);
!                      Otherwise copy of aa.
!       sig(10)      - Cauchy stress values at t_n+1
!       bb(6)        - Left Cauchy-Green deformation tensor
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'cdat1.h'
      include  'debugs.h'
      include  'elcount.h'
      include  'elengy.h'
      include  'elpers.h'
      include  'sdata.h'

      logical       :: bbar, plasfl
      integer       :: nh,istrt,isw, i,j,ii, imat, ntm, uprm,umat
      real (kind=8) :: ta, xlamd, ha

      real (kind=8) :: d(*),  bb(6),  detf(*),f(3,3),df(3,3)
      real (kind=8) :: hn(*), hn1(*), dd(*), sig(*), aa(6,6,5)
      real (kind=8) :: g(3,3)

      save

!     Set finite deformation flag for Hill-Mandel
      finflg = .true.

!     Set number of active stress components
      if(ndm.eq.1) then
        ntm = 1
      elseif(ndm.eq.2) then
        ntm = 4
      elseif(ndm.eq.3) then
        ntm = 6
      endif

!     Set material model type and user material pointers
      uprm  = ndd-nud
      umat  = int(d(uprm)) - 100

!     Zero stress and moduli
      do i = 1,6
        sig(i) = 0.0d0
        do j = 1,6
          aa(j,i,1) = 0.0d0
          aa(j,i,2) = 0.0d0
          aa(j,i,3) = 0.0d0
          aa(j,i,4) = 0.0d0
          aa(j,i,5) = 0.0d0
        end do ! j
      end do ! i

!     Set constant initial stress state
      if(nint(d(160)).eq.1) then
        do i = 1,6
          sig(i) = d(160+i)
        end do ! i
      endif

!     Compute Left Cauchy-Green deformation tensor
      bb(1) = f(1,1)*f(1,1) + f(1,2)*f(1,2) + f(1,3)*f(1,3)
      bb(2) = f(2,1)*f(2,1) + f(2,2)*f(2,2) + f(2,3)*f(2,3)
      bb(3) = f(3,1)*f(3,1) + f(3,2)*f(3,2) + f(3,3)*f(3,3)
      bb(4) = f(1,1)*f(2,1) + f(1,2)*f(2,2) + f(1,3)*f(2,3)
      bb(5) = f(2,1)*f(3,1) + f(2,2)*f(3,2) + f(2,3)*f(3,3)
      bb(6) = f(1,1)*f(3,1) + f(1,2)*f(3,2) + f(1,3)*f(3,3)

!     Program material models
      if(umat.lt.0) then

!       Set model type
        plasfl = nint(d(40)).eq.1

        imat = nint(d(20))

!       Compute elastic stress and tangents
        if    (imat.eq.1) then
          call stnh3f(d,detf(1),bb, sig,aa,xlamd,ha,estore)
        elseif(imat.eq.2) then
          call stvk(d,detf(1),f,df,sig,aa, estore)
        elseif(imat.eq.13) then ! RVE model from multi-scale analysis
          g(:,:) = f(:,:)
          do i = 1,3
            g(i,i) = g(i,i) - 1.0d0
          end do ! i
          call rvemat(d,g,detf(1),ta,hn,hn1,nh, sig,aa, isw)
        endif

        if(.not.plasfl) then
          nomats(1,1) = nomats(1,1) + 1
        endif

!     U s e r    M o d e l    I n t e r f a c e
      else
        call umodel(umat,f,detf,ta,d(1),d(uprm+1),hn(1),hn1(1),nh,
     &              ii,istrt,sig,aa, isw)
      endif

      if(debug) then
        call mprint(sig,1,4,1,'SIG')
        call mprint(aa(1,1,1),4,4,6,'AA')
      endif

!     Project aa to D-matrix for B-bar
      if(bbar) then
        call dmatmx ( aa, dd )
      else
        call pmove  ( aa, dd, 36 )
      end if

      end subroutine modlfd

      subroutine pushr2(f,s,sig,detf)

!-----[--.----+----.----+----.-----------------------------------------]
!     Purpose: Push forward 2nd rank tensor

!       sigma(i,j) = f(i,k)*s(k,l)*f(j,l)/detf

!     Inputs:
!         s(6)   - material stress (2nd Piola-Kirchhoff)
!         f(3,3) - deformation gradient
!         detf   - determinant of deformation gradient
!     Outputs:
!         sig(6) - spatial stress (Cauchy)

!                       | sig(1)  sig(4)  sig(6) |
!         sigma(i,j) =  | sig(4)  sig(2)  sig(5) |
!                       | sig(6)  sig(5)  sig(3) |
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer       :: i
      real (kind=8) :: detf, jrec
      real (kind=8) :: f(3,3), s(6), sig(6), fs(3,3)

      save

!     Reciprocal deformation gradient determinant
      jrec = 1.d0/detf

!     fs = f^t*s
      do i = 1,3
        fs(i,1) = f(i,1)*s(1) + f(i,2)*s(4) + f(i,3)*s(6)
        fs(i,2) = f(i,1)*s(4) + f(i,2)*s(2) + f(i,3)*s(5)
        fs(i,3) = f(i,1)*s(6) + f(i,2)*s(5) + f(i,3)*s(3)
      end do

!     sig = fs*f/j

      sig(1) = (fs(1,1)*f(1,1) + fs(1,2)*f(1,2) + fs(1,3)*f(1,3))*jrec
      sig(2) = (fs(2,1)*f(2,1) + fs(2,2)*f(2,2) + fs(2,3)*f(2,3))*jrec
      sig(3) = (fs(3,1)*f(3,1) + fs(3,2)*f(3,2) + fs(3,3)*f(3,3))*jrec
      sig(4) = (fs(1,1)*f(2,1) + fs(1,2)*f(2,2) + fs(1,3)*f(2,3))*jrec
      sig(5) = (fs(2,1)*f(3,1) + fs(2,2)*f(3,2) + fs(2,3)*f(3,3))*jrec
      sig(6) = (fs(3,1)*f(1,1) + fs(3,2)*f(1,2) + fs(3,3)*f(1,3))*jrec

      end subroutine pushr2

      subroutine pushr4(tl,tr,dm,ds,detf)

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:  Push forward 4th rank tensor

!         ds(i,j) = tl(k,i)*dm(k,l)*tr(l,j)/detf

!     Inputs:
!         dm(6,6) - material moduli
!         tl(6,6) - left  transformation array
!         tr(6,6) - right transformation array
!         detf    - determinant of deformation gradient
!     Outputs:
!         ds(6,6) - spatial moduli
!-----[--.----+----.----+----.-----------------------------------------]
      implicit none

      integer       :: i,j,k
      real (kind=8) :: detf, jrec
      real (kind=8) :: tl(6,6), tr(6,6), dm(6,6), ds(6,6), dtj(6)

      save

!     Reciprocal deformation gradient determinant
      jrec = 1.d0/detf

!     Compute matrix product: dtj = dm*tr
      do j = 1,6
        do i = 1,6
          dtj(i) = 0.0d0
          do k = 1,6
            dtj(i) = dtj(i) + dm(i,k)*tr(k,j)
          end do
        end do

!       Compute spatial tensor: ds = tl_trans*dt
        do i = 1,6
          ds(i,j) = 0.0d0
          do k = 1,6
            ds(i,j) = ds(i,j) + tl(k,i)*dtj(k)
          end do
          ds(i,j) = ds(i,j)*jrec
        end do
      end do

      end subroutine pushr4

      subroutine stnh3f(d,detf,bb, sig,aa,xlamd,ha,estore)

!-----[--.----+----.----+----.-----------------------------------------]
!     Finite Deformation Elasticity Neo-Hookean Model

!     INPUT variables

!         d(100)     Material parameters
!           d(21)     Bulk  modulus
!           d(22)     Shear modulus
!         detf       Jacobian determinant at t_n+1
!         bb(6)      Left Cauchy-Green tensor

!     OUTPUT variables

!         sig(6)     CAUCHY stress tensor
!         aa(6,6)    CAUCHY (spatial) elastic moduli
!         estore     Stored energy density
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer       :: i
      real (kind=8) :: detf, press,logj,uppj, mu, mu2, estore, xlamd,ha

      real (kind=8) :: d(*),sig(6),aa(6,6),bb(6)

      save

!     Compute pressure and its derivative
      logj  = log(abs(detf))
      press = d(21)*logj/detf + xlamd
      uppj  = d(21)/detf

!     Augment function
      ha    = detf - 1.0d0

!     Set CAUCHY stresses and elastic tangent moduli
      mu  =  d(22)/detf
      mu2 =  mu + mu
      do i = 1,3
        sig(i  )    = mu*bb(i) - mu + press
        sig(i+3)    = mu*bb(i+3)
        aa(i  ,i  ) = mu2 - 2.d0*press + uppj
        aa(i+3,i+3) = mu  - press
      end do

!     Add volumetric correction to aa
      aa(1,2) = aa(1,2) + uppj
      aa(2,1) = aa(1,2)
      aa(1,3) = aa(1,3) + uppj
      aa(3,1) = aa(1,3)
      aa(2,3) = aa(2,3) + uppj
      aa(3,2) = aa(2,3)

!     Compute stored energy
      estore = d(21)*logj*logj*0.5d0
     &       + d(22)*(0.5d0*(bb(1) + bb(2) + bb(3)) - 1.5d0 - logj)

      end subroutine stnh3f

      subroutine stvk(d, detf, fa,df, sig,ds, energy)

!     Purpose St. Venant-Kirchhoff Material: Orthotropic

!     Input:
!       d(*)    - Moduli and Poisson ratios
!       fa(9)   - Deformation gradient at time: t_n+a
!       df(9)   - Incremental Deformation gradient at time: t_n+a

!     Outputs:
!       sig(6)  - Cauchy stress
!       ds(6,6) - Spatial moduli
!       energy  - Energy density
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'debugs.h'
      include  'ddata.h'

      integer       :: i,j
      real (kind=8) :: detf, fac1,fac2, energy, dot
      real (kind=8) :: d(*), fa(9), df(9),  dm(6,6), ss(6), ee(6)
      real (kind=8) :: fn(9),f1(9), sig(6), ds(6,6), tl(6,6),tr(6,6)

      save

!     Set moduli

      do i = 1,6
        do j = 1,6
          dm(i,j) = 0.0d0
        end do
      end do
      do i = 1,3
        dm(i  ,i  ) = d(i+20)
        dm(i+3,i+3) = d(i+26)
      end do

      do i = 1,3
        j       = mod(i,3) + 1
        dm(i,j) = d(i+23)
        dm(j,i) = d(i+23)
      end do

!     Compute factors for updates
      if(energy.ge.0.0d0) then
        fac1 = 0.5d0 * theta(3)
        fac2 = 1.d0/theta(3) - 1.d0
      else
        fac1 = 0.5d0
        fac2 = 0.0d0
      endif

!     Compute deformation gradients at t_n and t_n+1
      do i = 1,9
        fn(i) = fa(i) -      df(i)
        f1(i) = fa(i) + fac2*df(i)
      end do

!     Compute Green-Lagrange strains
      fac2  = 0.5d0 - fac1

      ee(1) = fac1*(f1(1)*f1(1) + f1(2)*f1(2) + f1(3)*f1(3))
     &      + fac2*(fn(1)*fn(1) + fn(2)*fn(2) + fn(3)*fn(3)) - 0.5d0
      ee(2) = fac1*(f1(4)*f1(4) + f1(5)*f1(5) + f1(6)*f1(6))
     &      + fac2*(fn(4)*fn(4) + fn(5)*fn(5) + fn(6)*fn(6)) - 0.5d0
      ee(3) = fac1*(f1(7)*f1(7) + f1(8)*f1(8) + f1(9)*f1(9))
     &      + fac2*(fn(7)*fn(7) + fn(8)*fn(8) + fn(9)*fn(9)) - 0.5d0

      fac1  = fac1 + fac1
      fac2  = fac2 + fac2

      ee(4) = fac1*(f1(1)*f1(4) + f1(2)*f1(5) + f1(3)*f1(6))
     &      + fac2*(fn(1)*fn(4) + fn(2)*fn(5) + fn(3)*fn(6))
      ee(5) = fac1*(f1(4)*f1(7) + f1(5)*f1(8) + f1(6)*f1(9))
     &      + fac2*(fn(4)*fn(7) + fn(5)*fn(8) + fn(6)*fn(9))
      ee(6) = fac1*(f1(7)*f1(1) + f1(8)*f1(2) + f1(9)*f1(3))
     &      + fac2*(fn(7)*fn(1) + fn(8)*fn(2) + fn(9)*fn(3))

!     Compute 2nd P-K stress
      do i = 1,6
        ss(i) = 0.0d0
        do j = 1,6
          ss(i) = ss(i) + dm(i,j)*ee(j)
        end do
      end do

!     Push to current configuration
      if(energy.ge.0.0d0) then

        call tranr4(fa,fa,tl)
        call tranr4(f1,fa,tr)
        call pushr4(tl,tr,dm, ds,detf)
        call pushr2(fa,ss,sig,detf)

      if(debug) then
        call mprint(f1,3,3,3,'F1')
        call mprint(fn,3,3,3,'Fn')
        call mprint(tl,6,6,6,'TL')
        call mprint(tr,6,6,6,'TR')
        call mprint(ds,6,6,6,'DS')
      endif

!     Compute energy density
      else
        energy = 0.5d0*dot(ss,ee,6)
      endif

      end subroutine stvk

      subroutine tranr4(fl,fr,t)

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:   Transformation array for 4th rank tensor

!     Input:
!       fl(3,3) - left  deformation gradient
!       fr(3,3) - right deformation gradient
!     Output:
!       t(6,6) - transformation array

!       t(a,b) = fl(i,I)*fr(j,J) : a -> I,J ; b -> i,j

!         a,b  |  1    2    3    4    5    6
!        ------+-----------------------------
!        (I,J) | 1,1  2,2  3,3  1,2  2,3  3,1
!     or (i,j) |                2,1  3,2  1,3
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer       :: i,j
      integer       :: i1(6),i2(6)
      real (kind=8) :: fl(3,3),fr(3,3), t(6,6)

      save

      data      i1 /1,2,3,1,2,3/
      data      i2 /1,2,3,2,3,1/

!     Form transformation array for a 4th rank tensor in matrix form
      do i = 1,3
        do j = 1,3
          t(i,j) =  fl(i1(j),i1(i))*fr(i2(j),i2(i))
        end do
        do j = 4,6
          t(i,j) = (fl(i1(j),i1(i))*fr(i2(j),i2(i))
     &           +  fl(i2(j),i2(i))*fr(i1(j),i1(i)))*0.5d0
        end do
      end do

      do i = 4,6
        do j = 1,3
          t(i,j) =  fl(i1(j),i1(i))*fr(i2(j),i2(i))
     &           +  fl(i2(j),i2(i))*fr(i1(j),i1(i))
        end do
        do j = 4,6
          t(i,j) = (fl(i1(j),i1(i))*fr(i2(j),i2(i))
     &           +  fl(i2(j),i1(i))*fr(i1(j),i2(i))
     &           +  fl(i1(j),i2(i))*fr(i2(j),i1(i))
     &           +  fl(i2(j),i2(i))*fr(i1(j),i1(i)))*0.5d0
        end do
      end do

      end subroutine tranr4
