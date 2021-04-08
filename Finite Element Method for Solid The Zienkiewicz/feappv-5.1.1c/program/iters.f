!$Id:$
      subroutine iters(bkmax,isw)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:    Controls solution by iterative schemes. Also sets
!                  storage for compressed arrays for direct solution
!                  by sparse solvers or in blocks with disk stores.

!      Inputs:
!         bkmax  - Maximum size of block for direct solution
!         isw    - Switch: isw =  1 for TANGS
!                          isw =  2 for CMASS
!                          isw =  3 for DAMPS
!                          isw = -1 for USER module

!      Outputs:
!         none   - Outputs stored in blank common and pointers.
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'allotd.h'
      include  'cdata.h'
      include  'comblk.h'
      include  'compac.h'
      include  'compas.h'
      include  'iofile.h'
      include  'ndata.h'
      include  'pointer.h'
      include  'sdata.h'
      include  'ssolve.h'

      logical       :: setvar,palloc
      integer       :: isw,len,kp,bkmax, iptc, u1
      real (kind=4) :: tary(2), etime , tt

      save

!     Compute sparse storage for non-zero matrix
      if(isw.gt.0) then
        u1 = max(numnp*neq,neq)
        setvar = palloc(111,'TEMP1', 1, 1)
        call pzeroi(mr(np(111)),u1)
        call elcnt(numnp,numel,nen,nen1,mr(np(33)),mr(np(111)))
        call sumcnt(mr(np(111)),numnp*ndf,kp)

        setvar = palloc(112,'TEMP2', kp, 1)
        call pelcon(numel,nen,nen1,mr(np(33)),mr(np(31)),
     &              mr(np(111)),mr(np(112)),kp,1)
      endif

!     1. TANGENT Formation
      if(isw.eq.1) then

!       Slot for user supplied sparse solver
        if(ittyp.eq.-2) then
          iptc   = 1
!         ubycol =
!         udiag  =
!         uall   =
!         iptc   =
!         nspo   =

!       Program solvers with sparse assembly
        else
          kbycol = .true.     ! Store by columns
          kdiag  = .false.
          kall   = .false.
          iptc   =  neq
        endif

        setvar = palloc( 93,'OINC ', iptc , 1)
        if(np(94).ne.0) then
          setvar = palloc( 94,'OINO ',0, 1)
        endif
        setvar = palloc(113,'TEMP3',neq, 1)
        call comproa(numnp,nen,nen1,ndf,mr(np(33)),mr(np(31)),
     &              mr(np(111)),mr(np(112)),mr(np(113)),kp,
     &              kbycol,kdiag,kall)
        setvar = palloc(113,'TEMP3',  0, 1)
        setvar = palloc( 94,'OINO ', kp, 1)
        call comprob(numnp,nen,nen1,ndf,mr(np(33)),mr(np(31)),
     &              mr(np(111)),mr(np(112)),mr(np(94)),mr(np(93)),
     &              kbycol,kdiag,kall)

!       Delete temporary arrays
        setvar = palloc(112,'TEMP2', 0, 1)
        setvar = palloc(111,'TEMP1', 0, 1)

!       Set storage for sparse stiffness array
        if(ittyp.eq.-1) then

          kp  = max(kp+neq,bkmax)
          len = kp

!       User supplied sparse solver location
        elseif(ittyp.eq.-2) then

!       Profile solver assembly
        else

          kp  = kp + neq
          len = 0

        endif

        setvar = palloc(1,'TANG1', kp+len, 2)

        na     = np(1)
        nnr    = kp
        nau    = na  + neq
        nal    = nau + len
        numcels= 0
        compfl = .true.

!     2. Consistent MASS Formation
      elseif(isw.eq.2) then
        mbycol = .true.       ! Store by columns
        mdiag  = .false.
        mall   = .false.
        setvar =  palloc( 90,'OINMC', neq  , 1)
        if(np(91).ne.0) then
          setvar = palloc( 91,'OINMO',0, 1)
        endif
        setvar = palloc(113,'TEMP3',neq, 1)
        call comproa(numnp,nen,nen1,ndf,mr(np(33)),mr(np(31)),
     &               mr(np(111)),mr(np(112)),mr(np(113)),kp,
     &               mbycol,mdiag,mall)
        setvar = palloc(113,'TEMP3',  0, 1)
        setvar = palloc( 91,'OINMO', max(1,kp), 1)
        call comprob(numnp,nen,nen1,ndf,mr(np(33)),mr(np(31)),
     &               mr(np(111)),mr(np(112)),mr(np(91)),mr(np(90)),
     &               mbycol,mdiag,mall)

!       Delete temporary arrays
        setvar = palloc(112,'TEMP2', 0, 1)
        setvar = palloc(111,'TEMP1', 0, 1)

!       Allocate mass storage
        nnm = kp  + neq
        setvar = palloc(9,'CMAS1', nnm, 2)

!     3. Consistent DAMP Formation
      elseif(isw.eq.3) then
        cbycol = .true.       ! Store by columns
        cdiag  = .false.
        call   = .false.
        setvar =  palloc(203,'OINDC', neq  , 1)
        if(np(204).ne.0) then
          setvar = palloc(204,'OINDO',0, 1)
        endif
        setvar = palloc(113,'TEMP3',neq, 1)
        call comproa(numnp,nen,nen1,ndf,mr(np(33)),mr(np(31)),
     &               mr(np(111)),mr(np(112)),mr(np(113)),kp,
     &               cbycol,cdiag,call)
        setvar = palloc(113,'TEMP3',  0, 1)
        setvar = palloc(204,'OINDO', kp, 1)
        call comprob(numnp,nen,nen1,ndf,mr(np(33)),mr(np(31)),
     &               mr(np(111)),mr(np(112)),mr(np(204)),mr(np(203)),
     &               cbycol,cdiag,call)

!       Delete temporary arrays
        setvar = palloc(112,'TEMP2', 0, 1)
        setvar = palloc(111,'TEMP1', 0, 1)

!       Allocate mass storage
        nnc = kp  + neq
        setvar = palloc(17,'DAMP1', nnc, 2)

      elseif (isw.lt.0) then

        call uiters(kp,isw)
        bkmax = kp

      endif

!     Output solution properties
      tt = etime(tary)
      write(iow,2000) kp,tary
      if(ior.lt.0) then
        write(*,2000) kp,tary
      end if

!     Format

2000  format(10x,'Compressed Storage =',i9,20x,'t=',0p,2f9.2)

      end subroutine iters
