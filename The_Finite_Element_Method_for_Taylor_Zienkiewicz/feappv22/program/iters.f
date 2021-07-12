c$Id:$
      subroutine iters(bkmax,isw)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose:    Controls solution by iterative schemes. Also sets
c                  storage for compressed arrays for direct solution
c                  by sparse solvers or in blocks with disk stores.

c      Inputs:
c         bkmax  - Maximum size of block for direct solution
c         isw    - Switch: isw =  1 for TANGS
c                          isw =  2 for CMASS
c                          isw =  3 for DAMPS
c                          isw = -1 for USER module

c      Outputs:
c         none   - Outputs stored in blank common and pointers.
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'allotd.h'
      include  'cdata.h'
      include  'comblk.h'
      include  'compac.h'
      include  'compas.h'
      include  'iofile.h'
      include  'ndata.h'
      include  'pointer.h'
      include  'psize.h'
      include  'sdata.h'
      include  'ssolve.h'

      logical   setvar,palloc
      integer   isw,len,kp,bkmax, iptc
      real*4    tary(2), etime , tt

      save

c     Compute sparse storage for non-zero matrix

      if(isw.gt.0) then
        setvar = palloc(81,'TEMP1', numnp*ndf, 1)
        call elcnt(numnp,numel,nen,nen1,mr(np(33)),mr(np(81)),.true. )
        call sumcnt(mr(np(81)),numnp*ndf,kp)

        setvar = palloc(82,'TEMP2', kp, 1)
        call pelcon(numel,nen,nen1,mr(np(33)),mr(np(81)),mr(np(82)),
     &              kp,1)
      endif

c     1. TANGENT Formation

      if(isw.eq.1) then

c       Slot for user supplied sparse solver

        if(ittyp.eq.-2) then
          iptc   = 1
c         ubycol =
c         udiag  =
c         uall   =
c         iptc   =
c         nspo   =

c       Program solvers with sparse assembly

        else
          kbycol = .true.     ! Store by columns
          kdiag  = .false.
          kall   = .false.
          iptc   =  neq
        endif

        setvar = palloc( 2,'OINUC', iptc , 1)
        if(np(3).ne.0) then
          setvar = palloc( 3,'OINUO',0, 1)
        endif
        setvar = palloc( 3,'OINUO',  1, 1)
        call compro(numnp,nen,nen1,ndf,mr(np(33)),mr(np(31)),
     &              mr(np(81)),mr(np(82)),mr(np(3)),mr(np(2)),
     &              kp,maxm-mmax,kbycol,kdiag,kall)
        setvar = palloc( 3,'OINUO', kp, 1)

c       Delete temporary arrays

        setvar = palloc(82,'TEMP2', 0, 1)
        setvar = palloc(81,'TEMP1', 0, 1)

c       Set storage for sparse stiffness array

        if(ittyp.eq.-1) then

          kp  = max(kp+neq,bkmax)
          len = kp

c       User supplied sparse solver location

        elseif(ittyp.eq.-2) then

c       Profile solver assembly

        else

          kp  = kp + neq
          len = 0

        endif

        setvar = palloc(1,'TANGS', kp+len, 2)

        na     = np(1)
        nnr    = kp
        nau    = na  + neq
        nal    = nau + len
        numcels= 0
        compfl = .true.

c     2. Consistent MASS Formation

      elseif(isw.eq.2) then
        mbycol = .true.       ! Store by columns
        mdiag  = .false.
        mall   = .false.
        setvar =  palloc( 10,'OINMC', neq  , 1)
        if(np(11).ne.0) then
          setvar = palloc( 11,'OINMO',0, 1)
        endif
        setvar = palloc( 11,'OINMO',  1, 1)
        call compro(numnp,nen,nen1,ndf,mr(np(33)),mr(np(31)),
     &              mr(np(81)),mr(np(82)),mr(np(11)),mr(np(10)),
     &              kp,maxm-mmax,mbycol,mdiag,mall)
        setvar = palloc( 11,'OINMO', kp, 1)

c       Delete temporary arrays

        setvar = palloc(82,'TEMP2', 0, 1)
        setvar = palloc(81,'TEMP1', 0, 1)

c       Allocate mass storage

        nnm = kp  + neq
        setvar = palloc(9,'CMASS', nnm, 2)

c     3. Consistent DAMP Formation

      elseif(isw.eq.3) then
        cbycol = .true.       ! Store by columns
        cdiag  = .false.
        call   = .false.
        setvar =  palloc(18,'OINCC', neq  , 1)
        if(np(19).ne.0) then
          setvar = palloc(19,'OINCO',0, 1)
        endif
        setvar = palloc(19,'OINCO',  1, 1)
        call compro(numnp,nen,nen1,ndf,mr(np(33)),mr(np(31)),
     &              mr(np(81)),mr(np(82)),mr(np(19)),mr(np(18)),
     &              kp,maxm-mmax,cbycol,cdiag,call)
        setvar = palloc(19,'OINCO', kp, 1)

c       Delete temporary arrays

        setvar = palloc(82,'TEMP2', 0, 1)
        setvar = palloc(81,'TEMP1', 0, 1)

c       Allocate mass storage

        nnc = kp  + neq
        setvar = palloc(17,'DAMPS', nnc, 2)

      elseif (isw.lt.0) then

        call uiters(kp,isw)
        bkmax = kp

      endif

c     Output solution properties

      tt = etime(tary)
      write(iow,2000) kp,tary
      if(ior.lt.0) then
        write(*,2000) kp,tary
      end if

c     Format

2000  format(10x,'Compressed Storage =',i9,20x,'t=',0p,2f9.2)

      end
