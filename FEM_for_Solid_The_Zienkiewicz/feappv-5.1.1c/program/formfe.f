!$Id:$
      subroutine formfe(pnu,pnb,pna,pnl,aufl,bfl,dfl,
     &                  isw,nl1,nl2,nl3)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Forms finite element arrays as required

!      Inputs:
!         pnu    - Pointer to current nodal solution vectors
!         pnb    - Pointer for FEM Vector
!         pna    - Pointer for FEM diagonal and upper part of array
!         pnl    - Pointer for FEM lower part of array
!         aufl   - If true assemble 'a' array (which includes 'au')
!         bfl    - If true assemble 'b' array
!         dfl    - If true assembel 'b' uncompressed
!         isw    - Solution switch controlling action to be taken
!         nl1    - First element to be processed
!         nl2    - Last  element to be processed
!         nl3    - Increment to 'nl1'

!      Outputs:
!         hr(pnb)- Values for FEM Vector
!         hr(pna)- Values for FEM diagonal and upper array part
!         hr(pnl)- Values for FEM lower array part
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'cdata.h'
      include  'cdat1.h'
      include  'hdatam.h'
      include  'ldata.h'
      include  'ndata.h'
      include  'p_formfe.h'
      include  'pointer.h'
      include  'prflag.h'
      include  'sdata.h'
      include  'comblk.h'

      logical       :: aufl,bfl,dfl
      integer       :: isw, nl1, nl2, nl3

      save

!     Set flag for convergence checks

      if(isw.eq.3 .or. (isw.eq.6 .and. .not.dfl)) then
        lvcn     = lv
        floop(1) = .true.
      endif

!     Form appropriate finite element arrays

      call pform(hr(np(41)),hr(np(44)),hr(np(39)),mr(np(34)),hr(np(35)),
     &           hr(np(36)),mr(np(32)),hr(np(25)),mr(np(31)),hr(np(43)),
     &           mr(np(33)),hr(np(30)),hr(np(38)),
     &           mr(np(21)),hr(pnu),hr(np(42)),hr(pnb),hr(pna),
     &           hr(pnl),ndd,nie,ndf,ndm,nen1,nst,aufl,bfl,dfl,
     &           isw,nl1,nl2,nl3)

!     Reset update flag for history variables

      hflgu  = .false.
      h3flgu = .false.

      end subroutine formfe
