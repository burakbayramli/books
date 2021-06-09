!$Id:$
      subroutine optid()

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Optimize profile numbering

!      Inputs:
!        Come from pointer arrays and common blocks.

!      Outputs:
!        Optimized list through pointer array 'mr(np(89))'.

!      Arrays used from FEAP
!         mr(np(21)) - JP: Column pointers for profile
!         mr(id31)   - ID: Equation number list
!         mr(np(33)) - IX: Element connection list
!         mr(np(34)) - LD: Local equation list
!         mr(np(89)) - NREN: Renumber list for nodes
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'cdata.h'
      include  'compac.h'
      include  'fdata.h'
      include  'idptr.h'
      include  'region.h'
      include  'sdata.h'
!     include  'umac1.h'

      include  'pointer.h'
      include  'comblk.h'

      logical       :: setvar, palloc
      integer       :: nesz, kp

      save

!     Set initial numbering in renumber vector

      do kp = 0,numnp-1
        mr(np(89)+kp) = kp+1
      end do ! kp

!     Optimize profile using Wilson/Hoit method

      setvar = palloc(112,'TEMP2',numnp,1)                    ! ND
      setvar = palloc(113,'TEMP3',numnp,1)                    ! LN
      setvar = palloc(114,'TEMP4',numnp,1)                    ! NWD
      setvar = palloc(115,'TEMP5',max(numnp,numel),1) ! MSUM
      setvar = palloc(116,'TEMP6',numnp,1)                    ! NNID
      nesz   = numel*nen*nen
      setvar = palloc(117,'TEMP7',max(numnp,numel,nesz),1) !NE

      call optibc(mr(id31),mr(np(116)),ndf,numnp)

      call opnum(mr(np(33)),mr(np(112)),mr(np(113)),
     &           mr(np(117)),mr(np(114)),mr(np(115)),mr(np(89)),
     &           mr(np(116)),
     &           numnp,numel,nen,nen1,.false.)

      setvar = palloc(117,'TEMP7',0,1)
      setvar = palloc(116,'TEMP6',0,1)
      setvar = palloc(115,'TEMP5',0,1)
      setvar = palloc(114,'TEMP4',0,1)
      setvar = palloc(113,'TEMP3',0,1)
      setvar = palloc(112,'TEMP2',0,1)

      end subroutine optid
