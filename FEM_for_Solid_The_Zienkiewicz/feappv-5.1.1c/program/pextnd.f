!$Id:$
      subroutine pextnd()

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:

!      Inputs:

!      Outputs:
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'cdata.h'
      include  'cdat1.h'
      include  'pointer.h'
      include  'sdata.h'
      include  'comblk.h'

      logical       :: setvar,palloc

      save

!     Determine necessary storage for mesh lines and allocate storage

      setvar = palloc(206,'NORMV',numnp*3,2)

      call pnorml(mr(np(32)),mr(np(33)),hr(np(43)),hr(np(206)),
     &            mr(np(78)),nie,ndm,nen,nen1,numnp,numel)

      end subroutine pextnd
