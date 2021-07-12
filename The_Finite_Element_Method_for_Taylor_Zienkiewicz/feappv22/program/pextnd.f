c$Id:$
      subroutine pextnd()

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose:

c      Inputs:

c      Outputs:
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'cdata.h'
      include  'cdat1.h'
      include  'pointer.h'
      include  'sdata.h'
      include  'comblk.h'

      logical   setvar,palloc

      save

c     Determine necessary storage for mesh lines and allocate storage

      setvar = palloc(37,'NORMV',numnp*3,2)

      call pnorml(mr(np(32)),mr(np(33)),hr(np(43)),hr(np(37)),
     &            mr(np(78)),nie,ndm,nen1,numnp,numel)

      end
