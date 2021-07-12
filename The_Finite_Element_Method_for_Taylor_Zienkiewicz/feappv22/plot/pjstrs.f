c$Id:$
      subroutine pjstrs(trifl)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Project nodal stresses

c      Inputs:
c         trifl      - Flag, generate element size for tri2d if true

c      Outputs:
c         none       - Output stored in blank common arrays
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'comblk.h'
      include  'cdata.h'
      include  'pdata3.h'
      include  'pointer.h'
      include  'sdata.h'
      include  'strnum.h'

      logical   trifl
      integer   i,ii

      save

c     Stress projections

      istv = npstr - 1

      ii = 0
      do i = nen1-1,nen1*numel-1,nen1
        if(mr(np(91)+ii).lt.0) then
          mr(np(33)+i) = -abs(mr(np(33)+i))
        endif
        ii = ii + 1
      end do

      call pzero(hr(np(58)), npstr*numnp)
      call pzero(hr(np(57)),     8*numnp)
      if(.not.trifl) call pzero(hr(np(60)),numel)

      call formfe(np(40),np(26),np(26),np(26),
     &           .false.,.false.,.false.,8,1,numel,1)

      call pltstr(hr(np(58)),hr(np(57)+numnp),hr(np(58)+numnp),
     &            numnp,ndm)

      do i = nen1-1,nen1*numel-1,nen1
          mr(np(33)+i) = abs(mr(np(33)+i))
      end do

      end
