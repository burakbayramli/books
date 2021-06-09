!$Id:$
      subroutine pjstrs(trifl)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Project nodal stresses

!      Inputs:
!         trifl      - Flag, generate element size for tri2d if true

!      Outputs:
!         none       - Output stored in blank common arrays
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'comblk.h'
      include  'cdata.h'
      include  'cnurb.h'
      include  'hdatam.h'
      include  'qudshp.h'
      include  'pdata3.h'
      include  'pointer.h'
      include  'sdata.h'
      include  'strnum.h'

      logical   trifl
      integer   i,ii

      save

!     Stress projections

      istv = npstr - 1

      ii = 0
      do i = nen1-1,nen1*numel-1,nen1
        if(mr(np(128)+ii).lt.0) then
          mr(np(33)+i) = -abs(mr(np(33)+i))
        endif
        ii = ii + 1
      end do ! i

      call pzero(hr(npnp), npstr*numnp)
      call pzero(hr(nper),     8*numnp)
      if(.not.trifl) call pzero(hr(np(207)),numel)

!     fp(1)  = np(36)
!     np(36) = np(60)
!     pltmfl = .true.
      call formfe(np(40),np(26),np(26),np(26),
     &           .false.,.false.,.false.,8,1,numel,1)
!     pltmfl = .false.
!     np(36) = fp(1)

      call pltstr(hr(npnp),hr(nper+numnp),hr(npnp+numnp),
     &            numnp,ndm)
!    &            numnp,ndm,.true.)

      do i = nen1-1,nen1*numel-1,nen1
        mr(np(33)+i) = abs(mr(np(33)+i))
      end do ! i

      end
