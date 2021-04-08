!$Id:$
      subroutine autbac ( dtnew )

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:   Back up solution and adjust to new time step size.

!      Inputs:
!         dtnew - New time step

!      Outputs:
!               - Loads and solution at new time step (dtnew)

!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'augdat.h'
      include  'cdata.h'
      include  'fdata.h'
      include  'iofile.h'
      include  'pointer.h'
      include  'print.h'
      include  'prld1.h'
      include  'prlod.h'
      include  'rdata.h'
      include  'sdata.h'
      include  'tdata.h'
      include  'comblk.h'

      integer       :: i,k1
      real (kind=8) :: dtnew,propld

      integer       :: npl(2)

      save

!     1.  Backup time step

      ttim   = ttim - dt
      npl(1) = 0
      npl(2) = 0
      if(npld.gt.0) prop = propld(ttim,npl)
      if(prnt) then
        if(npld.gt.1) then
          k1 = npld
        else
          k1 = 0
        endif
        write(iow,2000) ttim,prop,(i,prldv(i),i=1,k1)
        if(ior.lt.0) then
          write(*,2000) ttim,prop,(i,prldv(i),i=1,k1)
        endif
      endif
      rnmax = 0.0d0

!     Reinitialize solution and dynamic vectors for step

          call pupdate(mr(np(31)),hr(np(30)),hr(np(40)),hr(np(42)),
     &                 hr(np(26)),fl(9),3)

!     Reinitialize history vectors for step

      call reshis(mr(np(33)+nen),nen1,numel,1, 2)

!     2.  Advance time to new start point

      if(dtnew .gt. 0.0d0) then

!       Increment time

        dt   = dtnew
        ttim = ttim + dt

        if(npld.gt.0) prop = propld(ttim,npl)
        if(prnt) then
          write(iow,2000) ttim,prop,(i,prldv(i),i=1,k1)
          if(ior.lt.0) then
            write(*,2000) ttim,prop,(i,prldv(i),i=1,k1)
          endif
        endif

!       Move interpolated force vector

        call pmove (hr(np(30)       ),hr(np(30)+  nneq), nneq)
        call pmove (hr(np(30)+2*nneq),hr(np(30)+3*nneq), nneq)

!       Update dynamic vectors for transient step

        if(fl(9)) then
          call dsetci
          call pupdate(mr(np(31)),hr(np(30)),hr(np(40)),hr(np(42)),
     &                 hr(np(26)),fl(9),1)
        end if

!       Zero displacement increments for time step

        call pzero(hr(np(40)+nneq),nneq+nneq)

!       Reset history variables

        call reshis(mr(np(33)+nen),nen1,numel,2, 1)

!       Reset parameters for solution step

        augf  = 1.0d0
        rnmax = 0.0d0
        fl( 8) = .false.
        fl(10) = .true.

      endif

!     Format

2000  format(/,'   Computing solution at time ',1p,1e11.4,
     &         ': Total proportional load ',1p,1e11.4:/
     &         '   Individual factors: '/(3x,4(i4,' =',1p,1e12.4)))

      end subroutine autbac
