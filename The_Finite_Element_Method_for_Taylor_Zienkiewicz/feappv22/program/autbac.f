c$Id:$
      subroutine autbac ( dtnew )

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose:   Back up solution and adjust to new time step size.

c      Inputs:
c         dtnew - New time step

c      Outputs:
c               - Loads and solution at new time step (dtnew)

c-----[--.----+----.----+----.-----------------------------------------]
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

      integer   i,k1
      real*8    dtnew,propld

      integer   npl(1)

      save

c     1.  Backup time step

      ttim   = ttim - dt
      npl(1) = 0
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

c     Reinitialize solution and dynamic vectors for step

          call update(mr(np(31)),hr(np(30)),hr(np(40)),hr(np(42)),
     &                hr(np(26)),fl(9),3)

c     Reinitialize history vectors for step

      call reshis(mr(np(33)+nen),nen1,numel,1, 2)

c     2.  Advance time to new start point

      if(dtnew .gt. 0.0d0) then

c       Increment time

        dt   = dtnew
        ttim = ttim + dt

        if(npld.gt.0) prop = propld(ttim,npl)
        if(prnt) then
          write(iow,2000) ttim,prop,(i,prldv(i),i=1,k1)
          if(ior.lt.0) then
            write(*,2000) ttim,prop,(i,prldv(i),i=1,k1)
          endif
        endif

c       Move interpolated force vector

        call pmove (hr(np(30)       ),hr(np(30)+  nneq), nneq)
        call pmove (hr(np(30)+2*nneq),hr(np(30)+3*nneq), nneq)

c       Update dynamic vectors for transient step

        if(fl(9)) then
          call dsetci
          call update(mr(np(31)),hr(np(30)),hr(np(40)),hr(np(42)),
     &                hr(np(26)),fl(9),1)
        end if

c       Zero displacement increments for time step

        call pzero(hr(np(40)+nneq),nneq+nneq)

c       Reset history variables

        call reshis(mr(np(33)+nen),nen1,numel,2, 1)

c       Reset parameters for solution step

        augf  = 1.0d0
        rnmax = 0.0d0
        fl( 8) = .false.
        fl(10) = .true.

      endif

c     Format

2000  format(/,'   Computing solution at time ',1p,1e11.4,
     &         ': Total proportional load ',1p,1e11.4:/
     &         '   Individual factors: '/(3x,4(i4,' =',1p,1e12.4)))

      end
