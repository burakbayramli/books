c$Id:$
      double precision function gamma1(id,pu,pr,du,t,s)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Compute solution energy for step size 's'

c      Inputs:
c         id(*)  - Equation numbers for each dof
c         pu     - Pointer to solution vectors
c         du(*)  - Last increment to solution
c         s      - Step size

c      Outputs:
c         gamma1 - Solution energy for current step

c      Scratch:
c         pr     - Pointer to residual array
c         t(*)   - Temporary storage for solution arrays
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'cdata.h'
      include  'counts.h'
      include  'ddata.h'
      include  'fdata.h'
      include  'ndata.h'
      include  'pointer.h'
      include  'prlod.h'
      include  'sdata.h'
      include  'tdata.h'
      include  'comblk.h'

      logical   fa,tr
      integer   n
      integer   pr,pu
      real*8    s

      integer   id(*)
      real*8    du(*),t(nneq,*)

      real*8    dot

      save

      data      fa,tr/.false.,.true./

c     Increment counter on RHS forms

      iform = iform + 1

c     Get search displacement

c     Move quantities for saves

      call pmove(hr(pu)  ,t     ,3*nneq)
      call pmove(    du  ,t(1,4),  nneq)
      if(np(42).gt.0) call pmove(hr(np(42)),t(1,5),nneq*nrt)

c     Multiply increment by current step size

      do n = 1,neq
        du(n) = du(n)*s
      end do

c     Update with step control

      call update(id,hr(np(30)),hr(pu),hr(np(42)),du,fl(9),2)

c     Compute residual

      call pload(id,hr(np(30)),hr(pr),prop,tr)
      call formfe(pu,pr,pr,pr,fa,tr,fa,6,1,numel,1)

c     Restore quantities from saves

      call pmove( t     ,hr(pu),3*nneq)
      call pmove( t(1,4),    du,  nneq)
      if(np(42).gt.0) call pmove(t(1,5),hr(np(42)),nneq*nrt)

c     Compute value of gamma

      gamma1 = dot (du,hr(pr),neq)

      end
