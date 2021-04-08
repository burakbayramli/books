!$Id:$
      double precision function gamma1(id,pu,pr,du,t,s)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Compute solution energy for step size 's'

!      Inputs:
!         id(*)  - Equation numbers for each dof
!         pu     - Pointer to solution vectors
!         du(*)  - Last increment to solution
!         s      - Step size

!      Outputs:
!         gamma1 - Solution energy for current step

!      Scratch:
!         pr     - Pointer to residual array
!         t(*)   - Temporary storage for solution arrays
!-----[--.----+----.----+----.-----------------------------------------]
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

      include  'p_gamma1.h'

      logical       :: fa,tr
      integer       :: n
      real (kind=8) :: s

      integer       :: id(*)
      real (kind=8) :: du(*),t(nneq,*)

      real (kind=8) :: dot

      save

      data      fa,tr/.false.,.true./

!     Increment counter on RHS forms

      iform = iform + 1

!     Get search displacement

!     Move quantities for saves

      call pmove(hr(pu)  ,t     ,3*nneq)
      call pmove(    du  ,t(1,4),  nneq)
      if(np(42).ne.0) call pmove(hr(np(42)),t(1,5),nneq*nrt)

!     Multiply increment by current step size

      do n = 1,neq
        du(n) = du(n)*s
      end do

!     Update with step control

      call pupdate(id,hr(np(30)),hr(pu),hr(np(42)),du,fl(9),2)

!     Compute residual

      call pload(id,hr(np(30)),hr(pr),prop,tr)
      call formfe(pu,pr,pr,pr,fa,tr,fa,6,1,numel,1)

!     Restore quantities from saves

      call pmove( t     ,hr(pu),3*nneq)
      call pmove( t(1,4),    du,  nneq)
      if(np(42).ne.0) call pmove(t(1,5),hr(np(42)),nneq*nrt)

!     Compute value of gamma

      gamma1 = dot (du,hr(pr),neq)

      end function gamma1
