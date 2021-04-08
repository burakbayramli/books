!$Id:$
      subroutine profil (jp,idl,eq,ix,iop,prt)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Compute profile of global arrays

!      Inputs:
!        ix(*)  - Element nodal connection list
!        iop    - Switch to control operation
!                  = 1 to set up equation numbers of dof's
!                  = 2 to compute the column/row lengths and profile.
!        prt    - Flag, print solution properties if true

!      Scratch:
!        idl(*) - Array to hold temporary information

!      Outputs:
!        eq(*)  - Equation numbers for degree of freedoms     (iop = 1)
!        jp(*)  - Pointer array to row/column ends of profile (iop = 2)
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'cdata.h'
      include  'iofile.h'
      include  'sdata.h'
      include  'pointer.h'
      include  'comblk.h'

      logical       :: prt, setvar,palloc
      integer       :: iop, mm,nad
      integer       :: jp(*),idl(*),eq(*),ix(*)

      save

!     Set up equation numbers
      if(iop.eq.1) then

        call seteq(eq,prt)

!       Reset size of solution array if necessary
        setvar = palloc(26,'DR   ',max(neq,numnp*max(ndf,ndm)),2)

!     Compute column heights
      elseif(iop.eq.2) then

        call rstprf(jp,idl,eq,ix,mr(np(32)),
     &              ndl,ndf,nen1,nen,neq,numel,nummat)

!       Compute diagonal pointers for profile
        call nwprof(jp,neq)

!       Output statistics

        if(neq.gt.0 .and. prt) then
          nad = jp(neq)
          mm = (nad+neq)/neq
          write(iow,2001) ndm,ndf,neq,numnp,mm,numel,nad,nummat
          if(ior.lt.0)
     &      write(*,2001) ndm,ndf,neq,numnp,mm,numel,nad,nummat
        endif
      endif

2001  format(/5x,'E q u a t i o n / P r o b l e m   S u m m a r y:'//
     & 10x,'Space dimension (ndm) =',i10,3x,'Number dof (ndf) =',i8/
     & 10x,'Number of equations   =',i10,3x,'Number nodes     =',i8/
     & 10x,'Average col. height   =',i10,3x,'Number elements  =',i8/
     & 10x,'Number profile terms  =',i10,3x,'Number materials =',i8/)

      end subroutine profil
