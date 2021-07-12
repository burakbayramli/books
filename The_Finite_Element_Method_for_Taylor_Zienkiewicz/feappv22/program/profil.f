c$Id:$
      subroutine profil (jp,idl,id,ix,iop,prt)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Compute profile of global arrays

c      Inputs:
c        ix(*)  - Element nodal connection list
c        iop    - Switch to control operation
c                  = 1 to set up equation numbers of dof's
c                  = 2 to compute the column/row lengths and profile.
c        prt    - Flag, print solution properties if true

c      Scratch:
c        idl(*) - Array to hold temporary information

c      Outputs:
c        id(*)  - Equation numbers for degree of freedoms     (iop = 1)
c        jp(*)  - Pointer array to row/column ends of profile (iop = 2)
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'cdata.h'
      include  'iofile.h'
      include  'machnc.h'
      include  'sdata.h'

      logical   prt
      integer   iop, mm,nad
      integer   jp(*),idl(*),id(*),ix(*)

      save

c     Set up equation numbers

      if(iop.eq.1) then

        call seteq(id,ndf,ndm,numnp,neq,prt)

c     Compute column heights

      elseif(iop.eq.2) then

        call rstprf(jp,idl,id,ix,ndf,nen1,nen,neq,numel)

c       Compute diagonal pointers for profile

        call nwprof(jp,neq)

c       Estimate solve time (dops = est. no. ops/sec)

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

      end
