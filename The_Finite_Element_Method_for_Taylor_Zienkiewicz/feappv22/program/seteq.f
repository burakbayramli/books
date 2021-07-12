c$Id:$
      subroutine seteq(id,ndf,ndm,numnp,neq,prt)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Set equation numbers for solution

c      Inputs:
c        id(ndf,*) - Boundary condition indicators
c        ndf       - Number dof/node
c        ndm       - Spatial dimension of problem
c        numnp     - Number of nodes in mesh
c        prt       - Flag, output results if true

c      Outputs:
c        id(ndf,*) - Equation numbers for each dof.  Active dof
c                    have positive numbers for equation, fixed
c                    dof have negative numbers
c        neq       - Number of active equations in problem
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'mxsiz.h'
      include  'pointer.h'
      include  'comblk.h'

      logical   prt
      integer   ndf,ndm,numnp,nad,neq,n,nn, i, j
      integer   id(ndf,numnp)

      save

c     Set equation numbers

      neq  = 0
      nad  = 0
      do n = 0,numnp-1
        nn = mr(np(47)+n)
        do i = 1,ndf
          j = id(i,nn)
          if(j.eq.0) then
            neq     = neq + 1
            id(i,nn) = neq
          else
            nad     = nad - 1
            id(i,nn) = nad
          endif
        end do
      end do

c     Link nodes from data

      if(lkflg) then
        call plink(id,hr(np(43)),ndm,ndf,numnp,neq,prt)
      endif

      end
