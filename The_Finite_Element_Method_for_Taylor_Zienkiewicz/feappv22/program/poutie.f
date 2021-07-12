c$Id:$
      subroutine poutie(itie,ix,nty,nen,nen1,numnp,numel,prt)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Output list of tied nodes

c      Inputs:
c         ix(nen1,*)- Element nodal connection list
c         nty(*)    - Nodal type
c         nen       - Number of nodes on an element
c         nen1      - Dimension of ix array
c         numnp     - Number of nodes in mesh
c         numel     - Number of elements in mesh
c         prt       - Flag, Output list of nodes if true, otherwise
c                     output number of nodes tied.

c      Scratch:
c         itie(*)   - List of nodes used on elements

c      Outputs:
c         none      - Output is list of nodes to screen/file
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'iofile.h'

      logical   prt,fflg
      integer   nen,nen1,numnp,numel
      integer   ii,i1, j1, nn

      integer   itie(*),ix(nen1,*),nty(*)
      integer   il(10)

      save

c     Compute and Output list of nodes removed

      do j1 = 1,numnp
        itie(j1) = 0
      end do

      do j1 = 1,numel
        do i1 = 1,nen
          if(ix(i1,j1).gt.0) then
            itie(ix(i1,j1)) = 1
          endif
        end do
      end do

      if(ior.lt.0. and. prt) write(  *,2001)
      write(iow,2001)
      i1   =  0
      nn   =  0
      fflg = .true.
      do j1 = 1,numnp

c       Delete equations for all unused nodes from a tie

        if(itie(j1).eq.0) then
          nty(j1) = -1
          i1         = i1 + 1
          nn         = nn + 1
          il(i1)     = j1
        endif

c       Output list of eliminated nodes

        if(i1.ge.10 .or. (i1.gt.0 .and. j1.eq.numnp)) then
          if(ior.lt.0 .and. prt) write(  *,2002) (il(ii),ii=1,i1)
          if(prt)write(iow,2002) (il(ii),ii=1,i1)
          i1 = 0
          fflg = .false.
        endif

      end do

c     Output results of tie command

      if(fflg) then
        if(ior.lt.0 .and. prt) write(  *,2003)
        write(iow,2003)
      else
        if(ior.lt.0 .and. prt) write(  *,2004) nn
        write(iow,2004) nn
      endif

2001  format(//6x,' T i e  - - -  N o d a l   C o o r d i n a t e s'//
     &         6x,'    The following nodes have been deleted:'/)

2002  format(6x,10i7)

2003  format(6x,'No nodes merged by the tie command'/1x)

2004  format(/6x,i5,' nodes merged by the tie command'/1x)

      end
