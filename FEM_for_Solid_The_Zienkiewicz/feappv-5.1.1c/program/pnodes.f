!$Id:$
      subroutine pnodes(xs,ndm,prt,prth)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

      implicit  none

      include  'iofile.h'

      logical       :: prt,prth, setvar,pinput
      integer       :: ndm, i,n,node
      real (kind=8) :: xs(3,*), td(6)

      save

      if(prt) then
        call prtitl(prth)
        write(iow,2000) (n,n=1,ndm)
        if(ior.lt.0) then
          write(*,2000) (n,n=1,ndm)
        endif
      endif

!     Input coordinates of supernodes for blending inputs

1     if(ior.lt.0) write(*,3000)

      setvar = pinput(td,ndm+1)

      node   = nint(td(1))

      if(node.gt.0) then

        do n = 1,ndm
          xs(n,node) = td(n+1)
        end do

        if(prt) then
          write(iow,2001) node,(xs(i,node),i=1,ndm)
          if(ior.lt.0) then
            write(*,2001) node,(xs(i,node),i=1,ndm)
          endif
        endif

      else
        return
      endif

      go to 1

3000  format('   Input: node, xs(i,node),i=1,ndm'/'   >',$)

2000  format('   S u p e r N o d e   C o o r d i n a t e s'//
     &   '       Node  ',i1,'-Coordinate':,'   ',i1,'-Coordinate':,
     &   '   ',i1,'-Coordinate'/)

2001  format(i10,1p,3e15.5)

      end subroutine pnodes
