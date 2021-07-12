c$Id:$
      subroutine paboun(td,x,ang,ntyp,ndm,numnp,numprt,prt,prth)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Set angle for sloping boundary based on coordinates

c      Inputs:
c         td(*)    - Array containing coordinate of search and angle
c         x(ndm,*) - Nodal coordinates
c         ntyp(*)  - Node type ( < zero for inactive)
c         ndm      - Spatial dimension of mesh
c         numnp    - Number of nodes in mesh
c         numprt   - Print counter
c         prt      - Print generated data if true
c         prth     - Print title/header if true

c      Outputs:
c         ang(*)   - Angles for sloping boundary conditions
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'iofile.h'

      logical   prt,prth,clflg
      integer   ndm,numnp,numprt, n,nbc
      real*8    xmn, tmn

      integer   ntyp(*)
      real*8    x(ndm,numnp),ang(*),td(*)

      real*8    dotx

      save

c     Find closest node to input coordinates

      if(prt .and. numprt.le.0) then
        call prtitl(prth)
        write(iow,2000)
        if(ior.lt.0) write(*,2000)
        numprt = 50
      endif

      clflg = .false.
      do n = 1,numnp
        if(ntyp(n).ge.0) then
          tmn = dotx(td(1),x(1,n),ndm)
          if(clflg) then
            if(tmn.lt.xmn) then
              xmn = tmn
              nbc = n
            endif
          else
            xmn   =  tmn
            nbc   =  n
            clflg = .true.
          endif
        endif
      end do

c     Set angle

      if(clflg) then
        ang(nbc) = td(ndm+1)

c       Output current restraint codes set

        if(prt) then
          write(iow,2001) nbc,ang(nbc)
          if(ior.lt.0) then
            write(*,2001) nbc,ang(nbc)
          endif
          numprt = numprt - 1
        endif
      endif

c     Format

2000  format('  C o o r d i n a t e    N o d a l    A n g l e s'/
     &       /(4x,'Node   Angle'))

2001  format(i8,1p,e12.4)

      end
