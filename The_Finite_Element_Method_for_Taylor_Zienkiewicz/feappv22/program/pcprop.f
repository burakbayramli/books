c$Id:$
      subroutine pcprop(td,x,icd,ntyp,ndm,ndf,numnp,numprt,prt,prth)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Input proportional load numbers at specified coordinates

c      Inputs:
c         td(*)    - Coordinates and values for point
c         x(ndm,*) - Nodal coordinates of mesh
c         ntyp(*)  - Node type (negative for inactive)
c         ndm      - Spatial dimension of mesh
c         ndf      - Number dof/node
c         numnp    - Number of nodes in mesh
c         numprt   - Print counter
c         prt      - Output generated values if true
c         prth     - Output title/header data if true

c      Outputs:
c         icd(ndf,*) - Generated force or displacements
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'iofile.h'

      logical   prt,prth, clflg
      integer   ndm,ndf,numnp,numprt, n,nbc
      real*8    xmn, tmn

      integer   ntyp(*), icd(ndf,*)
      real*8    x(ndm,numnp),td(*)

      real*8    dotx

      save

c     Find closest node to input coordinates

      if(prt .and. numprt.le.0) then
        call prtitl(prth)
        write(iow,2000) (n,n=1,ndf)
        if(ior.lt.0) write(*,2000) (n,n=1,ndf)
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

c     Set proportional load numbers

      if(clflg) then
        do n = 1,ndf
          icd(n,nbc) = nint(td(ndm+n))
        end do

c       Output current restraint codes set

        if(prt) then
          write(iow,2001) nbc,(icd(n,nbc),n=1,ndf)
          if(ior.lt.0) then
            write(*,2001) nbc,(icd(n,nbc),n=1,ndf)
          endif
          numprt = numprt - 1
        endif
      endif

c     Formats

2000  format('  C o o r d i n a t e    N o d a l    V a l u e s'/
     &      /4x,'Node',6(i3,'-Propld'):/(8x,6(i3,'-Propld')))

2001  format(i8,6i10:/(8x,6i10))

3000  format(' *ERROR* Surface loading file',a,' does not exist')

3001  format(' Input: x(i),i=1,ndm),(icd(i),i=1,',i2,')'/'   >',$)

3002  format(' *ERROR* Surface type ',a,' does not exist')

      end
