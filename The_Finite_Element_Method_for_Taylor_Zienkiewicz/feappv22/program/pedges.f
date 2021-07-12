c$Id:$
      subroutine pedges(x,id,ndm,ndf,numnp,type,prt,prth,name)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Set boundary restraint conditions based on specified
c               edge coordinates.

c      Inputs:
c         x(ndm,*)   - Nodal coordinates of mesh
c         ndm        - Spatial dimension of mesh
c         ndf        - Number dof/node
c         numnp      - Number of nodes in mesh
c         prt        - Output generated results if true
c         prth       - Output title/header data if true

c      Outputs:
c         id(ndf,*)  - Boundary restraint conditions
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'iofile.h'
      include  'pointer.h'
      include  'comblk.h'

      character text*15, type*4, label*4, name*(*)
      logical   prt,prth, errck, tinput, vinput, pcomp, gapfl
      integer   ndm,ndf,numnp, i,j,n, nty
      real*8    dx, x0, gap

      integer   id(ndf,numnp),idl(14)
      real*8    x(ndm,numnp),td(16)

      real*8    pdiff

      save

c     Read input of boundary edge for restraints

      label = name
      gap   = 1.0d-3
      gapfl = .false.
      nty   = np(49) - 1
100   if(ior.lt.0) write(*,3001)
      errck = tinput(text,1,td(2),ndf+1)
      if(errck) go to 100
      if(pcomp(text,'gap',3)) then
        gap   = td(2)
        gapfl = .true.
        if(prt) then
          write(iow,2002) gap
          if(ior.lt.0) then
            write(*,2002) gap
          endif
        endif
        go to 100
      else
        errck = vinput(text,15,td(1),1)
        i     = nint(td(1))
      endif
      if(i.le.0.or.i.gt.ndm) go to 4
      x0 = td(2)
      do j = 1,ndf
        idl(j) = td(j+2)
      end do
      if(gapfl) then
        dx = gap
      else
        dx = pdiff(x,i,ndm,numnp)*gap
      endif
      do n = 1,numnp
        if(mr(nty+n).ge.0.and.abs(x(i,n)-x0).le.dx) then
          if(pcomp(type,'add',3)) then
            do j = 1,ndf
              id(j,n) = max(abs(id(j,n)),abs(idl(j)))
            end do
          else
            do j = 1,ndf
              id(j,n) = abs(idl(j))
            end do
          endif
        endif
      end do
      go to 100

4     call prtitl(prth)
      if(prt) then
        write(iow,2000) label,(i,label,i=1,ndf)
        if(ior.lt.0) then
          write(*,2000) label,(i,label,i=1,ndf)
        endif
      endif

      do n = 1,numnp
        do i = 1,ndf
          if(id(i,n).ne.0) go to 400
        end do
        go to 410
400     if(prt) write(iow,2001) n,(id(i,n),i=1,ndf)
        if(prt.and.ior.lt.0) write(*,2001) n,(id(i,n),i=1,ndf)
410     continue
      end do

c     Formats

2000  format('  E d g e    N o d a l    ',a4/
     &       /(4x,'node',9(i3,'-',a4)))

2001  format(10i8)

2002  format(/'     Search gap =',1p,1e12.4/)

3001  format(' Input: ndir,x(ndir),(idl(i),i=1,ndf)','   >',$)

      end
