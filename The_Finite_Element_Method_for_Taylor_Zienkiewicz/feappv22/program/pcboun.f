c$Id:$
      subroutine pcboun(td,x,id,ntyp,ndm,ndf,numnp,numprt,gap0,
     &                  type,prt,prth)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Set boundary condition codes based on coordinate
c               inputs

c      Inputs:
c         td(*)     - Array containing coordinates and boundary
c                     restraint pattern
c         x(ndm,*)  - Nodal coordinates
c         ntyp(*)   - Node type ( < zero for inactive)
c         ndm       - Spatial dimension of mesh
c         ndf       - Number dof/node
c         numnp     - Number of nodes in mesh
c         numprt    - Print counter
c         gap0      - Search gap
c         type      - Replacement type: 'add' = accumulate; else reset.
c         prt       - Output generated results if true
c         prth      - Output title/header data if true

c      Outputs:
c         id(ndf,*) - Generated boundary restraint codes
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'iofile.h'

      character type*4
      logical   prt,prth,clflg, pcomp
      integer   ndm,ndf,numnp,numprt, i,n,nbc
      real*8    gmn, tmn, gap0

      integer   id(ndf,numnp),ntyp(*)
      real*8    xmn(3),xmx(3), x(ndm,numnp),td(*)

      real*8    dotx

      save

c     Find closest node to input coordinates

      if(prt .and. numprt.le.0) then
        call prtitl(prth)
        write(iow,2000) (i,i=1,ndf)
        if(ior.lt.0) write(*,2000) (i,i=1,ndf)
        numprt = 50
      endif

      clflg = .false.
      do n = 1,numnp
        if(ntyp(n).ge.0) then
          tmn = dotx(td(1),x(1,n),ndm)
          if(clflg) then
            if(tmn.lt.gmn) then
              gmn = tmn
            endif
            do i = 1,min(ndm,3)
              xmn(i) = min(xmn(i),x(i,n))
              xmx(i) = max(xmx(i),x(i,n))
            end do ! i
          else
            gmn   =  tmn
            clflg = .true.
            do i = 1,min(ndm,3)
              xmn(i) = x(i,n)
              xmx(i) = x(i,n)
            end do ! i
          endif
        endif
      end do

c     Set search gap

      tmn = 0.0d0
      do i = 1,min(ndm,3)
        tmn = max(xmx(i) - xmn(i),tmn)
      end do ! i

      if(gap0.eq.0.0d0) then
        gmn = gmn + tmn*1.d-10
      else
        gmn = gmn + gap0
      endif

c     Set b.c. restraint codes

      if(clflg) then
        do nbc = 1,numnp
          if(ntyp(nbc).ge.0) then
            tmn = dotx(td(1),x(1,nbc),ndm)
            if(tmn.le.gmn) then
              if(pcomp(type,'add',3)) then
                do n = 1,ndf
                  id(n,nbc) = max(abs(id(n,nbc)),abs(nint(td(ndm+n))))
                end do
              else
                do n = 1,ndf
                  id(n,nbc) = abs(nint(td(ndm+n)))
                end do
              endif

c             Output current restraint codes set

              if(prt) then
                write(iow,2001) nbc,(id(i,nbc),i=1,ndf)
                if(ior.lt.0) then
                  write(*,2001) nbc,(id(i,nbc),i=1,ndf)
                endif
                numprt = numprt - 1
              endif
            endif ! tmn < gmn
          endif ! ntyp(n) > 0
        end do ! nbc
      endif ! clflg = .true.

c     Format

2000  format('  C o o r d i n a t e    N o d a l    V a l u e s'/
     &       /(4x,'node',9(i3,'-b.c.',:)))

2001  format(10i8)

      end
