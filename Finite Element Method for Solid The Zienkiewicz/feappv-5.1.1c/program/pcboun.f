!$Id:$
      subroutine pcboun(td,x,id,ntyp,ndm,ndf,numnp,numprt,gap0,
     &                  vtype,prt,prth,aname)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Set boundary condition codes based on coordinate
!               inputs

!      Inputs:
!         td(*)     - Array containing coordinates and boundary
!                     restraint pattern
!         x(ndm,*)  - Nodal coordinates
!         ntyp(*)   - Node type ( < zero for inactive)
!         ndm       - Spatial dimension of mesh
!         ndf       - Number dof/node
!         numnp     - Number of nodes in mesh
!         numprt    - Print counter
!         gap0      - Search gap
!         vtype     - Replacement type: 'add' = accumulate; else reset.
!         prt       - Output generated results if true
!         prth      - Output title/header data if true
!         aname     - Header type (-B.C.)

!      Outputs:
!         id(ndf,*) - Generated boundary restraint codes
!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'iofile.h'

      character (len=6) :: pname
      character (len=4) :: vtype
      character         :: aname*(*)

      logical       :: prt,prth,clflg, pcomp
      integer       :: ndm,ndf,numnp,numprt, i,n,nbc, nipt
      real (kind=8) :: gmn, tmn, gap0

      integer       :: id(ndf,numnp),ntyp(*)
      real (kind=8) :: xmn(3),xmx(3), x(ndm,numnp),td(*)

      real (kind=8) :: dotx

      save

!     Find closest node to input coordinates

      pname = aname
      if(prt .and. numprt.le.0) then
        call prtitl(prth)
        write(iow,2000) (i,pname,i=1,ndf)
        if(ior.lt.0) write(*,2000) (i,pname,i=1,ndf)
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

!     Set search gap

      tmn = 0.0d0
      do i = 1,min(ndm,3)
        tmn = max(xmx(i) - xmn(i),tmn)
      end do ! i

      if(gap0.eq.0.0d0) then
        gmn = gmn + tmn*1.d-10
      else
        gmn = gmn + gap0
      endif

!     Set b.c. restraint codes

      if(clflg) then
        nipt = 0
        do nbc = 1,numnp
          if(ntyp(nbc).ge.0) then
            tmn = dotx(td(1),x(1,nbc),ndm)
            if(tmn.le.gmn) then
              nipt = nipt + 1
              if(pcomp(vtype,'add',3)) then
                do n = 1,ndf
                  id(n,nbc) = max(abs(id(n,nbc)),abs(nint(td(ndm+n))))
                end do
              else
                do n = 1,ndf
                  id(n,nbc) = abs(nint(td(ndm+n)))
                end do
              endif

!             Output current restraint codes set

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

        if(nipt.eq.0) then ! No data found
          write(  *,3000) pname
          write(iow,3000) pname
        endif
      endif ! clflg = .true.

!     Format

2000  format('  C o o r d i n a t e    N o d a l    V a l u e s'/
     &       /(4x,'Node',9(i3,a6,:))/(8x,9(i3,a6,:)))

2001  format(10i8)

3000  format(' --> WARNING: No nodes found for ',a,' data type')

      end subroutine pcboun
