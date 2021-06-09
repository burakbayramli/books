!$Id:$
      subroutine pfboun(td,x,f,ntyp,ndm,ndf,numnp,numprt,vtype,prt,prth)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Input nodal force or displacement values using
!               specified coordinates

!      Inputs:
!         td(*)    - Coordinates and values for point
!         x(ndm,*) - Nodal coordinates of mesh
!         ntyp(*)  - Node type (negative for inactive)
!         ndm      - Spatial dimension of mesh
!         ndf      - Number dof/node
!         numnp    - Number of nodes in mesh
!         numprt   - Print counter
!         prt      - Output generated values if true
!         prth     - Output title/header data if true

!      Outputs:
!         f(ndf,*) - Generated force or displacements
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'iofile.h'

      character (len=4) :: vtype

      logical       :: prt,prth, clflg, pcomp
      integer       :: ndm,ndf,numnp,numprt, n,nbc
      real (kind=8) :: xmn, tmn

      integer       :: ntyp(*)
      real (kind=8) :: x(ndm,numnp),td(*),f(ndf,numnp)

      real (kind=8) :: dotx

      save

!     Find closest node to input coordinates

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

!     Set b.c. forced values

      if(clflg) then
        if(pcomp(vtype,'add',3)) then
          do n = 1,ndf
            f(n,nbc) = f(n,nbc) + td(ndm+n)
          end do
        else
          do n = 1,ndf
            f(n,nbc) = td(ndm+n)
          end do
        endif

!       Output current restraint codes set

        if(prt) then
          write(iow,2001) nbc,(td(ndm+n),n=1,ndf)
          if(ior.lt.0) then
            write(*,2001) nbc,(td(ndm+n),n=1,ndf)
          endif
          numprt = numprt - 1
        endif
      endif

!     Format

2000  format('  C o o r d i n a t e    N o d a l    F o r c e d'/
     &       /3x,'Node',6(i2,'Force/Disp':)/(7x,6(i2,'Forcd/Disp':)))

2001  format(i7,1p,6e12.4/(7x,1p,6e12.4))

      end subroutine pfboun
