!$Id:$
      subroutine pcprop(td,x,icd,ntyp,ndm,ndf,numnp,numprt,prt,prth)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Input proportional load numbers at specified coordinates

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
!         icd(ndf,*) - Generated force or displacements
!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'iofile.h'

      logical       :: prt,prth, clflg
      integer       :: ndm,ndf,numnp,numprt, n,nbc
      real (kind=8) :: xmn, tmn

      integer       :: ntyp(*), icd(ndf,*)
      real (kind=8) :: x(ndm,numnp),td(*)

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

!     Set proportional load numbers

      if(clflg) then
        do n = 1,ndf
          icd(n,nbc) = nint(td(ndm+n))
        end do

!       Output current restraint codes set

        if(prt) then
          write(iow,2001) nbc,(icd(n,nbc),n=1,ndf)
          if(ior.lt.0) then
            write(*,2001) nbc,(icd(n,nbc),n=1,ndf)
          endif
          numprt = numprt - 1
        endif
      endif

!     Formats

2000  format('  C o o r d i n a t e    N o d a l    V a l u e s'/
     &      /4x,'Node',6(i3,'-Propld'):/(8x,6(i3,'-Propld')))

2001  format(i8,6i10:/(8x,6i10))

      end subroutine pcprop
