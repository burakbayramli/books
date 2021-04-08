!$Id:$
      subroutine ublk(ntyp,nn,nr,ns,nt,xl,x,ixl,ix,dr,ds,dt,
     &                ni,ne,ndm,nen1,ma,ctype,prt, isw)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: User interface to generate a block of node/elements

!      Inputs:
!         ntyp       - Block type number
!         nn         - Number of block nodes
!         nr         - Number of increments in r-direction
!         ns         - Number of increments in s-direction
!         nt         - Number of increments in t-direction
!         xl(3,*)    - Nodal coordinates for block
!         ixl(*)     - Nodes connected to block
!         dr         - Increment in r-direction
!         dr         - Increment in s-direction
!         dt         - Increment in t-direction
!         ni         - Initial node number
!         ne         - Initial element number
!         ndm        - Spatial dimension of mesh
!         nen1       - Dimension of ix array
!         ma         - Material set number for block
!         ctype      - Character array defining block node coordinate
!                      system (cartesian, polar, spherical)
!         prt        - Output if true
!         isw        - Switch: 1 - count nodes/elements; 2 - input mesh

!      Outputs:
!         x(ndm,*)   - Nodal coordinates genterated by block
!         ix(nen1,*) - Element nodal connection list
!         ni         - Final node number in block
!         ne         - Final element number in block
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'iofile.h'
      include  'ublk1.h'  ! Contains ublkdat(3,50), ublknum

      character (len=15) :: ctype

      logical       :: prt
      integer       :: ntyp,nn,nr,ns,nt,ixl(*),ix(*),ni,ne
      integer       :: ndm,nen1,ma,isw
      real (kind=8) :: dr,ds,dt,xl(*),x(*)

      save

      if(ior.lt.0) write(*,2000)
      write(iow,2000)

2000  format(' *ERROR* No user block generator loaded')

!     Set ni to last node in block
      ni = ni - 1

!     Set ne to last element in block
      ne = ne - 1

      end subroutine ublk
