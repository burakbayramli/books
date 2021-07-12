c$Id:$
      subroutine ublk(td,nn,nr,ns,nt,xl,x,ixl,ix,dr,ds,dt,
     &                ni,ne,ndm,nen1,ma,ctype,prt, isw)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: User interface to generate a block of node/elements

c      Inputs:
c         td(15)     - Parameters from 'BLOC'k read
c         nn         - Number of block nodes
c         nr         - Number of increments in r-direction
c         ns         - Number of increments in s-direction
c         nt         - Number of increments in t-direction
c         xl(3,*)    - Nodal coordinates for block
c         ixl(*)     - Nodes connected to block
c         dr         - Increment in r-direction
c         dr         - Increment in s-direction
c         dt         - Increment in t-direction
c         ni         - Initial node number
c         ne         - Initial element number
c         ndm        - Spatial dimension of mesh
c         nen1       - Dimension of ix array
c         ma         - Material set number for block
c         ctype      - Character array defining block node coordinate
c                      system (cartesian, polar, spherical)
c         prt        - Output if true
c         isw        - Switch: 1 - count nodes/elements; 2 - input mesh

c      Outputs:
c         x(ndm,*)   - Nodal coordinates genterated by block
c         ix(nen1,*) - Element nodal connection list
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'iofile.h'

      logical   prt
      character ctype*15
      integer   ixl(*),ix(*)
      integer   nn,nr,ns,nt,ni,ne,ndm,nen1,ma,isw
      real*8    dr,ds,dt,xl(*),x(*),td(15)

      save

      if(ior.lt.0) write(*,2000)
      write(iow,2000)

2000  format(' *ERROR* No user block generator loaded')

      end
