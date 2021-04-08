!$Id:$
      subroutine pline(x,ie,ix,id,ip,numnp,numel,ndm,
     &                 nen1,nen,nie,ct,isw)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Mesh plot and outline driver

!      Inputs:
!         x(ndm,*)  - Nodal coordinates
!         ie(nie,*) - Assembly data for material sets
!         ix(nen1,*)- Element nodal connection list
!         id(*)     - Number of elements attached to nodes
!         ip(*)     - Sorted element order for each quadrant
!         numnp     - Number of nodes in mesh
!         numel     - Number of elements in mesh
!         ndm       - Dimesion of x array
!         nen1      - Dimesion of ix array
!         nie       - Dimesion of ie array
!         ct(*)     - Color changing
!         isw       - Flag, Plot mesh if true, otherwise outline

!      Outputs:
!         none      - Plot output to screen/file
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'pointer.h'
      include  'comblk.h'

      logical       :: isw, setvar,palloc
      integer       :: numnp,numel,ndm,nen1,nen,nie
      integer       :: ie(*),ix(*),id(*),ip(*)
      real (kind=8) :: x(*),ct(*)

      save

!     Determine necessary storage for mesh lines and allocate storage

      call xcompp(ie,ix,id,nie,nen1,nen,numnp,numel)

      setvar = palloc(112,'TEMP2',id(numnp+1),1)

!     Draw mesh

      call xpline(x,ie,ix,id,mr(np(112)),ip,numnp,numel,ndm,
     &            nen1,nen,nie,ct(1),isw)

!     Delete storage for mesh lines

      setvar = palloc(112,'TEMP2',0,1)

      end subroutine pline
