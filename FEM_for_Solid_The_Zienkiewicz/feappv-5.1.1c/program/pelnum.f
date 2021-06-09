!$Id:$
      subroutine pelnum(tx,iel,errck)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Get material set numbers for FEAPpv elements

!              Current FEAPpv Element Types
!                 Name     |     iel
!              ------------+-------------
!               Solid      |     -1
!               Truss      |     -2
!               Frame      |     -3
!               Plate      |     -4
!               Shell      |     -5
!               Membrane   |     -6
!               Thermal    |     -7
!               Convection |     -8
!               Point      |     -9

!      Inputs:
!         tx     - Name of element type requested

!      Outputs:
!         iel    - Element type for request
!         errck  - Flag, true if request found
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      character (len=15) :: tx

      logical       :: pcomp, errck
      integer       :: iel

      save

      errck = .false.

!     Register program element type names

      if    (pcomp(tx,'soli',4)) then
        iel = -1
      elseif(pcomp(tx,'trus',4)) then
        iel = -2
      elseif(pcomp(tx,'fram',4)) then
        iel = -3
      elseif(pcomp(tx,'plat',4)) then
        iel = -4
      elseif(pcomp(tx,'shel',4)) then
        iel = -5
      elseif(pcomp(tx,'memb',4)) then
        iel = -6
      elseif(pcomp(tx,'ther',4)) then
        iel = -7
      elseif(pcomp(tx,'conv',4)) then
        iel = -8
      elseif(pcomp(tx,'poin',4)) then
        iel = -9
      else
        errck = .true.
      endif

      end subroutine pelnum
