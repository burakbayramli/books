c$Id:$
      subroutine pelnum(tx,iel,errck)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Get material set numbers for FEAPpv elements

c              Current FEAPpv Element Types
c                 Name     |     iel
c              ------------+-------------
c               Solid      |     -1
c               Truss      |     -2
c               Frame      |     -3
c               Plate      |     -4
c               Shell      |     -5
c               Membrane   |     -6
c               Thermal    |     -7
c               Convection |     -8

c      Inputs:
c         tx     - Name of element type requested

c      Outputs:
c         iel    - Element type for request
c         errck  - Flag, true if request found
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      logical   pcomp, errck
      character tx*15
      integer   iel

      save

      errck = .false.

c     Register program element type names

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
      else
        errck = .true.
      endif

      end
