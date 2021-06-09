!$Id:$
      subroutine setparam(par, pvalue, prt)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Command language instruction subprogram: Part 6
!               Set parameter 'par' to 'pvalue'.

!      Inputs:
!        par     - Parameter name to set
!        pvalue   - Value to assign
!        prt     - Echo value set if true

!      Outputs:
!        none
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'comfil.h'

      character (len=15) :: pname
      character          :: par*(*)

      logical       :: prt, redo, pconset
      real (kind=8) ::  pvalue

!     Put data into 'record'

      pname  = par
      record = ' '
      record( 1:14) = pname(1:14)
      record(15:15) = '='
      write(record(16:30),'(1p,1e15.7)') pvalue

      redo = pconset(prt)

      end subroutine setparam
