!$Id:$
      subroutine pltcur()

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Modification log                                Date (dd/mm/year)
!       Original version                                    01/11/2006
!       1. Change DFLIB to IFQWIN                           10/04/2014
!-----[--+---------+---------+---------+---------+---------+---------+-]
!      Purpose: Turn cursor on for text inputs

!      Inputs:

!      Outputs:
!-----[--+---------+---------+---------+---------+---------+---------+-]
      use      IFQWIN

      implicit none

      integer      :: vstatus

      vstatus = displaycursor($GCURSORON)

      end subroutine pltcur
