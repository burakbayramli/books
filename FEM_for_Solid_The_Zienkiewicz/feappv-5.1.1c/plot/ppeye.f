!$Id:$
      subroutine ppeye(v1, v2)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Display viewpoint as "eye" on screen

!      Inputs:
!         v1,v2      - Screen x,y coordinates for view point

!      Outputs:
!         none       - Plot outputs to screen/file
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      real (kind=8) :: v1,v2

      save

      call dplot( v1              , v2 + 0.0077325d0 , 3)
      call dplot( v1 - 0.015465d0 , v2               , 2)
      call dplot( v1              , v2 - 0.0077325d0 , 2)
      call dplot( v1 + 0.015465d0 , v2               , 2)
      call dplot( v1              , v2 + 0.0077325d0 , 2)
      call dplot( v1 - 0.0077325d0, v2               , 2)
      call dplot( v1              , v2 - 0.0077325d0 , 2)
      call dplot( v1 + 0.0077325d0, v2               , 2)
      call dplot( v1              , v2 + 0.0077325d0 , 2)

      end subroutine ppeye
