c$Id:$
      subroutine ppeye(v1, v2)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Display viewpoint as "eye" on screen

c      Inputs:
c         v1,v2      - Screen x,y coordinates for view point

c      Outputs:
c         none       - Plot outputs to screen/file
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      real*8    v1,v2

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

      end
