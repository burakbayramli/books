c$Id:$
      subroutine vecp (e1,e2,e3)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Vector (cross) product of two 3-d vectors

c      Inputs:
c        e1(3),e2(3) - vectors to be multiplied

c      Outputs:
c        e3(3)       - vector product ( e3 = e1xe2 )
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      real*8    e1(3),e2(3),e3(3)

      save

      e3(1) = e1(2)*e2(3) - e1(3)*e2(2)
      e3(2) = e1(3)*e2(1) - e1(1)*e2(3)
      e3(3) = e1(1)*e2(2) - e1(2)*e2(1)

      end
