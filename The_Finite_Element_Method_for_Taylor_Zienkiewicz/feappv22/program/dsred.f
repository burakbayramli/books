c$Id:$
      function dsred(au,ad,jh)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Reduce diagonal in symmetric triangular decomposition

c      Inputs:
c         au(*)  - Upper terms in column
c         ad(*)  - Reduced diagonals of previous equations
c         jh     - Length of column

c      Outputs:
c         dsred  - reduced diagonal for current equation
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer   j,jh
      real*8    dsred, ud, dj, au(jh),ad(jh)

      dj = 0.0d0
      do j = 1,jh
        ud    = au(j)*ad(j)
        dj    = dj + au(j)*ud
        au(j) = ud
      end do
      dsred = dj

      end
