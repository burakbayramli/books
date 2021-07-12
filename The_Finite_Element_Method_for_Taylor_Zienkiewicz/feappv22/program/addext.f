c$Id:$
      subroutine addext(fnam,fext,ifnam,ifext)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose:       Add extender to file name for disk i/o

c      Inputs:
c        fname(*)   - File name without extender
c        fext(*)    - Extender
c        ifnam      - Length of 'fname'
c        ifext      - Length of 'fext'

c      Outputs:
c        fname(*)   - File name with added  '.' and 'fext'
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer   ipos, iposl, iposx, ifnam,ifext
      character fnam*(*),fext*(*)

      iposl = ipos(fnam,ifnam) + 1
      iposx = ipos(fext,ifext)

      fnam(iposl:ifnam)         = '. '
      fnam(iposl+1:iposl+iposx) = fext(1:iposx)

      end
