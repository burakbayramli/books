c$Id:$
      logical function cksep(x1)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Check for existence of separator characters in data.
c               Separators are blank, comma, or equal sign.

c      Inputs:
c         x1  -  Character to check

c      Outputs:
c         cksep - True of character is a valid separator; else false.
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      character x1*1

      cksep = (x1.eq.' ') .or. (x1.eq.',') .or. (x1.eq.'=')

      end
