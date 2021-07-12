c$Id: pltcur.f,v 1.1 2000/08/24 20:49:59 rlt Exp $
      subroutine pltcur()

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--+---------+---------+---------+---------+---------+---------+-]
c      Purpose: Turn cursor on for text inputs

c      Inputs:

c      Outputs:
c-----[--+---------+---------+---------+---------+---------+---------+-]

      use      DFLIB

      implicit none

      integer  status

      status = displaycursor($GCURSORON)

      end
