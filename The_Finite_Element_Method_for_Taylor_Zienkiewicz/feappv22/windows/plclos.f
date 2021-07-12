c$Id: plclos.f,v 1.1 2000/08/24 20:49:59 rlt Exp $
      subroutine plclos()

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--+---------+---------+---------+---------+---------+---------+-]
c      Purpose: Close plot device

c      Inputs:
c         none

c      Outputs:
c         none      - Returns command outputs to text device
c-----[--+---------+---------+---------+---------+---------+---------+-]

      implicit  none

      include  'print.h'

      integer   status,vtxwin

      save

c     Close plot device

      fopn = .false.

      status = vtxwin()

      end
