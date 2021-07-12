c$Id: pstart.f,v 1.1 2000/08/24 20:49:59 rlt Exp $
      subroutine pstart()

c      * * F E A P * * A Finite Element Analysis Program
c                        -      -       -        -
c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

      implicit  none

      include  'pdata2.h'

      interface
        logical(4)  function initialsettings
        end function
      end interface

c     Graphics Driver for PC versioni

      idev = 2

c     Start Windows

      call pwopn()

      end

      logical(4) function initialsettings ( )

      use       DFLIB

      implicit  none

      type(qwinfo) winfo
      integer      status

      save

c     Maximize Frame

      winfo.type = qwin$max
      status     = setwsizeqq(qwin$framewindow,winfo)

      initialsettings = .true.

      end function
