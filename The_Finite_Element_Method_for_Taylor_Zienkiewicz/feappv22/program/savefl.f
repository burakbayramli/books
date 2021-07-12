c$Id:$
      logical function savefl(tx)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Save data in specified file.
c               (teminates with: save,end).

c      Inputs:
c         tx        - Name of file for reads


c      Outputs:
c         savefl    - Status of mesh input
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'iodata.h'
      include  'iofile.h'
      include  'iosave.h'

      logical   pcomp, lopen, lexist
      character tx*(*), fnams*21, fext*8

      save

      if(pcomp(tx,'end',3)) then
        inquire(file=fnams,opened=lopen,exist=lexist)
        if(lexist.and.lopen.and.lsave) then
          backspace lfile
          write(lfile,2000)
          close(lfile)
        endif
        savefl = .false.
      else
        fnams  = tx
        fext   = tx
        inquire(unit=ios,opened=lopen)
        if(lopen) then
          write(iow,3000)
          call plstop()
        endif
        call opnfil(fext,fnams,-1,ios,lopen)
        savefl =  .true.
      endif

2000  format('read,end')

3000  format(5x,' *ERROR* - Nested SAVE commands not allowed.')

      end
