c$Id:$
      subroutine pdelfl()

c      * * F E A P * * A Finite Element Analysis Program
c                        -      -       -        -

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Clean up old files by erasing temporary mesh input files

c      Inputs:
c         none

c      Outputs:
c         none
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'comfil.h'
      include  'cornum.h'
      include  'iodata.h'

      character fname*21,fext*4,ftyp(5)*4
      logical   exst
      integer   i,m,mmx

      data      ftyp /'sl0','bn0','an0','fr0','ds0'/

c     Check for maximum number of files active

      mmx = max(nsurf,nbouf,ndisf,nforf,nangf) - 1

c     Delete existing files

      do m = 0,mmx

        do i = 1,5
          fname = fsav
          fext  = ftyp(i)
          if(m.le.9) then
            write(fext(3:3),'(i1)') m
          else
            write(fext(2:3),'(i2)') m
          endif
          call addext(fname,fext,21,4)
          inquire(file=fname,exist=exst)
          if(exst) then
            open (unit = ios, file = fname, status = 'old')
            close(unit = ios, status = 'delete')
          endif
        end do
      end do

      end
