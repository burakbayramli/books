!$Id:$
      subroutine pdelfl()

!      * * F E A P * * A Finite Element Analysis Program
!                        -      -       -        -

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Clean up old files by erasing temporary mesh input files

!      Inputs:
!         none

!      Outputs:
!         none
!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'comfil.h'
      include  'cornum.h'
      include  'iodata.h'

      character (len=128) :: fname
      character (len=4)   :: fext,ftyp(5)

      logical       :: exst,isopen
      integer       :: i,m,mmx

      data      ftyp /'sl0','bn0','an0','fr0','ds0'/

!     Check for maximum number of files active

      mmx = max(nsurf,nbouf,ndisf,nforf,nangf) - 1

!     Delete existing files

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

!     Delete 'feaploop' mesh blocks

      fname = ' '   ! clear name
      fname = 'feaploop000.0'
      do m = 0,9
        write(fname(13:13),'(i1)') m
        inquire(file=fname,exist=exst,opened=isopen)
        if(exst) then
          if(.not.isopen) then
            open (unit = ios, file = fname, status = 'old')
          endif
          close(unit = ios, status = 'delete')
        endif
      end do ! m

      end subroutine pdelfl
