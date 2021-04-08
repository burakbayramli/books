!$Id:$
      logical function savefl(tx)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Save data in specified file.
!               (teminates with: save,end).

!      Inputs:
!         tx        - Name of file for reads


!      Outputs:
!         savefl    - Status of mesh input
!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'iodata.h'
      include  'iofile.h'
      include  'iosave.h'

      character (len=21) :: fnams
      character (len=8)  :: fext
      character          :: tx*(*)

      logical       :: pcomp, lopen, lexist

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
          call plstop(.true.)
        endif
        call opnfil(fext,fnams,-1,ios,lopen)
        savefl =  .true.
      endif

2000  format('read,end')

3000  format(5x,' *ERROR* - Nested SAVE commands not allowed.')

      end function savefl
