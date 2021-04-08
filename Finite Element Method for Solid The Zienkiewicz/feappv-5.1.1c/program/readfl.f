!$Id:$
      logical function readfl(tx)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Read data from specified file.
!               (teminates with: read,end).

!      Inputs:
!         tx        - Name of file for reads

!      Outputs:
!         readfl    - Status of mesh input
!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'iofile.h'
      include  'iosave.h'

      character (len=21) :: fnamr
      character (len=8)  :: fext
      character          :: tx*(*)

      logical       :: pcomp, lopen, lexist
      integer       :: isfile

      save

!     Set the default for returning error or close

      readfl = .false.

!     Close file and reset logical unit number

      if(pcomp(tx,'end',3)) then
        inquire(file=fnamr,opened=lopen,exist=lexist)
        if(lexist.and.lopen) close(lfile)
        if(lread) then
          ior    = isfile
          lread  = .false.
          readfl = .false.
        endif

!     Open file and set new logical unit number

      else
        fnamr    = tx
        fext     = tx
        call opnfil(fext,fnamr,-2,lfile,lread)
        if(.not.lread) return
        isfile = ior
        ior    = lfile
        readfl = .true.
      endif

      end function readfl
