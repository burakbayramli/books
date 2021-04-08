!$Id:$
      subroutine pincld(fname)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Control file for I/O from include files

!      Inputs:
!         fname    - Name of file to read include data from
!                    N.B. If fname = 'end', closes include file

!      Outputs:
!         none
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'comfil.h'
      include  'iodata.h'
      include  'ioincl.h'
      include  'iofile.h'
      include  'iosave.h'

      character (len=21) :: fnamr
      character (len=15) :: dnam
      character (len=8)  :: fext
      character          :: fname*(*)

      logical       :: errc,lopen,pcomp

      save

!     Perform inputs from an include file

      dnam = fname

      if(pcomp(dnam,'end',3)) then
        inquire(file=fnamr,opened=lopen,exist=errc)
        if(errc.and.lopen) then
          close(icf)
        else
          inquire(unit=icf,opened=lopen,exist=errc)
          if(errc.and.lopen) then
            close(icf)
          endif
        endif
        if(isf.gt.1) then
          fincld(isf) = ' '
        endif
        isf   = max(1,isf-1)
        ior   = isfile(isf)
        icf   = max(icl,abs(ior))
        lread = .false.
      else
  1     inquire(unit=icf,opened=lopen)
        if(lopen) then
          icf = icf + 1
          go to 1
        endif
        fnamr =  dnam
        fext  =  dnam(1:8)
        call opnfil(fext,fnamr,-2,icf,lread)
        if(.not.lread) then
          if(ior.lt.0) return
          call plstop(.true.)
        endif
        isfile(isf) = ior
        ior         = icf
        isf         = isf + 1
        fincld(isf) = fnamr
        irecrd(isf) = 0
      endif

      end subroutine pincld
