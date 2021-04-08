!$Id:$
      subroutine writer(ctc,b,nneq)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Save nodal displacement and stress values for later
!               use

!      Inputs:
!         ctc       - Name of array, file rewind, or file name
!         b(*)      - Array to write
!         numnp     - Length of array to write

!      Outputs:
!         none
!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'cdata.h'
      include  'comfil.h'
      include  'fdata.h'
      include  'iodata.h'
      include  'iofile.h'
      include  'p_point.h'
      include  'p_int.h'
      include  'pdata3.h'
      include  'prstrs.h'
      include  'strnum.h'
      include  'tdata.h'
      include  'comblk.h'

      character (len=15) :: fname
      character (len=4)  :: ct
      character (len=1)  :: y
      character          :: ctc*(*)

      logical       :: lflg,pcomp,cinput
      integer       :: nneq
      real (kind=8) :: b(nneq)

      save

      data      lflg/.false./

!     Set name

      ct = ctc

!     Save current eigenpairs

      if(pcomp(ct,'eige',4)) then

        if(lflg) then
          call peigsv(fname,1)
        else
          go to 920
        endif

!     Save current displacement state

      elseif(pcomp(ct,'disp',4)) then
        if(lflg) then
          write(iwd,err=910) ct,ttim
          write(iwd,err=910) b
        else
          go to 920
        endif

!     Save current nodal stress state

      elseif(pcomp(ct,'stre',4)) then
        if(lflg) then
          if(fl(11)) then
            write(iwd,err=910) ct,ttim
            fp(1) = nph -1 + npstr*numnp
            write(iwd,err=910) istv,(hr(point),point=nph,fp(1))
          else
            write(iow,2004)
            if(ior.lt.0) write(*,2004)
          endif
        else
          go to 920
        endif

!     Rewind file

      elseif(pcomp(ct,'wind',4)) then
        if(lflg) then
          rewind iwd
        endif

!     Close file

      elseif(pcomp(ct,'clos',4)) then
        close(iwd)
        lflg = .false.

!     Set filename

      else
        fname = ct
        inquire(file=fname,exist=lflg)

!       This is a old file name

        if(lflg) then
          if(ior.lt.0) then
            write(*,2002) fname
!10         read (*,1000,err=11,end=12) y
10          if(.not.cinput()) then
              goto 12
            end if
            y = record(1:1)
            goto  13
!11         call  errclr ('WRITER')
            call  errclr ('WRITER')
            goto  10
12          call  endclr ('WRITER',y)
13          if(y.ne.'y' .or. y.ne.'Y') return
          else
            write(iow,2003) fname
            call plstop(.true.)
          endif
          open(iwd,file=fname,status='old',form='unformatted')
          rewind iwd

!       This is a new file name

        else
          if(ior.lt.0) write(*,2001) fname
          write(iow,2001) fname
          open(iwd,file=fname,status='new',form='unformatted')
        endif
        lflg = .true.
      endif
      return

!     Error messages

910   if(ior.gt.0) then
        write(iow,3001) ct
        call plstop(.true.)
      else
        write(*,3001)
      endif
      return

920   if(ior.gt.0) then
        write(iow,3002)
        call plstop(.true.)
      else
        write(*,3002)
      endif

!     Formats

!1000  format(a1)

2001  format('   Output file for write operations is named ',a)

2002  format(' *WARNING* File ',a,' exists. Erase? (y or n) >',$)

2003  format(' *ERROR* File ',a,' exists.')

2004  format(' *ERROR* Nodal stresses do not exist for tape write')

3001  format(' *ERROR* On a tape write command for ',a4)

3002  format(' *ERROR* No write file is open.')

      end subroutine writer
