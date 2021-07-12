c$Id:$
      subroutine reader(ctc,b,nneq)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Read nodal displacement and stress values from disk

c      Inputs:
c         ct        - Name of array, file rewind, or file name
c         b(*)      - Array to read
c         numnp     - Length of array to read

c      Outputs:
c         none
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'cdata.h'
      include  'fdata.h'
      include  'iodata.h'
      include  'iofile.h'
      include  'pdata3.h'
      include  'pointer.h'
      include  'prstrs.h'
      include  'strnum.h'
      include  'tdata.h'
      include  'comblk.h'

      logical   lflg,setvar,palloc,pcomp
      integer   nneq, npp, nty
      character cc*4,ctc*(*),ct*4,fname*15
      real*8    b(nneq)

      save

      data      lflg /.false./

c     Set name

      ct = ctc

c     Read old eigenpairs

      if(pcomp(ct,'eige',4)) then

        if(lflg) then
          call peigsv(fname,2)
        else
          go to 940
        endif

c     Read displacement state from disk

      elseif(pcomp(ct,'disp',4)) then

        if(lflg) then
          read(ird,end=920,err=930) cc,ttim
          if(.not.pcomp(cc,ct,4)) go to 910
          read(ird,end=920,err=930) b
        else
          go to 940
        endif

c     Read nodal stress state from disk

      elseif(pcomp(ct,'stre',4)) then
        if(lflg) then
          read(ird,end=920,err=930) cc,ttim
          if(.not.pcomp(cc,ct,4)) go to 910
          if(plfl) setvar = palloc( 58,'NDNP ',numnp*npstr,2)
          nph = np(58)
          npp = nph - 1 + numnp*npstr
          read(ird,end=920,err=930) istv,(hr(nty),nty=nph,npp)
          fl(11) = .true.
        else
          go to 940
        endif

c     Rewind file

      elseif(pcomp(ct,'wind',4)) then
        if(lflg) rewind ird

c     Backspace file

      elseif(pcomp(ct,'back',4)) then
        if(lflg) then
          backspace ird
          backspace ird
        endif

c     Close file

      elseif(pcomp(ct,'clos',4)) then
        if(lflg) close(ird)
        if(ior.gt.0) write(iow,2002) fname
        if(ior.lt.0) write(  *,2002) fname
        lflg = .false.

c     Open file set with name 'ct'

      else
        if(.not.lflg) then
          fname = ct
          inquire(file=fname,exist=lflg)
          if(lflg) then
            write(iow,2001) fname
            if(ior.lt.0) write(*,2001) fname
            open(ird,file=fname,status='old',
     &           form='unformatted',err=930)
          else
            if(ior.gt.0) then
              write(iow,3000) fname
              call plstop()
            endif
            write(*,3000) fname
          endif
        else
          write(iow,3005) fname
          if(ior.lt.0) write(*,3005) fname
        endif
      endif
      return

c     Error messages

910   if(ior.gt.0) then
        write(iow,3001) ct,cc
        call plstop()
      endif
      write(*,3001) ct,cc
      backspace ird
      return

920   if(ior.gt.0) then
        write(iow,3002) ct
        call plstop()
      endif
      write(*,3002) ct
      return

930   if(ior.gt.0) then
        write(iow,3003) ct
        call plstop()
      endif
      write(*,3003) ct
      return

940   if(ior.gt.0) then
        write(iow,3004)
        call plstop()
      endif
      write(*,3004)

c     Formats

2001  format('   File name for a read has been set to ',a)

2002  format('   File name ',a,' closed.')

3000  format(' *ERROR* File ',a,' does not exist')

3001  format(' *ERROR* READ requested ',a4,' but found ',a4)

3002  format(' *ERROR* End of file on a read command for ',a4)

3003  format(' *ERROR* on a read command for ',a4)

3004  format(' *ERROR* No read file is open.')

3005  format(' *ERROR* File ',a,' is open.  Close before new',
     1       ' file specified.')

      end
