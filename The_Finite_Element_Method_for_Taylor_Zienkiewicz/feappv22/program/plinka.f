c$Id:$
      subroutine plinka(fext,name)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Read data and save on a file for future processing

c      Inputs:
c         fext      - Name of file extender for save file

c      Outputs:
c         none      - Data saved to disk
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'comfil.h'
      include  'conval.h'
      include  'iodata.h'
      include  'iofile.h'
      include  'ioincl.h'
      include  'linka.h'
      include  'print.h'

      logical   pcomp,lsave,pinput,lopen
      integer   i,n
      character fnamr*18,fext*4,yyy*255,type*4,name*(*)
      real*8    td(1)

      save

      fnamr =  fsav
      call addext(fnamr,fext,18,4)
      inquire(unit=iop,opened=lopen)
      if(lopen) then
        write(*,*) 'PLINKA: UNIT',iop,' is already open'
        call plstop()
      endif
      call opnfil(fext,fnamr,-1,iop,lsave)

c     Save values of current parameters

      type = name
      write(iop,1002) type,fincld(isf),irecrd(isf),prt,prth
      write(iop,1001) vvv
      iclink = 0
  10  if(ior.gt.0) then
        read(ior,1000,err=901,end=902) record
        irecrd(isf) = irecrd(isf) + 1
      else
        read(  *,1000) record
      endif
      yyy = record
      do n = 1,255
        if(yyy(n:n).eq.'!') then
          yyy(n:255) = ' '
          go to 20
        endif
      end do
      n = 255
  20  do i = n,1,-1
        if(yyy(i:i).ne.' ') go to 30
      end do ! i
      i = 1
  30  write(iop,1000) yyy(1:i)

      if(.not.pcomp(yyy,'        ',8)) then
        iclink = iclink + 1
        go to 10
      else
        close(iop)
      endif
      return

c     Read Error

 901  call errclr('PLINKA')
      return

c     Read EOF

 902  if(ior.eq.icf) then
        lsave = pinput(td,1)
        close(iop)
      else
        call endclr('PLINKA',yyy)
      endif

c     Format

1000  format (a)
1001  format (1p,4e20.12)
1002  format (a4,2x,a12,i8,2l5)

      end
