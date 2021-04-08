!$Id:$
      subroutine plinka(fext,fname)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Read data and save on a file for future processing

!      Inputs:
!         fext      - Name of file extender for save file

!      Outputs:
!         none      - Data saved to disk
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'comfil.h'
      include  'conval.h'
      include  'iodata.h'
      include  'iofile.h'
      include  'ioincl.h'
      include  'linka.h'
      include  'print.h'

      character (len=256) :: yyy
      character (len=128) :: fnamr
      character (len=4)   :: fext,vtype
      character           :: fname*(*)

      logical       :: pcomp,lsave,pinput,lopen,cinput, norec
      integer       :: i,n
      real (kind=8) :: td(1)

      save

      norec = .true.
      fnamr =  fsav
      call addext(fnamr,fext,18,4)
      inquire(unit=iop,opened=lopen)
      if(lopen) then
        write(*,*) 'PLINKA: UNIT',iop,' is already open'
        call plstop(.true.)
      endif
      call opnfil(fext,fnamr,-1,iop,lsave)

!     Avoid warning for 'umani' commands

      if(fext(1:1).eq.'u' .and. fext(3:3).eq.'a') norec = .false.

!     Save values of current parameters

      vtype = fname
      write(iop,1002) vtype,fincld(isf),irecrd(isf),prt,prth
      write(iop,1001) vvv
      iclink = 0
  10  if(ior.gt.0) then
        read(ior,1000,err=901,end=902) record
        irecrd(isf) = irecrd(isf) + 1
      else
!        read(  *,1000) record
        if(.not.cinput()) then
          write(*,*) 'Error in CINPUT in PLINKA'
        end if
      endif
      yyy = record
      do n = 1,256
        if(yyy(n:n).eq.'!') then
          yyy(n:256) = ' '
          go to 20
        endif
      end do
      n = 256
  20  do i = n,1,-1
        if(yyy(i:i).ne.' ') then
          norec = .false.
          go to 30
        endif
      end do ! i
      i = 1
  30  write(iop,1000) yyy(1:i)

      if(.not.pcomp(yyy,'        ',8)) then
        iclink = iclink + 1
        go to 10
      else
        write(iop,'(a)') ' ' ! Extra blank line for ndf > 13
        close(iop)
        if(norec) then
          write(  *,4001) fext(1:4)
          write(iow,4001) fext(1:4)
        endif

      endif
      return

!     Read Error

 901  call errclr('PLINKA')
      return

!     Read EOF

 902  if(ior.eq.icf) then
        lsave = pinput(td,1)
        close(iop)
      else
        call endclr('PLINKA',yyy)
      endif

!     Format

1000  format (a)
1001  format (1p,4e20.12)
1002  format (a4,2x,a12,i8,2l5)

4001  format(' *WARNING* PLINKA: No data found for a -> ',a4,
     &       ' <- data set.')

      end subroutine plinka
