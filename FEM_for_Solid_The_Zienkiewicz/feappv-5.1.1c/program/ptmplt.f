!$Id:$
      subroutine ptmplt(ftyp, ttim, tpl,ntplts, ntstep)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Output information to time history files: F77 Version

!      Inputs:
!         ftyp(*)   - Type of data
!         ttim      - Solution time for data
!         tpl(*)    - Time history data
!         ntplts    - Number of time history data items
!         ntstep    - Indicator for first time step

!      Outputs:
!         none      - Data saved to disk
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'comfil.h'
      include  'pdata2.h'
      include  'iodata.h'

      character (len=128) :: fnamr
      character (len=8)   :: fext
      character           :: ftyp*(*)

      logical       :: exst
      integer       :: ntplts,ntstep, n,nn,ntp,ntl, m, ipos
      real (kind=8) :: ttim, tpl(ntplts)
      real (kind=8) :: tdum

      save

!     Set file name for output of time history data (Get from 'feapname')

      fnamr = fplt

!     Extract name for file

      n = index(fnamr,'.')
      if(n.gt.0) then
        fnamr(n:128) = ' '
      endif

!     Locate character where added letter 'A' to 'J' to be added

!     n = 128
!     do while(fnamr(n:n).eq.' ' .and. n.gt.0)
!       n = n - 1
!     end do ! while

      n = ipos(fnamr,128)
      nn = 0
      do ntp = 1,ntplts,20
        nn             = nn + 1
        fnamr(n+1:n+1) = char(96+nn)
        ntl            = min(ntp+19,ntplts)

!       Add extender

        fext  =  ftyp
        call addext(fnamr,fext,128,8)

!       Check if file exists, if it does delete it

        inquire(file=fnamr,exist=exst)
        if(exst.and.ntstep.eq.1) then
          open(unit=24, file = fnamr, form = 'formatted',
     &         access = 'sequential', status = 'unknown')
          close(24,status='delete')
        end if

!       Open file and find end: F77 version

        open(unit=24, file = fnamr, form = 'formatted',
     &       access = 'sequential', status = 'unknown')
  10    read(24,2000,end=11) tdum
        go to 10

!       Add line of data

  11    backspace(24)
        write(24,2000) ttim,(tpl(m),m=ntp,ntl)
        close(24)

      end do ! ntp

!     Format

2000  format(1p,21e12.4)

      end subroutine ptmplt
