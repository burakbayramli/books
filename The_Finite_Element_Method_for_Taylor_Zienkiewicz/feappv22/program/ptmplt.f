c$Id:$
      subroutine ptmplt(ftyp, ttim, tpl,ntplts, ntstep)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Output information to time history files: F77 Version

c      Inputs:
c         ftyp(*)   - Type of data
c         ttim      - Solution time for data
c         tpl(*)    - Time history data
c         ntplts    - Number of time history data items
c         ntstep    - Indicator for first time step

c      Outputs:
c         none      - Data saved to disk
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'pdata2.h'
      include  'iodata.h'

      logical   exst
      character ftyp*(*), fnamr*24, fext*4
      integer   ntplts,ntstep, n,nn,ntp,ntl, m
      real*8    ttim, tpl(ntplts)
      real*8    tdum

      save

c     Set file name for output of time history data (Get from 'feapname')

      open(ios,file='feapname',status='old')
      read(ios,'(48x,a)') fnamr
      close(ios)

c     Extract name for file

      do n = 1,17
        if(fnamr(n:n).eq.'.') then
          fnamr( n:17) = fnamr(n+1:18)
          fnamr(18:18) = ' '
        endif
      end do

c     Locate character where added letter 'A' to 'J' to be added

      n = 18
      do while(fnamr(n:n).eq.' ' .and. n.gt.0)
        n = n - 1
      end do ! while

      nn = 0
      do ntp = 1,ntplts,20
        nn             = nn + 1
        fnamr(n+1:n+1) = char(96+nn)
        ntl            = min(ntp+19,ntplts)

c       Add extender

        fext  =  ftyp
        call addext(fnamr,fext,18,4)

c       Check if file exists, if it does delete it

        inquire(file=fnamr,exist=exst)
        if(exst.and.ntstep.eq.1) then
          open(unit=24, file = fnamr, form = 'formatted',
     &         access = 'sequential', status = 'unknown')
          close(24,status='delete')
        end if

c       Open file and find end: F77 version

        open(unit=24, file = fnamr, form = 'formatted',
     &       access = 'sequential', status = 'unknown')
  10    read(24,2000,end=11) tdum
        go to 10

c       Add line of data

  11    write(24,2000) ttim,(tpl(m),m=ntp,ntl)
        close(24)

      end do ! ntp

c     Format

2000  format(1p,21e12.4)

      end
