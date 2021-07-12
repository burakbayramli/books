c$Id:$
      subroutine opnfil(mac,name,iopl,ios,exst)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Open file ios for FEAPpv I/O operations

c      Inputs:
c         mac  - Name for report
c         name - Name of file to open
c         iopl - Indicator on type of file to open or report
c         ios  - Logical unit number to assign to file

c      Outputs:
c         exst - Flag, true if file exists
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'iofile.h'

      logical   exst
      character mac*(*),name*(*),y*1
      integer   iopl,ios

      save

1     inquire(file=name,exist=exst)
      if(exst) then
        if(iopl.eq.1) then
          write(*,2000) mac
10        read (*,1000,err=11,end=12) y
          goto  13
11        call  errclr ('OPNFIL')
          goto  10
12        call  endclr ('OPNFIL',y)
13        if(y.ne.'y'.and.y.ne.'Y') then
            mac = '0'
            return
          endif
        endif
        if( abs(iopl).eq.3 ) then
          open(ios,file=name,status='old',form='unformatted')
        else
          open(ios,file=name,status='old')
        endif
      else
        if(iopl.eq.2) then
          write(*,2001) mac
          return
        elseif(iopl.eq.-2) then
          write(*,2002) mac
          return
        elseif(iopl.eq. 3) then
          write(*,2003) name
          if(ior.lt.0) then
            read(*,1000) name
            go to 1
          else
            write(iow,3000)
            call plstop()
          endif
        endif
        if( abs(iopl).eq.3 ) then
          open(ios,file=name,status='new',form='unformatted')
        else
          open(ios,file=name,status='new')
        endif
      endif

c     Format

1000  format(a)

2000  format('  A procedure named ',a,' exist.'/
     &       '  OK to continue? (y or n) >',$)

2001  format('  No procedure or macro command named ',a,' exists.')

2002  format('  No file named ',a,' exists.')

2003  format('  No file named ',a,' exists: Reinput.')

3000  format('  *ERROR* Fatal error - incorrect file?')

      end
