!$Id:$
      subroutine opnfil(mac,name,iopl,ios,exst)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Open file ios for FEAPpv I/O operations

!      Inputs:
!         mac  - Name for report
!         name - Name of file to open
!         iopl - Indicator on type of file to open or report
!         ios  - Logical unit number to assign to file

!      Outputs:
!         exst - Flag, true if file exists
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'comfil.h'
      include  'iofile.h'

      character (len=1) :: y
      character         :: mac*(*),name*(*)

      logical       :: exst,cinput
      integer       :: iopl,ios

      save

1     inquire(file=name,exist=exst)
      if(exst) then
        if(iopl.eq.1) then
          write(*,2000) mac
!10        read (*,1000,err=11,end=12) y
10        if(.not.cinput()) then
            goto 12
          end if
          y = record(1:1)
          goto  13
!11        call  errclr ('OPNFIL')
          call  errclr ('OPNFIL')
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
!            read(*,1000) name
            if(.not.cinput()) then
              write(*,*) 'Error on cinput OPNFIL'
            end if
            name = record
            go to 1
          else
            write(iow,3000)
            call plstop(.true.)
          endif
        endif
        if( abs(iopl).eq.3 ) then
          open(ios,file=name,status='new',form='unformatted')
        else
          open(ios,file=name,status='new')
        endif
      endif

!     Format

!1000  format(a)

2000  format('  A procedure named ',a,' exist.'/
     &       '  OK to continue? (y or n) >',$)

2001  format('  No procedure or macro command named ',a,' exists.')

2002  format('  No file named ',a,' exists.')

2003  format('  No file named ',a,' exists: Reinput.')

3000  format('  *ERROR* Fatal error - incorrect file?')

      end subroutine opnfil
