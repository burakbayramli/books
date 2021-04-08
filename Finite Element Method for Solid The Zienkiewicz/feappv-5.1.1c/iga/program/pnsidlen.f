!$Id:$
      subroutine pnsidlen(dsid,nsid)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Set nurb sides

!      Data:
!        side i, lside(i), kside(i), nsides(j,i):j=1,lside(i)
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit   none

      include   'iodata.h'

      logical    pcomp,errck,tinput,pinput, startfl
      character  tx*15
      integer    j, jj, lsid, dsid,nsid
      real*8     td(16)

      startfl = .true.
      tx      = 'start'
      do while (.not.pcomp(tx,'    ',4))
        errck = tinput(tx,1,td,15)
        if(pcomp(tx,'side',4) .or. pcomp(tx,'bnet',4)) then
          if(startfl) then
            write(ios,'(a)') 'NSIDes'
            startfl = .false.
          endif
          nsid = max(nsid,nint(td(1)))
          lsid = nint(td(2))
          dsid = max(dsid,lsid)
          write(ios,2001) (nint(td(j)),j=1,min(15,lsid+3))
          do jj = 12,lsid-1,16
            errck = pinput(td,16)
            write(ios,2002) (nint(td(j)),j=1,min(16,lsid-jj))
          end do ! jj
        endif
      end do ! while

      write(ios,'(a)') ' '

!     Formats

2001  format('  side',3i8,12i8)
2002  format(16i8)

      end
