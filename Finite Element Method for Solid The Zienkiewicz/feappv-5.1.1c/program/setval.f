!$Id:$
      subroutine setval(xi,num, val)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Represent character constants by free inputs in strings

!      Inputs:
!         xi        - Input string
!         num       - Length of character string
!      Outputs:
!         val       - Value of string
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'codat.h'
      include  'comfil.h'
      include  'conval.h'
      include  'errchk.h'
      include  'iofile.h'

      character (len=75) :: xt,xs
      character          :: xi*(*)

      logical       :: errco, pcomp, cinput
      integer       :: i, nex, num
      real (kind=8) :: val, v(25)

      save

!     Read value if no constants have been set

      if(.not.coflg) then
        xs(1:15) = ' '
        nex      = 15 - num
        do i = 1,num
          xs(i+nex:i+nex) = xi(i:i)
        end do ! i
        errco = .false.
        call pinval(xs,val,errco)
        if(errco) go to 60
        return
      endif

!     Find letter number for this parameter

60    xs(1:75) = ' '
      xs(1:num) = xi(1:num)

      if(pcomp(xs,'               ',num)) return

!     Evaluate expression

1     nex = 0
      errck = .false.
      call parexp(xs,xt,v,nex,errck)
      if(errck) go to 150

      call pfuncs(xs,v,val,nex,errck)
      if(errck) go to 150
      return

!     An error has been detected in statement respecify

150   if(ior.lt.0) then
        write(*,2001) xi(1:num)
!151    read (*,1000,err=152,end=153) xt
151     if(.not.cinput()) then
          goto 153
        end if
        xt = record(1:75)
        goto  154
!152    call  errclr ('SETVAL')
        call  errclr ('SETVAL')
        goto  151
153     call  endclr ('SETVAL',xt)
154     write(iow,2002) xt
        call pcheck(1,xt,errck)
        xs( 1:74) = xt(1:74)
        xs(75:75) = ' '
        errck = .false.
        go to 1

!     Error on a read

      else
        call  errclr ('SETVAL')
      endif

!     Formats

!1000 format(75a1)

 2001 format(2x,a1,' = ',74a1/'   >',$)

 2002 format('  Correction:>',75a1)

      end subroutine setval
