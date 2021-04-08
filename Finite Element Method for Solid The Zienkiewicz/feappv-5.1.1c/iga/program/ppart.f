!$Id:$
      subroutine ppart(tx,isw)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Set element part numbers

!      Inputs:
!        tx(*)    - Names input from data
!        isw      - Switch number
!                   1. Set part name and initial element
!                   2. Set final element number

!      Outputs:
!        partname(*)
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'iofile.h'
      include   'print.h'
      include   'p_ptname.h'

      character  tx(*)*15
      logical    actpart,pcomp
      integer    isw, i

!     Set part name

      if(isw.eq.1) then

        ipart           = ipart + 1
        partname(ipart) = '     '
        actpart         = .true.
        do i = 2,7
          if(pcomp(tx(i),'part',4)) then
            partname(ipart) = tx(i+1)
            ptelm(1,ipart)  = last_elm + 1
            actpart         = .false.
            exit
          endif
        end do ! i

!       Set default name to part
        if(actpart) then
          call setext('PART',ipart, partname(ipart),.false.)
          ptelm(1,ipart)  = last_elm + 1
        endif

        if(prt) then
          write(iow,2000) ipart,partname(ipart)
          if(ior.lt.0) then
            write(*,2000) ipart,partname(ipart)
          endif
        endif

!     Set final element number

      elseif(isw.eq.2) then

        ptelm(2,ipart) = last_elm
        if(prt) then
          write(iow,2001) ipart,ptelm(1,ipart),ptelm(2,ipart)
        endif

      endif

!     Format

2000  format(/10x,'Part(',i3,') = ',a)

2001  format(/10x,'Part(',i3,'): Elm start =',i8,
     &       /21x,'Elm last  =',i8)

      end
