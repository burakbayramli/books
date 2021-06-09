!$Id:$
      subroutine pfe2clean()

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Modification log                                Date (dd/mm/year)
!       Original version                                    01/05/2018
!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:  Clean old FE^2 files
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'comfil.h' ! finp,fout,fmtl
      include   'contrl.h' ! flog
      include   'debugs.h'
      include   'iodata.h' ! ios

      logical             :: lopen,lexist
      character (len=128) :: ufinp
      character   (len=3) :: fext
      integer             :: n

      do n = 1,64
!       Set extender for file
        fext = '000'
        if(n.lt.10) then
          write(fext(3:3),'(i1)') n
        elseif(n.lt.100) then
          write(fext(2:3),'(i2)') n
        endif
!       Input files
        ufinp = ' '
        ufinp = finp
        if(debug) write(*,*) 'I:',ufinp
        call addext(ufinp,fext,128,3)
        inquire(file=ufinp,opened=lopen,exist=lexist)
        if(lexist) then
          if(.not.lopen) open(unit=ios,file = ufinp)
          close(unit=ios,status='delete')
        endif
!       Output files
        ufinp = fout
        if(debug) write(*,*) 'O:',ufinp
        call addext(ufinp,fext,128,3)
        inquire(file=ufinp,opened=lopen,exist=lexist)
        if(lexist) then
          if(.not.lopen) open(unit=ios,file = ufinp)
          close(unit=ios,status='delete')
        endif
!       Material files
        ufinp = finp
        ufinp(1:1) = 'M'
        if(debug) write(*,*) 'M:',ufinp
        call addext(ufinp,fext,128,3)
        inquire(file=ufinp,opened=lopen,exist=lexist)
        if(lexist) then
          if(.not.lopen) open(unit=ios,file = ufinp)
          close(unit=ios,status='delete')
        endif
!       Log files
        ufinp = finp
        ufinp(1:1) = 'L'
        if(debug) write(*,*) 'L:',ufinp
        call addext(ufinp,fext,128,3)
        inquire(file=ufinp,opened=lopen,exist=lexist)
        if(lexist) then
          if(.not.lopen) open(unit=ios,file = ufinp)
          close(unit=ios,status='delete')
        endif
      end do ! n

      end subroutine pfe2clean
